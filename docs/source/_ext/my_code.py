from __future__ import annotations

from docutils.parsers.rst import directives
from docutils import nodes

from sphinx.application import Sphinx

from sphinx.util.docutils import SphinxDirective, SphinxRole

from sphinx.util.typing import ExtensionMetadata

from sphinx.directives.code import CodeBlock
from sphinx import addnodes
from sphinx.transforms import SphinxTransform
from docutils.nodes import literal_block, Text
import json

class Testoutput(CodeBlock):
    def run(self) -> list[Node]:
        self.arguments.append("text")

        nodes = super().run()
        nodes[0].attributes['istestoutput'] = True
        return nodes

found = []

def find_code(app, doctree, fromdocname):
    last_code_block_name = None
    
    for node in doctree.traverse(literal_block):
        lang = node.attributes.get("language", "default")
        classes = node.attributes.get("classes", [])
        istestoutput = node.attributes.get("istestoutput", False)

        if istestoutput:
            code = found[-1]

            if code['testoutput']:
                raise Exception(f"multiple testoutputs: {src}:{line}")

            code['testoutput'] = node.children[0] + "\n"
            code['mode'] = "run"
            continue

        if lang != "roto":
            continue

        if "test-ignore" in classes:
            continue

        if "test-ignore" in classes:
            mode = "ignore"
        elif "test-error" in classes:
            mode = "error"
        else:
            mode = "check"
        
        for subnode in node.traverse(Text):
            found.append({
                "src": fromdocname,
                "lang": lang,
                "code": subnode,
                "source": node.source,
                "line": node.line,
                "testoutput": None,
                "mode": mode,
            })

lang_map = {
    'roto': 'Roto',
    'rust': 'Rust',
    'console': 'Console',
    'text': 'Text',
    'default': '',
}

class CodeBlockTransform(SphinxTransform):

    default_priority = 100

    def apply(self, **kwargs):
        for node in self.document.findall(literal_block):
            lang = node.attributes.get("language")

            if node.attributes.get("istestoutput"):
                lang_str = "Output"
            elif lang:
                lang_str = lang_map[lang]
            else:
                continue

            if not lang_str:
                continue

            lang = nodes.inline(nodes.Inline(), nodes.Text(lang_str))
            lang.set_class("lang-tag")
            new_node = nodes.container("", lang, node.deepcopy())
            new_node.set_class("codeblock-container")

            node.replace_self(new_node)


def output(app, exception):
    if exception is not None:
        return

    dest = app.config.test_code_output
    if dest is None:
        return

    for code in found:
        if code['testoutput'] is None:
            code['testoutput'] = ""

    with open(dest, "wt") as fd:
        json.dump(found, fd, indent=4)


def setup(app: Sphinx) -> ExtensionMetadata:
    app.add_config_value('test_code_output', None, '')

    app.add_transform(CodeBlockTransform)
    app.add_directive('testoutput', Testoutput)

    app.connect('doctree-resolved', find_code)
    app.connect('build-finished', output)

    return {
        "version": '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
