from __future__ import annotations

from docutils.parsers.rst import directives
from docutils import nodes

from sphinx.application import Sphinx

from sphinx.util.docutils import SphinxDirective, SphinxRole

from sphinx.util.typing import ExtensionMetadata

from sphinx.directives.code import CodeBlock
from sphinx import addnodes
from docutils.nodes import literal_block, Text
import json

class Code(CodeBlock):
    option_spec = {
        "notest": directives.flag, 
         **CodeBlock.option_spec,
    }

    def run(self) -> list[Node]:
        try:
            language = self.arguments[0]
            self.options['caption'] = language.title()
        except:
            pass

        nodes = super().run()

        if self.options.get('caption'):
            nodes[0].children[-1].attributes['notest'] = 'notest' in self.options
        else:
            nodes[0].attributes['notest'] = 'notest' in self.options

        return nodes

class Testoutput(CodeBlock):
    def run(self) -> list[Node]:
        self.options['caption'] = "Output"
        self.arguments.append("text")

        nodes = super().run()
        nodes[0].children[-1].attributes['istestoutput'] = True
        return nodes

found = []

def find_code(app, doctree, fromdocname):
    last_code_block_name = None
    
    for node in doctree.traverse(literal_block):
        # if "dballe.DB.connect" in str(node):
        lang = node.attributes.get("language", "default")
        notest = node.attributes.get("notest", None)
        istestoutput = node.attributes.get("istestoutput", False)

        if istestoutput:
            code = found[-1]

            if code['testoutput']:
                raise Exception(f"multiple testoutputs: {src}:{line}")

            print(node)
            code['testoutput'] = node.children[0] + "\n"
            continue

        if lang != "roto":
            continue

        if notest:
            continue
        
        for subnode in node.traverse(Text):
            found.append({
                "src": fromdocname,
                "lang": lang,
                "code": subnode,
                "source": node.source,
                "line": node.line,
                "testoutput": None
            })


def output(app, exception):
    if exception is not None:
        return

    dest = app.config.test_code_output
    if dest is None:
        return

    no_tests_for = "".join(f" - {code['src']}:{code['line']}\n" for code in found if code['testoutput'] is None)
    if no_tests_for:
        raise Exception(f"untested code blocks:\n{no_tests_for}")

    with open(dest, "wt") as fd:
        json.dump(found, fd, indent=4)

def setup(app: Sphinx) -> ExtensionMetadata:
    app.add_config_value('test_code_output', None, '')

    app.add_directive('code-block', Code)
    app.add_directive('sourcecode', Code)
    app.add_directive('code', Code)
    app.add_directive('testoutput', Testoutput)

    app.connect('doctree-resolved', find_code)
    app.connect('build-finished', output)

    return {
        "version": '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
