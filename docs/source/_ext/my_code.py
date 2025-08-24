from __future__ import annotations

from docutils import nodes

from sphinx.application import Sphinx

from sphinx.util.docutils import SphinxDirective, SphinxRole

from sphinx.util.typing import ExtensionMetadata

from sphinx.directives.code import CodeBlock
from sphinx import addnodes

class Code(CodeBlock):
    def run(self) -> list[Node]:
        try:
            language = self.arguments[0]
            self.options['caption'] = language.title()
        except:
            pass

        return super().run()


def setup(app: Sphinx) -> ExtensionMetadata:
    app.add_directive('code-block', Code)
    app.add_directive('sourcecode', Code)
    app.add_directive('code', Code)

    return {
        'version': '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
