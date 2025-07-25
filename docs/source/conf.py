# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
# This file does only contain a selection of the most common options. For a
# full list see the documentation:
# http://www.sphinx-doc.org/en/master/config

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

import sys
import os
import datetime
import sphinx_rtd_theme
from pygments.lexer import RegexLexer, words
from pygments import token
from sphinx.highlighting import lexers

sys.path.append(os.path.abspath("./_ext"))

# -- Project information -----------------------------------------------------

project = 'Roto'
year = datetime.datetime.now().year
copyright = f'2020–{year}, NLnet Labs'
author = 'NLnet Labs'

# The full version, including alpha/beta/rc tags.
# Note: this is not only used in the documentation text, but also in several
# commands!
version = "0.6.0"

# -- Sphinx Tabs configuration -----------------------------------------------

sphinx_tabs_disable_tab_closing = True
sphinx_tabs_disable_css_loading = True


# -- General configuration ---------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.todo',
    'sphinx.ext.ifconfig',
    'sphinx_tabs.tabs',
    'sphinx_copybutton',
    'sphinxcontrib.jquery',
    'sphinx.ext.intersphinx',
    'sphinx.ext.autosectionlabel',
    'sphinx_substitution_extensions',
    'roto_domain',
    'myst_parser',
]

autosectionlabel_prefix_document = True
suppress_warnings = ['autosectionlabel.*']

# Add any paths that contain templates here, relative to this directory.

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
#
# source_suffix = ['.rst', '.md']
source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

# The master toctree document.
master_doc = 'index'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = 'en'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path .
exclude_patterns = []

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'
# html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]
html_logo = 'resources/roto-logo-white.svg'
html_favicon = 'resources/rotonda-icon-offwhite-128x128.png'
html_theme_options = {
    'logo_only': True,
    'style_external_links': False,
    'globaltoc_maxdepth': 1,
}


# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['resources']

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# The default sidebars (for documents that don't match any pattern) are
# defined by theme itself.  Builtin themes are using these templates by
# default: ``['localtoc.html', 'relations.html', 'sourcelink.html',
# 'searchbox.html']``.
#
# html_sidebars = {}


# -- Options for HTMLHelp output ---------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = 'RotondaUserManualdoc'


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',

    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',

    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',

    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, 'RotondaUserManual.tex', 'Rotonda User Manual',
     'NLnet Labs', 'manual'),
]


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('manual-page', 'rotonda', 'Modular, analytical BGP engine',
     "Jasper den Hertog, Ximon Eighteen, Luuk Hendriks, Terts Diepraam", 1)
]


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc, 'RotondaUserManual', 'Rotonda User Manual',
     author, 'RotondaUserManual', 'One line description of project.',
     'Miscellaneous'),
]


# -- Options for Epub output -------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project
epub_author = author
epub_publisher = author
epub_copyright = copyright

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#
# epub_identifier = ''

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']


# -- Extension configuration -------------------------------------------------

# -- Options for todo extension ----------------------------------------------

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True


# -- Extension interface -----------------------------------------------------

from sphinx import addnodes
def parse_cmd_args_node(env, sig, signode):
    try:
        cmd, args = sig.strip().split(' ', 1)
    except ValueError:
        cmd, args = sig, None
    # distinguish cmd from its args
    signode += addnodes.desc_name(cmd, cmd)
    if args:
        args = ' ' + args
        signode += addnodes.desc_addname(args, args)
    return cmd
# define new directive/role that can be used as .. subcmd::/:subcmd:
def setup(app):
    app.add_object_type('subcmd', 'subcmd',
                        objname='module sub-command',
                        indextemplate='pair: %s; module sub-command',
                        parse_node=parse_cmd_args_node)
    app.add_css_file('css/dark.css')
    app.add_css_file('css/light.css')
    
# -- Options for copybutton extenstion ---------------------------------------

# Configure this so the prompt will not be copied to the clipboard. Also
# prevents output lines (lines not starting with the prompt) to be copied.
copybutton_prompt_text = "$"

# -- Options for substitution extension --------------------------------------

rst_prolog = ".. |version| replace:: {version}".format(version = version)

class RotoLexer(RegexLexer):
    name = 'roto'

    tokens = {
        'root': [
            (r'#.*?$', token.Comment.Singleline),
            (
                words(
                    ('type', 'fn', 'filtermap', 'filter', 'match', 'let', 'if', 'else', 'accept', 'reject', 'return', 'super', 'pkg', 'std', 'dep', 'import'),
                    suffix=r'\b'
                ),
                token.Keyword
            ),
            (words(('+', '-', '/', '*', '==', '>=', '>', '<=', '<', '=', '&&', '||')), token.Operator),
            (words(('{', '}', '(', ')', '[', ']', ':', '.', ';', ',')), token.Punctuation),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', token.Name),
            (r'[0-9]', token.Number),
            (r'\s+', token.Text.Whitespace),
            (r'"', token.String, "string"),
        ],
        'string': [
            (r'[^"]', token.String),
            (r'"', token.String, "#pop"),
        ]
    }

lexers['roto'] = RotoLexer(startinline=True)
