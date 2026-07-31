//! Formatter for Roto code

use std::{collections::HashSet, path::Path};

use crate::{
    FileTree, RotoError, RotoReport,
    ast::{self, Identifier},
    parser::{
        Extras,
        meta::{Meta, Span, Spans},
    },
};

#[derive(Debug)]
enum Node<'a> {
    /// A group of nodes with an id
    Group(usize, Vec<Node<'a>>),

    /// An indented list of nodes
    Indent(Vec<Node<'a>>),

    /// An indented list of nodes, where the first item is not indented
    IndentNext(Vec<Node<'a>>),

    /// Wrap the surrounding group
    WrapParent,

    /// A comment
    Comment(&'a str),

    /// A string with a cached width (because unicode is hard)
    Str(usize, &'a str),

    /// An ASCII string (which has a width equal to its length)
    Ascii(&'a str),

    /// Ensure that we are on a new line
    Line,

    /// An empty line
    EmptyLine,

    /// A line if wrapped, a space otherwise
    LineOrSpace,

    /// Decides what to render based on whether the referenced group wraps
    IfWrap(usize, Box<Node<'a>>, Box<Node<'a>>),
}

impl<'s> Node<'s> {
    fn ident(x: Identifier) -> Self {
        Self::str(x.as_str())
    }

    fn str(x: &'s str) -> Self {
        use unicode_width::UnicodeWidthStr;
        let width = x.width();
        Node::Str(width, x)
    }

    fn must_wrap(&self) -> bool {
        match self {
            Node::WrapParent => true,
            Node::Comment(_) => true,
            Node::Indent(n) => n.iter().any(|n| n.must_wrap()),
            _ => false,
        }
    }

    fn width(&self, wrapped: &mut HashSet<usize>) -> usize {
        match self {
            Node::Group(_, nodes)
            | Node::Indent(nodes)
            | Node::IndentNext(nodes) => {
                nodes.iter().map(|n| n.width(wrapped)).sum()
            }
            Node::WrapParent => 0,
            Node::Comment(_) => 0,
            Node::Str(w, _) => *w,
            Node::Ascii(s) => s.len(),
            Node::Line => 0,
            Node::LineOrSpace => 1,
            Node::EmptyLine => 0,
            Node::IfWrap(id, node, node1) => {
                if wrapped.contains(id) {
                    node.width(wrapped)
                } else {
                    node1.width(wrapped)
                }
            }
        }
    }
}

/// Format the contents of a file at a path
pub fn fmt_path(path: &Path) -> Result<String, RotoReport> {
    let tree = FileTree::single_file(path)?;
    let mut spans = Spans::default();
    let (ast, extras) = match crate::parser::Parser::parse(
        0,
        &mut spans,
        &tree.files[0].contents,
    ) {
        Ok(x) => x,
        Err(err) => {
            return Err(RotoReport {
                files: tree.files,
                errors: vec![RotoError::Parse(*err)],
                spans,
            });
        }
    };

    Ok(fmt_parsed(&ast, &tree, &spans, &extras))
}

/// Format a string
pub fn fmt_str(
    content: &str,
    source: &str,
    offset: usize,
) -> Result<String, RotoReport> {
    let mut spans = Spans::default();
    let tree = FileTree::test_file(content, source, offset);
    let (ast, extras) = match crate::parser::Parser::parse(
        0,
        &mut spans,
        &tree.files[0].contents,
    ) {
        Ok(x) => x,
        Err(err) => {
            return Err(RotoReport {
                files: tree.files,
                errors: vec![RotoError::Parse(*err)],
                spans,
            });
        }
    };

    Ok(fmt_parsed(&ast, &tree, &spans, &extras))
}

fn fmt_parsed(
    ast: &ast::SyntaxTree,
    source: &FileTree,
    spans: &Spans,
    extras: &Extras,
) -> String {
    let mut nodes = Vec::new();
    let mut idx = 0;
    let mut pos = 0;
    let mut extras = extras.clone();
    let mut state = State {
        nodes: &mut nodes,
        idx: &mut idx,
        extras: &mut extras,
        source,
        spans,
        pos: &mut pos,
    };

    let mut first = true;
    for dec in &ast.declarations {
        state.declaration(dec, first);
        first = false;
    }

    state.push(Node::Line);

    let mut renderer = Renderer {
        buf: String::new(),
        indent: 0,
        column: 0,
        max: 80,
        wrapped: HashSet::new(),
    };

    for node in &nodes {
        renderer.render_node(node, true);
    }

    renderer.buf
}

#[derive(PartialEq, Eq)]
enum WrapStyle {
    /// Wrap delimiters directly around content if not wrapping: `(x, y, z)`
    Tight,

    /// Put spaces between delimiter and content if not wrapping: `( x, y, z )`
    Spaced,

    /// Always wrap the content
    ///
    /// ```txt
    /// (
    ///     x,
    ///     y,
    ///     z,
    /// )
    /// ```
    Wrap,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Bottom,
    LogicalOr,
    LogicalAnd,
    Comparison,
    AddSub,
    MulDiv,
    Not,
    Chain,
    Unit,
}

struct State<'a, 's> {
    idx: &'a mut usize,
    nodes: &'a mut Vec<Node<'s>>,
    extras: &'a mut Extras,
    source: &'s FileTree,
    spans: &'s Spans,
    pos: &'a mut usize,
}

impl<'a, 's> State<'a, 's> {
    fn extend(&mut self, nodes: impl IntoIterator<Item = Node<'s>>) {
        self.nodes.extend(nodes);
    }

    fn push(&mut self, node: Node<'s>) {
        self.nodes.push(node);
    }

    fn group<'b>(&'b mut self) -> (usize, State<'b, 's>) {
        let idx = *self.idx;
        *self.idx += 1;
        self.nodes.push(Node::Group(idx, Vec::new()));
        let Some(Node::Group(_, nodes)) = self.nodes.last_mut() else {
            unreachable!();
        };
        (
            idx,
            State {
                idx: self.idx,
                nodes,
                extras: self.extras,
                source: self.source,
                spans: self.spans,
                pos: self.pos,
            },
        )
    }

    fn indent<'b>(&'b mut self) -> State<'b, 's> {
        self.nodes.push(Node::Indent(Vec::new()));
        let Some(Node::Indent(nodes)) = self.nodes.last_mut() else {
            unreachable!();
        };
        State {
            idx: self.idx,
            nodes,
            extras: self.extras,
            source: self.source,
            spans: self.spans,
            pos: self.pos,
        }
    }

    fn indent_next<'b>(&'b mut self) -> State<'b, 's> {
        self.nodes.push(Node::IndentNext(Vec::new()));
        let Some(Node::IndentNext(nodes)) = self.nodes.last_mut() else {
            unreachable!();
        };
        State {
            idx: self.idx,
            nodes,
            extras: self.extras,
            source: self.source,
            spans: self.spans,
            pos: self.pos,
        }
    }

    fn span_content(&mut self, span: Span) -> &'s str {
        &self.source.files[span.file].contents[span.start..span.end]
    }

    fn pop_trailing_comment(&mut self) {
        // First look for something that isn't whitespace or is a newline
        let Some(something) = self.source.files[0]
            .contents
            .get(*self.pos..)
            .and_then(|s| s.find(|c: char| !c.is_whitespace() || c == '\n'))
        else {
            return;
        };
        let something = something + *self.pos;

        // If that is the first comment, pop it!
        if let Some(comment) =
            self.extras.comments.pop_front_if(|s| s.start == something)
        {
            let content = self.span_content(comment);
            let content = content.trim_end();
            self.push(Node::Comment(content));
            *self.pos = comment.end;
        }
    }

    fn pop_whitespace(
        &mut self,
        position: usize,
        preserve_leading_whitespace: bool,
        preserve_trailing_whitespace: bool,
        preserve_only_whitespace: bool,
    ) -> bool {
        let mut first = true;
        while let Some(comment) =
            self.extras.comments.pop_front_if(|s| s.start < position)
        {
            if first {
                if preserve_leading_whitespace {
                    self.newlines(comment.start);
                } else {
                    self.ignore_newlines(comment.start);
                }
            } else {
                self.newlines(comment.start);
            }
            *self.pos = comment.end - 1;
            let content = self.span_content(comment);
            let content = content.trim_end();
            self.push(Node::Comment(content));
            first = false;
        }
        if !first && preserve_trailing_whitespace {
            self.newlines(position);
            true
        } else {
            if preserve_only_whitespace {
                self.newlines(position)
            } else {
                self.ignore_newlines(position);
                false
            }
        }
    }

    fn count_newlines(&self, pos: usize) -> usize {
        let Some(s) = self.source.files[0].contents.get(*self.pos..pos)
        else {
            return 0;
        };
        s.chars().filter(|c| *c == '\n').count()
    }

    fn newlines(&mut self, pos: usize) -> bool {
        let newlines = self.count_newlines(pos);

        match newlines {
            0 => {}
            1 => self.push(Node::Line),
            2.. => self.push(Node::EmptyLine),
        }

        *self.pos = pos;
        newlines > 0
    }

    fn ignore_newlines(&mut self, pos: usize) {
        let pos = if let Some(x) = self.extras.comments.front() {
            pos.min(x.start)
        } else {
            pos
        };

        *self.pos = pos;
    }

    fn separated<Elem>(
        &mut self,
        start: &'static str,
        end: &'static str,
        separator: &'static str,
        elems: &[Elem],
        span: Span,
        style: WrapStyle,
        mut f: impl FnMut(&mut State<'_, 's>, &Elem),
    ) {
        let (id, mut grouped) = self.group();

        if elems.is_empty() {
            grouped.push(Node::Ascii(start));
            grouped.push(Node::Line);
            let pos = span.end;
            let mut indented = grouped.indent();
            indented.pop_whitespace(pos, false, false, false);
            grouped.push(Node::Line);
            grouped.push(Node::Ascii(end));
            grouped.pop_trailing_comment();
            return;
        }

        if style == WrapStyle::Wrap {
            grouped.push(Node::WrapParent);
        }

        grouped.push(Node::Ascii(start));

        if style == WrapStyle::Spaced {
            grouped.push(Node::LineOrSpace);
        } else {
            grouped.push(Node::Line);
        }

        let mut indented = grouped.indent();
        let mut first = true;
        for elem in elems {
            if !first {
                if !separator.is_empty() {
                    indented.push(Node::Ascii(separator));
                }
                indented.push(Node::LineOrSpace);
            }
            f(&mut indented, elem);
            first = false;
        }
        if !separator.is_empty() {
            indented.push(Node::IfWrap(
                id,
                Box::new(Node::Ascii(separator)),
                Box::new(Node::Ascii("")),
            ));
        }
        let pos = span.end;
        indented.pop_whitespace(pos, true, false, false);

        if style == WrapStyle::Spaced {
            grouped.push(Node::LineOrSpace);
        } else {
            grouped.push(Node::Line);
        }
        grouped.push(Node::Ascii(end));
        grouped.pop_trailing_comment();
    }

    fn declaration(&mut self, dec: &ast::Declaration, first: bool) {
        match dec {
            ast::Declaration::FilterMap(x) => {
                let pos = self.spans.get(&x.ident).start;
                let whitespace =
                    self.pop_whitespace(pos, !first, true, !first);
                if !first && !whitespace {
                    self.push(Node::Line);
                }
                self.push(Node::Ascii("filter-map "));
                self.push(Node::ident(*x.ident));
                self.params(&x.params);
                self.block(&x.body);
            }
            ast::Declaration::Const(x) => {
                let pos = self.spans.get(&x.ident).start;
                let whitespace =
                    self.pop_whitespace(pos, !first, true, !first);
                if !first && !whitespace {
                    self.push(Node::Line);
                }
                self.extend([
                    Node::Ascii("const "),
                    Node::ident(*x.ident),
                    Node::Ascii(": "),
                ]);
                self.type_expr(&x.ty);
                self.push(Node::Ascii(" = "));
                self.expr(&x.expr);
                self.push(Node::Ascii(";"));
            }
            ast::Declaration::Record(x) => {
                let pos = self.spans.get(&x.ident).start;
                let whitespace =
                    self.pop_whitespace(pos, !first, true, !first);
                if !first && !whitespace {
                    self.push(Node::Line);
                }
                self.push(Node::Ascii("record "));
                self.push(Node::ident(*x.ident));
                if let Some(p) = &x.type_params
                    && !p.is_empty()
                {
                    self.type_params(p)
                }
                self.push(Node::Ascii(" "));
                self.record_type(&x.record_type, WrapStyle::Wrap);
            }
            ast::Declaration::Enum(x) => {
                let pos = self.spans.get(&x.ident).start;
                let whitespace =
                    self.pop_whitespace(pos, !first, true, !first);
                if !first && !whitespace {
                    self.push(Node::Line);
                }
                self.push(Node::Ascii("enum "));
                self.push(Node::ident(*x.ident));
                if let Some(p) = &x.type_params
                    && !p.is_empty()
                {
                    self.type_params(p)
                }
                self.push(Node::Ascii(" "));

                let span = self.spans.get(&x.variants);
                self.separated(
                    "{",
                    "}",
                    ",",
                    &x.variants,
                    span,
                    WrapStyle::Wrap,
                    |this, variant| {
                        this.push(Node::ident(*variant.ident));

                        if !variant.fields.is_empty() {
                            // TODO: We should have a span for this
                            let span = this
                                .spans
                                .get(variant.fields.last().unwrap());
                            this.separated(
                                "(",
                                ")",
                                ",",
                                &variant.fields,
                                span,
                                WrapStyle::Tight,
                                |this, field| {
                                    this.type_expr(field);
                                },
                            );
                        }
                    },
                );
            }
            ast::Declaration::Function(x) => {
                let pos = self.spans.get(&x.ident).start;
                let whitespace =
                    self.pop_whitespace(pos, !first, true, !first);
                if !first && !whitespace {
                    self.push(Node::Line);
                }
                self.push(Node::Ascii("fn "));
                self.push(Node::ident(*x.ident));
                self.params(&x.params);
                self.push(Node::Ascii(" "));
                if let Some(ret) = &x.ret {
                    self.push(Node::Ascii("-> "));
                    self.type_expr(ret);
                }
                self.block(&x.body);
            }
            ast::Declaration::Test(x) => {
                let pos = self.spans.get(&x.ident).start;
                let whitespace =
                    self.pop_whitespace(pos, !first, true, !first);
                if !first && !whitespace {
                    self.push(Node::Line);
                }
                self.push(Node::Ascii("test "));
                self.push(Node::ident(*x.ident));
                self.push(Node::Ascii(" "));
                self.block(&x.body);
            }
            ast::Declaration::Import(x) => {
                let pos = self.spans.get(x).start;
                let whitespace =
                    self.pop_whitespace(pos, !first, true, !first);
                if !first && !whitespace {
                    self.push(Node::Line);
                }
                self.push(Node::Ascii("import "));
                self.import_path(x);
                self.push(Node::Ascii(";"));
            }
        }
    }

    fn import_path(&mut self, path: &Meta<ast::ImportPath>) {
        if let Some(path) = &path.path {
            self.path(path, false);
        }

        if path.path.is_some() && path.group.is_some() {
            self.push(Node::Ascii("."));
        }

        if let Some(group) = &path.group {
            if let [x] = &group.node[..] {
                self.import_path(x);
                return;
            }
            let span = self.spans.get(group);
            self.separated(
                "{",
                "}",
                ",",
                group,
                span,
                WrapStyle::Tight,
                |this, path| {
                    this.import_path(path);
                },
            );
        }
    }

    fn params(&mut self, params: &Meta<ast::Params>) {
        let span = self.spans.get(params);
        self.separated(
            "(",
            ")",
            ",",
            &params.0,
            span,
            WrapStyle::Tight,
            |this, param| {
                this.push(Node::ident(*param.0));
                this.push(Node::Ascii(": "));
                this.type_expr(&param.1);
            },
        )
    }

    fn type_params(&mut self, params: &Meta<Vec<Meta<Identifier>>>) {
        let span = self.spans.get(params);
        self.separated(
            "[",
            "]",
            ",",
            params,
            span,
            WrapStyle::Tight,
            |this, name| {
                this.push(Node::ident(**name));
            },
        );
    }

    fn block(&mut self, block: &Meta<ast::Block>) {
        let (_, mut grouped) = self.group();
        grouped.push(Node::Ascii("{"));
        grouped.push(Node::Line);

        if !block.stmts.is_empty() || block.last.is_some() {
            grouped.push(Node::WrapParent);
        }

        let mut indented = grouped.indent();

        let mut first = true;
        for stmt in &block.stmts {
            let span = indented.spans.get(stmt);
            if first {
                indented.pop_whitespace(span.start, false, true, false);
            } else {
                indented.pop_whitespace(span.start, true, true, true);
            }
            indented.push(Node::Line);
            indented.stmt(stmt);
            indented.pop_whitespace(span.end, false, false, false);
            indented.pop_trailing_comment();
            first = false;
        }

        if let Some(last) = &block.last {
            let pos = indented.spans.get(&**last).start;
            if first {
                indented.pop_whitespace(pos, false, true, false)
            } else {
                indented.pop_whitespace(pos, true, true, true)
            };
            indented.push(Node::Line);
            indented.expr(last);
            indented.pop_trailing_comment();
            first = false;
        }

        let pos = indented.spans.get(block).end;
        if first {
            indented.pop_whitespace(pos, false, false, false);
        } else {
            indented.pop_whitespace(pos, true, false, false);
        }

        grouped.push(Node::Line);
        grouped.push(Node::Ascii("}"));
    }

    fn stmt(&mut self, stmt: &Meta<ast::Stmt>) {
        match &**stmt {
            ast::Stmt::Let(ident, ty, expr) => {
                self.push(Node::Ascii("let "));
                self.push(Node::ident(**ident));
                if let Some(ty) = ty {
                    self.push(Node::Ascii(": "));
                    self.type_expr(ty);
                }
                self.push(Node::Ascii(" = "));
                self.expr(expr);
                self.push(Node::Ascii(";"));
            }
            ast::Stmt::Expr(e) => {
                let (_idx, mut grouped) = self.group();
                grouped.expr(e);

                match &**e {
                    ast::Expr::Block(_)
                    | ast::Expr::Match(_)
                    | ast::Expr::IfElse(_, _, _)
                    | ast::Expr::While(_, _)
                    | ast::Expr::For(_, _, _) => {}
                    _ => {
                        grouped.push(Node::Ascii(";"));
                    }
                }
            }
        }
    }

    fn expr(&mut self, expr: &Meta<ast::Expr>) {
        self.expr_with_precedence(expr, Precedence::Bottom);
    }

    fn expr_precedence(&mut self, expr: &Meta<ast::Expr>) -> Precedence {
        match &**expr {
            ast::Expr::Return(_, _) => Precedence::Unit,
            ast::Expr::Literal(_) => Precedence::Unit,
            ast::Expr::Block(_) => Precedence::Unit,
            ast::Expr::Match(_) => Precedence::Unit,
            ast::Expr::FunctionCall(_, _) => Precedence::Chain,
            ast::Expr::Access(_, _) => Precedence::Chain,
            ast::Expr::Path(_) => Precedence::Chain,
            ast::Expr::Record(_) => Precedence::Unit,
            ast::Expr::TypedRecord(_, _) => Precedence::Unit,
            ast::Expr::List(_) => Precedence::Unit,
            ast::Expr::Not(_) => Precedence::Unit,
            ast::Expr::Assign(_, _) => Precedence::Unit,
            ast::Expr::CompoundAssign(_) => Precedence::Unit,
            ast::Expr::BinOp(_, op, _) => Self::binop_precedence(op),
            ast::Expr::Negate(_) => Precedence::Not,
            ast::Expr::IfElse(_, _, _) => Precedence::Unit,
            ast::Expr::While(_, _) => Precedence::Unit,
            ast::Expr::For(_, _, _) => Precedence::Unit,
            ast::Expr::QuestionMark(_) => Precedence::Chain,
            ast::Expr::FString(_) => Precedence::Unit,
        }
    }

    fn binop_precedence(op: &ast::BinOp) -> Precedence {
        match op {
            ast::BinOp::And => Precedence::LogicalAnd,
            ast::BinOp::Or => Precedence::LogicalOr,
            ast::BinOp::Eq
            | ast::BinOp::Ne
            | ast::BinOp::Lt
            | ast::BinOp::Le
            | ast::BinOp::Gt
            | ast::BinOp::Ge => Precedence::Comparison,
            ast::BinOp::Add | ast::BinOp::Sub => Precedence::AddSub,
            ast::BinOp::Mul | ast::BinOp::Div | ast::BinOp::Mod => {
                Precedence::MulDiv
            }
        }
    }

    fn expr_with_precedence(
        &mut self,
        expr: &Meta<ast::Expr>,
        outer: Precedence,
    ) {
        let precedence = self.expr_precedence(expr);
        match &**expr {
            ast::Expr::Return(kind, expr) => {
                let s = match kind {
                    ast::ReturnKind::Return => "return",
                    ast::ReturnKind::Accept => "accept",
                    ast::ReturnKind::Reject => "reject",
                };
                self.push(Node::Ascii(s));
                if let Some(expr) = expr {
                    self.push(Node::Ascii(" "));
                    self.expr(expr);
                }
            }
            ast::Expr::Literal(x) => {
                self.literal(x);
            }
            ast::Expr::Block(x) => {
                self.block(x);
            }
            ast::Expr::Match(x) => self.r#match(x),
            ast::Expr::FunctionCall(x, args) => {
                if precedence > outer {
                    let (_, mut grouped) = self.group();
                    let mut indented = grouped.indent_next();
                    indented.function_call(x, args);
                } else {
                    self.function_call(x, args);
                }
            }
            ast::Expr::Access(x, field) => {
                if precedence > outer {
                    let (_, mut grouped) = self.group();
                    let mut indented = grouped.indent_next();
                    indented.access(x, field);
                } else {
                    self.access(x, field);
                }
                self.access(x, field);
            }
            ast::Expr::Path(x) => {
                self.path(x, precedence > outer);
            }
            ast::Expr::Record(x) => {
                self.record(x);
            }
            ast::Expr::TypedRecord(name, record) => {
                self.path(name, true);
                self.push(Node::Ascii(" "));
                self.record(record);
            }
            ast::Expr::List(x) => {
                let span = self.spans.get(expr);
                self.separated(
                    "[",
                    "]",
                    ",",
                    x,
                    span,
                    WrapStyle::Tight,
                    |this, v| {
                        this.expr(v);
                    },
                );
            }
            ast::Expr::Not(x) => {
                self.push(Node::Ascii("!"));
                self.expr_with_precedence(x, precedence);
            }
            ast::Expr::Assign(x, e) => {
                self.path(x, true);
                self.push(Node::Ascii(" = "));
                self.expr(e);
            }
            ast::Expr::CompoundAssign(x) => {
                self.path(&x.path, true);
                let op = match *x.op {
                    ast::CompoundAssignOp::Add => " += ",
                    ast::CompoundAssignOp::Sub => " -= ",
                    ast::CompoundAssignOp::Mul => " *= ",
                    ast::CompoundAssignOp::Div => " /= ",
                    ast::CompoundAssignOp::Mod => " %= ",
                };
                self.push(Node::Ascii(op));
                self.expr(&x.expr);
            }
            ast::Expr::BinOp(left, op, right) => self.binop(left, op, right),
            ast::Expr::Negate(x) => {
                self.push(Node::Ascii("-"));
                self.expr(x);
            }
            ast::Expr::IfElse(c, t, e) => {
                self.push(Node::Ascii("if "));
                self.expr(c);
                self.push(Node::Ascii(" "));
                self.block(t);
                if let Some(e) = e {
                    self.push(Node::Ascii(" else "));
                    self.block(e);
                }
            }
            ast::Expr::While(c, b) => {
                self.push(Node::Ascii("while "));
                self.expr(c);
                self.push(Node::Ascii(" "));
                self.block(b);
            }
            ast::Expr::For(x, e, b) => {
                self.push(Node::Ascii("for "));
                self.push(Node::ident(**x));
                self.push(Node::Ascii(" in "));
                self.expr(e);
                self.push(Node::Ascii(" "));
                self.block(b);
            }
            ast::Expr::QuestionMark(e) => {
                self.expr_with_precedence(e, precedence);
                self.push(Node::Ascii("?"));
            }
            ast::Expr::FString(parts) => {
                let span = self.spans.get(expr);
                self.pop_whitespace(span.start, true, true, false);
                self.push(Node::Ascii("f\""));
                for part in parts {
                    let span = self.spans.get(part);
                    match &**part {
                        ast::FStringPart::String(_) => {
                            let y = self.span_content(span);
                            self.push(Node::Ascii(y));
                        }
                        ast::FStringPart::Expr(expr) => {
                            self.push(Node::Ascii("{"));
                            self.expr(expr);
                            self.push(Node::Ascii("}"));
                        }
                    }
                }
                self.push(Node::Ascii("\""));
            }
        }
        self.pop_trailing_comment();
    }

    fn function_call(
        &mut self,
        x: &Meta<ast::Expr>,
        args: &Meta<Vec<Meta<ast::Expr>>>,
    ) {
        self.expr_with_precedence(x, Precedence::Chain);
        let span = self.spans.get(args);
        self.separated(
            "(",
            ")",
            ",",
            args,
            span,
            WrapStyle::Tight,
            |this, arg| {
                this.expr(arg);
            },
        );
    }

    fn access(&mut self, e: &Meta<ast::Expr>, field: &Meta<Identifier>) {
        self.expr_with_precedence(e, Precedence::Chain);
        self.push(Node::Line);
        self.push(Node::Ascii("."));
        self.push(Node::ident(**field));
    }

    fn binop(
        &mut self,
        mut left: &Meta<ast::Expr>,
        op: &Meta<ast::BinOp>,
        right: &Meta<ast::Expr>,
    ) {
        let precedence = Self::binop_precedence(op);

        let mut ops = Vec::new();

        ops.push((op, right));

        loop {
            match &**left {
                ast::Expr::BinOp(l, op, r)
                    if self.expr_precedence(left) == precedence =>
                {
                    left = l;
                    ops.push((op, r));
                }
                _ => {
                    break;
                }
            }
        }

        let (_, mut grouped) = self.group();
        grouped.expr_with_precedence(left, precedence);
        grouped.push(Node::LineOrSpace);

        let mut first = true;
        let mut indented = grouped.indent();
        while let Some((op, right)) = ops.pop() {
            let symbol = match &**op {
                ast::BinOp::And => "&&",
                ast::BinOp::Or => "||",
                ast::BinOp::Eq => "==",
                ast::BinOp::Ne => "!=",
                ast::BinOp::Lt => "<",
                ast::BinOp::Le => "<=",
                ast::BinOp::Gt => ">",
                ast::BinOp::Ge => ">=",
                ast::BinOp::Add => "+",
                ast::BinOp::Sub => "-",
                ast::BinOp::Mul => "*",
                ast::BinOp::Div => "/",
                ast::BinOp::Mod => "%",
            };

            let pos = indented.spans.get(op).start;

            let whitespace = indented.pop_whitespace(pos, true, true, false);
            if !first && !whitespace {
                indented.push(Node::LineOrSpace);
            }

            indented.push(Node::Ascii(symbol));
            indented.push(Node::Ascii(" "));
            indented.expr(right);
            first = false;
        }
    }

    fn literal(&mut self, lit: &Meta<ast::Literal>) {
        let span = self.spans.get(lit);
        self.pop_whitespace(span.end, true, true, false);

        match &lit.node {
            ast::Literal::String(_)
            | ast::Literal::Char(_)
            | ast::Literal::Asn(_)
            | ast::Literal::IpAddress(_)
            | ast::Literal::Integer(_, _)
            | ast::Literal::Float(_, _) => {
                let span = self.spans.get(lit);
                let y = self.span_content(span);
                self.push(Node::Ascii(y));
            }
            ast::Literal::Bool(true) => self.push(Node::Ascii("true")),
            ast::Literal::Bool(false) => self.push(Node::Ascii("false")),
            ast::Literal::Unit => self.push(Node::Ascii("()")),
        }
    }

    fn record(&mut self, x: &Meta<ast::Record>) {
        let span = self.spans.get(x);
        self.separated(
            "{",
            "}",
            ",",
            &x.fields,
            span,
            WrapStyle::Spaced,
            |this, field| {
                this.push(Node::ident(*field.0));
                this.push(Node::Ascii(": "));
                this.expr(&field.1)
            },
        );
    }

    fn r#match(&mut self, x: &ast::Match) {
        self.push(Node::Ascii("match "));
        self.expr(&x.expr);
        self.push(Node::Ascii(" "));

        let span = self.spans.get(&x.arms);
        self.separated(
            "{",
            "}",
            "",
            &x.arms,
            span,
            WrapStyle::Wrap,
            |this, arm| {
                match &*arm.pattern {
                    ast::Pattern::Underscore => this.push(Node::Ascii("_")),
                    ast::Pattern::EnumVariant { variant, fields } => {
                        this.push(Node::ident(**variant));
                        if let Some(fields) = fields {
                            let span = self.spans.get(fields);
                            this.separated(
                                "(",
                                ")",
                                ",",
                                &fields.node,
                                span,
                                WrapStyle::Tight,
                                |this, field| {
                                    this.push(Node::ident(**field));
                                },
                            );
                        }
                    }
                }
                if let Some(guard) = &arm.guard {
                    this.push(Node::Ascii(" if "));
                    this.expr(guard);
                }
                this.push(Node::Ascii(" => "));
                this.expr(&arm.body);
                if !matches!(&*arm.body, ast::Expr::Block(_)) {
                    this.push(Node::Ascii(","))
                }
            },
        );
    }

    fn type_expr(&mut self, ty: &Meta<ast::TypeExpr>) {
        match &**ty {
            ast::TypeExpr::Option(ty) => {
                self.type_expr(ty);
                self.push(Node::Ascii("?"));
            }
            ast::TypeExpr::Path(p, args) => {
                self.path(p, true);
                if let Some(args) = args {
                    let span = self.spans.get(args);
                    self.separated(
                        "[",
                        "]",
                        ",",
                        args,
                        span,
                        WrapStyle::Tight,
                        |this, ty| {
                            this.type_expr(ty);
                        },
                    );
                }
            }
            ast::TypeExpr::Never => self.push(Node::Ascii("!")),
            ast::TypeExpr::Unit => self.push(Node::Ascii("()")),
            ast::TypeExpr::Record(record_type) => {
                self.record_type(record_type, WrapStyle::Tight);
            }
        }
    }

    fn record_type(&mut self, ty: &ast::RecordType, style: WrapStyle) {
        let span = self.spans.get(&ty.fields);
        self.separated(
            "{",
            "}",
            ",",
            &ty.fields,
            span,
            style,
            |this, field| {
                let (name, ty) = field;
                this.push(Node::ident(**name));
                this.push(Node::Ascii(": "));
                this.type_expr(ty);
            },
        );
    }

    fn path(&mut self, p: &Meta<ast::Path>, group: bool) {
        if group {
            let (_, mut grouped) = self.group();

            let ident = &p.idents[0];
            grouped.push(Node::ident(**ident));

            let mut indented = grouped.indent();

            for ident in &p.idents[1..] {
                indented.push(Node::Line);
                let pos = indented.spans.get(ident).end;
                indented.pop_whitespace(pos, false, false, false);
                indented.push(Node::Ascii("."));
                indented.push(Node::ident(**ident));
            }
        } else {
            let ident = &p.idents[0];
            self.push(Node::ident(**ident));

            for ident in &p.idents[1..] {
                self.push(Node::Line);
                let pos = self.spans.get(ident).end;
                self.pop_whitespace(pos, false, false, false);
                self.push(Node::Ascii("."));
                self.push(Node::ident(**ident));
            }
        }
        self.pop_trailing_comment();
    }
}

const INDENT: &str = "    ";

struct Renderer {
    buf: String,
    indent: usize,
    column: usize,
    max: usize,
    wrapped: HashSet<usize>,
}

impl Renderer {
    fn render_node(&mut self, node: &Node, wrap: bool) {
        match node {
            Node::Group(id, nodes) => {
                let must_wrap = nodes.iter().any(|n| n.must_wrap());
                let width: usize =
                    nodes.iter().map(|n| n.width(&mut self.wrapped)).sum();

                // If the column = 0 then we still have to write the indent
                // so we should check whether this group can fit after we write
                // the indent.
                let virtual_column = if self.column > 0 {
                    self.column
                } else {
                    4 * self.indent
                };

                let wrap = if must_wrap || virtual_column + width > self.max {
                    self.wrapped.insert(*id);
                    true
                } else {
                    false
                };

                for node in nodes {
                    self.render_node(node, wrap);
                }
            }
            Node::Indent(nodes) => {
                if wrap {
                    self.indent += 1;
                }

                for node in nodes {
                    self.render_node(node, wrap);
                }

                if wrap {
                    self.indent -= 1;
                }
            }
            Node::IndentNext(nodes) => {
                if wrap {
                    self.indent += 1;
                }

                for node in nodes {
                    self.render_node(node, wrap);
                }

                if wrap {
                    self.indent -= 1;
                }
            }
            Node::WrapParent => {}
            Node::Comment(s) => {
                self.comment(s);
            }
            Node::Str(size, s) => self.text(s, *size),
            Node::Ascii(s) => self.text(s, s.len()),
            Node::Line => {
                if wrap {
                    self.new_line();
                }
            }
            Node::EmptyLine => {
                self.buf.push('\n');
                self.new_line();
            }
            Node::LineOrSpace => {
                if wrap {
                    self.new_line();
                } else {
                    self.text(" ", 1)
                }
            }
            Node::IfWrap(id, a, b) => {
                if self.wrapped.contains(id) {
                    self.render_node(a, wrap);
                } else {
                    self.render_node(b, wrap);
                }
            }
        }
    }

    fn new_line(&mut self) {
        if self.column > 0 {
            self.buf.push('\n');
            self.column = 0;
        }
    }

    fn comment(&mut self, s: &str) {
        if self.column > 0 {
            self.buf.push(' ');
        } else {
            self.column = 4 * self.indent;
            for _ in 0..self.indent {
                self.buf.push_str(INDENT);
            }
        }
        self.buf.push_str(s);
        self.buf.push('\n');
        self.column = 0;
    }

    fn text(&mut self, s: &str, size: usize) {
        if self.column == 0 {
            self.column = 4 * self.indent;
            for _ in 0..self.indent {
                self.buf.push_str(INDENT);
            }
        }
        self.buf.push_str(s);
        self.column += size;
    }
}
