/**
 * @file A statically-typed, compiled, embedded scripting language for Rust
 * @author NLnet Labs <routing-team@nlnetlabs.nl>
 * @license BSD-3-Clause
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "roto",

  extras: $ => [
    /\s/,
    $.line_comment
  ],

  word: $ => $.identifier,

  rules: {
    source_file: $ => repeat($._declaration),

    identifier: _ => /(r#)?[_\p{XID_Start}][_\p{XID_Continue}]*/,

    line_comment: $ => seq(
      '#',
      token.immediate(prec(1, /.*/)),
    ),
    
    _declaration: $ => choice(
      $.filtermap_item,
      $.function_item,
      $.record_item,
      $.test_item,
      $.import,
    ),

    import: $ => seq(
      'import',
      $.path,
      ';',
    ),

    parameter_list: $ => seq(
      '(',
      commaSep($.parameter),
      ')',
    ),

    parameter: $ => seq(
      $.identifier,
      ':',
      $.type_expr,
    ),

    filtermap_item: $ => seq(
      choice('filtermap', 'filter'),
      field('name', $.identifier),
      field('parameters', $.parameter_list),
      field('body', $.block),
    ),

    function_item: $ => seq(
      'function',
      field('name', $.identifier),
      field('parameters', $.parameter_list),
      optional(seq('->', field("return_type", $.type_expr))),
      field('body', $.block),
    ),

    test_item: $ => seq(
      'test',
      field('name', $.identifier),
      field('body', $.block),
    ),

    record_item: $ => seq(
      'type',
      field('name', $.identifier),
      field('fields', $.record_type),
    ),

    block: $ => seq(
      '{',
      repeat($._statement),
      optional($._expression),
      '}',
    ),

    _statement: $ => choice(
      $.import,
      prec(1, $.if_else_expression),
      $.let_statement,
      seq($._expression, ';'),
    ),

    let_statement: $ => seq(
      'let',
      field("variable", $.identifier),
      '=',
      $._expression,
      ';'
    ),

    path: $ => prec.left(7, seq(
      $.identifier,
      repeat(seq('.', $.identifier)),
    )),

    _expression: $ => choice(
      $.return_expression,
      $._literal,
      // $.match_expr,
      $.call_expression,
      $.access_expression,
      $.path,
      // $.record_expr,
      // $.typed_record_expr,
      // $.list_expr,
      $.unary_expression,
      $.binary_expression,
      $.if_else_expression,
      $.parentheses_expression,
    ),

    return_expression: $ => prec.left(seq(
      choice('return', 'accept', 'reject'),
      optional($._expression),
    )),

    unary_expression: $ => prec(5, seq(
      'not',
      $._expression,
    )),

    binary_expression: $ => {
      const table = [
        [1, choice('&&', '||')],
        [2, choice('==', '!=', '<', '<=', '>', '>=')],
        [3, choice('+', '-')],
        [4, choice('*', '/')],
      ];

      return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
        field('left', $._expression),
        field('operator', operator),
        field('right', $._expression),
      ))));
    },

    if_else_expression: $ => prec(2, seq(
      'if',
      $._expression,
      $.block,
      optional(seq('else', choice($.block, $.if_else_expression))),
    )),

    access_expression: $ => prec(7, seq($._expression, '.', $.identifier)),

    call_expression: $ => prec(6, seq(
      field("function", $._expression),
      '(',
      commaSep($._expression),
      ')',
    )),

    parentheses_expression: $ => seq('(', $._expression, ')'),

    _literal: $ => choice(
      $.boolean_literal,
      $.integer_literal,
      $.ipv4_literal,
      // $.ipv6_literal,
      $.string_literal,
    ),

    boolean_literal: _ => choice('true', 'false'),

    integer_literal: _ => /[0-9][0-9_]*/,

    ipv4_literal: _ => /[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*/,

    string_literal: $ => token(seq(
      '"',
      /[^\"]*/,
      '"',
    )),
    
    type_expr: $ => choice(
      $.optional_type_expr,
      $.path,
      $.never,
      $.unit,
      $.record_type,
    ),

    optional_type_expr: $ => seq($.type_expr, '?'),

    never: $ => '!',
    unit: _ => token(seq('(', ')')),
    
    record_type: $ => seq(
      '{',
      commaSep($.record_type_field),
      '}',
    ),

    record_type_field: $ => seq(
      $.identifier,
      ':',
      $.type_expr,
    ),
  }
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)))
}

function commaSep(rule) {
  return optional(commaSep1(rule))
}
