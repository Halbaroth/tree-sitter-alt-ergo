function sep2(separator, rule) {
  return seq(rule, separator, rule, repeat(seq(separator, rule)))
}

function sep1(separator, rule) {
  return seq(rule, repeat(seq(separator, rule)))
}

function sep(separator, rule) {
  return optional(sep1(separator, rule))
}

const PREC = {
  fun_and: 14,
  cut: 13,
  check: 13,
  named: 12,
  not: 11,
  uminus: 10,
  pow: 9,
  mult: 8,
  add: 7,
  rel: 6,
  ite: 5,
  logic: 4,
  exists: 2,
  forall: 2,
  let: 1
}

module.exports = grammar({
  name: 'alt_ergo',

  extras: $ => [
    /\s/,
    $.comment
  ],

  externals: $ => [
    $.comment
  ],

  rules : {
    source_file: $ => repeat(choice(
      $._decl
    )),

    string: $ => /[a-zA-Z0-9_]*/,

    integer: $ => /[0-9]+/,

    decimal: $ => choice(/[0-9]+\.[0-9]*/,/[0-9]*\.[0-9]+/),

    hex_exp: $ => /[pP][-+]?[0-9]+/,

    hexadecimal: $ => seq('0x', /[0-9a-fA-F]+\.[0-9a-fA-F]*/, $.hex_exp),

    // Identifiers

    ident: $ => /[a-zA-Z_][a-zA-Z0-9_?'\\]*/,

    // Binders

    logic_binder: $ => seq(
      field('arg', $.ident),
      ':',
      field('ty', $.primitive_type)
    ),

    multi_logic_binder: $ => seq(sep1(',', $.ident), ':', $.primitive_type),

    label_expr: $ => seq($.ident, '=', $._expr),

    _array_assignment: $ => seq($._expr, '<-', $._expr),

    function_call: $ => seq(
      field('name', $.ident),
      '(',
      field('args', sep(',', $._expr)),
      ')'
    ),

    _simple_expr: $ => choice(
      $.ident,
      seq('(', $._expr, ')'),
      $.integer,
      $.decimal,
      $.hexadecimal,
      'true',
      'false',
      'void',
      seq('{', sep1(';', $.label_expr), '}'),
      seq('{', $._simple_expr, 'with',  sep1(';', $.label_expr),'}'),
      seq($._simple_expr, '.', $.ident),
      $.function_call,
      seq($._simple_expr, '[', $._expr, ']'),
      seq($._simple_expr, '[', sep1(',', $._array_assignment), ']'),
      seq($._simple_expr, ':', $.primitive_type),
      seq($._simple_expr, '?', $.ident),
      seq($._simple_expr, '#', $.ident)
    ),

    simple_pattern: $ => seq(
      field('pat', $.ident),
      field('args', optional(seq('(', sep1(',', $.ident), ')')))
    ),

    let_binder: $ => seq(
      field('var', $.ident),
      '=',
      field('expr', $._expr)
    ),

    match_case: $ => seq($.simple_pattern, '->', $._expr),

    match_cases: $ => seq(
      optional('|'),
      sep1('|', $.match_case)
    ),

    expr_or_dom: $ => choice(
      $._expr,
      seq($._expr, 'in', /[\[\]]/, $.bound, ',', $.bound, /[\[\]]/),
      seq($.ident, '|->', $._expr)
    ),

    trigger: $ => sep1(',', $.expr_or_dom),

    triggers: $ => seq('[', sep1('|', $.trigger), ']'),

    filters: $ => seq('{', sep1(',', $._expr), '}'),

    pow_operator: $ => choice('**', '**.', '@'),

    mult_operator: $ => choice('*', '%', '/'),

    add_operator: $ => choice('+', '-'),

    rel_operator: $ => choice('<', '>', '<=', '>=', '=', '<>'),

    logic_operator: $ => choice('or', 'and', 'xor', '<->', '->'),

    ite_expr: $ => prec(PREC.ite, seq(
      'if',
      field('if', $._expr),
      'then',
      field('then', $._expr),
      'else',
      field('else', $._expr)
    )),

    let_expr: $ => prec(PREC.let, seq(
      'let',
      field('binder', $.let_binder),
      'in',
      field('expr', $._expr)
    )),

    match_expr: $ => seq('match', $._expr, 'with', $.match_cases, 'end'),

    forall_expr: $ => prec(PREC.forall, seq(
      'forall',
      field('logic_binder', sep1(',', $.multi_logic_binder)),
      optional(field('triggers', $.triggers)),
      optional(field('filters', $.filters)),
      '.',
      field('body', $._expr)
    )),

    exists_expr: $ => prec(PREC.exists, seq(
      'exists',
      field('logic_binder', sep1(',', $.multi_logic_binder)),
      field('triggers', optional($.triggers)),
      field('filters', optional($.filters)),
      '.',
      field('body', $._expr)
    )),

    check_expr: $ => prec(PREC.check, seq(
      'check', $._expr
    )),

    cut_expr: $ => prec(PREC.cut, seq(
      'cut', $._expr
    )),

    _infix_operator: $ => choice(
      $.pow_operator,
      $.mult_operator,
      $.add_operator,
      $.rel_operator,
      $.logic_operator,
    ),

    infix_expr: $ => {
      const table = [
        {
          operator: $.pow_operator,
          precedence: PREC.pow,
          associativity: 'right'
        },
        {
          operator: $.mult_operator,
          precedence: PREC.mult,
          associativity: 'left'
        },
        {
          operator: $.add_operator,
          precedence: PREC.add,
          associativity: 'left'
        },
        {
          operator: $.rel_operator,
          precedence: PREC.rel,
          associativity: 'left'
        },
        {
          operator: $.logic_operator,
          precedence: PREC.logic,
          associativity: 'right'
        },
      ]

      return choice(...table.map(
        ({operator, precedence, associativity}) =>
          prec[associativity](precedence, seq(
            field('left', $._expr),
            field('op', operator),
            field('right', $._expr)
          ))
      ))
    },

    _expr: $ => choice(
      $._simple_expr,
      prec(PREC.not, seq('not', $._expr)),
      prec(PREC.uminus, seq('-', $._expr)),
      seq('[|', $.integer, '|]'),
      seq($._expr, '^{', $.integer, ',', $.integer ,'}'),
      seq('distinct', '(', sep2(',', $._expr), ')'),
      $.infix_expr,
      $.ite_expr,
      $.let_expr,
      $.match_expr,
      $.forall_expr,
      $.exists_expr,
    ),

    bound: $ => choice(
      '?',
      seq(optional('?'), $.ident),
      seq(optional('-'), choice($.integer, $.decimal, $.hexadecimal)),
    ),

    // Type variables

    type_var: $ => seq('\'', $.ident),

    type_vars: $ => choice(
      $.type_var,
      seq('(', sep1(',', $.type_var), ')')
    ),

    // Type definitions

    primitive_type: $ => choice(
      'bool',
      'unit',
      'int',
      'real',
      seq(token('bitv'), '[', /[0-9]+/, ']'),
      $.ident,
      $.type_var,
      seq($.primitive_type, field('cstr', $.ident)),
      seq('(', sep1(',', $.primitive_type), ')', $.ident)
    ),

    _primitive_type_or_prop: $ => choice(
      $.primitive_type,
      'prop'
    ),

    _logic_type: $ => choice(
      $._primitive_type_or_prop,
      seq(sep(',', $.primitive_type), '->', $._primitive_type_or_prop)
    ),

    record_label_with_type: $ => seq(
      field('lbl', $.ident),
      ':',
      field('ty', $.primitive_type)
    ),

    record_type: $ => seq(
      '{', sep1(';', $.record_label_with_type), '}'
    ),

    algebraic_label_with_type: $ => seq(
      field('lbl', $.ident),
      ':',
      field('ty', $.primitive_type)
    ),

    algebraic_args: $ => seq(
      'of', '{', sep1(';', $.algebraic_label_with_type), '}'
    ),

    algebraic_cstr: $ => seq(
      field('cstr', $.ident),
      field('args', optional($.algebraic_args))
    ),

    abstract_typedef: $ => seq(
      $.type_vars,
      field('name', $.ident)
    ),

    algebraic_typedef: $ => seq(
      optional($.type_vars),
      field('name', $.ident),
      '=',
      sep1('|', $.algebraic_cstr)
    ),

    algebraic_typedefs: $ =>
      sep1(token('and'), $.algebraic_typedef),

    record_typedef: $ => seq(
      $.type_vars,
      field('name', $.ident),
      '=',
      $.record_type
    ),

    // Top level declarations

    theory_elt: $ => choice(
      seq('axiom', $.ident, ':', $._expr),
      seq('case_split', $.ident, ':', $._expr)
    ),

    rewriting_list: $ => choice(
      $._expr,
      seq($._expr, ';'),
      seq($._expr, ';', $.rewriting_list)
    ),

    theory_decl: $ => seq(
      'theory',
      field('name', $.ident),
      'extends',
      $.ident, '=', repeat($.theory_elt),
      'end'
    ),

    _type_decl: $ => seq(
      'type',
      choice(
        $.abstract_typedef,
        $.record_typedef,
        $.algebraic_typedefs
      )
    ),

    logic_decl: $ => seq(
      'logic',
      optional('ac'),
      sep1(',', $.ident),
      ':',
      field('ty', $._logic_type)
    ),

    axiom_decl: $ => seq(
      'axiom',
      field('name', $.ident),
      ':',
      $._expr
    ),

    rewriting_decl: $ => seq(
      'rewriting',
      field('name', $.ident),
      ':',
      $.rewriting_list
    ),

    check_sat_decl: $ => seq(
      'check_sat',
      field('name', $.ident),
      ':',
      $._expr
    ),

    goal_decl: $ => seq(
      'goal',
      field('name', $.ident),
      ':',
      $._expr
    ),

    function_def: $ => seq(
      'function',
      field('name', $.ident),
      '(',
      field('args', sep(',', $.logic_binder)),
      ')',
      ':',
      field('ret_ty', $.primitive_type),
      '=',
      field('body', $._expr)
    ),

    predicate_def: $ => seq(
      'predicate',
      field('name', $.ident),
      optional(seq(
        '(',
        field('args', sep(',', $.logic_binder)),
        ')'
      )),
      '=',
      field('body', $._expr)
    ),

    _function_or_predicate_def: $ => choice(
      $.function_def,
      $.predicate_def
    ),

    _decl: $ => choice(
      $.theory_decl,
      $._type_decl,
      $.logic_decl,
      sep1(token(prec(PREC.fun_and, 'and')), $._function_or_predicate_def),
      $.axiom_decl,
      $.rewriting_decl,
      $.check_sat_decl,
      $.goal_decl,
    ),
  }
});
