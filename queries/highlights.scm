(integer) @number

[
  (decimal)
  (hexadecimal)
] @float

(string) @string

[ "true" "false" ] @boolean

[ "," ";" "|" ] @punctuation.delimiter

[ "(" ")" "{" "}" "[" "]" ] @punctuation.bracket

[
  "+"
  "-"
  "/"
  "*"
  "%"
  "**"
  "**."
  "@"
  "<"
  ">"
  "<="
  ">="
  "="
  "<>"
  "->"
  "<->"
  "|->"
] @operator


[
  "type"
  "and"
  "let"
  "in"
  "match"
  "with"
  "end"
  "forall"
  "exists"
  "axiom"
  "logic"
  "theory"
  "goal"
  "rewriting"
  "of"
] @keyword

[
  "or"
  "xor"
  "not"
] @keyword.operator

[ "function" "predicate" ] @keyword.function

[ "if" "then" "else" ] @conditional

[
  "bool"
  "unit"
  "int"
  "real"
  "bitv"
  "prop"
] @type.builtin

(abstract_typedef
  name: (ident) @type.definition)

(algebraic_typedef
  name: (ident) @type.definition)

(record_typedef
  name: (ident) @type.definition)

(primitive_type) @type

(type_var) @variable

(algebraic_constructor
  cstr: (ident) @constructor
  args: (_)*)

(algebraic_label_with_type
  lbl: (ident) @field
  type: (_))

(record_label_with_type
  lbl: (ident) @field
  type: (_))

(simple_pattern
  pat: (ident) @constructor
  args: (_)*)

(function_call
  name: (ident) @function.call
  args: (_))

(function_def
  name: (named_ident) @function
  args: (_)
  ret_ty: (_)
  body: (_))

(predicate_def
  name: (named_ident) @function
  args: (_)
  body: (_))

(logic_binder
  arg: (ident) @parameter
  type: (_))

(let_binder
  var: (ident) @variable
  expr: (_))
