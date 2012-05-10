package com.noimawesome.scalacc.parser

import util.parsing.combinator.RegexParsers


class CheckCSyntax extends RegexParsers {

  val char_const = """."""r
  val id = """[a-zA-Z]([a-zA-Z-_]*)[a-zA-Z]"""r
  val string = """.*"""r
  val numericLit = """[1-9][0-9]*"""r

  def translation_unit: Parser[Any] = external_decl | translation_unit ~ external_decl

  def external_decl: Parser[Any] = function_definition | decl

  def function_definition: Parser[Any] = ((decl_specs ~ declarator ~ decl_list ~ compound_stat)
    | (declarator ~ decl_list ~ compound_stat)
    | (decl_specs ~ declarator ~compound_stat)
    | declarator ~ compound_stat)

  def decl: Parser[Any] = decl_specs ~ init_declarator_list ~ ";" | decl_specs ~ ";"

  def decl_list: Parser[Any] = decl | decl_list ~ decl

  def decl_specs: Parser[Any] = (storage_class_spec ~ decl_specs
    | storage_class_spec
    | type_spec ~ decl_specs
    | type_spec
    | type_qualifier ~ decl_specs
    | type_qualifier)

  def storage_class_spec: Parser[String] = "auto" | "register" | "static" | "extern" | "typedef"
  
  def type_spec: Parser[Any] = "void" | "char" | "short" | "int" | "long" | "float" | "double" | "signed" | "unsigned" | struct_or_union_spec | enum_spec | typedef_name
  
  def type_qualifier: Parser[String] = "const" | "volatile"

  def struct_or_union_spec: Parser[Any] = (struct_or_union ~ id ~ "{" ~ struct_decl_list ~ "}" | struct_or_union ~ "{" ~ struct_decl_list ~ "}" | struct_or_union ~ id)

  def struct_or_union: Parser[String] = "struct" | "union"

  def struct_decl_list: Parser[Any] = (struct_decl
    | struct_decl_list ~ struct_decl)

  def init_declarator_list: Parser[Any] = (init_declarator
    | init_declarator_list ~ "," ~ init_declarator)

  def init_declarator: Parser[Any] = (declarator
    | declarator ~ "=" ~ initializer)

  def struct_decl: Parser[Any] = (spec_qualifier_list ~ struct_declarator_list ~ ";")

  def spec_qualifier_list: Parser[Any] = (type_spec ~ spec_qualifier_list
    | type_spec
    | type_qualifier ~ spec_qualifier_list
    | type_qualifier)

  def struct_declarator_list: Parser[Any] = (struct_declarator
  | struct_declarator_list ~ "," ~ struct_declarator)

  def struct_declarator: Parser[Any] = (declarator
  | declarator ~ ":" ~ const_exp
  | ":" ~ const_exp)

  def enum_spec: Parser[Any] = ("enum" ~ id ~ "{" ~ enumerator_list ~ "}"
    | "enum" ~ "{" ~ enumerator_list ~ "}"
    | "enum" ~ id)

  def enumerator_list: Parser[Any] = (enumerator
    | enumerator_list ~ "," ~ enumerator)

  def enumerator: Parser[Any] = (id
    | id ~ "=" ~ const_exp)

  def declarator: Parser[Any] = (pointer ~ direct_declarator
    | direct_declarator)

  def direct_declarator: Parser[Any] = ( id
    | "(" ~ declarator ~ ")"
    | direct_declarator ~ "[" ~ const_exp ~ "]"
    | direct_declarator ~ "[" ~ "]"
    | direct_declarator ~ "(" ~ param_type_list ~ ")"
    | direct_declarator ~ "(" ~ id_list ~ ")"
    | direct_declarator ~ "("~ ")")

  def pointer : Parser[Any] = ("*" ~ type_qualifier_list
    | "*"
    | "*" ~ type_qualifier_list ~ pointer
    | "*" ~ pointer)

  def type_qualifier_list : Parser[Any] = (type_qualifier
    | type_qualifier_list ~ type_qualifier)

  def param_type_list: Parser[Any] = param_list | param_list ~ "," ~ "..."

  def param_list: Parser[Any] = param_decl | param_list ~ "," ~ param_decl

  def param_decl: Parser[Any] = decl_specs ~ declarator | decl_specs ~ abstract_declarator | decl_specs

  def id_list: Parser[Any] = id | id_list ~ "," ~ id

  def initializer: Parser[Any] = assignment_exp | "{" ~ initializer_list ~ "}" | "{" ~ initializer_list ~ "," ~ "}"

  def initializer_list: Parser[Any] = initializer | initializer_list ~ "," ~ initializer

  def type_name: Parser[Any] = spec_qualifier_list ~ abstract_declarator | spec_qualifier_list

  def abstract_declarator: Parser[Any] = pointer | pointer ~ direct_abstract_declarator | direct_abstract_declarator

  def direct_abstract_declarator: Parser[Any] = ("(" ~ abstract_declarator ~ ")" | direct_abstract_declarator ~ "[" ~const_exp ~ "]" | "[" ~ const_exp ~ "]"
  | direct_abstract_declarator ~ "[" ~ "]"
  | "[" ~ "]"
  | direct_abstract_declarator ~ "(" ~ param_type_list ~ ")"
  | "(" ~ param_type_list ~ ")"
  | direct_abstract_declarator ~ "(" ~ ")"
  | "(" ~ ")")

  def typedef_name = id

  def stat: Parser[Any] = (labeled_stat
  | exp_stat
  | compound_stat
  | selection_stat
  | iteration_stat
  | jump_stat)

  def labeled_stat: Parser[Any] = (id ~ ":" ~ stat
    | "case" ~ const_exp ~ ":" ~ stat
    | "default" ~ ":" ~ stat)

  def exp_stat: Parser[Any] = (exp ~ ";"
    | ";")

  def compound_stat: Parser[Any] = ("{" ~ decl_list ~ stat_list ~ "}"
    | "{" ~ stat_list ~ "}"
    | "{" ~ decl_list ~ "}"
    | "{" ~ "}")

  def stat_list: Parser[Any] = stat | stat_list ~ stat

  def selection_stat = ( "if" ~ "(" ~ exp ~ ")" ~ stat
    | "if" ~ "(" ~ exp ~ ")" ~ stat ~ "else" ~ stat
    | "switch" ~ "(" ~ exp ~ ")" ~ stat)

  def iteration_stat = ("while" ~ "(" ~ exp ~ ")" ~ stat
    | "do" ~ stat ~ "while" ~ "(" ~ exp ~ ")" ~ ";"
    | "for" ~ "(" ~ exp ~ ";" ~ exp ~ ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~ exp ~ ";" ~ exp ~ ";" ~ ")" ~ stat
    | "for" ~ "(" ~ exp ~ ";" ~ ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~ exp ~ ";" ~";" ~ ")" ~ stat
    | "for" ~ "(" ~ ";" ~ exp ~ ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~ ";" ~ exp ~ ";" ~ ")" ~ stat
    | "for" ~ "(" ~ ";" ~ ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~ ";" ~ ";" ~ ")" ~ stat)

  def jump_stat: Parser[Any] = ("goto" ~ id ~ ";"
  | "continue" ~ ";"
  | "break" ~ ";"
  | "return" ~ exp ~ ";"
  | "return" ~ ";")

  def exp: Parser[Any] = (assignment_exp
  | exp ~ "," ~ assignment_exp)

  def assignment_exp: Parser[Any] = (conditional_exp
  | unary_exp ~ assignment_operator ~ assignment_exp)

  def assignment_operator = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="

  def conditional_exp: Parser[Any] = logical_or_exp | logical_or_exp ~ "?" ~ exp ~ ":" ~ conditional_exp

  def const_exp: Parser[Any] = conditional_exp

  def logical_or_exp: Parser[Any] = logical_and_exp | logical_or_exp ~ "||" ~ logical_and_exp

  def logical_and_exp: Parser[Any] = (inclusive_or_exp
    | logical_and_exp ~ "&&" ~ inclusive_or_exp)

  def inclusive_or_exp: Parser[Any] = (exclusive_or_exp
    | inclusive_or_exp ~ "|" ~ exclusive_or_exp)

  def exclusive_or_exp: Parser[Any] = (and_exp
    | exclusive_or_exp ~ "^" ~ and_exp)

  def and_exp: Parser[Any] = (equality_exp
    | and_exp ~ "&" ~ equality_exp)

  def equality_exp: Parser[Any] = (relational_exp
    | equality_exp ~ "==" ~ relational_exp
    | equality_exp ~ "!=" ~ relational_exp)

  def relational_exp: Parser[Any] = (shift_expression
    | relational_exp ~ "<" ~ shift_expression
    | relational_exp ~ ">" ~ shift_expression
    | relational_exp ~ "<=" ~ shift_expression
    | relational_exp ~ ">=" ~ shift_expression)

  def shift_expression: Parser[Any] = (additive_exp
    | shift_expression ~ "<<" ~ additive_exp
    | shift_expression ~ ">>" ~ additive_exp)

  def additive_exp: Parser[Any] = (mult_exp
    | additive_exp ~ "+" ~ mult_exp
    | additive_exp ~ "-" ~ mult_exp)

  def mult_exp: Parser[Any] = (cast_exp
    | mult_exp ~ "*" ~ cast_exp
    | mult_exp ~ "/" ~ cast_exp
    | mult_exp ~ "%" ~ cast_exp)

  def cast_exp: Parser[Any] = (unary_exp
    | "(" ~ type_name ~ ")" ~ cast_exp)

  def unary_exp: Parser[Any] = (postfix_exp
    | "++" ~ unary_exp
    | "--" ~ unary_exp
    | unary_operator ~ cast_exp
    | "sizeof" ~ unary_exp
    | "sizeof" ~ "(" ~ type_name ~ ")")

  def unary_operator: Parser[String] = "&" | "*" | "+" | "-" | "~" | "!"

  def postfix_exp: Parser[Any] = (primary_exp
    | postfix_exp ~ "["~ exp ~ "]"
    | postfix_exp ~ "(" ~ argument_exp_list ~ ")"
    | postfix_exp ~ "(" ~ ")"
    | postfix_exp ~ "." ~ id
    | postfix_exp ~ "->" ~ id
    | postfix_exp ~ "++"
    | postfix_exp ~ "--")

  def primary_exp: Parser[Any] = (id
    | const
    | string
    | "(" ~ exp ~ ")")

  def argument_exp_list: Parser[Any] = (assignment_exp
    | argument_exp_list ~ "," ~ assignment_exp)

  def const = ( numericLit
    | char_const
    );
}
