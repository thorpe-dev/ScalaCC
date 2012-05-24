package com.noimawesome.scalacc.parser

import scala.util.parsing.combinator.JavaTokenParsers


object CheckCSyntax extends JavaTokenParsers {

  def parse(s:String) = parseAll(translation_unit,s) match {
    case Success(res,_) => true
    case e:NoSuccess => {
      println(e)
      false }
  }

  // Constant or literal regex definitions go here
  val char_const = "\'.\'"

  // Parser partial functions begin here. Because C has a recursive grammar, we need to explicitly state the return type.
  // Being as vague as possible (i.e. Parser[Any]) appears to be the best approach

  def translation_unit: Parser[Any] = external_decl.+

  def external_decl = function_definition | decl

  def function_definition = (
    decl_specs ~ declarator ~ decl_list ~ compound_stat
  |              declarator ~ decl_list ~ compound_stat
  | decl_specs ~ declarator ~             compound_stat
  |              declarator ~             compound_stat )

  def decl: Parser[Any] = decl_specs ~ init_declarator_list.? ~ ";"

  def decl_list = decl.+

  def decl_specs: Parser[Any] = (
    storage_class_spec ~ decl_specs.?
  | type_spec ~ decl_specs.?
  | type_qualifier ~ decl_specs.? )

  def storage_class_spec = "auto" | "register" | "static" | "extern" | "typedef"

  def type_spec = (
      "void" | "char" | "short" | "int" | "long"
    | "float" | "double" | "signed" | "unsigned"
    | struct_or_union_spec | enum_spec | typedef_name )

  def type_qualifier = "const" | "volatile"

  def struct_or_union_spec: Parser[Any] = (
      struct_or_union ~ ident ~ "{" ~ struct_decl_list ~ "}"
    | struct_or_union ~         "{" ~ struct_decl_list ~ "}"
    | struct_or_union ~ ident )

  def struct_or_union = "struct" | "union"

  def struct_decl_list = struct_decl.+

  def init_declarator_list: Parser[Any] = (init_declarator ~ ",").* ~ init_declarator

  def init_declarator: Parser[Any] = ( declarator | declarator ~ "=" ~ initializer )

  def struct_decl: Parser[Any] = spec_qualifier_list ~ struct_declarator_list ~ ";"

  def spec_qualifier_list: Parser[Any] = (
    type_spec ~ spec_qualifier_list.?
  | type_qualifier ~ spec_qualifier_list.? )

  def struct_declarator_list: Parser[Any] = (struct_declarator ~ ",").* ~ struct_declarator

  def struct_declarator: Parser[Any] = (
    declarator
  | declarator ~ ":" ~ const_exp
  |              ":" ~ const_exp )

  def enum_spec: Parser[Any] = (
      "enum" ~ ident ~ "{" ~ enumerator_list ~ "}"
    | "enum" ~         "{" ~ enumerator_list ~ "}"
    | "enum" ~ ident )

  def enumerator_list: Parser[Any] = (enumerator ~ ",").* ~ enumerator

  def enumerator: Parser[Any] = ( ident | ident ~ "=" ~ const_exp )

  def declarator: Parser[Any] = pointer ~ direct_declarator | direct_declarator

  def direct_declarator: Parser[Any] = direct_declarator_1 ~ direct_declarator_2.*

  def direct_declarator_1 = ident | "(" ~ declarator ~ ")"

  def direct_declarator_2 = (
    "[" ~ const_exp.? ~ "]"
  | "(" ~ (param_type_list|id_list).? ~ ")" )

  def pointer : Parser[Any] = (
    "*" ~ type_qualifier_list
  | "*"
  | "*" ~ type_qualifier_list ~ pointer
  | "*" ~                       pointer )

  def type_qualifier_list : Parser[Any] = type_qualifier.+

  def param_type_list = (
    param_list
  | param_list ~ "," ~ "..." )

  def param_list = (param_decl ~ ",").* ~ param_decl

  def param_decl: Parser[Any] = (
    decl_specs ~ declarator
  | decl_specs ~ abstract_declarator
  | decl_specs )

  def id_list = (ident ~ ",").* ~ ident

  def initializer: Parser[Any] = (
    assignment_exp
  | "{" ~ initializer_list ~ "}"
  | "{" ~ initializer_list ~ "," ~ "}" )

  def initializer_list: Parser[Any] = (initializer ~ ",").* ~ initializer

  def type_name: Parser[Any] = spec_qualifier_list ~ abstract_declarator.?

  def abstract_declarator = (
    pointer
  | pointer ~ direct_abstract_declarator
  |           direct_abstract_declarator )

  def direct_abstract_declarator = direct_abstract_declarator_1 ~ direct_abstract_declarator_2.*

  def direct_abstract_declarator_1: Parser[Any] = (
    "(" ~ abstract_declarator ~ ")"
  | "[" ~ const_exp ~ "]"
  | "[" ~ "]"
  | "(" ~ param_type_list ~ ")"
  | "(" ~ ")" )

  def direct_abstract_declarator_2 = (
    "[" ~ const_exp ~ "]"
  | "[" ~ "]"
  | "(" ~ param_type_list ~ ")"
  | "(" ~ ")" )

  def typedef_name = ident

  def stat: Parser[Any] = (
    labeled_stat
  | exp_stat
  | compound_stat
  | selection_stat
  | iteration_stat
  | jump_stat)

  def labeled_stat: Parser[Any] = (
      ident ~              ":" ~ stat
    | "case" ~ const_exp ~ ":" ~ stat
    | "default" ~          ":" ~ stat)

  def exp_stat: Parser[Any] = exp ~ ";" | ";"

  def compound_stat: Parser[Any] = (
    "{" ~ decl_list ~ stat_list ~ "}"
  | "{" ~ stat_list ~             "}"
  | "{" ~             decl_list ~ "}"
  | "{" ~                         "}" )

  def stat_list: Parser[Any] = (stat.*) ~ stat

  def selection_stat = (
      "if" ~ "(" ~ exp ~ ")" ~ stat
    | "if" ~ "(" ~ exp ~ ")" ~ stat ~ "else" ~ stat
    | "switch" ~ "(" ~ exp ~ ")" ~ stat)

  def iteration_stat = (
      "while" ~ "(" ~ exp ~ ")" ~ stat
    | "do" ~ stat ~ "while" ~ "(" ~ exp ~ ")" ~ ";"
    | "for" ~ "(" ~ exp ~ ";" ~ exp ~ ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~ exp ~ ";" ~ exp ~ ";" ~       ")" ~ stat
    | "for" ~ "(" ~ exp ~ ";" ~       ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~ exp ~ ";" ~       ";" ~       ")" ~ stat
    | "for" ~ "(" ~       ";" ~ exp ~ ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~       ";" ~ exp ~ ";" ~       ")" ~ stat
    | "for" ~ "(" ~       ";" ~       ";" ~ exp ~ ")" ~ stat
    | "for" ~ "(" ~       ";" ~       ";" ~       ")" ~ stat )

  def jump_stat: Parser[Any] = (
    "goto" ~ ident ~ ";"
  | "continue" ~     ";"
  | "break" ~        ";"
  | "return" ~ exp.? ~ ";" )

  def exp: Parser[Any] = (assignment_exp ~ ",").* ~ assignment_exp

  def assignment_exp: Parser[Any] = (
    conditional_exp
  | unary_exp ~ assignment_operator ~ assignment_exp )

  def assignment_operator = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="

  def conditional_exp: Parser[Any] = logical_or_exp ~ ("?" ~ exp ~ ":" ~ conditional_exp).?

  def const_exp: Parser[Any] = conditional_exp

  def logical_or_exp: Parser[Any] = logical_and_exp ~ ("||" ~ logical_or_exp ).*

  def logical_and_exp: Parser[Any] = inclusive_or_exp ~ ("&&" ~ logical_and_exp).*

  def inclusive_or_exp: Parser[Any] = exclusive_or_exp ~ ("|" ~ inclusive_or_exp).*

  def exclusive_or_exp: Parser[Any] = and_exp ~ ("^" ~ exclusive_or_exp).*

  def and_exp: Parser[Any] = equality_exp ~ ("&" ~ and_exp).*

  def equality_exp: Parser[Any] = relational_exp ~ (( "==" | "!=") ~ equality_exp).*

  def relational_exp: Parser[Any] = shift_expression ~ (("<" | ">" | "<=" | ">=") ~ relational_exp).*

  def shift_expression: Parser[Any] = additive_exp ~ (("<<" | ">>") ~ shift_expression).*

  def additive_exp: Parser[Any] = mult_exp ~ (("+" | "-") ~ additive_exp).*

  def mult_exp: Parser[Any] = cast_exp ~ (("*" | "/" | "%") ~ mult_exp).*

  def cast_exp: Parser[Any] = (
      unary_exp
    | "(" ~ type_name ~ ")" ~ cast_exp)

  def unary_exp: Parser[Any] = (
      postfix_exp
    | "++" ~ unary_exp
    | "--" ~ unary_exp
    | unary_operator ~ cast_exp
    | "sizeof" ~ unary_exp
    | "sizeof" ~ "(" ~ type_name ~ ")")

  def unary_operator: Parser[String] = "&" | "*" | "+" | "-" | "~" | "!"

  def postfix_exp: Parser[Any] = (
      primary_exp
    | primary_exp ~ postfix_exp ~ "[" ~ exp ~ "]"
    | primary_exp ~ postfix_exp ~ "(" ~ argument_exp_list ~ ")"
    | primary_exp ~ postfix_exp ~ "(" ~                     ")"
    | primary_exp ~ postfix_exp ~ "." ~ ident
    | primary_exp ~ postfix_exp ~ "->" ~ ident
    | primary_exp ~ postfix_exp ~ "++"
    | primary_exp ~ postfix_exp ~ "--")

  def primary_exp: Parser[Any] = (
      ident
    | const
    | stringLiteral
    | "(" ~ exp ~ ")" )

  def argument_exp_list: Parser[Any] = (assignment_exp ~ ",").* ~ assignment_exp

  def const = wholeNumber | char_const | floatingPointNumber

}