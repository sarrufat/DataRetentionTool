package curam.util.oracle.sql.parser

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.RegexParsers

class SQLParser extends StandardTokenParsers {
  class SqlLexical extends StdLexical {
    case class FloatLit(chars: String) extends Token {
      override def toString = chars
    }
    override def token: Parser[Token] =
      (identChar ~ rep(identChar | digit) ^^ { case first ~ rest ⇒ processIdent(first :: rest mkString "") }
        | rep1(digit) ~ opt('.' ~> rep(digit)) ^^ {
          case i ~ None    ⇒ NumericLit(i mkString "")
          case i ~ Some(d) ⇒ FloatLit(i.mkString("") + "." + d.mkString(""))
        }
        | '\'' ~ rep(chrExcept('\'', '\n', EofCh)) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' ⇒ StringLit(chars mkString "") }
        | '\"' ~ rep(chrExcept('\"', '\n', EofCh)) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' ⇒ StringLit(chars mkString "") }
        | EofCh ^^^ EOF
        | '\'' ~> failure("unclosed string literal")
        | '\"' ~> failure("unclosed string literal")
        | delim
        | failure("illegal character"))
  }
  lexical.reserved += ("CREATE", "TABLE", "CHAR", "NUMBER", "CONSTRAINT", "UNIQUE", "NULL", "NOT", "VARCHAR")
  lexical.delimiters += ("*", "+", "-", "<", "=", "<>", "!=", "<=", ">=", ">", "/", "(", ")", ",", ".", ";")
  //  def integer: Parser[Any] = { regex("""\d+""".r) ^^ (_.toInt) }
  def charType: Parser[Any] = "CHAR" ~> opt("(" ~> numericLit <~ ")")
  def varcharType: Parser[Any] = "VARCHAR" ~> "(" ~> numericLit <~ ")"
  def characterType: Parser[Any] = { charType | varcharType }
  def numberType: Parser[Any] = "NUMBER" ~> opt("(" ~> numericLit ~ opt("," ~> numericLit) <~ ")")
  def numType: Parser[Any] = { numberType }
  def longType: Parser[Any] = { ident }
  def datetimeType: Parser[Any] = { ident }
  def rowIdType: Parser[Any] = { ident }
  def lobType: Parser[Any] = { ident }
  def unique: Parser[Any] = "UNIQUE"
  def nullConstr: Parser[Any] = "NULL"
  def notNullConstr: Parser[Any] = "NOT" ~ "NULL"
  def inlineConstraint: Parser[Any] = opt("CONSTRAINT" ~> ident) ~ (unique | nullConstr | notNullConstr)
  def datatype: Parser[Any] = (characterType | numType) ~ opt(inlineConstraint) // | longType | datetimeType | lobType | rowIdType
  def colDef: Parser[Any] = ident ~ datatype
  def relationalProps: Parser[Any] = "(" ~> repsep(colDef, ",") <~ ")"
  def createTable: Parser[CreateStmt] =
    "CREATE" ~> "TABLE" ~> ident ~ relationalProps ^^ (x ⇒ CreateStmt(x._1, ""))
  def statement: Parser[Any] = createTable
  def statements: Parser[Any] = rep(statement ~ ";")
  def parse(sql: String): Option[Any] = {
    val upperString = sql.toUpperCase
    phrase(statements)(new lexical.Scanner(upperString)) match {
      case Success(r, q) ⇒ Option(r)
      case x             ⇒ println(x); None
    }
  }
}
