package curam.util.oracle.sql.parser

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.StreamReader
import java.io.File

class SQLParser extends StandardTokenParsers {
  class ThisLexical extends StdLexical {
    override def whitespace: Parser[Any] = rep[Any](
      whitespaceChar
        | '/' ~ '*' ~ comment
        | '/' ~ '/' ~ rep(chrExcept(EofCh, '\n'))
        | '/' ~ '*' ~ failure("unclosed comment")
        | '-' ~ '-' ~ chrExcept(EofCh, '\n').*)
  }

  override val lexical = new ThisLexical

  lexical.reserved += ("CREATE", "TABLE", "CHAR", "CHARACTER", "NUMBER", "NUMERIC", "CONSTRAINT", "UNIQUE", "null", "not", "VARCHAR", "VARCHAR2", "DATE", "BLOB", "CLOB",
    "ALTER", "ADD", "PRIMARY", "KEY", "FOREIGN", "REFERENCES", "INDEX", "ASC", "DESC", "ON", "INTEGER", "SMALLINT", "INSERT", "INTO", "VALUES", "values")
  lexical.delimiters += ("*", "+", "-", "<", "=", "<>", "!=", "<=", ">=", ">", "/", "(", ")", ",", ".", ";")

  //  def integer: Parser[Any] = { regex("""\d+""".r) ^^ (_.toInt) }

  // Basic Types
  def charType: Parser[CharType] = ("CHAR" | "CHARACTER") ~> opt("(" ~> numericLit <~ ")") ^^ {
    case None    ⇒ CharType(1)
    case Some(i) ⇒ CharType(i.toInt)
  }
  def varcharType: Parser[VarcharType] = "VARCHAR" ~> "(" ~> numericLit <~ ")" ^^ { case i ⇒ VarcharType(i.toInt) }

  def varchar2Type: Parser[Varchar2Type] = "VARCHAR2" ~> "(" ~> numericLit <~ ")" ^^ { case i ⇒ Varchar2Type(i.toInt) }
  def characterType: Parser[CharacterType] = { charType | varcharType | varchar2Type }
  def numberType: Parser[NumberType] = ("NUMBER" | "NUMERIC") ~> opt("(" ~> numericLit ~ opt("," ~> numericLit) <~ ")") ^^ {
    case None ⇒ NumberType(38, 0)
    case Some(sc) ⇒ sc match {
      case s ~ None    ⇒ NumberType(s.toInt, 0)
      case s ~ Some(p) ⇒ NumberType(s.toInt, p.toInt)
    }
  }
  def intType: Parser[IntegerType] = "INTEGER" ^^ { i ⇒ new IntegerType {} }
  def smallIntType: Parser[SmallIntType] = "SMALLINT" ^^ { i ⇒ new SmallIntType {} }

  def numType: Parser[NumType] = { numberType | intType | smallIntType }
  def dateType: Parser[DateType] = "DATE" ^^ { d ⇒ new DateType {} }
  def dateTimeTypes: Parser[DateTimeTypes] = { dateType }
  // def longType: Parser[Any] = { ident }
  //def datetimeType: Parser[Any] = { ident }
  // def rowIdType: Parser[Any] = { i
  def lobTypes: Parser[LobType] = ("BLOB" | "CLOB") ^^ { t ⇒ LobType(t) }

  // Constraints
  def primaryKeyPart: Parser[PrimaryKey] = "PRIMARY" ~> "KEY" ~> "(" ~> repsep((ident | "KEY"), ",") <~ ")" ^^ (cols ⇒ PrimaryKey("", cols))
  def referencesClause: Parser[ReferencesClause] = "REFERENCES" ~> ident ~ ("(" ~> repsep(ident, ",") <~ ")") ^^ (x ⇒ ReferencesClause(x._1, x._2))
  def uniqueConstrPart: Parser[UniqueKeyClause] = "UNIQUE" ~> "(" ~> repsep((ident | "KEY"), ",") <~ ")" ^^ (cols ⇒ UniqueKeyClause("", cols))
  def foreignKeyPart: Parser[ForeignKey] = ("FOREIGN" ~> "KEY" ~> "(" ~> repsep(ident, ",") <~ ")") ~ referencesClause ^^ (x ⇒ ForeignKey("", x._1, x._2))

  def inlineConstraint: Parser[InlineConstraint] = opt("CONSTRAINT" ~> ident) ~ ("UNIQUE" | "null" | ("not" ~ "null")) ^^ {
    case Some(id) ~ ("not" ~ "null") ⇒ SimpleConstraint(id, "NOT NULL")
    case Some(id) ~ c                ⇒ SimpleConstraint(id, c.toString)
    case None ~ ("not" ~ "null")     ⇒ SimpleConstraint("", "NOT NULL")
    case None ~ c                    ⇒ SimpleConstraint("", c.toString)

  }
  def datatype: Parser[OracleBuiltIntDatatype] = (characterType | numType | dateTimeTypes | lobTypes)
  //~ opt(inlineConstraint) // | longType | datetimeType | lobType | rowIdType
  // They use key as identifier in BATCHGROUPTRANSLATION
  def colDef: Parser[ColumnDef] = ((ident | "KEY") ~ datatype) ~ opt(inlineConstraint) ^^ {
    case "KEY" ~ dt ~ ct ⇒ ColumnDef("KEY", dt, ct)
    case id ~ dt ~ ct    ⇒ ColumnDef(id, dt, ct)
  }
  def relationalProps: Parser[RelationalProps] = "(" ~> repsep(colDef, ",") <~ ")" ^^ (columns ⇒ RelationalProps(columns))
  // CREATE TABLE
  def createTable: Parser[CreateStmt] =
    "CREATE" ~> "TABLE" ~> ident ~ relationalProps ^^ (x ⇒ CreateStmt(x._1, x._2))
  def alterConstraint: Parser[AlterConstraint] = opt("CONSTRAINT" ~> ident) ~ (primaryKeyPart | foreignKeyPart | uniqueConstrPart) ^^ {
    case Some(id) ~ UniqueKeyClause(_, cols) ⇒ UniqueKeyClause(id, cols)
    case Some(id) ~ ForeignKey(_, cols, ref) ⇒ ForeignKey(id, cols, ref)
    case Some(id) ~ PrimaryKey(_, cols)      ⇒ PrimaryKey(id, cols)
    case None ~ UniqueKeyClause(_, cols)     ⇒ UniqueKeyClause("", cols)
    case None ~ ForeignKey(_, cols, ref)     ⇒ ForeignKey("", cols, ref)
    case None ~ PrimaryKey(_, cols)          ⇒ PrimaryKey("", cols)
  }
  // CREATE INDEX
  def tabIndxClause: Parser[CreateIndexStmt] = ident ~ ("(" ~> repsep(ident ~ opt("ASC" | "DESC"), ",") <~ ")") ^^ { x ⇒
    val cols = x._2.map {
      case id ~ Some(ord) ⇒ ColIndex(id, if (ord == "ASC") true else false)
      case id ~ None      ⇒ ColIndex(id, true)
    }
    CreateIndexStmt("", x._1, false, cols)
  }
  def createIndex: Parser[CreateIndexStmt] = ("CREATE" ~> opt("UNIQUE")) ~ ("INDEX" ~> ident) ~ ("ON" ~> tabIndxClause) ^^ {
    case Some(u) ~ id ~ idx ⇒ CreateIndexStmt(id, idx.table, true, idx.cols)
    case None ~ id ~ idx    ⇒ CreateIndexStmt(id, idx.table, false, idx.cols)
  }
  // ALTER TABLE
  def alterTable: Parser[AlterTableStmt] = ("ALTER" ~> "TABLE" ~> ident <~ "ADD") ~ alterConstraint ^^ (x ⇒ AlterTableStmt(x._1, x._2))

  // def number:  Parser[DummyStatement] = rep(chr
  def simpleExpr: Parser[DummyStatement] = (stringLit | (opt("-") ~ numericLit) | "null") ^^ (x ⇒ new DummyStatement {})
  def insertIntoClause: Parser[DummyStatement] = ("INSERT" ~> "INTO" ~> ident ~ "(" ~> repsep(ident, ",") <~ ")" <~ ("VALUES" | "values") ~ "(" ~> repsep(simpleExpr, ",") <~ ")") ^^ (x ⇒ new DummyStatement {})
  def statement: Parser[Statement] = createTable | alterTable | createIndex | insertIntoClause
  def statements: Parser[Seq[Statement]] = rep(statement <~ ";") ^^ (sts ⇒ sts)
  def parse(sql: String): Option[Seq[Statement]] = {
    val upperString = sql.toUpperCase
    phrase(statements)(new lexical.Scanner(upperString)) match {
      case Success(r, q) ⇒ Option(r)
      case x             ⇒ println(x); None
    }
  }
  def parse(file: File): Option[Seq[Statement]] = {
    val input = StreamReader(new java.io.FileReader(file))
    phrase(statements)(new lexical.Scanner(input)) match {
      case Success(r, q) ⇒ Option(r)
      case x             ⇒ println(x); None
    }
  }
}
