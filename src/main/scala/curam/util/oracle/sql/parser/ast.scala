package curam.util.oracle.sql.parser

trait Node extends PrettyPrinters {

  def emitsql: String
}

trait OracleBuiltIntDatatype
trait CharacterType extends OracleBuiltIntDatatype
trait Statement
trait InlineConstraint
trait AlterConstraint

case class CharType(len: Int) extends CharacterType
case class VarcharType(len: Int) extends CharacterType
case class Varchar2Type(len: Int) extends CharacterType

trait NumType extends OracleBuiltIntDatatype
case class NumberType(scale: Int, precision: Int) extends NumType
trait IntegerType extends NumType
trait SmallIntType extends NumType

trait DateTimeTypes extends OracleBuiltIntDatatype

trait DateType extends DateTimeTypes

case class LobType(dtype: String) extends OracleBuiltIntDatatype
case class SimpleConstraint(ident: String, constraint: String) extends InlineConstraint

case class ColumnDef(column: String, datatype: OracleBuiltIntDatatype, constr: Option[InlineConstraint])
case class RelationalProps(props: Seq[ColumnDef])
case class CreateStmt(table: String, props: RelationalProps) extends Statement

case class PrimaryKey(constraint: String, columns: Seq[String]) extends AlterConstraint
case class UniqueKeyClause(constraint: String, columns: Seq[String]) extends AlterConstraint
case class ReferencesClause(obj: String, columns: Seq[String])
case class ForeignKey(constraint: String, columns: Seq[String], reference: ReferencesClause) extends AlterConstraint
case class AlterTableStmt(table: String, const: AlterConstraint) extends Statement
trait DummyStatement extends Statement

// Create Index
case class ColIndex(col: String, asc: Boolean)
case class CreateIndexStmt(id: String, table: String, unique: Boolean, cols: Seq[ColIndex]) extends Statement

object Statement {
  // Selects statements by type
  def filter[T](stmts: Seq[Statement])(implicit m: Manifest[T]) = { stmts.filter { x ⇒ m.erasure.isInstance(x) } map { x ⇒ x.asInstanceOf[T] } }
  // Find CreateStmt by table id
  def findTable(stmts: Seq[CreateStmt], table: String) = stmts.find { ct ⇒ ct.table == table }
}

object Comparator {
  def findNewTables(current: Seq[CreateStmt], target: Seq[CreateStmt]) = for {
    t ← target
    if (Statement.findTable(current, t.table) == None)
  } yield t
}
