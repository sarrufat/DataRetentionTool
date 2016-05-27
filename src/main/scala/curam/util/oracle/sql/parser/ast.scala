package curam.util.oracle.sql.parser

trait Node extends PrettyPrinters {

  def emitsql: String
}

trait OracleBuiltIntDatatype {
  def emit: String
  def compare(other: OracleBuiltIntDatatype): Boolean
}
trait CharacterType extends OracleBuiltIntDatatype
trait Statement
trait InlineConstraint {
  def emit: String
  def compare(c: InlineConstraint): Boolean
}
trait AlterConstraint {
  val constraint: String
  def emit: String
  //  def compare(act: AlterConstraint): Boolean = constraint == act.constraint
}

case class CharType(len: Int) extends CharacterType {
  def emit: String = s"CHAR($len)"
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case CharType(l) ⇒ len == l
    case _           ⇒ false
  }
}
case class VarcharType(len: Int) extends CharacterType {
  def emit: String = s"VARCHAR($len)"
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case VarcharType(l) ⇒ len == l
    case _              ⇒ false
  }
}
case class Varchar2Type(len: Int) extends CharacterType {
  def emit: String = s"VARCHAR2($len)"
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case Varchar2Type(l) ⇒ len == l
    case _               ⇒ false
  }
}

trait NumType extends OracleBuiltIntDatatype
case class NumberType(scale: Int, precision: Int) extends NumType {
  def emit: String = if (precision > 0) s"NUMBER($scale, $precision)" else s"NUMBER($scale)"
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case NumberType(s, p) ⇒ s == scale && p == precision
    case _                ⇒ false
  }
}
trait IntegerType extends NumType {
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case t: IntegerType ⇒ true
    case _              ⇒ false
  }
}
trait SmallIntType extends NumType {
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case t: SmallIntType ⇒ true
    case _               ⇒ false
  }
}

trait DateTimeTypes extends OracleBuiltIntDatatype

trait DateType extends DateTimeTypes {
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case t: DateType ⇒ true
    case _           ⇒ false
  }
}

case class LobType(dtype: String) extends OracleBuiltIntDatatype {
  def emit = dtype;
  def compare(other: OracleBuiltIntDatatype): Boolean = other match {
    case LobType(t) ⇒ t == dtype
    case _          ⇒ false
  }
}
case class SimpleConstraint(ident: String, constraint: String) extends InlineConstraint {
  def emit = if ("".equals(ident)) constraint else s"CONSTRAINT $ident $constraint"
  def compare(c: InlineConstraint): Boolean = {
    c match {
      case SimpleConstraint(id, ctr) ⇒ ident == id && constraint == ctr
      case _                         ⇒ false
    }
  }
}

case class ColumnDef(column: String, datatype: OracleBuiltIntDatatype, constr: Option[InlineConstraint]) {
  def emit = {
    s"$column ${datatype.emit} " + (constr match {
      case None         ⇒ ""
      case Some(constr) ⇒ constr.emit
    })
  }
  def compareDef(other: ColumnDef) = {
    datatype.compare(other.datatype) || {
      (constr, other.constr) match {
        case (None, None)        ⇒ true
        case (Some(_), None)     ⇒ false
        case (None, Some(_))     ⇒ false
        case (Some(c), Some(oc)) ⇒ c.compare(oc)
      }
    }
  }
}
case class RelationalProps(props: Seq[ColumnDef])
case class CreateStmt(table: String, props: RelationalProps) extends Statement

case class PrimaryKey(constraint: String, columns: Seq[String]) extends AlterConstraint {
  def emit: String = s"$constraint PRIMARY KEY(" + columns.mkString(",") + ")"
}
case class UniqueKeyClause(constraint: String, columns: Seq[String]) extends AlterConstraint {
  def emit: String = s"$constraint UNIQUE(" + columns.mkString(",") + ")"
}
case class ReferencesClause(obj: String, columns: Seq[String])
case class ForeignKey(constraint: String, columns: Seq[String], reference: ReferencesClause) extends AlterConstraint {
  def emit: String = s"$constraint FOREIGN KEY(" + columns.mkString(",") + ")"
}

case class AlterTableStmt(table: String, const: AlterConstraint) extends Statement {
  def emit: String = s"ALTER TABLE $table ADD " + const.emit + ";"
}
trait DummyStatement extends Statement

case class InsertIntoStmt(table: String, properties: Seq[String], values: Seq[String] = Seq()) extends Statement

// Create Index
case class ColIndex(col: String, asc: Boolean)
case class CreateIndexStmt(id: String, table: String, unique: Boolean, cols: Seq[ColIndex]) extends Statement {
  def emit: String = {
    val colPart = (cols.map { x ⇒
      x.asc match {
        case true  ⇒ x.col
        case false ⇒ s"${x.col} DESC"
      }
    }).mkString(", ")
    unique match {
      case true  ⇒ s"CREATE UNIQUE INDEX $id ON $table ($colPart);"
      case false ⇒ s"CREATE INDEX $id ON $table ($colPart);"
    }
  }
}

object Statement {
  // Selects statements by type
  def filter[T](stmts: Seq[Statement])(implicit m: Manifest[T]) = { stmts.filter { x ⇒ m.erasure.isInstance(x) } map { x ⇒ x.asInstanceOf[T] } }
  // Find CreateStmt by table id
  def findTable(stmts: Seq[CreateStmt], table: String) = stmts.find { ct ⇒ ct.table == table }
}

object Comparator {
  trait Alter
  case class AlterNewProp(coldef: ColumnDef) extends Alter
  case class AlterDeltaProp(coldef: ColumnDef) extends Alter
  case class AlterTable(table: String, alters: Seq[Alter]) {
    assert(alters.size > 0)
    def emit: String = {
        def emitNews: String = {
          val newsAlter = alters.collect { case an: AlterNewProp ⇒ an }
          if (newsAlter.size > 0) {
            s"ALTER TABLE $table ADD (" + (newsAlter.map { na ⇒ na.coldef.emit }).mkString(",\n") + ");\n"
          } else
            ""
        }
        def emitMods: String = {
          val deltaAlter = alters.collect { case an: AlterDeltaProp ⇒ an }
          if (deltaAlter.size > 0) {
            s"ALTER TABLE $table MODIFY (" + (deltaAlter.map { na ⇒ na.coldef.emit }).mkString(",\n") + ");\n"
          } else
            ""
        }
      emitNews + emitMods
    }
  }

  def findNewTables(current: Seq[CreateStmt], target: Seq[CreateStmt]) = for {
    t ← target
    if (Statement.findTable(current, t.table) == None)
  } yield t

  private def compare(cur: CreateStmt, targ: CreateStmt): Seq[Alter] = {
    val retAlters = for {
      prop ← targ.props.props
      if (cur.props.props.find { p ⇒ p.column == prop.column } == None)
    } yield AlterNewProp(prop)
    val deltaAlters = for {
      prop ← targ.props.props
      oldProp ← cur.props.props.find { p ⇒ p.column == prop.column }
      if (!oldProp.compareDef(prop))
    } yield AlterDeltaProp(prop)
    retAlters ++ deltaAlters
  }
  def findAlterTables(current: Seq[CreateStmt], target: Seq[CreateStmt]): Seq[Comparator.AlterTable] = for {
    targ ← target
    curr ← Statement.findTable(current, targ.table)
    alters = compare(curr, targ)
    if (alters.size > 0)
  } yield AlterTable(targ.table, alters)

  def findAlterTabsDiff(current: Seq[AlterTableStmt], target: Seq[AlterTableStmt]): Seq[AlterTableStmt] = for {
    tgt ← target
    if (current.find { alter ⇒ alter.table == tgt.table && alter.const.constraint == tgt.const.constraint } == None)
  } yield tgt
  def findCreateIndexDiff(current: Seq[CreateIndexStmt], target: Seq[CreateIndexStmt]): Seq[CreateIndexStmt] = for {
    tgt ← target
    if (current.find { ctx ⇒ ctx.id == ctx.id && ctx.table == tgt.table } == None)
  } yield tgt
}
