package curam.util.lobs

import scala.xml.NodeSeq
import java.security.MessageDigest
import java.io.FileInputStream
import scala.xml.XML
import curam.util.oracle.sql.parser.PrimaryKeyDef

case class LobInsertWrapp(table: String, parameters: Map[String, String], node: NodeSeq, replacePath: String) {
  lazy val tlocator = (node \ "@locator").text
  val locator = {
    val cpath = "/var/lib/jenkins/workspace/BuildDummyDatabaseAndPushToGit/tmp/"
    if (tlocator.startsWith(cpath)) {
      replacePath + tlocator.substring(cpath.length())
    } else
      tlocator
  }
  def digest(): String = {
    locator match {
      case ""   ⇒ ""
      case path ⇒ Digest.digestFile(path)
    }
  }
  //  def compare(other: LobInsertWrapp) = digest() == other.digest()
}

object LobInsertWrappFact {
  def wrapp(table: NodeSeq, replacePath: String) = {
    val tabName = (table \ "@tablename").text
    val paramNodes = (table \\ "parameter")
    val parameters = paramNodes.map { param ⇒ ((param \ "@name").text, (param \ "@value").text) }.toMap
    LobInsertWrapp(tabName, parameters, table, replacePath)
  }
  def apply(table: NodeSeq, replacePath: String): LobInsertWrapp = wrapp(table, replacePath)
}

object LobReadFactory {
  def load(path: String, replacePath: String): Seq[LobInsertWrapp] = {
    val rootDoc = XML.load(path)
    val tables = (rootDoc \\ "lob" \\ "table")
    tables.map { t ⇒ LobInsertWrappFact(t, replacePath) }
  }
  def apply(path: String, replacePath: String): Seq[LobInsertWrapp] = load(path, replacePath)
}

object Digest {

  def digestFile(path: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val finp = new FileInputStream(path)
    val dataBytes = Array.fill[Byte](1024)(0)
    var nread = 0
    while ({ nread = finp.read(dataBytes); nread != -1 }) {
      md.update(dataBytes, 0, nread)
    }
    finp.close
    val digb = md.digest()
    digb.map(b ⇒ (b & 0xff).toHexString) mkString ""
  }
}

object LobComparator {
  /**
   * @param source
   * @param target
   * @param pks
   * @return
   * Compare source and target lobs, PrimaryKeys are a identity need
   */
  def compare(source: Seq[LobInsertWrapp], target: Seq[LobInsertWrapp], pks: Seq[PrimaryKeyDef]) = {
    val pkMap: Map[String, PrimaryKeyDef] = pks.map { x ⇒ (x.table, x) }.toMap
      def getKeyValues(lob: LobInsertWrapp) = {
        (pkMap.get(lob.table) match {
          case Some(pk) ⇒ pk.columns.map { lob.parameters.getOrElse(_, "") }
          case None     ⇒ Seq()
        }) mkString
      }
    val nodes = for {
      t ← target
      pk = pkMap(t.table)
      found = source.find { lob ⇒ lob.table == t.table && getKeyValues(lob) == getKeyValues(t) }
      // If new or content changed
      if (found == None || found.get.digest() != t.digest())
    } yield t
    println(s"${nodes.size} LOBS Generated")
    (<root>
       <lob>
         { nodes.map { _.node } }
       </lob>
     </root>)
  }
}
