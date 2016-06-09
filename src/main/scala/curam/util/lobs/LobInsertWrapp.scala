package curam.util.lobs

import scala.xml.NodeSeq
import java.security.MessageDigest
import java.io.FileInputStream

case class LobInsertWrapp(table: String, parameters: Map[String, String], node: NodeSeq) {
  lazy val locator = (node \ "@locator").text
  def digest(): String = {
    locator match {
      case ""   ⇒ ""
      case path ⇒ Digest.digestFile(path)
    }
  }
  def compare(other: LobInsertWrapp) = digest() == other.digest()
}

object LobInsertWrappFact {
  def wrapp(table: NodeSeq) = {
    val tabName = (table \ "@tablename").text
    val paramNodes = (table \\ "parameter")
    val parameters = paramNodes.map { param ⇒ ((param \ "@name").text, (param \ "@value").text) }.toMap
    LobInsertWrapp(tabName, parameters, table)
  }
  def apply(table: NodeSeq): LobInsertWrapp = wrapp(table)
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
