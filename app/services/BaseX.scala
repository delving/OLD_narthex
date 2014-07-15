package services

import java.io.{ByteArrayInputStream, File}

import org.basex.BaseXServer
import org.basex.core.cmd._
import org.basex.server.ClientSession

/**
 * Minimal connection with BaseX, wrapping the Java API
 *
 * note: sessions are not reused yet
 *
 * @author Gerald de Jong <gerald@delving.eu>
 */

class BaseX(host: String, port: Int, eport: Int, user: String, pass: String) {

  private var server: BaseXServer = null

  def startServer(dataDirectory: Option[File]) {
    dataDirectory match {
      case Some(dir) =>
        if (!dir.exists()) {
          if (!dir.mkdirs()) {
            throw new RuntimeException("Failed to create data directory for BaseX " + dataDirectory)
          }
        }
        System.setProperty("org.basex.path", dir.getAbsolutePath)
      case None =>
    }
    server = new BaseXServer(s"-e$eport", s"-p$port")
  }

  def stopServer() {
    BaseXServer.stop(port, eport)
  }

  def withSession[T](block: ClientSession => T): T = {
    val session = new ClientSession(host, port, user, pass)
    try {
      block(session)
    }
    finally {
      session.close()
    }
  }

  def createDatabase(name: String) =
    withSession(_.execute(new CreateDB(name)))

  def checkDatabase(name: String) =
    withSession(_.execute(new Check(name)))

  def dropDatabase(name: String) =
    withSession(_.execute(new DropDB(name)))

  def withDbSession[T](database: String)(block: ClientSession => T): T = {
    withSession {
      session =>
        session.execute(new Open(database))
        block(session)
    }
  }

  def add(database: String, path: String, document: String) =
    withDbSession(database)(_.add(path, new ByteArrayInputStream(document.getBytes("utf-8"))))

  def replace(database: String, path: String, document: String) =
    withDbSession(database)(_.replace(path, new ByteArrayInputStream(document.getBytes("utf-8"))))

  def rename(database: String, path: String, newPath: String) =
    withDbSession(database)(_.execute(new Rename(path, newPath)))

  def delete(database: String, path: String) =
    withDbSession(database)(_.execute(new Delete(path)))

  def query(database: String, query: String) =
    withDbSession(database){
      session =>
        val q = session.query(query)
        q.execute()
    }
}
