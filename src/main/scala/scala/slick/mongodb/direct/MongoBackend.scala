package scala.slick.mongodb.direct

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClientURI
import org.slf4j.LoggerFactory

import scala.slick.backend.DatabaseComponent
import scala.slick.util.SlickLogger

trait MongoBackend extends DatabaseComponent{
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoBackend].getName+".statement"))

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef

  val Database = new DatabaseFactoryDef {}
  val backend: MongoBackend = this

  // TODO: add possibility to create DatabaseDef without url
  // In case user wants to create the connection with separate
  // parameters: username, password, etc. we don't really need to concatenate
  // them into URI and then pass it to MongoClientURI for parsing
  class DatabaseDef(val connectionUrl:String) extends super.DatabaseDef{

    override def createSession(): Session = {
      val mongoUri = MongoClientURI(connectionUrl)
      val mongoClient = MongoClient(mongoUri)
      //TODO: check if there's better way without using Option.get:
      val mongoDb = mongoClient(mongoUri.database.get)
      new Session(mongoDb)
    }

    override def withTransaction[T](f: Session => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    override def withDynTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")
  }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef{
    //TODO: add other methods and parameters here
    def forURL(url: String):DatabaseDef = new DatabaseDef(url)
  }

  /**
   * Provides access to the Mongo database.
   *
   * @param mongoDb database session points to
   */
  class SessionDef(val mongoDb: MongoDB) extends super.SessionDef{
    def collectionByName(collectionName: String): MongoCollection = mongoDb(collectionName)

    /**
     * Inherited method
     *
     * MongoDB session does nothing when closing
     * since the database connection is shared between sessions
     *
     * */
    override def close(): Unit = {}

    /**
     * Inherited method
     *
     * Transactions are not supported by MongoDB
     * */
    override def rollback(): Unit = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    /**
     * Inherited method
     *
     * Transactions are not supported by MongoDB
     * */
    override def withTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    /**
     * Inherited method
     *
     * Mongo sessions cannot be forced since MongoClient manages connections automatically
     */
    // TODO: discuss if we should throw an exception here or do nothing
    override def force(): Unit =
      throw new UnsupportedOperationException("Mongo session cannot be forced since MongoClient manages connections automatically")
  }

}

object MongoBackend extends MongoBackend {}
