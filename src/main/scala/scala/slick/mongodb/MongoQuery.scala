package scala.slick.mongodb

import scala.language.implicitConversions
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.util.JSON


// TODO: finish documentation here
/**
 *
 * MongoQuery points to single collection in a MongoDB
 *
 * @param collectionName Name of the collection in the MongoDB to fetch data from
 * @param queryString Represents the query that is used to fetch data
 * @param converter Mapper from MongoDbObject to Scala object
 * @param session Implicit parameter used for connecting to the database
 * @tparam R Result type of the query
 */
// TODO: see if we can refactor to simplify usage of converter here - probably we can use Salat to perform conversions automatically
// TODO: check if we can make R covariant
class MongoQuery[-P,R](val collectionName:String,val queryString: Option[String])(implicit session: MongoBackend#Session, converter: GetResult[R]) extends (P => MongoInvoker[R]){
  lazy val mongoCollection = session.collectionByName(collectionName)

  private def parsedQuery = queryString.map(JSON.parse(_).asInstanceOf[DBObject])

  override def apply(v1: P): MongoInvoker[R] = {
    val typedMonoCollection = new TypedMongoCollection[R](mongoCollection,converter)
    new MongoInvoker[R](typedMonoCollection,parsedQuery)
  }
}

// TODO: think how collection name may be received implicitly from result type - probably some macro required
// Example: instead of
// Q.query[Employee]("employee","{name:John}") foreach{...}
// I want to see
// Q.query[Employee]("{name:John}") foreach{...}
// however, this conflicts with
// Q.query[Employee]("employee") foreach{...}
// which is also useful when you don't want to apply any filters
object MongoQuery{

  def apply[R](collection:String)(implicit converter: GetResult[R], session: MongoBackend#Session) = query(collection)(converter,session)

  def apply[P,R](collection:String,filter:String)(implicit converter: GetResult[R], session: MongoBackend#Session) = query(collection,filter)(converter,session)

  private def query[R](collection: String)(implicit converter: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[Unit,R](collection, None)(session, converter)

  def query[P,R](collection: String, filter: String)(implicit converter: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[P,R](collection, Some(filter))(session, converter)

  @inline implicit def mongoQueryAsInvoker[R](s: MongoQuery[Unit, R]): MongoInvoker[R] = s(())

  @inline implicit def mongoQueryAsTypedMongoCollection[R](s: MongoQuery[Unit, R])(implicit converter: GetResult[R]): TypedMongoCollection[R] =
    new TypedMongoCollection[R](s.mongoCollection,converter)

  @inline implicit def mongoDBObjectAsR[R](mongoDBObject: MongoDBObject)(implicit converter: GetResult[R]):R = converter(mongoDBObject)

  @inline implicit def DBObjectAsR[R](dbObject: DBObject)(implicit converter: GetResult[R]):R = converter(new MongoDBObject(dbObject))
}
