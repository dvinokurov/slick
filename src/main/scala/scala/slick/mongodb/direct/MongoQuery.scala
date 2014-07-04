package scala.slick.mongodb.direct

import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.util.JSON

import scala.language.implicitConversions
import scala.slick.mongodb.direct.MongoInterpolation._


// TODO: finish documentation here
/**
 *
 * MongoQuery points to single collection in a MongoDB
 *
 * @param collectionName Name of the collection in the MongoDB to fetch data from
 * @param queryString Represents the query that is used to fetch data
 * @tparam R Result type of the query
 */
// TODO: see if we can refactor to simplify usage of converter here - probably we can use Salat to perform conversions automatically
// TODO: check if we can make R covariant
class MongoQuery[-P,R](val collectionName:String,val queryString: Option[String]) extends ((P,MongoBackend#Session,GetResult[R]) => MongoInvoker[R]){

  private def interpolatedQuery(queryParameters: P): Option[String] = queryString.map(interpolate[P](_,queryParameters))

  private def parsedQuery(queryParameters: P) = interpolatedQuery(queryParameters).map(JSON.parse(_).asInstanceOf[DBObject])

  override def apply(queryParameters: P, session: MongoBackend#Session, converter: GetResult[R]): MongoInvoker[R] = {
    val typedMonoCollection = new TypedMongoCollection[R](collectionName)(session,converter)
    new MongoInvoker[R](typedMonoCollection,parsedQuery(queryParameters))
  }

  // Hack with swapping session and converter parameters is required to make them implicit
  def apply(queryParameters: P)(implicit converter: GetResult[R], session: MongoBackend#Session): MongoInvoker[R] =
    apply(queryParameters,session,converter)
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

  def query[R](collection: String)(implicit converter: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[Unit,R](collection, None)

  def query[P,R](collection: String, filter: String)(implicit converter: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[P,R](collection, Some(filter))

  @inline implicit def mongoQueryAsInvoker[R](s: MongoQuery[Unit, R])(implicit session: MongoBackend#Session, converter: GetResult[R]): MongoInvoker[R] = s.apply({},session,converter)

  @inline implicit def mongoQueryAsTypedMongoCollection[R](s: MongoQuery[Unit, R])(implicit session: MongoBackend#Session, converter: GetResult[R]): TypedMongoCollection[R] =
    new TypedMongoCollection[R](s.collectionName)(session,converter)

  @inline implicit def mongoDBObjectAsR[R](mongoDBObject: MongoDBObject)(implicit converter: GetResult[R]):R = converter(mongoDBObject)

  @inline implicit def DBObjectAsR[R](dbObject: DBObject)(implicit converter: GetResult[R]):R = converter(new MongoDBObject(dbObject))

  implicit class MongoInterpolation(val s: StringContext) extends AnyVal{
    def mq(parameters: Any*) = new MongoInterpolationResult(s,parameters: _*)
  }

  class MongoInterpolationResult(val s: StringContext,val parameters: Any*){
    def in[R](collectionName: String)(implicit converter: GetResult[R], session: MongoBackend#Session): MongoQuery[Unit,R] =
      MongoQuery[Seq[_], R](collectionName,s.s(new ParametersKeeper[Seq[Any]](parameters).iterator.toSeq: _*))

  }

}