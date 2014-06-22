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
// TODO: remove session as a required implicit parameter. It's required for Invoker component, not Query
class MongoQuery[-P,R](val collectionName:String,val queryString: Option[String])(implicit session: MongoBackend#Session, converter: GetResult[R]) extends (P => MongoInvoker[R]){
  lazy val mongoCollection = session.collectionByName(collectionName)

  private def interpolatedQuery(queryParameters: P): Option[String] = queryString.map(MongoQueryInterpolator.interpolate[P](_,queryParameters))

  private def parsedQuery(queryParameters: P) = interpolatedQuery(queryParameters).map(JSON.parse(_).asInstanceOf[DBObject])

  override def apply(queryParameters: P): MongoInvoker[R] = {
    val typedMonoCollection = new TypedMongoCollection[R](mongoCollection,converter)
    new MongoInvoker[R](typedMonoCollection,parsedQuery(queryParameters))
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

  implicit class MongoInterpolation(val s: StringContext) extends AnyVal{
    def mq[P](collectionName: String)(param: P) = new MongoInterpolationResult[P](collectionName,s.parts,param)
  }
}

class MongoInterpolationResult[P](val collectionName:String,val parts: Seq[String],val param: P){
  def as[R](implicit converter: GetResult[R], session: MongoBackend#Session): MongoQuery[Unit,R] =
    MongoQuery[P,R](collectionName,MongoQueryInterpolator.interpolate(parts.mkString("?"),param))
}

object MongoQueryInterpolator{
  val NO_QUOTES = 0
  val IN_SINGLE_QUOTE = 1
  val IN_DOUBLE_QUOTE = 2

  // TODO: refactor in order to avoid iterating through all the query by a single character
  def interpolate[P](query: String, parameters: P): String = {
    class ParametersKeeper(val parameters: P) extends Iterable[Any] {
      override def iterator: Iterator[Any] = parameters match {
        case iterable:Iterable[Any] => iterable.iterator
        case product: Product => List.range(0, product.productArity).map(product.productElement).iterator
        case a: Any => List(a).iterator //TODO: unnecessary list creation
      }
    }

    val params = new ParametersKeeper(parameters).iterator
    val sb = new StringBuilder
    var state = NO_QUOTES

    val tokensIterator = query.iterator
    while(tokensIterator.hasNext){
      val char = tokensIterator.next()

      if(char=='\'' && state==NO_QUOTES) state=IN_SINGLE_QUOTE
      else if(char=='\'' && state==IN_SINGLE_QUOTE) state=NO_QUOTES
      else if(char=='"' && state==NO_QUOTES) state=IN_DOUBLE_QUOTE
      else if(char=='"' && state==IN_DOUBLE_QUOTE) state=NO_QUOTES

      if(char=='?' && state==NO_QUOTES){
        if(params.hasNext) {
          val p = params.next()
          if(p.isInstanceOf[Number])sb.append(p)
          else sb.append("\"").append(p).append("\"")
        }
        else throw new IllegalStateException("Number of parameters is less than required by query")
      }else{
        sb.append(char)
      }
    }

    sb.toString()
  }
}
