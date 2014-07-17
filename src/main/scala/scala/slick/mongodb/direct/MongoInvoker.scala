package scala.slick.mongodb.direct

import com.mongodb.DBObject
import com.mongodb.util.JSON

import scala.slick.common.GenericInvoker

class MongoInvoker[T](val mongoCollection: TypedMongoCollection[T],val query: Option[DBObject]) extends GenericInvoker[T]{self=>
  override type Session = MongoBackend#Session

  /** Execute the statement and return the first row of the result set wrapped
    * in Some, or None if the result set is empty. */
  override def firstOption(implicit session: Session): Option[T] = query match{
    case Some(q) => mongoCollection.findOneTyped(q)
    case None => mongoCollection.findOneTyped()
  }

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly.
    * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  override def iteratorTo(maxRows: Int)(implicit session: Session): TypedMongoCursor[T] = {
    iterator.limit(maxRows)
  }

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly. */
  override def iterator(implicit session: Session): TypedMongoCursor[T] = query match{
    case Some(q) => mongoCollection.find(q)
    case None => mongoCollection.find()
  }
}
object MongoInvoker{
  def apply[P,R](collectionName: String, queryString: Option[String],queryParameters: Option[P])
                (implicit session: MongoBackend#Session, converter: GetResult[R]): MongoInvoker[R] = {
    val typedMongoCollection = new TypedMongoCollection[R](collectionName)(session,converter)
    new MongoInvoker[R](typedMongoCollection,parsedQuery(queryString,queryParameters))
  }

  import scala.slick.mongodb.direct.MongoInterpolation._
  private def interpolatedQuery[P](queryString:Option[String], queryParameters: Option[P]): Option[String] = queryParameters match{
    case Some(parameters) => queryString.map(interpolate[P](_,parameters))
    case None => queryString
  }

  private def parsedQuery[P](queryString: Option[String],queryParameters: Option[P]) =
    interpolatedQuery[P](queryString,queryParameters).map(JSON.parse(_).asInstanceOf[DBObject])

}
