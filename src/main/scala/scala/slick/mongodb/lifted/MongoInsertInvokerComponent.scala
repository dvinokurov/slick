package scala.slick.mongodb.lifted

import com.mongodb.{DBObject, WriteResult}
import com.mongodb.casbah.BulkWriteResult

import scala.slick.mongodb.direct.TypedMongoCollection
import scala.slick.profile.BasicInsertInvokerComponent

trait MongoInsertInvokerComponent extends BasicInsertInvokerComponent{ driver: MongoDriver =>
  override type InsertInvoker[T] = InsertInvokerDef[T]

  /** Create an InsertInvoker -- this method should be implemented by drivers as needed */
  override def createInsertInvoker[T](compiled: CompiledInsert): InsertInvoker[T] = ???
  override type CompiledInsert = Any

  class InsertInvokerDef[T](val mongoCollection: TypedMongoCollection[T]) extends super.InsertInvokerDef[T] {

    // TODO: should we use Unit as a write result instead of Mongo driver results?
    override type SingleInsertResult = WriteResult
    override type MultiInsertResult = BulkWriteResult

    /** Insert a single value */
    def +=(value: T)(implicit session: Backend#Session, binder: T=>DBObject): SingleInsertResult = {mongoCollection.insert(binder(value))}
    /** Insert a single value */
    override def +=(value: T)(implicit session: Backend#Session): SingleInsertResult =
      throw new UnsupportedOperationException("MongoDB += operator requires implicit binder parameter of type T=>DBObject")

    /** Insert a collection of values */
    def ++=(values: Iterable[T])(implicit session: Backend#Session, binder: T=>DBObject): MultiInsertResult = {
      val builder = mongoCollection.initializeOrderedBulkOperation
      for{document <- values} builder.insert(binder(document))
      builder.execute()
    }
    /** Insert a collection of values */
    override def ++=(values: Iterable[T])(implicit session: Backend#Session): MultiInsertResult =
      throw new UnsupportedOperationException("MongoDB ++= operator requires implicit binder parameter of type T=>DBObject")
  }
}
