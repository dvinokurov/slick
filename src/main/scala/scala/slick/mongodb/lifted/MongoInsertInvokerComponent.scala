package scala.slick.mongodb.lifted


import com.mongodb.casbah.Imports._
import com.mongodb.WriteResult
import com.mongodb.casbah.BulkWriteResult
import com.mongodb.casbah.commons.MongoDBObject

import scala.annotation.implicitNotFound
import scala.slick.ast.{Node, TableExpansion, TableNode}
import scala.slick.mongodb.direct.{GetResult, TypedMongoCollection}
import scala.slick.profile.BasicInsertInvokerComponent

import scala.language.implicitConversions

trait MongoInsertInvokerComponent extends BasicInsertInvokerComponent{ driver: MongoDriver =>
  override type InsertInvoker[T] = InsertInvokerDef[T]

  /** Create an InsertInvoker -- this method should be implemented by drivers as needed */
  override def createInsertInvoker[T](compiled: CompiledInsert): InsertInvoker[T] = new InsertInvokerDef[T](compiled)
  override type CompiledInsert = Node     // TODO: change to MongoNode when moving from SQL node tree to mongo tree

  // TODO: use mongo-specific nodes, add support for nested document structure
  @implicitNotFound("Implicit converter of type ${TU}=>DBObject required for MongoDB InsertInvoker")
  class InsertInvokerDef[TU](node: CompiledInsert) extends super.InsertInvokerDef[TU] {
    /** Used to convert data from DBObject to specified type after find operation - required for TypedMongoCollection creation */
    val converter: GetResult[Product] = ???
    /** Used to convert specified type to DBObject */
    val binder: TU => MongoDBObject = ???

    // TODO: generify, use mongo-specific nodes
    private def collection(session: Backend#Session): TypedMongoCollection[Product] = node match{
      case te: TableExpansion =>
        val collectionName = te.table.asInstanceOf[TableNode].tableName
        new TypedMongoCollection[Product](collectionName)(session,converter)
      case _ => throw new IllegalArgumentException("Only nodes of type TableExpansion supported")
    }

    // TODO: should we use Unit as a write result instead of Mongo driver results?
    override type SingleInsertResult = WriteResult
    override type MultiInsertResult = BulkWriteResult

    /** Insert a single value */
    override def +=(value: TU)(implicit session: Backend#Session): SingleInsertResult = collection(session).insert(binder(value))
    /** Insert a collection of values */
    override def ++=(values: Iterable[TU])(implicit session: Backend#Session): MultiInsertResult = {
      val builder = collection(session).initializeOrderedBulkOperation
      for {document <- values} builder.insert(binder(document))
      builder.execute()
    }
  }
}
