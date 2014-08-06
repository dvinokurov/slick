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
  // TODO: generify: InsertInvoker -> InsertInvoker[T]
  override type InsertInvoker = InsertInvokerDef

  /** Create an InsertInvoker -- this method should be implemented by drivers as needed */
  override def createInsertInvoker[T](compiled: CompiledInsert): InsertInvoker = new InsertInvokerDef(compiled)
  override type CompiledInsert = Node     // TODO: change to MongoNode when moving from SQL node tree to mongo tree

  // TODO: use mongo-specific nodes, add support for nested document structure
  @implicitNotFound("Implicit converter of type ${T}=>DBObject required for MongoDB InsertInvoker")
  class InsertInvokerDef(val node: CompiledInsert) extends super.InsertInvokerDef[(Int,String)] {
    /** Used to convert data from DBObject to specified type after find operation - required for TypedMongoCollection creation */
    val converter: GetResult[(Int,String)] = ???
    /** Used to convert specified type to DBObject */
    val binder: (Int,String) => MongoDBObject = ???

    /** Collection requires session to be instantiated, so we have to use var+def instead of lazy val here */
    private def collection(session: Backend#Session) = cachedCollection match{
      case Some(c) => c
      case None =>
        cachedCollection = Some(newCollection(session))
        cachedCollection.get
    }
    private var cachedCollection: Option[TypedMongoCollection[(Int,String)]] = None
    // TODO: generify, use mongo-specific nodes
    private def newCollection(session: Backend#Session): TypedMongoCollection[(Int,String)] = node match{
      case te: TableExpansion =>
        val collectionName = te.table.asInstanceOf[TableNode].tableName
        new TypedMongoCollection[(Int,String)](collectionName)(session,converter)
      case _ => throw new IllegalArgumentException("Only nodes of type TableExpansion supported")
    }

    // TODO: should we use Unit as a write result instead of Mongo driver results?
    override type SingleInsertResult = WriteResult
    override type MultiInsertResult = BulkWriteResult

    /** Insert a single value */
    override def +=(value: (Int,String))(implicit session: Backend#Session): SingleInsertResult = collection(session).insert(binder(value._1,value._2))
    /** Insert a collection of values */
    override def ++=(values: Iterable[(Int,String)])(implicit session: Backend#Session): MultiInsertResult = {
      val builder = collection(session).initializeOrderedBulkOperation
      for {document <- values} builder.insert(binder(document._1,document._2))
      builder.execute()
    }
  }
}
