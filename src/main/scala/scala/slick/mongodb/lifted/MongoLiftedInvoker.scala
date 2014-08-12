package scala.slick.mongodb.lifted

import scala.slick.ast._
import scala.slick.mongodb.direct.{MongoBackend, TypedMongoCollection, GetResult}


trait MongoLiftedInvoker[T] {
  protected def query: Node

  /** Used to retrieve attribute names from the query (CompiledInsert) */
  protected def attributeNames: Seq[String] = query match {
    case TableExpansion(_,_,pn: ProductNode) => pn.nodeChildren.map(_.asInstanceOf[Select].field.name)
    case _ => throw new IllegalArgumentException("Only nodes of type TableExpansion supported")
  }
  /** Used to convert data from DBObject to specified type after find operation - required for TypedMongoCollection creation */
  val converter: GetResult[Product] =
  // TODO: add support for arbitrary Product depending on attribute types, not only (Int,String)
    GetResult[Product](r => (r.get(attributeNames(0)).asInstanceOf[Int],r.get(attributeNames(1)).asInstanceOf[String]))


  // Collection requires session to be instantiated, so we have to use var+def instead of lazy val here for lazy initialization
  protected def collection(session: MongoBackend#Session) = cachedCollection match{
    case Some(c) => c
    case None =>
      cachedCollection = Some(newCollection(session))
      cachedCollection.get
  }
  private var cachedCollection: Option[TypedMongoCollection[Product]] = None
  // TODO: use mongo-specific nodes
  private def newCollection(session: MongoBackend#Session): TypedMongoCollection[Product] = query match{
    case te: TableExpansion =>
      val collectionName = te.table.asInstanceOf[TableNode].tableName
      new TypedMongoCollection[Product](collectionName)(session,converter)
    case _ => throw new IllegalArgumentException("Only nodes of type TableExpansion supported")
  }
}
