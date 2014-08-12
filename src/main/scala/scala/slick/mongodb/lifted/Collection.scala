package scala.slick.mongodb.lifted

import com.mongodb.casbah.Imports.ObjectId

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.macros.Context
import scala.slick.ast._
import scala.slick.lifted.{AbstractTable, BaseTag, Column, TableQuery}
import scala.slick.mongodb.direct.{GetResult, MongoBackend, SimpleMongoInvoker, TypedMongoCollection}

// TODO: review, implement methods - not sure that BaseTag should be used as a superclass
class DocumentTag extends BaseTag{
  /** Return a new instance of the AbstractTable carrying this Tag, with a new path */
  override def taggedAs(path: List[Symbol]): AbstractTable[_] = ???
}

// TODO: consider case when number of fields > 22 and Tuples are not applicable - check if existing solution applies
// TODO: provide overriding possibility for _id
// TODO: constructor parameter for tag
abstract class Collection[T](val collectionName: String) extends AbstractTable[T](new DocumentTag, None, collectionName) {
  type TableElementType = T
  import scala.slick.mongodb.lifted.Collection._
  def _id = value[ObjectId]("_id")
}
object Collection{
  implicit def objectIdTypedType: TypedType[ObjectId] = ???

  /** Shortcut for arrays of elements of the same given type */
  def array[T](name: String, options: ColumnOption[T]*)(implicit tpe: TypedType[T]): DocumentArray[T] = DocumentArray[T](name, options: _*)
  def value[T](name: String, options: ColumnOption[T]*)(implicit tpe: TypedType[T]): Value[T] = Value[T](name, options: _*)
}

abstract class Document[T](val name: String, options: ColumnOption[T]*)(implicit final val tpe2: TypedType[T]) extends Column[T]{
  /** Get the Node for this Rep */
  override def toNode: Node = ???
}
case class DocumentArray[T](override val name: String, options: ColumnOption[T]*)(implicit final val tpe3: TypedType[T]) extends Document[T](name, options: _*) {
  /** Get the Node for this Rep */
  override def toNode: Node = ???
}
case class Value[T](override val name: String, options: ColumnOption[T]*)(implicit final val tpe3: TypedType[T]) extends Document[T](name, options: _*) {
  /** Get the Node for this Rep */
  override def toNode: Node = ???
}


//TODO: constructor
class CollectionQuery[T <: Collection[_]](val document: T) extends TableQuery[T](d=>throw new RuntimeException){
  val collectionName = document.collectionName
}
object CollectionQuery {
  // TODO: implement ?
  def converter[T <: Collection[_]](documentQuery: CollectionQuery[T]): GetResult[Product] = ???
  // TO DO: update for complicated document structure, not only Tuple2
//  {
//    val fields = documentQuery.document.*
//    fields match {
//      // TO DO: do we really need `.asInstanceOf[f1.Wrapped]` ?
//      case (f1: InnerDocument[_], f2: InnerDocument[_]) => GetResult[Product](mongoDBObject => (mongoDBObject.get(f1.name).get.asInstanceOf[f1.Wrapped],mongoDBObject.get(f2.name).get.asInstanceOf[f2.Wrapped]))
//    }
//  }

  @inline implicit def documentQueryAsInvoker[T <: Collection[_]](dq: CollectionQuery[T])(implicit session: MongoBackend#Session): SimpleMongoInvoker[Product] =
    SimpleMongoInvoker[Unit,Product](dq.collectionName,None,None)(session,converter(dq))

  @inline implicit def documentQueryAsTypedMongoCollection[T <: Collection[_]](dq: CollectionQuery[T])(implicit session: MongoBackend#Session):  TypedMongoCollection[Product] =
    new TypedMongoCollection[Product](dq.collectionName)(session,converter(dq))

  def apply[T <: Collection[_]] = macro applyMacroImpl[T]

  def applyMacroImpl[T <: Collection[_]](c: Context)(implicit e: c.WeakTypeTag[T]): c.Expr[CollectionQuery[T]] = {
    import c.universe._
    c.Expr[CollectionQuery[T]](
      // new DocumentQuery[T](new T())
      Apply(Select(New(AppliedTypeTree(Ident(newTypeName("DocumentQuery")), List(TypeTree(e.tpe)))), nme.CONSTRUCTOR), List(Apply(Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR), List())))
    )
  }
}
