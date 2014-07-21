package scala.slick.mongodb.lifted

import com.mongodb.casbah.Imports.ObjectId

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.macros.Context
import scala.slick.mongodb.direct.{TypedMongoCollection, MongoBackend, MongoInvoker, GetResult}

// TODO: consider case when number of fields > 22 and Tuples are not applicable
// TODO: provide overriding possibility for _id
//abstract class Document[+W](val collectionName: String) {
abstract class Document(val collectionName: String) {
  import scala.slick.mongodb.lifted.Document._
  def _id = value[ObjectId]("_id")

  def * : Product
}
object Document{
  def array[T](name: String): DocumentArray[T] = DocumentArray[T](name)
  def value[T](name: String): Value[T] = Value[T](name)
}

abstract class InnerDocument[T](val name: String){
  type Wrapped = T
}
case class DocumentArray[T](override val name: String) extends InnerDocument[T](name)
case class Value[T](override val name: String) extends InnerDocument[T](name)


class DocumentQuery[T <: Document](val document: T) {
  val collectionName = document.collectionName

  // TODO: remove - created just to test that macros works
  def sayHi() = println(collectionName)

  def flatMap = ???

  def map = ???

  def withFilter = ???
}
object DocumentQuery {
  // TODO: update for complicated document structure
  def converter[T <: Document](documentQuery: DocumentQuery[T]): GetResult[Product] = {
    val fields = documentQuery.document.*
    fields match {
      case (f1: InnerDocument[_], f2: InnerDocument[_]) => GetResult[Product](mongoDBObject => (mongoDBObject.get(f1.name).asInstanceOf[f1.Wrapped],mongoDBObject.get(f1.name).asInstanceOf[f1.Wrapped]))
    }
  }

  @inline implicit def documentQueryAsInvoker[T <: Document](dq: DocumentQuery[T])(implicit session: MongoBackend#Session): MongoInvoker[Product] =
    MongoInvoker[Unit,Product](dq.collectionName,None,None)(session,converter(dq))

  @inline implicit def documentQueryAsTypedMongoCollection[T <: Document](dq: DocumentQuery[T])(implicit session: MongoBackend#Session):  TypedMongoCollection[Product] =
    new TypedMongoCollection[Product](dq.collectionName)(session,converter(dq))

  def apply[T <: Document] = macro applyMacroImpl[T]


  // TODO: refactor - use document method of DocumentQuery
  def applyMacroImpl[T <: Document](c: Context)(implicit e: c.WeakTypeTag[T]): c.Expr[DocumentQuery[T]] = {
    import c.universe._
    c.Expr[DocumentQuery[T]](
      // instance is created to retrieve the `collectionName` for specific subclass of `Document`:
      // val instance = new T()
      // new DocumentQuery[T](instance)
      Block(
        List(ValDef(Modifiers(), newTermName("instance"), TypeTree(), Apply(Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR), List()))),
        Apply(Select(New(AppliedTypeTree(Ident(newTypeName("DocumentQuery")), List(TypeTree(e.tpe)))), nme.CONSTRUCTOR), List(Ident(newTermName("instance"))))
      )
    )
  }
}

class WrappingDocumentQuery(val query: Map[String, Any])
object WrappingDocumentQuery{
  // TODO: implicit conversions
//  implicit def wrappingDocumentQueryAsMongoQuery(wrappingQuery: WrappingDocumentQuery): MongoQuery = ???
}

