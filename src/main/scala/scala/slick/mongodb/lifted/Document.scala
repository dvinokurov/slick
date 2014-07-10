package scala.slick.mongodb.lifted

import com.mongodb.casbah.Imports.ObjectId

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.slick.mongodb.direct._

abstract class Document(val collectionName: String) {
  import scala.slick.mongodb.lifted.Document._
  def _id = value[ObjectId]("_id")
}
object Document{
//  def document[T <: InnerDocument](name: String): InnerDocument
  def array[T](name: String): DocumentArray[T] = DocumentArray(name)
  def value[T](name: String): Value[T] = Value[T](name)
}

abstract class InnerDocument(val name: String)
case class DocumentArray[T](override val name: String) extends InnerDocument(name)
case class Value[T](override val name: String) extends InnerDocument(name)


class DocumentQuery[T <: Document](override val collectionName: String) extends MongoQuery[Unit,T](collectionName,None){
  // TODO: remove - created just to test that macros works
  def sayHi() = println(collectionName)
}
object DocumentQuery {
  def apply[T <: Document] = macro applyMacroImpl2[T]

  def applyMacroImpl2[E <: Document](c: Context)(implicit e: c.WeakTypeTag[E]): c.Expr[DocumentQuery[E]] = {
    import c.universe._
    c.Expr[DocumentQuery[E]](
      // instance is created to retrieve the `collectionName` for specific subclass of `Document`:
      // val instance = new E()
      // new DocumentQuery[E](instance.collectionName)
      Block(
        List(ValDef(Modifiers(), newTermName("instance"), TypeTree(), Apply(Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR), List()))),
        Apply(Select(New(AppliedTypeTree(Ident(newTypeName("DocumentQuery")), List(TypeTree(e.tpe)))), nme.CONSTRUCTOR), List(Select(Ident(newTermName("instance")), newTermName("collectionName"))))
      )
    )
  }
}