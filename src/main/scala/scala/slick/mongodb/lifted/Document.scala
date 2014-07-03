package scala.slick.mongodb.lifted

import com.mongodb.casbah.Imports.ObjectId

abstract class Document(val collectionName: String) {
  import scala.slick.mongodb.lifted.Document._
  def _id = value[ObjectId]("_id")
}
object Document{
  def array[T](name: String): DocumentArray[T] = DocumentArray(name)
  def value[T](name: String): Value[T] = Value[T](name)
}

abstract class InnerDocument(val name: String)
case class DocumentArray[T](override val name: String) extends InnerDocument(name)
case class Value[T](override val name: String) extends InnerDocument(name)
