package scala.slick.mongodb.lifted

import scala.slick.ast._

/** Common trait for all nodes for MongoDB*/
sealed trait MongoNode extends Node

/** Root node for MongoDB query */
final case class QueryNode(elements: IndexedSeq[AttributeNode]) extends MongoNode {
  override type Self = QueryNode

  // TODO: do we need some logic here or returning current object is ok?
  override protected[this] def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = this

  /** Rebuild this node with a new list of children. Implementations of this
    * method *must not* perform any optimization to reuse the current node.
    * This method always returns a fresh copy. */
  override protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = {
    require(ch.forall(child => child.isInstanceOf[AttributeNode]), "All children of mongo QueryNode must be mongo AttributeNode`s")
    if(ch == elements) this else QueryNode(ch.asInstanceOf[IndexedSeq[AttributeNode]])
  }

  /** All child nodes of this node. */
  override def nodeChildren: Seq[Node] = elements
}

/** Common superclass for all attribute nodes */
abstract class AttributeNode(val attributeName: String) extends MongoNode

/** Node for plain document attributes */
final case class PlainAttributeNode(override val attributeName: String) extends AttributeNode(attributeName) with NullaryNode with TypedNode {
  override protected[this] def nodeRebuild: Self = copy()

  override type Self = PlainAttributeNode

  override def tpe: Type = ???
}

/** Node for array document attributes */
sealed case class ArrayNode(override val attributeName: String) extends AttributeNode(attributeName) with SimplyTypedNode {
  override type Self = ArrayNode

  override protected def buildType: Type = ???

  /** Rebuild this node with a new list of children. Implementations of this
    * method *must not* perform any optimization to reuse the current node.
    * This method always returns a fresh copy. */
  override protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = new ArrayNode(attributeName){
    override val nodeChildren = ch
  }.asInstanceOf[Self]

  /** All child nodes of this node. */
  override def nodeChildren: Seq[Node] = Nil
}
