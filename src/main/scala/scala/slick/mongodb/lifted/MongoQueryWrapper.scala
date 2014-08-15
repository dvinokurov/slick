package scala.slick.mongodb.lifted

import scala.slick.ast.{SymbolScope, Node}

final case class MongoQueryWrapper(node: Node) extends Node {
  override type Self = MongoQueryWrapper

  override protected[this] def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = ???

  /** Rebuild this node with a new list of children. Implementations of this
    * method *must not* perform any optimization to reuse the current node.
    * This method always returns a fresh copy. */
  override protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = {
    require(ch.size == 1, "MongoQueryWrapper can wrap single node only")
    MongoQueryWrapper(ch.head)
  }

  /** All child nodes of this node. Must be implemented by subclasses. */
  override def nodeChildren: Seq[Node] = List(node)
}