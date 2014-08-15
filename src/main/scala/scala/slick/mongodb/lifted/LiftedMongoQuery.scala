package scala.slick.mongodb.lifted

import scala.language.higherKinds

import scala.slick.ast._
import scala.slick.lifted._

// TODO: add aliases to hide slick.lifted.Aliases in MongoProfile.SimpleQL
trait MongoAliases{
}

// TODO: create common superclass with slick.lifted.Query where operations like `map` and `filter` are copied from
sealed abstract class LiftedMongoQuery[+E,U,C[_]] extends Rep[C[U]] { self =>
  def shaped: ShapedValue[_ <: E, U]
  final lazy val packed = shaped.toNode


  /** Build a new query by applying a function to all elements of this query
    * and using the elements of the resulting queries. This corresponds to an
    * implicit inner join in SQL. */
  def flatMap[F, T, D[_]](f: E => Query[F, T, D]): Query[F, T, C] = ???

  /** Build a new query by applying a function to all elements of this query. */
  def map[F, G, T](f: E => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): Query[G, T, C] =
    flatMap(v => Query[F, T, G](f(v)))

  /** Select all elements of this query which satisfy a predicate. */
  private def filterHelper[T](f: E => T, wrapExpr: Node => Node)
                             (implicit wt: CanBeQueryCondition[T]): Query[E, U, C] = ???

  /** Select all elements of this query which satisfy a predicate. Unlike
    * `withFilter`, this method only allows `Column`-valued predicates, so it
    * guards against the accidental use use plain Booleans. */
  def filter[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    withFilter(f)
  def filterNot[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    filterHelper(f, node => Library.Not.typed(node.nodeType, node) )

  /** Select all elements of this query which satisfy a predicate. This method
    * is used when desugaring for-comprehensions over queries. There is no
    * reason to call it directly because it is the same as `filter`. */
  def withFilter[T : CanBeQueryCondition](f: E => T) = filterHelper(f, identity)


  /** Join two collections.
    * An optional join predicate can be specified later by calling `on`. */
  def join[E2, U2, D[_]](q2: Query[E2, U2, D], jt: JoinType = JoinType.Inner): LiftedMongoQuery[(E,E2),(U,U2),C] = ???
  /** Join two collections with an inner join.
    * An optional join predicate can be specified later by calling `on`. */
  def innerJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Inner)
  /** Join two collections with a left outer join.
    * An optional join predicate can be specified later by calling `on`. */
  def leftJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Left)
  /** Join two collections with a right outer join.
    * An optional join predicate can be specified later by calling `on`. */
  def rightJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Right)
  /** Join two collections with a full outer join.
    * An optional join predicate can be specified later by calling `on`. */
  def outerJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Outer)
  /** Return a query formed from this query and another query by combining
    * corresponding elements in pairs. */
  def zip[E2, U2, D[_]](q2: Query[E2, U2, D]): LiftedMongoQuery[(E, E2), (U, U2), C] = join(q2, JoinType.Zip)
  /** Return a query formed from this query and another query by combining
    * corresponding elements with the specified function. */
  def zipWith[E2, U2, F, G, T, D[_]](q2: Query[E2, U2, D], f: (E, E2) => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): Query[G, T, C] =
    join(q2, JoinType.Zip).map[F, G, T](x => f(x._1, x._2))
  /** Zip this query with its indices (starting at 0). */
  def zipWithIndex = ???

  /** Sort this query according to a function which extracts the ordering
    * criteria from the query's elements. */
  def sortBy[T <% Ordered](f: E => T): Query[E, U, C] = ???

  /** Sort this query according to a the ordering of its elements. */
  def sorted(implicit ev: (E => Ordered)): Query[E, U, C] = sortBy(identity)

  /** Partition this query into a query of pairs of a key and a nested query
    * containing the elements for the key, according to some discriminator
    * function. */
  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G],
                                     vshape: Shape[_ <: FlatShapeLevel, E, _, P]): Query[(G, Query[P, U, Seq]), (T, Query[P, U, Seq]), C] = ???

  def encodeRef(path: List[Symbol]): Query[E, U, C] = ???

  /** Return a new query containing the elements from both operands. Duplicate
    * elements are eliminated from the result. */
  def union[O >: E, R, D[_]](other: Query[O, U, D]) = ???

  /** Return a new query containing the elements from both operands. Duplicate
    * elements are preserved. */
  def unionAll[O >: E, R, D[_]](other: Query[O, U, D]) = ???

  /** Return a new query containing the elements from both operands. Duplicate
    * elements are preserved. */
  def ++[O >: E, R, D[_]](other: Query[O, U, D]) = unionAll(other)

  /** The total number of elements (i.e. rows). */
  def length: Column[Int] = ???
  /** The total number of elements (i.e. rows). */
  def size = length

  // TODO: do we need it for MongoDB ?
  /** The number of distinct elements of the query. */
  def countDistinct: Column[Int] = ???

  /** Test whether this query is non-empty. */
  def exists = ???

  def pack[R](implicit packing: Shape[_ <: FlatShapeLevel, E, _, R]): Query[R, U, C] = ???

  /** Select the first `num` elements. */
  def take(num: ConstColumn[Long]): Query[E, U, C] = ???
  /** Select the first `num` elements. */
  def take(num: Long): Query[E, U, C] = ??? //take(LiteralColumn(num))
  /** Select the first `num` elements. */
  def take(num: Int): Query[E, U, C] = take(num.toLong)

  /** Select all elements except the first `num` ones. */
  def drop(num: ConstColumn[Long]): Query[E, U, C] = ???
  /** Select all elements except the first `num` ones. */
  def drop(num: Long): Query[E, U, C] = ??? //drop(LiteralColumn(num))
  /** Select all elements except the first `num` ones. */
  def drop(num: Int): Query[E, U, C] = drop(num.toLong)

  def to[D[_]](implicit ctc: TypedCollectionTypeConstructor[D]): Query[E, U, D] = ???
}

