package scala.slick.mongodb.lifted

import scala.language.{higherKinds, implicitConversions}
import scala.slick.ast._
import scala.slick.compiler.QueryCompiler
import scala.slick.lifted.Query
import scala.slick.mongodb.direct.MongoBackend
import scala.slick.profile.{RelationalDriver, RelationalProfile}

// TODO: split into traits?
trait MongoProfile extends RelationalProfile with MongoInsertInvokerComponent with MongoTypesComponent{ driver: MongoDriver =>

  override type Backend = MongoBackend
  override val backend: Backend = MongoBackend

  override val Implicit: Implicits = new Implicits {}
  override val simple: SimpleQL = new SimpleQL {}


  // TODO: extend for complicated node structure, probably mongodb nodes should be used
  /** (Partially) compile an AST for insert operations */
  override def compileInsert(n: Node): CompiledInsert = n

  trait ImplicitColumnTypes extends super.ImplicitColumnTypes{
    override implicit def charColumnType: BaseColumnType[Char] = ScalaBaseType.charType
    override implicit def longColumnType: BaseColumnType[Long] with NumericTypedType = ScalaBaseType.longType
    override implicit def byteColumnType: BaseColumnType[Byte] with NumericTypedType = ScalaBaseType.byteType
    override implicit def intColumnType: BaseColumnType[Int] with NumericTypedType = ScalaBaseType.intType
    override implicit def booleanColumnType: BaseColumnType[Boolean] = ScalaBaseType.booleanType
    override implicit def shortColumnType: BaseColumnType[Short] with NumericTypedType = ScalaBaseType.shortType
    override implicit def doubleColumnType: BaseColumnType[Double] with NumericTypedType = ScalaBaseType.doubleType
    override implicit def bigDecimalColumnType: BaseColumnType[BigDecimal] with NumericTypedType = ScalaBaseType.bigDecimalType
    override implicit def floatColumnType: BaseColumnType[Float] with NumericTypedType = ScalaBaseType.floatType
    override implicit def stringColumnType: BaseColumnType[String] = ScalaBaseType.stringType
  }

  trait Implicits extends super.Implicits with ImplicitColumnTypes{
    override implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = createDDLInvoker(d)
    implicit def queryToLiftedMongoInvoker[T,C[_]](q: Query[_,T,C])(implicit session: MongoBackend#Session): LiftedMongoInvoker[T] =
      new LiftedMongoInvoker[T](queryCompiler.run(q.toNode).tree,session)
  }

  trait SimpleQL extends super.SimpleQL with Implicits with MongoAliases

  // TODO: not required for MongoDB:
  /** Create a DDLInvoker -- this method should be implemented by drivers as needed */
  override def createDDLInvoker(ddl: SchemaDescription): DDLInvoker = throw new UnsupportedOperationException("Mongo driver doesn't support ddl operations.")

  /** The compiler used for queries */
  override def queryCompiler: QueryCompiler = {
//    compiler -
//      Phase.expandRecords -
//      Phase.flattenProjections -
//      Phase.relabelUnions -
//      Phase.pruneFields -
//      Phase.assignTypes
    compiler ++ QueryCompiler.relationalPhases
  }
  /** The compiler used for updates */
  override def updateCompiler: QueryCompiler = ???
  /** The compiler used for deleting data */
  override def deleteCompiler: QueryCompiler = ???
  /** The compiler used for inserting data */
  override def insertCompiler: QueryCompiler = ???

  override type SchemaDescription = SchemaDescriptionDef
  override def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  override def buildTableSchemaDescription(table: Table[_]): SchemaDescription = ???

  override type QueryExecutor[T] = QueryExecutorDef[T]
  override type UnshapedQueryExecutor[T] = UnshapedQueryExecutorDef[T]
  /** Create an executor -- this method should be implemented by drivers as needed */
  override def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = ???
  override def createUnshapedQueryExecutor[M](value: M): UnshapedQueryExecutor[M] = ???
}

// TODO: make it a class?
trait MongoDriver extends MongoProfile with RelationalDriver {
  override val profile: MongoProfile = this
}
object MongoDriver extends MongoDriver


