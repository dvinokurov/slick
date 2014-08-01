package scala.slick.mongodb.lifted

import scala.language.implicitConversions
import scala.slick.ast._
import scala.slick.compiler.QueryCompiler
import scala.slick.mongodb.direct.MongoBackend
import scala.slick.profile.{RelationalDriver, RelationalProfile}

// TODO: split into traits: InvokerComponent, ExecutorConponent etc.
class MongoProfile extends RelationalProfile with MongoInsertInvokerComponent with MongoTypesComponent{ driver: MongoDriver =>

  override type Backend = MongoBackend
  override val backend: Backend = MongoBackend

  override val Implicit: Implicits = new Implicits {}
  override val simple: SimpleQL = new super.SimpleQL with Implicits {}  //todo: make a separate class?


  /** (Partially) compile an AST for insert operations */
  override def compileInsert(n: Node): CompiledInsert = ???
  /** The compiler used for queries */
  override def queryCompiler: QueryCompiler = ???
  /** The compiler used for updates */
  override def updateCompiler: QueryCompiler = ???
  /** The compiler used for deleting data */
  override def deleteCompiler: QueryCompiler = ???
  /** The compiler used for inserting data */
  override def insertCompiler: QueryCompiler = ???


  //TODO: implement
  trait ImplicitColumnTypes extends super.ImplicitColumnTypes{
    override implicit def charColumnType: BaseColumnType[Char] = ???
    override implicit def longColumnType: BaseColumnType[Long] with NumericTypedType = ???
    override implicit def byteColumnType: BaseColumnType[Byte] with NumericTypedType = ???
    override implicit def intColumnType: BaseColumnType[Int] with NumericTypedType = ???
    override implicit def booleanColumnType: BaseColumnType[Boolean] = ???
    override implicit def shortColumnType: BaseColumnType[Short] with NumericTypedType = ???
    override implicit def doubleColumnType: BaseColumnType[Double] with NumericTypedType = ???
    override implicit def bigDecimalColumnType: BaseColumnType[BigDecimal] with NumericTypedType = ???
    override implicit def floatColumnType: BaseColumnType[Float] with NumericTypedType = ???
    override implicit def stringColumnType: BaseColumnType[String] = ???
  }

  trait Implicits extends super.Implicits with ImplicitColumnTypes{
    override implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = throw new UnsupportedOperationException("Mongo driver doesn't support ddl operations.")
  }

  // TODO: not required for MongoDB:
  /** Create a DDLInvoker -- this method should be implemented by drivers as needed */
  override def createDDLInvoker(ddl: SchemaDescription): DDLInvoker = throw new UnsupportedOperationException("Mongo driver doesn't support ddl operations.")

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
  override val profile: RelationalProfile = this
}


