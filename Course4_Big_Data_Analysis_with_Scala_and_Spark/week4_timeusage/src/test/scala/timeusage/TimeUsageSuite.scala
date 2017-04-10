package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{
  DoubleType,
  StringType,
  StructField,
  StructType
}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {
  test("schema") {
    val res = StructType(
      StructField("ca",StringType,false)
      ::StructField("cb",DoubleType,false)
      ::StructField("cc",DoubleType,false)
      ::Nil
    )
    assert(TimeUsage.dfSchema("ca"::"cb"::"cc"::Nil) === res)
  }

  test("row") {
    val res = Row fromSeq List("foo", 1.0, 2.0)
    val input = List("foo", "1.0", "2.0")
    assert(TimeUsage.row(input) === res)
  }


}
