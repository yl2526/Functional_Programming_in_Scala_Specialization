package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File


import org.apache.log4j.Logger
import org.apache.log4j.Level

import StackOverflow._

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  ignore("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  trait testSample extends StackOverflow {
    val lines: RDD[String] = sc.parallelize(
      "1,27233496,,,0,C#"::
        "2,23698767,,27233496,9,C#"::
        "1,5484340,,,0,C#"::
        "2,5494879,,5484340,1,C#"::
        "1,20990204,,,6,PHP"::
        "1,5077978,,,-2,Python"
        ::Nil)
    val samples: RDD[Posting] = rawPostings(lines).cache()
    val samplesList: Array[Posting] = samples.collect()
  }


  ignore("group questions answers on id") {
    new testSample {
      val result = sc.parallelize(
        (27233496, (samplesList(0), samplesList(1)))::
          (5484340, (samplesList(2), samplesList(3)))::
          Nil
      ).groupByKey().mapValues(_.toList)
      assert(groupedPostings(samples).mapValues(_.toList).collect === result.collect)
    }
  }

}
