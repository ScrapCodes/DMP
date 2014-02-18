package dmp

import org.scalatest.FunSuite
import scala.collection.mutable

class DAGGeneratorSuite extends FunSuite {
  test("initial Skewed Arrangement") {
    val a = DistributedMatrix("A", 10, 10)
    val b = DistributedMatrix("B", 10, 10)
    val tasks = DAGGenerator.initSkewedArrangement(a, b, 2, 4)

    println(DAGGenerator.initSkewedArrangement(a, b, 2, 4).mkString("\n"))
  }

  test("canons") {
    import DAGGenerator._
    val a = DistributedMatrix("A", 10, 10)
    val b = DistributedMatrix("B", 10, 10)
    val temp: mutable.HashMap[Int, mutable.HashMap[(Int, Int), (String, String)]]
    = for (row <- cannonsMultiplication(a, b, 36)) yield {
      row._1 -> (for (block <- row._2) yield {
        blockIdToijPair(block._1, 6) ->("A" + blockIdToijPair(block._2.left.blockId, 6),
          "B" + blockIdToijPair(block._2.right.blockId, 6))
      })
    }
    val temp2 = for (row <- cannonsMultiplicationSimple(a, b, 36)) yield {
      row._1 -> (for (block <- row._2) yield {
        blockIdToijPair(block._1, 6) -> ("A" + blockIdToijPair(block._2.left.blockId, 6),
          "B" + blockIdToijPair(block._2.right.blockId, 6))
      })
    }
    println(temp.mkString("\n"))
    println(temp2.mkString("\n"))

    //println(DAGGenerator.cannonsMultiplication(a, b, 36).mkString("\n"))
  }

  
}
