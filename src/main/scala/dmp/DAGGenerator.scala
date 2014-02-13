package dmp

import datastructure.Matrix
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.IndexedSeq

/**
 * Generates a DAG of sub operation given a matrix operation. This is actually the place to implement algorithms for
 * processing the Matrices.
 */
object DAGGenerator {
  type BlocksMap = mutable.HashMap[Int, MatrixTask]
  type RoundsMap = mutable.HashMap[Int, BlocksMap]

  def cannonsMultiplication(left: DistributedMatrix, right: DistributedMatrix, noOfBlocks: Int): RoundsMap = {
    val rounds = Math.sqrt(noOfBlocks).toInt
    val initTasks: BlocksMap = initSkewedArrangement(left, right, rounds, noOfBlocks)
    val dags: RoundsMap = mutable.HashMap[Int, BlocksMap]()
    dags += 0 -> initTasks
    for (i <- 1 until rounds) {
      var blockMap = mutable.HashMap[Int, MatrixTask]()
      for (blockId <- 0 until noOfBlocks) {
        val initBlockIDLeft = dags(i - 1)(blockId).left.blockId
        val initBlockIDRight = dags(i - 1)(blockId).right.blockId
        blockMap += (blockId -> MatrixTask(Seq(), left.getPartition(rightPid(initBlockIDLeft, 1, rounds)),
          right.getPartition(downPid(initBlockIDRight, 1, rounds)), (a, b) => a x b))
      }
      dags += i -> blockMap
    }
    dags
  }

  def cannonsMultiplicationSimple(left: DistributedMatrix, right: DistributedMatrix, noOfBlocks: Int) = {
    import Math._
    val rounds = sqrt(noOfBlocks).toInt
    def f(i: Int, j: Int, t: Int) = (i + j + t) % rounds
    val z = for (t <- 0 until rounds) yield {
      t -> (for {
        blockId <- 0 until noOfBlocks
        (i, j) = blockIdToijPair(blockId, rounds)
        magicNumber = f(i, j, t)
      } yield blockId -> MatrixTask(Seq(),
          MatrixPartition(ijPairToBlockId(i, magicNumber, rounds), left.name, left.m, left.n, t),
          MatrixPartition(ijPairToBlockId(magicNumber, j, rounds), right.name, right.m, right.n, t), (a, b) => a x b))
    }
    z
  }

  @tailrec def downPid(pid: Int, recurse: Int, rounds: Int): Int = {
    if (recurse == 0)
      pid
    else {
      val (i, j) = blockIdToijPair(pid, rounds)
      downPid((if (i + 1 >= rounds) 0 else i + 1) * rounds + j, recurse - 1, rounds)
    }
  }

  @tailrec def rightPid(pid: Int, recurse: Int, rounds: Int): Int = {
    if (recurse == 0) {
      pid
    } else {
      val (i, j) = blockIdToijPair(pid, rounds)
      rightPid(i * rounds + (if (j + 1 >= rounds) 0 else j + 1), recurse - 1, rounds)
    }
  }

  def blockIdToijPair(pid: Int, rounds: Int): (Int, Int) = (pid / rounds, pid % rounds)

  def ijPairToBlockId(i: Int, j: Int, rounds: Int) = i * rounds + j

  /**
   * for A x B
   */
  def initSkewedArrangement(A: DistributedMatrix, B: DistributedMatrix, rounds: Int, noOfBlocks: Int) = {

    val tasks = mutable.HashMap[Int, MatrixTask]()
    for (blockId <- 0 until noOfBlocks) {
      val (i, j) = blockIdToijPair(blockId, rounds)
      val rPid = rightPid(blockId, i, rounds)
      val dPid = downPid(blockId, j, rounds)
      tasks += (blockId -> MatrixTask(List.empty, A.getPartition(rPid), B.getPartition(dPid), (a, b) => a x b))
    }
    tasks
  }
}

case class MatrixPartition(blockId: Int, name: String, m: Int, n: Int, roundId: Int) {
  override def toString = s"${name + m + n}_${roundId}_$blockId"
}

case
class MatrixTask(prev: Seq[MatrixTask], left: MatrixPartition, right: MatrixPartition, f: (Matrix, Matrix) => Matrix)

case class DistributedMatrix(name: String, m: Int, n: Int) {
  def getPartition(blockId: Int): MatrixPartition = {
    MatrixPartition(blockId, name, m, n, 0)
  }
}
