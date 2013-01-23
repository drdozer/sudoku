package uk.co.turingatemyhamster.sudoku

/**
 * Created with IntelliJ IDEA.
 * User: nmrp3
 * Date: 22/01/13
 * Time: 22:50
 * To change this template use File | Settings | File Templates.
 */
object Solve {

  type Number = Int
  type Index = Int
  type Board = Vector[Cell]

  implicit class CellIndex(val i: Index) extends AnyVal {
    @inline def row = i % 9
    @inline def col = i / 9
    @inline def box = row / 3 + (col / 3 * 3)
  }

  lazy val cells = 9*9
  lazy val rows = 0 until 9
  lazy val cols = 0 until 9
  lazy val allIndecies = 0 until cells
  lazy val sameRow = allIndecies map (i => (i, allIndecies filter (i.row == _.row) toSet)) toMap
  lazy val sameCol = allIndecies map (i => (i, allIndecies filter (i.col == _.col) toSet)) toMap
  lazy val sameBox = allIndecies map (i => (i, allIndecies filter (i.box == _.box) toSet)) toMap

  lazy val numbers = 1 to 9 toSet

  lazy val blankCell = UnsolvedCell(numbers) : Cell
  lazy val blankBoard = Vector.fill(cells)(blankCell)

  def indx(r: Int, c: Int) = c * 9 + r

  def clues(c: ((Int, Int), Int)*) = c map { case ((r, c), n) => Clue(indx(r, c), n) }

  val hDashes = (cols ++ (0 until 2)) map (_ => "-") mkString

  def printBoardCompact(b: Board) {
    for(r <- rows) {
      if(r % 3 == 0) println(" " + hDashes)
      for(c <- cols) {
        if(c % 3 == 0) print("|")
        print(b(indx(r, c)) match {
          case SolvedCell(n) => n
          case _ => " "
        })
      }
      println("|")
    }
    println(" " + hDashes)
  }

  def othersInArea(i: Index, sameArea: Index => Set[Index])(b: Board) =
    (sameArea(i) - i) map b map (_.numbers) flatten

  def uniqueToArea(i: Index, sameArea: Index => Set[Index])(b: Board) =
    b(i).numbers -- othersInArea(i, sameArea)(b)

  def findAllUnique(b: Board): Set[Move] = {
    val slvd = for(
      i <- allIndecies if b(i).isInstanceOf[UnsolvedCell];
      (areaName, sameArea) <- List("row" -> sameRow, "col" -> sameCol, "box" -> sameBox))
    yield {
      val uniques = uniqueToArea(i, sameArea)(b)
      if(uniques.size > 1) {
        println("*** Problem: i: %d\tns: %s".format(i, uniques))
        Set()
      } else {
        uniques map { u => CellSolved(i, u) }
      }
    }

    slvd.toSet.flatten
  }


  def main(args: Array[String]) {
    for (i <- allIndecies) {
      println("i: %d\trow: %d\tcol: %d\tbox: %d\tindx: %d".format(i, i.row, i.col, i.box, indx(i.row, i.col)))
      println("\t%s".format(sameRow(i)))
      println("\t%s".format(sameCol(i)))
      println("\t%s".format(sameBox(i)))
    }

    val cls = clues(
      (1, 0) -> 3, (4, 0) -> 4, (5, 0) -> 1, (8, 0) -> 6,
      (3, 1) -> 9, (5, 1) -> 6, (7, 1) -> 1,
      (4, 2) -> 3, (7, 2) -> 9,
      (0, 3) -> 4, (2, 3) -> 7, (6, 3) -> 8,
      (2, 4) -> 8, (4, 4) -> 5, (6, 4) -> 6,
      (2, 5) -> 2, (6, 5) -> 5, (8, 5) -> 1,
      (1, 6) -> 7, (4, 6) -> 1,
      (1, 7) -> 4, (3, 7) -> 3, (5, 7) -> 5,
      (0, 8) -> 2, (3, 8) -> 4, (4, 8) -> 9, (7, 8) -> 7)

    for(c <- cls) println(c)

    def setValues(b: Board, mvs: Set[Move]): Board = {
      mvs.headOption map { mv =>
        val (newB, newMvs) = mv(b)
        println("Applied: " + mv)
        printBoardCompact(newB)
        println
        setValues(newB, mvs.tail ++ newMvs)
      } getOrElse b
    }

    def loop(b: Board, mvs: Set[Move]): Board = {
      val s1 = setValues(b, mvs)
      val au = findAllUnique(s1)
      if(au.isEmpty) s1 else loop(s1, au)
    }

    val s = loop(blankBoard, cls.toSet)
    printBoardCompact(s)

  }
}

import Solve._

sealed trait Move {
  def apply(b: Board): (Board, Set[Move])
}

/** A move that sets the value of a cell. */
abstract class SetCellValue extends Move {
  def i: Index
  def n: Number

  final def apply(b: Board) = {
    val withClue = b.updated(i, SolvedCell(n))

    val sr = sameRow(i)
    val sc = sameCol(i)
    val sb = sameBox(i)
    val moves = (sr ++ sc ++ sb - i) map (s => CancelOut(s, n) : Move)

    withClue -> moves
  }
}

/** A cell has been solved. */
case class CellSolved(i: Index, n: Number) extends SetCellValue

/** A clue, provided at the beginning of the game. */
case class Clue(i: Index, n: Number) extends SetCellValue


/** An action to cancel out a number from the possibilities at an index. */
case class CancelOut(i: Index, n: Number) extends Move {
  def apply(b: Board) = {
    b(i) match {
      case SolvedCell(_) =>
        b -> Set()
      case UnsolvedCell(ns) =>
        val ns_n = ns - n
        b.updated(i, UnsolvedCell(ns_n)) -> (
          if(ns_n.size == 1)
            Set(CellSolved(i, ns_n.head))
          else
            Set())
    }
  }
}

sealed trait Cell {
  def numbers: Set[Number]
}

case class SolvedCell(n: Number) extends Cell {
  def numbers = Set(n)
}

case class UnsolvedCell(ns: Set[Number]) extends Cell {
  def numbers = ns
}