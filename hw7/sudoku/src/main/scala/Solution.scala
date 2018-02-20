import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def time(f: => Any) : Long = {
    val s = System.currentTimeMillis
    f
    (System.currentTimeMillis - s) / 1000
  }

  def parse_helper(row: Int, col: Int, charArr : Array[Char], peerList: List[(Int,Int)]): List[Int] = {
    val takenVal: Char = charArr(row * 9 + col)

    if(takenVal != '.')
      List(takenVal.asDigit)
    else
      1.to(9).filterNot{ case (avail) =>
        peerList.exists{ case (r,c) => 
          //println("Checking peerlist: (" + r + ", " + c + ")" + " chararr: " + charArr(r * 9 + c))

          charArr(r * 9 + c).asDigit == avail}
      }.toList
  }

  def parse(str: String): Board = {
    val charArr : Array[Char] = str.replaceAll(" ", "").toCharArray
    val availmap = Map((0.to(8).flatMap{r => 0.to(8).map{c => 
      (r,c) -> parse_helper(r, c, charArr, peersTbl.getOrElse((r,c), List()))}
      }):_*)
    val newavailmap = scala.collection.mutable.Map[(Int, Int), List[Int]]((0.to(8).flatMap{r => 0.to(8).map{c => 
      (r,c) -> 1.to(9).toList}}):_*)
    availmap.foreach{ // add values to mutable map, filtering each value so no peers have the same constrained value
        case (k,v)  =>
          newavailmap += (k -> v.filterNot{availVal => Solution.peers(k._1, k._2).exists{ peerVal => 
              val listAvail = newavailmap(peerVal._1, peerVal._2)
              listAvail.length == 1 && listAvail(0) == availVal
            }})
        }
    new Board(newavailmap.toMap)
  }

  def calcPeers(row: Int, col: Int): List[(Int,Int)] = {
    val rowPeers = 0.to(8).map{r => (r,col)}
    val colPeers = 0.to(8).map{c => (row, c)}
    val boxRow : Int = (row / 3) * 3
    val boxCol : Int = (col / 3) * 3
    val boxPeers = boxRow.to(boxRow + 2).flatMap{r => boxCol.to(boxCol + 2).map{c=>(r,c)}}

    (rowPeers ++ colPeers ++ boxPeers).filterNot{
      case(r,c) => r == row && c == col
    }.toList.distinct
  }

  val peersTbl = Map((0.to(8).flatMap{r => 0.to(8).map{c => ((r,c) -> calcPeers(r,c))}}):_*)

  def peers(row: Int, col: Int): Seq[(Int, Int)] = peersTbl.getOrElse((row,col), Seq.empty[(Int,Int)])

  // You can use a Set instead of a List (or, any Iterable)
  /*
  def peers(row: Int, col: Int): List[(Int, Int)] = {

    def yieldRows = 
      for(i_row <- 0 until 9 if i_row != row)
        yield (i_row, col)
    def yieldCols =
      for(i_col <- 0 until 9 if i_col != col)
        yield (row, i_col)
    def yieldSameBox = {
      val boxRowStart : Int = (row / 3) * 3
      val boxColStart : Int = (col/3) * 3

      for(i_row <- boxRowStart until boxRowStart + 3; i_col <- boxColStart until boxColStart + 3 if (i_row != row && i_col != col))
        yield (i_row, i_col)
    }


    yieldRows.toList ::: yieldCols.toList ::: yieldSameBox.toList
  }*/
}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    available.get(row, col) match {
      case Some(l : List[Int]) => 
        if(l.length == 1) Some(l(0))
        else None
      case None => None
    }
  }

  def isSolved(): Boolean = { //works trivially
    available.forall{ case (k,v) => v.length == 1}
  }

  def isUnsolvable(): Boolean = { //works trivially
    available.exists{ case (k,v) => v.length == 0}
  }

     /*def checkPeers(b : Board): Board = {
        new Board(Map((0.to(8).flatMap{r => 0.to(8).map{c => 
        (r,c) -> if (Solution.peers(r,c).exists{ peerval => b.available.getOrElse(peerval, 1.to(9).toList).length == 1}}
        }):_*))
    }*/
/*
  def constrain_single_values(row: Int, col: Int, value: Int): Board = {
    Solution.peers(row, col) match {
      case Seq.empty[(Int,Int)] => 
        this
      case peerList: Seq[(Int, Int)] =>
        available.map{
          case (k,v) if peerList.contains(k) => k -> v.filterNot{case a => a == value}
          case (k,v) if k._1 == row && k._2 == col => k -> List(value)
          case (k,v) => k -> v
        }
       
        val newAvail = ((row,col) -> availVals.filterNot{v => v == value})
        if(newAvail.length == 1){
            new Board(available + newAvail).constrain_single_values(row, col, newAvail(0))
        }
        else 
          new Board(available + newAvail)

        if(col == 8)
          constrain_single_values(row + 1, 0, value)
        else
          constrain_single_values(row, col + 1, value)
    }
  }
*/
  def compare(row1: Int,col1: Int, row2: Int, col2: Int): Boolean = {
    row1 < row2 & row1 + col1 < row2 + col2
  }

  def place(row: Int, col: Int, value: Int): Board = {

    require(availableValuesAt(row, col).contains(value))
    val peerList = Solution.peers(row,col)
    val new_available = available.map{
      case (k,v) if Solution.peers(row,col).contains(k) => k -> v.filterNot{case avail => avail == value}
      case (k,v) if k._1 == row && k._2 == col => k -> List(value)
      case (k,v) => k -> v}

    //println(new_available)

    val availList = new_available.foldLeft(List[((Int,Int),List[Int])]()){
      case (a: List[((Int,Int),List[Int])],(k,v: List[Int])) =>
          a :+ (k,v)
    }.sortBy{a => a._1}

    val newstr = availList.foldLeft(""){
      case (a: String,(k: (Int,Int), v: List[Int])) =>
        if (v.length == 1)
          a + v(0)
        else
          a + "."
    }
    //println("placed val : " + value + " at (" + row + ", " + col + "):\n\t" + availList)
    Solution.parse(newstr)

    /*new Board(available.map{
      case (k,v) if Solution.peers(row,col).contains(k) => k -> v.filterNot{case avail => avail == value}
      case (k,v) if k._1 == row && k._2 == col => k -> List(value)
      case (k,v) => k -> v}.map{ //map again to eliminate resulting single peer vals from other peers
        case (k,v)  =>
          k -> v.filterNot{availVal => Solution.peers(k._1, k._2).exists{ peerVal => 
              val listAvail = availableValuesAt(peerVal._1, peerVal._2)
              listAvail.length == 1 && listAvail(0) == availVal
            }}
        })*/
  }
/*
  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    if (isUnsolvable()) scala.collection.mutable.ListBuffer.empty[Board]
    val buf = scala.collection.mutable.ListBuffer.empty[Board]
    
    available.foreach{
      case (k,v) => 
        if (v.length != 1) v.foreach{
          case availval => //place each val in avail list, create new board
            val placed_board = place(k._1, k._2, availval)
            if (!placed_board.isUnsolvable) // get rid of invalid boards
              buf += placed_board
        }
    }

    buf.toList.sortWith{
      case (board1,board2) => 
        val board1count = board1.available.foldLeft(0){
          case (a: Int,(k, v : List[Int])) if v.length != 1 =>
            a + v.length
          case (a: Int, (k,v: List[Int])) => a
        }
        val board2count = board2.available.foldLeft(0){
          case (a: Int,(k, v: List[Int])) if v.length != 1 =>
            a + v.length
          case (a: Int, (k,v: List[Int])) => a
        }

        val truefalse = board1count < board2count

        truefalse
    }.distinct
  }*/
  def nextStates() : Stream[Board] = {
    def row_loop(row: Int): Stream[Board] = {
      def col_loop(col: Int) : Stream[Board] = {
        def value_loop(value: Int) : Stream[Board] = {
          if(availableValuesAt(row,col).contains(value) && value <= 9)
            place(row,col,value) #:: value_loop(value + 1)
          else Stream.empty
        }
        if(col <= 8)
          value_loop(1) ++ col_loop(col + 1)
        else Stream.empty
      }
      if(row <= 8)
        col_loop(0) ++ row_loop(row + 1)
      else Stream.empty
    }

    row_loop(0).sortWith{
      case (board1,board2) => 
        val board1count = board1.available.foldLeft(0){
          case (a: Int,(k, v : List[Int])) if v.length != 1 =>
            a + v.length
          case (a: Int, (k,v: List[Int])) => a
        }
        val board2count = board2.available.foldLeft(0){
          case (a: Int,(k, v: List[Int])) if v.length != 1 =>
            a + v.length
          case (a: Int, (k,v: List[Int])) => a
        }

        val truefalse = board1count < board2count

        truefalse
    }.distinct
  }

  def boardCount: Int = available.foldLeft(0){
    case (a: Int,(k, v : List[Int])) if v.length != 1 =>
        a + v.length
    case (a: Int, (k,v: List[Int])) => a
  }

  override def hashCode: Int = available.foldLeft(0) {
    case (a: Int,(k, v: List[Int])) =>
      a + v.foldLeft(0){ _ + _ }
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Board => this.available.forall{case (k,v) => v == that.availableValuesAt(k._1, k._2)}
      case _ => false
   }

  def solve(): Option[Board] = {
    //println("Checking board... :\n" + this)
    if(isSolved)
      Some(this)
    else if (isUnsolvable)
      None
    else
    {
      val next_boards = nextStates
      val ret = scala.collection.mutable.ListBuffer.empty[Board]

      next_boards.find{
            state =>
              if(!state.isUnsolvable && state.boardCount < this.boardCount){
                if(state.isSolved)
                {
                  //println("solved!\n")
                  ret += state
                  true
                }
                else 
                  state.solve match {
                  case Some(a: Board) => 
                    //println("solved!\n")
                    ret += a
                    true
                  case None => 
                    false
                }
              }
              else false
      }
      if(!ret.isEmpty)
        Some(ret(0))
      else
        None

      /*
      next_boards.find{ //this is unnecessary, just search nextstates and solve those boards
        state => 
          acc = acc + 1
          //println("checking state : \n" + state + " acc " + acc)
          state.isSolved
      } match {
        case Some(a) => 
          Some(a)
        case None =>
          nextStates.find{
            state =>
              (state.solve match {
              case Some(a: Board) => 
                println("solved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\nsolved!\n")
                true
              case None => 
                false
          })
          }
      }*/
    }
  }
}
