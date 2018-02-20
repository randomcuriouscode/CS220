import Solution._

class TrivialTestSuite extends org.scalatest.FunSuite {

	test ("The solution object must be defined" ) {
		val obj : hw.sudoku.SudokuLike = Solution
	}

	test("peers work"){
		assert(peers(0,0) == List((1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (0,7), (0,8), (1,1), (1,2), (2,1), (2,2)))
	}
	val str = "....8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
	val empty_brd = "................................................................................."
	
	test("parse on empty board works"){

	}

	test("nextStates properly sorted"){
		
		val statesList = parse(str).nextStates
		assert(!statesList.isEmpty || (statesList, statesList.tail).zipped.forall{
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

	        	board1count <= board2count
			})
	}

	test("valueAt works"){
		assert(parse(str).valueAt(0,4) == Some(8))
	}

	test("solve works"){
		val fromCS121_1 = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
		val fromCS121_2 = ".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2"
		val puz1 = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
		val puz2 = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
		val s = System.currentTimeMillis
		val s1 = parse(fromCS121_1).solve
		println("s1 solved")
		val s2 = parse(fromCS121_2).solve
		println("s2 solved")
		val s3 = parse(puz1).solve
		println("s3 solved")
		val s4 = parse(puz2).solve
		println("s4 solved")

		val s1s = s1 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}
		val s2s = s2 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}
		val s3s = s3 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}
		val s4s = s4 match { case Some(a: Board) => a 
			case None => parse(empty_brd)}

		println("__First: \n" + s1s)
		println("__Second: \n" + s2s)
		println("__Third: \n" + s3s)
		println("__Fourth: \n" + s4s)

		println("solve runtime : " + (System.currentTimeMillis - s) / 1000.00)
		assert(s1 match { case Some(a) => true
			case _ => false })
		assert(s2 match { case Some(a) => true
			case _ => false })
		assert(s3 match { case Some(a) => true
			case _ => false })
		assert(s4 match { case Some(a) => true
			case _ => false })

	}

}