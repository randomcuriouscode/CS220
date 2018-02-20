class TrivialTestSuite extends org.scalatest.FunSuite {

  test("The solution object must be defined") {
    val obj : MinimaxLike = Solution
  }

  val x_rigged_3x3 = Solution.createGame(X, 3, Map((0,0)->X, (1,1)-> X)) //winner should always be X
  val x_rigged_3x3_complex = Solution.createGame(O, 3, Map((0,0)->X, (1,1)-> X, (1,0) -> X)) //winner always X
  val blank_3x3_O = Solution.createGame(O, 3, Map())
  val blank_3x3_X = Solution.createGame(X, 3, Map())

  test("minimax on blank 3x3 is draw"){
  	assert(Solution.minimax(blank_3x3_O) == None)
  	assert(Solution.minimax(blank_3x3_X) == None)
  }

  test("minimax on x-rigged 3x3 board"){
  	assert(Solution.minimax(x_rigged_3x3) == Some(X))
  	assert(Solution.minimax(x_rigged_3x3_complex) == Some(X))
  }

  test("defining 3x3 board"){
  	Solution.createGame(X, 3, Map((1,1) -> O))
  }

  test("finished 2x2 board"){
  	val game = Solution.createGame(X, 2, Map((0,0) -> X, (0,1) -> O, (1,0) -> X, (1,1) -> O))
  	assert(game.isFinished)
  }
}