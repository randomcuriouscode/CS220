class Game(turn: Player, dim: Int, board: Matrix[Option[Player]]) extends GameLike[Game] {

  def isFinished(): Boolean = { 
  	if(hasPlayerWon(X) || hasPlayerWon(O) || board.toMap.size >= dim * dim) //check X/O player win, then check if draw(no wins, board full)
  		true
  	else
  		false
  }

  def hasPlayerWon(player: Player)= {
  	board.rows.exists{row => row.forall{cell => cell == Some(player)}} || 
  	board.cols.exists{col => col.forall{cell => cell == Some(player)}} ||
  	board.mainDiagonal.forall{cell => cell == Some(player)} ||
  	board.antiDiagonal.forall{cell => cell == Some(player)}
  }

  /* Assume that isFinished is true */
  def getWinner(): Option[Player] = {
  	if(hasPlayerWon(X))
  		Some(X)
	else if(hasPlayerWon(O))
		Some(O)
	else
		None
  }

  def printBoard = {
  	println("Player turn : " + (turn match {case X => "X"
  		case O => "O" 
  		case _ => "Whatever"}))
  	board.rows.foreach {(row) => println(row)}
  }

  def getTurn: Player = turn

  def getBoard: Matrix[Option[Player]] = board

  def nextBoards(): List[Game] = {
  	if(isFinished) Nil
  	
  	val emptySpots : List[(Int,Int,Option[Player])] = board.toList((a,b,c) => (a,b,c)).collect{case (x,y, None) => (x,y, None)}
  	val gameList = scala.collection.mutable.ListBuffer.empty[Game]

  	emptySpots.foreach{case (x, y, player) => gameList += Solution.createGame(turn match {case O => X
  		case X => O}, dim, board.set(x,y,Some(turn)).toMap.collect{case ((x,y),Some(p)) => (x,y) -> p})}

  	gameList.toList
  }
}

object Solution extends MinimaxLike {

  type T = Game // T is an "abstract type member" of MinimaxLike

  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
  	new Game(turn, dim, Matrix.fromMap(dim, None, board.mapValues((p) => Some(p))))
  }

	/*
	If it is Xs turn:
	1. If X has won the game, return Some(X).
	2. If the game is a draw, return None. (If all squares are filled
	and nobody has won, then the game is a draw. However, you are
	free to detect a draw earlier, if you wish.)
	3. Recursively apply minimax to all the successor states of game
	- If any recursive call produces X, return Some(X)
	- Or, if any recursive call produces None, return None
	- Or, return Some(O)
	The case for Os turn is similar.
	*/
  def minimax(board: Game): Option[Player] = {
  	board.getTurn match {
  		case X => 
  			if (board.hasPlayerWon(X)) Some(X)
  			else if (board.isFinished && board.getWinner == None) None
  			else
  			{
  				val recursedList = board.nextBoards.map{b => minimax(b)}
  				if(recursedList.exists(v => v == Some(X))) Some(X)
  				else if(recursedList.exists(v => v == None)) None
  				else Some(O)
  			}
  		case O => 
  			if (board.hasPlayerWon(O)) Some(O)
  			else if (board.isFinished && board.getWinner == None) None
  			else
  			{
  				val recursedList = board.nextBoards.map{b => minimax(b)}
  				if(recursedList.exists(v => v == Some(O))) Some(O)
  				else if(recursedList.exists(v => v == None)) None
  				else Some(X)
  			}

  		case _ => None //shouldnt reach this
  	}
  }

}
