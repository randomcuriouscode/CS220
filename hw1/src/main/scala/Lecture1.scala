object Lecture1 {
	val oddNumbers = 1 :: 3 :: 5 :: Nil

	def sumDouble(l : List[Int]) : Int = {
		l match {
			case Nil => 0
			case x :: l1 => 2 * x + sumDouble(l1)
		}

		/*
		if (l.isEmpty) 0
		else 2 * l.head + sumDouble(l.tail)
		*/
	}

	def removeZeroes(l : List[Int]) : List[Int] = {
		/*l.filterNot(x => x == 0)*/
		l match {
			case Nil => Nil
			case 0 :: l2 =>  removeZeroes(l2)
			case x :: 0 :: l2 => x :: removeZeroes(l2)
			case x :: l2 => x :: removeZeroes(l2)
		}
	}

	def countEvens(l : List[Int]) : Int = {
		// l.count(x => x % 2 == 0)
		l match {
			case Nil => 0
			case x :: l2 => 
				if ((x % 2) == 0) 1 + countEvens(l2)
				else countEvens(l2)
		}
	}

	def removeAlternating(l : List[String]) : List[String] = {
		//l.zipWithIndex.collect{case (e,i) if (i % 2) == 0 => e}

		l match {
			case Nil => Nil
			case x :: y :: l1 => x :: removeAlternating(l1)
			case x :: Nil => List(x)
		}
/*
		var retList : List[String] = List()
		for(x <- l.indices){
			if(x % 2 == 0) retList = retList ::: List(l.apply(x))
		}

		return retList*/
	}

	def isAscending(l : List[Int]) : Boolean = {
		l match {
			case Nil => true
			case x :: Nil => true
			case x :: y :: l2 => x <= y && isAscending(l2)
		}
	}

	def addSub(l : List[Int]) : Int = {
		l match {
			case Nil => 0
			case x :: y :: l1 => x - y + addSub(l1)
			case x :: Nil => x
		}
	}

	def p_alternate(l1 : List[Int], l2: List[Int], whichList: Int) : List[Int] = {
		if(whichList == 1)
			l1 match {
				case Nil => Nil
				case x :: l3 => x :: p_alternate(l3, l2, 2)
			}
		else
			l2 match {
				case Nil => Nil
				case x :: l4 => x :: p_alternate(l1, l4, 1)
			}
	}

	def alternate(l1 : List[Int], l2 : List[Int]): List[Int] = {
		//l1.zip(l2).flatMap(e => List(e._1, e._2))
		p_alternate(l1, l2, 1)
	}

	def fromTo(x : Int, y : Int) : List[Int] = {
		if(x == y) Nil
		else x :: fromTo(x + 1, y)
	}

	def insertOrdered(n: Int, lst: List[Int]): List[Int] = {
		//(lst :+ n).sortWith((x, y) => x < y)

		lst match {
			case Nil => List(n)
			case x :: Nil =>
				if (n <= x) List(n, x)
				else List(x,n)
			case x :: y :: l1 => 
				if (n <= x ) n :: x :: y :: l1
				else if (n > x && n < y) x :: n :: y :: l1
				else x :: y :: insertOrdered(n, l1)
		}
	}

	def p_minimum(l : List[Int]) : Int = {
		l match { 
			case Nil => 0
			case x :: Nil => x
			case x :: l1 =>
				val min = p_minimum(l1)
				if (x < min) x
				else min
		}
	}

	def p_addItem(lst : List[Int], i : Int) : List[Int] = {
		lst match {
			case Nil => i :: Nil
			case x :: Nil => x :: i :: Nil
			case x :: l1 => x :: p_addItem(l1, i)
		}
	}

	/**
	Selection sort
	**/
	def sort(lst: List[Int]): List[Int] = { 
		lst match {
			case Nil => Nil
			case x :: Nil => x :: Nil
			case x :: l1 => 
				if (x > p_minimum(l1)) p_addItem(sort(l1), x)
				else x :: sort(l1)
		}
	}
}