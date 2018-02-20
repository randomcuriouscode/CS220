object Homework2 {
	def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = {
		lst1 match {
			case Nil => Nil
			case hl1 :: tl1 => lst2 match {
				case Nil => Nil
				case hl2 :: tl2 => f(hl1, hl2) :: map2(f, tl1, tl2)
			}

		}
	}

	def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = {
		lst1 match {
			case Nil => Nil
			case hl1 :: tl1 => lst2 match {
				case Nil => Nil
				case hl2 :: tl2 => (hl1, hl2) :: zip(tl1, tl2)
			}
		}
	}

	def p_appendList[A](lst1: List[A], lst2 : List[A]) : List[A] = {
		lst1 match {
			case Nil => Nil
			case x :: Nil => x :: lst2
			case x :: tl => x :: p_appendList(tl, lst2)
		}
	}
	
	def flatten[A](lst: List[List[A]]): List[A] = {
		lst match {
			case Nil => Nil
			case Nil :: tl1 => flatten(tl1)
			case hl1 :: Nil => hl1
			case hl1 :: tl1 => p_appendList(hl1, flatten(tl1))
		}
	}

	def flatten3[A](lst: List[List[List[A]]]): List[A] = {
		lst match {
			case Nil => Nil
			case hl1 :: Nil => flatten(hl1)
			case hl1 :: tl1 => p_appendList(flatten(hl1), flatten3(tl1))
		}
	}

	def p_addItem[A](lst : List[A], i : A) : List[A] = {
		lst match {
			case Nil => i :: Nil
			case x :: Nil => x :: i :: Nil
			case x :: l1 => x :: p_addItem(l1, i)
		}
	}

	def buildList[A](length: Int, f: Int => A): List[A] = {
		if (length <= 0)
			Nil
		else if (length == 1)
			f(0) :: Nil
		else p_addItem(buildList(length - 1, f), f(length - 1))
	}

	def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = {
		lst match {
			case Nil => Nil
			case h :: Nil => f(h)
			case h :: t => p_appendList(f(h), mapList(t, f))
		}
	}

	def prependTupleList[A](lst1 : (List[A], List[A]), item : A, whichList : Int) : (List[A], List[A]) = {
		if (whichList == 1)
			(item :: lst1._1, lst1._2)
		else 
			(lst1._1, item :: lst1._2)
	}

	def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = {
		lst match {
			case Nil => (Nil, Nil)
			case h :: Nil => f(h) match {
				case true => (List(h), Nil)
				case false => (Nil, List(h))
			}
			case h :: t => f(h) match {
				case true => prependTupleList(partition(f, t), h, 1)
				case false => prependTupleList(partition(f, t), h, 2)
			}
		}
	}
}