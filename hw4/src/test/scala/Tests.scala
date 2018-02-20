class Tests extends org.scalatest.FunSuite {

  import FunctionalDataStructures._

  def fromList[A](lst: List[A]): JoinList[A] = lst match {
    case Nil => Empty()
    case List(x) => Singleton(x)
    case _  => {
      val len = lst.length
      val (lhs, rhs) = lst.splitAt(len / 2)
      Join(fromList(lhs), fromList(rhs), len)
    }
  }

  def toList[A](lst: JoinList[A]): List[A] = lst match {
    case Empty() => Nil
    case Singleton(x) => List(x)
    case Join(lst1, lst2, _) => toList(lst1) ++ toList(lst2)
  }

  test("test nth"){
    val testval = fromList(List(5,4,3,2,1))
    assert(nth(testval, 4) match {
        case Some(a) => a == 1
        case _ => false
      })
    assert(nth(testval, 0) match {
        case Some(a) => a == 5
        case _ => false
      })
    assert(nth(testval, 5) match {
        case Some(a) => false
        case None => true
      })
  }

  test("test filter"){
    val testval = fromList(List(5,4,3,2,1))
    assert(toList(filter((x : Int) => x != 5, testval)) == List(4,3,2,1))

    val testval2 = fromList(List(2,4,6,8,10))
    assert(toList(filter((x : Int) => x % 2 == 1, testval2)) == List())
  }

  test("test map"){
    val testval = fromList(List(5,4,3,2,1))
    assert(toList(map((x : Int) => x * 2, testval)) == List(10,8,6,4,2))
  }

  test("test rest"){
    val testval = fromList(List(5,4,3,2,1))
    assert(rest(testval) match { 
        case Some(x) => {
          x.size == 4 &&
          toList(x) == List(4,3,2,1)
        }
        case _ => false;
      })
  }

  test("test first of rest"){
    val testval = fromList(List(5,4,3,2,1))
    assert(first(rest(testval) match { 
      case Some(x) => {
        x
      }
      case _ => Empty[Int];
      }) match {
      case Some(x) => x == 4
      case _ => false
    })
  }

  test("test first"){
     val testval = fromList(List(5,4,1,4,3,10))
     assert(first(testval) match {
      case Some(x) => x == 5
      case _ => false
      })
  }

  test("test max"){
    val testval = fromList(List(5,4,1,4,3,10))
    max(testval, (A: Int, B: Int) => A >= B) match {
      case Some(x) => assert(x == 10)
      case None => assert(false)
    }

    val testval2 = fromList(List(5,4,1,11,-4, 5000,4,3,10))
    max(testval2, (A: Int, B: Int) => A >= B) match {
      case Some(x) => assert(x == 5000)
      case None => assert(false)
    }

    val testval3 = Singleton(1)
    max(testval3, (A: Int, B: Int) => A >= B) match {
      case Some(x) => assert(x == 1)
      case None => assert(false)
    }

    val testval4 = Empty[Int]
    max(testval4, (A: Int, B: Int) => A >= B) match {
      case None => assert(true)
      case _ => assert(false)
    }
    
  }

  test("Test Enqueue/Dequeue"){
    val a = Queue[Int](List(), List())
    assert(enqueue(1, a) == Queue[Int](List(), List(1)))

    val b = Queue[Int](List(1), List())
    assert(enqueue(2, b) == Queue[Int](List(1), List(2)))

    val c = Queue[Int](List(1), List(2))
    assert(enqueue(3, c) == Queue[Int](List(1), List(3,2)))

    val d = Queue[Int](List(), List(1))
    assert(enqueue(2, d) == Queue[Int](List(), List(2,1)))

    assert(dequeue(a) match { 
      case None => true
      case _ => false })

    assert(dequeue(b) match {
      case Some((1, Queue(List(), List()))) => true
      case _ => false
      })
    assert(dequeue(c) match {
      case Some((1, Queue(List(), List(2)))) => true
      case _ => false
      })
    assert(dequeue(c) match {
      case Some((1, q)) => dequeue(q) match {
        case Some((2, Queue(List(), List()))) => true
        case _ => false
      }
      case _ => false
      })

    assert(dequeue(d) match {
      case Some((1, Queue(List(), List()))) => true
      case _ => false
      })

    val advanced_q = Queue[Int](List(), List(2,1))
    assert(dequeue(advanced_q) match {
      case Some((1, Queue(List(2), List()))) => true
      case _ => false
    })
  }

}