object FunctionalDataStructures {

  //
  // Part 1. Persistent Queues
  //

  def enqueue[A](elt: A, q: Queue[A]): Queue[A] = {
    q match {
      case Queue(Nil, back) => Queue[A](List[A](), elt :: back)
      case Queue(front, Nil) => Queue[A](front, List[A](elt))
      case Queue(front, back) => Queue[A](front, elt :: back)
      case _ => Queue(List[A](), List[A](elt))
    }
  }

  def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = q match {
    case Queue(Nil, Nil) => None
    case Queue(Nil, back) => Some((back.reverse(0), Queue(back.reverse match {
      case head :: Nil => List[A]()
      case head :: tail => tail
      case Nil => List()
    }, List())))
    case Queue(front, back) => Some((front(0), Queue(front match { 
      case head :: Nil => List[A]() 
      case head :: tail => tail
      case Nil => List()
    }
      , back)))
    case _ => None
  }

  //
  // Part 2. Join Lists
  //



  def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = {
    lst match{
      case x : Empty[A] => None
      case x : Singleton[A] => Some(x.elt)
      case x : Join[A] =>  {
        val lst1Max : Option[A] = x.lst1.size match {
          case 0 => None
          case _ => max(x.lst1, compare)
        }
        val lst2Max : Option[A] = x.lst2.size match {
          case 0 => None
          case _ => max(x.lst2, compare)
        }

        lst1Max match {
          case Some(x) => lst2Max match {
            case Some(z) => 
              if (compare(x,z))
                Some(x)
              else
                Some(z)
            case None => 
              None
          }
          case None => lst2Max match {
            case Some(y) => Some(y)
            case None => None
          }
        } 
      } 
      case _ => None
    }
  }
  def first[A](lst: JoinList[A]): Option[A] = {
    lst match{
      case x : Empty[A] => None
      case x : Singleton[A] => Some(x.elt)
      case x : Join[A] =>  {
        if(x.lst1.size == 0 && x.lst2.size > 0)
          first(x.lst2)
        else if(x.lst1.size > 0 && x.lst2.size == 0)
          first(x.lst1)
        else if(x.lst1.size == 0 && x.lst2.size == 0)
          None
        else {
          first(x.lst1)
        }
      } 
      case _ => None
    }
  }

  def p_rest[A](lst: JoinList[A]) : JoinList[A] = {
    lst match {
      case x: Empty[A] => x
      case x: Singleton[A] => x

      case x: Join[A] =>
        x.lst1.size match {
          case 0 => 
            Join[A](p_rest(x.lst2), Empty[A], x.lst2.size - 1)
          case 1 => 
            x.lst2
          case _ =>
            Join[A](p_rest(x.lst1) , x.lst2, x.lst1.size - 1 + x.lst2.size)
      }
    }
  }

  def rest[A](lst: JoinList[A]): Option[JoinList[A]] = {
    lst.size match {
      case 0 => None
      case 1 => Some(Empty[A])
      case _ => { 
        Some(p_rest(lst))
      }
    }
  }

  def nth[A](lst: JoinList[A], n: Int): Option[A] = 
  {
    if(n < 0)
      None

    n match {
      case 0 => first(lst) 
      case _ => rest(lst) match {
        case Some(x) => nth(x, n - 1)
        case None => None
      }
    }
  }

  def map[A,B](f: A => B, lst: JoinList[A]): JoinList[B] = {
    if (lst.size == 0)
      Empty[B]

    lst match {
      case x : Empty[A] => Empty[B]
      case x : Singleton[A] => Singleton[B](f(x.elt))
      case x : Join[A] => 
        Join[B](map[A,B](f, x.lst1), map(f, x.lst2), lst.size)
    }
  }

  def filter[A](pred: A => Boolean, lst: JoinList[A]): JoinList[A] = {
    if (lst.size == 0)
      Empty[A]

    lst match {
      case x : Empty[A] => Empty[A]
      case x : Singleton[A] => pred(x.elt) match {
        case true => Singleton[A](x.elt)
        case false => Empty[A]
      }
      case x : Join[A] => 
        Join[A](filter[A](pred, x.lst1), filter(pred, x.lst2), lst.size)
    }
  }

}