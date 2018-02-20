object Homework3 {

  import edu.umass.cs.CSV


  // WARNING: this may take a very long time. Cut the file or work with a
  // small, made-up dataset if you have trouble.
  // val allBirths = CSV.fromFile("ssa-births.csv")

  val lifeExpectancy = CSV.fromFile("cdc-life-expectancy.csv")

  /** Restrict the data to the year `year`. */
  def yearIs(data: List[List[String]], n: Int): List[List[String]] = {
    data.filter(x => x(0).toInt == n)
  }


  /** Restrict the data to years greater than `bound`. */
  def yearGT(data: List[List[String]], bound: Int): List[List[String]] = {
    data.filter(x => x(0).toInt > bound)
  }

  /** Restrict the data to years less than `bound` */
  def yearLT(data: List[List[String]], bound: Int): List[List[String]] = {
    data.filter(x => x(0).toInt < bound)
  }

  /** Restrict the data to the name `name`. */
  def onlyName(data: List[List[String]], name: String): List[List[String]] = {
    data.filter(x => x(1) == name)
  }

  /** Calculate the most popular name and the number of children born with
      that name. */
  def mostPopular(data: List[List[String]]): (String, Int) = {
    data.groupBy(x=> x(1)).mapValues(_.map(_(3))).mapValues(_.foldLeft(0)(_+_.toInt)).maxBy(_._2)
  }
  /** Calculate the number of children born in the given dataset. */
  def count(data: List[List[String]]): Int = {
    data.map(_(3)).foldLeft(0)(_ + _.toInt)
  }


  /** Produce a tuple with the number of girls and boys respectively. */
  def countGirlsAndBoys(data: List[List[String]]): (Int, Int) = {
    (count(data.filter(x => x(2) == "F")), count(data.filter(x => x(2) == "M")))
  }

  /** Calculate the set of names that are given to both girls and boys. */
  def unisexNames(data: List[List[String]]): Set[String] = {
    
    val trimmedData = data.map(x => List(x(1),x(2)))
    val females = trimmedData.filter(_(1) == "F").map(x => x(0)).distinct
    val males = trimmedData.filter(_(1) == "M").map(x => x(0)).distinct
    females.map{ case x:String => 
      if (males.contains(x)) x
      else ""
    }.filterNot(x => x.isEmpty).toSet
  }


  /** Determine if a person with the specified `gender` (either "M" or "F") who
      was born in `birthYear` is expected to be alive, according to the CDC
      life-expectancy data.

      If `currentYear` is the last year the person is estimated to be alive, be
      optimistic and produce `true`.

      The CDC data only ranges from 1930 -- 2010. Therefore, assume that
      `birthYear` is in this range too. */
  def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = {
    val expectancyDataRow = lifeExpectancy.find(x => x(0).toInt == birthYear) match { case Some(x) => x
      case _ => List()}
    val expectedDeathYear = birthYear + (gender match {
      case "M" => expectancyDataRow(1).toInt
      case "F" => expectancyDataRow(2).toInt
      case _ => 0
    })

    currentYear <= expectedDeathYear

  }

  def p_getExpectedAliveYear(gender: String, birthYear: Int) : Int = {
    val expectancyDataRow = lifeExpectancy.find(x => x(0).toInt == birthYear) match { case Some(x) => x
      case _ => List()}
      gender match {
        case "M" => birthYear + expectancyDataRow(1).toInt
        case "F" => birthYear + expectancyDataRow(2).toInt
        case _ => 0
      }
  }

  /** Estimate how many people from `data` will be alive in `year`. */
  def estimatePopulation(data: List[List[String]], year: Int): Int = {
    if(year < 1930) 0

    val data_trimmed = yearLT(yearGT(data, 1929), 2011).map(x => List(x(0),x(2),x(3)))
    //(0: year, 1: gender, 2: count)

    data_trimmed.foldLeft(0){ (n, x: List[String]) => 
      if(year >= x(0).toInt && year <= p_getExpectedAliveYear(x(1), x(0).toInt)) n + x(2).toInt
      else n + 0
    }
  }

}