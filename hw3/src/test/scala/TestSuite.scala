// You may modify or delete this file
class TestSuite extends org.scalatest.FunSuite {

  import edu.umass.cs.CSV
  import Homework3._

  val le = CSV.fromFile("cdc-life-expectancy.csv")
  val ssa = CSV.fromFile("ssa-births.csv")
  val simple = CSV.fromFile("simple-test.csv")

  test("Have life expectancies from 1930 -- 2010") {
    assert(le.map(x => x(0).toInt).reverse
           == 1930.to(2010))
  }

  test("birth filter 1880") {
  	assert(yearIs(ssa, 1880).length == 2000)
  }

  test("yearGT(2007) life expectancies") {
  	assert(yearGT(le, 2007).length == 3)
  }

  test("yearLT(2007) life expectancies") {
  	assert(yearLT(le, 2007).length == 77)
  }

  test("onlyName Margaret is 254") {
  	assert(onlyName(ssa, "Margaret").length == 254)
  }

  test("mostPopular on simple list is (James,9018)") {
  	assert(mostPopular(simple) match { case ("James", 9018) => true
  		case _ => false})
  }

  test("count on simple list is 9275"){
  	assert(count(simple) == 9276)
  }

  test("countGirlsAndBoys on simple list is (9272,4)"){
  	assert(countGirlsAndBoys(simple) == (9272,4))
  }

  test("count equals countGirlsAndBoys on ssa-births"){
  	val girlsAndBoys = countGirlsAndBoys(ssa)
  	assert(count(ssa) == girlsAndBoys._1 + girlsAndBoys._2)
  }

  test("unisexnames on simple list is 2"){
  	assert(unisexNames(simple).size == 2)
  }

  test("I am expected to be alive 2066, not 2067"){
  	assert(expectedAlive("M",1994,2066))
  	assert(!expectedAlive("M",1994,2067))
  }

  test("estimate popluation works on simple list"){
  	assert(estimatePopulation(simple,2066) == 2)
  	assert(estimatePopulation(simple,2068) == 1)
  	assert(estimatePopulation(simple,2069) == 0)
  }

  test("compound queries work"){
  	val tst = countGirlsAndBoys(yearGT(yearLT(simple, 2001), 1989))
  	assert((tst._1 + tst._2) > 0)
  	assert(count(onlyName(yearIs(simple, 1994), "Tony")) == count(onlyName(yearIs(simple, 1995), "Tony")))
  	assert(mostPopular(yearIs(simple, 1882)) == ("Sena", 19))
  	assert(estimatePopulation(yearGT(simple, 1993), 2015) == 2)
  }

}