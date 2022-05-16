package at.tugraz.ist.qs2022

import at.tugraz.ist.qs2022.simple.SimpleFunctions
import at.tugraz.ist.qs2022.simple.SimpleFunctions._
//import at.tugraz.ist.qs2022.simple.SimpleFunctionsMutant1._
//import at.tugraz.ist.qs2022.simple.SimpleFunctionsMutant2._
//import at.tugraz.ist.qs2022.simple.SimpleFunctionsMutant3._
//import at.tugraz.ist.qs2022.simple.SimpleFunctionsMutant4._
import at.tugraz.ist.qs2022.simple.SimpleJavaFunctions
import org.junit.runner.RunWith
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}

// Consult the following scalacheck documentation
// https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#concepts
// https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#generators

@RunWith(classOf[ScalaCheckJUnitPropertiesRunner])
class SimpleFunctionsTest extends Properties("SimpleFunctionsTest") {

  // Gen is some sort of function from scala check,
  // it is responsible to provide you random generated test data
  private val nonEmptyIntListGen: Gen[List[Int]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int])

  // insertionSort Java style
  property("insertionSort Java: ordered") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val sorted = SimpleJavaFunctions.insertionSort(xs.toArray)
    var correctFlag = true;
    if (xs.nonEmpty) {
      for (i <- 0 until sorted.length - 1) {
        if (sorted(i) > sorted(i + 1))
          correctFlag = false;
      }
      correctFlag // would be the return val
    }
    else
      false // returns false if xs is empty
  }

  // insertionSort the beautiful scala way
  property("insertionSort: ordered") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val sorted = insertionSort(xs)
    xs.nonEmpty ==> xs.indices.tail.forall((i: Int) => sorted(i - 1) <= sorted(i))
  }
  property("insertionSort: permutation") = forAll { (xs: List[Int]) =>
    val sorted = insertionSort(xs)

    def count(a: Int, as: List[Int]) = as.count(_ == a)

    xs.forall((x: Int) => count(x, xs) == count(x, sorted))
  }


  // maximum
  property("max: biggest element returned") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val max = SimpleFunctions.max(xs)
    xs.nonEmpty ==> xs.forall((x: Int) => x <= max)
  }

  // minimal index
  property("minIdx: minimal Index returned") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val minIdx = SimpleFunctions.minIndex(xs)
    xs.nonEmpty ==> xs.indices.forall((i: Int) => xs(i) >= xs(minIdx))
  }

  property("minIdx: positive Index returned") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val minIdx = SimpleFunctions.minIndex(xs)
    minIdx >= 0
  }

  // symmetric difference
  property("symmetricDifference: each element must ONLY be in one of the lists") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int],list2: List[Int]) =>
    val diff = SimpleFunctions.symmetricDifference(list1,list2)
    diff.indices.forall((i: Int) => list1.contains(diff(i)) && !list2.contains(diff(i)) || !list1.contains(diff(i)) && list2.contains(diff(i)) )
  }
  // TODO: distinct list as a property??
  property("symmentricDifference: Lists must be distinct") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int], list2: List[Int]) =>
    val temp = list2.toSet
    val result1 = list1.filterNot(temp)
    val temp2 = list1.toSet
    val result2 = list2.filterNot(temp2)
    //result1.nonEmpty && result2.nonEmpty
    //list1 != list2
    true
  }
  property("symmetricDifference: output must only have the size of the input at max") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int], list2: List[Int]) =>
    val diff = SimpleFunctions.symmetricDifference(list1, list2)
    list1.nonEmpty ==> list2.nonEmpty ==> diff.size <= list2.size + list1.size
  }

  property("symmetricDifference: non empty lists") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int], list2: List[Int]) =>
    list1.nonEmpty && list2.nonEmpty
  }

  property("intersection: non empty lists") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int], list2: List[Int]) =>
    list1.nonEmpty && list2.nonEmpty
  }
  // intersection
  property("intersection: each element in intersect must be in both of the lists") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int],list2: List[Int]) =>
    val intersect = SimpleFunctions.intersection(list1,list2)
    list1.nonEmpty ==> list2.nonEmpty ==> intersect.indices.forall((i: Int) => list1.contains(intersect(i)) && list2.contains(intersect(i)))
  }

  property("intersection: output must be size of the smaller list at max") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int], list2: List[Int]) =>
    val intersect = SimpleFunctions.intersection(list1, list2)
    val smaller = if (list1.size < list2.size) list1.size else list2.size
    list1.nonEmpty ==> list2.nonEmpty ==> intersect.size <= smaller
  }

  property("intersection: Lists must be distinct") = forAll(nonEmptyIntListGen, nonEmptyIntListGen) { (list1: List[Int], list2: List[Int]) =>
    val temp = list2.toSet
    val result1 = list1.filterNot(temp)
    val temp2 = list1.toSet
    val result2 = list2.filterNot(temp2)
    //result1.nonEmpty && result2.nonEmpty
    list1 != list2
    true
  }

  // Smallest missing positive integer
  property("smallestMissingPositiveInteger: non empty list") = forAll(nonEmptyIntListGen) { (list1: List[Int]) =>
    list1 != null
  }

  property("smallestMissingPositiveInteger: return positive int") = forAll(nonEmptyIntListGen) { (list1: List[Int]) =>
    val smallestInt = SimpleFunctions.smallestMissingPositiveInteger(list1)
    smallestInt > 0
  }

  property("smallestMissingPositiveInteger: return smallest int") = forAll(nonEmptyIntListGen) { (list1: List[Int]) =>
    val smallestInt = SimpleFunctions.smallestMissingPositiveInteger(list1)
    list1.nonEmpty ==> !list1.contains(smallestInt) ==> checkSmallest(smallestInt-1, list1)
  }

  def checkSmallest(smallest: Int, comp: List[Int]): Boolean = {
    if (comp.contains(smallest)) {
      if (smallest == 0) true else checkSmallest(smallest - 1, comp)
    } else if(smallest == 0) {
      true
    }else {
      false
    }
  }
}
