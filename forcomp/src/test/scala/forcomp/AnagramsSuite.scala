package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }
  
  /*test("temp") {
    def dup(add:(Char, Int)) = {
      {for {
        j <- 1 to add._2

      } yield List((add._1, j))}.toList:::List(List())
    }
    
    dup(('a', 2)).foreach(p => println(p))
    println("----------------")
    
    def adder(from: List[List[(Char, Int)]], add:(Char, Int)) = {
      {for {
        i <- from
        j <- {for {
          k <- 1 to add._2
        } yield List((add._1, k))}.toList:::List(List())
        
      } yield i:::j } 
    }
    
    //val temp = List(('a', 2), ('b', 2), ('c', 3))
    val temp = List(List())
    //val temp = List(List(), List(('a', 2)))
    
    adder(temp, ('d', 2)).foreach(l => println(l))
    println("------------")
    //adder(List(), ('d', 2)).foreach(l => println(l))
    
    def itr(from: List[List[(Char, Int)]], add:List[(Char, Int)]):List[List[(Char, Int)]] = add match {
      case List() => from
      case a::xs => itr(adder(from, a), xs)
    }
    
    itr(List(List()), List(('a', 2), ('b', 2), ('c', 3))).foreach(l => println(l))
  }*/

   test("subtract") {
      val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 3))
      val r = List(('a', 1), ('d', 2))
   

    
    
    def op(a:(Char, Int), b: (Char, Int)) = {
      if(a._1 == b._1) (a._1, a._2 - b._2) 
			else a
    }
      
      subtract(lard, r) match {
        case x::xs => println(x::xs)
        case List() => println("empty")
        case _ => println("error")
      }
         
        
    val lad = List(('a', 1), ('d', 1), ('l', 1))
   }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }
  
  test("sentence anagrams: heather") {

    
    val ex = sentenceAnagrams(List("heather"))
    ex.foreach(l => println("each: "+l))
    println("-------------------")
    val anas2 = List(List("re", "the", "ha"), List("he", "hat", "re"), List("three", "ha"), List("hare", "the"), List("her", "Thea"), List("at", "he", "her"), List("her", "et", "ha"), List("re", "ah", "the"), List("he", "he", "art"), List("hear", "the"), List("et", "her", "ha"), List("he", "re", "hat"), List("hat", "re", "he"), List("heather"), List("he", "heart"), List("re", "ha", "the"), List("here", "hat"), List("he", "at", "her"), List("he", "hater"), List("ah", "three"), List("her", "heat"), List("he", "earth"), List("there", "ha"), List("ha", "re", "the"), List("re", "heath"), List("et", "ha", "her"), List("ah", "et", "her"), List("ah", "her", "et"), List("re", "the", "ah"), List("he", "he", "rat"), List("rat", "he", "he"), List("ha", "ether"), List("ha", "her", "et"), List("he", "tar", "he"), List("he", "rat", "he"), List("at", "her", "he"), List("ha", "there"), List("ether", "ha"), List("art", "he", "he"), List("ah", "re", "the"), List("tar", "he", "he"), List("hate", "her"), List("the", "hear"), List("he", "art", "he"), List("earth", "he"), List("Rhea", "the"), List("three", "ah"), List("et", "ah", "her"), List("her", "et", "ah"), List("the", "hare"), List("the", "re", "ah"), List("re", "hat", "he"), List("her", "at", "he"), List("hat", "he", "re"), List("he", "he", "tar"), List("ha", "the", "re"), List("the", "Hera"), List("ah", "the", "re"), List("et", "her", "ah"), List("ha", "three"), List("heat", "her"), List("hater", "he"), List("the", "ha", "re"), List("he", "her", "at"), List("heart", "he"), List("there", "ah"), List("ah", "there"), List("ah", "ether"), List("Hera", "the"), List("her", "he", "at"), List("hat", "here"), List("ha", "et", "her"), List("the", "ah", "re"), List("her", "ah", "et"), List("ether", "ah"), List("the", "Rhea"), List("her", "hate"), List("re", "he", "hat"), List("heath", "re"), List("Thea", "her"), List("her", "ha", "et"), List("the", "re", "ha"))
    //anas2.foreach(l => println(l))
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    
    println(ex.toSet.size)
    println(anas2.size)
    
    assert(ex.toSet === anas2.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }
  test("sentence anagrams: Lukas Rytz") {
    val sentence = List("Lukas", "Rytz") 
    val anas = List(List("Ku", "Salz", "try"), List("Katz", "surly"), List("Salz", "try", "Ku"), List("Ku", "try", "Salz"), List("Salz", "Ku", "try"), List("try", "Ku", "Salz"), List("surly", "Katz"), List("try", "Salz", "Ku"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }
  

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    
    //println(sentenceAnagrams(sentence))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}

