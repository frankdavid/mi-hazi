package com.kismatka.mi

import com.kismatka.mi.model.Article
import java.io.{FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream}
import scala.util.Random

object Program extends App {
  val data = PubMedLoader.load("Test", 400)
//  private val stream = new ObjectOutputStream(new FileOutputStream("data.dat"))
//  stream.writeObject(data)
//  stream.close()

//  val data = new ObjectInputStream(new FileInputStream("data.dat")).readObject().asInstanceOf[IndexedSeq[Article]]
//  println(data.size)
//  println((Set() ++ data).size)


  println("XML Loaded")
  val favs = (1 to 3) map { x =>
    data(Random.nextInt(data.size))
  }
  val recommendationsMax =  Recommender.findClosestByMax(favs, data, 3)
  val recommendationsAvg =  Recommender.findClosestByAvg(favs, data, 3)
  val recommendationsMin =  Recommender.findClosestByMin(favs, data, 3)

  println("MAX: ")
  for (recommendation <- recommendationsMax) {
//    println("Recommendation: ")
    display(recommendation.recommended)
//    println("Based on:")
//    display(recommendation.basedOn)
    println("---------------------")
    println()
  }
  println("AVG: ")

  for (recommendation <- recommendationsAvg) {
//    println("Recommendation: ")
    display(recommendation.recommended)
    println("---------------------")
    println()
  }

  println("MIN: ")
  for (recommendation <- recommendationsMin) {
    //    println("Recommendation: ")
    display(recommendation.recommended)
    println("---------------------")
    println()
  }


  def display(article: Article) {
    println("Title:\t" + article.title)
    println("Authors:\t" + article.authors.mkString(", "))
    println("Journal:\t" + article.journal)
    println("Keywords:\t" + article.keywords.mkString(", "))
    println("Pub Types:\t" + article.pubTypes.mkString(", "))
  }
}
