package com.kismatka.mi

import com.kismatka.mi.model.{Recommendation, Similarity, Article}
import scala.collection.mutable

object Recommender {

  val distanceCache = mutable.HashMap[(Article, Article), Double]()

  def findClosest(article: Article, articles: Seq[Article]) = {
    articles.filter(_ != article).maxBy(distance(article, _))
  }

  def findClosestByMax(favoriteArticles: Seq[Article], allArticles: Seq[Article], num: Int) = {
    val queue = mutable.PriorityQueue[(Article, (Article, Double))]()(new Ordering[(Article, (Article, Double))]{
      def compare(x: (Article, (Article, Double)), y: (Article, (Article, Double))): Int = x._2._2.compareTo(y._2._2)
    })
    queue.enqueue(allArticles.filterNot(favoriteArticles.contains(_)).
      map(article =>(article, favoriteArticles.map(fav => (fav, distance(article, fav))).maxBy(_._2))) : _*)
    for(i <- 0 until num) yield {
      val closest = queue.dequeue()
      Recommendation(closest._1, closest._2._1, similarity(closest._1, closest._2._1))
    }
//    val closest = allArticles.filterNot(favoriteArticles.contains(_)).
//      map(article =>(article, favoriteArticles.map(fav => (fav, distance(article, fav))).maxBy(_._2))).maxBy(_._2._2) // max distance
//    Recommendation(closest._1, closest._2._1, similarity(closest._1, closest._2._1))
  }
  def findClosestByAvg(favoriteArticles: Seq[Article], allArticles: Seq[Article], num: Int) = {
    val queue = mutable.PriorityQueue[(Article, Double)]()(new Ordering[(Article, Double)]{
      def compare(x: (Article, Double), y: (Article, Double)): Int = x._2.compareTo(y._2)
    })
    queue.enqueue(allArticles.filterNot(favoriteArticles.contains(_)).
      map(article =>(article, favoriteArticles.map(fav => distance(article, fav)).sum / favoriteArticles.size.toDouble)) : _*)
    for(i <- 0 until num) yield {
      val closest = queue.dequeue()
      Recommendation(closest._1, null, null)
    }
  }

  def findClosestByMin(favoriteArticles: Seq[Article], allArticles: Seq[Article], num: Int) = {
    val queue = mutable.PriorityQueue[(Article, Double)]()(new Ordering[(Article, Double)]{
      def compare(x: (Article, Double), y: (Article, Double)): Int = x._2.compareTo(y._2)
    })
    queue.enqueue(allArticles.filterNot(favoriteArticles.contains(_)).
      map(article =>(article, favoriteArticles.map(fav => distance(article, fav)).min)) : _*)
    for(i <- 0 until num) yield {
      val closest = queue.dequeue()
      Recommendation(closest._1, null, null)
    }
  }


  def distance(article1: Article, article2: Article): Double = {
    val tuple = if (article1 < article2) (article1, article2) else (article2, article1)
    distanceCache.getOrElseUpdate(tuple, calculateDistance(article1, article2))
  }

  def similarity(article1: Article, article2: Article): Similarity =
    Similarity(
      journal = journalDistance(article1, article2),
      authors = authorsDistance(article1, article2),
      keywords = keywordsDistance(article1, article2),
      pubTypes = pubTypesDistance(article1, article2),
      title = titleDistance(article1, article2))


  def calculateDistance(article1: Article, article2: Article): Double =
      calculateDistance(similarity(article1, article2))


  def calculateDistance(similarity: Similarity): Double =
    similarity.journal + similarity.authors + 2 * similarity.keywords + 0.6 * similarity.pubTypes + 2 * similarity.title

  def titleDistance(article1: Article, article2: Article) = stringSeqDistance(article1.titleParts, article2.titleParts)

  def pubTypesDistance(article1: Article, article2: Article) = stringSeqDistance(article1.pubTypes, article2.pubTypes)

  def keywordsDistance(article1: Article, article2: Article) = stringSeqDistance(article1.keywords, article2.keywords)

  def authorsDistance(article1: Article, article2: Article) = stringSeqDistance(article1.authors, article2.authors)

  def journalDistance(article1: Article, article2: Article) = stringDistance(article1.journal, article2.journal)

  def stringSeqDistance(s1: Seq[String], s2: Seq[String]) = s1.count(s2.contains) / (1 max s1.size max s2.size).toDouble

  def stringDistance(s1: String, s2: String) = if (s1 == s2) 1 else 0

}
