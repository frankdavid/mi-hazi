package com.kismatka.mi.model

import org.joda.time.DateTime


case class Article(
                    title: String,
                    dateCreated: DateTime,
                    dateCompleted: Option[DateTime],
                    authors: Seq[String],
                    journal: String,
                    pubTypes: Seq[String],
                    keywords: Seq[String]
                    ) extends Ordered[Article]{
  def compare(that: Article): Int = title.compareTo(that.title)
  lazy val titleParts = title.toLowerCase.split("\\.*,*\\s+")
  override def toString = title
}
