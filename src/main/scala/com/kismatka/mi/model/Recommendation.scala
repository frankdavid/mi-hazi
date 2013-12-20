package com.kismatka.mi.model

case class Recommendation(recommended: Article, basedOn: Article, similarity: Similarity)