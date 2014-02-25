package edu.umass.ciir.strepsirank.ede.featgen

import collection.mutable

/**
 * User: jdalton
 * Date: 4/3/13
 */
class FeaturesToSvmConverter(domainMapFile:String) {

  val domainMap = {
    val domainMap = new mutable.HashMap[String, Int]()
    val f = io.Source.fromFile(domainMapFile)
    for (line <- f.getLines()) {
      val fields = line.split("\t")
      domainMap += (fields(0) -> fields(1).toInt)
    }
    domainMap
  }
  val m2eDomainSet = domainMap.map(_._1).toSet
  val map = m2eDomainSet.zipWithIndex.toMap
  val featureDomainMap = map
  println("Feature file: " + domainMapFile + " domain map loaded with: " + featureDomainMap.size + " entries")


  //  def setFeatureDomain(domain: Map[String, Int]) {
  //      featureDomainMap = domain
  //  }

  //   println("Domain size: " + featureDomainMap.size)
  //   for ((feature, featureIdx) <- featureDomainMap) {
  //     println(feature + "\t" + featureIdx)
  //   }

  def resultFeaturesToSvmFormat(query: Int, docId: String, features: Seq[(String, Double)], relevance:Int) : String = {
    val sb = new StringBuilder
    sb append relevance
    sb append " "
    sb append "qid:"
    sb append query
    sb append " "

    for ((feature, value) <- features) {
      val domain = featureDomainMap.getOrElse(feature, -1) + 1
      // skip some features
      if (domain > 0) {
        sb append domain
        sb append ":"
        sb append value
        sb append " "
      }
    }

    sb append "#"
    sb append " "
    sb append docId
    //println(sb.toString)
    sb.toString

  }

}
