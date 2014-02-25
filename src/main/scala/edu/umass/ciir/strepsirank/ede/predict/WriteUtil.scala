package edu.umass.ciir.strepsirank.ede.predict

import scala.collection.mutable
import java.io.{PrintWriter, File}
import edu.umass.ciir.strepsirank.ede.{TrecRunWriter, ScoredDocument}

object WriteUtil {

  def justWrite(outputFilePrefix: String, results: Map[Int, Seq[ScoredDocument]], qrels: String,
                queryPrefix:String="") {

    val linkedMap = mutable.LinkedHashMap[String, Seq[ScoredDocument]]()
    for ((queryId, queryResults) <- results) {
      linkedMap.put(queryPrefix + queryId, queryResults)
    }
    val resultMap = linkedMap.toMap

    TrecRunWriter.writeRunFile(new File(outputFilePrefix + ".run"), resultMap)

  }


  def writeOutputRun(outputFilePrefix: String, results: Map[Int, Seq[ScoredDocument]], queryPrefix:String="") = {

    val linkedMap = mutable.LinkedHashMap[String, Seq[ScoredDocument]]()
    for ((queryId, queryResults) <- results) {
      linkedMap.put(queryPrefix + queryId, queryResults)
    }
    val resultMap = linkedMap.toMap

    TrecRunWriter.writeRunFile(new File(outputFilePrefix + ".run"), resultMap)

  }

}
