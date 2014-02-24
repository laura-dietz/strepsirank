package edu.umass.ciir.strepsirank.ede

import scala.collection.mutable
import java.io.{PrintWriter, File}

object EvalUtil {

  def justWrite(outputFilePrefix: String, results: Map[Int, Seq[ScoredDocument]], qrels: String,
                queryPrefix:String="") {

    val linkedMap = mutable.LinkedHashMap[String, Seq[ScoredDocument]]()
    for ((queryId, queryResults) <- results) {
      linkedMap.put(queryPrefix + queryId, queryResults)
    }
    val resultMap = linkedMap.toMap

    TrecRunWriter.writeRunFile(new File(outputFilePrefix + ".run"), resultMap)

  }

//  def evalAndWrite(outputFilePrefix: String, results: Map[Int, Seq[ScoredDocument]], qrels: String, queryPrefix:String=""): Seq[(String, Double)] = {
//
//    val linkedMap = mutable.LinkedHashMap[String, Seq[ScoredDocument]]()
//    for ((queryId, queryResults) <- results) {
//      linkedMap.put(queryPrefix + queryId, queryResults)
//    }
//    val resultMap = linkedMap.toMap
//
//    TrecRunWriter.writeRunFile(new File(outputFilePrefix + ".run"), resultMap)
//
//    val pw = new PrintWriter(outputFilePrefix + ".eval")
//    val evaluation = GalagoEvaluator.evaluate(qrels, resultMap, Some(pw))
//    pw.close()
//
//    // val evalFormat = "%2$-16s%1$3s %3$6.5f";
//    //  evaluation._2.map(m => println(evalFormat.format(m.queryId, m.metric, m.evalScore)))
//    evaluation._1
//
//  }

  def writeOutputRun(outputFilePrefix: String, results: Map[Int, Seq[ScoredDocument]], queryPrefix:String="") = {

    val linkedMap = mutable.LinkedHashMap[String, Seq[ScoredDocument]]()
    for ((queryId, queryResults) <- results) {
      linkedMap.put(queryPrefix + queryId, queryResults)
    }
    val resultMap = linkedMap.toMap

    TrecRunWriter.writeRunFile(new File(outputFilePrefix + ".run"), resultMap)

  }

}
