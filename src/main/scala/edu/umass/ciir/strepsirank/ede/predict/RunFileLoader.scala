package edu.umass.ciir.strepsirank.ede.predict

import scala.collection.mutable.ListBuffer
import java.io._
import scala.io.Source
import scala.collection.immutable.Map
import scala.collection.JavaConversions._
import edu.umass.ciir.strepsirank.ede.ScoredDocument


object RunFileLoader {
  type QueryId = String
  case class QueryDocument(query:QueryId, name:String, m_rank:Int, m_score:Double, contextSource:String) extends ScoredDocument(name, m_rank, m_score)


  def readRunFileWithQuery(runFile: File, resultLimit: Int = 1000, prefix:String="") : Map[QueryId, Seq[ScoredDocument]] = {

    //    println("Loading run file: " + runFile.getAbsolutePath())
    val runResults = new ListBuffer[QueryDocument]()
    val source = Source.fromFile(runFile, "UTF-8")
    for (line <- source.getLines ) {
      val data = line.split("\\s+")
      if (data.length != 6) {
        throw new Exception("Wrong number of fields in line!" + line)
      }
      val doc = QueryDocument(data(0).replace(prefix, ""), data(2), data(3).toInt, data(4).toDouble, runFile.getName())
      doc.source = doc.contextSource
      runResults += doc
    }
    val resultsByQuery = runResults.groupBy(t => t.query).filterNot(queries => queries._1 equals "672")  // robust document 672 has no relevant documents. remove.
    val resultsbyQuerySorted = resultsByQuery.map( f => {
        val sorted = f._2.sortBy(d => d.rank) take resultLimit

        (f._1, sorted.toSeq)
      })
    //  resultsbyQuerySorted.map(f => Util.maxMinNorm(f._2))
    resultsbyQuerySorted
  }


}