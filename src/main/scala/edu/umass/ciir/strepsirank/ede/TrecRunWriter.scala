package edu.umass.ciir.strepsirank.ede


/**
 * User: jdalton
 * Date: 6/5/13
 */
object TrecRunWriter {



  def writeRunFile(f: java.io.File, results: Map[String, Seq[ScoredDocument]], runId : String = "Default")  {
    val p = new java.io.PrintWriter(f, "UTF-8")
    try {
      for( (query, results) <- results) {
        results.map(r => {
          if (r == null){
            println("where does this null result come from?")
          }
          if (r.documentName == null){
            println("documentname == null "+r)
          }
          if (r.rank == null){
            println("rank == null "+r)
          }
          p.println("%s Q0 %s %d %s %s".format(query, r.documentName, r.rank, "%10.8f".format(r.score), runId))
        })
      }
    } finally { p.close() }
  }


  case class TrecResultDocument(docname:String, score:Double, rank:Int)
  case class TrecRanking(query:String, ranking:Seq[TrecResultDocument])

  def writeRunFileFromRanking(f: java.io.File, results: Seq[TrecRanking], runId : String = "Default") {
    val p = new java.io.PrintWriter(f, "UTF-8")
    try {
      for( TrecRanking(query, results) <- results) {
        results.map(r => p.println("%s Q0 %s %d %s %s".format(query, r.docname, r.rank, "%10.8f".format(r.score), runId)))
      }
    } finally { p.close() }
  }


  def writeRunFileFromTuple(f: java.io.File, results: Seq[(String, Seq[(String,Double, Int)])], runId : String = "Default") {
    val p = new java.io.PrintWriter(f, "UTF-8")
    try {
      for( (query, results) <- results) {
        results.map(r => p.println("%s Q0 %s %d %s %s".format(query, r._1, r._3, "%10.8f".format(r._2), runId)))
      }
    } finally { p.close() }
  }


  def writeLines(f: java.io.File, lines: Seq[String]) {
    val p = new java.io.PrintWriter(f, "UTF-8")
    try {
      for( line <- lines) {
        p.println(line)
      }
    } finally { p.close() }
  }

}
