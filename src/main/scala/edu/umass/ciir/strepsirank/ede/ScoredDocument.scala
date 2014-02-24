package edu.umass.ciir.strepsirank.ede


/**
 * User: dietz
 * Date: 2/24/14
 * Time: 3:50 PM
 */
class ScoredDocument(val documentName: String, val rank: Int, val score: Double) {
  var _source:String=""
  def source = _source
  def source_=(s:String) {
    _source = s
  }

  def withNewRank(newrank:Int):ScoredDocument = {
    val x = new ScoredDocument(documentName, newrank, score)
    x.source = _source
    x
  }


  override def toString: String = {
    documentName + " " + rank + " " + formatScore(score) + " galago"
  }

  def toString(qid: String): String = {
    qid + " Q0 " + documentName + " " + rank + " " + formatScore(score) + " galago"
  }

  def toTRECformat(qid: String): String = {
    qid + " Q0 " + documentName + " " + rank + " " + formatScore(score) + " galago"
  }

  protected def formatScore(score: Double): String = {
    val difference: Double = Math.abs(score - score.asInstanceOf[Int])
    if (difference < 0.00001) {
      Integer.toString(score.asInstanceOf[Int])
    } else {
      "%10.8f".format(score)
    }
  }
}
