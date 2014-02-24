package edu.umass.ciir.strepsirank.ede

import java.lang.String
import scala.Predef.String

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
    String.format("%s %d %s galago", documentName, rank, formatScore(score))
  }

  def toString(qid: String): String = {
    String.format("%s Q0 %s %d %s galago", qid, documentName, rank, formatScore(score))
  }

  def toTRECformat(qid: String): String = {
    String.format("%s Q0 %s %d %s galago", qid, documentName, rank, formatScore(score))
  }

  protected def formatScore(score: Double): String = {
    val difference: Double = Math.abs(score - score.asInstanceOf[Int])
    if (difference < 0.00001) {
      return Integer.toString(score.asInstanceOf[Int])
    }
    String.format("%10.8f", score)
  }
}
