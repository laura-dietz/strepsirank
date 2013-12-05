// todo delete
//package edu.umass.ciir.strepsirank.rank
//
//import ciir.umass.edu.learning.RankList
//import edu.umass.ciir.newsy.boltldc.qrel.ResultEntry
//import ciir.bolt.galago.prep.data.Post
//import edu.umass.ciir.newsy.boltldc.pipeline.ExperimentConfig
//
///**
// * User: dietz
// * Date: 1/8/13
// * Time: 6:17 PM
// */
//object RankTools {
//  def docidswithscoreToResultEntry(docIdWithScore: Seq[(String, Option[String], Double)]): Seq[ResultEntry] = {
//    val predictedRanking =
//      for (((docid, passage, score), rank) <- docIdWithScore.sortBy(-_._3).zipWithIndex) yield {
//        ResultEntry(docid = docid, passageInfo = passage, score = score, rank = rank)
//      }
//    predictedRanking
//  }
//
//
//  def rankListToSeq(rankList: RankList): IndexedSeq[ResultEntry] = {
//    val docidsWithScore =
//      for (idx <- 0 until rankList.size()) yield {
//        val desc = rankList.get(idx).getDescription
//        assert (desc.startsWith("#"))
//        val (docid, passage) = fromDescription(desc.substring(1).trim())
//        (docid, passage, -idx.toDouble)
//      }
//
//    docidswithscoreToResultEntry(docidsWithScore.toIndexedSeq).toIndexedSeq
//
//  }
//
//
//  val passageDelim = "-"
//
//  def getDescription(post:Post):String = {
//    post.docname+passageDelim+""
//  }
//
//  def getDescription(entry:ResultEntry):String = {
//    val pinfo = entry.passageInfo match {
//      case Some(p:String) => p
//      case None => ""
//    }
//
//    entry.docid+passageDelim+ pinfo
//  }
//
//  def fromDescription(desc:String):(String, Option[String]) = {
////    if(!desc.contains(passageDelim)) {
////      val docid = desc
////      (docid, None)
////    } else {
//      val lastIdx = desc.lastIndexOf(passageDelim)
//    if (lastIdx <0) throw new Error("Could not find passageDelim in sting \""+desc+"\"")
//      val docid = desc.substring(0, lastIdx)
//      val pinfo = desc.substring(lastIdx + passageDelim.length).trim()
//      if (pinfo == "") {
//        (docid, None)
//      } else {
//        (docid, Some(pinfo))
//      }
////    }
//  }
//
//}
//
