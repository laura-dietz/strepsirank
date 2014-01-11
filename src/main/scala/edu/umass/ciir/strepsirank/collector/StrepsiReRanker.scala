package edu.umass.ciir.strepsirank.collector

import ciir.umass.edu.learning.RANKER_TYPE
import edu.umass.ciir.strepsirank.rank.{FeatureVec, RankTools}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import ciir.umass.edu.metric.APScorer
import edu.umass.ciir.strepsirank.rank.RankTools._
import scala.Some

/**
 * User: dietz
 * Date: 12/9/13
 * Time: 6:30 PM
 */
class StrepsiReRanker(featureCollector: StrepsiFeatureCollector, rankerType: RANKER_TYPE) {
  def trainTestSplit(trainQueries: Set[String], defaultFeatures: Option[Seq[(String, Double)]], testQueries: Option[Set[String]], modelfilename: Option[String] = None): Option[MultiRankings] = {
    val data = featureCollector

    val trainRankings = constructFeatureVectors(trainQueries, data, defaultFeatures)
    val testQuerySet = if (testQueries.isDefined) {
      testQueries.get
    } else data.keySet.toSet diff trainQueries
    val testRankings = constructFeatureVectors(testQuerySet, data, defaultFeatures)

    //    val resultOpt = RankTools.trainPredict(rankerType, new APScorer(), trainRankings, None,None)
    val resultOpt = RankTools.trainPredict(rankerType, new APScorer(), trainRankings, modelfilename, Some(testRankings))

    resultOpt
  }


  def constructFeatureVectors(querySet: Set[String]
                              , data: mutable.HashMap[String, mutable.HashMap[String, (Option[Int], mutable.Buffer[(String, Double)])]]
                              , defaultFeatures: Option[Seq[(String, Double)]]): MultiRankings = {
    val multiRankings = new ListBuffer[(String, Seq[FeatureVec])]
    for (query <- querySet) {
      val qdataOpt = data.get(query)
      if (qdataOpt == None) {
        //        throw new RuntimeException("Can't find features for query " + query + ". Given features for queries " + data.keySet.mkString("(", ",", ")"))
        System.err.println("======================================================")
        System.err.println("======================================================")
        System.err.println("======================================================")
        System.err.println("Can't find features for query " + query + ". Given features for queries " + data.keySet.mkString("(", ",", ")"))
        System.err.println("======================================================")
        System.err.println("======================================================")
      } else {

        val qdata = qdataOpt.get
        val vecList = new ListBuffer[FeatureVec]
        for ((doc, (classLabel, features)) <- qdata) {
          val ffeatures =
            if (defaultFeatures.isEmpty) features
            else {
              val featMap = features.toMap
              val fullfeatures =
                for ((featname, defaultValue) <- defaultFeatures.get) yield {
                  if (featMap.contains(featname)) featname -> featMap(featname)
                  else featname -> defaultValue
                }
              //            features.filter(pair => defaultFeatures.get.contains(pair._1))
              fullfeatures
            }
          vecList += RankTools.createFeatureVec(doc, ffeatures, classLabel)
        }
        multiRankings += (query -> vecList)
      }
    }
    multiRankings
  }
}
