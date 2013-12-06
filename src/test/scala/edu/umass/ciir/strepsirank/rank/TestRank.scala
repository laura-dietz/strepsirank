package edu.umass.ciir.strepsirank.rank

import edu.umass.ciir.strepsirank.rank.antirank.{LabeledFeatureVec, RankListConv}
import scala.collection.mutable.ListBuffer
import ciir.umass.edu.learning._
import ciir.umass.edu.metric.APScorer
import scala.collection.JavaConversions._

/**
 * User: dietz
 * Date: 12/5/13
 * Time: 6:34 PM
 */
object TestRank {

  def main(args:Array[String]){
    val rankListConv = new RankListConv(trackIgnoreFeature = false, ignoreNewFeatures = false)
    val filename = "rankmodel"

    val train = new ListBuffer[LabeledFeatureVec]()
    train += new LabeledFeatureVec(Seq("a"->1,"b"->0),1, "train1-1")
    train += new LabeledFeatureVec(Seq("a"->1,"b"->0.5),1, "train1-2")
    train += new LabeledFeatureVec(Seq("a"->0,"b"->0),0, "train0-1")
    train += new LabeledFeatureVec(Seq("a"->0.1,"b"->0.5),0, "train0-2")
    
    val test = new ListBuffer[LabeledFeatureVec]()
    test += new LabeledFeatureVec(Seq("a"->0.2,"b"->0),0, "test0-1")
    test += new LabeledFeatureVec(Seq("a"->0.9,"b"->0),1, "test1-1")
    test += new LabeledFeatureVec(Seq("a"->1,"b"->0.9, "c" -> 0.5),1, "test1-2")
    test += new LabeledFeatureVec(Seq("a"->0.0,"b"->0.9),0, "test0-2")
    
    val trainingList = rankListConv.createRankList(train, "1")
    val toPredictList = rankListConv.createRankList(test, "2")

    trainTestAndSave(rankListConv, trainingList, filename, toPredictList)


    println("==============\n============\n============")
    // ============ test re-loading =======

    loadAndPredict(trainingList, filename, test)


  }


  def trainTestAndSave(rankListConv: RankListConv, trainingList: RankList, filename: String, toPredictList: RankList) {
    val rt = new RankerTrainer()
    val featureIndices = rankListConv.fc.featureIndices

    val ranker = rt.train(RANKER_TYPE.COOR_ASCENT, List(trainingList), featureIndices, new APScorer())

    ranker.save(filename + ".ranklib")
    rankListConv.fc.save(filename + ".featureconv")
    val predictedRanking = rankListToSeq(ranker.rank(toPredictList), ranker.eval)
    predictedRanking.foreach(println _)
  }

  def loadAndPredict(trainingList: RankList, filename: String, test: ListBuffer[LabeledFeatureVec]) {
    val rt2 = new RankerTrainer()
    val rankListConv2 = new RankListConv(trackIgnoreFeature = false, ignoreNewFeatures = false)
    rankListConv2.fc.load(filename + ".featureconv")

    val ranker2 = rt2.createEmptyRanker(RANKER_TYPE.COOR_ASCENT, rankListConv2.fc.featureIndices, new APScorer())
    ranker2.load(filename + ".ranklib")

    val toPredictList2 = rankListConv2.createRankList(test, "2")

    val predictedRanking2 = rankListToSeq(ranker2.rank(toPredictList2), ranker2.eval)
    predictedRanking2.foreach(println _)
  }

  def train(rankListConv: RankListConv, train: ListBuffer[LabeledFeatureVec], test: ListBuffer[LabeledFeatureVec], filename: String): (Array[Int], RankList) = {
    val trainingList = rankListConv.createRankList(train, "1")
    val toPredictList = rankListConv.createRankList(test, "2")

    val rt = new RankerTrainer()
    val featureIndices = rankListConv.fc.featureIndices

    val ranker = rt.train(RANKER_TYPE.COOR_ASCENT, List(trainingList), featureIndices, new APScorer())

    ranker.save(filename + ".ranklib")
    rankListConv.fc.save(filename + ".featureconv")
    val predictedRanking = rankListToSeq(ranker.rank(toPredictList), ranker.eval)


    predictedRanking.foreach(println _)
    (featureIndices, trainingList)
  }

  def rankListToSeq(rankList: RankList, scorer:(DataPoint => Double)): IndexedSeq[String] = {
    val ranking =
      for (idx <- 0 until rankList.size()) yield {
        val datapoint = rankList.get(idx)
        val desc = datapoint.getDescription
        val score = scorer(datapoint)
        assert (desc.startsWith("#"))
        //        println(desc)
        val datadesc = desc.substring(1).trim()
        //        println(docid)
        //        println(passage)
        datadesc
      }

    ranking

  }

}
