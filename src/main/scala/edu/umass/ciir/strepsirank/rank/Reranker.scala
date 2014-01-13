package edu.umass.ciir.strepsirank.rank

import ciir.umass.edu.learning.{Ranker, RANKER_TYPE, RankerTrainer}
import ciir.umass.edu.metric.{MetricScorer, APScorer}
import scala.collection.JavaConversions._
import RankTools.MultiRankings

/**
 * Scala friendly interface to the re-ranking classes.
 *
 *
 * User: dietz
 * Date: 12/6/13
 * Time: 5:04 PM
 */
class Reranker(rankertype: RANKER_TYPE = RANKER_TYPE.COOR_ASCENT, metricScorer: MetricScorer = new APScorer()) {

  def trainPredict(train: MultiRankings, modelfilename: Option[String] = None, testData: Option[MultiRankings] = None, submitTrainScore: (Double, String) => Unit = Reranker.printTrainScore, submitValidationScore: (Double, String) => Unit = Reranker.printValidationScore): Option[MultiRankings] = {
    val rankListConv = new RankListConv(trackIgnoreFeature = false, ignoreNewFeatures = false)

    val trainingList = rankListConv.multiDataToRankList(train)



    val rt = new RankerTrainer()
    val featureIndices = rankListConv.fc.featureIndices

    val ranker = if (testData.isDefined) {
      val validationList = rankListConv.multiDataToRankList(testData.get)
      val r = rt.train(rankertype, trainingList, validationList, featureIndices, metricScorer)
      //      println(metricScorer.name + " on training data: " + r.getScoreOnTrainingData)
      //      println(metricScorer.name + " on validation data: " + r.getScoreOnValidationData)
      submitTrainScore(r.getScoreOnTrainingData, metricScorer.name)
      submitValidationScore(r.getScoreOnValidationData, metricScorer.name)
      r
    } else {
      val r = rt.train(rankertype, trainingList, featureIndices, metricScorer)
      //      println(metricScorer.name + " on training data: " + r.getScoreOnTrainingData)
      submitTrainScore(r.getScoreOnTrainingData, metricScorer.name)
      r
    }





    modelfilename match {
      case Some(filename) =>
        ranker.save(filename + ".ranklib")
        rankListConv.fc.save(filename + ".featureconv")
      case _ =>
    }

    testData.map {
      test => {
        predict(rankListConv, test, ranker)
      }
    }
  }


  private def predict(rankListConv: RankListConv, test: RankTools.MultiRankings, ranker: Ranker): RankTools.MultiRankings = {
    val toPredictList = rankListConv.multiDataToRankList(test)
    val rankLists = ranker.rank(toPredictList)
    val predictedRankings = rankListConv.rankListsToMultiData(
      rankLists, test, ranker.eval)
    predictedRankings
  }

  def loadPredict(modelfilename: String, testData: MultiRankings): MultiRankings = {
    val rt2 = new RankerTrainer()
    val rankListConv = new RankListConv(trackIgnoreFeature = false, ignoreNewFeatures = true)
    rankListConv.fc.load(modelfilename + ".featureconv")
    val ranker = rt2.createEmptyRanker(rankertype, rankListConv.fc.featureIndices, metricScorer)
    ranker.load(modelfilename + ".ranklib")

    predict(rankListConv, testData, ranker)
  }


}

object Reranker {
  val printTrainScore: (Double, String) => Unit = {
    (score, name) => println(name + " on training data: " + score)
  }
  val printValidationScore: (Double, String) => Unit = {
    (score, name) => println(name + " on validation data: " + score)
  }
}

