package edu.umass.ciir.strepsirank.rank

import ciir.umass.edu.learning.RANKER_TYPE
import ciir.umass.edu.metric.{APScorer, MetricScorer}

/**
 * User: dietz
 * Date: 12/9/13
 * Time: 3:53 PM
 */
object RankTools {
  type MultiRankings = Seq[(String, Seq[FeatureVec])]

  def trainPredict(rankertype:RANKER_TYPE, metricScorer:MetricScorer, train:MultiRankings, modelfilename:Option[String]=None, testData:Option[MultiRankings]=None):Option[MultiRankings] = {
    new Reranker(rankertype, metricScorer).trainPredict(train, modelfilename, testData)
  }

  def loadPredict(rankertype:RANKER_TYPE, metricScorer:MetricScorer, modelfilename:String,testData:MultiRankings ):MultiRankings = {
    new Reranker(rankertype, metricScorer).loadPredict(modelfilename, testData)
  }

  /**
   * Create a feature vector for re-ranking
   * @param description  unique ID of the datapoint
   * @param features  feature values with string-based feature names
   *@param trainLabel optional, a label for training / validation
   * @return
   */
  def createFeatureVec(description:String, features:Seq[(String, Double)], trainLabel:Option[Int]):FeatureVec = {
    new LabeledFeatureVec(features, trainLabel, description)
  }

}
