package edu.umass.ciir.strepsirank.rank

import ciir.umass.edu.learning.{Ranker, RANKER_TYPE}
import ciir.umass.edu.metric.{APScorer, MetricScorer}
import ciir.umass.edu.utilities.MyThreadPool

/**
 * Static interface to the re-ranking lib
 * User: dietz
 * Date: 12/9/13
 * Time: 3:53 PM
 */
object RankTools {
  type MultiRankings = Seq[(String, Seq[FeatureVec])]
  Ranker.verbose = false



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
  def createFeatureVec(description:String, features:Seq[(String, Double)], trainLabel:Option[Int], defaultFeatures:Map[String, Double]=Map.empty):FeatureVec = {
    val uncoveredFeatures = defaultFeatures.keySet diff features.map(_._1).toSet
    val fullFeatures = features ++ uncoveredFeatures.map(feat => feat -> defaultFeatures(feat))
    new LabeledFeatureVec(fullFeatures, trainLabel, description)
  }


  def setThreadPoolSize(poolsize:Int){
    MyThreadPool.init(poolsize)
  }
}
