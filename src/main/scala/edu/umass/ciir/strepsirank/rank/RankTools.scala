package edu.umass.ciir.strepsirank.rank

import ciir.umass.edu.learning.{DataPoint, Ranker, RANKER_TYPE}
import ciir.umass.edu.metric.MetricScorer
import ciir.umass.edu.utilities.MyThreadPool

import scala.collection.mutable.ListBuffer

/**
 * Static interface to the re-ranking lib
 * User: dietz
 * Date: 12/9/13
 * Time: 3:53 PM
 */
object RankTools {
  type MultiRankings = Seq[(String, Seq[FeatureVec])]
  Ranker.verbose = false


  def trainPredict(rankertype: RANKER_TYPE,
                   metricScorer: MetricScorer,
                   train: MultiRankings,
                   modelfilename: Option[String] = None,
                   testData: Option[MultiRankings] = None,
                   submitTrainScore: (Double, String) => Unit = Reranker.printTrainScore,
                   submitValidationScore: (Double, String) => Unit = Reranker.printValidationScore,
                   submitWeightVector: (Seq[(String, Double)]) => Unit = (x) => {}
                    ): Option[MultiRankings] = {
    new Reranker(rankertype, metricScorer).trainPredict(train, modelfilename, testData, submitTrainScore,
      submitValidationScore, submitWeightVector = submitWeightVector)
  }

  def loadPredict(rankertype: RANKER_TYPE,
                  metricScorer: MetricScorer,
                  modelfilename: String,
                  testData: MultiRankings): MultiRankings = {
    new Reranker(rankertype, metricScorer).loadPredict(modelfilename, testData)
  }

  /**
   * Create a feature vector for re-ranking
   * @param description  unique ID of the datapoint
   * @param features  feature values with string-based feature names
   * @param trainLabel optional, a label for training / validation
   * @return
   */
  def createFeatureVec(description: String,
                       features: Seq[(String, Double)],
                       trainLabel: Option[Int],
                       defaultFeatures: Map[String, Double]): FeatureVec = {
    val uncoveredFeatures = defaultFeatures.keySet diff features.map(_._1).toSet
    val fullFeatures = features ++ uncoveredFeatures.map(feat => feat -> defaultFeatures(feat))
    new LabeledFeatureVec(fullFeatures, trainLabel, description)
  }


  def setThreadPoolSize(poolsize: Int) {
    MyThreadPool.init(poolsize)
  }

  def convertToDataPoint(inputFeatures: Seq[(String, Double)],
    classLabel: Int,
    qid: String,
    comment: String = "", domainMap : Map[String, Int]): DataPoint = {
    val line = convertToSvmLine(inputFeatures, classLabel, qid, comment, domainMap)
    new DataPoint(line)
  }

  // 3 qid:1 1:1 2:1 3:0 4:0.2 5:0 # 1A
  def convertToSvmLine(inputFeatures: Seq[(String, Double)],
                       classLabel: Int,
                       qid: String,
                       comment: String = "", domainMap : Map[String, Int]): String = {
    val featureStrBuf = ListBuffer[String]()
    val domainList = domainMap.toList.sortBy(_._2)
    val featureMap = inputFeatures.toMap

    for ((featName, featIdx) <- domainList) {
       val featVal = featureMap.getOrElse(featName, 0.0)
      featureStrBuf += s"${featIdx+1}:$featVal"
    }

    classLabel + " qid:" + qid + " " + featureStrBuf.mkString(" ") + (if (comment != "") {
      " # " + comment
    } else {
      ""
    })
  }

}
