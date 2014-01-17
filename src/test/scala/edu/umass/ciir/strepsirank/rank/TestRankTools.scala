package edu.umass.ciir.strepsirank.rank

import scala.collection.mutable.ListBuffer
import ciir.umass.edu.learning.RANKER_TYPE
import ciir.umass.edu.metric.APScorer

/**
 * User: dietz
 * Date: 12/9/13
 * Time: 3:44 PM
 */
object TestRankTools {

  def exampleTrain: ListBuffer[FeatureVec] = {
    val train = new ListBuffer[FeatureVec]()
    train += RankTools.createFeatureVec("train-1-1", Seq("a" -> 1, "b" -> 0), Some(1))

    train += RankTools.createFeatureVec("train1-2", Seq("a" -> 1, "b" -> 0.5), Some(1))
    train += RankTools.createFeatureVec("train0-1", Seq("a" -> 0, "b" -> 0), Some(0))
    train += RankTools.createFeatureVec("train0-2", Seq("a" -> 0.1, "b" -> 0.5), Some(0))
    train
  }

  def exampleTest: ListBuffer[FeatureVec] = {
    val test = new ListBuffer[FeatureVec]()
    test += RankTools.createFeatureVec("test0-1", Seq("a" -> 0.2, "b" -> 0), Some(0))
    test += RankTools.createFeatureVec("test1-1", Seq("a" -> 0.9, "b" -> 0), Some(1))
    test += RankTools.createFeatureVec("test1-2", Seq("a" -> 1, "b" -> 0.9, "c" -> 0.5), Some(1))
    test += RankTools.createFeatureVec("test0-2", Seq("a" -> 0.0, "b" -> 0.9), Some(0))
    test
  }

  def main(args: Array[String]) {


    val resultOpt = RankTools.trainPredict(RANKER_TYPE.LAMBDAMART, new APScorer(),
      Seq("1" -> exampleTrain, "2" -> exampleTrain), testData = Some(Seq("3" -> exampleTest)))

    print(resultOpt.get)

  }

}
