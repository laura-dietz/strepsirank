package edu.umass.ciir.strepsirank.rank

import scala.collection.mutable.ListBuffer

/**
 * User: dietz
 * Date: 12/9/13
 * Time: 3:44 PM
 */
object TestReranker {

  def exampleTrain: ListBuffer[LabeledFeatureVec] = {
    val train = new ListBuffer[LabeledFeatureVec]()
    train += new LabeledFeatureVec(Seq("a" -> 1, "b" -> 0), Some(1), "train1-1")
    train += new LabeledFeatureVec(Seq("a" -> 1, "b" -> 0.5), Some(1), "train1-2")
    train += new LabeledFeatureVec(Seq("a" -> 0, "b" -> 0), Some(0), "train0-1")
    train += new LabeledFeatureVec(Seq("a" -> 0.1, "b" -> 0.5), Some(0), "train0-2")
    train
  }

  def exampleTest: ListBuffer[LabeledFeatureVec] = {
    val test = new ListBuffer[LabeledFeatureVec]()
    test += new LabeledFeatureVec(Seq("a" -> 0.2, "b" -> 0), Some(0), "test0-1")
    test += new LabeledFeatureVec(Seq("a" -> 0.9, "b" -> 0), Some(1), "test1-1")
    test += new LabeledFeatureVec(Seq("a" -> 1, "b" -> 0.9, "c" -> 0.5), Some(1), "test1-2")
    test += new LabeledFeatureVec(Seq("a" -> 0.0, "b" -> 0.9), Some(0), "test0-2")
    test
  }

  def main(args: Array[String]) {
    val reranker = new Reranker()
    val resultOpt = reranker.trainPredict(Seq("1" -> exampleTrain), testData = Some(Seq("2" -> exampleTest)))

    print(resultOpt.get)

  }

}
