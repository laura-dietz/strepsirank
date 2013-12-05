package edu.umass.ciir.strepsirank.rank.antirank

import java.lang.String
import scala.Double

/**
 * User: dietz
 * Date: 12/17/12
 * Time: 5:30 PM
 */
trait FeatureVec {
  def features: Seq[(String, Double)]
  def classLabelOpt: Option[Int]
  def description: String
}


class LabeledFeatureVec(val features:Seq[(String, Double)],val classLabel:Int, val description:String) extends FeatureVec {
  def classLabelOpt = Some(classLabel)
}

case class Prediction(score:Double, rank:Int)

class PredictedFeatureVec(val features:Seq[(String, Double)], val description:String) extends FeatureVec {
  def classLabelOpt = None
  private var _prediction:Option[Prediction] = None
  def prediction = _prediction
  def prediction_=(value:Prediction) {_prediction = Some(value)}
}
