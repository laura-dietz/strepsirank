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
  def classPredictionOpt: Option[Prediction]
  def description: String
}

class LabeledFeatureVec(val features:Seq[(String, Double)],val classLabelOpt:Option[Int], val description:String) extends FeatureVec {
  private var _prediction:Option[Prediction] = None
  def classPredictionOpt = _prediction
  def prediction_=(value:Prediction) {_prediction = Some(value)}
  def resetPrediction() = {_prediction = None}
}

case class Prediction(score:Double, rank:Int)

