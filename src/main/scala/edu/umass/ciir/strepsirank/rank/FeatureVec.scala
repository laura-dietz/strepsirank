package edu.umass.ciir.strepsirank.rank

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
  def prediction:Option[Prediction]
  def prediction_=(value:Option[Prediction])
  def description: String
  def createImmutablePrediction(value:Prediction):FeatureVec
}

class LabeledFeatureVec(val features:Seq[(String, Double)],val classLabelOpt:Option[Int], val description:String) extends FeatureVec {
  private var _prediction:Option[Prediction] = None
  def prediction = _prediction
  def prediction_= (value:Option[Prediction]):Unit = {_prediction = value}
  def createImmutablePrediction(value:Prediction):FeatureVec = {
    val predictLabel = new LabeledFeatureVec(features = features, classLabelOpt = classLabelOpt, description = description)
    predictLabel.prediction=Some(value)
    predictLabel
  }
  override def toString = description+" ("+classLabelOpt+") = "+prediction.toString
}

case class Prediction(score:Double, rank:Int)

