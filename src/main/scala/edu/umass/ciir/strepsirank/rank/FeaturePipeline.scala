package edu.umass.ciir.strepsirank.rank



/**
 * User: dietz
 * Date: 12/13/12
 * Time: 2:26 PM
 */
trait FeaturePipeline[A] {
  def createFeatures(elem:A):Seq[(String, Double)]
  def name:String
  def runmeta:List[(String,String)]

//  def computeBackgroundFrom(collection:Seq[A], useWeighting:Boolean):Map[String, Double] = Map()
}


class CachedFeaturePipeline[A](val features:FeaturePipeline[A])  extends FeaturePipeline[A]{
  private val cache = new scala.collection.mutable.HashMap[A, Seq[(String, Double)]]()
  def resetCache() { cache.clear()}

  def createFeatures(elem: A):Seq[(String, Double)] = {
    cache.getOrElseUpdate(elem, {features.createFeatures(elem)})
  }

  def name = features.name

  def runmeta = features.runmeta
}

