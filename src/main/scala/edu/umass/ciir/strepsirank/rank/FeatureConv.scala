package edu.umass.ciir.strepsirank.rank

import ciir.umass.edu.learning.DataPoint
import collection.mutable.ListBuffer
import java.io.{File, FileWriter}
import scala.collection.JavaConversions._
import edu.umass.ciir.strepsi.StringTools

/**
 * Holds the featureDescription -> Idx map
 * THe map is built up from the training data
 *
 * If ignoreNewFeatures == true, no new features are added to the map (used for prediction)
 *
 */
class FeatureConv(trackIgnoreFeature:Boolean, _featureIdxs:Seq[(String,Int)]=Seq.empty) {
  var featureDescriptionMap =  Map.empty[String,Int]

  val firstFeatureIdx = 1
  private var lastFeatureIdx = 0
  private var _ignoreNewFeatures:Boolean = true
  def ignoreNewFeatures = _ignoreNewFeatures
  def ignoreNewFeatures_=(ignoreNewFeatures:Boolean) { _ignoreNewFeatures=ignoreNewFeatures }
  val IGNORED_FEATURE = "___ignored_feature___"

  if (trackIgnoreFeature) getOrCreateFeature(IGNORED_FEATURE)


  def getOrCreateFeature(featureName:String):Option[Int] = {
    featureDescriptionMap.get(featureName) match {
      case Some(index) => Some(index)
      case None => {
        lastFeatureIdx += 1
        val newIdx = lastFeatureIdx
        featureDescriptionMap += (featureName -> newIdx)
        Some(newIdx)
      }
    }
  }

  def getOrIgnoreFeature(featureName:String):Option[Int] = {
    featureDescriptionMap.get(featureName)
  }

  def getOrDefaultFeature(featureName:String):Option[Int] = {
    Some(
      featureDescriptionMap.getOrElse(featureName,
        featureDescriptionMap.get(IGNORED_FEATURE).get)
    )
  }

  def maxFeatureIdx:Int = lastFeatureIdx

  def featureIndices = (firstFeatureIdx to lastFeatureIdx).toArray



  // 3 qid:1 1:1 2:1 3:0 4:0.2 5:0 # 1A
  def convertToSvmLine(inputFeatures:Seq[(String, Double)], classLabel:Int, qid:Int, comment:String=""):String = {
    val featureStrBuf = ListBuffer[String]()
    for ((featName, featValue)<- inputFeatures) {
      val fidxOpt =
        if (ignoreNewFeatures)
          if (trackIgnoreFeature)
            getOrDefaultFeature(featName)
          else
            getOrIgnoreFeature(featName)
        else
          getOrCreateFeature(featName)
      fidxOpt match {
        case Some(fidx) => featureStrBuf += fidx+":"+featValue
        case None => {}
      }
    }

    classLabel+" qid:"+qid+" " + featureStrBuf.mkString(" ") + (if(comment != "") {" # "+comment} else {""})
  }

  def convertToDataPoint(inputFeatures:Seq[(String, Double)], classLabel:Int, qid:Int, comment:String=""):DataPoint = {
    val line = convertToSvmLine(inputFeatures, classLabel, qid, comment)
    new DataPoint(line)
  }


  def save(filename:String) {
    new File(filename).delete()
    System.setProperty("file.encoding","UTF-8")
    val w = new FileWriter(new File(filename))
    for ((name, idx) <-featureDescriptionMap.toSeq.sortBy(_._2)){
      w.write(idx+"\t"+name+"\n")
    }
    w.write("\n")
    w.write("trackIgnoreFeature = "+trackIgnoreFeature)
    w.close()
  }

  def load(filename:String) = {

    val s = io.Source.fromFile(filename)
    val featureIdxs=
      for(line <- s.getLines().takeWhile(_.trim.length > 0)) yield {
        val splits = StringTools.getSepSplits(line, sep = "\t", min = 2)
        val id = splits(1)
        val name = splits(0).toInt
        id -> name
      }
    featureDescriptionMap = featureIdxs.toMap[String,Int]

    s.close()
  }

}


