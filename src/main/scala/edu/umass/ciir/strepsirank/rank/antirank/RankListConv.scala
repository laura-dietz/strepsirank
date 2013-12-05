package edu.umass.ciir.strepsirank.rank.antirank

import collection.mutable.ListBuffer
import ciir.umass.edu.learning.{DataPoint, RankList}
import collection.immutable.IntMap
import edu.umass.ciir.strepsirank.rank.FeatureConv
import scala.collection.JavaConversions._
import ciir.umass.edu.features.{LinearNormalizer, SumNormalizor}

/**
 * User: dietz
 * Date: 12/17/12
 * Time: 3:57 PM
 */
class RankListConv(trackIgnoreFeature:Boolean, ignoreNewFeatures:Boolean, normalizeFeatures:Boolean =false) {
  val fc = new FeatureConv(trackIgnoreFeature)
  fc.ignoreNewFeatures = ignoreNewFeatures

  var qidmap = IntMap.empty[String]

  val qidmapping = scala.collection.mutable.HashMap[String, Int]()


  def createRankList(vectors:Seq[LabeledFeatureVec], qid:String):RankList = {
    val currentQidInt = qidmapping.getOrElseUpdate(qid, {
      qid.toInt
    })

//    val currentQidInt = qidmap.size
//    assert(!qidmap.values.contains(qid), {"qidmap already contains an index for qid "+qid+". qidmap = "+qidmap})
    qidmap+= (currentQidInt -> qid)
    val rankList = new ListBuffer[DataPoint] //new RankList()
    for (vector <- vectors) {
      rankList += fc.convertToDataPoint(vector.features, vector.classLabel, currentQidInt, vector.description)
    }
    val wrappedRankList = new RankList(bufferAsJavaList(rankList))
    if (normalizeFeatures){
      (new LinearNormalizer).normalize(wrappedRankList, fc.featureIndices)
    }
    wrappedRankList
  }



}
