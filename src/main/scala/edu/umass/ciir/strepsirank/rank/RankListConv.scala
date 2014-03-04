package edu.umass.ciir.strepsirank.rank

import collection.mutable.ListBuffer
import ciir.umass.edu.learning.{DataPoint, RankList}
import collection.immutable.IntMap
import scala.collection.JavaConversions._
import ciir.umass.edu.features.LinearNormalizer
import RankTools.MultiRankings

/**
 * User: dietz
 * Date: 12/17/12
 * Time: 3:57 PM
 */
class RankListConv(trackIgnoreFeature: Boolean, ignoreNewFeatures: Boolean, normalizeFeatures: Boolean = false) {
  val fc = new FeatureConv(trackIgnoreFeature)
  fc.ignoreNewFeatures = ignoreNewFeatures

  //  var qidmap = IntMap.empty[String]

  //  val qidmapping = scala.collection.mutable.HashMap[String, Int]()


  def createRankList(vectors: Seq[FeatureVec], qid: String): RankList = {
    //    val currentQidInt = qidmapping.getOrElseUpdate(qid, {
    // //      qid.toInt
    //      qidmapping.size+1
    //    })

    //    qidmap += (currentQidInt -> qid)
    val rankList = new ListBuffer[DataPoint]
    for (vector <- vectors; if vector.classLabelOpt.isDefined) {
      rankList += fc.convertToDataPoint(vector.features, vector.classLabelOpt.get, qid, vector.description)
    }
    val wrappedRankList = new RankList(bufferAsJavaList(rankList))
    if (normalizeFeatures) {
      (new LinearNormalizer).normalize(wrappedRankList, fc.featureIndices)
    }
    wrappedRankList
  }

  def convertFromRankList(vectors: Seq[FeatureVec],
                          rankList: RankList,
                          scorer: (DataPoint => Double)): IndexedSeq[FeatureVec] = {
    val vectorsMap = vectors.map(vec => vec.description -> vec).toMap
    val ranking =
      for (idx <- 0 until rankList.size()) yield {
        val datapoint = rankList.get(idx)
        val desc = datapoint.getDescription
        val rank = idx + 1
        val score = scorer(datapoint)
        assert(desc.startsWith("#"))
        //        println(desc)
        val datadesc = desc.substring(1).trim()

        val vector = vectorsMap(datadesc)
        vector.createImmutablePrediction(Prediction(score, rank))
      }

    ranking
  }


  def multiDataToRankList(multidata: MultiRankings): List[RankList] = {
    val result = new ListBuffer[RankList]
    for ((qid, data) <- multidata) {
      val ranklist = createRankList(data, qid)
      result += ranklist
    }
    result.toList
  }

  def rankListsToMultiData(rankLists: java.util.List[RankList],
                           origMultiData: MultiRankings,
                           scorer: (DataPoint => Double)): MultiRankings = {
    val origMultiDataMap = origMultiData.toMap
    val result = new ListBuffer[(String, IndexedSeq[FeatureVec])]
    for (rankList <- rankLists) {
      val qid = rankList.getID
      val test = origMultiDataMap(qid)
      val resultRanking = convertFromRankList(test, rankList, scorer)
      result += (qid -> resultRanking)
    }
    result.toList
  }

}
