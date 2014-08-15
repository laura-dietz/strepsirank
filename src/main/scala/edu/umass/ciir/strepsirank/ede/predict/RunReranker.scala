package edu.umass.ciir.strepsirank.ede.predict

import java.io.File

import ciir.umass.edu.learning.{DataPoint, Ranker, RankerFactory}
import edu.umass.ciir.strepsi.ScoredDocument

import scala.io.Source

/**
 * Created by jdalton on 1/20/14.
 */
class RunReranker(conf:RerankConf, justWrite:Boolean, featureDescrToDocId:(String) => String = x => x) {


  def runReranker() {

    new File(conf.outputDir).mkdirs()

    val runs = for (rf <- conf.runFiles) yield {
      val runFile = new File(rf)
      val pooledDocs = RunFileLoader.readRunFileWithQuery(runFile, conf.numResults, conf.stringPrefix)
      pooledDocs
    }


    val ltrModel = new RankerFactory().loadRanker(conf.ltrModelBase + ".model")

    val testFeatureFile = new File(conf.featureDir + "/" + conf.featureName + "_all")
    val features = RunReranker.loadSvmFeatureFile(testFeatureFile)
    val featuresByQuery = features.groupBy(_.getID)

    val queriesInRuns = runs.flatMap(_.keys.map(_.toInt)).distinct
    val foldQueries = queriesInRuns
    val allResults = for (queryId <- foldQueries) yield {

      val pooledDocs = runs.map(run => run(queryId.toString).map(_.documentName)).flatten.toSet
      val queryFeaturesOption = featuresByQuery.get(queryId.toString) //, List[DataPoint]())

      queryFeaturesOption match {
        case Some(queryFeatures) => {
          //val featuresByDoc = queryFeatures.map(f => f.getDescription -> f).toMap
          val rerankedResults = RunReranker.rerankResults(ltrModel, pooledDocs, queryFeatures, featureDescrToDocId) take conf.numResults
          queryId -> rerankedResults
        }
        case None => {
          println("WARN: No features for query: " + queryId)
          queryId -> Seq[ScoredDocument]()
        }
      }

    }


    WriteUtil.justWrite(conf.outputFile, allResults.toMap, conf.qrels, conf.stringPrefix)

  }
}
  object RunReranker{

  def loadSvmFeatureFile(featureFile:File) = {

    val source = Source.fromFile(featureFile, "UTF-8")
    val datapoints = for (line <- source.getLines() ) yield {
      val cappedfeats = line.replaceAll("[eE][1-9][01-9][01-9]","e20")
      val featureData = new DataPoint(cappedfeats)
      featureData
    }
    datapoints.toList
  }


  def rerankResults(ranker: Ranker, docSet: Set[String], expansionFeatures :  Seq[DataPoint], featureDescrToDocId:(String) => String ) : Seq[ScoredDocument] = {

    val workingSetFeatrs = expansionFeatures.filter(m => docSet.contains(featureDescrToDocId(m.getDescription.replace("#","").trim)))

    val scoredDocuments = for (datapoint <- workingSetFeatrs) yield {

      val docId = featureDescrToDocId(datapoint.getDescription.replace("#","").trim)
      val score = ranker.eval(datapoint)
      //  println(score)
      val sd = new ScoredDocument(docId, -1, score)
      sd
    }
    val reranked = scoredDocuments.sortBy(d => (-d.score, d.rank))
    val rerankedNoGaps =
      for ((result, idx) <- reranked.zipWithIndex) yield {
        result.withNewRank(idx + 1)
      }
    rerankedNoGaps.toSeq
  }

}

