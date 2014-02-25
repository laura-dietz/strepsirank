package edu.umass.ciir.strepsirank.ede.predict

import ciir.umass.edu.learning.{DataPoint, Ranker, RankerFactory}
import java.io.File
import edu.umass.ciir.strepsirank.ede.ScoredDocument

import scala.io.Source
import edu.umass.ciir.strepsirank.ede.predict.RunReranker.RerankConf

/**
 * Created by jdalton on 1/20/14.
 */
object RunReranker {
  case class RerankConf(ltrModelBase:String, runFiles:Seq[String],featureDir:String,
                        featureName:String,
                        outputDir:String, outputFile:String, queryfolds:Array[Array[Int]], stringPrefix:String,
                        qrels:String)
}

class RunReranker(conf:RerankConf, justWrite:Boolean) {


  new File(conf.outputDir).mkdirs()

  val runs = for (rf <- conf.runFiles) yield {
    val runFile = new File(rf)
    val pooledDocs = RunFileLoader.readRunFileWithQuery(runFile, 1000, conf.stringPrefix)
    pooledDocs
  }

  val resultsByFold = for (fold <- 0 to 4) yield {

    val ltrModel = new RankerFactory().loadRanker(conf.ltrModelBase + fold + ".model")

    val testFeatureFile = new File(conf.featureDir + "/" + conf.featureName+"_" + fold+"test.combined")
    val features = loadSvmFeatureFile(testFeatureFile)
    val featuresByQuery = features.groupBy(_.getID)

    val foldQueries = conf.queryfolds(fold)
    val batchResults = for (queryId <- foldQueries) yield {

      val pooledDocs = runs.map(run => run(queryId.toString).map(_.documentName)).flatten.toSet
      val queryFeaturesOption = featuresByQuery.get(queryId.toString) //, List[DataPoint]())

      queryFeaturesOption match {
        case Some(queryFeatures) => {
          //val featuresByDoc = queryFeatures.map(f => f.getDescription -> f).toMap
          val rerankedResults = rerankResults(ltrModel, pooledDocs, queryFeatures) take 1000
          queryId -> rerankedResults
        }
        case None => {
          println("WARN: No features for query: " + queryId)
          queryId -> Seq[ScoredDocument]()
        }
      }

    }
    batchResults
  }

  val allResults = resultsByFold.flatten.toMap

  WriteUtil.justWrite(conf.outputFile, allResults, conf.qrels , conf.stringPrefix)

  def loadSvmFeatureFile(featureFile:File) = {

    val source = Source.fromFile(featureFile, "UTF-8")
    val datapoints = for (line <- source.getLines() ) yield {
      val featureData = new DataPoint(line)
      featureData
    }
    datapoints.toList
  }


  def rerankResults(ranker: Ranker, docSet: Set[String], expansionFeatures :  Seq[DataPoint] ) : Seq[ScoredDocument] = {

    val workingSetFeatrs = expansionFeatures.filter(m => docSet.contains(m.getDescription.replace("#","").trim))

    val scoredDocuments = for (datapoint <- workingSetFeatrs) yield {

      val docId = datapoint.getDescription.replace("#","").trim
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

