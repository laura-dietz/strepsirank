package edu.umass.ciir.strepsirank.ede.predict


import ciir.umass.edu.learning.{DataPoint, Ranker, RankerFactory}
import java.io.File
import scala.io.Source
import edu.umass.ciir.strepsirank.ede.featgen.FeaturesToSvmConverter
import edu.umass.ciir.strepsi.ScoredDocument

/**
* To evaluate performance of individual features, assumed test data split into 5 folds.
*/
class SingleFeatureReranker(conf:RerankConf) {

  val domainMap = conf.featureDir + "/domainMap_eqe"

  val converter = new FeaturesToSvmConverter(domainMap)
  val featureDomain = converter.featureDomainMap


  //
  //
  new File(conf.outputDir).mkdirs()

  val runs = for (rf <- conf.runFiles) yield {
    val runFile = new File(rf)
    val pooledDocs = RunFileLoader.readRunFileWithQuery(runFile, 1000, conf.stringPrefix)
    pooledDocs
  }

  val featuresByFold = for (fold <- 0 to 4) yield {


    val testFeatureFile = new File(conf.featureDir + "/" + conf.featureName + "_" + fold + "test.combined")
    val features = loadSvmFeatureFile(testFeatureFile)
    features
  }

  val featuresByQuery = featuresByFold.flatten.groupBy(_.getID)

  for ((feature, idx) <- featureDomain) {

    println("reranking by : " + feature + " idx:" + idx)
    val batchResults = for (queryId <- featuresByQuery.keySet) yield {

      val pooledDocs = runs.map(run => run(queryId.toString).map(_.documentName)).flatten.toSet
      val queryFeaturesOption = featuresByQuery.get(queryId.toString) //, List[DataPoint]())

      val results = queryFeaturesOption match {
        case Some(queryFeatures) => {
          //val featuresByDoc = queryFeatures.map(f => f.getDescription -> f).toMap
          val rerankedResults = rerankResults(idx, pooledDocs, queryFeatures) take 1000
          queryId.toInt -> rerankedResults
        }
        case None => {
          println("WARN: No features for query: " + queryId)
          queryId.toInt -> Seq[ScoredDocument]()
        }
      }

      results
    }

    val allResults = batchResults.toMap

    WriteUtil.justWrite(conf.outputFile + "-" + feature, allResults, conf.stringPrefix)
  }

  def loadSvmFeatureFile(featureFile: File) = {

    var numLines = 0
    val source = Source.fromFile(featureFile, "UTF-8")
    val datapoints = for (line <- source.getLines().toList) yield {
      val featureData = new DataPoint(line)
      numLines += 1
      featureData

    }
    source.close()
    //  println("Num lines: " + numLines + " " + datapoints.size)
    datapoints.toList
  }

  def rerankResults(featureIdx: Int, docSet: Set[String], expansionFeatures: Seq[DataPoint]): Seq[ScoredDocument] = {

    val workingSetFeatrs = expansionFeatures.filter(m => {
      val docId = m.getDescription.replace("#", "").trim
      val contains = docSet.contains(docId)
      contains
    }).toSeq

    val scoredDocuments = for (datapoint <- workingSetFeatrs) yield {

      val docId = datapoint.getDescription.replace("#", "").trim
      val score = datapoint.getFeatureValue(featureIdx+1)
      //  println(score)
      val sd = new ScoredDocument(docId, -1, score)
      sd
    }
    val filtered = scoredDocuments.filter(sd => sd.score != 0.0)
    val reranked = filtered.sortBy(d => (-d.score, d.rank))
    val rerankedWithNewRank =
      for ((result, idx) <- reranked.zipWithIndex) yield {
        result.withNewRank(idx + 1)
        //println(result.rank + " " + result.documentName + " " + result.score)

      }
    rerankedWithNewRank.toSeq
  }

}
