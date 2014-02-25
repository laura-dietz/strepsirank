package edu.umass.ciir.strepsirank.ede.featgen

import java.io.PrintWriter

object FeatureWriter {

  
  def writeExpansionFeatures(expansionFeatures: Map[Int, Seq[(String, Seq[(String, Double)])]],
                             domainMapFile: String,
                             featureDir: String,
                             runName: String,
                             qrels: Map[String, Map[String,Int]],
                             queryPrefix: String = "",
                             splits:Array[Array[Int]]) = {

    // serialize expansion feature domain
    val features = expansionFeatures.map(_._2.map(r => r._2.map(_._1))).flatten.flatten
    val featureSet = features.toSet
    val featureMap = featureSet.zipWithIndex.toMap
    println("Number of features: " + featureMap.size)

    // serialize the domain file for later use
    val domainWriter = new PrintWriter(domainMapFile)
    for ((k, v) <- featureMap) domainWriter.println(k + "\t" + v)
    domainWriter.close()



    // generate split data
    for (fold <- 0 to 4) {

      val curEvalSet = splits(fold).toSet

      val basePrefix = featureDir + "/" + runName

      val evalResults = expansionFeatures.filterKeys(k => curEvalSet.contains(k))
      val trainResults = expansionFeatures.filterKeys(k => !curEvalSet.contains(k))

      val features2SvmConverter = new FeaturesToSvmConverter(domainMapFile)

      writeFeats(basePrefix + "_" + fold + "train", features2SvmConverter, trainResults, qrels, queryPrefix)
      writeFeats(basePrefix + "_" + fold + "test", features2SvmConverter, evalResults, qrels, queryPrefix)
      writeFeats(basePrefix + "_" + "all", features2SvmConverter, expansionFeatures, qrels, queryPrefix)

    }
  }

  def writeExpansionFeaturesNoQrel(expansionFeatures: Map[Int, Seq[(String, Seq[(String, Double)])]],
                                   domainMapFile: String,
                                   featureDir: String,
                                   runName: String,
                                   queryPrefix: String = "",
                                   splits:Array[Array[Int]]) = {

    // serialize expansion feature domain
    val features = expansionFeatures.map(_._2.map(r => r._2.map(_._1))).flatten.flatten
    val featureSet = features.toSet
    val featureMap = featureSet.zipWithIndex.toMap
    println("Number of features: " + featureMap.size)

    // serialize the domain file for later use
    val domainWriter = new PrintWriter(domainMapFile)
    for ((k, v) <- featureMap) domainWriter.println(k + "\t" + v)
    domainWriter.close()



    // generate split data
    for (fold <- 0 to 4) {

      val curEvalSet = splits(fold).toSet

      val basePrefix = featureDir + "/" + runName

      val evalResults = expansionFeatures.filterKeys(k => curEvalSet.contains(k))
      val trainResults = expansionFeatures.filterKeys(k => !curEvalSet.contains(k))

      val features2SvmConverter = new FeaturesToSvmConverter(domainMapFile)

      writeFeatsWithoutQrel(basePrefix + "_" + fold + "train", features2SvmConverter, trainResults, queryPrefix)
      writeFeatsWithoutQrel(basePrefix + "_" + fold + "test", features2SvmConverter, evalResults, queryPrefix)
      writeFeatsWithoutQrel(basePrefix + "_" + "all", features2SvmConverter, expansionFeatures,  queryPrefix)

    }
  }

  def writeFeats(outputFilePath: String, features2SvmConverter: FeaturesToSvmConverter,
                 queryResults: Map[Int, Seq[(String, Seq[(String, Double)])]],
                 qrels: Map[String, Map[String,Int]],
                 queryPrefix: String = "") = {

    for ((query, results) <- queryResults) {
      val pw = new PrintWriter(outputFilePath + "-" + query)
      val queryJudgments = qrels.get(queryPrefix + query)
      if (queryJudgments == null) {
        throw new Exception("No valid query judgments for query: " + query)
      }

      for ((doc, feats) <- results) {
        val svmString = features2SvmConverter.resultFeaturesToSvmFormat(query, doc, feats, queryJudgments.get(doc))
        pw.println(svmString)
      }
      pw.close()
    }


  }


  def writeFeatsWithoutQrel(outputFilePath: String, features2SvmConverter: FeaturesToSvmConverter,
                            queryResults: Map[Int, Seq[(String, Seq[(String, Double)])]],
                            queryPrefix: String = "") = {

    for ((query, results) <- queryResults) {
      val pw = new PrintWriter(outputFilePath + "-" + query)

      for ((doc, feats) <- results) {
        val svmString = features2SvmConverter.resultFeaturesToSvmFormat(query, doc, feats, 0)
        pw.println(svmString)
      }
      pw.close()
    }


  }
}
