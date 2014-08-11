package edu.umass.ciir.strepsirank.ede.predict

/**
 * User: dietz
 * Date: 2/25/14
 * Time: 11:56 AM
 */
case class RerankConf(ltrModelBase:String, runFiles:Seq[String],featureDir:String,
                      featureName:String,
                      outputDir:String, outputFile:String, queryfolds:Array[Array[Int]], stringPrefix:String,
                      qrels:String, numResults:Int){}

