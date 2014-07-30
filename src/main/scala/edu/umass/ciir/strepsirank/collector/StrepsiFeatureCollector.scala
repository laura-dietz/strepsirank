package edu.umass.ciir.strepsirank.collector

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import edu.umass.ciir.strepsi.{StringTools, SeqTools}
import java.io.FileWriter

/**
 *
 * queryid -> {document -> (Option[Label], features)}
 * where features = Seq[(String,Double)]
 * User: dietz
 * Date: 12/9/13
 * Time: 6:00 PM
 */

class StrepsiKeyTypedFeatureCollector[Key](val featureSetName: String = "") extends mutable.HashMap[String, mutable.HashMap[Key, (Option[Int], mutable.Buffer[(String, Double)])]] {
  val defaultFeatures = new ListBuffer[(String, Double)]()

  def addDefaultFeatures(features: Seq[(String, Double)]) {
    for ((fname, fvalue) <- features) {
      defaultFeatures.find(_._1 == featureSetName + fname) match {
        case None => {
          defaultFeatures += (featureSetName + fname) -> fvalue
        } // new default feature, fill!
        case Some((_, `fvalue`)) => {
          // value already filled, but with same value - no need to complain
        }
        case Some((_, othervalue)) =>
          throw new RuntimeException("FeatureCollector#addDefaultFeatures: " +
            "Tried to set default feature " + featureSetName + fname + " with value " + fvalue + ", " +
            "but it was already set to default value " + othervalue)
      }
    }
  }

  def computeFeatureUnion:Seq[String] = {
    val featureSeq =
      for((_, vals) <- this; (_, (_, features)) <- vals; (featurename, _) <- features) yield featurename
    featureSeq.toSet.toSeq.sorted
  }

  def prependFeatureSetName(features: Seq[(String, Double)]): Seq[(String, Double)] = {
    features.map(feat => (featureSetName + feat._1, feat._2))
  }

  def addEntry(query: String, entry: Key, classLabel: Option[Int]) {
    val queryMap = this.getOrElseUpdate(query, new mutable.HashMap())
    if (!queryMap.contains(entry)) {
      queryMap.update(entry, (classLabel, new ListBuffer[(String, Double)]))
    }
  }

  def setClassLabel(query: String, entry: Key, classLabel: Option[Int]) {
    val queryMap = this.getOrElseUpdate(query, new mutable.HashMap())
    val (_, features) = queryMap.getOrElseUpdate(entry, (classLabel, new ListBuffer[(String, Double)]))
    queryMap.update(entry, (classLabel, features))
  }


  def addFeatures(query: String, entry: Key, features: Seq[(String, Double)]) {
    val queryMap = this.getOrElseUpdate(query, new mutable.HashMap())
    val (_, oldFeatures) = queryMap.getOrElseUpdate(entry, (None, new ListBuffer[(String, Double)]))
    oldFeatures ++= prependFeatureSetName(features)
  }

  def addFeaturesNoPrepend(query: String, entry: Key, features: Seq[(String, Double)]) {
    val queryMap = this.getOrElseUpdate(query, new mutable.HashMap())
    val (_, oldFeatures) = queryMap.getOrElseUpdate(entry, (None, new ListBuffer[(String, Double)]))
    oldFeatures ++= features
  }

  def getFeatures(query: String, entry: Key): Option[Seq[(String, Double)]] = {
    // here we exploit that an option is a sequence of length 0 or 1, map.get(..) gives and Option
    for (queryMap <- this.get(query); (_, features) <- queryMap.get(entry)) yield {
      features
    }
  }

  def getFeaturesWithDefault(query:String, entry:Key):Seq[(String,Double)] = {
    val localFeatures = getFeatures(query, entry).getOrElse(Seq.empty).toMap
    for((fname, defvalue) <- defaultFeatures) yield {
      val value = localFeatures.getOrElse(fname, defvalue)
      fname -> value
    }
  }


  def addClassAndFeatures(query: String, entry: Key, classLabel: Option[Int], features: Seq[(String, Double)]) {
    val queryMap = this.getOrElseUpdate(query, new mutable.HashMap())
    val (_, oldFeatures) = queryMap.getOrElseUpdate(entry, (None, new ListBuffer[(String, Double)]))
    oldFeatures ++= prependFeatureSetName(features)
    queryMap.update(entry, (classLabel, oldFeatures))
  }

  def addClassAndFeaturesNoPrepend(query: String,
                                   entry: Key,
                                   classLabel: Option[Int],
                                   features: Seq[(String, Double)]) {
    val queryMap = this.getOrElseUpdate(query, new mutable.HashMap())
    val (_, oldFeatures) = queryMap.getOrElseUpdate(entry, (None, new ListBuffer[(String, Double)]))
    oldFeatures ++= features
    queryMap.update(entry, (classLabel, oldFeatures))
  }

  def addFeature(query: String, entry: Key, featureName: String, featureValue: Double) {
    val queryMap = this.getOrElseUpdate(query, new mutable.HashMap())
    val (_, oldFeatures) = queryMap.getOrElseUpdate(entry, (None, new ListBuffer[(String, Double)]))
    oldFeatures += (featureSetName + featureName) -> featureValue
  }

  def distinctFeatures() {
    for ((query, map) <- this; (doc, (classLabel, features)) <- map) {
      val newFeatures = SeqTools.distinctByLast[(String, Double), String](features, by = _._1).toBuffer
      this(query).update(doc, (classLabel, newFeatures))
    }
  }

  def featureView(featureFilter: ((String, Double)) => Boolean): mutable.HashMap[String, mutable.HashMap[Key, (Option[Int], mutable.Buffer[(String, Double)])]] = {
    for ((query, map) <- this) yield {
      query ->
        (for ((doc, (classLabel, features)) <- map) yield {
          doc -> (
            (classLabel, features.filter(featureFilter).toBuffer)
            )
        })
    }
  }

  def featureView2(featureFilter: ((String, Double)) => Boolean,
                   defaultFeatureFilter: ((String, Double)) => Boolean,
                   newFeatureCollector: StrepsiKeyTypedFeatureCollector[Key]): StrepsiKeyTypedFeatureCollector[Key] = {
    for ((query, map) <- this) {
      for ((doc, (classLabel, features)) <- map) {
        newFeatureCollector.addClassAndFeatures(query, doc, classLabel, features.filter(featureFilter).toBuffer)
      }
    }
    newFeatureCollector.addDefaultFeatures(defaultFeatures.filter(defaultFeatureFilter))

    newFeatureCollector
  }

  def merge(fcs: Seq[StrepsiKeyTypedFeatureCollector[Key]]) = {
    for (fc <- fcs) {
      addDefaultFeatures(fc.defaultFeatures)
      for ((queryId, map) <- fc; (doc, (classLabel, features)) <- map) {
        addClassAndFeatures(queryId, doc, classLabel, features)
      }
    }
  }

  def mergeOther[OtherKey](fcs: Seq[StrepsiKeyTypedFeatureCollector[OtherKey]], matcher:(OtherKey, Key) => Boolean) = {
    for (fc <- fcs) {
      addDefaultFeatures(fc.defaultFeatures)
      for ((queryId, map) <- fc; (doc, (classLabel, features)) <- map) {

        val thiskeys = this.getOrElse(queryId, Map.empty).keySet
        val matchingkeys = thiskeys.filter(matcher(doc,_))
        for(key <- matchingkeys) {
          addClassAndFeatures(queryId, key, classLabel, features)
        }
      }
    }
  }

}

object StrepsiFeatureCollector {
  def save(fc: StrepsiKeyTypedFeatureCollector[String], filename: String, keySerialization:String => String = x => x) {
    saveSerial(fc, filename, keySerialization)
  }
  def load(filename: String, keyDeserialization:String => String = x => x):StrepsiKeyTypedFeatureCollector[String] = {
    loadSerial(filename, keyDeserialization)
  }
  def saveSerial[KeyType](fc: StrepsiKeyTypedFeatureCollector[KeyType], filename: String, keySerialization:KeyType => String) {
    val out = new FileWriter(filename)
    out.write(fc.featureSetName + "\n\n")

    val defaultline = fc.defaultFeatures.map(pair => pair._1 + "\t" + pair._2)
    out.write(defaultline + "\n\n")

    for ((query, map) <- fc) {
      for ((doc, (classLabel, features)) <- map) {
        val line = Seq(query, keySerialization(doc), classLabel, features.map(pair => pair._1 + "\t" + pair._2).mkString("\t")).mkString(
          "\t")
        out.write(line + "\n")
      }
    }

    out.close()
  }

  def loadSerial[KeyType](filename: String, keyDeserialization:String => KeyType): StrepsiKeyTypedFeatureCollector[KeyType] = {
    val in = io.Source.fromFile(filename)

    val lines = in.getLines()

    val featureSetName = lines.next().trim()
    val fc = new StrepsiKeyTypedFeatureCollector[KeyType](featureSetName)
    lines.next()
    val nextlineInclBuffer = lines.next().trim()
    val nextline = StringTools.substringMinusEnd(nextlineInclBuffer, ")".length, "ListBuffer(".length)
    // empty
    val defaultFeaturesSplits = StringTools.getSepSplits(nextline, ", ")
    val defaultFeatures = {
      val splits = defaultFeaturesSplits
      for (pair <- splits) yield {
        //      for (fidx <- Range(0, splits.length, 2).toBuffer[Int]) yield {
        val pairsplits = StringTools.getSepSplits(pair, "\t", 1)
        val fname = pairsplits(0)
        val value = pairsplits(1).toDouble
        fname -> value
      }
    }
    fc.addDefaultFeatures(defaultFeatures)
    lines.next() // empty

    for (line <- lines; if line.trim.length != 0) {
      //      val line = Seq(query, doc, classLabel, features.map(pair => pair._1+"\t"+pair._2)).mkString("\t")
      val splits = StringTools.getSepSplits(line.trim(), sep = "\t", 5)
      val query = splits(0)
      val doc = splits(1)
      val classLabelOptString = splits(2)
      val classLabelOpt =
        if (classLabelOptString == "None") None
        else {
          val classLabelString = StringTools.substringMinusEnd(classLabelOptString, ")".length, "Some(".length)
          val classLabel = classLabelString.toInt
          Some(classLabel)
        }
      val features =
        for (fidx <- Range(3, splits.length, 2).toBuffer[Int]) yield {
          val fname = splits(fidx)
          val value = splits(fidx + 1).toDouble
          fname -> value
        }

      fc.addClassAndFeaturesNoPrepend(query, keyDeserialization(doc), classLabelOpt, features)
    }

    in.close()
    fc
  }


}