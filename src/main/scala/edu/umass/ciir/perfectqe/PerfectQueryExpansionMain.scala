package edu.umass.ciir.perfectqe

import edu.umass.ciir.strepsi.SeqTools
import edu.umass.ciir.strepsirank.collector.{StrepsiReRanker, StrepsiFeatureCollector}
import edu.umass.ciir.strepsirank.rank.RankTools
import ciir.umass.edu.learning.RANKER_TYPE
import ciir.umass.edu.metric.APScorer

/**
 * User: dietz
 * Date: 1/18/14
 * Time: 8:14 PM
 */
object PerfectQueryExpansionMain {
  val mu = 100.0

  def computeTermDocFeature(tf: Int, docLen: Int, ctf: Int, cLen: Int, mu: Double): Double = {
    //    math.log(tf + mu*ctf/cLen) - math.log(docLen+mu)
    tf
  }

  def doit(query: (String, Seq[String]), docs: Seq[Seq[String]], truth: Seq[Int]) {
    val queryId = query._1
    val queryTerms = query._2

    val fc = new StrepsiFeatureCollector("")

    generateTermDocTablaux(docs, fc, truth, queryId)

    StrepsiFeatureCollector.save(fc, "q" + queryId + ".fc")

    def submitTrainScore(score: Double, msg: String) {
      println("score = " + score + " " + msg)
    }

    var weightVector: Seq[(String, Double)] = null
    def submitWeightVector(weightVectorSet: Seq[(String, Double)]) {
      println("weightVector = " + weightVectorSet)
      weightVector = weightVectorSet
    }


    val reranker = new StrepsiReRanker(fc, RANKER_TYPE.COOR_ASCENT)
    val metricScorer = new APScorer()
    metricScorer.setRelDocCount(queryId, truth.count(_ > 0))
    reranker.trainTestSplit(Set(queryId), Some(fc.defaultFeatures), metricScorer = metricScorer,
      testQueries = None, modelfilename = Some
        ("perfectqe-q" + queryId), submitTrainScore = submitTrainScore, submitWeightVector = submitWeightVector)

  }


  def generateTermDocTablaux(docs: Seq[Seq[String]], fc: StrepsiFeatureCollector, truth: Seq[Int], queryId: String) {
    val docsTf = docs.map(SeqTools.countMap(_))
    val docLens = docs.map(_.length)
    val corpusTf = SeqTools.sumMaps(docsTf)
    val corpusLens = docLens.sum

    val termPool = corpusTf.keys.toIndexedSeq
    val defFeatures =
      for (term <- termPool) yield {
        term -> computeTermDocFeature(0, 0, corpusTf.getOrElse(term, 0), corpusLens, mu)
      }
    fc.addDefaultFeatures(defFeatures)

    for ((isRel, docno) <- truth.zipWithIndex) {
      fc.addEntry(queryId, docno + "", Some(isRel))
    }

    for (((tf, len), docno) <- docsTf.zip(docLens).zipWithIndex) {
      val features =
        for (term <- termPool) yield {
          term -> computeTermDocFeature(tf.getOrElse(term, 0), len, corpusTf.getOrElse(term, 0), corpusLens, mu)
        }
      fc.addFeatures(queryId, docno + "", features)
    }
  }

  def main(args: Array[String]) {
    val q1 = "161" -> "mutations raf gene associated cancer".split(" ").toSeq
    val q2 = "200" -> "role ide alzheimers disease".split(" ").toSeq


    val docs1 = Seq(
      "\t\t\tglucose was preserved in alzheimer disease when cases with diabetes were \n\t\t\texcluded from the " +
        "analysis pathology studies islet amyloid in alzheimer \n\t\t\tdisease protocol 3 and brain amyloid in type 2 diabetes protocol 4 protocol \n\t\t\t3 islet amyloid in alzheimer disease both the frequency and the extent of ",
      "\t\t\tinsulin-degrading enzyme ide on chromosome 10 and 2-macroglobulin a2m and \n\t\t\tlow-density lipoprotein " +
        "receptor-related gene lrp1 on chromosome 12 8 12 13 \n\t\t\talthough none has been proven to be an ad gene 14 17 in this report we ",
      "\t\t\t800 rose st lexington ky 40536-0084 tel 859-323-5549 fax 859-323-1727 \n\t\t\te-mail lhersh{at}uky.edu 1 " +
        "the abbreviations used are a amyloid peptide(s \n\t\t\tpppi tripolyphosphate ad alzheimer disease ide insulin-degrading enzyme \n\t\t\thplc high pressure liquid chromatography abz 2-aminobenzoyl eddnp \n\t\t\tn-(2,4-dinitrophenyl)ethylenediamine acknowledgments mass spectral data ",
      "\t\t\tabstract 47 hansen l.a masliah e galasko d and terry r.d 1993 plaque-only \n\t\t\talzheimer disease is usually the lewy body variant and vice versa j \n\t\t\tneuropathol exp neurol 52 648-54 medline abstract 48 wisniewski k jervis g \n\t\t\tmoretz r and wisniewski h.k 1979 alzheimer neurofibrillary tangles in "
    ).map(_.split("\\s+").toSeq)

    val docs2 = Seq(
      "\t\t\tof the mannose binding lectin mbl pathway of complement activation with \n\t\t\tdifferent disease " +
        "parameters and disease activity in patients with systemic \n\t\t\tlupus erythematosus sle methods mbl genotype mbl serum concentration mbl \n\t\t\tcomplex activity and mbl pathway activity were assessed in 53 patients the ",
      "\t\t\tassociation of an endothelial autoantigen of 38 40 kda with lupus nephritis \n\t\t\tusing sequential serum" +
        " samples from the patient with active nephritis we \n\t\t\tfound that anti-ribosomal p protein peptide antibody concentrations \n\t\t\tcorresponded with general markers of disease activity our data also showed ",
      "\t\t\thowever it supports the anti-p association with different skin \n\t\t\tmanifestations as well as the " +
        "presence of anticardiolipin in a subset of \n\t\t\tpatients with sle characterized by early disease onset key words anti-p \n\t\t\tantibodies systemic lupus erythematosus anticardiolipin antibodies \n\t\t\tneuropsychiatric lupus introduction top abstract introduction patients and ",
      "\t\t\tthe pathogenesis of lupus nephritis 24 25 a positive association of \n\t\t\tanti-ncs antibodies with renal damage has been demonstrated previously both \n\t\t\tin murine models of lupus and in sle 5 6 24 25 this association seems to \n\t\t\tdepend on a complex interaction between charges associated with the "
    ).map(_.split("\\s+").toSeq)


    val truth1 = Seq(1, 0, 1, 0)
    val truth2 = Seq(1, 1, 0, 0)



    doit(q1, docs1, truth1)
    doit(q2, docs2, truth2)

  }
}
