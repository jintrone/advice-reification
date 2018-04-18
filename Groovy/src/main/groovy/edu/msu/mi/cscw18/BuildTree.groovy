package edu.msu.mi.cscw18

import edu.msu.mi.shared.Loader
import groovy.sql.Sql
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVPrinter

/**
 * Created by josh on 2/7/17.
 */
class BuildTree {

    static Loader l = Loader.init("webmd", "webmd", "webmd_enriched")
    static def basepath = "/Users/josh/Dropbox/@PAPERS/2017/CSCW/data"
    Map contextsToPosts = [:]
    Map peopleToContext = [:]
    List links = []
    Sql cnx = l.getSqlConnection()
    int threadgap
    String corpus
    String topic
    List cols = ["uniqueId", "date", "poster", "class", "topic", "pi", "ri"]
    boolean forceTree


    public BuildTree(String corpus, int threadGap, String topic = "default", boolean forceTree = true) {
        this.corpus = corpus
        this.threadgap = threadGap
        this.topic = topic
        this.forceTree = forceTree
    }


    def createTree() {
        String query = "select uniqueId, qid, date, ${corpus}.poster,class,cleancontent, ${corpus}_topics.topicId topic, pi, ri, replyTo, inferred_replies from ${corpus} left join ${corpus}_topics on ${corpus}.uniqueId = ${corpus}_topics.uid and ${corpus}_topics.topicRun='${topic}' " +
                "inner join ${corpus}_users on ${corpus}.poster=${corpus}_users.poster order by date"
        cnx.rows(query).each { Map row ->
            pruneOldStuff(row.date)
            if (!contextsToPosts.containsKey(row.qid)) {
                contextsToPosts[row.qid] = []
            }
            if (!peopleToContext.containsKey(row.poster)) {
                peopleToContext[row.poster] = [:]
            }

            contextsToPosts[row.qid] << row
            peopleToContext[row.poster][row.qid] = row.date
            if (row.topic != null) {
                if (forceTree) {
                    def result = getValidPriorContexts(row)?.inject([]) { best, item ->
                        if (item.topic == row.topic) {
                            if (!best) {
                                best << item
                                best << jaccard(item.cleancontent, row.cleancontent)
                            } else {
                                float nsim = jaccard(item.cleancontent, row.cleancontent)
                                if (nsim > best[1] || ((nsim == best[1]) && (best[0].date < item.date))) {
                                    best[0] = item
                                    best[1] = nsim
                                }


                            }

                        }
                        best
                    }
                    if (result) {
                        link(result[0], row)
                    }

                } else {
                    getValidPriorContexts(row)?.each {
                        if (row.topic == it.topic && row.poster != it.poster) {
                            link(it, row)
                        }

                    }
                }
            }


        }
        links.size()

    }

    def dump() {
        CSVPrinter printer = new CSVPrinter(new FileWriter("${basepath}/${corpus}_${forceTree ? "tree" : "dag"}.${threadgap<0?"Inf":threadgap}.${topic}.csv"), CSVFormat.DEFAULT.withHeader((cols.collect {
            "f." + it
        } + cols.collect { "t." + it } + "similarity") as String[]))
        links.each { l ->
            printer.printRecord(cols.collect { l[0][it] } + cols.collect {
                l[1][it]
            } + jaccard(l[0].cleancontent, l[1].cleancontent))
        }
        printer.flush()
        printer.close()

    }

    def getValidPriorContexts(Map row) {
        peopleToContext[row.poster]?.entrySet()?.sum { ent ->
            //println ent
            if (ent.key != row.qid) {
                contextsToPosts[ent.key]
            } else []

        }

    }

    def jaccard(String one, String two) {
        List a = one.split(/\s+/)
        List b = two.split(/\s+/)
        a.intersect(b).size() / (float) ((a + b) as Set).size()

    }

    def link(orow, nrow) {
        if (orow.date >= nrow.date) {
            println "   -----------------           Problem"
        } else if (nrow.date - orow.date > threadgap) {
            //println "   -----------------           Too long"
        }
        //println "Link ${orow.uniqueId}:${orow.date} -> ${nrow.uniqueId}:${nrow.date}"
        links << [orow, nrow]
    }

    def pruneOldStuff(Date d) {
        if (threadgap > 0) {
            peopleToContext.entrySet().removeAll { a ->
                int i = a.value.size()
                a.value.entrySet().removeAll { b ->

                    (d - b.value) > threadgap

                }
//           if (i-a.value.size()) {
//               println "Removed ${i-a.value.size()} posts"
//           }
//           if (a.value.size() ==0) {
//               println ("Pruning ${a.key}")
//           }
                a.value.size() == 0

            }
        }
    }

    public static void main(String[] args) {
        //def c = ["add_and_adhd_exchange", "alzheimers_exchange", "asthma_exchange", "back_pain_exchange", "breast_cancer_exchange", "cholesterol_management_exchange", "diet_exchange", "diabetes_exchange","digestive_disorders_exchange", "epilepsy_exchange", "fibromyalgia_exchange", "fitness_and_exercise_exchange", "hepatitis_exchange", "hiv_and_aids_exchange", "menopause_exchange", "multiple_sclerosis_exchange", "osteoporosis_exchange", "pain_management_exchange", "parenting_exchange", "parkinsons_disease_exchange", "relationships_and_coping_community", "sex_and_relationships_exchange", "sexual_conditions_and_stds_exchange"]
        //def done_corpora = ["allergies_exchange", "anxiety_and_panic_disorders_exchange", "bipolar_disorder_exchange", "cancer_community","colorectal_cancer_exchange", "depression_exchange", "erectile_dysfunction_exchange", "eye_health_community","food_and_cooking_exchange", "gynecology_exchange", "heart_disease_exchange", "hypertension_and_high_blood_pressure_exchange", "infertility_and_reproduction_exchange", "knee_and_hip_replacement_exchange", "lupus_exchange", "mens_health_community", "migraines_and_headaches_exchange", "newborn_and_baby_exchange", "oral_health_exchange", "osteoarthritis_exchange", "pet_health_exchange", "pregnancy_exchange", "prostate_cancer_exchange", "raising_fit_kids_community"]
        //def corpora = [ "rheumatoid_arthritis_exchange", "skin_and_beauty_exchange", "skin_problems_and_treatments_exchange", "sleep_disorders_exchange", "smoking_cessation_exchange", "sports_medicine_exchange", "stroke_exchange", "substance_abuse_exchange"]

        //couldn't find dynamic topics for these
        def exclude = ["cancer_community","eye_health_community","raising_fit_kids_community"]

        def c = l.corpora.collect {it.name} - (l.the23() + exclude)
        boolean first = true
        Map cutoffs = [:]
        new File(basepath,"cutoffs.csv").splitEachLine(",") {
            if (first) {
                first = false
            } else {
                cutoffs[it[1][1..-2]]=Math.round(it[3] as float)
            }
        }
        println(cutoffs)
        c.each {
            BuildTree bt = new BuildTree(it, cutoffs[it], "NMF", false)
            println "Created ${bt.createTree()}"
            bt.dump()
        }


    }


}
