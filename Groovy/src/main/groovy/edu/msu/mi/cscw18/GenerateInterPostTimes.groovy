package edu.msu.mi.cscw18

import edu.msu.mi.shared.Loader
import groovy.sql.Sql
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVPrinter

/**
 * Created by josh on 4/11/17.
 */
class GenerateInterPostTimes {
    static Loader l = Loader.init("webmd", "webmd", "webmd_enriched")
    String basepath = "/Users/josh/Dropbox/@PAPERS/2017/CSCW/data/"
    Sql cnx = l.getSqlConnection()


    public GenerateInterPostTimes(List corpora) {


        def ptimes = [:]
        def lastQid = null
        CSVPrinter p = new CSVPrinter(new FileWriter(new File(basepath,"all.innerposttimes.csv")),CSVFormat.DEFAULT.withHeader("corpus","uniqueId","poster","class","delta"))
        corpora.each { corpus->
            ptimes = [:]
            lastQid = null
            cnx.eachRow("select qid, uniqueID, localID, ${corpus}.poster,class k, date from ${corpus} inner join ${corpus}_users on ${corpus}_users.poster=${corpus}.poster order by qid, localID" as String) {
                if (lastQid == null || lastQid != it.qid) {
                    ptimes.clear()
                    lastQid = it.qid
                }

                if (ptimes[it.poster] && !((ptimes[it.poster][1]+1 == it.localID) && (it.date.time - ptimes[it.poster][0].time) < 60*60*1000)) {
                    p.printRecord(corpus, it.uniqueID, it.poster, it.k, (it.date.time - ptimes[it.poster][0].time) / (1000 * 60 * 60 * 24))
                }
                ptimes[it.poster] = [it.date, it.localID]

            }

            println("Done ${corpus}")
        }
        p.close()

    }

    public static void main(String[] args) {

        new GenerateInterPostTimes(l.getCorpora().collect {it.name})
    }


}
