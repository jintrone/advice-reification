package edu.msu.mi.shared

import groovy.sql.Sql
import groovy.time.TimeCategory

/**
 * Created by josh on 7/13/17.
 */
class Loader {

    static ConfigObject props;

//    static Map COLS = [(WebMdConversation.Column.ID)  : "uniqueID", (WebMdConversation.Column.THREAD): "qid", (WebMdConversation.Column.CREATED_TS): "date", (WebMdConversation.Column.AUTHOR): "poster",
//                       (WebMdConversation.Column.TEXT): "content", (WebMdConversation.Column.REPLY): "replyTo", (WebMdConversation.Column.SUBJECT): "title"]

    private static Loader instance;


    private Loader(String user, String pass, String database = null) {

        URL propsFile = getClass().getClassLoader().getResource("webmd_analysis.groovy")
        props = new ConfigSlurper().parse(propsFile)
        props.username = user
        props.password = pass
        if (database) props.database = database
        instance = this
    }

    public static Loader init(String user, String pass, String database = null) {
        instance ?: new Loader(user, pass, database)
    }

    public static Loader instance() {
        instance
    }


    List<Corpus> getCorpora() {
        sqlConnection.rows("select * from all_corpora").collect {
            new Corpus(it.table_name)
        }
    }

    static List<String> the23() {
        ["add_and_adhd_exchange","alzheimers_exchange","asthma_exchange","back_pain_exchange","breast_cancer_exchange",
         "cholesterol_management_exchange","diabetes_exchange","diet_exchange","digestive_disorders_exchange","epilepsy_exchange",
         "fibromyalgia_exchange","fitness_and_exercise_exchange","hepatitis_exchange","hiv_and_aids_exchange","menopause_exchange",
         "multiple_sclerosis_exchange","osteoporosis_exchange","pain_management_exchange","parenting_exchange","parkinsons_disease_exchange",
         "relationships_and_coping_community","sex_and_relationships_exchange","sexual_conditions_and_stds_exchange"]
    }

    public Corpus getCorpus(String name) {
        getCorpora().collectEntries {
            [it.name, it]
        }[name] ?: null
    }


    public static Sql getSqlConnection() {
        Sql.newInstance("jdbc:mysql://localhost:3306/${props.database}", props.username, props.password, 'com.mysql.jdbc.Driver')
    }


    public static class Corpus {

        String table
        String name

        String longQuery = "select uniqueID, qid, date, poster, title, replyTo, content from ${table}" as String
        String authorQuery = "select distinct poster from ${table}" as String
        String postsByAuthor = "select poster, title, content from ${table} order by poster" as String
        String postsByResponder = "select b.poster topposter, a.qid, a.date, a.poster respondant, a.content, a.`uniqueID` from ${table} a inner join (select poster,`qid` from ${table} where localID=-1 order by poster) b on (a.qid=b.qid) where a.localID > -1 and a.poster<>b.poster and right(a.`replyTo`,4)='_top' order by topposter, a.qid, a.localID" as String
        String postsByNamedAuthors = "select count(*) count from ${table} where poster in " as String
        String postCount = "select count(*) count from ${table}" as String


        public Corpus(String table) {

            this.table = table
            this.name = table
        }


        public List<String> getPosters() {
            getSqlConnection().rows(authorQuery).collect {
                it.poster

            }
        }

        public int getNumberOfPosts() {
            getSqlConnection().firstRow(postCount).count as int
        }

        public int getNumberOfPostsBy(Collection names) {
            def values = "'${names.join('\',\'')}'"
            def query = "${postsByNamedAuthors}($values)" as String

            getSqlConnection().firstRow(query).count as int
        }

        public Date getLastPostTime() {
            getSqlConnection().firstRow("select max(date) d from ${table}" as String).d as Date
        }

        public Date getFirstPostTime() {
            getSqlConnection().firstRow("select min(date) d from ${table}" as String).d as Date
        }

        public Date[] getIntervals(int days) {
            List<Date> result = []
            Date lastPostTime =getLastPostTime()
            Date firstPostTime =getFirstPostTime()
            while (!result || result.last() < lastPostTime) {
                use(TimeCategory) {
                    result << (result ? result.last() : firstPostTime) + (days).day
                }
            }
            result as Date[]
        }


        public String toString() {
            name
        }
    }

}
