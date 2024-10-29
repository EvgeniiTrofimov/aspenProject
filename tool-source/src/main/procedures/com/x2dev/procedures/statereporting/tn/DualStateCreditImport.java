/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2002-2016 Follett School Solutions.
 * All rights reserved.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.UserToolDetail;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.utils.io.file.CSVWriter;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.io.IOUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This imports dual credit data from the state of TN into StudentAssessment
 * objects.
 * <p>
 * This requires you to already have an AssessmentDefinition (which is passed
 * via the {@link #PARAM_ASSESSMENT_DEFINITION_OID} parameter). Preferably this
 * should be hard-coded into the input parameters as a hidden value so the user
 * doesn't have to interact with this.
 * <p>
 * The AssessmentDefinition this relates to needs to have
 * AssessmentColumnDefinitions set up that define the "Percentile Score" and
 * "Semester Taken". (See {@link #COL_PERCENTILE_SCORE} and
 * {@link #COL_SEMESTER_TAKEN}).
 * <p>
 * Additionally: this relies on the alias for the TDOE state ID (See
 * {@link #ALIAS_DOE_STATE_ID}), and it assumes there is a DataDictionaryField
 * for StudentAssessment with the long name "Test Description" (See
 * {@link #COL_TEST_DESCRIPTION}).
 * <p>
 * Because this pulls data from a remote server, any number of problems may come
 * up that are beyond our control. The login credentials for the remote server
 * are built-in to this import.
 * <p>
 * This draft bundles several helper classes that are in Customization's repo as
 * inner classes. (But the ability to deploy a jar is still not yet live for
 * customers, so we have to improvise for now. Hopefully eventually we can
 * delete those and clean this up.)
 * <p>
 * The recommended input definition for this class resembles the following,
 * although the exact AssessmentDefinition oid will vary across customers:
 * <p>
 *
 * <pre>
 * &lt;tool-input allow-school-select="false" district-support="true">
 *
 *     &lt;!-- ********************* -->
 *     &lt;!-- ASSESSMENT DEFINITION -->
 *     &lt;!-- ********************* -->
 *     &lt;input name="assessmentDefinitionOid" data-type="string" display-type="hidden" display-name="Assessment Definition" default-value="ASD000000HfErM">
 *      &lt;picklist field-id="asdName" required="true">
 *       &lt;field id="asdName" sort="true" />
 *       &lt;field id="asdSubject" />
 *      &lt;/picklist>
 *     &lt;/input>
 *
 *     &lt;input name="previewOnly" data-type="boolean" display-type="checkbox" display-name="Preview Only" default-value="true" />
 *     &lt;input name="writeTable" data-type="boolean" display-type="checkbox" display-name="Write Table of Incoming Data" default-value="true" />
 *
 * &lt;/tool-input>
 * </pre>
 *
 * @author Follett School Solutions
 */
public class DualStateCreditImport extends ToolJavaSource {

    /**
     * The Class AccessToken.
     */
    static class AccessToken {
        String m_token;
        long created = System.currentTimeMillis();
        Integer seconds;

        /**
         * Instantiates a new access token.
         *
         * @param data Map
         */
        public AccessToken(Map data) {
            m_token = (String) data.get("access_token");
            seconds = Integer.parseInt(data.get("expires_in").toString());
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return m_token;
        }

        /**
         * Checks if is expired.
         *
         * @return true, if is expired
         */
        public boolean isExpired() {
            long elapsedMillis = System.currentTimeMillis() - created;
            long elapsedSeconds = elapsedMillis / 1000;
            return elapsedSeconds > seconds - 10;
        }
    }

    /**
     * A semester term such as "FALL 2015" or "SPRING 2014".
     * <p>
     * The static {@link #get(String)} method will parse any variation of "Fall
     * 2014", "F2014", "Fall 14" as the same object.
     *
     * @author Follett School Solutions
     */
    static class FuzzyTerm {
        /**
         * A season.
         */
        static enum Season {
            FALL, SPRING, SUMMER
        }

        private static HashSet<FuzzyTerm> ALL_TERMS = new HashSet<>();

        /**
         * This should parse "F2014", "Fall 2014" and "Fall 14" as the same object.
         *
         * @param str String
         * @return FuzzyTerm
         */
        public static FuzzyTerm get(String str) {
            if (str == null) {
                throw new NullPointerException();
            }
            if (str.length() == 0) {
                throw new IllegalArgumentException();
            }

            StringBuffer letters = new StringBuffer();
            StringBuffer digits = new StringBuffer();
            for (int a = 0; a < str.length(); a++) {
                char ch = str.charAt(a);
                if (Character.isLetter(ch)) {
                    letters.append(ch);
                } else if (Character.isDigit(ch)) {
                    digits.append(ch);
                }
            }

            int year = Integer.parseInt(digits.toString());
            // convert "14" to "2014"
            if (year < 100) {
                year = 2000 + year;
            }

            Season season = null;
            String seasonStr = letters.toString().toUpperCase();
            if (seasonStr.startsWith("F")) {
                season = Season.FALL;
            } else if (seasonStr.startsWith("SU")) {
                season = Season.SUMMER;
            } else if (seasonStr.startsWith("S")) {
                season = Season.SPRING;
            } else {
                throw new RuntimeException("The season \"" + letters.toString() + "\" could not be parsed.");
            }

            FuzzyTerm newTerm = new FuzzyTerm(season, year);
            synchronized (ALL_TERMS) {
                for (FuzzyTerm existing : ALL_TERMS) {
                    if (existing.equals(newTerm)) {
                        return existing;
                    }
                }
                ALL_TERMS.add(newTerm);
            }

            return newTerm;
        }

        /**
         * The season for this term.
         */
        final protected Season m_season;

        /**
         * The year for this term.
         */
        final protected int m_year;

        /**
         * Instantiates a new fuzzy term.
         *
         * @param season Season
         * @param year int
         */
        private FuzzyTerm(Season season, int year) {
            m_season = season;
            m_year = year;
        }

        /**
         * Return the season for this term.
         *
         * @return Season the season for this term.
         */
        public Season getSeason() {
            return m_season;
        }

        /**
         * Return the year for this term.
         *
         * @return int the year for this term.
         */
        public int getYear() {
            return m_year;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return m_season.name().charAt(0) + m_season.name().toLowerCase().substring(1) + " " + m_year;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_year;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof FuzzyTerm)) {
                return false;
            }
            FuzzyTerm other = (FuzzyTerm) obj;

            return other.m_year == m_year && other.m_season.equals(m_season);
        }
    }

    /**
     * This helps write sections of plaintext output. A summary is printed towards
     * the top of the output, and then an optional method to write large blocks of
     * data follows.
     *
     * @author Follett School Solutions
     */
    static abstract class SectionWriter {

        /**
         * Return a sentence summary, such as: "There are X records from the state that
         * were imported as new records to Aspen. See section \"TITLE\" below for
         * details."
         *
         * @param rolledBackTransaction boolean
         * @return String
         */
        public abstract String getSummary(boolean rolledBackTransaction);

        /**
         * A short title (up to about 5 words), such as "Aspen-Only Records". If this
         * returns null then {@link #write(Writer)} is not called.
         *
         * @return String a short title (up to about 5 words) such as "Aspen-Only
         *         Records". If this returns null then {@link #write(Writer)} is not
         *         called.
         */
        public abstract String getTitle();

        /**
         * Write the data, presumably in table form. This will be stated below the
         * title, and it may be indefinitely long.
         *
         * @param broker X2Broker
         * @param writer Writer
         * @param rolledBackTransaction boolean
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public abstract void write(X2Broker broker, Writer writer, boolean rolledBackTransaction) throws IOException;

        /**
         * Return true if this section describes an error.
         *
         * @return boolean true if this section describes an error.
         */
        public abstract boolean isError();

        /**
         * Describe.
         *
         * @param assessments Collection<StudentAssessment>
         * @return String
         */
        protected String describe(Collection<StudentAssessment> assessments) {
            DataGrid grid = new DataGrid();
            for (StudentAssessment assessment : assessments) {
                grid.append();
                grid.set("Date", assessment.getDate());
                grid.set("FieldA001", assessment.getFieldA001());
                grid.set("FieldA002", assessment.getFieldA002());
                grid.set("FieldB001", assessment.getFieldB001());
                grid.set("FieldC001 (Test Description)", assessment.getFieldC001());
                grid.set("Name View", assessment.getStudent().getNameView());
                grid.set("Oid", assessment.getOid());
                grid.set("StudentOid", assessment.getStudentOid());
            }
            grid.sort("Name View", true);
            return grid.format(false, false, false);
        }
    }

    int problemCtr = 1;

    /**
     * This is a description of a problem, which may or may not be considered an
     * error that requires this import to abort.
     *
     * @author Follett School Solutions
     */
    class ProblemSection extends SectionWriter {

        String m_message;
        Throwable m_throwable;
        boolean m_error;
        int problemId = problemCtr++;

        /**
         * Instantiates a new problem section.
         *
         * @param t Throwable
         * @param error boolean
         */
        public ProblemSection(Throwable t, boolean error) {
            m_throwable = t;
            m_error = error;
        }

        /**
         * Instantiates a new problem section.
         *
         * @param str String
         * @param error boolean
         */
        public ProblemSection(String str, boolean error) {
            m_message = str;
            m_error = error;
        }

        /**
         * Checks if is error.
         *
         * @return true, if is error
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#isError()
         */
        @Override
        public boolean isError() {
            return m_error;
        }

        /**
         * Gets the summary.
         *
         * @param rolledBackTransaction boolean
         * @return String
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#getSummary(boolean)
         */
        @Override
        public String getSummary(boolean rolledBackTransaction) {
            String summary;
            if (m_error) {
                summary = "An error occurred. See the \"Problem\"(s) section below.";
            } else {
                summary = "A potential error occurred. See the \"Problem\" section(s) below.";
            }
            if (m_message != null) {
                summary += " " + m_message;
            }
            if (m_throwable != null) {
                if (m_throwable.getMessage() != null) {
                    summary += " " + m_throwable.getMessage();
                }
            }

            return summary;
        }

        /**
         * Gets the title.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#getTitle()
         */
        @Override
        public String getTitle() {
            return "Problem " + problemId;
        }

        /**
         * Write.
         *
         * @param broker X2Broker
         * @param writer Writer
         * @param rolledBackTransaction boolean
         * @throws IOException Signals that an I/O exception has occurred.
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#write(com.follett.fsc.core.k12.business.X2Broker,
         *      java.io.Writer, boolean)
         */
        @Override
        public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction) throws IOException {
            if (m_message != null) {
                writer.write("\n" + m_message + "\n");
            }
            if (m_throwable != null) {
                String stackTrace = LoggerUtils.convertThrowableToString(m_throwable);
                writer.write("\n" + stackTrace + "\n");
            }
        }
    }

    /**
     * This queries for student Data against Dynetic's servers.
     *
     * @author Follett School Solutions
     */
    class QueryStudentData implements Runnable {

        /**
         * Run this query.
         */
        @Override
        public void run() {
            int attemptMax = 20;
            for (int attempt = 0; attempt < attemptMax; attempt++) {
                if (pullStudentsForSchool()) {
                    return;
                }
            }
            m_output.add(new ProblemSection(
                    "Received " + attemptMax + " consecutive errors while trying to fetch results.", true));
        }

        /**
         * Pull students for school.
         *
         * @return true, if successful
         */
        private boolean pullStudentsForSchool() {
            validateStatus();

            HttpURLConnection connection = null;

            try {
                URL url = new URL("https://epso-api.azurewebsites.net/api/SDCExamResultQuery?hs_district_no="
                        + getOrganization().getId() + "&course_term="
                        + getParameter(PARAM_COURSE_TERM_CONNECTION));

                connection = (HttpURLConnection) url.openConnection();
                connection.setRequestMethod("GET");
                connection.setRequestProperty("Authorization", "Bearer " + getAccessToken());
                connection.setDoInput(true);

                connection.connect();

                try (InputStream response = connection.getInputStream()) {
                    String data = read(response);

                    try {
                        ObjectMapper mapper = new ObjectMapper();
                        List list = mapper.readValue(data, List.class);
                        for (int a = 0; a < list.size(); a++) {
                            Map map = (Map) list.get(a);
                            if (map.size() != 0) {
                                SisStudent student = getStudentbyId(map.get("tdoe_student_id"));
                                if (student != null) {
                                    map.put(KEY_ASPEN_STD_OID, student.getOid());
                                    map.put(KEY_ASPEN_STD_PSN_FIRST_NAME, student.getPerson().getFirstName());
                                    map.put(KEY_ASPEN_STD_PSN_LAST_NAME, student.getPerson().getLastName());
                                    map.put(KEY_ASPEN_STD_PSN_DOB, student.getPerson().getDob());
                                }
                                m_studentRecords.add(map);
                            }
                        }
                        return true;
                    } catch (Exception t) {
                        System.err.println(data);
                        throw t;
                    }
                } finally {
                    connection.disconnect();
                }
            } catch (Exception e) {
                try {
                    ObjectMapper mapper = new ObjectMapper();
                    String error = read(connection.getErrorStream());
                    Map map = mapper.readValue(error, Map.class);
                    Throwable throwable = new Exception("An error occurred parsing the results for: " + map, e);
                    m_output.add(new ProblemSection(throwable, false));
                } catch (Throwable t) {
                    Throwable throwable = new Exception("An error occurred parsing the results.", e);
                    m_output.add(new ProblemSection(throwable, false));
                }
                return false;
            }
        }

        /**
         * Finds an Aspen SisSutdent object by searching it on all students in the
         * system based on the provided StateID.
         *
         * @param object Object
         * @return Sis student
         */
        private SisStudent getStudentbyId(Object object) {
            String tdoe_student_oid = (String) object;

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_STATE_ID);

            String stateIdFieldFromAlias = field.getJavaName();

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(stateIdFieldFromAlias, tdoe_student_oid);
            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            SisStudent student = (SisStudent) getBroker().getBeanByQuery(query);

            return student;
        }
    }

    /**
     * This is a description of ValidationErrors if a bean isn't saved.
     *
     * @author Follett School Solutions
     */
    class ValidationErrors extends SectionWriter {
        /**
         * The validation errors when we attempted to save the bean.
         */
        List<ValidationError> m_validationErrors;

        /**
         * The bean that we tried to save.
         */
        X2BaseBean m_bean;

        /**
         * Instantiates a new validation errors.
         *
         * @param validationErrors The validation errors when we attempted to save the bean.
         * @param bean The bean that we tried to save.
         */
        public ValidationErrors(List validationErrors, X2BaseBean bean) {
            m_validationErrors = validationErrors;
            m_bean = bean;
        }

        /**
         * Checks if is error.
         *
         * @return true, if is error
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#isError()
         */
        @Override
        public boolean isError() {
            return true;
        }

        /**
         * Gets the summary.
         *
         * @param rolledBackTransaction boolean
         * @return String
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#getSummary(boolean)
         */
        @Override
        public String getSummary(boolean rolledBackTransaction) {
            return "An error occurred saving a bean. See the section \"Validation Error\" below.";
        }

        /**
         * Gets the title.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#getTitle()
         */
        @Override
        public String getTitle() {
            return "Validation Error";
        }

        /**
         * Write.
         *
         * @param broker X2Broker
         * @param writer Writer
         * @param rolledBackTransaction boolean
         * @throws IOException Signals that an I/O exception has occurred.
         * @see com.x2dev.procedures.statereporting.tn.DualStateCreditImport.SectionWriter#write(com.follett.fsc.core.k12.business.X2Broker,
         *      java.io.Writer, boolean)
         */
        @Override
        public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction) throws IOException {
            for (int a = 0; a < m_validationErrors.size(); a++) {
                writer.write(m_validationErrors.get(a) + "\n");
            }
            writer.write("\n");
        }

    }

    /**
     * The alias for TDOE state IDs in SisStudent objects.
     */
    public static final String ALIAS_DOE_STATE_ID = "DOE EIS STATE ID";

    /**
     * The parameter name that maps to an AssessmentDefinition oid.
     */
    public static final String PARAM_ASSESSMENT_DEFINITION_ID = "assessmentDefinitionId";

    /**
     * The Parameter for Course Term Parameter to be passed on the connection URL.
     * i.e.: Spring2018 = "S2018" or Fall2018 = "F2018"
     */
    public static final String PARAM_COURSE_TERM_CONNECTION = "courseTermConnetionParameter";

    /**
     * The parameter for preview-only mode. When this is true the transaction is
     * rolled back even if there isn't an error.
     */
    public static final String PARAM_PREVIEW_ONLY = "previewOnly";

    /**
     * This boolean paramater indicates whether we should write a table of data as
     * the output, or the human-readable log. The table of data is a CSV file of the
     * data the state provided us with a few additional Aspen-specific columns
     * sprinkled in.
     * <p>
     * Regardless of this parameter: the human-readable summary is available in the
     * "logs" section of this tool.
     */
    public static final String PARAM_WRITE_TABLE = "writeTable";

    /**
     * The is the long name of the AssessmentColumnDefinition used to resolve what
     * field the "Percentile Score" field is in the StudentAssessment.
     */
    public static final String COL_PERCENTILE_SCORE = "Percentile Score";

    /**
     * The is the long name of the AssessmentColumnDefinition used to resolve what
     * field the "Semester Taken" field is in the StudentAssessment.
     */
    public static final String COL_SEMESTER_TAKEN = "Semester Taken";

    /**
     * The field long name that relates to the DataDictionaryField used by a
     * StudentAssessment to store the test description.
     */
    public static final String COL_TEST_DESCRIPTION = "Test Description";

    /** The key in m_studentRecords that relates to Aspen's student oid */
    protected static final String KEY_ASPEN_STD_OID = "aspenStdOid";

    /** The key in m_studentRecords that relates to Aspen's dob of that student. */
    protected static final String KEY_ASPEN_STD_PSN_DOB = "aspenStdPsnDob";

    /**
     * The key in m_studentRecords that relates to Aspen's first name of that
     * student.
     */
    protected static final String KEY_ASPEN_STD_PSN_FIRST_NAME = "aspenStdPsnFirstName";

    /**
     * The key in m_studentRecords that relates to Aspen's last name of that
     * student.
     */
    protected static final String KEY_ASPEN_STD_PSN_LAST_NAME = "aspenStdPsnLastName";

    /**
     * A map of student oids to all the StudentAssessments associated with that oid
     * (and the AssessmentDefinition this import relates to).
     */
    protected Map<String, StudentAssessment[]> m_assessmentsByStudentOid;

    /**
     * The AssessmentDefinition this import relates to.
     */
    protected AssessmentDefinition m_assessmentDefinition;

    /**
     * A list of all the StudentAssessments that perfectly matched, so no edits were
     * necessary.
     */
    protected List<StudentAssessment> m_exactMatches = new ArrayList<>();

    /**
     * This is a list of student IDs actively being queried (used for debugging).
     */
    // protected Set<String> m_activeQueries = Collections.synchronizedSet(new
    // HashSet<String>());

    /**
     * A list of all the StudentAssessments this import modified.
     */
    protected List<StudentAssessment> m_modifiedStudentAssessments = new ArrayList<>();

    /**
     * A list of all the new StudentAssessments this import created.
     */
    protected List<StudentAssessment> m_newStudentAssessments = new ArrayList<>();

    /**
     * This is a list of student IDs actively being queried (used for debugging).
     */
    // protected Set<String> m_activeQueries = Collections.synchronizedSet(new
    // HashSet<String>());

    /**
     * The SectionWriters used to describe this import to the user.
     */
    protected List<SectionWriter> m_output = new ArrayList<>();

    /**
     * A list of all the StudentAssessments this import removed.
     */
    protected List<StudentAssessment> m_removedStudentAssessments = new ArrayList<>();

    /**
     * Each element in this collection is a map of state data, plus a few fields we
     * artificially populate like {@link #KEY_ASPEN_STD_OID}.
     */
    protected Collection<Map> m_studentRecords = Collections.synchronizedSet(new HashSet<Map>());

    /**
     * A Map of TDOE course names to abbreviated names stored in Aspen. For example
     * this converts "Agriculture Business Finance" (key) to "Agri. Business
     * Finance" (value).
     */
    protected Map<String, String> m_tdoeCourseNameToAbbrevName = createTDOECourseNameToAbbreviationMap();



    /**
     * This is a list of student IDs actively being queried (used for debugging).
     */
    // protected Set<String> m_activeQueries = Collections.synchronizedSet(new
    // HashSet<String>());

    protected Runnable m_updateProgressRunnable;

    private AccessToken m_accessToken;

    /**
     * A LUT mapping AssessmentColumnDefinition long names to java names. For
     * example it may map "Percentile Score" to "FieldA002".
     */
    private Map<String, String> assessmentColumnLUT;

    /**
     * A LUT of DataDictionaryFields based on their user long name.
     */
    private Map<String, DataDictionaryField> m_dataDictionaryFieldLUT;

    /**
     * This method is provided as a convenient way for subclasses to initialize
     * member variables before the <code>run()</code> method is called.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void initialize() throws X2BaseException {
        getJob().getInput().setFormat(ToolInput.TXT_FORMAT);
        super.initialize();
    }

    /**
     * This logs a message and returns the UserToolDetail so you can continually
     * update that message over time.
     *
     * @param detail
     *        the detail to update, or null if we're creating a new detail.
     * @param level
     *        the Level to associate with the message.
     * @param message
     *        the message to log.
     * @return the UserToolDetail object that was created and may be updated going
     *         forward.
     */
    protected UserToolDetail logToolMessage(UserToolDetail detail, Level level, String message) {
        ToolJob job = getJob();

        if (detail == null) {
            detail = X2BaseBean.newInstance(UserToolDetail.class, getBroker().getPersistenceKey());
        }
        detail.setMessage(message);
        detail.setTimestamp(System.currentTimeMillis());
        detail.setType(level.getName());
        detail.setUserToolLogOid(job.getJobLogManager().getUserToolLog().getOid());

        getBroker().saveBeanForced(detail);

        return detail;
    }

    /**
     * This throws a RuntimeException if something has already gone wrong (including
     * previous runnables failing, or the user cancelling this task).
     */
    protected void validateStatus() {
        ThreadUtils.checkInterrupt();
        if (hasError()) {
            throw new RuntimeException("Cancelled because preexisting errors were detected.");
        }
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        final UserToolDetail detail = logToolMessage(null, Level.INFO, "Beginning run() method...");

        Boolean previewOnly = (Boolean) getParameter(PARAM_PREVIEW_ONLY);

        getBroker().beginTransaction();
        try {
            String id = (String) getParameter(PARAM_ASSESSMENT_DEFINITION_ID);
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(AssessmentDefinition.COL_ID, id);
            BeanQuery query = new BeanQuery(AssessmentDefinition.class, criteria);
            m_assessmentDefinition = (AssessmentDefinition) getBroker().getBeanByQuery(query);
            if (m_assessmentDefinition == null) {
                throw new NullPointerException("No AssessmentDefinition for \"" + id + "\" found.");
            }

            m_assessmentsByStudentOid = getStudentAssessmentsByOid(m_assessmentDefinition.getOid());
            new QueryStudentData().run();

            logToolMessage(detail, Level.INFO,
                    "Queried all " + NumberFormat.getInstance().format(m_studentRecords.size()) + " students.");

            // add the summary message:
            m_output.add(new SectionWriter() {

                @Override
                public String getSummary(boolean rolledBackTransaction) {
                    int students = 0;
                    int noMatch = 0;
                    int records = m_studentRecords.size();
                    Set<String> studentOids = new HashSet<>();
                    for (Map<String, String> stateData : m_studentRecords) {
                        String studentOid = stateData.get(KEY_ASPEN_STD_OID);
                        if (!StringUtils.isEmpty(studentOid)) {
                            if (studentOids.add(studentOid)) {
                                students++;
                            }
                        } else {
                            ++noMatch;
                        }
                    }

                    return "I queried " + NumberFormat.getIntegerInstance().format(records)
                            + " records from the state, and found " + NumberFormat.getIntegerInstance().format(students)
                            + " matching students had one or more dual-credit scores.\n" + "(There were "
                            + NumberFormat.getIntegerInstance().format(noMatch) + " records that do not match.)";
                }

                @Override
                public boolean isError() {
                    return false;
                }

                @Override
                public String getTitle() {
                    return null;
                }

                @Override
                public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction) {
                    // intentionally empty
                }
            });

            final List<Map> noMatches = getNoMatches();
            if (!noMatches.isEmpty()) {
                // add the summary message:
                m_output.add(new SectionWriter() {

                    @Override
                    public String getSummary(boolean rolledBackTransaction) {
                        return "There are " + NumberFormat.getInstance().format(noMatches.size())
                                + " students queried from Dynetics that don't match Aspen.";
                    }

                    @Override
                    public boolean isError() {
                        return false;
                    }

                    @Override
                    public String getTitle() {
                        return "Dynetics Names Without Matches";
                    }

                    @Override
                    public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction)
                            throws IOException {
                        DataGrid grid = new DataGrid();
                        for (Map map : noMatches) {
                            grid.append();
                            grid.set("SASID", map.get("tdoe_student_id"));
                            grid.set("First Name (remote)", map.get("student_first_name"));
                            grid.set("Last Name (remote)", map.get("student_last_name"));
                            grid.set("Date of Birth (remote)", map.get("student_dob"));
                        }
                        grid.sort(Arrays.asList("Last Name (remote)", "First Name (remote)"), true);
                        writer.write(grid.format(false, false, false));
                    }
                });
            }

            final List<Map> suspiciousNameMatches = getSuspiciousNames();
            if (!suspiciousNameMatches.isEmpty()) {
                // add the summary message:
                m_output.add(new SectionWriter() {

                    @Override
                    public String getSummary(boolean rolledBackTransaction) {
                        return "There are " + NumberFormat.getInstance().format(suspiciousNameMatches.size())
                                + " student names that don't match between Aspen and Dynetics. Someone might have the wrong student ID.";
                    }

                    @Override
                    public boolean isError() {
                        return false;
                    }

                    @Override
                    public String getTitle() {
                        return "Suspicious Names / Matches";
                    }

                    @Override
                    public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction)
                            throws IOException {
                        DataGrid grid = new DataGrid();
                        for (Map map : suspiciousNameMatches) {
                            grid.append();
                            grid.set("SASID", map.get("tdoe_student_id"));
                            grid.set("First Name (remote)", map.get("student_first_name"));
                            grid.set("Last Name (remote)", map.get("student_last_name"));
                            grid.set("Date of Birth (remote)", map.get("student_dob"));
                            grid.set("First Name (Aspen)", map.get(KEY_ASPEN_STD_PSN_FIRST_NAME));
                            grid.set("Last Name (Aspen)", map.get(KEY_ASPEN_STD_PSN_LAST_NAME));
                            grid.set("Date of Birth (Aspen)", map.get(KEY_ASPEN_STD_PSN_DOB));
                            grid.set("Student Oid (Aspen)", map.get(KEY_ASPEN_STD_OID));
                        }
                        grid.sort(Arrays.asList("Last Name (remote)", "First Name (remote)"), true);
                        writer.write(grid.format(false, false, false));
                    }
                });
            }

            final Collection<StudentAssessment> allPreexistingAssessments = new HashSet<>(
                    m_assessmentsByStudentOid.size());
            for (StudentAssessment[] studentAssessments : m_assessmentsByStudentOid.values()) {
                for (StudentAssessment studentAssessment : studentAssessments) {
                    validateStatus();
                    allPreexistingAssessments.add(studentAssessment);
                }
            }

            SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            for (Map stateRecord : m_studentRecords) {
                validateStatus();

                String studentOID = (String) stateRecord.get(KEY_ASPEN_STD_OID);
                // skip NoMatches
                if (StringUtils.isEmpty(studentOID)) {
                    continue;
                }
                FuzzyTerm term = FuzzyTerm.get((String) stateRecord.get("course_term"));
                String courseName = (String) stateRecord.get("tdoe_course_name");
                PlainDate date = new PlainDate(format.parse((String) stateRecord.get("exam_date")));
                StudentAssessment preexistingRecord = getStudentAssessment(studentOID, term, courseName);

                StudentAssessment bean;
                boolean isNew;
                if (preexistingRecord == null) {
                    bean = X2BaseBean.newInstance(StudentAssessment.class, getBroker().getPersistenceKey());
                    m_newStudentAssessments.add(bean);
                    isNew = true;
                } else {
                    isNew = false;
                    allPreexistingAssessments.remove(preexistingRecord);
                    bean = (StudentAssessment) preexistingRecord.cloneBean();
                }

                bean.setStudentOid(studentOID);
                bean.setAssessmentDefinitionOid(m_assessmentDefinition.getOid());
                bean.setDate(date);
                String score;
                if (stateRecord.get("exam_score") == null) {
                    score = "n/a";
                } else {
                    score = stateRecord.get("exam_score").toString();
                }

                setAssessmentProperty(bean, COL_PERCENTILE_SCORE, score);
                setAssessmentProperty(bean, COL_SEMESTER_TAKEN, term.toString());
                setField(bean, COL_TEST_DESCRIPTION, "SDC Exam (" + courseName + ")");

                if (bean.isDirty()) {
                    List validationErrors = getBroker().saveBean(bean);

                    if (validationErrors.size() > 0) {
                        m_output.add(new ValidationErrors(validationErrors, bean));
                    } else if (!isNew) {
                        m_modifiedStudentAssessments.add(preexistingRecord);
                        m_modifiedStudentAssessments.add(bean);
                    }
                } else {
                    m_exactMatches.add(bean);
                }
            }

            validateStatus();

            allPreexistingAssessments.removeAll(m_removedStudentAssessments);

            // list this warning first:

            if (allPreexistingAssessments.size() > 0) {
                m_output.add(new SectionWriter() {

                    @Override
                    public String getSummary(boolean rolledBackTransaction) {
                        return "WARNING: There are "
                                + NumberFormat.getIntegerInstance().format(allPreexistingAssessments.size())
                                + " records in Aspen that DO NOT MATCH data provided by the state. This suggests either:\n"
                                + "1. This import isn't matching records correctly, or\n"
                                + "2. Aspen's data is wrong, or\n" + "3. The state's data is incomplete.\n"
                                + "See the section titled \"Aspen-Only Records\" below.";

                    }

                    @Override
                    public String getTitle() {
                        return "Aspen-Only Records";
                    }

                    @Override
                    public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction)
                            throws IOException {
                        String s = describe(allPreexistingAssessments);
                        writer.write(s);
                    }

                    @Override
                    public boolean isError() {
                        return false;
                    }
                });
            }

            // list all other normal-feedback messages next:
            if (m_newStudentAssessments.size() > 0) {
                m_output.add(new SectionWriter() {

                    @Override
                    public String getSummary(boolean rolledBackTransaction) {
                        if (rolledBackTransaction) {
                            return "There are "
                                    + NumberFormat.getIntegerInstance().format(m_newStudentAssessments.size())
                                    + " records from the state that will be imported as new records in Aspen. See section \"New Records\" below.";
                        }

                        return "There are " + NumberFormat.getIntegerInstance().format(m_newStudentAssessments.size())
                                + " records from the state that were imported as new records in Aspen. See section \"New Records\" below.";
                    }

                    @Override
                    public String getTitle() {
                        return "New Records";
                    }

                    @Override
                    public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction)
                            throws IOException {
                        String s = describe(m_newStudentAssessments);
                        writer.write(s);
                    }

                    @Override
                    public boolean isError() {
                        return false;
                    }
                });
            }

            if (m_modifiedStudentAssessments.size() > 0) {
                m_output.add(new SectionWriter() {

                    @Override
                    public String getSummary(boolean rolledBackTransaction) {
                        if (rolledBackTransaction) {
                            return "There are "
                                    + NumberFormat.getIntegerInstance().format(m_modifiedStudentAssessments.size())
                                    + " records from the state that resemble Aspen records but will be saved with minor changes. See section \"Updated Records\" below.";
                        }

                        return "There are "
                                + NumberFormat.getIntegerInstance().format(m_modifiedStudentAssessments.size())
                                + " records from the state that resemble Aspen records but were saved with minor changes. See section \"Updated Records\" below.";
                    }

                    @Override
                    public String getTitle() {
                        return "Updated Records";
                    }

                    @Override
                    public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction)
                            throws IOException {
                        String s = describe(m_modifiedStudentAssessments);
                        writer.write(s);
                    }

                    @Override
                    public boolean isError() {
                        return false;
                    }
                });
            }

            if (m_removedStudentAssessments.size() > 0) {
                m_output.add(new SectionWriter() {

                    @Override
                    public String getSummary(boolean rolledBackTransaction) {
                        if (rolledBackTransaction) {
                            return "There are "
                                    + NumberFormat.getIntegerInstance().format(m_removedStudentAssessments.size())
                                    + " records in Aspen that I should remove because they appear redundant. See section \"Removed Records\" below.";
                        }

                        return "There are "
                                + NumberFormat.getIntegerInstance().format(m_removedStudentAssessments.size())
                                + " records in Aspen that I removed because they appeared redundant. See section \"Removed Records\" below.";
                    }

                    @Override
                    public String getTitle() {
                        return "Removed Records";
                    }

                    @Override
                    public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction)
                            throws IOException {
                        String s = describe(m_removedStudentAssessments);
                        writer.write(s);
                    }

                    @Override
                    public boolean isError() {
                        return false;
                    }
                });
            }

            if (m_exactMatches.size() > 0) {
                m_output.add(new SectionWriter() {

                    @Override
                    public String getSummary(boolean rolledBackTransaction) {
                        return "There are " + NumberFormat.getIntegerInstance().format(m_exactMatches.size())
                                + " records from the state that matched Aspen records (no changes were necessary). See section \"Matching Records\" below.";
                    }

                    @Override
                    public String getTitle() {
                        return "Matching Records";
                    }

                    @Override
                    public void write(X2Broker broker, Writer writer, boolean rolledBackTransaction)
                            throws IOException {
                        String s = describe(m_exactMatches);
                        writer.write(s);
                    }

                    @Override
                    public boolean isError() {
                        return false;
                    }
                });
            }
        } catch (Throwable t) {
            if (!hasError()) {
                m_output.add(new ProblemSection(t, true));
            }
        } finally {
            boolean hasError = hasError();
            if (hasError || previewOnly.booleanValue()) {
                getBroker().rollbackTransaction();
                writeResults(true);
            } else {
                getBroker().commitTransaction();
                writeResults(false);
            }
        }
    }

    /**
     * Return an access token for a session with Dynetics to retrieve state data.
     * This creates a new AccessToken if necessary (either for first-time use or
     * because the existing token is about to expire).
     *
     * @return String an access token for a session with Dynetics to retrieve state
     *         data.
     * @throws Exception exception
     */
    public synchronized AccessToken getAccessToken() throws Exception {
        if (m_accessToken == null || m_accessToken.isExpired()) {
            m_accessToken = createAccessToken();
        }
        return m_accessToken;
    }

    /**
     * Make this method public so inner classes can access it non-synthetically.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    public X2Broker getBroker() {
        return super.getBroker();
    }

    /**
     * Create a map of TDOE course names to abbreviated Aspen names.
     *
     * @return Map<String,String> a map of TDOE course names to abbreviated Aspen
     *         names.
     */
    protected Map<String, String> createTDOECourseNameToAbbreviationMap() {
        Map<String, String> returnValue = new HashMap<>();
        returnValue.put("Agriculture Business Finance", "Agri. Business Finance");
        returnValue.put("Criminal Justice 1", "Criminal Justice I");

        return returnValue;
    }

    /**
     * Return a collection of all the beans of a given type.
     *
     * @param <T> the generic type
     * @param type the type of bean to retrieve
     * @return Collection<T> a collection of all the beans of the given type.
     */
    protected <T extends X2BaseBean> Collection<T> getBeans(Class<T> type) {
        X2Criteria criteria = new X2Criteria();
        BeanQuery query = new BeanQuery(type, criteria);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Return a map of student oids to all the StudentAssessments that relate to
     * them.
     *
     * @param definitionOid
     *        the AssessmentDefinition oid.
     *
     * @return Map<String,StudentAssessment[]> a map of student oids to all the
     *         StudentAssessments that student uses (that are part of the
     *         <code>definitionOid</code> parameter).
     */
    protected Map<String, StudentAssessment[]> getStudentAssessmentsByOid(String definitionOid) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, definitionOid);
        BeanQuery query = new BeanQuery(StudentAssessment.class, criteria);
        Map<String, StudentAssessment[]> map = new HashMap<>();

        FuzzyTerm selectedTerm = FuzzyTerm.get((String) getParameter(PARAM_COURSE_TERM_CONNECTION));

        QueryIterator iter = getBroker().getIteratorByQuery(query);
        try {
            while (iter.hasNext()) {
                StudentAssessment sa = (StudentAssessment) iter.next();
                String oid = sa.getStudentOid();
                StudentAssessment[] array = map.get(oid);
                FuzzyTerm term = null;
                try {
                    term = FuzzyTerm.get(getAssessmentProperty(sa, COL_SEMESTER_TAKEN));
                } catch (Exception e) {
                    logToolMessage(Level.INFO, "IF YOU SEE THIS, THERE IS A PROBLEM", false);
                }
                if (selectedTerm.equals(term)) {
                    if (array == null) {
                        map.put(oid, new StudentAssessment[] {sa});
                    } else {
                        StudentAssessment[] newArray = new StudentAssessment[array.length + 1];
                        System.arraycopy(array, 0, newArray, 0, array.length);
                        newArray[newArray.length - 1] = sa;
                        map.put(oid, newArray);
                    }
                }
            }
        } finally {
            iter.close();
        }

        return map;
    }

    /**
     * Return true if an error has been recorded.
     *
     * @return boolean true if an error has been recorded.
     */
    protected boolean hasError() {
        for (SectionWriter w : m_output) {
            if (w.isError()) {
                return true;
            }
        }

        return false;
    }

    /**
     * Convert an InputStream into a String.
     *
     * @param in the stream to read.
     * @return String a String representing of the argument.
     * @throws IOException Signals that an I/O exception has occurred.
     */
    protected static String read(InputStream in) throws IOException {
        byte[] data = IOUtils.toByteArray(in);

        return new String(data, "UTF8");
    }

    /**
     * Write the results of this import. This should only be called when
     * {@link #m_output} is stable.
     *
     * @param rolledBackTransaction if the transaction was rolled back. This may effect the verbiage
     *        of some of the messages.
     * @throws IOException Signals that an I/O exception has occurred.
     */
    protected void writeResults(boolean rolledBackTransaction) throws IOException {
        StringWriter writer = new StringWriter();
        if (rolledBackTransaction) {
            writer.write("This import was NOT committed to the database.\n\n");
        } else {
            writer.write("This import was committed to the database.\n\n");
        }

        int titleBarLength = 15;
        for (int a = 0; a < m_output.size(); a++) {
            if (a != 0) {
                writer.write("\n\n");
            }
            SectionWriter section = m_output.get(a);
            writer.write(section.getSummary(rolledBackTransaction));
            String title = section.getTitle();
            if (title != null) {
                titleBarLength = Math.max(titleBarLength, title.length());
            }
        }

        String headerBar = StringUtils.padRight("", titleBarLength + 6, '#');

        for (int a = 0; a < m_output.size(); a++) {
            SectionWriter section = m_output.get(a);
            String title = section.getTitle();
            if (title != null) {
                writer.write("\n\n");
                writer.write(headerBar + "\n");
                String titleLine = "## " + StringUtils.padRight(title, titleBarLength, ' ') + " ##";
                writer.write(titleLine + "\n");
                writer.write(headerBar + "\n");
                section.write(getBroker(), writer, rolledBackTransaction);
            }
        }

        String str = writer.toString();
        logToolMessage(Level.INFO, str, false);

        if (((Boolean) getParameter(PARAM_WRITE_TABLE)).booleanValue()) {
            try (OutputStream out = getResultHandler().getOutputStream()) {
                OutputStreamWriter outWriter = new OutputStreamWriter(out);
                try (CSVWriter exporter = new CSVWriter(outWriter)) {
                    // get headings
                    List<String> columns = new ArrayList<>();
                    for (Map map : m_studentRecords) {
                        for (Object key : map.keySet()) {
                            if (!columns.contains(key.toString())) {
                                columns.add(key.toString());
                            }
                        }
                    }

                    // output headings
                    for (String column : columns) {
                        exporter.writeField(column, true);
                    }
                    exporter.newLineCRLF();

                    // output records
                    for (Map map : m_studentRecords) {
                        for (int a = 0; a < columns.size(); a++) {
                            exporter.writeField(String.valueOf(map.get(columns.get(a))), true);
                        }
                        exporter.newLineCRLF();
                    }
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        } else {
            try (OutputStream out = getResultHandler().getOutputStream()) {
                out.write(str.getBytes("UTF-8"));
            }
        }
    }

    /**
     * Create a new AccessToken.
     *
     * @return a new AccessToken.
     * @throws Exception exception
     */
    private AccessToken createAccessToken() throws Exception {
        String username = URLEncoder.encode("follett@epso.tdoe.gov", "UTF-8");
        String password = URLEncoder.encode("D0BBB0C8-5E7D-41A6-9936-7CF1FCAD1E00", "UTF-8");
        URL url = new URL("https://epso-api.azurewebsites.net/token");
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("POST");
        connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
        connection.setDoOutput(true);
        connection.setDoInput(true);
        try {
            String body = "grant_type=password&client_id=epso&username=" + username + "&password=" + password;
            connection.setRequestProperty("Content-Length", Integer.toString(body.length()));
            connection.connect();
            try (OutputStream out = connection.getOutputStream()) {
                out.write(body.getBytes("UTF8"));
            }

            try (InputStream response = connection.getInputStream()) {
                String data = read(response);

                try {
                    ObjectMapper mapper = new ObjectMapper();
                    Map map = mapper.readValue(data, Map.class);
                    return new AccessToken(map);
                } catch (Exception t) {
                    System.err.println(data);
                    throw t;
                }
            } catch (IOException e) {
                InputStream err = connection.getErrorStream();
                String error = read(err);

                try {
                    ObjectMapper mapper = new ObjectMapper();
                    Map map = mapper.readValue(error, Map.class);
                    System.err.println(map);
                } catch (Throwable t) {
                    System.err.println(error);
                }
                throw e;
            }
        } finally {
            connection.disconnect();
        }
    }

    /**
     * This looks up what field "Percentile Score" or "Semester Taken" map to (it
     * will be an A or B UDF) and fetches that property.
     *
     * @param bean the bean to modify
     * @param columnName an AssessmentColumnDefinition name such as "Percentile Score" or
     *        "Semester Taken"
     * @return the value requested by this method.
     * @throws IllegalAccessException exception
     * @throws InvocationTargetException exception
     * @throws NoSuchMethodException exception
     */
    private String getAssessmentProperty(StudentAssessment bean, String columnName)
            throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        initializeAssessmentColumnLUT();
        String keyName = assessmentColumnLUT.get(columnName);
        if (keyName == null) {
            throw new NullPointerException(
                    "No column found for \"" + columnName + "\" among " + assessmentColumnLUT.keySet().toString());
        }

        return (String) PropertyUtils.getSimpleProperty(bean, keyName);
    }

    /**
     * This looks up which UDF the name "Test Description" maps to and retrieves
     * that value.
     *
     * @param bean the bean to modify
     * @param columnName String
     * @return the value requested by this method.
     * @throws IllegalAccessException exception
     * @throws InvocationTargetException exception
     * @throws NoSuchMethodException exception
     */
    private String getField(StudentAssessment bean, String columnName)
            throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        initializeDataDictionaryFieldLUT();
        DataDictionaryField field = m_dataDictionaryFieldLUT.get(columnName);
        if (field == null) {
            throw new NullPointerException(
                    "No field found for \"" + columnName + "\" among " + m_dataDictionaryFieldLUT.keySet().toString());
        }

        return (String) PropertyUtils.getSimpleProperty(bean, field.getJavaName());
    }

    /**
     * Gets a list of students where ID didn't match Aspen matches.
     *
     * @return List
     */
    private List<Map> getNoMatches() {
        List<Map> returnValue = new ArrayList<>();
        for (Map map : this.m_studentRecords) {
            String oid = (String) map.get(KEY_ASPEN_STD_OID);
            if (StringUtils.isEmpty(oid)) {
                returnValue.add(map);
            }
        }
        return returnValue;
    }


    /**
     * Identify a StudentAssessment that matches the criteria given. If more than
     * one bean is identified, then the other beans are deleted and added to
     * {@link #m_removedStudentAssessments}.
     *
     * @param studentOID the student oid
     * @param term the term the assessment relates to
     * @param courseName The exact course name from TDOE, such as "College Algebra" or
     *        "Greenhouse Management"
     * @return the unique StudentAssessment matching the arguments, or null if no
     *         such assessment is found.
     * @throws RuntimeException exception
     * @throws IllegalAccessException exception
     * @throws InvocationTargetException exception
     * @throws NoSuchMethodException exception
     */
    private StudentAssessment getStudentAssessment(String studentOID, FuzzyTerm term, String courseName)
            throws RuntimeException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        String alternateCourseName = courseName;
        if (m_tdoeCourseNameToAbbrevName.containsKey(courseName)) {
            alternateCourseName = m_tdoeCourseNameToAbbrevName.get(courseName);
        }

        Set<StudentAssessment> returnValue = new HashSet<>();
        StudentAssessment[] allAssessments = m_assessmentsByStudentOid.get(studentOID);
        if (allAssessments == null) {
            return null;
        }
        for (StudentAssessment assessment : allAssessments) {
            FuzzyTerm aTerm = FuzzyTerm.get(getAssessmentProperty(assessment, COL_SEMESTER_TAKEN));
            if (term.equals(aTerm)) {
                String aCourseName = getField(assessment, COL_TEST_DESCRIPTION);
                if (aCourseName.startsWith("SDC Exam (")) {
                    aCourseName = aCourseName.substring("SDC Exam (".length(), aCourseName.length() - 1);
                    if (aCourseName.equals(courseName) || aCourseName.equals(alternateCourseName)) {
                        returnValue.add(assessment);
                    }
                }
            }
        }
        if (returnValue.size() == 0) {
            return null;
        }
        StudentAssessment[] array = returnValue.toArray(new StudentAssessment[returnValue.size()]);
        for (int a = 1; a < array.length; a++) {
            m_removedStudentAssessments.add(array[a]);
            getBroker().deleteBean(array[a]);
        }
        return array[0];
    }

    /**
     * Return a list of maps where the state's first/last name don't resemble
     * Aspen's.
     * <p>
     * This was added when we noticed someone (either Aspen or Dynetics) had used
     * the wrong state ID for a student record, and we imported that data as-is.
     * Instead we should flag these for a human's review.
     *
     * @return a list of maps where the state's first/last name don't resemble
     *         Aspen's.
     */
    private List<Map> getSuspiciousNames() {
        List<Map> returnValue = new ArrayList<>();
        for (Map map : this.m_studentRecords) {
            String oid = (String) map.get(KEY_ASPEN_STD_OID);
            if (!StringUtils.isEmpty(oid)) {
                String firstName1 = (String) map.get("student_first_name");
                String lastName1 = (String) map.get("student_last_name");
                // students who didn't pass may have empty name fields in Dynetic's data.
                if (!StringUtils.isEmpty(firstName1) && !StringUtils.isEmpty(lastName1)) {
                    String firstName2 = (String) map.get(KEY_ASPEN_STD_PSN_FIRST_NAME);
                    String lastName2 = (String) map.get(KEY_ASPEN_STD_PSN_LAST_NAME);
                    if (!(firstName1.equalsIgnoreCase(firstName2) && lastName1.equalsIgnoreCase(lastName2))) {
                        returnValue.add(map);
                    }
                }
            }
        }
        return returnValue;
    }

    /**
     * Initialize the assessmentColumnLUT field.
     */
    private void initializeAssessmentColumnLUT() {
        if (assessmentColumnLUT == null) {
            assessmentColumnLUT = new HashMap<>();
            Collection<AssessmentColumnDefinition> columnDefs = m_assessmentDefinition.getAssessmentColumnDefinitions();
            for (AssessmentColumnDefinition def : columnDefs) {
                assessmentColumnLUT.put(def.getUserLongName(), def.getAssessmentBeanAttribute());
            }
        }
    }

    /**
     * Initialize the m_dataDictionaryFieldLUT field.
     */
    private void initializeDataDictionaryFieldLUT() {
        if (m_dataDictionaryFieldLUT == null) {
            m_dataDictionaryFieldLUT = new HashMap<>();

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            Iterator<DataDictionaryField> iter = dictionary.getFieldIterator();
            while (iter.hasNext()) {
                DataDictionaryField field = iter.next();
                if (field.getDataTableOid().trim().equalsIgnoreCase(StudentAssessment.DICTIONARY_ID)) {
                    m_dataDictionaryFieldLUT.put(field.getUserLongName(), field);
                }
            }
        }
    }

    /**
     * This looks up what field "Percentile Score" or "Semester Taken" map to (it
     * will be an A or B UDF) and makes that assignment.
     *
     * @param bean the bean to modify
     * @param columnName an AssessmentColumnDefinition name such as "Percentile Score" or
     *        "Semester Taken"
     * @param value the value to assign.
     * @throws IllegalAccessException exception
     * @throws InvocationTargetException exception
     * @throws NoSuchMethodException exception
     */
    private void setAssessmentProperty(StudentAssessment bean, String columnName, String value)
            throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        initializeAssessmentColumnLUT();
        String keyName = assessmentColumnLUT.get(columnName);
        if (keyName == null) {
            throw new NullPointerException(
                    "No column found for \"" + columnName + "\" among " + assessmentColumnLUT.keySet().toString());
        }
        PropertyUtils.setSimpleProperty(bean, keyName, value);
    }

    /**
     * This looks up which UDF the name "Test Description" maps to and makes that
     * assignment.
     *
     * @param bean the bean to modify
     * @param fieldLongName a DataDictionaryField's long user name, such as "Test Description"
     * @param value the value to assign.
     * @throws IllegalAccessException exception
     * @throws InvocationTargetException exception
     * @throws NoSuchMethodException exception
     */
    private void setField(StudentAssessment bean, String fieldLongName, String value)
            throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        initializeDataDictionaryFieldLUT();
        DataDictionaryField field = m_dataDictionaryFieldLUT.get(fieldLongName);
        if (field == null) {
            throw new NullPointerException("No field found for \"" + fieldLongName + "\" among "
                    + m_dataDictionaryFieldLUT.keySet().toString());
        }
        PropertyUtils.setSimpleProperty(bean, field.getJavaName(), value);
    }
}
