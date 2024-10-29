/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GpaClassSizeLookup;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Florida state export procedure for Student ELL.
 *
 * @author X2 Development Corporation
 */
public class GSFCTranscript extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the FL ELL export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class GSFCTranscriptEntity extends StateReportEntity {
        /**
         * The highest priority contact for the student.
         */
        private Contact m_contact = null;

        /**
         * Lists of row definitions and root beans for record generation.
         */
        private List<String> m_definitionId = new ArrayList<String>();
        private List<X2BaseBean> m_elements = new ArrayList<X2BaseBean>();
        private Map<String, List<HealthImmunizationDose>> m_immunizationSeriesMap;
        private Map<ScheduleTerm, List<Transcript>> m_transcriptTermMap;
        private Map<ScheduleTerm, String> m_termGradeMap;
        private Map<ScheduleTerm, SisSchool> m_termSchoolMap;
        private StudentEnrollment m_studentEnrollment;

        /**
         * Entity variables
         */
        protected String m_transcriptContextOid = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public GSFCTranscriptEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize the entity.
         * Find all reportable data, count required rows and row types.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            GSFCTranscript gsfcData = (GSFCTranscript) data;

            // build lists of beans and definition Ids in order for output in report file.
            // Should start with an 01, and 02 and then multiple 03/04/05 as needed.
            // First record (school record), the 01 (indicated by null).
            m_definitionId.add(null);
            m_elements.add(bean);

            // Second record, the 02 (indicated by "02").
            m_definitionId.add("02");
            m_elements.add(bean);

            lookupEnrollmentStatus();

            // Lookup all transcripts, group them by term.
            // Each term group is an 03 record, the transcripts in them are 04 records.
            m_transcriptTermMap = new HashMap<ScheduleTerm, List<Transcript>>();
            m_termGradeMap = new HashMap<ScheduleTerm, String>();
            m_termSchoolMap = new HashMap<ScheduleTerm, SisSchool>();
            List<Transcript> allTrans = gsfcData.m_transcriptMap.get(bean.getOid());
            if (allTrans != null) {
                for (Transcript transcript : allTrans) {
                    // Get the term for the transcript record.
                    ScheduleTerm term = null;
                    if (transcript.getMasterSchedule() != null
                            && transcript.getMasterSchedule().getScheduleTerm() != null) {
                        term = transcript.getMasterSchedule().getScheduleTerm();
                    } else {
                        String schoolOid = transcript.getSchoolOid();
                        String contextOid = transcript.getDistrictContextOid();
                        String termView = transcript.getTermCode();
                        String key = schoolOid + "-" + contextOid + "-" + termView;
                        term = gsfcData.m_termsMap.get(key);

                        // If the transcript Master Schedule Term is not available then searching on
                        // schoolOid, contextOid and termView.
                        if (term == null) {
                            term = getAlternateScheduleTerm(data.getBroker(), schoolOid, contextOid, transcript);
                            // D-19030, since we are overloading the gradeTermMap here, it is fine
                            // to use the full map
                            // BaseTermMap and GradeTermMap fields are used temporary for the values
                            // for Start Date and End Date respectively.
                            key = schoolOid + "-" + contextOid + "-" + termView + term.getBaseTermMap()
                                    + term.getGradeTermMap();
                            gsfcData.m_termsMap.put(key, term);
                        }
                    }

                    if (term != null) {
                        if (transcript.getSchool() != null) {
                            m_termSchoolMap.put(term, transcript.getSchool());
                        }

                        List<Transcript> transcripts = m_transcriptTermMap.get(term);
                        if (transcripts == null) {
                            transcripts = new ArrayList<Transcript>();
                            m_transcriptTermMap.put(term, transcripts);
                            m_termGradeMap.put(term, transcript.getGradeLevel());
                            m_definitionId.add("03");
                            m_elements.add(term);
                        }
                        transcripts.add(transcript);
                        m_definitionId.add("04");
                        m_elements.add(transcript);

                        m_transcriptContextOid = transcript.getDistrictContextOid();
                    }
                }
            }

            // Lookup all assessments for the student.
            List<StudentAssessment> allAssessments = gsfcData.m_assessmentMap.get(bean.getOid());
            if (allAssessments != null) {
                for (StudentAssessment assessment : allAssessments) {
                    m_definitionId.add("05");
                    m_elements.add(assessment);
                }
            }

            // Look up health immunization data for student.
            // There is a full list of doses. Remap into series.
            m_immunizationSeriesMap = new HashMap<String, List<HealthImmunizationDose>>();
            List<HealthImmunizationDose> allDoses =
                    gsfcData.m_immunizationMap.get(bean.getOid());
            List<HealthImmunizationDose> seriesDoses = null;
            String seriesOid = null;
            if (allDoses != null) {
                for (HealthImmunizationDose dose : allDoses) {
                    if (dose.getDate() != null && dose.getImmunizationSeriesOid() != null) {
                        if (!dose.getImmunizationSeriesOid().equals(seriesOid)) {
                            seriesDoses = new ArrayList<HealthImmunizationDose>();
                            seriesOid = dose.getImmunizationSeriesOid();
                            m_immunizationSeriesMap.put(dose.getImmunizationSeriesOid(), seriesDoses);
                            HealthImmunizationSeries his = dose.getImmunizationSeries();
                            if (his != null) {
                                m_elements.add(his);
                                m_definitionId.add("06");
                            }
                        }
                        seriesDoses.add(dose);
                    }
                }
            }
            this.setRowCount(m_definitionId.size());
        }

        /**
         * Find an alternative Schedule Term using a Transcript's schoolOid, contextOid and
         * termView.
         *
         * @param broker X2Broker
         * @param schoolOid String
         * @param contextOid String
         * @param transcript Transcript
         * @return ScheduleTerm
         */
        public ScheduleTerm getAlternateScheduleTerm(X2Broker broker,
                                                     String schoolOid,
                                                     String contextOid,
                                                     Transcript transcript) {
            ScheduleTerm scheduleTerm = null;
            String termView = transcript.getTermCode();

            // Check if there is an Schedule Term by schoolOid and contextOid
            X2Criteria scheduleCriteria = new X2Criteria();
            scheduleCriteria.addEqualTo(Schedule.COL_SCHOOL_OID, schoolOid);
            scheduleCriteria.addEqualTo(Schedule.COL_DISTRICT_CONTEXT_OID, contextOid);
            SubQuery scheduleQuery = new SubQuery(Schedule.class, X2BaseBean.COL_OID, scheduleCriteria);

            X2Criteria scheduleTermCriteria = new X2Criteria();
            scheduleTermCriteria.addIn(ScheduleTerm.COL_SCHEDULE_OID, scheduleQuery);
            scheduleTermCriteria.addEqualTo(ScheduleTerm.COL_CODE, termView);
            QueryByCriteria scheduleTermQuery = new QueryByCriteria(ScheduleTerm.class, scheduleTermCriteria);
            scheduleTermQuery.addOrderByDescending(X2BaseBean.COL_OID);

            Collection<ScheduleTerm> scheduleTerms = broker.getCollectionByQuery(scheduleTermQuery);

            // There is a possibility that there is more than one set of schedule terms if they
            // rebuilt their schedule.
            // If so then take the newest one.
            for (ScheduleTerm lastScheduleTerm : scheduleTerms) {
                scheduleTerm = lastScheduleTerm;
                break;
            }

            // If still there is no alternate schedule available, then pull the the term code and
            // dates form the transcript.
            // Most likely these are transcripts form other schools that came over form data
            // convert.
            if (scheduleTerm == null) {
                String alternateStartDate = (String) transcript.getFieldValueByAlias(ALIAS_DOE_ALT_START_DATE);
                String alternateEndDate = (String) transcript.getFieldValueByAlias(ALIAS_DOE_ALT_END_DATE);

                scheduleTerm = new ScheduleTerm(broker.getPersistenceKey());
                scheduleTerm.setCode(termView);
                scheduleTerm.setBaseTermMap(alternateStartDate);
                scheduleTerm.setGradeTermMap(alternateEndDate);
            }

            return scheduleTerm;
        }

        /**
         * Override getBean() to return alternate types of beans for different rows.
         * Transcript type rows should return a student transcript record.
         * Immunization rows should return an immunization series record.
         * The default is to return the super
         *
         * @return X 2 base bean
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getBean()
         */
        @Override
        public X2BaseBean getBean() {
            X2BaseBean bean = super.getBean();
            int rowNumber = getCurrentRow() < m_elements.size() ? getCurrentRow()
                    : getCurrentRow() == m_elements.size() ? getCurrentRow() - 1 : 0;
            if (getCurrentRow() >= 0 && m_elements.get(rowNumber) != null) {
                bean = m_elements.get(rowNumber);
            }
            return bean;
        }

        /**
         * Retrieve the students contact with the lowest priority number.
         * Keep a local reference for multiple lookups.
         *
         * @return Contact
         */
        public Contact getContact() {
            if (m_contact == null) {
                StudentContact ctj = null;
                Collection<StudentContact> contacts = ((SisStudent) super.getBean()).getContacts();
                for (StudentContact contact : contacts) {
                    if (ctj == null || contact.getEmergencyPriority() < ctj.getEmergencyPriority()) {
                        ctj = contact;
                    }
                }
                if (ctj != null) {
                    m_contact = ctj.getContact();
                }
            }
            return m_contact;
        }

        /**
         * Specify the definition Id for the current row based on getCurrentRow().
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#
         *      getCurrentFormatDefinitionId()
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            String definitionId = null;
            if (getCurrentRow() >= 0 && getCurrentRow() < m_definitionId.size()) {
                definitionId = m_definitionId.get(getCurrentRow());
            }
            return definitionId;
        }

        /**
         * Returns a list of immunization doses for a series Oid.
         *
         * @param seriesOid String
         * @return List
         */
        public List<HealthImmunizationDose> getDoses(String seriesOid) {
            List<HealthImmunizationDose> doses = m_immunizationSeriesMap.get(seriesOid);
            return doses;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = null;
            SisStudent student = null;
            X2BaseBean baseBean = getBean();

            if (baseBean instanceof SisStudent) {
                student = (SisStudent) baseBean;
            } else if (getStudentEnrollment() != null) {
                student = getStudentEnrollment().getStudent();
            }

            if (student != null) {
                name = student.getNameView() +
                        " [LASID: " + student.getLocalId() +
                        ", SASID: " + student.getStateId() + "]";
            }

            return name;
        }

        /**
         * Returns the most recent student enrollment within this year for the student.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getStudentEnrollment() {
            return m_studentEnrollment;
        }


        /**
         * Returns the grade level associated with a schedule term.
         *
         * @param term ScheduleTerm
         * @return String
         */
        public String getTermGrade(ScheduleTerm term) {
            String grade = null;
            if (m_termGradeMap != null && term != null) {
                grade = m_termGradeMap.get(term);
            }
            return grade;
        }

        /**
         * Return the school name for a term.
         * Allow preservation of school name when the term has no schedule.
         *
         * @param term ScheduleTerm
         * @return String
         */
        public SisSchool getTermSchool(ScheduleTerm term) {
            SisSchool school = null;
            if (m_termSchoolMap != null && term != null) {
                school = m_termSchoolMap.get(term);
            }
            return school;
        }

        /**
         * Get Context Oid.
         *
         * @return String
         */
        public String getContextOid() {
            return m_transcriptContextOid;
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Look at the students enrollment records to see if there is an entry or withdrawal this
         * year.
         */
        private void lookupEnrollmentStatus() {
            GSFCTranscript data = (GSFCTranscript) getData();
            List<StudentEnrollment> enrollments = data.m_studentEnrollmentMap.get(getBean().getOid());
            if (enrollments != null) {
                m_studentEnrollment = enrollments.iterator().next();
            }
        }
    }

    /*
     * General Constants.
     */
    private static final String STRING_EMPTY = "";
    private static final String STRING_CARRIAGE_RETURN = "\r";
    private static final String STRING_NEW_LINE = "\n";
    private static final String STRING_ONE = "1";

    /*
     * Heading text constants.
     */
    private static final String HR = "HR";
    private static final String EOL = System.getProperty("line.separator");

    /*
     * Required value delimiter is a control character 0x1E (Decimal 30).
     */
    private static final Character VALUE_DELIMITER = Character.valueOf((char) 30 /* 0x1E */ );

    /*
     * Alias names.
     */
    private static final String ALIAS_GSFC_TRANSCRIPT = "GSFC Transcript";
    private static final String ALIAS_GSFC_ASSESSMENT = "GSFC Assessment";
    private static final String ALIAS_DOE_ALT_START_DATE = "DOE ALT START DATE";
    private static final String ALIAS_DOE_ALT_END_DATE = "DOE ALT END DATE";
    private static final String ALIAS_TRN_SUBJECT = "all-trn-Subject";

    // Temporary, provide an override to flag a student as graduated.
    private static final String ALIAS_GRAD_STATUS_OVERRIDE = "GSFC Grad Override";
    private static final String PARAM_GRAD_OVERRIDE_DATE = "gradOverrideDate";

    /*
     * Tool input parameters.
     */
    private static final String PARAM_ACTIVE_SELECTION = "activeSelection";
    private static final String PARAM_GSFC_FLAG = "useGsfcFlag";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_TRANS_DEF_1 = "tranDef1";
    private static final String PARAM_GPA_DEF_1 = "gpaDef1";

    /*
     * Export Retriever Field Names
     */
    private static final String GSFC_CLASS = "GSFC-CLASS";
    private static final String GSFC_CONTACT = "GSFC-CONTACT";
    private static final String GSFC_COURSE_NUMBER = "GSFC-COURSE-NUMBER";
    private static final String GSFC_IMMUN_DATE = "GSFC-IMMUN-DATE";
    private static final String GSFC_STATUS = "GSFC-STATUS";
    private static final String GSFC_TERM = "GSFC-TERM";
    private static final String GSFC_TRANS_CREDIT = "GSFC-TRANS-CREDIT";
    private static final String GSFC_TRANS_WEIGHT = "GSFC-TRANS-WEIGHT";

    /*
     * Retriever parameter keys.
     */
    private static final String VALUE_TERM_SESSION = "SESSION";
    private static final String VALUE_TERM_GRADE = "GRADE";
    private static final String VALUE_TERM_SCHOOL = "SCHOOL";
    private static final String VALUE_TERM_START = "START";
    private static final String VALUE_TERM_END = "END";

    /*
     * Local variables: translated alias names, formatter.
     */
    protected Map<String, List<StudentAssessment>> m_assessmentMap;
    protected List<DistrictSchoolYearContext> m_contextList;
    protected GpaClassSizeLookup m_classSizeLookup;
    protected GradePointAverageDefinition m_gpaDefinition;
    protected Set<String> m_gradCodes;
    protected String m_gsfcAssessmnetAlias = null;
    protected String m_gsfcTranscriptAlias = null;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyyMMdd");
    protected Map<String, List<HealthImmunizationDose>> m_immunizationMap;
    protected Map<String, List<StudentEnrollment>> m_studentEnrollmentMap;
    protected Map<String, List<Transcript>> m_transcriptMap;
    protected Map<String, ScheduleTerm> m_termsMap;
    protected HashMap<String, Map> m_termOverrideMap;

    // temp, grad status override indicator for student.
    protected PlainDate m_gradOverrideDate;
    protected String m_fieldGradOverride;
    protected String m_fieldTrnSubject;

    /**
     * Retrieve the class size for the student/school from their GPA calculation.
     *
     * There may not be a recent GPA calculation for the current year. Look backwards through
     * context years until one is found.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveClassSize implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Integer value = null;
            if (m_gpaDefinition != null) {
                SisStudent student = (SisStudent) entity.getBean();
                for (DistrictSchoolYearContext context : m_contextList) {
                    value = m_classSizeLookup.getClassSize(student.getSchoolOid(),
                            m_gpaDefinition.getGpaDefinitionName(),
                            student.getYog(),
                            context.getOid());
                    if (value != null) {
                        break;
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the highest priority contact for the student.
     * From that contact, retrieve the bean value specified in the parameter.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveContact implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            Contact contact = ((GSFCTranscriptEntity) entity).getContact();
            if (contact != null && field.getParameter() != null) {
                value = (String) getProperty(contact, (String) field.getParameter());
            }
            return value;
        }
    }

    /**
     * Retrieve the standard course number, if there is one. Replace with the override field
     * "GSFC Course Number"
     * if there is one. Return the resulting course number
     * When trnUserDescInd = True, pull from relTrnCskEquv.cskCourseNum.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourseNumber implements FieldRetriever {
        public static final String ALIAS_GSFC_COURSE_NUMBER = "GSFC Course Number";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Transcript transcript = (Transcript) entity.getBean();
            String value = null;
            if (transcript.getUserDescriptionIndicator()) {
                if (transcript.getEquivalentSchoolCourse() != null) {
                    value = transcript.getEquivalentSchoolCourse().getNumber();
                }
            } else {
                SchoolCourse schoolCourse = transcript.getSchoolCourse();
                if (schoolCourse != null) {
                    // get course number
                    value = schoolCourse.getNumber();

                    Course course = schoolCourse.getCourse();
                    if (course != null) {
                        // get "GSFC Course Number" from course table.
                        String gsfcCourseNumber = (String) course.getFieldValueByAlias(ALIAS_GSFC_COURSE_NUMBER);

                        // replace course number with "GSFC Course Number" if there is one.
                        if (!StringUtils.isEmpty(gsfcCourseNumber)) {
                            value = gsfcCourseNumber;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the standard course department code.
     *
     * When trnUserDescInd = True, pull from trn.[all-trn-Subject].
     * When trnUserDescInd = False, pull from trn.schoolCourse.departmentCode.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourseDepartmentCode implements FieldRetriever {
        private static final String CALC_ID = "GSFC-DEP-CODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Transcript transcript = (Transcript) entity.getBean();
            Object value = null;
            if (transcript.getUserDescriptionIndicator()) {
                value = transcript.getFieldValueByBeanPath(m_fieldTrnSubject);
            } else {
                if (transcript.getSchoolCourse() != null) {
                    value = transcript.getSchoolCourse().getDepartmentCode();
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the standard course title.
     *
     * When trnUserDescInd = True, pull from trn.userDescription.
     * When trnUserDescInd = False, pull from trn.courseDescriptiom.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourseTitle implements FieldRetriever {
        private static final String CALC_ID = "GSFC-CRS-TITLE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Transcript transcript = (Transcript) entity.getBean();
            String value = null;
            if (transcript.getUserDescriptionIndicator()) {
                value = transcript.getCourseDescription();
            } else {
                value = transcript.getCourseDescription();
            }
            return value;
        }
    }

    /**
     * Return an immunization series date from an immunization series based on an index 1-8.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveImmunDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            PlainDate immunDate = null;
            int index = Integer.parseInt((String) field.getParameter()) - 1;
            Object bean = entity.getBean();
            HealthImmunizationSeries series = null;
            if (bean != null && bean instanceof HealthImmunizationSeries) {
                series = (HealthImmunizationSeries) bean;
                List<HealthImmunizationDose> doses = ((GSFCTranscriptEntity) entity).getDoses(series.getOid());
                if (doses != null &&
                        doses.size() > index) {
                    immunDate = doses.get(index).getDate();
                }
            }
            return immunDate;
        }
    }

    /**
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveImmunName implements FieldRetriever {

        private static final String CALC_ID = "GSFC-IMM-NAME";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Object bean = entity.getBean();
            HealthImmunizationSeries series = null;
            if (bean != null && bean instanceof HealthImmunizationSeries) {
                series = (HealthImmunizationSeries) bean;
                return series.getImmunizationDefinition() != null ? series.getImmunizationDefinition().getSeriesName()
                        : null;
            }
            return null;
        }
    }

    /**
     * Retrieve student enrollment status and date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStatus implements FieldRetriever {
        private static final String PARAM_GRAD = "GRAD"; // Grad date
        private static final String PARAM_STATUS = "STATUS"; // grad status
        private static final String PARAM_WITHDRAW = "WITHDRAW";

        private static final String STATUS_GRADUATED = "G";
        private static final String STATUS_ENROLLED = "E";
        private static final String STATUS_WT = "WT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            GSFCTranscript gsfcData = (GSFCTranscript) data;
            GSFCTranscriptEntity gsfcEntity = (GSFCTranscriptEntity) entity;
            StudentEnrollment enrollment = gsfcEntity.getStudentEnrollment();

            String status = null;
            PlainDate gradDate = null;
            PlainDate exitDate = null;

            // Check override field for graduated status and set graduated date.
            String gradOverride = (String) data.getProperty(entity.getBean(), m_fieldGradOverride);
            if (STRING_ONE.equals(gradOverride)) {
                status = STATUS_GRADUATED; // Graduated;
                gradDate = m_gradOverrideDate;
            } else if (enrollment != null) {
                if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                    if (gsfcData.m_gradCodes.contains(enrollment.getEnrollmentCode())) {
                        status = STATUS_GRADUATED; // Graduated;
                        gradDate = enrollment.getEnrollmentDate();
                    } else {
                        // TODO: Decode StudentEnrollment withdrawal enrollment codes into GSFC
                        // status.
                        status = STATUS_WT;
                        exitDate = enrollment.getEnrollmentDate();
                    }
                } else {
                    status = STATUS_ENROLLED;
                }
            }

            if (PARAM_STATUS.equals(param)) {
                value = status;
            } else if (PARAM_GRAD.equals(param)) {
                value = gradDate;
            } else if (PARAM_WITHDRAW.equals(param)) {
                value = exitDate;
            }

            return value;
        }
    }

    /**
     * Retrieve term related information.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermInfo implements FieldRetriever {
        protected GSFCTranscript m_gsfcData = null;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            m_gsfcData = (GSFCTranscript) data;
            ScheduleTerm term = (ScheduleTerm) entity.getBean();
            GSFCTranscriptEntity gsfcEntity = (GSFCTranscriptEntity) entity;

            SisSchool school = gsfcEntity.getTermSchool(term);
            String contextOid = gsfcEntity.getContextOid();

            if (term.getScheduleTermDates() == null || term.getScheduleTermDates().isEmpty()) {
                ScheduleTerm possibleOverrideTerm = getOverrideTerm(school, contextOid, term);
                if (possibleOverrideTerm != null) {
                    term = possibleOverrideTerm;
                }
            }
            if (VALUE_TERM_GRADE.equals(param)) {
                value = gsfcEntity.getTermGrade(term);
            } else if (VALUE_TERM_SCHOOL.equals(param)) {
                value = school.getName();
            } else if (VALUE_TERM_START.equals(param)) {
                PlainDate startDate = null;
                for (ScheduleTermDate termDate : term.getScheduleTermDates()) {
                    if (startDate == null || startDate.after(termDate.getStartDate())) {
                        startDate = termDate.getStartDate();
                    }
                }

                // If there is still no Start Date available on the Schedule Term, take the the
                // fields BaseTermMap.
                // This temporary field stores the Start Date pulled from the Student's Transcript.
                if (startDate == null) {
                    String alternateStartDate = term.getBaseTermMap();
                    if (alternateStartDate != null) {
                        startDate = DateUtils.getDate(alternateStartDate);
                    }
                }
                value = startDate;
            } else if (VALUE_TERM_END.equals(param) || VALUE_TERM_SESSION.equals(param)) {
                PlainDate endDate = null;
                for (ScheduleTermDate termDate : term.getScheduleTermDates()) {
                    if (endDate == null || endDate.before(termDate.getEndDate())) {
                        endDate = termDate.getEndDate();
                    }
                }

                // If there is still no End Date available on the Schedule Term, take the the fields
                // GradeTermMap.
                // This temporary field stores the End Date pulled from the Student's Transcript.
                if (endDate == null) {
                    // D-19030, since we are overloading the gradeTermMap here, it is fine to use
                    // the full map
                    String alternateEndDate = term.getGradeTermMap();
                    if (alternateEndDate != null) {
                        endDate = DateUtils.getDate(alternateEndDate);
                    }
                }
                value = endDate;
            }
            /*
             * else if (VALUE_TERM_SESSION.equals(param))
             * {
             *
             * @SuppressWarnings("synthetic-access")
             * List <Transcript> transcriptList = gsfcEntity.m_transcriptTermMap.get(term);
             * Transcript transcript = transcriptList.get(0);
             * int year = transcript.getDistrictContext().getSchoolYear();
             *
             * value = term.getSchedule().getEndDate().toString();
             *
             * return value;
             * }
             */

            return value;
        }

        /**
         * Gets the override term.
         *
         * @param school SisSchool
         * @param schoolContext String
         * @param term ScheduleTerm
         * @return Schedule term
         */
        private ScheduleTerm getOverrideTerm(SisSchool school, String schoolContext, ScheduleTerm term) {
            String schoolOid = school.getOid();
            Map<String, ScheduleTerm> overrideTerms = new HashMap<String, ScheduleTerm>();
            ScheduleTerm overrideTerm;
            if (m_gsfcData.m_termOverrideMap.containsKey(schoolOid)) {
                overrideTerms = m_gsfcData.m_termOverrideMap.get(schoolOid);
                if (overrideTerms.containsKey(schoolContext)) {
                    overrideTerm = overrideTerms.get(schoolContext);
                } else {
                    overrideTerm = queryTerm(schoolOid, schoolContext, term);
                    overrideTerms.put(schoolContext, overrideTerm);
                }

            } else {
                overrideTerm = queryTerm(schoolOid, schoolContext, term);
                overrideTerms.put(schoolContext, overrideTerm);
                m_gsfcData.m_termOverrideMap.put(schoolOid, overrideTerms);
            }
            return overrideTerm;
        }

        /**
         * Query term.
         *
         * @param schoolOid String
         * @param schoolContext String
         * @param term ScheduleTerm
         * @return ScheduleTerm
         */
        private ScheduleTerm queryTerm(String schoolOid, String schoolContext, ScheduleTerm term) {
            X2Criteria schoolContextCriteria = new X2Criteria();
            schoolContextCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, schoolContext);
            schoolContextCriteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID, schoolOid);
            SubQuery schoolContextQuery = new SubQuery(SchoolScheduleContext.class,
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, schoolContextCriteria);

            X2Criteria termCriteria = new X2Criteria();
            termCriteria.addIn(ScheduleTerm.COL_SCHEDULE_OID, schoolContextQuery);
            termCriteria.addEqualTo(ScheduleTerm.COL_CODE, term.getCode());
            QueryByCriteria termQuery = new QueryByCriteria(ScheduleTerm.class, termCriteria);
            ScheduleTerm queriedTerm = (ScheduleTerm) getBroker().getBeanByQuery(termQuery);

            return queriedTerm;
        }
    }

    /**
     * Return the credit available for a transcript course.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscriptCreditAvail implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Number credit = BigDecimal.ZERO;
            Transcript trans = (Transcript) entity.getBean();
            if (trans != null) {
                if (!StringUtils.isEmpty(trans.getPotentialCredit())) {
                    credit = Float.valueOf(trans.getPotentialCredit());
                } else if (trans.getSchoolCourse() != null && trans.getSchoolCourse().getCredit() != null) {
                    credit = trans.getSchoolCourse().getCredit();
                }
            }
            return credit;
        }
    }

    /**
     * Return the weight for a transcript course.
     * The weight is converted into a set of codes based on range.
     *
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscriptWeight implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            // TODO: Do not know the scale to use. Return empty for unweighted.
            return "";
        }
    }

    /**
     * Return a custom heading line.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        // Header row
        // Stripped EOL chars at end of heading to have print on one line
        String version =
                super.getHeading().replace(STRING_CARRIAGE_RETURN, STRING_EMPTY).replace(STRING_NEW_LINE, STRING_EMPTY);
        StringBuilder header = new StringBuilder(70);
        header.append(HR)
                .append(VALUE_DELIMITER)
                .append(version)
                .append(VALUE_DELIMITER)
                .append(m_dateFormat.format(new Date()))
                .append(EOL);

        return header.toString();
    }

    /**
     * Override the delimiter character.
     *
     * @return Character
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getValueDelimiter()
     */
    @Override
    public Character getValueDelimiter() {
        return VALUE_DELIMITER;
    }

    /**
     * Initialize date export object.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        if (getSetupErrors().size() == 0) {
            // build the query for students to report.
            Criteria studentCriteria = getStudentCriteria();

            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            Integer sort = (Integer) getParameter(PARAM_SORT);
            switch (sort != null ? sort.intValue() : 0) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            setQuery(studentQuery);
            setEntityClass(GSFCTranscriptEntity.class);

            // Load maps of data for the selected students.
            initializeMaps(studentCriteria);

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(GSFC_CLASS, new RetrieveClassSize());
            calcs.put(GSFC_CONTACT, new RetrieveContact());
            calcs.put(GSFC_COURSE_NUMBER, new RetrieveCourseNumber());
            calcs.put(GSFC_IMMUN_DATE, new RetrieveImmunDate());
            calcs.put(GSFC_STATUS, new RetrieveStatus());
            calcs.put(GSFC_TERM, new RetrieveTermInfo());
            calcs.put(GSFC_TRANS_CREDIT, new RetrieveTranscriptCreditAvail());
            calcs.put(GSFC_TRANS_WEIGHT, new RetrieveTranscriptWeight());
            calcs.put(RetrieveCourseTitle.CALC_ID, new RetrieveCourseTitle());
            calcs.put(RetrieveCourseDepartmentCode.CALC_ID, new RetrieveCourseDepartmentCode());
            calcs.put(RetrieveImmunName.CALC_ID, new RetrieveImmunName());
            super.addCalcs(calcs);

            /*
             * HashMap validators = new HashMap<String, FieldRetriever>();
             * validators.put("FTE-RESIDENT", new ValidateResidentStatus());
             * super.addValidators(validators);
             */
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     * @param matchPath String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName, String matchPath) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(matchPath, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria studentCriteria = new X2Criteria();

        boolean useGsfcFlag = ((Boolean) getParameter(PARAM_GSFC_FLAG)).booleanValue();
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();

        /*
         * Values for activeSelection:
         * 0 = All
         * 1 = Active only
         * 2 = Grad only
         * 3 = Active and Grad only
         */
        int activeSelection = ((Integer) getParameter(PARAM_ACTIVE_SELECTION)).intValue();

        // Add graduated students.
        Criteria gradCriteria = new Criteria();
        Criteria enrCriteria = new Criteria();
        enrCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        if (activeSelection == 2 || activeSelection == 3) {
            enrCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_CODE, m_gradCodes);
        }
        // enrCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
        // getOrganization().getCurrentContext().getStartDate());
        if (isSchoolContext()) {
            enrCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            enrCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            enrCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }
        switch (queryBy) {
            case 1: // YOG
                enrCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                enrCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                enrCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(enrCriteria, queryString, StudentEnrollment.COL_STUDENT_OID);
                break;

            default:
                // Take all students in the district
                break;
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrCriteria);
        gradCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        Criteria activeCriteria = new Criteria();
        activeCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
        if (isSchoolContext()) {
            activeCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activeCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            activeCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        /*
         * Check student selection criteria user input.
         */
        switch (queryBy) {
            case 1: // YOG
                activeCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                activeCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                activeCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(activeCriteria, queryString, X2BaseBean.COL_OID);
                break;

            default:
                // Take all students in the district
                break;
        }

        if (activeSelection != 1) {
            studentCriteria.addOrCriteria(gradCriteria);
        }
        if (activeSelection != 2) {
            studentCriteria.addOrCriteria(activeCriteria);
        }

        if (useGsfcFlag) {
            studentCriteria.addEqualTo(m_gsfcTranscriptAlias, BooleanAsStringConverter.TRUE);
        }

        // Check school selection user input parameter.

        return studentCriteria;
    }

    /**
     * Initialize aliases and other report data.
     */
    private void initializeFields() {
        m_gradOverrideDate = (PlainDate) getParameter(PARAM_GRAD_OVERRIDE_DATE);

        m_gsfcTranscriptAlias = translateAliasToJavaName(ALIAS_GSFC_TRANSCRIPT, true);
        m_gsfcAssessmnetAlias = translateAliasToJavaName(ALIAS_GSFC_ASSESSMENT, true);
        m_fieldGradOverride = translateAliasToJavaName(ALIAS_GRAD_STATUS_OVERRIDE, true);
        m_fieldTrnSubject = translateAliasToJavaName(ALIAS_TRN_SUBJECT, true);

        m_classSizeLookup = new GpaClassSizeLookup(getOrganization(), getBroker());

        // Load Withdrawal codes with state code "G". These are codes for graduation,
        // and determine if the student is graduates or withdrawn other.
        m_gradCodes = new HashSet<String>();
        String withdrawalCodesRefTbl = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        Criteria withdrawalCodesCriteria = new Criteria();
        withdrawalCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, withdrawalCodesRefTbl);
        withdrawalCodesCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, "G");
        QueryByCriteria withdrawalCodesQuery = new QueryByCriteria(ReferenceCode.class, withdrawalCodesCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(withdrawalCodesQuery);
        try {
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                m_gradCodes.add(code.getCode());
            }
        } finally {
            iterator.close();
        }

    }

    /**
     * Build maps of student data necessary for the report.
     * Transcripts, Immunization doses, Schedule terms.
     *
     * @param studentCriteria Criteria
     */
    private void initializeMaps(Criteria studentCriteria) {
        // A subquery of student Oids for various lookups.
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        String transDefOid = (String) getParameter(PARAM_TRANS_DEF_1);
        String gpaDefOid = (String) getParameter(PARAM_GPA_DEF_1);

        Criteria transcriptCriteria = new Criteria();
        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, subQuery);
        transcriptCriteria.addNotNull(Transcript.COL_FINAL_GRADE);
        if (!StringUtils.isEmpty(transDefOid)) {
            transcriptCriteria.addEqualTo(Transcript.COL_TRANSCRIPT_DEFINITION_OID, transDefOid);
        }
        QueryByCriteria transcriptQuery = new QueryByCriteria(Transcript.class, transcriptCriteria);
        transcriptQuery.addOrderBy(Transcript.COL_STUDENT_OID, true);
        transcriptQuery.addOrderBy(
                Transcript.REL_DISTRICT_CONTEXT + PATH_DELIMITER + DistrictSchoolYearContext.COL_CONTEXT_ID, true);
        transcriptQuery.addOrderBy(Transcript.COL_TERM_CODE, true);
        transcriptQuery.addOrderBy(Transcript.COL_SCHOOL_OID, true);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(transcriptQuery, Transcript.COL_STUDENT_OID, 1000);

        // Load Student Enrollments for this year.
        List<String> enrTypes = new ArrayList<String>();
        enrTypes.add(StudentEnrollment.ENTRY);
        enrTypes.add(StudentEnrollment.WITHDRAWAL);
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, subQuery);
        enrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrTypes);
        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        enrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        enrollmentQuery.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
        m_studentEnrollmentMap =
                getBroker().getGroupedCollectionByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 1000);

        // Load assessments
        Criteria assessmentCriteria = new Criteria();
        assessmentCriteria.addEqualTo(
                StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER + m_gsfcAssessmnetAlias,
                BooleanAsStringConverter.TRUE);
        assessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, subQuery);
        QueryByCriteria assessmentQuery = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);
        m_assessmentMap =
                getBroker().getGroupedCollectionByQuery(assessmentQuery, StudentAssessment.COL_STUDENT_OID, 1000);

        // Load a map of student immunization doses by student oid.
        Criteria immunizationCriteria = new Criteria();
        immunizationCriteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, subQuery);
        QueryByCriteria immunizationQuery = new QueryByCriteria(HealthImmunizationDose.class, immunizationCriteria);
        immunizationQuery.addOrderBy(HealthImmunizationDose.COL_STUDENT_OID, true);
        immunizationQuery.addOrderBy(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, true);
        immunizationQuery.addOrderBy(HealthImmunizationDose.COL_DATE, true);
        m_immunizationMap = getBroker().getGroupedCollectionByQuery(immunizationQuery,
                HealthImmunizationDose.COL_STUDENT_OID, 1000);

        // Terms maps key={schoolOid}-{yearContextOid}-{termCode}
        m_termsMap = new HashMap<String, ScheduleTerm>();
        SubQuery scheduleQuery =
                new SubQuery(SchoolScheduleContext.class, SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, null);
        Criteria termCriteria = new Criteria();
        termCriteria.addIn(ScheduleTerm.COL_SCHEDULE_OID, scheduleQuery);
        QueryByCriteria query = new ReportQueryByCriteria(ScheduleTerm.class, termCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ScheduleTerm term = (ScheduleTerm) iterator.next();
                String key = term.getSchedule().getSchoolOid() + "-" + term.getSchedule().getDistrictContextOid() + "-"
                        + term.getCode();
                m_termsMap.put(key, term);
            }
        } finally {
            iterator.close();
        }

        // Load gpa definition for gpa definition name.
        if (gpaDefOid != null) {
            m_gpaDefinition = (GradePointAverageDefinition) getBroker().getBeanByOid(GradePointAverageDefinition.class,
                    gpaDefOid);
            if (m_gpaDefinition != null) {
                Criteria contextCriteria = new Criteria();
                contextCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                        Integer.valueOf(getOrganization().getCurrentContext().getSchoolYear()));
                QueryByCriteria contextQuery = new QueryByCriteria(DistrictSchoolYearContext.class, contextCriteria);
                contextQuery.addOrderBy(DistrictSchoolYearContext.COL_SCHOOL_YEAR, false);
                m_contextList = (List<DistrictSchoolYearContext>) getBroker().getCollectionByQuery(contextQuery);
            }
        }

        // initialize term override map
        m_termOverrideMap = new HashMap<String, Map>();
    }
}
