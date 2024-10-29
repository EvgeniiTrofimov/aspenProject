/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */


package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * FL Title I Supplemental Educational Services report.
 */
public class FLStudentCourseTranscriptInfData extends FLStateReportData {

    /**
     * The Class FLStudentCourseTranscriptInfEntity.
     */
    public static class FLStudentCourseTranscriptInfEntity extends FLStateReportEntity {
        private SisStudent m_bean;
        private FLStudentCourseTranscriptInfData m_data;
        private List<Transcript> m_records;
        private List<Integer> m_sequences;

        /**
         * Instantiates a new FL student course transcript inf entity.
         */
        public FLStudentCourseTranscriptInfEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         */
        @Override
        public String getEntityName() {
            Transcript info = getTranscript();
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", " + info.getSchoolCourse().getNumber() +
                    "] ";
            return name;
        }

        /**
         * Gets the sequence.
         *
         * @return Integer
         */
        public Integer getSequence() {
            return m_sequences.get(getCurrentRow());
        }

        /**
         * Gets the transcript.
         *
         * @return Transcript
         */
        public Transcript getTranscript() {
            return m_records.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (FLStudentCourseTranscriptInfData) data;
            m_bean = (SisStudent) getBean();

            int rowCount = 0;
            StudentInfo studentInfo = m_data.getStudentHelper().getStudentInfo(m_bean);
            String gradeLevel = studentInfo.getGradeLevel(m_data.getSurveyPeriod().getDateCertain());

            if (studentInfo != null && FLStudentHelper.GRADE_LEVELS_9_12.contains(gradeLevel)
                    && m_data.getStudentHelper().isStudentEligible(m_bean)) {
                m_records = m_data.getStudentHelper().getStudentTranscripts(m_bean);
                if (m_records != null && m_records.size() > 0) {
                    sortRecords();
                    initSequence();
                    rowCount = m_records.size();
                }
            }
            setRowCount(rowCount);
        }

        /**
         * Compare transcript.
         *
         * @param trn1 Transcript
         * @param trn2 Transcript
         * @return true, if successful
         */
        private boolean compareTranscript(Transcript trn1, Transcript trn2) {
            if (trn1.getDistrictContextOid() != null &&
                    trn1.getDistrictContextOid().equals(trn2.getDistrictContextOid())) {
                Course course1 = trn1.getSchoolCourse() == null ? null : trn1.getSchoolCourse().getCourse();
                Course course2 = trn2.getSchoolCourse() == null ? null : trn2.getSchoolCourse().getCourse();
                Object value1 =
                        course1 == null ? null : course1.getFieldValueByBeanPath(m_data.m_fieldCrsNo.getJavaName());
                Object value2 =
                        course2 == null ? null : course2.getFieldValueByBeanPath(m_data.m_fieldCrsNo.getJavaName());
                if (value1 != null && value1.equals(value2)) {
                    return true;
                }
            }
            return false;
        }

        /**
         * Inits the sequence.
         */
        private void initSequence() {
            m_sequences = new ArrayList(m_records.size());
            Transcript prior = null;
            int seq = 1;
            for (Transcript trn : m_records) {
                if (prior != null && compareTranscript(trn, prior)) {
                    seq++;
                } else {
                    seq = 1;
                }
                prior = trn;
                m_sequences.add(Integer.valueOf(seq));
            }
        }

        /**
         * Sort records.
         */
        private void sortRecords() {
            Collections.sort(m_records, new Comparator<Transcript>() {
                @Override
                public int compare(Transcript o1, Transcript o2) {
                    int value = beanCompare(o1.getDistrictContext(), o2.getDistrictContext(),
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR);
                    if (value == 0) {
                        Course course1 = o1.getSchoolCourse() == null ? null : o1.getSchoolCourse().getCourse();
                        Course course2 = o2.getSchoolCourse() == null ? null : o2.getSchoolCourse().getCourse();
                        value = beanCompare(course1, course2, m_data.m_fieldCrsNo.getJavaName());
                    }
                    if (value == 0) {
                        value = beanCompare(o1, o2, X2BaseBean.COL_OID);
                    }
                    return value;
                }

                private int beanCompare(X2BaseBean bean1, X2BaseBean bean2, String beanPath) {
                    if (bean1 == null) {
                        if (bean2 == null) {
                            return 0;
                        }
                        return 1;
                    } else if (bean2 == null) {
                        return -1;
                    } else {
                        Comparable value1 = (Comparable) bean1.getFieldValueByBeanPath(beanPath);
                        Comparable value2 = (Comparable) bean2.getFieldValueByBeanPath(beanPath);
                        if (value1 == null) {
                            return value2 == null ? 0 : -1;
                        } else if (value2 == null) {
                            return 1;
                        }
                        return value1.compareTo(value2);
                    }
                }
            });
        }
    }

    /**
     * Field retriever for Student Course Transcript Information fields.
     */
    protected class RetrieveTranscriptInfo implements FieldRetriever {

        public static final String CALC_ID = "CALC_SCTI";

        private static final String ALIAS_CRS_FLAG = "all-crs-Flag";
        private static final String ALIAS_CRS_NUMB_SUBSTIT = "all-crs-NumbSubstit";
        private static final String ALIAS_CRS_SUBSTIT_SSAR = "all-crs-SubstitSSAR";
        private static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";
        private static final String ALIAS_ONLINE_CRS = "all-crs-OnlineCourse";
        private static final String ALIAS_SCHOOL_NUMBER_CE = "all-skl-StateId";
        private static final String ALIAS_STATE_SAR = "all-crs-StateSAR";
        private static final String ALIAS_TRNSFR_DISTRICT = "all-trn-TransferDistrictNumber";
        private static final String ALIAS_TRNSFR_SCHOOL = "all-trn-TransferSchoolNumber";
        private static final String ALIAS_TRNSFR_TERM = "all-trn-TransferTerm";

        private static final String PARAM_SCTI_COURSE_FLAG = "SCTI_COURSE_FLAG";
        private static final String PARAM_SCTI_COURSE_ID = "SCTI_COURSE_ID";
        private static final String PARAM_SCTI_COURSE_ID_SUBS = "SCTI_COURSE_ID_SUBS";
        private static final String PARAM_SCTI_COURSE_STATE_SUB = "SCTI_COURSE_ST_SUB";
        private static final String PARAM_SCTI_COURSE_SUBS_SSAR = "SCTI_COURSE_SUBS_SSAR";
        private static final String PARAM_SCTI_CREDIT_ATTEMPT = "SCTI_CREDIT_ATTEMPT";
        private static final String PARAM_SCTI_CREDIT_EARNED = "SCTI_CREDIT_EARNED";
        private static final String PARAM_SCTI_DISTRICT_NUMBER_WCRE = "SCTI_DISTRICT_NUMBER_WCRE";
        private static final String PARAM_SCTI_FINAL_GRADE = "SCTI_FINAL_GRADE";
        private static final String PARAM_SCTI_GRADE_LEVEL = "SCTI_GRADE_LEVEL";
        private static final String PARAM_SCTI_ONLINE_COURSE = "SCTI_ONLINE_COURSE";
        private static final String PARAM_SCTI_SCHOOL_NUMBER_CE = "SCTI_SCHOOL_NUMBER_CE";
        private static final String PARAM_SCTI_SCHOOL_NUMBER_WCRE = "SCTI_SCHOOL_NUMBER_WCRE";
        private static final String PARAM_SCTI_SEQ_COURSE_ID = "SCTI_SEQ_COURSE_ID";
        private static final String PARAM_SCTI_SURVEY_PERIOD_CODE = "SCTI_SURVEY_PERIOD_CODE";
        private static final String PARAM_SCTI_TERM_CODE = "SCTI_TERM_CODE";
        private static final String PARAM_SCTI_YEAR_COURSE_TAKEN = "SCTI_YEAR_COUR_TAKEN";
        private static final String PARAM_SCTI_YEAR_REC_SUB = "SCTI_YEAR_REC_SUB";

        final private BigDecimal BIG_DECIMAL_100 = new BigDecimal(100);

        private DataDictionaryField m_fieldCourseId;
        private DataDictionaryField m_fieldCrsFlag;
        private DataDictionaryField m_fieldCrsNumbSubstit;
        private DataDictionaryField m_fieldCrsOnlineCourse;
        private DataDictionaryField m_fieldCrsStateSAR;
        private DataDictionaryField m_fieldCrsSubstitSSAR;
        private DataDictionaryField m_fieldDistrictNumber;
        private DataDictionaryField m_fieldScheduleTermCode;
        private DataDictionaryField m_fieldSchoolNumberCE;
        private DataDictionaryField m_fieldTransferDistrictNumber;
        private DataDictionaryField m_fieldTransferSchoolNumber;
        private DataDictionaryField m_fieldTransferTerm;
        private Calendar m_calendar = Calendar.getInstance();
        private Map<Integer, SisDistrictSchoolYearContext> m_contextMap;
        private GradesManager m_gradesManager;
        private Map<String, GradeScale> m_gradeScales;

        /**
         * Instantiates a new retrieve transcript info.
         *
         * @throws X2BaseException exception
         */
        public RetrieveTranscriptInfo() throws X2BaseException {
            m_contextMap = getBroker().getMapByQuery(
                    new BeanQuery(SisDistrictSchoolYearContext.class),
                    SisDistrictSchoolYearContext.COL_SCHOOL_YEAR, 32);
            m_gradesManager = new GradesManager(getBroker());
            loadGradeScales();

            m_fieldSchoolNumberCE = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER_CE, true);
            m_fieldScheduleTermCode =
                    getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
            m_fieldCourseId = translateAliasToDictionaryField(ALIAS_COURSE_NUMBER, true);
            m_fieldCrsNumbSubstit = translateAliasToDictionaryField(ALIAS_CRS_NUMB_SUBSTIT, true);
            m_fieldCrsStateSAR = translateAliasToDictionaryField(ALIAS_STATE_SAR, true);
            m_fieldCrsFlag = translateAliasToDictionaryField(ALIAS_CRS_FLAG, true);
            m_fieldCrsSubstitSSAR = translateAliasToDictionaryField(ALIAS_CRS_SUBSTIT_SSAR, true);
            m_fieldCrsOnlineCourse = translateAliasToDictionaryField(ALIAS_ONLINE_CRS, true);
            m_fieldDistrictNumber = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
            m_fieldTransferDistrictNumber = translateAliasToDictionaryField(ALIAS_TRNSFR_DISTRICT, true);
            m_fieldTransferSchoolNumber = translateAliasToDictionaryField(ALIAS_TRNSFR_SCHOOL, true);
            m_fieldTransferTerm = translateAliasToDictionaryField(ALIAS_TRNSFR_TERM, true);

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Transcript trn = ((FLStudentCourseTranscriptInfEntity) entity).getTranscript();
            FLStudentCourseTranscriptInfData flData = (FLStudentCourseTranscriptInfData) data;
            SchoolCourse schoolCourse = trn.getSchoolCourse();
            Course course = schoolCourse == null ? null : schoolCourse.getCourse();

            String parameter = (String) field.getParameter();
            Object value = null;

            switch (parameter) {
                case PARAM_SCTI_SCHOOL_NUMBER_CE:
                    value = flData.getFieldValue(trn.getSchool(), m_fieldSchoolNumberCE);
                    break;
                case PARAM_SCTI_DISTRICT_NUMBER_WCRE:
                    value = flData.getFieldValue(trn, m_fieldTransferDistrictNumber);
                    if (StringUtils.isEmpty((String) value)) {
                        value = flData.getFieldValue(((SisStudent) entity.getBean()).getOrganization1(),
                                m_fieldDistrictNumber);
                    }
                    break;
                case PARAM_SCTI_SCHOOL_NUMBER_WCRE:
                    value = flData.getFieldValue(trn, m_fieldTransferSchoolNumber);
                    if (StringUtils.isEmpty((String) value)) {
                        value = flData.getFieldValue(trn.getSchool(), m_fieldSchoolNumberCE);
                    }
                    break;
                case PARAM_SCTI_SURVEY_PERIOD_CODE:
                    value = flData.getSurveyPeriodCode();
                    break;
                case PARAM_SCTI_YEAR_REC_SUB:
                    if (!StringUtils.isEmpty(trn.getGradeLevel()) && trn.getDistrictContext() != null) {
                        int intGradeLevel = 9;
                        try {
                            intGradeLevel = Integer.parseInt(trn.getGradeLevel());
                        } catch (NumberFormatException e) {
                            // gradeLevel cannot be parsed - no value
                        }
                        SisDistrictSchoolYearContext context = trn.getDistrictContext();
                        if (intGradeLevel < 9) {
                            int yearSubmitted = context.getSchoolYear() + 9 - intGradeLevel;
                            SisDistrictSchoolYearContext tmpContext = m_contextMap.get(Integer.valueOf(yearSubmitted));
                            if (tmpContext != null) {
                                context = tmpContext;
                            }
                        }
                        value = FLStateReportData.getFiscalYear(m_calendar, context);
                    }
                    break;
                case PARAM_SCTI_YEAR_COURSE_TAKEN:
                    if (trn.getDistrictContext() != null) {
                        value = FLStateReportData.getFiscalYear(m_calendar, trn.getDistrictContext());
                    }
                    break;
                case PARAM_SCTI_GRADE_LEVEL:
                    value = trn.getGradeLevel();
                    break;
                case PARAM_SCTI_TERM_CODE:
                    if (trn.getMasterSchedule() != null && trn.getMasterSchedule().getScheduleTerm() != null) {
                        value = flData.getFieldValue(trn.getMasterSchedule().getScheduleTerm(),
                                m_fieldScheduleTermCode);
                    } else {
                        value = flData.getFieldValue(trn, m_fieldTransferTerm);
                    }
                    break;
                case PARAM_SCTI_COURSE_ID:
                    value = flData.getFieldValue(course, m_fieldCourseId);
                    break;
                case PARAM_SCTI_SEQ_COURSE_ID:
                    value = ((FLStudentCourseTranscriptInfEntity) entity).getSequence();
                    break;
                case PARAM_SCTI_COURSE_ID_SUBS:
                    value = flData.getFieldValue(course, m_fieldCrsNumbSubstit);
                    break;
                case PARAM_SCTI_COURSE_STATE_SUB:
                    value = flData.getFieldValue(course, m_fieldCrsStateSAR);
                    break;
                case PARAM_SCTI_COURSE_FLAG:
                    value = flData.getFieldValue(course, m_fieldCrsFlag);
                    break;
                case PARAM_SCTI_CREDIT_ATTEMPT:
                    if (!StringUtils.isEmpty(trn.getPotentialCredit())) {
                        BigDecimal creditAsNumber = null;
                        try {
                            creditAsNumber = new BigDecimal(trn.getPotentialCredit()).multiply(BIG_DECIMAL_100);
                        } catch (NumberFormatException nfe) {
                            // nothing. The credit is not numeric.
                        }
                        value = creditAsNumber;
                    }
                    if (value == null) {
                        if (schoolCourse != null && schoolCourse.getCredit() != null) {
                            value = schoolCourse.getCredit().multiply(BIG_DECIMAL_100);
                        } else if (course != null && course.getCredit() != null) {
                            value = course.getCredit().multiply(BIG_DECIMAL_100);
                        }
                    }
                    break;
                case PARAM_SCTI_CREDIT_EARNED:
                    value = trn.getTotalCredit() == null ? BigDecimal.ZERO
                            : trn.getTotalCredit().multiply(BIG_DECIMAL_100);
                    break;
                case PARAM_SCTI_FINAL_GRADE:
                    String finalGrade = trn.getFinalGrade();

                    if (!StringUtils.isEmpty(finalGrade)) {
                        value = finalGrade;
                        GradeScale scale = m_gradeScales.get(trn.getTranscriptDefinitionOid());
                        if (StringUtils.isNumeric(finalGrade) && scale != null) {
                            // Try the final grade as a number.
                            BigDecimal gradeAsNumber = null;
                            try {
                                gradeAsNumber = new BigDecimal(finalGrade);
                            } catch (NumberFormatException nfe) {
                                // nothing. The grade is not numeric.
                            }

                            if (gradeAsNumber != null) {
                                value = m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                        trn.getSchool(), trn.getSchoolCourseOid());
                            }
                        }
                    }
                    break;
                case PARAM_SCTI_COURSE_SUBS_SSAR:
                    value = flData.getFieldValue(course, m_fieldCrsSubstitSSAR);
                    break;
                case PARAM_SCTI_ONLINE_COURSE:
                    value = flData.getFieldValue(course, m_fieldCrsOnlineCourse);
                    break;
            }
            return value;
        }

        /**
         * Load grade scales.
         */
        private void loadGradeScales() {
            /*
             * map grade scales by transcript definition Oid for easier retrieval.
             */
            m_gradeScales = new HashMap<String, GradeScale>();
            X2Criteria criteria = new X2Criteria();

            // Find the column definition that points to TRN_FINAL_GRADE
            criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                    Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
            QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    TranscriptColumnDefinition tcd = (TranscriptColumnDefinition) iterator.next();
                    m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
                }
            } finally {
                iterator.close();
            }
        }

    }

    /**
     * Instance variables.
     */

    protected DataDictionaryField m_fieldCrsNo;

    protected static final String ALIAS_COURSE_NUMBER = "all-crs-StateId";
    private static final List<String> ALGEBRA_COURSE_NUMBERS =
            Arrays.asList("1200310", "1200320", "1200370", "1200380", "1200390", "1209810");
    private static final List<String> SCTI_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_1,
                    FLStateReportData.SURVEY_PERIOD_2, FLStateReportData.SURVEY_PERIOD_3,
                    FLStateReportData.SURVEY_PERIOD_4, FLStateReportData.SURVEY_PERIOD_5,
                    FLStateReportData.SURVEY_PERIOD_9,
                    FLStateReportData.SURVEY_PERIOD_F, FLStateReportData.SURVEY_PERIOD_G,
                    FLStateReportData.SURVEY_PERIOD_S,
                    FLStateReportData.SURVEY_PERIOD_W, FLStateReportData.SURVEY_PERIOD_X);

    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() == 0) {
            m_fieldCrsNo = translateAliasToDictionaryField(ALIAS_COURSE_NUMBER, true);
            initializeTranscriptCriteria();
            setQuery(getStudentHelper().getStudentQuery(false));
            setEntityClass(FLStudentCourseTranscriptInfEntity.class);

            // Add any necessary FieldRetrievers an Field Validators
            registerFieldRetrievers();
            registerFieldValidators();
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return SCTI_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Initialize transcript criteria.
     *
     * @throws X2BaseException exception
     */
    private void initializeTranscriptCriteria() throws X2BaseException {
        X2Criteria transcriptCriteria = new X2Criteria();

        X2Criteria gradeLevelCriteria = new X2Criteria();
        gradeLevelCriteria.addIn(Transcript.COL_GRADE_LEVEL, FLStudentHelper.GRADE_LEVELS_9_12);

        X2Criteria algebraCriteria = new X2Criteria();
        algebraCriteria.addIn(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                m_fieldCrsNo.getJavaName(), ALGEBRA_COURSE_NUMBERS);

        X2Criteria highSchoolCreditCriteria = new X2Criteria();
        Collection<String> codes = getCodesForStateValue(Course.class, Course.COL_SCHOOL_LEVEL, Arrays.asList("HS"));
        highSchoolCreditCriteria.addIn(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                Course.COL_SCHOOL_LEVEL, codes);

        transcriptCriteria.addOrCriteria(gradeLevelCriteria);
        transcriptCriteria.addOrCriteria(algebraCriteria);
        transcriptCriteria.addOrCriteria(highSchoolCreditCriteria);

        getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_TRANSCRIPT, Boolean.TRUE);
        getStudentHelper().getStudentTranscriptCriteria().addAndCriteria(transcriptCriteria);
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */

    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveTranscriptInfo.CALC_ID,
                new RetrieveTranscriptInfo());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */

    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
