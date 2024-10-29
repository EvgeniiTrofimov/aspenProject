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

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Assessment export base class for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
abstract public class AssessmentExportData extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used for PARCC exports.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class AssessmentEntity extends StateReportEntity {
        private AssessmentExportData m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public AssessmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            AssessmentDefinition assessmentDefinition = studentAssessment.getAssessmentDefinition();
            Student student = studentAssessment.getStudent();

            String name = " [" + assessmentDefinition.getId() + "]"
                    + " [" + student.getStateId() + "]"
                    + " [" + student.getLocalId() + "]"
                    + " [" + student.getNameView() + "]";

            return name;
        }

        /**
         * Returns the current school.
         *
         * @return SisSchool
         */
        public SisSchool getSchool() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            SisSchool school = studentAssessment.getSchool();

            return school;
        }

        /**
         * Returns the current student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            SisStudent student = studentAssessment.getStudent();

            return student;
        }

        /**
         * This method returns the latest student enrollment record for Entry type before the report
         * date.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getStudentEnrollment() {
            return m_data.m_helper.getEnrollmentForDate(getBean().getOid(), m_data.m_endDate, StudentEnrollment.ENTRY);
        }

        /**
         * Initialize the entity for the student bean provided.
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
            AssessmentExportData asmData = (AssessmentExportData) data;
            StudentAssessment assessment = (StudentAssessment) bean;

            String testContextValue = asmData.getContextValue(assessment);
            String testPeriod = asmData.getTestPeriod(assessment);
            String asmSchoolOid = assessment.getSchoolOid();

            boolean exportAssessment =
                    asmData.m_cycle.equals(testPeriod) && asmData.m_ctxForParccValue.equals(testContextValue) &&
                            (!asmData.isSchoolContext() || asmData.getSchool().getOid().equals(asmSchoolOid));

            if (!exportAssessment) {
                setRowCount(0);
            }

            super.intitialize(data, bean);
            m_data = (AssessmentExportData) data;
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

    }

    /**
     * Retriever for fields that are using programs to get value.
     *
     */
    protected class RetrieveProgramInfo implements FieldRetriever {
        protected static final String CALC_ID = "PGM_INFO";

        /**
         * Calculation parameters
         */
        protected static final String CALC_PARAM_3_LEP_STATUS = "3_LEP_STATUS";
        protected static final String CALC_PARAM_ENG_LEARNER = "ENG_LEARNER";
        protected static final String CALC_PARAM_LEP_EXEMPT = "LEP_EXEMPT";
        protected static final String CALC_PARAM_LEP_STATUS = "LEP_STATUS";
        protected static final String CALC_PARAM_HOMELESS = "HOMELESS";

        private PlainDate m_dateStartCurrentYear;
        private PlainDate m_dateStartLastYear;
        private PlainDate m_dateStartTwoYearsBefore;

        private PlainDate m_dateEndLastYear;
        private PlainDate m_dateEndTwoYearsBefore;

        private DateFormat m_df;

        /**
         * Instantiates a new retrieve program info.
         */
        public RetrieveProgramInfo() {
            m_df = new SimpleDateFormat("yyyy-MM-dd");

            m_dateStartCurrentYear = getCurrentContext().getStartDate();

            Pair<PlainDate, PlainDate> lastYearDates = getDatesOfSchoolYear(-1);
            m_dateStartLastYear = lastYearDates == null ? null : lastYearDates.getLeft();
            m_dateEndLastYear = lastYearDates == null ? null : lastYearDates.getRight();

            Pair<PlainDate, PlainDate> twoYearsBeforeDates = getDatesOfSchoolYear(-2);
            m_dateStartTwoYearsBefore = twoYearsBeforeDates == null ? null : twoYearsBeforeDates.getLeft();
            m_dateEndTwoYearsBefore = twoYearsBeforeDates == null ? null : twoYearsBeforeDates.getRight();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData
         *      data, StateReportEntity entity, FieldDefinition field) throws X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = CODE_NO;
            AssessmentEntity pnpEntity = (AssessmentEntity) entity;
            SisStudent student = pnpEntity.getStudent();
            String param = (String) field.getParameter();

            if (CALC_PARAM_ENG_LEARNER.equalsIgnoreCase(param)) {
                Collection<StudentProgramParticipation> pgms = m_validLEPPgmsMap.get(student.getOid());

                if (pgms != null && !pgms.isEmpty()) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_HOMELESS.equalsIgnoreCase(param)) {
                Collection<StudentProgramParticipation> pgms = m_homelessPgmsMap.get(student.getOid());

                if (pgms != null && !pgms.isEmpty()) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_3_LEP_STATUS.equalsIgnoreCase(param)) {
                Collection<StudentProgramParticipation> pgms = m_validLEPPgmsMap.get(student.getOid());

                if (pgms != null && !pgms.isEmpty()) {
                    value = CODE_YES;
                }
            } else if (CALC_PARAM_LEP_EXEMPT.equalsIgnoreCase(param)) {
                String firstEntUsSkl = (String) student.getFieldValueByBeanPath(m_studentFirstEntryDateInUSField);

                if (!StringUtils.isEmpty(firstEntUsSkl)) {
                    Calendar cal = Calendar.getInstance();
                    cal.add(Calendar.YEAR, -1);
                    PlainDate dateToCompare = new PlainDate(cal.getTime());

                    try {
                        Date firstEntUsSklDate = m_df.parse(firstEntUsSkl);
                        if (firstEntUsSklDate.after(dateToCompare)) {
                            String strGrade = student.getGradeLevel();

                            if (isValidGradeForLEP(strGrade)) {
                                Collection<StudentProgramParticipation> pgms = m_validLEPPgmsMap.get(student.getOid());

                                if (pgms != null && !pgms.isEmpty()) {
                                    value = CODE_YES;
                                }
                            }
                        }
                    } catch (ParseException e) {
                        return value;
                    }
                }
            } else if (CALC_PARAM_LEP_STATUS.equalsIgnoreCase(param)) {
                Collection<StudentProgramParticipation> pgms = m_validLEPPgmsMap.get(student.getOid());

                if (pgms != null && !pgms.isEmpty()) {
                    value = getLepStatus(pgms.iterator().next());
                }
            }

            return value;
        }

        /**
         * Program Participation = LEP
         * <li>< = where pgmActionStart is > 7/1/15 and pgmActionEnd = blank or > today's date</li>
         * <li>1 = where pgmActionStart is between 7/1/14 and 6/30/15 and pgmActionEnd is blank or >
         * today's date</li>
         * <li>2 = where pgmActionStart is between 7/1/13 and 6/30/14 and pgmActionEnd is blank or >
         * today's date where ParentRefusedServices= pgmActionEnd is blank or > today's date</li>
         * <li>3 = where pgmActionStart is < 7/1/13 and pgmActionEnd is blank or > today's date
         * </li>
         * <li>F1 = where pgmActionEnd is between 7/1/14 and current date</li>
         * <li>F2 = where pgmActionEd is between 7/1/13 and 6/30/14</li>
         * <li>Y = where prgActionStart = blank/null</li>
         * <li>R = where DOE PARENT REFUED SERVICES (LEP) = True and pgmActionEnd is blank or >
         * today's date.</li>
         * <li>RE = where DOE PARENT REFUED SERVICES (LEP) = True and pgmActionEnd is < today's
         * date.</li>
         *
         * @param lep StudentProgramParticipation
         * @return String
         */
        private String getLepStatus(StudentProgramParticipation lep) {
            String value = null;
            PlainDate startDate = lep.getStartDate();
            PlainDate endDate = lep.getEndDate();
            boolean hasEnded = lep.getEndDate() != null && !m_endDate.before(lep.getEndDate());

            if (BooleanAsStringConverter.TRUE.equals(lep.getFieldValueByAlias(ALIAS_DOE_PARENT_REFUSED_SERVICES))) {
                value = hasEnded ? "RE" : "R";
            } else {
                if (!hasEnded) {
                    if (startDate == null) {
                        value = "";
                    } else if (startDate.after(m_dateStartCurrentYear)) {
                        value = "<";
                    } else if (startDate.after(m_dateStartLastYear) && startDate.before(m_dateEndLastYear)) {
                        value = "1";
                    } else if (startDate.after(m_dateStartTwoYearsBefore)
                            && startDate.before(m_dateEndTwoYearsBefore)) {
                        value = "2";
                    } else if (startDate.before(m_dateStartTwoYearsBefore)) {
                        value = "3";
                    }
                } else {
                    if (endDate.after(m_dateStartLastYear)) {
                        value = "F1";
                    } else if (endDate.after(m_dateStartTwoYearsBefore)) {
                        value = "F2";
                    }
                }
            }

            return value;
        }

        /**
         * Check if 3<= stdGradeLevel <= 8.
         *
         * @param strGrade String
         * @return true, if is valid grade for LEP
         */
        private boolean isValidGradeForLEP(String strGrade) {
            boolean valid = false;
            if (strGrade.matches("\\d+")) {
                if (strGrade.startsWith("0")) {
                    strGrade = strGrade.replaceFirst("0", "");
                }

                int intGrade = Integer.valueOf(strGrade).intValue();

                if (3 <= intGrade && intGrade <= 8) {
                    valid = true;
                }
            }
            return valid;
        }

        /**
         * Return date with added num of years.
         *
         * @param date Date
         * @param addYears int
         * @return PlainDate
         */
        private PlainDate getDateAddYears(Date date, int addYears) {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(date);
            calendar.add(Calendar.YEAR, addYears);

            return new PlainDate(calendar.getTime());
        }

        /**
         * Returns pair "Start date/End date" of school year context with School year == current
         * school year +
         * passed parameter.
         *
         * @param addSchoolYears int
         * @return Pair
         */
        private Pair<PlainDate, PlainDate> getDatesOfSchoolYear(int addSchoolYears) {
            Pair<PlainDate, PlainDate> dates = null;

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                    Integer.valueOf(getCurrentContext().getSchoolYear() + addSchoolYears));
            QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
            Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);

            if (!contexts.isEmpty()) {
                DistrictSchoolYearContext context = contexts.iterator().next();
                PlainDate start = context.getStartDate();
                PlainDate end = context.getEndDate();
                dates = Pair.of(start, end);
            } else {
                PlainDate start = getCurrentContext().getStartDate();
                PlainDate end = getCurrentContext().getEndDate();

                dates = Pair.of(getDateAddYears(start, addSchoolYears),
                        getDateAddYears(end, addSchoolYears));
            }

            return dates;
        }
    }

    protected static final String PARAM_CTX = "schoolYearContext";
    protected static final String PARAM_CTX_BEAN_PATH = "ctxBeanPath";
    protected static final String PARAM_CYCLE = "cycle";

    /*
     * Input Definition Parameters
     */
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_END_DATE = "endDate";
    protected static final String PARAM_REPORT_START_DATE = "startDate";
    protected static final String PARAM_SORT_PARAM = "sort";

    /*
     * Aliases
     */
    protected static final String ALIAS_DOE_PARENT_REFUSED_SERVICES = "DOE PARENT REFUSED SERVICES";
    protected static final String ALIAS_FIRST_ENTRY_DATE_IN_US_SCHOOL = "DOE FIRST ENT DATE US SC";
    // ASSESSMENT_DEFINITION table (PARCC)
    protected static final String ALIAS_PARCC_PARCCEXPPASS = "PARCCEXPPASS";
    protected static final String ALIAS_PARCC_PARCCSCHOOLLESS1YR = "PARCCSCHOOLLESS1YR";
    protected static final String ALIAS_PARCC_RETEST = "PARCC_RETEST";
    protected static final String ALIAS_PARCC_TESTPERIOD = "PARCCTSTPERIOD";
    protected static final String ALIAS_PARCC_TESTYEAR = "PARCCTSTYEAR";
    protected static final String ALIAS_PARCC_TSTCODE = "PARCCTSTCODE";
    protected static final String ALIAS_PARCC_TSTFORMAT = "PARCCTSTFORMAT";

    protected static final String ERROR_MESSAGE_ASSESS_DEF_ALIAS = " Assessment Definition Alias: ";
    protected static final String ERROR_MESSAGE_IS_NOT_DEFINED = " is not defined";
    protected static final String ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED =
            " Assessment Definition is not loaded in the Data Dictionary.";
    protected static final String ERROR_MESSAGE_NO_ACTIVE_STUDENTS =
            "No students were active in the previous School Year.";
    protected static final String ERROR_TYPE_WARNING = "Warning";

    /**
     * Other internal constants
     */
    protected static final String CODE_YES = "Y";
    protected static final String CODE_NO = "N";
    protected static final String PROGRAM_CODE_LEP = "LEP";
    protected static final String PROGRAM_CODE_HOMELESS = "Homeless";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    public Map<String, String> m_asmAlias = new HashMap();
    public PlainDate m_endDate;
    public StudentHistoryHelper m_helper;
    public Map<String, Collection<StudentProgramParticipation>> m_homelessPgmsMap;
    public Map<String, Collection<StudentProgramParticipation>> m_validLEPPgmsMap;
    protected String m_ctxForParccValue;
    protected String m_cycle;
    protected DataDictionary m_dataDictionary;
    protected Map<String, IepData> m_iepMap;
    protected Boolean m_removeHeader;
    protected PlainDate m_startDate;
    protected String m_studentFirstEntryDateInUSField;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeader == null || m_removeHeader.booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        // System Parameters
        m_startDate = (PlainDate) getParameter(PARAM_REPORT_START_DATE);
        if (m_startDate == null) {
            m_startDate = getOrganization().getCurrentContext().getStartDate();
        }

        // Load Input Definition Parameters
        m_endDate = (PlainDate) getParameter(PARAM_REPORT_END_DATE);
        if (m_endDate == null) {
            m_endDate = new PlainDate();
        }
        m_removeHeader = (Boolean) getParameter(PARAM_REMOVE_HEADER);
        if (m_removeHeader == null) {
            m_removeHeader = Boolean.valueOf(false);
        }
        loadPARCCAssessmentDefinition();

        DistrictSchoolYearContext ctx =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        (String) getParameter(PARAM_CTX));
        m_ctxForParccValue = ctx.getFieldValueByBeanPath((String) getParameter(PARAM_CTX_BEAN_PATH)).toString();

        m_cycle = (String) getParameter(PARAM_CYCLE);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

        QueryByCriteria stdQueryBy = m_helper.getStudentQuery(false);
        int studentCount = getBroker().getCount(stdQueryBy);
        if (studentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, ERROR_MESSAGE_NO_ACTIVE_STUDENTS);
        }

        loadHomelessProgramsMap();
        loadLEPProgramsMap();
        buildIepMap();

        // Check Student count

        // Check Student Assessment count
        X2Criteria studentAssessmentCriteria = getStudentAssessmentCriteria();
        QueryByCriteria studentAssessmentQuery = m_helper.getStudentSelectionQuery(StudentAssessment.class,
                studentAssessmentCriteria, StudentAssessment.COL_STUDENT_OID);

        int studentAssessmentCount = getBroker().getCount(studentAssessmentQuery);
        if (studentAssessmentCount == 0) {
            addSetupError(ERROR_TYPE_WARNING, getNoStudentAssessmentMessage());
        }

        if (getSetupErrors().size() == 0) {
            // Assign the custom entity class, if there is one.
            setQuery(studentAssessmentQuery);
            setEntityClass(AssessmentEntity.class);

            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveProgramInfo.CALC_ID, new RetrieveProgramInfo());
            super.addCalcs(calcs);
        }

    }

    /**
     * This method should be overridden to return a list of additional assessment aliases to load.
     *
     * @return Collection
     */
    protected Collection<String> getAdditionalAliases() {
        return Collections.EMPTY_LIST;
    }

    /**
     * This method should be overridden to return Assessment Definition ID.
     *
     * @return String
     */
    abstract protected String getAsmDefinitionId();

    /**
     * Gets the context value.
     *
     * @param assessment StudentAssessment
     * @return String
     */
    protected String getContextValue(StudentAssessment assessment) {
        return (String) assessment.getFieldValueByAliasExtended(ALIAS_PARCC_TESTYEAR, m_dataDictionary);
    }

    /**
     * This method returns the county, district code by splitting the countyDistrictSchoolCode code.
     *
     * @param countyDistrictSchoolCode String
     * @return String
     */
    protected String getCountyDistrictCode(String countyDistrictSchoolCode) {
        String countyDistrictCode = null;
        if (!StringUtils.isEmpty(countyDistrictSchoolCode) && countyDistrictSchoolCode.length() > 6) {
            countyDistrictCode = countyDistrictSchoolCode.substring(0, 6);
        }

        return countyDistrictCode;
    }

    /**
     * This method should be overridden to return message informing that needed Student Assessments
     * weren't created.
     *
     * @return String
     */
    abstract protected String getNoStudentAssessmentMessage();

    /**
     * Gets the test period.
     *
     * @param assessment StudentAssessment
     * @return String
     */
    protected String getTestPeriod(StudentAssessment assessment) {
        return (String) assessment.getFieldValueByAliasExtended(ALIAS_PARCC_TESTPERIOD, m_dataDictionary);
    }

    /**
     * build Iep Map .
     */
    private void buildIepMap() {
        m_iepMap = new HashMap<String, IepData>();

        // Pre-load the Discarded IEPs first and then Rejected IEPs and then Previous and then Draft
        // IEPs.
        // Make sure that these IEPS have an initial eligibility Date.class Once these IEPs are
        // loaded, load
        // the Previous IEPs and then the Active IEPs.
        X2Criteria discardedIepCriteria = new X2Criteria();
        discardedIepCriteria.addEqualTo(IepData.COL_STATUS_CODE,
                Integer.valueOf(IepData.StatusCode.DISCARDED.ordinal()));
        QueryByCriteria discardedIepQuery =
                m_helper.getStudentSelectionQuery(IepData.class, discardedIepCriteria, IepData.COL_STUDENT_OID);
        discardedIepQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        Map<String, IepData> m_iepMap1 = getBroker().getMapByQuery(discardedIepQuery, IepData.COL_STUDENT_OID, 128);
        m_iepMap.putAll(m_iepMap1);

        X2Criteria rejectedIepCriteria = new X2Criteria();
        rejectedIepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.REJECTED.ordinal()));
        QueryByCriteria rejectedIepQuery =
                m_helper.getStudentSelectionQuery(IepData.class, rejectedIepCriteria, IepData.COL_STUDENT_OID);
        rejectedIepQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        Map<String, IepData> m_iepMap2 = getBroker().getMapByQuery(rejectedIepQuery, IepData.COL_STUDENT_OID, 128);
        m_iepMap.putAll(m_iepMap2);

        X2Criteria draftIepCriteria = new X2Criteria();
        draftIepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.DRAFT.ordinal()));
        QueryByCriteria draftIepQuery =
                m_helper.getStudentSelectionQuery(IepData.class, draftIepCriteria, IepData.COL_STUDENT_OID);
        draftIepQuery.addOrderByAscending(IepData.COL_INITIAL_ELIGIBILITY_DATE);
        draftIepQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        Map<String, IepData> m_iepMap3 = getBroker().getMapByQuery(draftIepQuery, IepData.COL_STUDENT_OID, 128);
        m_iepMap.putAll(m_iepMap3);

        X2Criteria previousIepCriteria = new X2Criteria();
        previousIepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal()));
        QueryByCriteria previousIepQuery =
                m_helper.getStudentSelectionQuery(IepData.class, previousIepCriteria, IepData.COL_STUDENT_OID);
        previousIepQuery.addOrderByAscending(IepData.COL_START_DATE);
        previousIepQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        Map<String, IepData> m_iepMap4 = getBroker().getMapByQuery(previousIepQuery, IepData.COL_STUDENT_OID, 128);
        m_iepMap.putAll(m_iepMap4);

        // Preload the Active IEPs for students next
        // Any student with an Active IEP would override a Previous IEP, if the student had one
        X2Criteria activeIepCriteria = new X2Criteria();
        activeIepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        QueryByCriteria activeIepQuery =
                m_helper.getStudentSelectionQuery(IepData.class, activeIepCriteria, IepData.COL_STUDENT_OID);
        Map<String, IepData> m_iepMap5 = getBroker().getMapByQuery(activeIepQuery, IepData.COL_STUDENT_OID, 128);
        m_iepMap.putAll(m_iepMap5);
    }

    /**
     * determine if a particular calculation is contained in the export.
     *
     * @param calcId String
     * @param calcParam String
     * @return true, if successful
     */
    private boolean fieldInExport(String calcId, String calcParam) {
        boolean value = false;
        for (int i = 0; i < this.getFieldCount(); ++i) {
            FieldDefinition field = this.getFieldDefinition(i);
            if (calcId.equals(field.getCalcId()) && calcParam.equals(field.getParameter())) {
                value = true;
                break;
            }
        }
        return value;
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return String
     */
    private String getAsmJavaName(String alias, DataDictionary dataDictionary) {
        String javaName = null;

        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField != null && dataDictionary.containsAlias(alias)) {
            javaName = dictField.getJavaName();
        }

        return javaName;
    }

    /**
     * Get Student Assessment Criteria using the selected Student Oids.
     *
     * @return X2Criteria
     */
    private X2Criteria getStudentAssessmentCriteria() {
        X2Criteria studentAssessmentCriteria = new X2Criteria();

        studentAssessmentCriteria
                .addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID, getAsmDefinitionId());
        studentAssessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE,
                getOrganization().getCurrentContext().getStartDate());
        studentAssessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE,
                getOrganization().getCurrentContext().getEndDate());

        return studentAssessmentCriteria;
    }

    /**
     * initialize Fields.
     */
    private void initializeFields() {
        if (fieldInExport(RetrieveProgramInfo.CALC_ID, RetrieveProgramInfo.CALC_PARAM_LEP_EXEMPT)) {
            m_studentFirstEntryDateInUSField = translateAliasToJavaName(ALIAS_FIRST_ENTRY_DATE_IN_US_SCHOOL, true);
        }
    }

    /**
     * Load map with current Homeless programs (with blank end date) keyed on stdOid.
     */
    private void loadHomelessProgramsMap() {
        X2Criteria pgmCriteria = new X2Criteria();
        pgmCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_CODE_HOMELESS);
        pgmCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        QueryByCriteria query = m_helper.getStudentSelectionQuery(StudentProgramParticipation.class, pgmCriteria,
                StudentProgramParticipation.COL_STUDENT_OID);

        m_homelessPgmsMap =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);
    }

    /**
     * Load map with current LEP programs keyed on stdOid.
     */
    private void loadLEPProgramsMap() {
        X2Criteria pgmCriteria = new X2Criteria();
        pgmCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_CODE_LEP);

        QueryByCriteria query = m_helper.getStudentSelectionQuery(StudentProgramParticipation.class, pgmCriteria,
                StudentProgramParticipation.COL_STUDENT_OID);
        query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);

        m_validLEPPgmsMap =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);

    }

    /**
     * Load PARCC Assessment Definition Alias field names.
     */
    private void loadPARCCAssessmentDefinition() {
        X2Criteria assessmentDefinitonCriteria = new X2Criteria();
        assessmentDefinitonCriteria.addEqualTo(AssessmentDefinition.COL_ID, getAsmDefinitionId());

        QueryByCriteria assessmentDefinitonQuery =
                new QueryByCriteria(AssessmentDefinition.class, assessmentDefinitonCriteria);

        AssessmentDefinition pARCCDefinition =
                (AssessmentDefinition) getBroker().getBeanByQuery(assessmentDefinitonQuery);

        // Load PARCC database field names by the PARCC Assessment Definition
        if (pARCCDefinition == null) {
            addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId() + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
        } else {
            m_dataDictionary = DataDictionary.getDistrictDictionary(pARCCDefinition,
                    getBroker().getPersistenceKey());

            if (m_dataDictionary == null) {
                addSetupError(ERROR_TYPE_WARNING, getAsmDefinitionId() + ERROR_MESSAGE_ASSESS_DEF_NOT_LOADED);
            } else {
                m_asmAlias.put(ALIAS_PARCC_PARCCSCHOOLLESS1YR,
                        getAsmJavaName(ALIAS_PARCC_PARCCSCHOOLLESS1YR, m_dataDictionary));
                m_asmAlias.put(ALIAS_PARCC_PARCCEXPPASS, getAsmJavaName(ALIAS_PARCC_PARCCEXPPASS, m_dataDictionary));
                m_asmAlias.put(ALIAS_PARCC_RETEST, getAsmJavaName(ALIAS_PARCC_RETEST, m_dataDictionary));
                m_asmAlias.put(ALIAS_PARCC_TESTYEAR, getAsmJavaName(ALIAS_PARCC_TESTYEAR, m_dataDictionary));
                m_asmAlias.put(ALIAS_PARCC_TESTPERIOD, getAsmJavaName(ALIAS_PARCC_TESTPERIOD, m_dataDictionary));
                m_asmAlias.put(ALIAS_PARCC_TSTCODE, getAsmJavaName(ALIAS_PARCC_TSTCODE, m_dataDictionary));
                m_asmAlias.put(ALIAS_PARCC_TSTFORMAT, getAsmJavaName(ALIAS_PARCC_TSTFORMAT, m_dataDictionary));
                for (String alias : getAdditionalAliases()) {
                    m_asmAlias.put(alias, getAsmJavaName(alias, m_dataDictionary));
                }
            }
        }
    }
}
