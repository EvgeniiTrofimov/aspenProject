/*
 * ====================================================================
 * X2 Development Corporation
 * Copyright (c) 2002-2010 X2 Development Corporation. All rights reserved. Redistribution and use
 * in source and binary forms, with or without modification, is not permitted without express
 * written agreement from X2 Development Corporation.
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.GradePointAverageDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentGradePoint;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote style="color:green; font-size:16pt">NH Credits And GPA export.</blockquote>
 *
 * Procedure ID:&nbsp; <code>NHCreditsAndGPA</code>
 * <p>
 *
 * @author X2 Development Corporation
 * @since v3.0
 */
public class NHCreditsAndGPA extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NHCreditsAndGPAEntity extends StateReportEntity {

        Double m_creditsEarned;
        Double m_cumCreditsEarned;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NHCreditsAndGPAEntity() {
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
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";
            return name;
        }

        /**
         * Intitialize.
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

            NHCreditsAndGPA crgpaData = (NHCreditsAndGPA) getData();
            SisStudent student = (SisStudent) bean;

            List<Transcript> allTrns = crgpaData.m_helper.getStudentTranscripts(student);

            if (allTrns != null) {
                double creditsEarned = 0.0;
                double cumCreditsEarned = 0.0;

                for (Transcript trn : allTrns) {
                    if (crgpaData.includeSchool(trn.getSchoolOid())) {
                        if (crgpaData.getCurrentContext().getOid().equals(trn.getDistrictContextOid())) {
                            creditsEarned += trn.getTotalCredit().doubleValue();
                        }

                        cumCreditsEarned += trn.getTotalCredit().doubleValue();
                    }
                }

                m_creditsEarned = Double.valueOf(creditsEarned);
                m_cumCreditsEarned = Double.valueOf(cumCreditsEarned);
            } else {
                setRowCount(0);
            }
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
         * Gets the credits earned.
         *
         * @return Credits Earned
         */
        public Double getCreditsEarned() {
            return m_creditsEarned;
        }

        /**
         * Gets the cum credits earned.
         *
         * @return Cumulative Credits Earned
         */
        public Double getCumCreditsEarned() {
            return m_cumCreditsEarned;
        }
    }

    /**
     * Returns information of </br>
     * <li>Credits Earned (this year)</li>
     * <li>Cumulative Credits Earned to date (all HS Years)</li>.
     */
    protected class RetrieveCreditsEarnedInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NHCreditsAndGPAEntity crgpaEntity = (NHCreditsAndGPAEntity) entity;
            String parameter = (String) field.getParameter();
            Object value = null;

            if (CALC_PARAM_CREDITS_EARNED.equals(parameter)) {
                value = crgpaEntity.getCreditsEarned();
            } else if (CALC_PARAM_CREDITS_CUM.equals(parameter)) {
                value = crgpaEntity.getCumCreditsEarned();
            }
            return value;
        }
    }

    /**
     * Returns information of </br>
     * <li>End of Year Grade Point Average</li>
     * <li>Cumulative Grade Point Average</li>.
     */
    protected class RetrieveGPAInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent std = (SisStudent) entity.getBean();
            NHCreditsAndGPA crgpaData = (NHCreditsAndGPA) data;
            StudentGradePoint gpt = crgpaData.getStdGradePoint(std.getOid());
            String parameter = (String) field.getParameter();
            Object value = null;

            if (gpt != null) {
                if (CALC_PARAM_GPA_EOY.equals(parameter)) {
                    value = gpt.getGpa();
                } else if (CALC_PARAM_GPA_CUM.equals(parameter)) {
                    value = gpt.getCumulativeGpa();
                }
            }
            return value;
        }
    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_GPA_DEF = "gpaDef";
    protected static final String PARAM_INCLUDE_NAMES = "includeStudentName";

    /**
     * Retriever Parameters
     */
    private static final String CALC_ID_GPA_INFO = "GPA-INFO";
    private static final String CALC_ID_TRN_INFO = "TRN-INFO";
    private static final String CALC_PARAM_CREDITS_CUM = "CREDITS_CUM";
    private static final String CALC_PARAM_CREDITS_EARNED = "CREDITS_EARNED";
    private static final String CALC_PARAM_GPA_CUM = "GPA_CUM";
    private static final String CALC_PARAM_GPA_EOY = "GPA_EOY";

    private static final String ALIAS_EXCLUDE_SCHOOL = "i4see EXCLUDE SCHOOL";
    private static final String ALIAS_STD_STATUS = "i4see Status";
    private static final String STUDENT_NAME = "name view";

    /**
     * Supporting instance variables.
     */
    private String m_excludeSchool;
    private Map m_excludeSchoolMap;
    private String m_fieldStdStatus;
    protected StudentHistoryHelper m_helper;
    private boolean m_removeExcludedSkl = false;
    private Map<String, StudentGradePoint> m_stdGradePointMap;

    private List<String> GRADE_STATE_CODES = Arrays.asList(new String[] {"9", "09", "10", "11", "12"});

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return Student.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "NH Credits And GPA";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Gets the std grade point.
     *
     * @param stdOid String
     * @return the m_stdGradePointMap
     */
    public StudentGradePoint getStdGradePoint(String stdOid) {
        return m_stdGradePointMap.get(stdOid);
    }

    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        if (m_removeExcludedSkl) {
            return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
        }
        return true;
    }

    /**
     * Initialize the data module. Initialize necessary working resources. Define query for students
     * to load. Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadSchoolExcludeMap();

            /*
             * Build query object that will be used to retrieve export students.
             */
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_TRANSCRIPT, Boolean.TRUE);

            m_helper.getStudentTranscriptCriteria().addNotNull(Transcript.COL_TOTAL_CREDIT);
            m_helper.getStudentTranscriptCriteria().addGreaterThan(Transcript.COL_TOTAL_CREDIT, Integer.valueOf(0));
            m_helper.getStudentTranscriptCriteria().addIn(Transcript.COL_GRADE_LEVEL, getGradeCodes());

            SubQuery stdOidsSubQuery =
                    new SubQuery(Transcript.class, Transcript.COL_STUDENT_OID, m_helper.getStudentTranscriptCriteria());
            m_helper.getStudentCriteria().addIn(X2BaseBean.COL_OID, stdOidsSubQuery);
            m_helper.getStudentCriteria().addNotIn(m_fieldStdStatus, getCodesToExcludeStudents());


            X2Criteria activeCriteria = new X2Criteria();
            activeCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                    Student.COL_ENROLLMENT_STATUS));

            X2Criteria graduateCriteria = new X2Criteria();
            graduateCriteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, "Graduate");
            graduateCriteria.addEqualTo(Student.COL_YOG, Integer.valueOf(getCurrentContext().getSchoolYear()));

            activeCriteria.addOrCriteria(graduateCriteria);
            m_helper.getStudentCriteria().addAndCriteria(activeCriteria);

            if (isSchoolContext()) {
                m_helper.getStudentCriteria().addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
            }

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(NHCreditsAndGPAEntity.class);

            loadGradePointMap();

            /*
             * Add the Student Name field if requested
             */
            if (getParameter(PARAM_INCLUDE_NAMES) != null
                    && ((Boolean) getParameter(PARAM_INCLUDE_NAMES)).booleanValue()) {
                getFieldDefinitions().add(0, new FieldDefinition(STUDENT_NAME, SisStudent.COL_NAME_VIEW,
                        null, false, 1, 32, null,
                        null, null, null, null));
            }

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_TRN_INFO, new RetrieveCreditsEarnedInfo());
            calcs.put(CALC_ID_GPA_INFO, new RetrieveGPAInfo());
            super.addCalcs(calcs);
        }
    }

    /**
     * Gets the codes to exclude students.
     *
     * @return Sets the
     */
    private Set<String> getCodesToExcludeStudents() {
        Set<String> codesToExcludeStudents = new HashSet<String>();
        DataDictionaryField stdStatusField = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_STD_STATUS);
        if (stdStatusField != null && stdStatusField.hasReferenceTable()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, stdStatusField.getReferenceTableOid());
            criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, "2");
            QueryByCriteria exclusionQuery = new QueryByCriteria(ReferenceCode.class, criteria);
            codesToExcludeStudents.addAll(
                    getBroker().getGroupedCollectionByQuery(exclusionQuery, ReferenceCode.COL_CODE, 1024).keySet());
        }
        return codesToExcludeStudents;
    }

    /**
     * Gets the grade codes.
     *
     * @return List of local codes of Free and Reduced Indicator.
     */
    private Set<String> getGradeCodes() {
        Set<String> codes = new HashSet<String>();
        DataDictionaryField gradeDataField = getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
        if (gradeDataField != null && !StringUtils.isEmpty(gradeDataField.getReferenceTableOid())) {
            X2Criteria gradeCodesCriteria = new X2Criteria();
            gradeCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, gradeDataField.getReferenceTableOid());
            gradeCodesCriteria.addIn(ReferenceCode.COL_STATE_CODE, GRADE_STATE_CODES);

            QueryByCriteria gradeCodesQuery = new QueryByCriteria(ReferenceCode.class, gradeCodesCriteria);

            codes.addAll(
                    getBroker().getGroupedCollectionByQuery(gradeCodesQuery, ReferenceCode.COL_CODE, 1024).keySet());
        }
        return codes;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_removeExcludedSkl = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldStdStatus = translateAliasToJavaName(ALIAS_STD_STATUS, true);
    }

    /**
     * Build a map of StudentGradePoint for the current context keyed on student oid.
     */
    private void loadGradePointMap() {
        String gpdOid = (String) getParameter(PARAM_GPA_DEF);

        X2Criteria gptCriteria = new X2Criteria();

        if (!StringUtils.isEmpty(gpdOid)) {
            gptCriteria.addEqualTo(StudentGradePoint.REL_GRADE_POINT_AVERAGE_DEFINITION + ModelProperty.PATH_DELIMITER
                    + X2BaseBean.COL_OID, gpdOid);
        } else {
            gptCriteria.addEqualTo(StudentGradePoint.REL_GRADE_POINT_AVERAGE_DEFINITION + ModelProperty.PATH_DELIMITER
                    + GradePointAverageDefinition.COL_GPA_DEFINITION_NAME, "__dummy__");
        }

        gptCriteria.addEqualTo(StudentGradePoint.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        SubQuery stdOidsSubQuery =
                new SubQuery(Transcript.class, Transcript.COL_STUDENT_OID, m_helper.getStudentTranscriptCriteria());
        gptCriteria.addIn(StudentGradePoint.COL_STUDENT_OID, stdOidsSubQuery);

        QueryByCriteria gptQueryByCriteria = new QueryByCriteria(StudentGradePoint.class, gptCriteria);

        m_stdGradePointMap = getBroker().getMapByQuery(gptQueryByCriteria, StudentGradePoint.COL_STUDENT_OID, 1024);
    }

    /**
     * Build a map of schools with the alias set to exclude school.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);

        BeanQuery query = new BeanQuery(School.class, schoolCriteria);

        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }
}
