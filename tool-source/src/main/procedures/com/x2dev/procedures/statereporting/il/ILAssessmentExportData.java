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

package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.AssessmentExportData;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * IL Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class ILAssessmentExportData extends AssessmentExportData {

    /**
     * The Class ILAssessmentEntity.
     */
    public static class ILAssessmentEntity extends AssessmentEntity {
        /**
         * The effective Entry student enrollment record for report date.
         */
        StudentEnrollment m_enrollment = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
        }

        /**
         * @see com.x2dev.procedures.statereporting.AssessmentExportData.AssessmentEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            ILAssessmentExportData asmData = (ILAssessmentExportData) data;

            // Do not report students not active on report date.
            m_enrollment = asmData.m_helper.getEnrollmentForDate(getStudent().getOid(), asmData.m_endDate, "EW");
            if ((m_enrollment == null) || !asmData.m_activeCode.equals(m_enrollment.getStatusCode())) {
                setRowCount(0);
            }

            // Get effective "E"ntry enrollment record for this date.
            m_enrollment = asmData.m_helper.getEnrollmentForDate(getStudent().getOid(), asmData.m_endDate, "E");
            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(asmData.m_excludeSklField))) {
                m_rcdtsMap = lookupOverrides();
            } else {
                setRowCount(0);
            }
        }

        /**
         * Lookup district and school codes, including overrides on the enrollment record.
         *
         * @return Map
         */
        private Map<String, String> lookupOverrides() {
            Map<String, String> calcValueMap = new HashMap<String, String>();
            ILAssessmentExportData asmData = (ILAssessmentExportData) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (asmData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode =
                        asmData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            asmData.lookupStateValue(StudentEnrollment.class, asmData.m_fieldServiceSchoolCode,
                                    serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
        }
    }

    /**
     * "Yes" if student is new arrival to US, "No" otherwise.
     */
    protected class RetrieveDateEntryUS implements FieldRetriever {
        protected static final String CALC_ID = "ASM-DATE-ENTRY-US";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String studentOid = ((StudentAssessment) entity.getBean()).getStudent().getOid();
            return m_newArrivalToUS.contains(studentOid) ? "01" : "02";
        }
    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {
        protected static final String CALC_ID = "ASM-RCDTS";
        protected static final String FIELD_HOME_SKL = "RCDTSHomeSch";
        /**
         * Calculation parameters
         */
        protected static final String CALC_PARAM_HOME_SCHOOL = "H";
        protected static final String CALC_PARAM_SERVICE_SCHOOL = "S";
        protected static final String CALC_PARAM_TEST_SCHOOL = "T";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ILAssessmentEntity asmEntity = (ILAssessmentEntity) entity;
            ILAssessmentExportData asmData = (ILAssessmentExportData) data;

            String param = (String) field.getParameter();
            String rcdts = null;
            if (param.equals(CALC_PARAM_HOME_SCHOOL) && asmEntity.getEffectiveEnrollment() != null
                    && asmEntity.getEffectiveEnrollment().getSchool() != null) {
                rcdts = (String) asmEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(asmData.m_fieldSchoolCode);
            } else if (param.equals(CALC_PARAM_SERVICE_SCHOOL)) {
                rcdts = asmEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = (String) asmEntity.getSchool().getFieldValueByBeanPath(asmData.m_fieldSchoolCode);
                }
            } else if (param.equals(CALC_PARAM_TEST_SCHOOL)) {
                SisStudent std = ((StudentAssessment) entity.getBean()).getStudent();
                rcdts = (String) (std.getFieldValueByBeanPath(m_stdTestSchool) != null
                        ? std.getFieldValueByBeanPath(m_stdTestSchool) : entity.getFieldValue(FIELD_HOME_SKL));
            }

            return rcdts;
        }
    }

    /**
     * Retrieve Session Name
     *
     * Course Code + Section + Teacher Last Name + Cycle Days .
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSessionName implements FieldRetriever {

        protected static final String CALC_ID = "SESSION-NAME";

        /**
         * Calculation Parameters.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StringBuilder value = new StringBuilder();
            StudentAssessment studentAssessment = (StudentAssessment) entity.getBean();
            MasterSchedule section = studentAssessment.getMasterSchedule();

            if (section != null && section.getPrimaryStaff() != null && section.getPrimaryStaff().getPerson() != null) {
                value.append(section.getCourseView());
                value.append(" ");
                value.append(section.getPrimaryStaff().getPerson().getLastName());
                value.append(" ");
                value.append(section.getScheduleDisplay());
            }
            return value.toString();
        }
    }

    /**
     * Retrieve school year.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {
        protected static final String CALC_ID = "SCHOOL-YEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return Integer.valueOf(getCurrentContext().getSchoolYear());
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_BIRTH_COUNTRY_CODE = "DOE BIRTH COUNTRY";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_FIRST_YEAR_IN_US = "DOE NEW ARRIVAL";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_STD_TEST_SCHOOL = "DOE SCHOOL TEST";
    /**
     * Other internal constants
     */
    protected static final String CODE_ASSESSMENT_DEFINITION_ID_IL = "IL PARCC";
    protected static final String CODE_US = "US";
    protected static final String ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS =
            "No IL PARCC Student Assessment's were created by the selected students.";
    protected static final String STATE_CODE_IL = "IL";

    /**
     * A map of reference codes for RCDTS serving school codes, for use in RCDTS retriever
     */
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();

    /*
     * Instance variables
     */
    protected String m_activeCode;
    protected String m_birthCountry;
    protected String m_excludeSklField;
    protected String m_fieldDateEntryUS;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected Collection<String> m_newArrivalToUS;
    protected String m_stdTestSchool;

    /**
     * Helper class:
     * For student selection by enrollment.
     * For Student schedule span.
     */
    protected StudentHistoryHelper m_helperSched;

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeader == null || m_removeHeader.booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        super.initialize();

        initializeOuplSchools();

        loadNewArrivalStudents();

        m_helperSched = new StudentHistoryHelper(this);
        m_helperSched.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_helperSched.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helperSched.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
        applyInputCriteria(m_helperSched.getStudentCriteria(), true, null);

        if (getSetupErrors().size() == 0) {
            setEntityClass(ILAssessmentEntity.class);

            // Assign custom field retriever calculations.
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveDateEntryUS.CALC_ID, new RetrieveDateEntryUS());
            calcs.put(RetrieveRcdts.CALC_ID, new RetrieveRcdts());
            calcs.put(RetrieveSchoolYear.CALC_ID, new RetrieveSchoolYear());
            calcs.put(RetrieveSessionName.CALC_ID, new RetrieveSessionName());
            super.addCalcs(calcs);
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getAsmDefinitionId()
     */
    @Override
    protected String getAsmDefinitionId() {
        return CODE_ASSESSMENT_DEFINITION_ID_IL;
    }

    /**
     * @see com.x2dev.procedures.statereporting.AssessmentExportData#getNoStudentAssessmentMessage()
     */
    @Override
    protected String getNoStudentAssessmentMessage() {
        return ERROR_MESSAGE_NO_STUDENT_ASSESSMENTS;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldDateEntryUS = translateAliasToJavaName(ALIAS_FIRST_YEAR_IN_US, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_birthCountry = translateAliasToJavaName(ALIAS_BIRTH_COUNTRY_CODE, true);
        m_stdTestSchool = translateAliasToJavaName(ALIAS_STD_TEST_SCHOOL, true);
    }

    /**
     * Initialize secondary OUTPLACEMENT school map.
     */
    private void initializeOuplSchools() {
        X2Criteria secondaryOutplacementCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                "OUTPLACEMENT");
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());
        secondaryOutplacementCriteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_endDate);
        X2Criteria sskEndDate = new X2Criteria();
        sskEndDate.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, m_startDate);
        X2Criteria sskEndDateNull = new X2Criteria();
        sskEndDateNull.addEmpty(StudentSchool.COL_END_DATE, getBroker().getPersistenceKey());
        sskEndDate.addOrCriteria(sskEndDateNull);
        secondaryOutplacementCriteria.addAndCriteria(sskEndDate);

        QueryByCriteria secondaryOutplacementQuery =
                new QueryByCriteria(StudentSchool.class, secondaryOutplacementCriteria);
        QueryIterator iter = getBroker().getIteratorByQuery(secondaryOutplacementQuery);
        try {
            while (iter.hasNext()) {
                StudentSchool item = (StudentSchool) iter.next();
                m_secondaryOutplacementSchoolMap.put(item.getStudentOid(),
                        (String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool));
            }
        } finally {
            iter.close();
        }
    }

    /**
     * Loads students who birth country isn't US and date entry US is on or after May 1 of the
     * previous school year.
     */
    private void loadNewArrivalStudents() {
        X2Criteria studentCriteria = new X2Criteria();
        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }

        studentCriteria.addNotEmpty(SisStudent.REL_PERSON + PATH_DELIMITER + m_birthCountry,
                getBroker().getPersistenceKey());
        studentCriteria.addNotEqualTo(SisStudent.REL_PERSON + PATH_DELIMITER + m_birthCountry, CODE_US);

        // May 1 of the previous school year
        Calendar cornerDate = Calendar.getInstance();
        cornerDate.setTime(getOrganization().getCurrentContext().getStartDate());
        cornerDate.set(Calendar.MONTH, Calendar.MAY);
        cornerDate.set(Calendar.DAY_OF_MONTH, 1);
        studentCriteria.addGreaterOrEqualThan(m_fieldDateEntryUS, new PlainDate(cornerDate.getTime()));

        SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        m_newArrivalToUS = getBroker().getSubQueryCollectionByQuery(studentQuery);
    }
}
