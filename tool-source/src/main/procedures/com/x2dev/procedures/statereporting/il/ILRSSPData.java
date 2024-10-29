/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ILRSSPData.
 */
public class ILRSSPData extends StateReportData {

    /**
     * The Class ILRSSPDataEntity.
     */
    public static class ILRSSPDataEntity extends StateReportEntity {
        /**
         * The effective Entry student enrollment record for report date.
         */
        private StudentEnrollment m_enrollment = null;

        /**
         * ILRSSPData data.
         */
        private ILRSSPData m_ilData = null;
        private List<StudentProgramParticipation> m_programs = null;
        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ILRSSPDataEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_ilData = (ILRSSPData) data;
            SisStudent student = (SisStudent) getBean();

            m_enrollment = m_ilData.m_helper.getEnrollmentForDate(student.getOid(),
                    m_ilData.m_reportDate, "E");
            m_programs = (List<StudentProgramParticipation>) student.getProgramParticipation();
            if ((m_programs != null && !m_programs.isEmpty()) &&
                    (m_enrollment != null && m_enrollment.getSchool() != null &&
                            !BooleanAsStringConverter.TRUE.equals(
                                    m_enrollment.getSchool().getFieldValueByBeanPath(m_ilData.m_excludeSklField)))) {
                setRowCount(m_programs.size());
                m_rcdtsMap = lookupOverrides();
            } else {
                setRowCount(0);
            }
        }

        /**
         * Gets the current program.
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getCurrentProgram() {
            return m_programs.get(getCurrentRow());
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if (m_enrollment != null && m_enrollment.getSchool() != null) {
                school = (String) m_enrollment.getSchool().getFieldValueByBeanPath(m_ilData.m_fieldSchoolCode);
            }

            return school;
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
        }

        /**
         * Lookup district and school codes, including overrides on the enrollment record.
         *
         * @return Map
         */
        private Map<String, String> lookupOverrides() {
            Map<String, String> calcValueMap = new HashMap<String, String>();
            ILRSSPData rsspData = (ILRSSPData) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (rsspData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode =
                        rsspData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            rsspData.lookupStateValue(StudentEnrollment.class, rsspData.m_fieldServiceSchoolCode,
                                    serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
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

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            ILRSSPDataEntity rsspEntity = (ILRSSPDataEntity) entity;
            String rcdts = null;
            if (param.equals("H") && rsspEntity.getEffectiveEnrollment() != null
                    && rsspEntity.getEffectiveEnrollment().getSchool() != null) {
                ILRSSPData rsspData = (ILRSSPData) data;
                rcdts = (String) rsspEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(rsspData.m_fieldSchoolCode);
            } else if (param.equals("S")) {
                rcdts = rsspEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = rsspEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve values from StudentProgramParticipations.
     */
    protected class RetrievePgm implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            ILRSSPDataEntity ilEntity = (ILRSSPDataEntity) entity;
            StudentProgramParticipation pgm = ilEntity.getCurrentProgram();
            try {
                value = WebUtils.getProperty(pgm, field.getBeanPath());
            } catch (X2BaseException x2be) {
                // Exception can happen in custom bean paths.
                String errorId = LocalizationCache.getMessages(getBroker().getPersistenceKey())
                        .getMessage("error.state.report.exception");
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity.getEntityName(),
                                getPropertyName(entity.getBean().getClass(),
                                        field.getBeanPath()),
                                errorId, x2be.getMessage()));
            }
            return value;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_STATE_COURSE_ID = "DOE STATE COURSE ID";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";

    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected String m_excludeSklField;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;

    /**
     * Helper class:
     * For student selection by enrollment.
     * For Student schedule span.
     */
    protected StudentHistoryHelper m_helper;
    protected StudentHistoryHelper m_helperSched;

    protected PlainDate m_reportDate;

    /**
     * A map of reference codes for RCDTS serving school codes, for use in RCDTS retriever
     */
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        setEntityClass(ILRSSPDataEntity.class);

        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        if (getSetupErrors().size() == 0) {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            applyInputCriteria(m_helper.getStudentCriteria(), true, null);

            setQuery(m_helper.getStudentQuery(false));

            // Additional rule for secondary OUTPLACEMENT school
            X2Criteria secondaryOutplacementCriteria = new X2Criteria();
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
            secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                    "OUTPLACEMENT");
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                    getOrganization().getCurrentContextOid());
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

            // Build a map of calculations/retrievers.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-RCDTS", new RetrieveRcdts());
            calcs.put("PGM", new RetrievePgm());
            super.addCalcs(calcs);
        }

    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);

        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);

        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
    }
}
