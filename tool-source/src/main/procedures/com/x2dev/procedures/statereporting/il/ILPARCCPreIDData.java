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
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class ILPARCCPreIDData.
 */
public class ILPARCCPreIDData extends StateReportData {

    /**
     * The Class ILPARCCPreIDDataEntity.
     */
    public static class ILPARCCPreIDDataEntity extends StateReportEntity {
        /**
         * ILPARCCPreIDData data.
         */
        protected ILPARCCPreIDData m_ilData = null;

        /**
         * Map of the student's RCDTS information.
         */
        protected Map<String, String> m_rcdtsMap = null;

        /**
         * The effective Entry student enrollment record for report date.
         */
        private StudentEnrollment m_enrollment = null;


        /**
         * Student's courses.
         */
        private List<StudentScheduleSpan> m_studentCourses = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ILPARCCPreIDDataEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current schedule span.
         *
         * @return Student schedule span
         */
        public StudentScheduleSpan getCurrentScheduleSpan() {
            return m_studentCourses.get(getCurrentRow());
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;

            StudentScheduleSpan span = getCurrentScheduleSpan();
            if (span != null) {
                SisSchool spanSchool = span.getSection().getSchedule().getSchool();
                school = (String) spanSchool.getFieldValueByBeanPath(m_ilData.m_fieldSchoolCode);
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
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_ilData = (ILPARCCPreIDData) data;
            SisStudent student = (SisStudent) getBean();

            m_enrollment = m_ilData.m_helper.getEnrollmentForDate(student.getOid(),
                    m_ilData.m_reportDate, "E");
            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_ilData.m_excludeSklField))) {
                m_studentCourses = m_ilData.m_helperSched.getStudentScheduleSpans(student);

                m_rcdtsMap = lookupOverrides();
                setRowCount(m_studentCourses.size());
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
            ILPARCCPreIDData ppData = (ILPARCCPreIDData) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (ppData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = ppData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            ppData.lookupStateValue(StudentEnrollment.class, ppData.m_fieldServiceSchoolCode,
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
            ILPARCCPreIDDataEntity ppEntity = (ILPARCCPreIDDataEntity) entity;
            String rcdts = null;
            if (param.equals("H") && ppEntity.getEffectiveEnrollment() != null
                    && ppEntity.getEffectiveEnrollment().getSchool() != null) {
                ILPARCCPreIDData ppData = (ILPARCCPreIDData) data;
                rcdts = (String) ppEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(ppData.m_fieldSchoolCode);
            } else if (param.equals("S")) {
                rcdts = ppEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = ppEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /**
     * Returns a schedule's section information.
     */
    protected class RetrieveSection implements FieldRetriever {
        private final String PARAM_SECTION_NUMBER = "SECTION_NUMBER";
        private final String PARAM_TERM = "TERM";

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
            String parameter = (String) field.getParameter();
            StudentScheduleSpan span = ((ILPARCCPreIDDataEntity) entity).getCurrentScheduleSpan();

            if (PARAM_SECTION_NUMBER.equals(parameter)) {
                value = span.getSection().getSectionNumber();
            } else if (PARAM_TERM.equals(parameter)) {
                String termView = span.getSection().getTermView();
                value = data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termView,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return value;
        }

    }

    /**
     * Returns course information associated with this section.
     */
    protected class RetrieveCourseInfo implements FieldRetriever {
        private final String PARAM_STATE_COURSE_CODE = "STATE_COURSE_CODE";
        private final String PARAM_LOCAL_COURSE_ID = "LOCAL_COURSE_ID";
        private final String PARAM_LOCAL_COURSE_TITLE = "LOCAL_COURSE_TITLE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String parameter = (String) field.getParameter();
            StudentScheduleSpan span = ((ILPARCCPreIDDataEntity) entity).getCurrentScheduleSpan();
            SchoolCourse sklCourse = span.getSection().getSchoolCourse();

            if (PARAM_STATE_COURSE_CODE.equals(parameter)) {
                value = span.getSection().getSchoolCourse().getCourse().getFieldValueByAlias(ALIAS_STATE_COURSE_ID);
            } else if (PARAM_LOCAL_COURSE_ID.equals(parameter)) {
                value = sklCourse.getCourse().getNumber();
            } else if (PARAM_LOCAL_COURSE_TITLE.equals(parameter)) {
                value = sklCourse.getCourse().getDescription();
            }

            return value;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_STATE_COURSE_ID = "DOE STATE COURSE ID";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";

    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected String m_excludeSklField;
    protected String m_excludeStdField;
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
        setEntityClass(ILPARCCPreIDDataEntity.class);

        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        if (getSetupErrors().size() == 0) {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            applyInputCriteria(m_helper.getStudentCriteria(), true, null);

            m_helperSched = new StudentHistoryHelper(this);
            m_helperSched.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helperSched.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            applyInputCriteria(m_helperSched.getStudentCriteria(), true, null);

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
            calcs.put("CRS-SECTION", new RetrieveSection());
            calcs.put("CRS-COURSE", new RetrieveCourseInfo());
            super.addCalcs(calcs);
        }

    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
    }
}
