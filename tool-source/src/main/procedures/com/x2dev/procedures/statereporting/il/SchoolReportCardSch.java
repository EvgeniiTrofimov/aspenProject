/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois report data source for School Report Cards schedule data.
 *
 * @author X2 Development Corporation
 */
public class SchoolReportCardSch extends StateReportData {
    /**
     * Entity class for School Report Cars schedule data.
     *
     * @author X2 Development Corporation
     */
    public static class SchoolReportCardSchEntity extends StateReportEntity {
        /**
         * Map of calculated values that are accessible to the retrievers.
         */
        private List<ClassInfo> m_classList = new ArrayList<ClassInfo>();

        /**
         * Returns a calculated value from the values map by key.
         *
         * @return Double
         */
        public ClassInfo getClassInfo() {
            return m_classList.get(getCurrentRow());
        }

        /**
         * Returns current school (school of schedule span).
         *
         * @return String
         */
        public String getCurrentSchool() {
            String currentSchoolId = null;

            MasterSchedule section = getClassInfo().m_section;
            if (section != null && section.getSchoolCourse() != null && section.getSchoolCourse().getSchool() != null) {
                currentSchoolId =
                        (String) section.getSchoolCourse().getSchool()
                                .getFieldValueByBeanPath(m_srcData.m_fieldSchoolCode);
            }

            return currentSchoolId;
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
         * Student's enrollment
         */
        StudentEnrollment m_enrollment = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * SchoolReportCardSch data.
         */
        SchoolReportCardSch m_srcData = null;

        /**
         * Initialize student, calculate membership and attendance counts.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_srcData = (SchoolReportCardSch) data;
            SisStudent student = (SisStudent) bean;
            String calendarId = student.getCalendarCode();
            if (!m_srcData.m_calendarIdDay.keySet().contains(calendarId)) {
                calendarId = m_srcData.m_calendarIdDay.keySet().iterator().next();
            }

            List<StudentScheduleSpan> spans = m_srcData.m_helper.getStudentScheduleSpans(student);
            List<String> period2Sections = m_srcData.m_period2Sections.get(calendarId);
            List<String> period5Sections = m_srcData.m_period5Sections.get(calendarId);
            SchoolCalendarDate reportDate = m_srcData.m_calendarIdDay.get(calendarId);
            m_enrollment = m_srcData.m_helper.getEnrollmentForDate(student.getOid(), reportDate.getDate(), "E");
            m_rcdtsMap = lookupOverrides();

            if (m_enrollment != null
                    && m_enrollment.getSchool() != null
                    && !BooleanAsStringConverter.TRUE.equals(m_enrollment.getSchool().getFieldValueByBeanPath(
                            m_srcData.m_excludeSklField))) {
                for (StudentScheduleSpan span : spans) {
                    if (span.getEntryDate() != null && !reportDate.getDate().before(span.getEntryDate()) &&
                            span.getExitDate() != null && !reportDate.getDate().after(span.getExitDate())) {
                        if (period2Sections != null && period2Sections.contains(span.getSection().getOid())) {
                            ClassInfo info = new ClassInfo();
                            info.m_date = reportDate.getDate();
                            info.m_period = "2";
                            info.m_section = span.getSection();
                            m_classList.add(info);
                        }
                        if (period5Sections != null && period5Sections.contains(span.getSection().getOid())) {
                            ClassInfo info = new ClassInfo();
                            info.m_date = reportDate.getDate();
                            info.m_period = "5";
                            info.m_section = span.getSection();
                            m_classList.add(info);
                        }
                    }
                }
                setRowCount(m_classList.size());
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
            SchoolReportCardSch srcs = (SchoolReportCardSch) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (m_enrollment != null
                    && srcs.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = srcs.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            srcs.lookupStateValue(StudentEnrollment.class, srcs.m_fieldServiceSchoolCode,
                                    serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
        }
    }

    /**
     * The Class ClassInfo.
     */
    /*
     * Inner class for holding information for a class for a student.
     */
    protected static class ClassInfo {
        public MasterSchedule m_section;
        public PlainDate m_date;
        public String m_period;
    }

    /**
     * Constants: aliases, codes, fields, parameters
     */
    public static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    public static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    public static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    public static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    public static final String ALIAS_SCHOOL_NUMBER = "DOE SCHOOL ID";
    public static final String ALIAS_SCHOOL_REP_CARD_PERIOD = "DOE SCHOOL REPORT CARD PERIOD";
    public static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";

    /**
     * Instance variables.
     */
    protected Map<String, SchoolCalendarDate> m_calendarIdDay;
    protected String m_excludeSklField;
    protected String m_fieldSchoolId;
    protected String m_fieldDistrictCode;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected StudentHistoryHelper m_helper;
    protected Map<String, List<String>> m_period2Sections;
    protected Map<String, List<String>> m_period5Sections;
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();

    /**
     * Retrieve a ClassInfo value from the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveInfo implements FieldRetriever {
        private static final String PARAM_DATE = "DATE";
        private static final String PARAM_PERIOD = "PERIOD";
        private static final String PARAM_CLASS = "CLASS";
        private static final String PARAM_SCHOOL = "SCHOOL";
        private static final String PARAM_DEPARTMENT = "DEPARTMENT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            SchoolReportCardSchEntity srcEntity = (SchoolReportCardSchEntity) entity;
            ClassInfo info = srcEntity.getClassInfo();
            if (PARAM_DATE.equals(param)) {
                value = info.m_date;
            } else if (PARAM_PERIOD.equals(param)) {
                value = info.m_period;
            } else if (PARAM_CLASS.equals(param)) {
                value = info.m_section.getCourseView();
            } else if (PARAM_DEPARTMENT.equals(param)) {
                String localCode = info.m_section.getSchoolCourse().getCourse().getDepartmentCode();
                if (!StringUtils.isEmpty(localCode)) {
                    String stateCode = data.lookupStateValue(Course.class, Course.COL_DEPARTMENT_CODE, localCode);
                    if (!StringUtils.isEmpty(stateCode)) {
                        value = stateCode;
                    }
                }
            } else if (PARAM_SCHOOL.equals(param)) {
                value = info.m_section.getSchedule().getSchool().getFieldValueByBeanPath(m_fieldSchoolId);
            }
            return value;
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
    protected class RetrieveRCDTS implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            SchoolReportCardSchEntity srcsEntity = (SchoolReportCardSchEntity) entity;
            String rcdts = null;
            if (param.equals("H") && srcsEntity.getEffectiveEnrollment() != null
                    && srcsEntity.getEffectiveEnrollment().getSchool() != null) {
                SchoolReportCardSch srcsData = (SchoolReportCardSch) data;
                rcdts =
                        (String) srcsEntity.getEffectiveEnrollment().getSchool()
                                .getFieldValueByBeanPath(srcsData.m_fieldSchoolCode);
            } else if (param.equals("S")) {
                rcdts = srcsEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = srcsEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /**
     * Initialize the export.
     * Prepare the StudentHistoryHelper and retrievers.
     */
    @Override
    public void initialize() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_NUMBER, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);

        // Get user parameters.
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_NUMBER, true);

        lookupScheduleInfo();

        // Prepare the StudentHistoryHelper.
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);

        X2Criteria sscAppend = new X2Criteria();
        X2Criteria sccAppend = new X2Criteria();
        for (String calendarId : m_calendarIdDay.keySet()) {
            List<String> period2Sections = m_period2Sections.get(calendarId);
            List<String> period5Sections = m_period5Sections.get(calendarId);
            if (period2Sections != null && period2Sections.size() > 0) {
                X2Criteria additional = new X2Criteria();
                additional.addIn(StudentSchedule.COL_SECTION_OID, period2Sections);
                sscAppend.addOrCriteria(additional);
                additional = new X2Criteria();
                additional.addIn(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, period2Sections);
                sccAppend.addOrCriteria(additional);
            }
            if (period5Sections != null && period5Sections.size() > 0) {
                X2Criteria additional = new X2Criteria();
                additional.addIn(StudentSchedule.COL_SECTION_OID, period5Sections);
                sscAppend.addOrCriteria(additional);
                additional = new X2Criteria();
                additional.addIn(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, period5Sections);
                sccAppend.addOrCriteria(additional);
            }
        }
        X2Criteria studentScheduleCriteria = m_helper.getStudentScheduleCriteria();
        X2Criteria studentScheduleChangeCriteria = m_helper.getStudentScheduleChangeCriteria();
        studentScheduleCriteria.addAndCriteria(sscAppend);
        studentScheduleChangeCriteria.addAndCriteria(sccAppend);

        // Additional rule for secondary OUTPLACEMENT school
        Criteria secondaryOutplacementCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                "OUTPLACEMENT");
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getOrganization()
                .getCurrentContextOid());
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

        // Prepare the StateReportData.
        setQuery(m_helper.getStudentQuery(false));
        setEntityClass(SchoolReportCardSchEntity.class);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("SRC-SCH-INFO", new RetrieveInfo());
        calcs.put("SRC-SCH-RCDTS", new RetrieveRCDTS());
        super.addCalcs(calcs);
    }

    /**
     * Lookup required scheduling information:
     * 1. Find first active date in May. get day number.
     * 2. Find schedule terms that meet on the day.
     * 3. Class sections that meet on period 2 or 5 of that day and term.
     */
    private void lookupScheduleInfo() {
        Calendar calendar = Calendar.getInstance(OrganizationManager.getTimeZone(getOrganization()));
        calendar.setTime(getOrganization().getCurrentContext().getEndDate());
        calendar.set(Calendar.MONTH, Calendar.MAY);
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        PlainDate may1 = new PlainDate(calendar.getTime());

        // 1. Lookup calendar date(s) for first in session day in May. Map by calendar ID.
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                getSchool().getOid());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, may1);
        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        query.addOrderBy(SchoolCalendarDate.COL_DATE, false);
        m_calendarIdDay =
                getBroker().getMapByQuery(query,
                        SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID, 5);

        // 2. Find schedule terms that meet around the day.
        criteria = new Criteria();
        criteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
        criteria.addGreaterOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER
                + ScheduleTermDate.COL_END_DATE, may1);
        criteria.addLessOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER
                + ScheduleTermDate.COL_START_DATE, may1);
        SubQuery termQuery = new SubQuery(ScheduleTerm.class, X2BaseBean.COL_OID, criteria);
        List<String> termOids = (List<String>) getBroker().getSubQueryCollectionByQuery(termQuery);

        // For each calendar ID, find the sections that meet on that day and period.
        m_period2Sections = new HashMap<String, List<String>>();
        m_period5Sections = new HashMap<String, List<String>>();

        Collection<String> period2Codes = new ArrayList<>();
        Collection<String> period5Codes = new ArrayList<>();

        DataDictionaryField fieldRepCardPeriod =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_SCHOOL_REP_CARD_PERIOD);
        ModelProperty prop =
                new ModelProperty(Course.class, Course.COL_DEPARTMENT_CODE, getBroker().getPersistenceKey());

        DataDictionaryField fieldDepartment =
                getDataDictionary().findDataDictionaryField(prop.getFieldId());

        if (fieldRepCardPeriod != null && fieldRepCardPeriod.hasReferenceTable() && fieldDepartment != null
                && fieldDepartment.hasReferenceTable()) {
            String beanPathRepCardPeriod = fieldRepCardPeriod.getJavaName();
            Collection<ReferenceCode> refCodesPeriod = fieldRepCardPeriod.getReferenceTable().getReferenceCodes();
            if (refCodesPeriod != null && !refCodesPeriod.isEmpty()) {
                for (ReferenceCode rcd : refCodesPeriod) {
                    if ("2".equals(rcd.getStateCode())) {
                        period2Codes.add(rcd.getCode());
                    }
                    if ("5".equals(rcd.getStateCode())) {
                        period5Codes.add(rcd.getCode());
                    }
                }
            }

            for (String calendarId : m_calendarIdDay.keySet()) {
                SchoolCalendarDate calDate = m_calendarIdDay.get(calendarId);

                criteria = new Criteria();

                // Filter for terms that meet on the specified day.
                criteria.addIn(MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER + MasterTerm.COL_SCHEDULE_TERM_OID,
                        termOids);

                // Day filter for day of meeting, first day of may.
                criteria.addEqualTo(MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER + MasterTerm.REL_MASTER_MATRICES
                        + PATH_DELIMITER + MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                        + ScheduleMatrix.REL_SCHEDULE_DAY + PATH_DELIMITER + ScheduleDay.COL_NUMBER,
                        Integer.valueOf(calDate.getScheduleDayNumber()));

                if (!period2Codes.isEmpty()) {
                    criteria.addIn(beanPathRepCardPeriod, period2Codes);
                } else {
                    criteria.addEqualTo(beanPathRepCardPeriod, "___dummy___");
                }

                SubQuery subQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, criteria);
                List<String> period2Sections = (List<String>) getBroker().getSubQueryCollectionByQuery(subQuery);
                m_period2Sections.put(calendarId, period2Sections);

                criteria = new Criteria();

                // Filter for terms that meet on the specified day.
                criteria.addIn(MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER + MasterTerm.COL_SCHEDULE_TERM_OID,
                        termOids);

                // Day filter for day of meeting, first day of may.
                criteria.addEqualTo(MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER + MasterTerm.REL_MASTER_MATRICES
                        + PATH_DELIMITER + MasterScheduleMatrix.REL_SCHEDULE_MATRIX + PATH_DELIMITER
                        + ScheduleMatrix.REL_SCHEDULE_DAY + PATH_DELIMITER + ScheduleDay.COL_NUMBER,
                        Integer.valueOf(calDate.getScheduleDayNumber()));

                // Period filter for period 5
                if (!period5Codes.isEmpty()) {
                    criteria.addIn(beanPathRepCardPeriod, period5Codes);
                } else {
                    criteria.addEqualTo(beanPathRepCardPeriod, "___dummy___");
                }

                subQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, criteria);
                List<String> period5Sections = (List<String>) getBroker().getSubQueryCollectionByQuery(subQuery);
                m_period5Sections.put(calendarId, period5Sections);
            }
        }
    }
}
