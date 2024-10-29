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
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
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
 * Illinois report data source for Elementary School Report Card Class data.
 *
 * @author X2 Development Corporation
 */
public class ILElSchoolReportCard extends StateReportData {
    /**
     * Entity class for Elementary School Report Card Class data.
     *
     * @author X2 Development Corporation
     */
    public static class ILElSchoolReportCardEntity extends StateReportEntity {

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
        }
    }

    /**
     * Constants: aliases, codes, fields, parameters
     */
    public static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    public static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    public static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    public static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    public static final String ALIAS_SCHOOL_NUMBER = "DOE SCHOOL ID";

    /**
     * Instance variables.
     */
    protected Map<String, SchoolCalendarDate> m_calendarIdDay;
    protected Map<String, Integer> m_classSizeMap = new HashMap<>();
    protected String m_excludeSklField;
    protected String m_fieldDistrictCode;
    protected String m_fieldExcludeMst;
    protected String m_fieldSchoolId;
    protected String m_fieldSchoolCode;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected Collection<MasterSchedule> m_sections;

    /**
     * Retrieve a ClassInfo value from the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveInfo implements FieldRetriever {

        private static final String PARAM_CLASS_SIZE = "CRS_SIZE";
        private static final String PARAM_DATE = "DATE";
        private static final String PARAM_DESCRIPTION = "DESCRIPTION";
        private static final String PARAM_PRIM_STAFF = "PRIM_NAME";
        private static final String PARAM_COURSE = "COURSE";
        private static final String PARAM_SCHOOL = "SCHOOL";

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
            MasterSchedule section = (MasterSchedule) entity.getBean();
            if (PARAM_DATE.equals(param)) {
                value = m_reportDate;
            } else if (PARAM_COURSE.equals(param)) {
                value = section.getCourseView();
            } else if (PARAM_SCHOOL.equals(param)) {
                value = section.getSchedule().getSchool().getFieldValueByBeanPath(m_fieldSchoolId);
            } else if (PARAM_PRIM_STAFF.equals(param)) {
                value = section.getPrimaryStaff() != null ? section.getPrimaryStaff().getNameView() : null;
            } else if (PARAM_CLASS_SIZE.equals(param)) {
                if (m_classSizeMap.containsKey(section.getOid())) {
                    value = String.valueOf(m_classSizeMap.get(section.getOid()).intValue());
                }
            } else if (PARAM_DESCRIPTION.equals(param)) {
                value = section.getDescription();
            }
            return value;
        }
    }

    /**
     * Initialize the export.
     * Prepare the StudentHistoryHelper and retrievers.
     */
    @Override
    public void initialize() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_NUMBER, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldExcludeMst = translateAliasToJavaName(ALIAS_EXCLUDE_MST, false);

        // Get user parameters.
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_NUMBER, true);

        // Prepare the StateReportData.
        setQuery(initializeQuery());
        setEntityClass(ILElSchoolReportCardEntity.class);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("EL-SCH-INFO", new RetrieveInfo());
        super.addCalcs(calcs);
    }

    /**
     * Lookup required scheduling information:
     * 1. Find first active date in May. get day number.
     * 2. Find schedule terms that meet on the day.
     */
    private QueryByCriteria initializeQuery() {
        Calendar calendar = Calendar.getInstance(OrganizationManager.getTimeZone(getOrganization()));
        calendar.setTime(getOrganization().getCurrentContext().getEndDate());
        calendar.set(Calendar.MONTH, Calendar.MAY);
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        PlainDate may1 = new PlainDate(calendar.getTime());

        // 1. Lookup calendar date(s) for first in session day in May. Map by calendar ID.
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                getSchool().getOid());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                + SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, may1);
        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        query.addOrderBy(SchoolCalendarDate.COL_DATE, true);

        SchoolCalendarDate cas = (SchoolCalendarDate) getBroker().getBeanByQuery(query);
        if (cas != null) {
            m_reportDate = cas.getDate();
        }
        // 2. Find schedule terms that meet around the day.
        criteria = new X2Criteria();
        criteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, ((SisSchool) getSchool()).getActiveScheduleOid());
        criteria.addGreaterOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER
                + ScheduleTermDate.COL_END_DATE, may1);
        criteria.addLessOrEqualThan(ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER
                + ScheduleTermDate.COL_START_DATE, may1);
        SubQuery termQuery = new SubQuery(ScheduleTerm.class, X2BaseBean.COL_OID, criteria);
        List<String> termOids = (List<String>) getBroker().getSubQueryCollectionByQuery(termQuery);

        Collection<String> localCodes = new ArrayList<>();
        ModelProperty departmentProp =
                new ModelProperty(Course.class, Course.COL_DEPARTMENT_CODE, getBroker().getPersistenceKey());
        DataDictionaryField ddDepartment = getDataDictionary().findDataDictionaryField(departmentProp.getFieldId());
        if (ddDepartment != null && ddDepartment.hasReferenceTable()) {
            Collection<ReferenceCode> departmentCodes = ddDepartment.getReferenceTable().getReferenceCodes();

            if (!departmentCodes.isEmpty()) {
                for (ReferenceCode rcd : departmentCodes) {
                    if ("HOMEROOM".equals(rcd.getStateCode())) {
                        localCodes.add(rcd.getCode());
                    }
                }
            }
        }

        criteria = new X2Criteria();

        // Filter for terms that meet on the specified day.
        criteria.addIn(MasterSchedule.REL_MASTER_TERMS + PATH_DELIMITER + MasterTerm.COL_SCHEDULE_TERM_OID,
                termOids);
        if (m_fieldExcludeMst != null) {
            criteria.addNotEqualTo(m_fieldExcludeMst, BooleanAsStringConverter.TRUE);
        }
        // Filter for core courses, by department.
        if (!localCodes.isEmpty()) {
            criteria.addIn(
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                            + Course.COL_DEPARTMENT_CODE,
                    localCodes);
        } else {
            criteria.addEqualTo(
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                            + Course.COL_DEPARTMENT_CODE,
                    "___dummy___");
        }
        calculateClassSize(criteria);

        return new QueryByCriteria(MasterSchedule.class, criteria);
    }

    /**
     * Calculates number of students for section As of Date
     */
    private void calculateClassSize(Criteria mstCriteria) {
        // Prepare the StudentHistoryHelper.
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        X2Criteria sscAppend = new X2Criteria();
        X2Criteria sccAppend = new X2Criteria();
        X2Criteria additional = new X2Criteria();
        SubQuery mstSubQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, mstCriteria);
        Collection<String> mstOids = getBroker().getSubQueryCollectionByQuery(mstSubQuery);

        additional.addIn(StudentSchedule.COL_SECTION_OID, mstSubQuery);
        sscAppend.addOrCriteria(additional);
        additional = new X2Criteria();
        additional.addIn(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, mstSubQuery);
        sccAppend.addOrCriteria(additional);
        X2Criteria studentScheduleCriteria = m_helper.getStudentScheduleCriteria();
        X2Criteria studentScheduleChangeCriteria = m_helper.getStudentScheduleChangeCriteria();
        studentScheduleCriteria.addAndCriteria(sscAppend);
        studentScheduleChangeCriteria.addAndCriteria(sccAppend);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(m_helper.getStudentQuery(false));
        for (SisStudent student : students) {
            Collection<StudentScheduleSpan> spans = m_helper.getStudentScheduleSpans(student);
            for (StudentScheduleSpan span : spans) {
                if (mstOids.contains(span.getSection().getOid()) && !m_reportDate.before(span.getEntryDate())
                        && !m_reportDate.after(span.getExitDate())) {
                    Integer count = m_classSizeMap.get(span.getSection().getOid());
                    if (count == null) {
                        count = Integer.valueOf(1);
                    } else {
                        count = Integer.valueOf(count.intValue() + 1);
                    }
                    m_classSizeMap.put(span.getSection().getOid(), count);
                }
            }
        }
    }
}
