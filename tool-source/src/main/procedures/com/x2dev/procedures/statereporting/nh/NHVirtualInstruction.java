/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2021 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */


package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import org.apache.ojb.broker.query.QueryByCriteria;


public class NHVirtualInstruction extends StateReportData {

    public static class NHVirtualInstructionEntity extends StateReportEntity {

        private static final List<String> includedEnrStatusCodes = Arrays.asList("1", "2", "8", "11", "12");

        private NHVirtualInstruction m_data;
        private Student m_student;
        private List<String> m_schoolIds;
        private List<String> m_secondarySchoolIds;
        private List<Integer> m_percentages;
        private Map<String, List<PlainDate>> m_insessionDays;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NHVirtualInstructionEntity() {
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

            Student student = (Student) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";
            return name;
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
         * Gets the percentage.
         *
         * @return Credits Earned
         */
        public String getSchoolId() {
            return m_schoolIds.get(getCurrentRow());
        }

        /**
         * Gets the percentage.
         *
         * @return Credits Earned
         */
        public Integer getPercentage() {
            return m_percentages.get(getCurrentRow());
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

            m_data = (NHVirtualInstruction) getData();
            m_student = (Student) bean;
            m_schoolIds = new ArrayList<String>();
            m_secondarySchoolIds = new ArrayList<String>();
            m_percentages = new ArrayList<Integer>();
            m_insessionDays = new HashMap<String, List<PlainDate>>();

            X2Criteria sskCriteria = new X2Criteria();
            sskCriteria.addEqualTo(StudentSchool.COL_STUDENT_OID, m_student.getOid());
            sskCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, m_data.getCurrentContext().getOid());

            String[] columns = new String[] {StudentSchool.REL_SCHOOL + PATH_DELIMITER + m_data.m_fieldSklId};
            ColumnQuery sskQuery = new ColumnQuery(StudentSchool.class, columns, sskCriteria);
            ReportQueryIterator queryItr = m_data.getBroker().getReportQueryIteratorByQuery(sskQuery);
            try {
                while (queryItr.hasNext()) {
                    Object[] row = (Object[]) queryItr.next();
                    String sklId = (String) row[0];
                    m_secondarySchoolIds.add(sklId);
                }
            } finally {
                queryItr.close();
            }

            List<StudentEnrollmentSpan> spans =
                    new ArrayList(m_data.m_helper.getStudentEnrollmentSpans(m_student, true));

            if (spans.isEmpty()) {
                setRowCount(0);
                return;
            }

            StudentEnrollmentSpan mostRecentSpan = null;
            boolean stdHasValidEnrStatusCode = false;
            for (StudentEnrollmentSpan span : spans) {
                if (span.getFirstActiveEnrollment() != null) {
                    String enrStatusStateCode = m_data.lookupStateValue(StudentEnrollment.class, m_data.m_fieldEnrCode,
                            (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_data.m_fieldEnrCode));

                    if (includedEnrStatusCodes.contains(enrStatusStateCode)) {
                        stdHasValidEnrStatusCode = true;
                    }

                    if (mostRecentSpan == null) {
                        mostRecentSpan = span;
                    } else if (span.getFirstActiveEnrollment().getEnrollmentDate()
                            .after(mostRecentSpan.getFirstActiveEnrollment().getEnrollmentDate())) {
                        mostRecentSpan = span;
                    }
                    addMemberDays(span);
                }
            }

            String mostRecentEnrStatusStateCode =
                    m_data.lookupStateValue(StudentEnrollment.class, m_data.m_fieldEnrCode,
                            (String) mostRecentSpan.getFirstActiveEnrollment()
                                    .getFieldValueByBeanPath(m_data.m_fieldEnrCode));

            if (!m_insessionDays.isEmpty() && (includedEnrStatusCodes
                    .contains(mostRecentEnrStatusStateCode)
                    || (!includedEnrStatusCodes.contains(mostRecentEnrStatusStateCode) && stdHasValidEnrStatusCode))) {
                X2Criteria attCriteria = new X2Criteria();
                attCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, m_student.getOid());
                attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE,
                        m_data.getCurrentContext().getStartDate());
                attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_data.m_reportDate);

                QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, attCriteria);
                Map<String, Collection<StudentAttendance>> attendanceMap =
                        m_data.getBroker().getGroupedCollectionByQuery(query,
                                StudentAttendance.REL_SCHOOL + PATH_DELIMITER + m_data.m_fieldSklId,
                                m_data.getBroker().getCount(query));

                int rowCount = 0;
                for (String sklId : attendanceMap.keySet()) {
                    if (m_secondarySchoolIds.contains(sklId)) {
                        continue;
                    }
                    m_schoolIds.add(sklId);

                    int virtAttCount = 0;
                    for (StudentAttendance att : attendanceMap.get(sklId)) {
                        if (isVirtual(att)) {
                            virtAttCount++;
                        }
                    }
                    int membershipDays = 0;
                    if (m_insessionDays.get(sklId) != null) {
                        membershipDays = m_insessionDays.get(sklId).size();
                    } else {
                        rowCount = 0;
                        break;
                    }

                    Double decimalPercentage = new Double(virtAttCount) / new Double(membershipDays);
                    Integer virtPerc = new Integer((int) (Math.round(decimalPercentage * 100)));
                    m_percentages.add(virtPerc);
                    rowCount++;
                }
                setRowCount(rowCount);
            } else {
                setRowCount(0);
            }


        }

        /**
         * Adds the member days.
         *
         * @param span StudentEnrollmentSpan
         */
        private void addMemberDays(StudentEnrollmentSpan span) {
            Set<PlainDate> insessionDates =
                    m_data.m_helper.getCalendarDays(span.getSchool(), m_student.getCalendarCode());
            if (insessionDates == null
                    && !StudentEnrollmentSpan.DEFAULT_CALENDAR_ID.equals(m_student.getCalendarCode())) {
                insessionDates = m_data.m_helper.getCalendarDays(span.getSchool(),
                        StudentEnrollmentSpan.DEFAULT_CALENDAR_ID);
            }
            if (insessionDates == null) {
                insessionDates =
                        m_data.m_helper.getCalendarDays(span.getSchool(), StudentHistoryHelper.CALENDAR_ANY);
            }

            // Count in session days between (and including) first and last active dates.
            PlainDate endDate = span.getLastActiveDate();
            if (endDate == null) {
                endDate = m_data.m_reportDate;
            }
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (span.getFirstActiveDate() != null && !date.before(span.getFirstActiveDate())
                            && !date.after(endDate)) {
                        String sklId = (String) span.getSchool().getFieldValueByBeanPath(m_data.m_fieldSklId);
                        List<PlainDate> dates = m_insessionDays.get(sklId);
                        if (dates == null) {
                            dates = new ArrayList<PlainDate>();
                        }
                        dates.add(date);
                        m_insessionDays.put(sklId, dates);
                    }
                }
            }
        }

        /**
         * Determine if attendance record has a virtual code.
         *
         * @param attendance StudentAttendance
         * @return True is either other code 1 or 2 are flagged as isVirtual
         */
        private boolean isVirtual(StudentAttendance attendance) {
            String other1 = attendance.getOtherCode();
            String other2 = attendance.getOtherCode02();
            String isVirtual = null;

            if (!StringUtils.isEmpty(other1)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other1);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.m_fieldRatIsVirtual);
                }
            }

            if (!StringUtils.isEmpty(other2) && StringUtils.isEmpty(isVirtual)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other1);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.m_fieldRatIsVirtual);
                }
            }

            return BooleanAsStringConverter.TRUE.equals(isVirtual);
        }

    }

    /**
     * Returns the virtual percentage.
     */
    protected class RetrieveSchoolId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NHVirtualInstructionEntity vEntity = (NHVirtualInstructionEntity) entity;
            return vEntity.getSchoolId();
        }
    }

    /**
     * Returns the virtual percentage.
     */
    protected class RetrievePercentage implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NHVirtualInstructionEntity vEntity = (NHVirtualInstructionEntity) entity;
            return vEntity.getPercentage();
        }
    }

    /**
     * Retriever Parameters
     */
    private static final String CALC_ID_VIRTUAL_PERCENTAGE = "VIRTUAL-PERCENTAGE";
    private static final String CALC_ID_SCHOOL_ID = "I4SEE050";

    private static final String ALIAS_SCHOOL_EXCLUDE = "i4see EXCLUDE SCHOOL";
    private static final String ALIAS_SCHOOL_ID = "i4see 050";
    private static final String ALIAS_ENR_CODE = "i4see 210";
    private static final String ALIAS_RAT_VIRTUAL_FLAG = "all-rat-VirtualFlag";

    /**
     * Supporting instance variables.
     */
    protected PlainDate m_reportDate;
    protected StudentHistoryHelper m_helper;

    protected Map<String, RefAttendanceStudent> m_refAttendanceMap = null;

    protected String m_fieldSklExclude;
    protected String m_fieldSklId;
    protected String m_fieldEnrCode;
    protected String m_fieldRatIsVirtual;

    /**
     * Initialize the data module.
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
            loadRefAttendanceMap();
            /*
             * Build query object that will be used to retrieve export students.
             */
            m_reportDate = new PlainDate(getTimeZone());
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);

            if (isSchoolContext()) {
                m_helper.getStudentCriteria().addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                m_helper.getStudentCriteria().addNotEqualTo(
                        Student.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                m_helper.getStudentCriteria().addNotEqualTo(
                        Student.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }
            m_helper.getStudentCriteria().addNotEqualTo(
                    Student.REL_SCHOOL + PATH_DELIMITER + m_fieldSklExclude, BooleanAsStringConverter.TRUE);

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(NHVirtualInstructionEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_VIRTUAL_PERCENTAGE, new RetrievePercentage());
            calcs.put(CALC_ID_SCHOOL_ID, new RetrieveSchoolId());
            super.addCalcs(calcs);
        }
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSklExclude = translateAliasToJavaName(ALIAS_SCHOOL_EXCLUDE, true);
        m_fieldSklId = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldEnrCode = translateAliasToJavaName(ALIAS_ENR_CODE, true);
        m_fieldRatIsVirtual = translateAliasToJavaName(ALIAS_RAT_VIRTUAL_FLAG, true);
    }

    /**
     * Initialize a map of Daily Attendance Reference Codes.
     */
    protected void loadRefAttendanceMap() {
        DataDictionaryField isVirtualField =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_RAT_VIRTUAL_FLAG);
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE, RefAttendanceStudent.ATTENDANCE_TYPE_DAILY);
        criteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE, RefAttendanceStudent.TYPE_OTHER_CODE);
        if (isVirtualField != null) {
            criteria.addEqualTo(isVirtualField.getJavaName(), BooleanAsStringConverter.TRUE);
        }

        BeanQuery query = new BeanQuery(RefAttendanceStudent.class, criteria);

        m_refAttendanceMap = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 8);
    }

    /**
     * Returns the correct time zone for this tool.
     *
     * @return TimeZone
     */
    @Override
    public TimeZone getTimeZone() {
        return OrganizationManager.getTimeZone(getOrganization());
    }

}
