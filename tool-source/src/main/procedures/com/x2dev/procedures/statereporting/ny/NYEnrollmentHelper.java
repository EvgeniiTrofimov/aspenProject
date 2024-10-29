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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentContextAttributes;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.types.PlainDate;
/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) Follett School Solutions
 * All rights reserved.
 *
 * ====================================================================
 */
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class NYEnrollmentHelper.
 */
public class NYEnrollmentHelper {

    /**
     * The Class NYStudentHistoryHelper.
     */
    public class NYStudentHistoryHelper extends StudentHistoryHelper {
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;
        private Map<String, List<StudentEnrollment>> m_studentEnrollmentMap;
        private Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars =
                new HashMap<String, Map<String, Set<PlainDate>>>();
        protected Map<String, StudentContextAttributes> m_studentAttributesMap;

        /**
         * Instantiates a new NY student history helper.
         *
         * @param data StateReportData
         */
        NYStudentHistoryHelper(StateReportData data) {
            super(data);
        }

        /**
         * Gets the organization.
         *
         * @return Organization
         */
        public Organization getOrganization() {
            return getData().getOrganization();
        }

        /**
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getCalendarDays(com.x2dev.sis.model.beans.SisSchool,
         *      java.lang.String)
         */
        @Override
        public Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            Map<String, Set<PlainDate>> calendarData = null;
            Set<PlainDate> calendarDates = null;
            Schedule schedule = null;
            if (school != null && !m_schoolsToCalendars.containsKey(school.getOid())) {
                PlainDate startDate = null;
                PlainDate endDate = null;
                if (school.getActiveSchedule() != null) {
                    schedule = school.getActiveSchedule();
                }
                if (schedule == null
                        || !schedule.getDistrictContextOid().equals(getData().getCurrentContext().getOid())) {
                    Collection<SchoolScheduleContext> schoolContexts =
                            school.getSchoolScheduleContexts(getData().getBroker());
                    for (SchoolScheduleContext schoolContext : schoolContexts) {
                        if (schoolContext.getDistrictContextOid().equals(getData().getCurrentContext().getOid())) {
                            schedule = schoolContext.getActiveSchedule();
                            break;
                        }
                    }
                }
                if (schedule != null) {
                    startDate = schedule.getStartDate();
                    endDate = schedule.getEndDate();
                } else {
                    startDate = getData().getCurrentContext().getStartDate();
                    endDate = getData().getCurrentContext().getEndDate();
                }
                calendarData = getEnrollmentManager().getCalendarLookup(school, startDate, endDate,
                        getData().getCurrentContext().getOid());
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            }

            if (school != null) {
                calendarData = m_schoolsToCalendars.get(school.getOid());
                // Get any calendar after checking the calendars map is not empty
                if (CALENDAR_ANY.equals(calendar) && !calendarData.isEmpty()) {
                    calendarDates = calendarData.values().iterator().next();
                } else {
                    calendarDates = calendarData.get(calendar);
                }
            }
            return calendarDates;
        }

        /**
         * Returns reference code of grade based on dates.
         *
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return Reference code
         */
        public ReferenceCode getGradeLevelByDates(SisStudent student, PlainDate startDate, PlainDate endDate) {
            if (m_referenceGradeCodeMap == null) {
                m_referenceGradeCodeMap = loadRefCodeMapByField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
            }
            List<NYStudentEnrollmentSpan> spans = getNYStudentEnrollmentSpans(student, true);
            Collections.sort(spans, new Comparator<NYStudentEnrollmentSpan>() {
                @Override
                public int compare(NYStudentEnrollmentSpan o1, NYStudentEnrollmentSpan o2) {
                    return o1.getFirstActiveEnrollment().getEnrollmentDate()
                            .compareTo(o2.getFirstActiveEnrollment().getEnrollmentDate());
                }
            });

            int yog = student.getYog();
            for (NYStudentEnrollmentSpan span : spans) {
                if (!span.getFirstActiveEnrollment().getEnrollmentDate().after(endDate) &&
                        (span.getFirstInactiveEnrollment() == null
                                || !span.getFirstInactiveEnrollment().getEnrollmentDate().before(startDate))) {
                    yog = span.getYog();
                }
            }

            ReferenceCode gradeCode = null;
            if (yog == student.getYog()) {
                String gradeLevel = student.getGradeLevel();
                gradeCode = m_referenceGradeCodeMap.get(gradeLevel);
            } else {
                ModelBroker broker = new ModelBroker(getData().getPrivilegeSet());
                TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
                int maxGradeLevel = StudentManager.getMaxGradeLevel(getData().getBroker());
                List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                        getData().getCurrentContext().getSchoolYear(), sortedGradeLevels);
                for (String matchingGradeLevel : matchingGradeLevels) {
                    gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                    if (gradeCode != null) {
                        break;
                    }
                }
            }
            return gradeCode;
        }

        /**
         * Returns a list of student enrollment spans that represent all of the
         * students enrollment activity and segments.
         * <p>
         * Spans are identified by one or more E enrollment records followed
         * by one or more consecutive W enrollment records and any records in between.
         * There will typically be only one E and one W record, however messy data may provide more.
         * <p>
         * If the properties PROPERTY_SPAN_BREAK_ON_YOG or PROPERTY_SPAN_BREAK_ON_STATUS
         * are set to TRUE, the presence of these enrollment records will also cause a break.
         * The STATUS or YOG record that caused the break will be included in BOTH enrollment spans
         * as the appropriate end or begin enrollment record. This may make looking up entry
         * enrollment
         * record values such as entry code or override school difficult if those values are not
         * also
         * in the YOG or STATUS record.
         *
         * @param student Student
         * @param limit - Limit spans to only those that overlap the reporting
         *        date range.
         * @return List<NYStudentEnrollmentSpan>
         */
        public List<NYStudentEnrollmentSpan> getNYStudentEnrollmentSpans(Student student, boolean limit) {
            PlainDate beginDate = (PlainDate) getNYStudentSelectionProperty(PROPERTY_BEGIN_DATE, PlainDate.class,
                    getData().getCurrentContext().getStartDate());
            PlainDate endDate =
                    (PlainDate) getNYStudentSelectionProperty(PROPERTY_END_DATE, PlainDate.class, m_currentDate);
            PlainDate waDate =
                    (PlainDate) getNYStudentSelectionProperty(PROPERTY_WITHDRAWN_AFTER_DATE, PlainDate.class, null);
            boolean yogBreak =
                    ((Boolean) getNYStudentSelectionProperty(PROPERTY_SPAN_BREAK_ON_YOG, Boolean.class, Boolean.FALSE))
                            .booleanValue();
            boolean statusBreak = ((Boolean) getNYStudentSelectionProperty(PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.class,
                    Boolean.FALSE)).booleanValue();
            if (waDate != null) {
                beginDate = waDate;
            }

            // Get the list of student enrollment records.
            List<StudentEnrollment> enrollments = getStudentEnrollments(student.getOid());

            // Determine starting status (current status). This should be based on the latest
            // enrollment record if possible, or the student otherwise.
            // isOpen indicates the current enrollment status is active.
            String enrollStatus = null;
            if (enrollments != null && enrollments.size() > 0) {
                StudentEnrollment currentEnrollment = enrollments.iterator().next();
                enrollStatus = currentEnrollment.getStatusCode();
                if (StringUtils.isEmpty(enrollStatus)) {
                    enrollStatus = student.getEnrollmentStatus();
                }
            } else {
                enrollStatus = student.getEnrollmentStatus();
            }
            // Current status of student.
            boolean isActive = m_preferenceStudentActiveStatus.equals(enrollStatus);
            // If the enrollment span has a non-withdrawal record in it so far.
            // Used to determine if another withdrawal signifies a break in the span.
            boolean hasNonWithdrawals = isActive;

            // Work through enrollment records going backward in time and build spans.
            // This will build all spans, regardless of the setting of limit.
            List<NYStudentEnrollmentSpan> enrollmentSpans = new ArrayList<NYStudentEnrollmentSpan>();

            List<StudentEnrollment> currentEnrollments = new ArrayList<StudentEnrollment>();
            if (enrollments != null) {
                /*
                 * Since we need to look ahead in the list of enrollments to combine Y and E records
                 * on the same date,
                 * we need to create an array list and navigate using an index.
                 */
                enrollments = new ArrayList(enrollments);
                for (int index = 0; index < enrollments.size(); ++index) {
                    StudentEnrollment enrollment = enrollments.get(index);
                    if (StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) {
                        // Only report a YOG as a break if the student is active or there are other
                        // records in the span already. Not for inactive students between spans.
                        isActive = m_preferenceStudentActiveStatus.equals(enrollment.getStatusCode());
                        if (yogBreak && (isActive || currentEnrollments.size() > 0)) {
                            // Complete the previous span. Start a new one.
                            currentEnrollments.add(0, enrollment);
                            enrollmentSpans.add(0, new NYStudentEnrollmentSpan(currentEnrollments, this,
                                    getStudentContextAttributes(enrollment.getStudentOid())));
                            /*
                             * check to see if next span should be combined. This will occur if the
                             * next span is an E
                             * record on the same date at the same school.
                             */
                            currentEnrollments = new ArrayList<StudentEnrollment>();
                        }
                        currentEnrollments.add(0, enrollment);
                        hasNonWithdrawals = true;
                    } else if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                        // Only report a STATUS as a break if the student is active or there are
                        // other
                        // records in the span already. Not for inactive students between spans.
                        isActive = m_preferenceStudentActiveStatus.equals(enrollment.getStatusCode());
                        if (statusBreak && (isActive || currentEnrollments.size() > 0)) {
                            // Complete the previous span. Start a new one.
                            currentEnrollments.add(0, enrollment);
                            enrollmentSpans.add(0, new NYStudentEnrollmentSpan(currentEnrollments, this,
                                    getStudentContextAttributes(enrollment.getStudentOid())));
                            currentEnrollments = new ArrayList<StudentEnrollment>();
                        }
                        currentEnrollments.add(0, enrollment);
                        hasNonWithdrawals = true;
                    } else if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                        if (hasNonWithdrawals && !currentEnrollments.isEmpty()) {
                            enrollmentSpans.add(0, new NYStudentEnrollmentSpan(currentEnrollments, this,
                                    getStudentContextAttributes(enrollment.getStudentOid())));
                            currentEnrollments = new ArrayList<StudentEnrollment>();
                        }

                        isActive = m_preferenceStudentActiveStatus.equals(enrollment.getStatusCode());
                        currentEnrollments.add(0, enrollment);
                        hasNonWithdrawals = false;
                    } else if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                        currentEnrollments.add(0, enrollment);
                        isActive = m_preferenceStudentActiveStatus.equals(enrollment.getStatusCode());
                        hasNonWithdrawals = true;
                    }
                }
                if (hasNonWithdrawals && !currentEnrollments.isEmpty()) {
                    enrollmentSpans.add(0, new NYStudentEnrollmentSpan(currentEnrollments, this,
                            getStudentContextAttributes(student.getOid())));
                    currentEnrollments = new ArrayList<StudentEnrollment>();
                }
            }

            // remove spans without an active interval
            Iterator<NYStudentEnrollmentSpan> iterator = enrollmentSpans.iterator();
            while (iterator.hasNext()) {
                NYStudentEnrollmentSpan span = iterator.next();
                if (span.getFirstActiveEnrollment() == null) {
                    iterator.remove();
                }
            }

            // If limit is set, go back through spans and remove any that are out of date range.
            if (limit) {
                iterator = enrollmentSpans.iterator();
                while (iterator.hasNext()) {
                    NYStudentEnrollmentSpan span = iterator.next();
                    if (span.getFirstActiveDate() == null) {
                        iterator.remove();
                        continue;
                    }
                    if (span.getFirstActiveDate() != null && endDate.before(span.getFirstActiveDate())) {
                        iterator.remove();
                        continue;
                    }
                    if (span.getLastActiveDate() != null && beginDate.after(span.getLastActiveDate())) {
                        iterator.remove();
                        continue;
                    }
                    if (MODE_STUDENT_ACTIVE_SNAPSHOT.equals(getStudentSelectionMode())) {
                        if (span.getLastActiveDate() != null && endDate.after(span.getLastActiveDate())) {
                            iterator.remove();
                            continue;
                        }
                    }
                }
            }

            return enrollmentSpans;
        }

        /**
         * Gets the student context attributes.
         *
         * @param stdOid String
         * @return Student context attributes
         */
        StudentContextAttributes getStudentContextAttributes(String stdOid) {
            if (m_studentAttributesMap == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(StudentContextAttributes.COL_CONTEXT_OID, getData().getCurrentContext().getOid());

                BeanQuery query = this.getStudentSelectionQuery(StudentContextAttributes.class, criteria,
                        StudentContextAttributes.COL_STUDENT_OID);
                m_studentAttributesMap =
                        m_data.getBroker().getMapByQuery(query, StudentContextAttributes.COL_STUDENT_OID, 1024);
            }
            return m_studentAttributesMap.get(stdOid);
        }

        /**
         * Return the user value entered for a parameter. Verify that the value matches a specified
         * type.
         *
         * @param selectKey The key of the property to retrieve.
         * @param expectedClass The class type expected for the value.
         * @param defaultValue The value to return if the property is not present or is null.
         *
         * @return Object
         */
        Object getNYStudentSelectionProperty(String selectKey, Class expectedClass, Object defaultValue) {
            Object value = getSelectionProperty(selectKey);
            if (value != null) {
                if (!expectedClass.isInstance(value)) {
                    throw new ClassCastException("getNYStudentSelectionProperty(" + selectKey + "): Expected "
                            + expectedClass.getName() + ", found " + value.getClass().getName());
                }
            } else {
                value = defaultValue;
            }
            return value;
        }

        /**
         * Get collection of program codes corresponding to given state reference code.
         *
         * @return Collection of program codes
         */

        // preserve in case need to reverse change in getStudentEnrollments()
        @SuppressWarnings("unused")
        private Collection<String> getEnrollmentCodes() {
            X2Criteria criteria = new X2Criteria();
            DataDictionaryField field = getData().getDataDictionaryField(StudentEnrollment.class,
                    StudentEnrollment.COL_ENROLLMENT_CODE);

            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getData().getBroker().getPersistenceKey());

            String[] columns = new String[] {ReferenceCode.COL_CODE};

            ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

            List<String> result = new ArrayList<String>();
            ReportQueryIterator iterator = getData().getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] record = (Object[]) iterator.next();
                    String code = (String) record[0];
                    result.add(code);
                }
            } finally {
                iterator.close();
            }
            return result;
        }

        /**
         * Returns the map of student enrollment records for a student.
         * The order of enrollment records in the list is descending, newest to oldest.
         *
         * @param studentOid String
         * @return List<StudentEnrollment>
         */
        private List<StudentEnrollment> getStudentEnrollments(String studentOid) {
            if (m_studentEnrollmentMap == null) {
                SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, getStudentCriteria());

                X2Criteria studentEnrollmentCriteria = new X2Criteria();
                studentEnrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
                studentEnrollmentCriteria.addNotNull(StudentEnrollment.COL_ENROLLMENT_DATE);

                QueryByCriteria studentEnrollmentQuery =
                        new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
                studentEnrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_TYPE, true);
                m_studentEnrollmentMap = getData().getBroker().getGroupedCollectionByQuery(studentEnrollmentQuery,
                        StudentEnrollment.COL_STUDENT_OID, 500);
            }

            return m_studentEnrollmentMap.get(studentOid);
        }

        /**
         * Load reference code map by field name.
         *
         * @param beanClass Class
         * @param fieldName String
         * @return Map<String, ReferenceCode>
         */
        private Map<String, ReferenceCode> loadRefCodeMapByField(Class beanClass, String fieldName) {
            Map<String, ReferenceCode> refCodeMap = null;
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getData().getBroker().getPersistenceKey());
            ModelProperty prop = new ModelProperty(beanClass, fieldName, getData().getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                refCodeMap = referenceTable.getCodeMap();
            }
            return refCodeMap;
        }

    }

    /**
     * A NYStudentEnrollmentSpan describes one enrollment of a student from entry to exit,
     * with all enrollment records, first and last active dates, and the ability to locate
     * relevant enrollment records within the span.
     *
     * @author X2 Development Corporation
     *
     */
    public class NYStudentEnrollmentSpan {

        private static final String STATUS_CHANGE = "S";
        /*
         * Constants
         */
        private static final String DEFAULT_CALENDAR_ID = "Standard";
        private static final String PREVIOUS_CALENDAR_ALIAS = "all-enr-StudentCalendar";

        /*
         * Instance variables
         */
        List<StudentEnrollment> m_enrollments;
        PlainDate m_firstActiveDate;
        StudentEnrollment m_firstActiveEnrollment;
        StudentEnrollment m_firstEntry;
        StudentEnrollment m_firstInactiveEnrollment;
        StudentEnrollment m_firstWithdrawal;
        NYStudentHistoryHelper m_helper;
        PlainDate m_lastActiveDate;
        Integer m_membershipDays;
        boolean m_preferenceMemberOnEntry;
        boolean m_preferenceMemberOnWithdrawal;
        SisSchool m_school;
        SisStudent m_student;
        List<StudentAttendance> m_studentAttendance;
        String m_calendarId;


        /**
         * Constructor:
         * Find other values from the enrollment list.
         *
         * @param enrollments List<StudentEnrollment>
         * @param helper NYStudentHistoryHelper
         * @param studentContextAttributes StudentContextAttributes
         */
        NYStudentEnrollmentSpan(List<StudentEnrollment> enrollments, NYStudentHistoryHelper helper,
                StudentContextAttributes studentContextAttributes) {
            m_helper = helper;
            Organization organization = getData().getOrganization();
            PlainDate districtBeginDate = getData().getCurrentContext().getStartDate();

            m_preferenceMemberOnEntry = Boolean.valueOf(PreferenceManager.getPreferenceValue(organization,
                    SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
            m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(organization,
                    SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

            boolean splitYog =
                    ((Boolean) helper.getNYStudentSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG,
                            Boolean.class, Boolean.FALSE)).booleanValue();
            boolean splitStatus =
                    ((Boolean) helper.getNYStudentSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS,
                            Boolean.class, Boolean.FALSE)).booleanValue();

            boolean isActive = false;
            boolean isInactiveAgain = false;
            DistrictSchoolYearContext selectedYear = getData().getCurrentContext();
            boolean isCurrentYear = getData().getOrganization().getCurrentContext().equals(selectedYear);
            m_enrollments = enrollments;

            for (StudentEnrollment enrollment : enrollments) {
                // Get student and school from the enrollment record.
                if (m_student == null) {
                    m_student = enrollment.getStudent();
                }
                String type = enrollment.getEnrollmentType();
                if (m_school == null && (!STATUS_CHANGE.equals(type))) {
                    m_school = enrollment.getSchool();
                    break;
                }
            }

            for (StudentEnrollment enrollment : enrollments) {
                if (!isActive) {
                    // Active code, or sometimes empty status and a non-withdrawal record.
                    String statusCode = enrollment.getStatusCode();

                    if (StudentManager.isActiveStudent(getData().getOrganization(), statusCode) ||
                            (StringUtils.isEmpty(statusCode) &&
                                    !StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()))) {
                        isActive = true;
                        m_firstActiveEnrollment = enrollment;
                        if (m_preferenceMemberOnEntry) {
                            m_firstActiveDate = enrollment.getEnrollmentDate();
                            if (m_firstActiveDate.before(districtBeginDate)) {
                                m_firstActiveDate = districtBeginDate;
                            }
                        } else {
                            // Lookup next in-session date for the school.
                            m_firstActiveDate = findSessionDate(enrollment.getEnrollmentDate(), true);
                        }
                    }
                } else if (!isInactiveAgain) {
                    String statusCode = enrollment.getStatusCode();
                    if (!StudentManager.isActiveStudent(getData().getOrganization(), statusCode)) {
                        isInactiveAgain = true;
                        m_firstInactiveEnrollment = enrollment;
                        if (m_preferenceMemberOnWithdrawal) {
                            m_lastActiveDate = enrollment.getEnrollmentDate();
                        } else {
                            // Lookup previous in-session date for the school.
                            m_lastActiveDate = findSessionDate(enrollment.getEnrollmentDate(), false);
                        }
                    }
                }

                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                        m_firstEntry == null) {
                    m_firstEntry = enrollment;
                }
                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()) &&
                        m_firstWithdrawal == null) {
                    m_firstWithdrawal = enrollment;
                }
            }

            // If no end-of-enrollment records was found (in case of YOG, STATUS) determine if
            // the last record should be treated as the exit record.
            if ((m_lastActiveDate == null || m_firstInactiveEnrollment == null) &&
                    m_enrollments.size() > 1) {
                StudentEnrollment enrollment = m_enrollments.get(m_enrollments.size() - 1);

                if ((splitYog && StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType())) ||
                        (splitStatus && StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) ||
                        StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    m_firstInactiveEnrollment = enrollment;
                    if (m_preferenceMemberOnWithdrawal) {
                        m_lastActiveDate = enrollment.getEnrollmentDate();
                    } else {
                        m_lastActiveDate = findSessionDate(enrollment.getEnrollmentDate(), false);
                    }
                }
            }

            // Find the calendar code to use for session days. Use the override alias on the last
            // enrollment for the span if populated
            // Otherwise, for current year use the student's calendar code
            // For previous years, use the calendar id from the StudentContextAttributes, if
            // available.
            StudentEnrollment lastEnrollment = m_enrollments.get(m_enrollments.size() - 1);
            if (!StringUtils.isEmpty((String) lastEnrollment.getFieldValueByAlias(PREVIOUS_CALENDAR_ALIAS))) {
                m_calendarId = (String) lastEnrollment.getFieldValueByAlias(PREVIOUS_CALENDAR_ALIAS);
            } else if (isCurrentYear) {
                m_calendarId = m_student.getCalendarCode();
            } else {
                if (getStudentHistoryHelper().m_studentAttributesMap != null &&
                        getStudentHistoryHelper().m_studentAttributesMap.get(m_student.getOid()) != null &&
                        getStudentHistoryHelper().m_studentAttributesMap.get(m_student.getOid())
                                .getCalendarCode() != null) {
                    m_calendarId =
                            getStudentHistoryHelper().m_studentAttributesMap.get(m_student.getOid()).getCalendarCode();
                }
            }
        }

        /**
         * Find the nearest in session date to the date provided.
         * <br>
         * parameter "after" indicates if the nearest date should be
         * after the date provided. Otherwise, the nearest in session date
         * before the date provided is returned.
         *
         * @param enrollmentDate PlainDate
         * @param after boolean
         * @return PlainDate
         */
        private PlainDate findSessionDate(PlainDate enrollmentDate, boolean after) {
            PlainDate nearestDate = null;
            Set<PlainDate> insessionDates = m_helper.getCalendarDays(m_school, m_calendarId);
            if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(m_calendarId)) {
                insessionDates = m_helper.getCalendarDays(m_school, DEFAULT_CALENDAR_ID);
            }
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (after && date.after(enrollmentDate)) {
                        if (nearestDate == null || nearestDate.after(date)) {
                            nearestDate = date;
                        }
                    } else if (!after && date.before(enrollmentDate)) {
                        if (nearestDate == null || nearestDate.before(date)) {
                            nearestDate = date;
                        }
                    }
                }
            }
            if (nearestDate == null) {
                nearestDate = enrollmentDate;
            }
            return nearestDate;
        }

        public int getAttendanceDays() {
            return getAttendanceDays(m_data.getCurrentContext().getStartDate(),
                    m_data.getCurrentContext().getEndDate());
        }

        public int getAttendanceDays(PlainDate startDateInterval, PlainDate endDateInterval) {
            int numDays = 0;
            for (StudentAttendance attendance : getStudentAttendance()) {
                if (attendance.getAbsentIndicator()) {
                    PlainDate date = attendance.getDate();
                    if (!date.before(startDateInterval) && !date.after(endDateInterval)) {
                        numDays++;
                    }
                }
            }
            return numDays;
        }

        /**
         * Return the first active date for the student in the enrollment span.
         * <p>
         * This finds the first enrollment record in the span that indicates
         * the student is active. It then checks the system preference for membership
         * on entry date to determine if this is the first active date or looks up
         * the next in session date as the first active date.
         * <p>
         * This value will be adjusted to fit the current school year. It is not
         * representative of reportable enrollment dates. First and last active dates
         * are most useful for counting membership and attendance days for an
         * enrollment span.
         *
         * @return PlainDate
         */
        public PlainDate getFirstActiveDate() {
            return m_firstActiveDate;
        }

        /**
         * Return the first enrollment record to indicate active status.
         * This is usually considered to be the entry record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getFirstActiveEnrollment() {
            return m_firstActiveEnrollment;
        }

        /**
         * Return the first enrollment record to indicate inactive status after having been active
         * in this span.
         * This is usually considered to be the exit record for the enrollment span.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getFirstInactiveEnrollment() {
            return m_firstInactiveEnrollment;
        }

        /**
         * Return the most recent enrollment record of the specified types that exists on or before
         * the specified date.
         *
         * @param date as of date to find enrollment records for.
         * @param types a String that includes a combination of the four StudentEnrollment type
         *        constants ('E','W','S','Y').
         *        <br>
         *        EX: "ES" to search for only Entry or Status Change records.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEnrollmentForDate(PlainDate date, String types) {
            StudentEnrollment lastEnrollment = null;
            for (StudentEnrollment enrollment : m_enrollments) {
                if (!enrollment.getEnrollmentDate().after(date)) {
                    if (types.contains(enrollment.getEnrollmentType())) {
                        lastEnrollment = enrollment;
                    }
                }
            }
            return lastEnrollment;
        }

        /**
         * Return the list of enrollments in this span.
         *
         * @return List<StudentEnrollment>
         */
        public List<StudentEnrollment> getEnrollments() {
            return m_enrollments;
        }

        /**
         * Return the last active date for the student in the enrollment span.
         * <p>
         * This finds the first enrollment record in the span that indicates
         * the student is inactive after having been active previously. It then
         * checks the system preference for membership on withdrawal date to
         * determine if this is the last active date or looks up the previous
         * in session date as the last active date.
         * <p>
         * This value will be adjusted to fit the current school year. It is not
         * representative of reportable enrollment dates. First and last active dates
         * are most useful for counting membership and attendance days for an
         * enrollment span.
         *
         * @return PlainDate
         */
        public PlainDate getLastActiveDate() {
            return m_lastActiveDate;
        }

        /**
         * Return the number of days the student is in membership in the specified enrollment span.
         * <br>
         * This uses first and last active dates, the school calendar and student calendar ID to
         * identify membership days.
         *
         * @return int
         */
        public int getMembershipDays() {
            return getMembershipDays(m_data.getCurrentContext().getStartDate(),
                    m_data.getCurrentContext().getEndDate());
        }

        /**
         * Return the number of days the student is in membership in the specified enrollment span.
         * <br>
         * This uses first and last active dates, the school calendar and student calendar ID to
         * identify membership days.
         *
         * @param startDateInterval PlainDate
         * @param endDateInterval PlainDate
         * @return int
         */
        public int getMembershipDays(PlainDate startDateInterval, PlainDate endDateInterval) {
            if (m_membershipDays == null) {
                // Get the in session days for the school and calendar.
                Set<PlainDate> insessionDates = m_helper.getCalendarDays(m_school, m_calendarId);
                if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(m_calendarId)) {
                    insessionDates = m_helper.getCalendarDays(m_school, DEFAULT_CALENDAR_ID);
                }
                if (insessionDates == null) {
                    insessionDates = m_helper.getCalendarDays(m_school, StudentHistoryHelper.CALENDAR_ANY);
                }

                // Count in session days between (and including) first and last active dates.
                PlainDate endDate = m_lastActiveDate;
                if (endDate == null) {
                    endDate = (PlainDate) m_helper.getNYStudentSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                            PlainDate.class,
                            new PlainDate(OrganizationManager.getTimeZone(getData().getOrganization())));
                }
                int count = 0;
                if (insessionDates != null) {
                    for (PlainDate date : insessionDates) {
                        if (m_firstActiveDate != null && !date.before(m_firstActiveDate) && !date.after(endDate)) {
                            if (!date.before(startDateInterval) && !date.after(endDateInterval)) {
                                count++;
                            }
                        }
                    }
                }
                m_membershipDays = Integer.valueOf(count);
            }
            return m_membershipDays.intValue();
        }

        /**
         * Return the school for the enrollment span.
         *
         * @return School
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Returns a list of student attendance records for the student within the
         * specified date range first active to last active.
         *
         * @return List<StudentAttendance>
         */
        public List<StudentAttendance> getStudentAttendance() {
            if (m_studentAttendance == null) {
                m_studentAttendance = new ArrayList<StudentAttendance>(1);
                List<StudentAttendance> parentList = m_helper.getStudentAttendances(m_student.getOid());

                if (parentList != null) {
                    PlainDate endDate = m_lastActiveDate;
                    if (endDate == null) {
                        endDate = (PlainDate) m_helper.getNYStudentSelectionProperty(
                                StudentHistoryHelper.PROPERTY_END_DATE,
                                PlainDate.class,
                                new PlainDate(OrganizationManager.getTimeZone(getData().getOrganization())));
                    }

                    // Get the in session days for the school and calendar.
                    Set<PlainDate> insessionDates = m_helper.getCalendarDays(m_school, m_calendarId);
                    if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(m_calendarId)) {
                        insessionDates = m_helper.getCalendarDays(m_school, DEFAULT_CALENDAR_ID);
                    }
                    if (insessionDates == null) {
                        insessionDates = m_helper.getCalendarDays(m_school, StudentHistoryHelper.CALENDAR_ANY);
                    }

                    for (StudentAttendance attendance : parentList) {
                        if (m_firstActiveDate != null &&
                                !attendance.getDate().before(m_firstActiveDate) &&
                                !attendance.getDate().after(endDate) &&
                                insessionDates != null &&
                                insessionDates.contains(attendance.getDate())) {
                            m_studentAttendance.add(attendance);
                        }
                    }
                }
            }
            return m_studentAttendance;
        }

        /**
         * Gets the yog.
         *
         * @return int
         */
        public int getYog() {
            int value = 0;
            if (m_firstActiveEnrollment != null) {
                value = m_firstActiveEnrollment.getYog();
            }
            return value;
        }
    }

    protected PlainDate m_currentDate;
    protected StateReportData m_data;
    protected String m_preferenceStudentActiveStatus;

    NYStudentHistoryHelper m_studentHistoryHelper;

    /**
     * Instantiates a new NY enrollment helper.
     *
     * @param data StateReportData
     */
    public NYEnrollmentHelper(StateReportData data) {
        m_data = data;
        m_studentHistoryHelper = new NYStudentHistoryHelper(data);
        m_preferenceStudentActiveStatus = PreferenceManager.getPreferenceValue(m_studentHistoryHelper.getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_currentDate = new PlainDate(OrganizationManager.getTimeZone(m_studentHistoryHelper.getOrganization()));
    }

    /**
     * Instantiates a new NY enrollment helper.
     *
     * @param m_selectedStartDate PlainDate
     * @param m_selectedEndDate PlainDate
     * @param data StateReportData
     */
    public NYEnrollmentHelper(PlainDate m_selectedStartDate, PlainDate m_selectedEndDate, StateReportData data) {
        this(data);
        m_studentHistoryHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHistoryHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_selectedStartDate);
        m_studentHistoryHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_selectedEndDate);
        m_studentHistoryHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.valueOf(true));
        m_studentHistoryHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS,
                Boolean.valueOf(true));
        m_studentHistoryHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE,
                Boolean.valueOf(true));
    }

    /**
     * Gets the student history helper.
     *
     * @return NY student history helper
     */
    public NYStudentHistoryHelper getStudentHistoryHelper() {
        return m_studentHistoryHelper;
    }

    /**
     * Gets the data.
     *
     * @return State report data
     */
    protected StateReportData getData() {
        return m_data;
    }

}
