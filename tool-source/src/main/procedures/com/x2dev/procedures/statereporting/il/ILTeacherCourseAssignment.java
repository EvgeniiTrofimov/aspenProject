/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffAttendance;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Teacher Course Assignment.
 *
 * @author X2 Development Corporation
 */
public class ILTeacherCourseAssignment extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by IL Teacher Course Assignment.
     *
     * @author X2 Development Corporation
     */
    public static class TeacherCourseAssignmentEntity extends StateReportEntity {
        /**
         * Cached values for retrievers to share.
         */
        ILTeacherCourseAssignment m_scaData = null;

        /**
         * Keep track of staff absences. For use in actual attendance.
         */
        Collection<StaffAttendance> m_absences;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TeacherCourseAssignmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ScheduleTeacher mtc = (ScheduleTeacher) getBean();
            SisStaff staff = mtc.getStaff();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "] ";
            name += "Course Number = ";
            name += mtc.getSection().getSchoolCourse().getCourse().getNumber();
            return name;
        }

        /**
         * Initialize the entity for the student bean provided.
         * This method finds the student schedule and student schedule change records for the
         * student
         * and generates a list of reportable schedule items.
         * The entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_scaData = (ILTeacherCourseAssignment) data;
            SisStaff staff = ((ScheduleTeacher) bean).getStaff();
            String staffOid = staff.getOid();

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StaffAttendance.COL_STAFF_OID, staffOid);
            criteria.addEqualTo(StaffAttendance.COL_CODE, CODE_ABSENT);
            criteria.addGreaterOrEqualThan(StaffAttendance.COL_DATE,
                    m_scaData.getOrganization().getCurrentContext().getStartDate());
            BeanQuery query = new BeanQuery(StaffAttendance.class, criteria);
            query.addOrderByAscending(StaffAttendance.COL_DATE); // sort by ascending (oldest to
                                                                 // newest)
            m_absences = m_scaData.getBroker().getCollectionByQuery(query);
            m_scaData.m_totalCount++;
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
         * By default, use the Teacher Course Assignment Entry (IL-EXPDATA-TCA-E) export definition.
         *
         * @return String
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            String result = EXPORT_TYPE_ENTRY;
            if (m_scaData.m_exportType.equals(EXPORT_TYPE_EXIT)) {
                result = EXPORT_TYPE_EXIT;
            }
            return result;
        }
    }

    /**
     * Retrieves the Course End Date. Use DOE MTC END DATE. If blank, use term end date from the
     * section.
     *
     * @author Follett Software Company
     */
    protected class RetrieveCourseEndDate implements FieldRetriever {

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
            TeacherCourseAssignmentEntity scae = (TeacherCourseAssignmentEntity) entity;
            ScheduleTeacher scheduleTeacher = (ScheduleTeacher) scae.getBean();
            Object result = null;
            String scheduleTermOid = null;
            if (scheduleTeacher.getSection() != null) {
                scheduleTermOid = scheduleTeacher.getSection().getScheduleTermOid();
                if (!StringUtils.isEmpty(scheduleTermOid)) {
                    Collection<ScheduleTermDate> termDates = getTermDates(scheduleTermOid);
                    for (ScheduleTermDate termDate : termDates) {
                        PlainDate termEnd = termDate.getEndDate();
                        if (termEnd != null) {
                            result = termEnd;
                            break;
                        }
                    }
                }
            }

            String endDate = (String) data.getProperty(scheduleTeacher, m_fieldTeacherEndDate);
            if (!StringUtils.isEmpty(endDate)) {
                try {
                    result = new PlainDate(m_dateFormat.parse(endDate));
                } catch (ParseException e1) {
                    // Do nothing, invalid date format.
                }
            }

            return result;
        }
    }

    /**
     * Retrieves the Exit Reason. If no DOE MTC END DATE is defined, default to "01".
     *
     * @author Follett Software Company
     */
    protected class RetrieveExitReason implements FieldRetriever {

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
            TeacherCourseAssignmentEntity scae = (TeacherCourseAssignmentEntity) entity;
            ScheduleTeacher scheduleTeacher = (ScheduleTeacher) scae.getBean();

            String result = null;
            String endDate = (String) data.getProperty(scheduleTeacher, m_fieldTeacherEndDate);
            if (StringUtils.isEmpty(endDate)) {
                result = "01";
            } else {
                result = (String) getProperty(scheduleTeacher, field.getBeanPath());
                result = lookupStateValue(ScheduleTeacher.class, field.getBeanPath(), result);
            }
            return result;
        }
    }

    /**
     * Retrieve the RCDTS of the student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {
        private static final String PARAM_SERVING_SCHOOL = "S";

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
            String param = (String) field.getParameter();
            String rcdts = null;

            ScheduleTeacher mtc = (ScheduleTeacher) entity.getBean();

            if (param.equals(PARAM_SERVING_SCHOOL)) // serving rcdts (the course's school's rcdts)
            {
                MasterSchedule mst = mtc.getSection();
                SchoolCourse csk = mst.getSchoolCourse();
                SisSchool school = csk.getSchool();
                if (school != null) {
                    rcdts = (String) school.getFieldValueByBeanPath(m_fieldSchoolCode);
                }
            }

            return rcdts;
        }

    }

    /**
     * Returns course view that has been stripped of illegal characters for a section number field.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSectionNumber implements FieldRetriever {

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
            ScheduleTeacher schedTeacher = ((ScheduleTeacher) entity.getBean());

            String crsView = schedTeacher.getSection().getCourseView();
            String cleanValue = EMPTY;

            if (!StringUtils.isEmpty(crsView)) {
                Matcher matcher = m_illegalCrsViewChars.matcher(crsView);
                cleanValue = matcher.replaceAll(HYPHEN);
            }

            return cleanValue;
        }
    }

    /**
     * Retrieve course start date.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveStartDate implements FieldRetriever {

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
            TeacherCourseAssignmentEntity scae = (TeacherCourseAssignmentEntity) entity;
            ScheduleTeacher scheduleTeacher = (ScheduleTeacher) scae.getBean();
            Object result = null;
            String scheduleTermOid = null;
            if (scheduleTeacher.getSection() != null) {
                scheduleTermOid = scheduleTeacher.getSection().getScheduleTermOid();
                if (!StringUtils.isEmpty(scheduleTermOid)) {
                    Collection<ScheduleTermDate> termDates = getTermDates(scheduleTermOid);
                    for (ScheduleTermDate termDate : termDates) {
                        PlainDate termStart = termDate.getStartDate();
                        if (termStart != null) {
                            result = termStart;
                            break;
                        }
                    }
                }
            }

            String startDate = (String) data.getProperty(scheduleTeacher, m_fieldTeacherStartDate);
            if (!StringUtils.isEmpty(startDate)) {
                try {
                    result = new PlainDate(m_dateFormat.parse(startDate));
                } catch (ParseException e1) {
                    // Do nothing, invalid date format.
                }
            }

            return result;

        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            Matcher matcher = m_illegalNameCharacters.matcher(value);

            return matcher.replaceAll(EMPTY);
        }
    }

    /**
     * Retrieves the teacher's attendance for a course.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAttendance implements FieldRetriever {
        private final String PARAM_ACTUAL = "ACTUAL";
        private final String PARAM_TOTAL = "TOTAL";

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
            TeacherCourseAssignmentEntity scae = (TeacherCourseAssignmentEntity) entity;
            ScheduleTeacher mtc = (ScheduleTeacher) scae.getBean();
            SisStaff staff = mtc.getStaff();
            Set<PlainDate> calendarDays = getCalendarDays(staff.getSchool(), staff.getCalendarId());
            Object result = null;
            if (calendarDays != null) {
                String param = (String) field.getParameter();
                if (param.equals(PARAM_ACTUAL)) {
                    result = Integer.valueOf(calendarDays.size() - scae.m_absences.size());
                } else if (param.equals(PARAM_TOTAL)) {
                    result = Integer.valueOf(calendarDays.size());
                }
            }

            return result;
        }

    }

    /**
     * Retrieves the teacher's role.
     *
     * @author Follett Software Company
     */
    protected class RetrieveRole implements FieldRetriever {

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
            TeacherCourseAssignmentEntity scae = (TeacherCourseAssignmentEntity) entity;
            ScheduleTeacher mtc = (ScheduleTeacher) scae.getBean();
            String result = null;
            if (mtc != null) {
                String code = (String) mtc.getFieldValueByBeanPath(m_fieldMtcPosCodeOverride);
                if (!StringUtils.isEmpty(code)) {
                    result = data.lookupStateValue(ScheduleTeacher.class, m_fieldMtcPosCodeOverride, code);
                }
                if (result == null) {
                    result = data.lookupStateValue(SisStaff.class, m_fieldStfPosCode,
                            (String) mtc.getStaff().getFieldValueByBeanPath(m_fieldStfPosCode));
                }
            }
            return result;
        }
    }

    /**
     * Alias fields
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_SECTION = "DOE EXCLUDE MST";
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_STAFF = "DOE EXCLUDE STF";
    protected static final String ALIAS_HOME_SCHOOL_CODE = "DOE SCHOOL HOME";
    protected static final String ALIAS_MTC_POSITION_CODE = "DOE EIS POSITION CODE OVERRIDE";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_STATE_COURSE_ID = "DOE STATE COURSE ID";
    protected static final String ALIAS_STF_POS_CODE = "DOE EIS POSITION CODE";
    protected static final String ALIAS_TEACHER_START_DATE = "DOE MTC START DATE";
    protected static final String ALIAS_TEACHER_END_DATE = "DOE MTC END DATE";

    protected static final String CODE_ABSENT = "A";
    protected static final String CODE_PRIMARY_TEACHER = "01";

    protected static final String EMPTY = "";

    protected static final String EXPORT_TYPE_ENTRY = "E";
    protected static final String EXPORT_TYPE_EXIT = "X";

    protected static final String HYPHEN = "-";

    protected static final String ILLEGAL_NAME_CHARACTERS = "[^-A-z ]";
    protected static final String ILLEGAL_CRS_VIEW_CHARACTERS = "[^-A-z0-9]";
    protected static final String DATE_FORMAT = "yyyy-MM-dd";

    protected static final String PARAM_EXPORT_TYPE = "exportType";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SKIP_ZERO_ENR = "skipZeroEnrollment";

    private static final String RETRIEVER_TCA_ATTENDANCE = "TCA-ATTENDANCE";
    private static final String RETRIEVER_TCA_CLEAN = "TCA-CLEAN";
    private static final String RETRIEVER_TCA_RCDTS = "TCA-RCDTS";
    private static final String RETRIEVER_TCA_ROLE = "TCA-ROLE";
    private static final String RETRIEVER_TCA_SECTION_NUM = "TCA-SECTION-NUM";
    private static final String RETRIEVER_TCA_START = "TCA-START";
    private static final String RETRIEVER_TCA_EXIT_REASON = "TCA-EXIT-REASON";
    private static final String RETRIEVER_TCA_COURSE_END = "TCA-COURSE-END";


    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected DateFormat m_dateFormat;
    protected EnrollmentManager m_enrollmentManager;
    protected String m_excludeCourseField;
    protected String m_excludeSectionField;
    protected String m_excludeStaffField;
    protected String m_exportType;
    protected String m_fieldDistrictCode;
    protected String m_fieldHomeSchoolCode;
    protected String m_fieldMtcPosCodeOverride;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldStateCourseId;
    protected String m_fieldStfPosCode;
    protected String m_fieldTeacherEndDate;
    protected String m_fieldTeacherStartDate;
    protected Pattern m_illegalCrsViewChars;
    protected Pattern m_illegalNameCharacters;
    protected PlainDate m_reportDate;
    protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars;
    protected Boolean m_skipZeroEnrollment;
    protected Map<String, Collection<ScheduleTermDate>> m_termDateMap;
    protected int m_totalCount;

    protected String[] m_validStateRoles = {"01", "02", "03", "04", "05", "06", "07", "08", "10"};

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append(m_exportType.equals(EXPORT_TYPE_ENTRY) ? "Teacher Course Assignment Entry"
                : "Teacher Course Assignment Exit");
        heading.append(',');
        heading.append(m_totalCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(new SimpleDateFormat("MM/dd/yyyy").format(m_reportDate));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        // Load initialization data
        initializeFields();

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap<String, Map<String, Set<PlainDate>>>();
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
        m_illegalCrsViewChars = Pattern.compile(ILLEGAL_CRS_VIEW_CHARACTERS);
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            X2Criteria masterScheduleTeacherCriteria = getScheduleMasterTeacherCriteria();
            BeanQuery masterScheduleTeacherQuery = new BeanQuery(ScheduleTeacher.class, masterScheduleTeacherCriteria);
            applyInputSort(masterScheduleTeacherQuery, ScheduleTeacher.REL_STAFF);
            setQuery(masterScheduleTeacherQuery);
            setEntityClass(TeacherCourseAssignmentEntity.class);

            // Build maps of retriever functions
            Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RETRIEVER_TCA_RCDTS, new RetrieveRcdts());
            calcs.put(RETRIEVER_TCA_ATTENDANCE, new RetrieveAttendance());
            calcs.put(RETRIEVER_TCA_START, new RetrieveStartDate());
            calcs.put(RETRIEVER_TCA_SECTION_NUM, new RetrieveSectionNumber());
            calcs.put(RETRIEVER_TCA_CLEAN, new RetrieveStripNameChar());
            calcs.put(RETRIEVER_TCA_ROLE, new RetrieveRole());
            calcs.put(RETRIEVER_TCA_EXIT_REASON, new RetrieveExitReason());
            calcs.put(RETRIEVER_TCA_COURSE_END, new RetrieveCourseEndDate());
            super.addCalcs(calcs);
        }
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
        Map<String, Set<PlainDate>> calendarData = null;
        Set<PlainDate> calendarDates = null;
        if (school != null && !m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = null;
            if (school.getActiveSchedule() != null) {
                startDate = school.getActiveSchedule().getStartDate();
            } else {
                startDate = getOrganization().getCurrentContext().getStartDate();
            }
            calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        if (school != null) {
            calendarData = m_schoolsToCalendars.get(school.getOid());
            calendarDates = calendarData.get(calendar);
        }
        return calendarDates;
    }

    /**
     * Load the schedule term dates for a schedule term oid.
     * Keep a map of existing codes for lookup.
     *
     * @param scheduleTermOid String
     * @return Collection<ScheduleTermDate>
     */
    protected Collection<ScheduleTermDate> getTermDates(String scheduleTermOid) {
        Collection<ScheduleTermDate> dates = null;

        if (m_termDateMap == null) {
            m_termDateMap = new HashMap<String, Collection<ScheduleTermDate>>();
        }

        if (!m_termDateMap.containsKey(scheduleTermOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            dates = getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }

        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Get only active students.
     *
     * @return X 2 criteria
     */
    private X2Criteria getScheduleMasterTeacherCriteria() {
        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addNotEmpty(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_fieldStateCourseId, getBroker().getPersistenceKey());

        reportingCriteria.addGreaterThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_CREDIT,
                BigDecimal.ZERO);

        applyInputCriteria(reportingCriteria, true, ScheduleTeacher.REL_STAFF);

        // Check exclusion flags for staff, section and course.
        if (!StringUtils.isEmpty(m_excludeStaffField)) {
            reportingCriteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER +
                    m_excludeStaffField, BooleanAsStringConverter.TRUE);
        }

        if (!StringUtils.isEmpty(m_excludeSectionField)) {
            reportingCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    m_excludeSectionField,
                    BooleanAsStringConverter.TRUE);
        }

        if (!StringUtils.isEmpty(m_excludeCourseField)) {
            reportingCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }

        // Take all courses in the current school year.
        reportingCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.COL_SCHEDULE_OID);

        // Take only for current school
        if (getSchool() != null) {
            reportingCriteria.addEqualTo(
                    ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                            Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }

        // Skip sections with zero enrollments if needed.
        if (m_skipZeroEnrollment.booleanValue()) {
            reportingCriteria.addGreaterThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.COL_ENROLLMENT_TOTAL, Integer.valueOf(0));
        }

        // If export type is entry, include the ScheduleTeacher only if the Teacher Role contains a
        // valid state code
        if (m_exportType.equals(EXPORT_TYPE_ENTRY)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            ModelProperty prop =
                    new ModelProperty(ScheduleTeacher.class, ScheduleTeacher.COL_ROLE, getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            ReferenceTable referenceTable = field.getReferenceTable();

            X2Criteria rolesCriteria = new X2Criteria();

            if (referenceTable != null) {
                Map<String, ReferenceCode> m_roleCodes = referenceTable.getCodeMap();

                Set<Entry<String, ReferenceCode>> entries = m_roleCodes.entrySet();

                Collection<String> roleCodes = new ArrayList<String>();

                for (Entry<String, ReferenceCode> entry : entries) {
                    String stateCode = entry.getValue().getStateCode();

                    if (stateCode != null && Arrays.asList(m_validStateRoles).contains(stateCode)) {
                        roleCodes.add(entry.getKey());
                    }
                }
                rolesCriteria.addIn(ScheduleTeacher.COL_ROLE, roleCodes);
            }
            X2Criteria primaryCriteria = new X2Criteria();
            primaryCriteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, Boolean.TRUE);

            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addOrCriteria(rolesCriteria);
            orCriteria.addOrCriteria(primaryCriteria);
            reportingCriteria.addAndCriteria(orCriteria);
        }

        return reportingCriteria;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldHomeSchoolCode = translateAliasToJavaName(ALIAS_HOME_SCHOOL_CODE, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_excludeCourseField = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, false);
        m_excludeSectionField = translateAliasToJavaName(ALIAS_EXCLUDE_SECTION, false);
        m_excludeStaffField = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF, false);
        m_fieldStateCourseId = translateAliasToJavaName(ALIAS_STATE_COURSE_ID, true);
        m_fieldTeacherStartDate = translateAliasToJavaName(ALIAS_TEACHER_START_DATE, true);
        m_fieldTeacherEndDate = translateAliasToJavaName(ALIAS_TEACHER_END_DATE, true);

        m_exportType = (String) getParameter(PARAM_EXPORT_TYPE);
        if (m_exportType == null) {
            m_exportType = EXPORT_TYPE_ENTRY;
        }
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        m_skipZeroEnrollment = (Boolean) getParameter(PARAM_SKIP_ZERO_ENR);
        m_fieldMtcPosCodeOverride = translateAliasToJavaName(ALIAS_MTC_POSITION_CODE, true);
        m_fieldStfPosCode = translateAliasToJavaName(ALIAS_STF_POS_CODE, true);
    }
}
