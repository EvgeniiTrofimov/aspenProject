/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.nj;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for Staff Course Roster export.
 *
 * @author X2 Development Corporation
 */

public class StaffCourseRoster extends StateReportData {
    /**
     * Entity class for Staff Course Roster export.
     *
     * @author X2 Development Corporation
     */

    public static class StaffCourseRosterEntity extends StateReportEntity {
        static final String FIELD_SECTION_ENTRY_DATE = "Section Entry Date";

        StaffCourseRoster scrData;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StaffCourseRosterEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            scrData = (StaffCourseRoster) data;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ScheduleTeacher teacherSchedule = (ScheduleTeacher) getBean();
            String name = teacherSchedule.getStaff().getNameView() +
                    " [LOCALID: " + teacherSchedule.getStaff().getLocalId() +
                    "] " + teacherSchedule.getSection().getCourseView() +
                    " " + teacherSchedule.getSection().getSchoolCourse().getDescription();

            return name;
        }

        /**
         * This method returns the current teacher schedule.
         *
         * @return Schedule teacher
         */
        public ScheduleTeacher getTeacherSchedule() {
            return (ScheduleTeacher) getBean();
        }

        /**
         * This method is filtering out the staffs who comes before or after the current
         * school year.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            Date entryDate = null;
            StateReportValidationError error = null;
            String startDate = getFieldValue(FIELD_SECTION_ENTRY_DATE);

            if (!StringUtils.isEmpty(startDate)) {
                String dateString =
                        startDate.substring(0, 4) + "-" + startDate.substring(4, 6) + "-" + startDate.substring(6);
                entryDate = java.sql.Date.valueOf(dateString);
            }
            if (null == entryDate || scrData.getCurrentContext().getStartDate().after(entryDate)) {
                error = new StateReportValidationError(getEntityName(), FIELD_SECTION_ENTRY_DATE,
                        "Improper Section Entry Date", "Excluding all sections before district start date");
            }

            return error;
        }
    }

    /**
     * This class returns the course's district and school code using the Teacher's schedule.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseDistrictSchoolCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleTeacher teacherSchedule = ((StaffCourseRosterEntity) entity).getTeacherSchedule();
            if (teacherSchedule != null) {
                MasterSchedule masterSchedule = teacherSchedule.getSection();
                if (masterSchedule != null) {
                    SchoolCourse course = masterSchedule.getSchoolCourse();
                    if (course != null) {
                        SisSchool school = course.getSchool();
                        if (school != null) {
                            if (PARAM_DISTRICT_CODE.equalsIgnoreCase(param)) {
                                String districtCode = (String) school.getFieldValueByAlias(ALIAS_DISTRICT_CODE);
                                if (!StringUtils.isEmpty(districtCode)
                                        && districtCode.length() >= NJ_DOE_DISTRICT_CODE_LENGTH) {
                                    value = districtCode.substring(0, NJ_DOE_DISTRICT_CODE_LENGTH);
                                }
                            } else if (PARAM_SCHOOL_CODE.equals(param)) {
                                String schoolCode = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_CODE);
                                if (!StringUtils.isEmpty(schoolCode)
                                        && schoolCode.length() >= NJ_DOE_SCHOOL_CODE_LENGTH) {
                                    value = schoolCode.substring(0, NJ_DOE_SCHOOL_CODE_LENGTH);
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retriever for the county code.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCountyCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            ScheduleTeacher teacherSchedule = ((StaffCourseRosterEntity) entity).getTeacherSchedule();
            SisStaff staff = teacherSchedule.getStaff();
            String tempValue = null;
            if (staff != null
                    && (tempValue = (String) staff.getOrganization1()
                            .getFieldValueByBeanPath(m_orgCountyCode.getJavaName())) != null) {
                value = m_orgCountyCode.hasReferenceTable()
                        ? lookupStateValue(Organization.class, m_orgCountyCode.getJavaName(), tempValue) : tempValue;
            }
            return value;
        }
    }

    /**
     * This class returns the course's entry and exit date using the teacher's schedule.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveStaffCourseEntryExitDate implements FieldRetriever {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleTeacher teacherSchedule = ((StaffCourseRosterEntity) entity).getTeacherSchedule();
            if (teacherSchedule != null) {
                // Read Entry Exit Dates from the Teacher Schedule. If the value is null,
                // pull it from the term dates.
                if (PARAM_SECTION_ENTRY_DATE.equalsIgnoreCase(param)) {
                    PlainDate entryDate = null;
                    String entryDateStr = (String) teacherSchedule.getFieldValueByAlias(ALIAS_SECTION_ENTRY_DATE);
                    if (entryDateStr != null) {
                        entryDate = DateUtils.getDate(entryDateStr);
                    }

                    // If Entry Date not found then get it from the teacherSchedule's Schedule Term
                    if (entryDate == null) {
                        PlainDate termDate = null;
                        ScheduleTerm term = teacherSchedule.getScheduleTerm();
                        if (term != null) {
                            Collection<ScheduleTermDate> termDates = term.getScheduleTermDates();
                            for (ScheduleTermDate schedTermDate : termDates) {
                                if (termDate == null || termDate.after(schedTermDate.getStartDate())) {
                                    termDate = schedTermDate.getStartDate();
                                }
                            }
                            entryDate = termDate;
                        }
                    }

                    // Format date if there is one.
                    if (entryDate != null) {
                        value = dateFormat.format(entryDate);
                    }
                } else if (PARAM_SECTION_EXIT_DATE.equalsIgnoreCase(param)) {
                    PlainDate exitDate = null;
                    String exitDateStr = (String) teacherSchedule.getFieldValueByAlias(ALIAS_SECTION_EXIT_DATE);
                    if (!StringUtils.isEmpty(exitDateStr)) {
                        exitDate = DateUtils.getDate(exitDateStr);
                    }

                    // If Exit Date not found then get it from the teacherSchedule's Schedule Term
                    if (exitDate == null) {
                        PlainDate termDate = null;
                        ScheduleTerm term = teacherSchedule.getSection().getScheduleTerm();
                        if (term != null) {
                            Collection<ScheduleTermDate> termDates = term.getScheduleTermDates();
                            for (ScheduleTermDate schedTermDate : termDates) {
                                if (termDate == null || termDate.before(schedTermDate.getEndDate())) {
                                    termDate = schedTermDate.getEndDate();
                                }
                            }
                            if (termDate != null) {
                                exitDate = termDate;
                            }
                        }
                    }

                    // Format date if there is one.
                    if (exitDate != null) {
                        value = dateFormat.format(exitDate);
                    }
                }
            }
            return value;
        }
    }

    /**
     * This class retrieves the subject area code and course id using the teacher's
     * schedule.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseSCEDCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleTeacher teacherSchedule = ((StaffCourseRosterEntity) entity).getTeacherSchedule();
            if (teacherSchedule != null) {
                MasterSchedule masterSchedule = teacherSchedule.getSection();
                if (masterSchedule != null) {
                    SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                    if (schoolCourse != null) {
                        Course course = schoolCourse.getCourse();
                        if (course != null) {
                            String subjAreaCode =
                                    (String) course.getFieldValueByBeanPath(m_subjectAreaCode);
                            if (!StringUtils.isEmpty(subjAreaCode)) {
                                subjAreaCode = lookupStateValue(Course.class, m_subjectAreaCode,
                                        subjAreaCode);
                            }
                            if (!StringUtils.isEmpty(subjAreaCode) && subjAreaCode.length() > 2) {
                                if (PARAM_SUBJECT_AREA.equalsIgnoreCase(param)) {
                                    subjAreaCode = subjAreaCode.substring(0, 2);
                                    if (!StringUtils.isEmpty(subjAreaCode)) {
                                        value = subjAreaCode;
                                    }
                                } else if (PARAM_COURSE_ID.equalsIgnoreCase(param)) {
                                    subjAreaCode = subjAreaCode.substring(2);
                                    if (!StringUtils.isEmpty(subjAreaCode)) {
                                        value = subjAreaCode;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * This class retrieves the course's details using the teacher's schedule.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveCourseDetails implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleTeacher teacherSchedule = ((StaffCourseRosterEntity) entity).getTeacherSchedule();
            if (teacherSchedule != null) {
                MasterSchedule masterSchedule = teacherSchedule.getSection();
                if (PARAM_COURSE_LEVEL.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                value = course.getFieldValueByBeanPath(m_courseLevel);
                                value = lookupReferenceCodeByBeanPath(Course.class, m_courseLevel,
                                        (String) value, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            }
                        }
                    }
                } else if (PARAM_GRADESPAN.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                String code = (String) course.getFieldValueByAlias(ALIAS_GRADE_SPAN);
                                if (code != null) {
                                    String stateCode = lookupReferenceCodeByBeanPath(Course.class, m_gradeSpan, code,
                                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                    // If we haven't reference table for Grade Code we removed the
                                    // “-“ in Grade Code.
                                    // If the result is exactly 2 characters, we will duplicate. In
                                    // this case, “09” becomes “0909”.
                                    if (stateCode == null) {
                                        String[] stateCodeArray = code.split("-");
                                        if (stateCodeArray.length == 2) {
                                            value = stateCodeArray[0] + stateCodeArray[1];
                                        }
                                        // If the result is exactly 2 characters, we will duplicate.
                                        // In this case, “09” becomes “0909”.
                                        else if (stateCodeArray.length == 1) {
                                            if (stateCodeArray[0].length() == 2) {
                                                value = stateCodeArray[0] + stateCodeArray[0];
                                            } else {
                                                // s-33134 if code is not 2 length stay it like
                                                // exist
                                                value = stateCodeArray[0];
                                            }
                                        }
                                    } else {
                                        value = stateCode;
                                    }
                                }
                            }
                        }
                    }
                } else if (PARAM_COURSE_SEQ.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                value = course.getFieldValueByAlias(ALIAS_COURSE_SEQ);
                                if (value != null) {
                                    value = Integer.valueOf((String) value);
                                }
                            }
                        }
                    }
                } else if (PARAM_CREDIT.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                value = course.getCredit();
                            }
                        }
                    }
                } else if (PARAM_LOCAL_COURSE_TITLE.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            value = schoolCourse.getDescription();
                        }
                    }
                } else if (PARAM_LOCAL_COURSE_CODE.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            value = schoolCourse.getNumber();
                        }
                    }
                } else if (PARAM_LOCAL_SECTION_CODE.equalsIgnoreCase(param)) {
                    if (masterSchedule != null) {
                        value = masterSchedule.getCourseView();
                    }
                }
            }
            return value;
        }
    }

    /**
     * NJ DOE Constants.
     */
    protected static final int NJ_DOE_DISTRICT_CODE_LENGTH = 4;
    protected static final int NJ_DOE_SCHOOL_CODE_LENGTH = 3;

    /**
     * Aliases for excluding specific records from the export.
     */
    protected static final String ALIAS_COURSE_LEVEL = "DOE COURSE LEVEL";
    protected static final String ALIAS_COURSE_SEQ = "DOE COURSE SEQ";
    protected static final String ALIAS_DISTRICT_CODE = "DOE DISTRICT CODE";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_STAFF = "DOE EXCLUDE STF";
    protected static final String ALIAS_EXCLUDE_STAFF_SCHEDULE = "DOE EXCLUDE SMT";
    protected static final String ALIAS_GRADE_SPAN = "DOE GRADE SPAN";
    protected static final String ALIAS_ORG_COUNTY_CODE = "DOE COUNTY CODE";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL CODE";
    protected static final String ALIAS_SECTION_ENTRY_DATE = "DOE SECTION ENTRY DATE";
    protected static final String ALIAS_SECTION_EXIT_DATE = "DOE SECTION EXIT DATE";
    protected static final String ALIAS_SCED_CODE = "DOE SCED CODE";

    /**
     * Parameters
     */
    protected static final String PARAM_COURSE_ID = "COURSE-ID";
    protected static final String PARAM_COURSE_LEVEL = "COURSE-LEVEL";
    protected static final String PARAM_COURSE_SEQ = "COURSE-SEQ";
    protected static final String PARAM_CREDIT = "CREDIT";
    protected static final String PARAM_DISTRICT_CODE = "DISTRICT-CODE";
    protected static final String PARAM_GRADESPAN = "GRADESPAN";
    protected static final String PARAM_LOCAL_COURSE_CODE = "LOCAL-CRS-CODE";
    protected static final String PARAM_LOCAL_COURSE_TITLE = "LOCAL-CRS-TITLE";
    protected static final String PARAM_LOCAL_SECTION_CODE = "LOCAL-SECT-CODE";
    protected static final String PARAM_SCHOOL_CODE = "SCHOOL-CODE";
    protected static final String PARAM_SECTION_ENTRY_DATE = "SECTION-ENTRYDATE";
    protected static final String PARAM_SECTION_EXIT_DATE = "SECTION-EXITDATE";
    protected static final String PARAM_SUBJECT_AREA = "SUBJ-AREA";

    /**
     * Other Constants
     */
    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    /**
     * Input parameter: Select organization level.
     */
    protected static final String PARAM_ORGANIZATION = "orgOid";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    protected static final String PARAM_QUERY_BY = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    protected static final String PARAM_QUERY_STRING = "queryString";

    /**
     * Name for the enumerated "requireSced" parameter. The value is an Boolean.
     */
    protected static final String PARAM_EXCLUDE_EMPTY_ENROLLMENT = "excludeEmptyEnroll";

    /**
     * Require SCED Name. The value is an Boolean.
     */
    protected static final String PARAM_REQUIRE_SCED = "requireSced";

    /**
     * Report Date
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    protected static final String PARAM_SORT = "sort";


    /**
     * Instance variables.
     */
    protected String m_courseLevel;
    protected String m_courseSeq;
    protected String m_districtCode;
    protected String m_excludeCourseField = null;
    protected Boolean m_excludeEmptyEnroll = Boolean.valueOf(false);
    protected String m_excludeSchedField = null;
    protected String m_excludeStaffField = null;
    protected String m_gradeSpan;
    protected DataDictionaryField m_orgCountyCode;
    protected String m_orgFieldStr;
    protected String m_orgOid;
    protected PlainDate m_reportDate;
    protected String m_schoolCode;
    protected String m_sectionEntryDate;
    protected String m_sectionExitDate;
    protected String m_subjectAreaCode;

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        // initialize report fields.
        initializeFields();

        /*
         * Build criteria, query and retrievers map.
         */
        if (getSetupErrors().size() == 0) {
            Criteria scheduleCriteria = getScheduleCriteria();
            QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, scheduleCriteria);
            applyInputSort(query, null);
            setQuery(query);
            setEntityClass(StaffCourseRosterEntity.class);

            // Build maps of retriever functionsSTD-SCHOOLCODE
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-SCHOOLCODE", new RetrieveCourseDistrictSchoolCode());
            calcs.put("STD-COURSE-DETAIL", new RetrieveCourseDetails());
            calcs.put("STD-DATE", new RetrieveStaffCourseEntryExitDate());
            calcs.put("STD-SCED", new RetrieveCourseSCEDCode());
            calcs.put("COUNTY-CODE", new RetrieveCountyCode());
            addCalcs(calcs);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_excludeEmptyEnroll = (Boolean) getParameter(PARAM_EXCLUDE_EMPTY_ENROLLMENT);

        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(PARAM_ORGANIZATION);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        // Lookup potential exclude fields. Not required.
        m_excludeCourseField = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, false);
        m_excludeStaffField = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF, false);
        m_excludeSchedField = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF_SCHEDULE, false);
        m_courseLevel = translateAliasToJavaName(ALIAS_COURSE_LEVEL, true);
        m_courseSeq = translateAliasToJavaName(ALIAS_COURSE_SEQ, true);
        m_districtCode = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_gradeSpan = translateAliasToJavaName(ALIAS_GRADE_SPAN, true);
        m_schoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_sectionEntryDate = translateAliasToJavaName(ALIAS_SECTION_ENTRY_DATE, true);
        m_sectionExitDate = translateAliasToJavaName(ALIAS_SECTION_EXIT_DATE, true);
        m_subjectAreaCode = translateAliasToJavaName(ALIAS_SCED_CODE, true);
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_orgCountyCode = dictionary.findDataDictionaryFieldByAlias(ALIAS_ORG_COUNTY_CODE);
        if (m_orgCountyCode == null) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, ALIAS_ORG_COUNTY_CODE);
        }
    }

    /**
     * Returns the criteria that retrieves all school courses that should be
     * included in the export.
     *
     * @return Criteria
     */
    private Criteria getScheduleCriteria() {
        X2Criteria scheduleCriteria = new X2Criteria();
        scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From current year schedule
        scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(PARAM_REQUIRE_SCED);
        if (requireSced != null && requireSced.booleanValue()) {
            scheduleCriteria.addNotEmpty(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_subjectAreaCode,
                    getBroker().getPersistenceKey());
        }

        // Check exclusion flags for staff, course and teacher schedule.
        // Only include Teacher Schedule Section that have a Studetn's assigned to them
        if (null != m_excludeEmptyEnroll && m_excludeEmptyEnroll.booleanValue())

        {
            X2Criteria sscCriteria = new X2Criteria();
            X2Criteria studentScheduleCriteria = new X2Criteria();
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            sscCriteria.addIn(ScheduleTeacher.COL_SECTION_OID,
                    new SubQuery(StudentSchedule.class, StudentSchedule.COL_SECTION_OID, studentScheduleCriteria));

            X2Criteria sccCriteria = new X2Criteria();
            X2Criteria studentScheduleChangeCriteria = new X2Criteria();
            studentScheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            sccCriteria.addIn(ScheduleTeacher.COL_SECTION_OID,
                    new SubQuery(StudentScheduleChange.class,
                            StudentScheduleChange.COL_MASTER_SCHEDULE_OID, studentScheduleChangeCriteria));

            sscCriteria.addOrCriteria(sccCriteria);
            studentScheduleCriteria.addAndCriteria(sscCriteria);
        }
        if (!StringUtils.isEmpty(m_excludeStaffField)) {
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER +
                    m_excludeStaffField,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_excludeCourseField)) {
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_excludeSchedField)) {
            scheduleCriteria.addNotEqualTo(m_excludeSchedField,
                    BooleanAsStringConverter.TRUE);
        }

        applyInputCriteria(scheduleCriteria, false, ScheduleTeacher.REL_STAFF);

        if (isSchoolContext()) {
            scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                scheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            scheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        return scheduleCriteria;
    }
}
