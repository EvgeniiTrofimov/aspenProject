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

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sched.utils.ScheduleMap;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for Class Size Verification report. Any changes to the various enums and the
 * ClassSizeVerificationDataGrid
 * needs to be applied to ClassSizeVerificationExport.
 *
 * @author X2 Development Corporation
 */
public class ClassSizeVerificationData extends ReportJavaSourceNet {
    private static final String ALIAS_STD_PGM_PROGRAM_CODE_ALIAS = "std-pgm-designation-code";
    private static final String ALIAS_STUDENT_DESIGNATION = "std-sped-category";

    /**
     * Fields used in export operations.
     */
    public enum EXPORT_FIELDS {
        FIELD_SCHOOL("school", "School", EnumSet.of(EXPORT_TYPE.REPORT)), FIELD_RUN_DATE("runDate", "Run Date",
                EnumSet.of(EXPORT_TYPE.EXPORT)), FIELD_DISTRICT_NUMER("districtNumber", "District Number",
                        EnumSet.of(EXPORT_TYPE.EXPORT)), FIELD_DISTRICT_NAME("districtName", "District Name",
                                EnumSet.of(EXPORT_TYPE.EXPORT)), FIELD_SCHOOL_CODE("schoolCode", "School Code",
                                        EnumSet.of(EXPORT_TYPE.EXPORT)), FIELD_SCHOOL_NAME("schoolName", "School Name",
                                                EnumSet.of(EXPORT_TYPE.EXPORT)), FIELD_SUBJECT_AREA("subjectArea",
                                                        "Subject Area",
                                                        EnumSet.of(EXPORT_TYPE.REPORT)), FIELD_SUBJECT_CODE(
                                                                "subjectCode", "Subject Code",
                                                                EXPORT_ALL_EXPORTS), FIELD_DIVISION_COURSE_NAME(
                                                                        "divisionCourseName", "Division or Course Name",
                                                                        EXPORT_TYPE_ALL), FIELD_TEACHER_NAME(
                                                                                "teacherName", "Teacher Name",
                                                                                NON_MINISTRY), FIELD_SECTION("section",
                                                                                        "Section",
                                                                                        EXPORT_TYPE_ALL), FIELD_TERM(
                                                                                                "term", "Term",
                                                                                                NON_MINISTRY), FIELD_DAY(
                                                                                                        "day", "Day",
                                                                                                        NON_MINISTRY), FIELD_PERIOD(
                                                                                                                "period",
                                                                                                                "Period",
                                                                                                                NON_MINISTRY), FIELD_GRADE_KH(
                                                                                                                        "gradeKH",
                                                                                                                        "KH",
                                                                                                                        EXPORT_TYPE_ALL), FIELD_GRADE_KF(
                                                                                                                                "gradeKF",
                                                                                                                                "KF",
                                                                                                                                EXPORT_TYPE_ALL), FIELD_GRADE_1(
                                                                                                                                        "grade1",
                                                                                                                                        "1",
                                                                                                                                        EXPORT_TYPE_ALL), FIELD_GRADE_2(
                                                                                                                                                "grade2",
                                                                                                                                                "2",
                                                                                                                                                EXPORT_TYPE_ALL), FIELD_GRADE_3(
                                                                                                                                                        "grade3",
                                                                                                                                                        "3",
                                                                                                                                                        EXPORT_TYPE_ALL), FIELD_GRADE_4(
                                                                                                                                                                "grade4",
                                                                                                                                                                "4",
                                                                                                                                                                EXPORT_TYPE_ALL), FIELD_GRADE_5(
                                                                                                                                                                        "grade5",
                                                                                                                                                                        "5",
                                                                                                                                                                        EXPORT_TYPE_ALL), FIELD_GRADE_6(
                                                                                                                                                                                "grade6",
                                                                                                                                                                                "6",
                                                                                                                                                                                EXPORT_TYPE_ALL), FIELD_GRADE_7(
                                                                                                                                                                                        "grade7",
                                                                                                                                                                                        "7",
                                                                                                                                                                                        EXPORT_TYPE_ALL), FIELD_GRADE_8(
                                                                                                                                                                                                "grade8",
                                                                                                                                                                                                "8",
                                                                                                                                                                                                EXPORT_TYPE_ALL), FIELD_GRADE_9(
                                                                                                                                                                                                        "grade9",
                                                                                                                                                                                                        "9",
                                                                                                                                                                                                        EXPORT_TYPE_ALL), FIELD_GRADE_10(
                                                                                                                                                                                                                "grade10",
                                                                                                                                                                                                                "10",
                                                                                                                                                                                                                EXPORT_TYPE_ALL), FIELD_GRADE_11(
                                                                                                                                                                                                                        "grade11",
                                                                                                                                                                                                                        "11",
                                                                                                                                                                                                                        EXPORT_TYPE_ALL), FIELD_GRADE_12(
                                                                                                                                                                                                                                "grade12",
                                                                                                                                                                                                                                "12",
                                                                                                                                                                                                                                EXPORT_TYPE_ALL), FIELD_SPEC_EDU(
                                                                                                                                                                                                                                        "specialEducation",
                                                                                                                                                                                                                                        "Special Ed",
                                                                                                                                                                                                                                        NON_MINISTRY), FIELD_EA_SUPPORT_PROVIDED(
                                                                                                                                                                                                                                                "eaSupportProvided",
                                                                                                                                                                                                                                                "EA Support Provided",
                                                                                                                                                                                                                                                EnumSet.of(
                                                                                                                                                                                                                                                        EXPORT_TYPE.MINISTRY_EXPORT)), FIELD_IEP(
                                                                                                                                                                                                                                                                "iep",
                                                                                                                                                                                                                                                                "IEP",
                                                                                                                                                                                                                                                                EnumSet.of(
                                                                                                                                                                                                                                                                        EXPORT_TYPE.MINISTRY_EXPORT)), FIELD_GIFTER(
                                                                                                                                                                                                                                                                                "gifter",
                                                                                                                                                                                                                                                                                "Gifted",
                                                                                                                                                                                                                                                                                EXPORT_TYPE_ALL), FIELD_ESL_ESD(
                                                                                                                                                                                                                                                                                        "esl_esd",
                                                                                                                                                                                                                                                                                        "ESL/ESD",
                                                                                                                                                                                                                                                                                        EXPORT_TYPE_ALL), FIELD_RATIONALE(
                                                                                                                                                                                                                                                                                                "rationale",
                                                                                                                                                                                                                                                                                                "Rationale",
                                                                                                                                                                                                                                                                                                EnumSet.of(
                                                                                                                                                                                                                                                                                                        EXPORT_TYPE.MINISTRY_EXPORT)), FIELD_TOTAL_CLASS(
                                                                                                                                                                                                                                                                                                                "totalClass",
                                                                                                                                                                                                                                                                                                                "Total Class",
                                                                                                                                                                                                                                                                                                                NON_MINISTRY);

        private String m_fieldId;
        private String m_fieldName;
        private EnumSet<EXPORT_TYPE> m_fieldType;

        /**
         * Gets the field id.
         *
         * @return String
         */
        public String getFieldId() {
            return m_fieldId;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Gets the field type.
         *
         * @return Enum set
         */
        public EnumSet<EXPORT_TYPE> getFieldType() {
            return m_fieldType;
        }

        /**
         * Instantiates a new export fields.
         *
         * @param fieldId String
         * @param fieldName String
         * @param fieldType EnumSet<EXPORT_TYPE>
         */
        private EXPORT_FIELDS(String fieldId, String fieldName, EnumSet<EXPORT_TYPE> fieldType) {
            m_fieldId = fieldId;
            m_fieldName = fieldName;
            m_fieldType = fieldType;
        }
    }

    /**
     * Enum represents report/export type.
     */
    public enum EXPORT_TYPE {
        REPORT, EXPORT, MINISTRY_EXPORT;

        /**
         * Gets the by ordinal.
         *
         * @param ordinal int
         * @return export type
         */
        public static EXPORT_TYPE getByOrdinal(int ordinal) {
            EXPORT_TYPE exportType = null;

            switch (ordinal) {
                case 0:
                    exportType = EXPORT_TYPE.REPORT;
                    break;
                case 1:
                    exportType = EXPORT_TYPE.EXPORT;
                    break;
                case 2:
                    exportType = EXPORT_TYPE.MINISTRY_EXPORT;
                    break;
            }

            return exportType;
        }
    }

    public static final EnumSet<EXPORT_TYPE> EXPORT_TYPE_ALL =
            EnumSet.of(EXPORT_TYPE.REPORT, EXPORT_TYPE.EXPORT, EXPORT_TYPE.MINISTRY_EXPORT);
    public static final EnumSet<EXPORT_TYPE> EXPORT_ALL_EXPORTS =
            EnumSet.of(EXPORT_TYPE.EXPORT, EXPORT_TYPE.MINISTRY_EXPORT);
    public static final EnumSet<EXPORT_TYPE> NON_MINISTRY = EnumSet.of(EXPORT_TYPE.REPORT, EXPORT_TYPE.EXPORT);

    /**
     * Fields used in row validations.
     */
    public enum VALIDATION_RULES {
        SCHOOL_CODE("School Code (Min Code)", "Must exist on School Table", "Error",
                "Exception handling for this field is handled within the BCeSIS application"), SUBJECT_CODE(
                        "Subject Code", "Cannot be NULL", "Error",
                        "Subject Code '" + SUBJECT_KEYWORD + "'  not found"), CLASS_DIVISION_NAME("Class/Division Name",
                                "Either Course Code or Class / Division Name must be provided", "Error",
                                "Exception handling for this field is handled within the BCeSIS application"), TEACHER(
                                        "Teacher", "Should not be NULL", "Warning",
                                        "Class should be associated with at least one teacher"), COUNT_300_ERROR(
                                                "Headcount", "Must be blank or a whole number between 1 and 300",
                                                "Error",
                                                " headcount must be blank or between 1 and 300"), COUNT_100_WARN(
                                                        "Headcount", "Should be 100 or less", "Warning",
                                                        " headcount should be 100 or less"), COUNT_CLASS_SIZE_WARN(
                                                                "Tally",
                                                                "Should be less than or equal to Total Class Size",
                                                                "Warning",
                                                                " headcount should be less than the Total Class Size"), COUNT_NEEDS_WARN(
                                                                        "Heacount Special Needs",
                                                                        "Should be less than 3", "Warning",
                                                                        "Note: Special Needs  headcount is more than 3");

        private String m_name;
        private String m_rule;
        private String m_type;
        private String m_message;

        /**
         * Gets the message.
         *
         * @param key String
         * @param replacement String
         * @return String
         */
        public String getMessage(String key, String replacement) {
            String message = m_message;

            if (!StringUtils.isEmpty(key)) {
                message = m_message.replaceAll(key, replacement);
            }

            return message;
        }

        /**
         * Gets the name.
         *
         * @return String
         */
        public String getName() {
            return m_name;
        }

        /**
         * Gets the rule.
         *
         * @return String
         */
        public String getRule() {
            return m_rule;
        }

        /**
         * Gets the type.
         *
         * @return String
         */
        public String getType() {
            return m_type;
        }

        /**
         * Instantiates a new validation rules.
         *
         * @param name String
         * @param rule String
         * @param type String
         * @param message String
         */
        private VALIDATION_RULES(String name, String rule, String type, String message) {
            m_name = name;
            m_rule = rule;
            m_type = type;
            m_message = message;
        }
    }

    /**
     * The class generates a grid with counts per section by grade level.
     *
     * @author Follett Software Company
     */
    public class ClassSizeVerificationDataGrid extends ReportDataGrid {
        /*
         * Field aliases
         */
        private static final String ALIAS_EA_SUPPORT = "csk-ea-support";
        private static final String ALIAS_EXCLUDE_COURSE = "csk-exclude-class-size";
        private static final String ALIAS_SUBJECT_CODE = "crs-subject-code";

        private final static String GIFTED_STUDENT_DISABILITY_CODE = "Gifted";

        /*
         * Included grade levels
         */
        private final String[] ALLOWED_GRADE_CODES =
                new String[] {"KH", "KF", "EU", "GA", "SU",
                        "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"};

        /*
         * Exception grid fields
         */
        private static final String VALIDATION_ESL_GROUP = "ESL/ESD";
        private static final String VALIDATION_GIFTED_GROUP = "Gifted";
        private static final String VALIDATION_KF_GROUP = "K Full Day";
        private static final String VALIDATION_KH_GROUP = "K Half Day";
        private static final String VALIDATION_MESSAGE = "message";
        private static final String VALIDATION_NEEDS_GROUP = "Special Needs";
        private static final String VALIDATION_SCHOOL = "school";
        private static final String VALIDATION_SECTION = "section";
        private static final String VALIDATION_HOMEROOM = "homeroom";
        private static final String VALIDATION_TYPE = "type";

        /*
         * SPED category codes
         */
        private final List<String> SPED_CATEGORY_CODES =
                Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H", "K", "Q", "R");
        private final List<String> GIFTED_CATEGORY_CODES = Arrays.asList("P");

        /*
         * Other constants
         */
        private static final String STRING_DELIMITER = ",";
        private static final String ESL_PROGRAM_STATE_CODE = "1700";
        private static final String GRADE_LEVEL_SU = "SU";
        private static final String GRADE_LEVEL_GA = "GA";
        private static final String HEADCOUNT_ESL_ESD = "HESLESD";
        private static final String HEADCOUNT_GIFTED = "HG";
        private static final String HEADCOUNT_GRADE_KINDERGARTEN_HALFDAY = "KH";
        private static final String HEADCOUNT_GRADE_KINDERGARTEN_FULLDAY = "KF";
        private static final int HEADCOUNT_GRADE_INITIAL_1 = 1;
        private static final int HEADCOUNT_GRADE_END_12 = 12;
        private static final String HEADCOUNT_SPECIAL_NEEDS = "HSN";
        private static final String HEADCOUNT_TOTAL = "Total";
        private static final String SCHOOL_LEVEL_MIDDLE = "Middle";

        /*
         * Members
         */
        private X2Broker m_broker;
        private List<ScheduleDay> m_days;
        private PlainDate m_endDate;
        private ReportDataGrid m_exceptionGrid;
        private List<SchedulePeriod> m_periods;
        private Collection<String> m_scheduleTerms;
        private PlainDate m_startDate;
        private Map<String, StudentProgramParticipation> m_studentESLPrograms;
        private Map<String, Collection<StudentSchedule>> m_studentSchedules;

        /*
         * Tracking sections that need to be grouped together
         */
        private List<MasterSchedule> m_processedSections;
        private Map<String, String> m_sectionStdpMap;
        private Map<String, Collection<MasterSchedule>> m_sectionsByStdp;

        /*
         * Reference lookup members
         */
        private ReferenceDescriptionLookup m_referenceLookup;
        private String m_subjectCodeTableOid;

        /*
         * Default types for access in inner classes
         */
        private COURSE_DESCRIPTION_TYPE m_courseDescription = COURSE_DESCRIPTION_TYPE.DISTRICT;
        private Map<String, SisStudent> m_studentDesignationsMap;
        private Map<String, ReferenceCode> m_disabilityCodes;
        private Boolean m_excludeCourses = Boolean.FALSE;
        private EXPORT_TYPE m_exportType;
        private Schedule m_schedule;
        private String m_studentQueryStatus;
        private boolean m_wrapValues = false;

        /**
         * Constructor for the data grid.
         *
         * @param broker X2Broker
         * @param parameterMap Map<String,Object>
         */
        public ClassSizeVerificationDataGrid(X2Broker broker, Map<String, Object> parameterMap) {
            m_broker = broker;
            m_exceptionGrid = new ReportDataGrid(10);

            /*
             * Schedule information
             */
            String scheduleOid = (String) parameterMap.get(PARAM_SCHEDULE);
            m_schedule = (Schedule) broker.getBeanByOid(Schedule.class, scheduleOid);

            String scheduleTermOid = (String) parameterMap.get(PARAM_SCHEDULE_TERM);
            ScheduleTerm scheduleTerm = (ScheduleTerm) broker.getBeanByOid(ScheduleTerm.class, scheduleTermOid);

            ScheduleManager manager = new ScheduleManager(broker);
            m_scheduleTerms = manager.getOverlappedTermOids(scheduleTerm);

            for (ScheduleTermDate date : scheduleTerm.getScheduleTermDates()) {
                if (m_startDate == null || date.getStartDate().before(m_startDate)) {
                    m_startDate = date.getStartDate();
                }

                if (m_endDate == null || date.getEndDate().after(m_endDate)) {
                    m_endDate = date.getEndDate();
                }
            }

            /*
             * Date ranges
             */
            DistrictSchoolYearContext context = m_schedule.getDistrictContext();
            m_startDate = context.getStartDate();
            m_endDate = context.getEndDate();

            /*
             * Map initialization
             */
            m_processedSections = new ArrayList<MasterSchedule>();
            m_sectionStdpMap = new HashMap<String, String>(4096);
            m_sectionsByStdp = new HashMap<String, Collection<MasterSchedule>>(4096);

            initializeReferenceLookups(m_schedule.getSchool().getOrganization1());
            initializeScheduleInfo();

            /*
             * Get the student status to compare against
             */
            STUDENT_STATUS_TYPE studentStatus = STUDENT_STATUS_TYPE.ADMITTED;
            Integer studentStatusValue = (Integer) parameterMap.get(PARAM_STUDENT_STATUS);
            if (studentStatusValue != null) {
                studentStatus = STUDENT_STATUS_TYPE.getByOrdinal(studentStatusValue.intValue());
            }
            m_studentQueryStatus = PreferenceManager.getPreferenceValue(m_schedule.getSchool().getOrganization1(),
                    studentStatus.getStudentStatus());

            /*
             * Determine where to pull the course description from
             */
            Integer courseDescriptionValue = (Integer) parameterMap.get(PARAM_COURSE_DESCRIPTION);
            if (courseDescriptionValue != null) {
                m_courseDescription = COURSE_DESCRIPTION_TYPE.getByOrdinal(courseDescriptionValue.intValue());
            }

            m_excludeCourses = (Boolean) parameterMap.get(PARAM_EXCLUDE_COURSES);

            Integer exportTypeValue = (Integer) parameterMap.get(PARAM_REPORT_EXPORT_TYPE);
            m_exportType = EXPORT_TYPE.getByOrdinal(exportTypeValue.intValue());
            m_wrapValues = EXPORT_TYPE.EXPORT == m_exportType;

            String pgmTblOid =
                    BeanManager.getFullOid(StudentProgramParticipation.DICTIONARY_ID, broker.getPersistenceKey());
            m_disabilityCodes = loadRefCodes(m_broker, pgmTblOid, StudentProgramParticipation.COL_PROGRAM_CODE);
            loadDesignations(true);
        }

        /**
         * Loads the reference code.
         *
         * @param broker X2Broker
         * @param tblOid String
         * @param columnJavaName String
         * @return Map
         */
        private Map loadRefCodes(X2Broker broker, String tblOid, String columnJavaName) {
            Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();

            X2Criteria extendedProgramFieldCriteria = new X2Criteria();
            extendedProgramFieldCriteria.addEqualTo(ExtendedDataField.COL_ALIAS, ALIAS_STD_PGM_PROGRAM_CODE_ALIAS);
            extendedProgramFieldCriteria.addEqualTo(ExtendedDataField.REL_DATA_FIELD_CONFIG + PATH_DELIMITER +
                    DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_DATA_TABLE_OID, tblOid);
            extendedProgramFieldCriteria.addEqualTo(ExtendedDataField.REL_DATA_FIELD_CONFIG + PATH_DELIMITER +
                    DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME, columnJavaName);

            QueryByCriteria extendedProgramFieldQuery =
                    new QueryByCriteria(ExtendedDataField.class, extendedProgramFieldCriteria);

            ExtendedDataField pgmCodeExtended = (ExtendedDataField) broker.getBeanByQuery(extendedProgramFieldQuery);
            if (pgmCodeExtended != null && !StringUtils.isEmpty(pgmCodeExtended.getReferenceTableOid())) {
                X2Criteria refCodeCriteria = new X2Criteria();
                refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID,
                        pgmCodeExtended.getReferenceTableOid());
                QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
                refCodes = broker.getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 100);
            } else {
                AppGlobals.getLog().log(Level.SEVERE, "Reference table not found for designation code");
            }

            return refCodes;
        }

        /**
         * Populate grid with appropriate data according business logic.
         */
        public void evaluateGrid() {
            if (SCHOOL_LEVEL_MIDDLE.equals(m_schedule.getSchool().getSchoolLevelCode())) {
                evaluateMiddleSchool();
            } else {
                evaluateGeneralSchool();
            }

            filterFields();
        }

        /**
         * Returns the exception grid.
         *
         * @return ReportDataGrid
         */
        public ReportDataGrid getExceptionGrid() {
            m_exceptionGrid.beforeTop();

            return m_exceptionGrid;
        }

        /**
         * Sorts the exception grid. The grid is sorted by course description/section number then
         * error type. Warnings
         * are displayed before errors.
         */
        public void sortExceptionGrid() {
            String[] columns = new String[] {VALIDATION_SECTION + PATH_DELIMITER + MasterSchedule.COL_DESCRIPTION,
                    VALIDATION_SECTION + PATH_DELIMITER + MasterSchedule.COL_SECTION_NUMBER,
                    VALIDATION_HOMEROOM,
                    VALIDATION_TYPE};

            Boolean[] sortOrders = new Boolean[] {Boolean.TRUE, Boolean.TRUE, Boolean.TRUE, Boolean.FALSE};

            m_exceptionGrid.sort(Arrays.asList(columns), Arrays.asList(sortOrders), true);
        }

        /**
         * Prepare one row for grid based on argument passed.
         *
         * @param section MasterSchedule
         */
        private void addGeneralRow(MasterSchedule section) {
            append();

            SchoolCourse schoolCourse = section.getSchoolCourse();
            Course course = schoolCourse == null ? null : schoolCourse.getCourse();
            Course ministryCourse = course == null ? null : course.getRootCourse();
            SisSchool school = section.getSchedule().getSchool();

            /*
             * Determine if teacher has multiple course sections for same term based on STDP value
             */
            String stdp = m_sectionStdpMap.get(section.getOid());
            Collection<MasterSchedule> relatedSections = m_sectionsByStdp.get(stdp);
            if (relatedSections == null) {
                relatedSections = new ArrayList<MasterSchedule>();
                relatedSections.add(section);
            }

            /*
             * Track included sections
             */
            for (MasterSchedule includedSection : relatedSections) {
                m_processedSections.add(includedSection);
            }

            /*
             * Set initial identifying fields
             */
            set(EXPORT_FIELDS.FIELD_RUN_DATE.getFieldId(), new SimpleDateFormat("dd-MMM-yyyy").format(new PlainDate()));
            set(EXPORT_FIELDS.FIELD_DISTRICT_NUMER.getFieldId(), school.getParentOrganization().getId());
            set(EXPORT_FIELDS.FIELD_DISTRICT_NAME.getFieldId(), school.getParentOrganization().getName());
            set(EXPORT_FIELDS.FIELD_SCHOOL_CODE.getFieldId(), school.getSchoolId());
            set(EXPORT_FIELDS.FIELD_SCHOOL_NAME.getFieldId(), school.getName());
            set(EXPORT_FIELDS.FIELD_SCHOOL.getFieldId(), school);

            /*
             * Set the staff display - <teacher last name>, <teacher first name> <teacher local ID>
             */
            SisStaff staff = section.getPrimaryStaff();
            if (staff != null) {
                String display = staff.getNameView() + " " + staff.getLocalId();
                set(EXPORT_FIELDS.FIELD_TEACHER_NAME.getFieldId(), wrap(display));
            }

            /*
             * Set ministry course elements
             */
            if (ministryCourse != null) {
                String subjectCode = (String) ministryCourse.getFieldValueByAlias(ALIAS_SUBJECT_CODE);
                set(EXPORT_FIELDS.FIELD_SUBJECT_CODE.getFieldId(), wrap(subjectCode));

                String subjectArea = getSubjectArea(subjectCode);
                set(EXPORT_FIELDS.FIELD_SUBJECT_AREA.getFieldId(), wrap(subjectArea));
            }

            /*
             * Set school course specific elements
             */
            if (schoolCourse != null) {
                String support = (String) schoolCourse.getFieldValueByAlias(ALIAS_EA_SUPPORT);
                set(EXPORT_FIELDS.FIELD_EA_SUPPORT_PROVIDED.getFieldId(),
                        BooleanAsStringConverter.TRUE.equals(support) ? "Y" : "N");
            }

            /*
             * Set the division course name display
             */
            StringBuilder divisionCourseNames = new StringBuilder();
            for (MasterSchedule relatedSection : relatedSections) {
                String divisionCourseName = "";

                if (m_courseDescription == COURSE_DESCRIPTION_TYPE.DISTRICT) {
                    divisionCourseName = relatedSection.getSchoolCourse().getCourse().getDescription();
                } else {
                    divisionCourseName = relatedSection.getSchoolCourse().getDescription();
                }

                // set delimiter
                if (divisionCourseNames.length() > 0 && !divisionCourseName.isEmpty()) {
                    divisionCourseNames.append(STRING_DELIMITER);
                    divisionCourseNames.append(" ");
                }

                divisionCourseNames.append(divisionCourseName);
                divisionCourseNames.append(" (");
                divisionCourseNames.append(relatedSection.getSectionNumber());
                divisionCourseNames.append(")");
            }

            /*
             * Only set course names if a value is present. For ministry export length cannot exceed
             * 250 characters
             */
            if (divisionCourseNames.length() > 0) {
                String courseNames = divisionCourseNames.toString();

                if (EXPORT_TYPE.MINISTRY_EXPORT == m_exportType && courseNames.length() > 250) {
                    courseNames = StringUtils.padRight(courseNames, 250);
                }

                set(EXPORT_FIELDS.FIELD_DIVISION_COURSE_NAME.getFieldId(), wrap(courseNames));
            }

            /*
             * Set section-related elements
             */
            set(EXPORT_FIELDS.FIELD_SECTION.getFieldId(), String.valueOf(relatedSections.size()));
            set(EXPORT_FIELDS.FIELD_TERM.getFieldId(), section.getTermView());
            setDayPeriodFields(section);

            /*
             * Load statistics
             */
            Map<String, Integer> studentClassStatistic = getClassStudentsStatistic(relatedSections);
            calculateClassSizeTotal(studentClassStatistic);

            set(EXPORT_FIELDS.FIELD_GRADE_KH.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_GRADE_KINDERGARTEN_HALFDAY)));
            set(EXPORT_FIELDS.FIELD_GRADE_KF.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_GRADE_KINDERGARTEN_FULLDAY)));

            for (int index = HEADCOUNT_GRADE_INITIAL_1; index <= HEADCOUNT_GRADE_END_12; index++) {
                set("grade" + index, String.valueOf(studentClassStatistic.get(String.valueOf(index))));
            }

            /*
             * Set statistic elements
             */
            set(EXPORT_FIELDS.FIELD_SPEC_EDU.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_SPECIAL_NEEDS)));
            set(EXPORT_FIELDS.FIELD_GIFTER.getFieldId(), String.valueOf(studentClassStatistic.get(HEADCOUNT_GIFTED)));
            set(EXPORT_FIELDS.FIELD_ESL_ESD.getFieldId(), String.valueOf(studentClassStatistic.get(HEADCOUNT_ESL_ESD)));
            set(EXPORT_FIELDS.FIELD_TOTAL_CLASS.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_TOTAL)));
            set(EXPORT_FIELDS.FIELD_IEP.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_SPECIAL_NEEDS)));
        }

        /**
         * Prepare one row based on the passed homeroom information.
         *
         * @param homeroom String
         * @param students Collection<SisStudent>
         * @param staffList Collection<SisStaff>
         */
        private void addMiddleRow(String homeroom, Collection<SisStudent> students, Collection<SisStaff> staffList) {
            append();

            SisSchool school = m_schedule.getSchool();

            /*
             * Set initial identifying fields
             */
            set(EXPORT_FIELDS.FIELD_RUN_DATE.getFieldId(), new SimpleDateFormat("dd-MMM-yyyy").format(new PlainDate()));
            set(EXPORT_FIELDS.FIELD_DISTRICT_NUMER.getFieldId(), school.getParentOrganization().getId());
            set(EXPORT_FIELDS.FIELD_DISTRICT_NAME.getFieldId(), school.getParentOrganization().getName());
            set(EXPORT_FIELDS.FIELD_SCHOOL_CODE.getFieldId(), school.getSchoolId());
            set(EXPORT_FIELDS.FIELD_SCHOOL_NAME.getFieldId(), school.getName());
            set(EXPORT_FIELDS.FIELD_SCHOOL.getFieldId(), school);
            set(EXPORT_FIELDS.FIELD_SUBJECT_CODE.getFieldId(), "Full Grade");
            set(EXPORT_FIELDS.FIELD_SUBJECT_AREA.getFieldId(), "Full Grade");
            set(EXPORT_FIELDS.FIELD_DIVISION_COURSE_NAME.getFieldId(),
                    wrap("Division " + StringUtils.coalesce(homeroom, "<none>")));

            /*
             * Set teacher display
             */
            List<String> staffDisplays = new LinkedList<>();
            if (!CollectionUtils.isEmpty(staffList)) {
                // <teacher last name>, <teacher first name> <teacher local ID>
                for (SisStaff staff : staffList) {
                    String staffDisplay = staff.getNameView() + " " + staff.getLocalId();
                    staffDisplays.add(staffDisplay);
                }
            }
            set(EXPORT_FIELDS.FIELD_TEACHER_NAME.getFieldId(),
                    wrap(StringUtils.convertCollectionToDelimitedString(staffDisplays, "; ")));

            /*
             * Load statistics
             */
            Map<String, Integer> studentClassStatistic = getHomeroomStudentsStatistic(homeroom, students, staffList);
            calculateClassSizeTotal(studentClassStatistic);

            set(EXPORT_FIELDS.FIELD_GRADE_KH.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_GRADE_KINDERGARTEN_HALFDAY)));
            set(EXPORT_FIELDS.FIELD_GRADE_KF.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_GRADE_KINDERGARTEN_FULLDAY)));

            for (int index = HEADCOUNT_GRADE_INITIAL_1; index <= HEADCOUNT_GRADE_END_12; index++) {
                set("grade" + index, String.valueOf(studentClassStatistic.get(String.valueOf(index))));
            }

            /*
             * Set statistic elements
             */
            set(EXPORT_FIELDS.FIELD_SPEC_EDU.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_SPECIAL_NEEDS)));
            set(EXPORT_FIELDS.FIELD_GIFTER.getFieldId(), String.valueOf(studentClassStatistic.get(HEADCOUNT_GIFTED)));
            set(EXPORT_FIELDS.FIELD_ESL_ESD.getFieldId(), String.valueOf(studentClassStatistic.get(HEADCOUNT_ESL_ESD)));
            set(EXPORT_FIELDS.FIELD_TOTAL_CLASS.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_TOTAL)));
            set(EXPORT_FIELDS.FIELD_IEP.getFieldId(),
                    String.valueOf(studentClassStatistic.get(HEADCOUNT_SPECIAL_NEEDS)));
        }

        /**
         * Adds the staff to the collection for the homeroom.
         *
         * @param homeroomMap Map<String,Collection<SisStaff>>
         * @param staff SisStaff
         * @param homeroom String
         */
        private void addStaffToList(Map<String, Collection<SisStaff>> homeroomMap, SisStaff staff, String homeroom) {
            if (!StringUtils.isEmpty(homeroom)) {
                Collection<SisStaff> staffList = homeroomMap.get(homeroom);

                if (staffList == null) {
                    staffList = new LinkedList<>();
                    homeroomMap.put(homeroom, staffList);
                }

                staffList.add(staff);
            }
        }

        /**
         * Appends a row to the validation grid with the passed information.
         *
         * @param target Object
         * @param rule VALIDATION_RULES
         * @param messagePrefix String
         */
        private void addValidation(Object target, VALIDATION_RULES rule, String messagePrefix) {
            m_exceptionGrid.append();
            m_exceptionGrid.set(VALIDATION_MESSAGE, messagePrefix + rule.getMessage("", ""));
            m_exceptionGrid.set(VALIDATION_TYPE, rule.getType());
            m_exceptionGrid.set(VALIDATION_SCHOOL, m_schedule.getSchool());

            if (target instanceof String) {
                m_exceptionGrid.set(VALIDATION_HOMEROOM, target);
            } else if (target instanceof MasterSchedule) {
                m_exceptionGrid.set(VALIDATION_SECTION, target);
            }
        }

        /**
         * Appends a row to the validation grid with the passed information. Allows for modification
         * of the rule's
         * message.
         *
         * @param target Object
         * @param rule VALIDATION_RULES
         * @param messagePrefix String
         * @param key String
         * @param replacement String
         */
        private void addValidation(Object target,
                                   VALIDATION_RULES rule,
                                   String messagePrefix,
                                   String key,
                                   String replacement) {
            m_exceptionGrid.append();
            m_exceptionGrid.set(VALIDATION_MESSAGE, messagePrefix + rule.getMessage(key, replacement));
            m_exceptionGrid.set(VALIDATION_TYPE, rule.getType());
            m_exceptionGrid.set(VALIDATION_SCHOOL, m_schedule.getSchool());

            if (target instanceof String) {
                m_exceptionGrid.set(VALIDATION_HOMEROOM, target);
            } else if (target instanceof MasterSchedule) {
                m_exceptionGrid.set(VALIDATION_SECTION, target);
            }
        }

        /**
         * Builds the staff homeroom map.
         *
         * @return Map
         */
        private Map<String, Collection<SisStaff>> buildStaffHomeroomMap() {
            Map<String, Collection<SisStaff>> homeroomMap = new HashMap<>(2048);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, m_schedule.getSchoolOid());

            String activeCode = PreferenceManager.getPreferenceValue(m_schedule.getSchool().getParentOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
            criteria.addEqualTo(SisStaff.COL_STATUS, activeCode);

            QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
            QueryIterator iterator = m_broker.getIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    SisStaff staff = (SisStaff) iterator.next();
                    String homeroom1 = staff.getHomeroom();
                    String homeroom2 = staff.getHomeroom2();

                    addStaffToList(homeroomMap, staff, homeroom1);
                    addStaffToList(homeroomMap, staff, homeroom2);
                }
            } finally {
                iterator.close();
            }

            return homeroomMap;
        }

        /**
         * Calculate total value for class size statistic.
         *
         * @param classSizeMap Map<String,Integer>
         */
        private void calculateClassSizeTotal(Map<String, Integer> classSizeMap) {
            int total = 0;

            Collection<String> groupsToIgnore = Arrays.asList(HEADCOUNT_ESL_ESD,
                    HEADCOUNT_GIFTED,
                    HEADCOUNT_SPECIAL_NEEDS,
                    HEADCOUNT_TOTAL);
            for (String groupKey : classSizeMap.keySet()) {
                if (!groupsToIgnore.contains(groupKey)) {
                    total += classSizeMap.get(groupKey).intValue();
                }
            }

            classSizeMap.put(HEADCOUNT_TOTAL, Integer.valueOf(total));
        }

        /**
         * Process the section to determine its STDP key and add the values to the appropriate maps
         * for future lookup.
         *
         * @param section MasterSchedule
         */
        private void calculateStdp(MasterSchedule section) {
            String staffOid = section.getPrimaryStaffOid();
            String term = section.getTermView();
            String dayString = "";
            String periodString = "";

            /*
             * Get day and period
             */
            ScheduleMap sectionMap = section.getScheduleMap(m_broker);

            boolean foundFirstScheduledSlot = false;
            for (ScheduleDay day : m_days) {
                for (SchedulePeriod period : m_periods) {
                    if (sectionMap.isScheduled(section.getScheduleTerm().getBaseTermMap(), day.getNumber(),
                            period.getNumber())) {
                        foundFirstScheduledSlot = true;

                        dayString = day.getId();
                        periodString = period.getId();

                        /*
                         * Calculate STDP and assign to maps
                         */
                        String stdp = staffOid + STRING_DELIMITER + term + STRING_DELIMITER + dayString
                                + STRING_DELIMITER + periodString;
                        m_sectionStdpMap.put(section.getOid(), stdp);

                        Collection<MasterSchedule> stdpSections = m_sectionsByStdp.get(stdp);
                        if (stdpSections == null) {
                            stdpSections = new LinkedList<>();
                            m_sectionsByStdp.put(stdp, stdpSections);
                        }

                        stdpSections.add(section);

                        if (foundFirstScheduledSlot) {
                            break;
                        }
                    }
                }

                if (foundFirstScheduledSlot) {
                    break;
                }
            }
        }

        /**
         * Executes the Class Size Verification for non-middle schools. This looks at sections and
         * student schedule counts.
         */
        private void evaluateGeneralSchool() {
            String excludeCoursesPath = null;
            if (m_excludeCourses.booleanValue()) {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
                DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_EXCLUDE_COURSE);
                if (field != null) {
                    excludeCoursesPath = field.getJavaName();
                }
            }

            /*
             * Preload Student Schedules
             */
            BeanQuery studentQuery = new BeanQuery(StudentSchedule.class,
                    getSectionCriteria(StudentSchedule.REL_SECTION + PATH_DELIMITER, excludeCoursesPath));
            m_studentSchedules =
                    m_broker.getGroupedCollectionByQuery(studentQuery, StudentSchedule.COL_SECTION_OID, 1000);

            /*
             * Process sections
             */
            Criteria sectionCriteria = getSectionCriteria("", excludeCoursesPath);
            BeanQuery sectionQuery = new BeanQuery(MasterSchedule.class, sectionCriteria);

            /*
             * Order based on selected course description
             */
            if (m_courseDescription == COURSE_DESCRIPTION_TYPE.DISTRICT) {
                sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        Course.COL_DESCRIPTION);
            } else {
                sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.COL_DESCRIPTION);
            }

            /*
             * Iterate over included sections to calculate their STDP (staff-term-day-period) for
             * grouping.
             */
            Collection<MasterSchedule> sections = m_broker.getCollectionByQuery(sectionQuery);

            for (MasterSchedule section : sections) {
                calculateStdp(section);
            }

            for (MasterSchedule section : sections) {
                if (!m_processedSections.contains(section)) {
                    addGeneralRow(section);
                }
            }
        }

        /**
         * Executes the Class Size Verification for middle schools. This looks at students grouped
         * by homeroom.
         */
        private void evaluateMiddleSchool() {
            // Load staff by homeroom
            Map<String, Collection<SisStaff>> staffHomeroomMap = buildStaffHomeroomMap();

            // Query for students and group by homeroom
            Criteria criteria = new Criteria();
            criteria.addEqualTo(Student.COL_SCHOOL_OID, m_schedule.getSchoolOid());
            criteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, m_studentQueryStatus);

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            query.addOrderByAscending(SisStudent.COL_HOMEROOM);

            // Iterate over homerooms and add to grid
            Map<String, Collection<SisStudent>> homeroomMap =
                    m_broker.getGroupedCollectionByQuery(query, SisStudent.COL_HOMEROOM, 1048);
            for (Entry<String, Collection<SisStudent>> mapEntry : homeroomMap.entrySet()) {
                String homeroom = mapEntry.getKey();

                addMiddleRow(homeroom, mapEntry.getValue(), staffHomeroomMap.get(homeroom));
            }
        }

        /**
         * Clean up fields from grid according report/export type.
         */
        private void filterFields() {
            for (EXPORT_FIELDS exportField : EXPORT_FIELDS.values()) {
                if (!exportField.getFieldType().contains(m_exportType)) {
                    deleteColumn(exportField.getFieldId());
                }
            }
        }

        /**
         * Calculate class headcount for appropriate section.
         *
         * @param sections Collection<MasterSchedule>
         * @return Map
         */
        private Map<String, Integer> getClassStudentsStatistic(Collection<MasterSchedule> sections) {
            Map<String, Integer> classStatistic = getDefaultStudentStatistic();

            for (MasterSchedule section : sections) {
                Map<String, Integer> sectionStatistic = getDefaultStudentStatistic();
                Collection<StudentSchedule> studentSchedules = m_studentSchedules.get(section.getOid());

                if (studentSchedules != null) {
                    for (StudentSchedule studentSchedule : studentSchedules) {
                        SisStudent student = studentSchedule.getStudent();
                        SchoolCourse course = section.getSchoolCourse();
                        String studentGradeLevel = getStudentGradeLevel(student);
                        String courseGradeLevel = course.getGradeLevel();

                        if (studentGradeLevel != null) {
                            if (studentGradeLevel.equalsIgnoreCase(GRADE_LEVEL_GA)
                                    || studentGradeLevel.equalsIgnoreCase(GRADE_LEVEL_SU)) {
                                incrementStatistic(classStatistic, courseGradeLevel);
                                incrementStatistic(sectionStatistic, courseGradeLevel);
                            } else {
                                incrementStatistic(classStatistic, studentGradeLevel);
                                incrementStatistic(sectionStatistic, studentGradeLevel);

                                if (isSpecialNeeds(student)) {
                                    incrementStatistic(classStatistic, HEADCOUNT_SPECIAL_NEEDS);
                                    incrementStatistic(sectionStatistic, HEADCOUNT_SPECIAL_NEEDS);
                                } else if (isESLESD(student)) {
                                    incrementStatistic(classStatistic, HEADCOUNT_ESL_ESD);
                                    incrementStatistic(sectionStatistic, HEADCOUNT_ESL_ESD);
                                } else if (isGifted(student)) {
                                    incrementStatistic(classStatistic, HEADCOUNT_GIFTED);
                                    incrementStatistic(sectionStatistic, HEADCOUNT_GIFTED);
                                }
                            }
                        }
                    }
                }

                validateSection(section, sectionStatistic);
            }

            return classStatistic;
        }

        /**
         * Populate class headcount statistic with default values.
         *
         * @return Map<String, Integer>
         */
        private Map<String, Integer> getDefaultStudentStatistic() {
            LinkedHashMap<String, Integer> studentStatistic = new LinkedHashMap<String, Integer>();
            studentStatistic.put(HEADCOUNT_GRADE_KINDERGARTEN_HALFDAY, Integer.valueOf(0));
            studentStatistic.put(HEADCOUNT_GRADE_KINDERGARTEN_FULLDAY, Integer.valueOf(0));

            for (int index = HEADCOUNT_GRADE_INITIAL_1; index <= HEADCOUNT_GRADE_END_12; index++) {
                studentStatistic.put(String.valueOf(index), Integer.valueOf(0));
            }

            studentStatistic.put(HEADCOUNT_SPECIAL_NEEDS, Integer.valueOf(0));
            studentStatistic.put(HEADCOUNT_GIFTED, Integer.valueOf(0));
            studentStatistic.put(HEADCOUNT_ESL_ESD, Integer.valueOf(0));
            studentStatistic.put(HEADCOUNT_TOTAL, Integer.valueOf(0));

            return studentStatistic;
        }

        /**
         * Calculate class headcount for the list of students for a homeroom.
         *
         * @param homeroom String
         * @param students Collection<SisStudent>
         * @param staffList Collection<SisStaff>
         * @return Map<String, Integer>
         */
        private Map<String, Integer> getHomeroomStudentsStatistic(String homeroom,
                                                                  Collection<SisStudent> students,
                                                                  Collection<SisStaff> staffList) {
            Map<String, Integer> homeroomStatistic = getDefaultStudentStatistic();
            for (SisStudent student : students) {
                String studentGradeLevel = getStudentGradeLevel(student);

                if (studentGradeLevel != null) {
                    if (!studentGradeLevel.equalsIgnoreCase(GRADE_LEVEL_GA)
                            && !studentGradeLevel.equalsIgnoreCase(GRADE_LEVEL_SU)) {
                        incrementStatistic(homeroomStatistic, studentGradeLevel);

                        if (isSpecialNeeds(student)) {
                            incrementStatistic(homeroomStatistic, HEADCOUNT_SPECIAL_NEEDS);
                        } else if (isESLESD(student)) {
                            incrementStatistic(homeroomStatistic, HEADCOUNT_ESL_ESD);
                        } else if (isGifted(student)) {
                            incrementStatistic(homeroomStatistic, HEADCOUNT_GIFTED);
                        }
                    }
                }
            }

            validateHomeroom(homeroom, homeroomStatistic, staffList);

            return homeroomStatistic;
        }

        /**
         * Loads the program codes that have a state code designating them as ESL codes.
         *
         * @return Collection<String>
         */
        private Collection<String> getEslPrograms() {
            Collection<String> codes = new LinkedList<>();

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                    StudentProgramParticipation.COL_PROGRAM_CODE);

            if (field != null) {
                String tableOid = field.getReferenceTableOid();
                if (!StringUtils.isEmpty(tableOid)) {
                    Criteria criteria = new Criteria();
                    criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, tableOid);
                    criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, ESL_PROGRAM_STATE_CODE);

                    SubQuery codeQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                    codes = m_broker.getSubQueryCollectionByQuery(codeQuery);
                }
            }

            if (CollectionUtils.isEmpty(codes)) {
                codes.add("***No match code***");
            }

            return codes;
        }

        /**
         * Returns criteria related to Master Schedule records. Currently made to support queries
         * against
         * Master Schedule or Student Schedule. If other classes are to be added will need to make
         * an adjustment.
         *
         * @param sectionPath String
         * @param excludeCoursesPath String
         * @return Criteria
         */
        private Criteria getSectionCriteria(String sectionPath, String excludeCoursesPath) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(sectionPath + MasterSchedule.COL_SCHEDULE_OID, m_schedule.getOid());
            criteria.addIn(sectionPath + MasterSchedule.COL_SCHEDULE_TERM_OID, m_scheduleTerms);
            criteria.addEqualTo(
                    sectionPath + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                    SchoolCourse.MASTER_TYPE_CLASS);

            if (!StringUtils.isEmpty(sectionPath)) {
                criteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.COL_ENROLLMENT_STATUS,
                        m_studentQueryStatus);
            }

            if (excludeCoursesPath != null) {
                criteria.addNotEqualTo(
                        sectionPath + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + excludeCoursesPath,
                        BooleanAsStringConverter.TRUE);
            }

            return criteria;
        }

        /**
         * Get student grade level or null otherwise.
         *
         * @param student SisStudent
         * @return String
         */
        private String getStudentGradeLevel(SisStudent student) {
            String actualGradeLevel = null;

            if (student != null) {
                String studentGradeLevel = student.getGradeLevel();
                if (Arrays.asList(ALLOWED_GRADE_CODES).contains(studentGradeLevel)) {
                    int intStudentGradeLevel = -1;
                    try {
                        intStudentGradeLevel = Integer.parseInt(studentGradeLevel);
                    } catch (NumberFormatException ex) {
                        // ignore exception
                    }

                    if (intStudentGradeLevel != -1) {
                        actualGradeLevel = String.valueOf(intStudentGradeLevel);
                    } else {
                        actualGradeLevel = studentGradeLevel;
                    }
                }
            }

            return actualGradeLevel;
        }

        /**
         * Looks up the subject area (reference code description) based on the passed subject code.
         *
         * @param subjectCode String
         * @return String
         */
        private String getSubjectArea(String subjectCode) {
            String area = "";

            if (!StringUtils.isEmpty(subjectCode) && !StringUtils.isEmpty(m_subjectCodeTableOid)) {
                String description = m_referenceLookup.getDescription(m_subjectCodeTableOid, subjectCode);
                if (description.length() > 2) {
                    area = description;
                }
            }

            return area;
        }

        /**
         * Increment headcount value for appropriate grade level (defined by key).
         *
         * @param studentStatistic Map<String,Integer>
         * @param key String
         */
        private void incrementStatistic(Map<String, Integer> studentStatistic, String key) {
            if (key != null && studentStatistic.containsKey(key)) {
                int headcount = studentStatistic.get(key).intValue();
                studentStatistic.put(key, Integer.valueOf(headcount + 1));
            }
        }

        /**
         * Initializes the lookup information for the reference codes used in the export.
         *
         * @param organization Organization
         */
        private void initializeReferenceLookups(Organization organization) {
            m_referenceLookup = new ReferenceDescriptionLookup(m_broker, organization);

            /*
             * Subject codes
             */
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SUBJECT_CODE);
            if (field != null) {
                m_subjectCodeTableOid = field.getReferenceTableOid();
            }
        }

        /**
         * Sets schedule constants for lookup when adding sections to grid.
         */
        private void initializeScheduleInfo() {
            /*
             * Load days
             */
            Criteria dayCriteria = new Criteria();
            dayCriteria.addEqualTo(ScheduleDay.COL_SCHEDULE_OID, m_schedule.getTimeScheduleOid());

            QueryByCriteria dayQuery = new QueryByCriteria(ScheduleDay.class, dayCriteria);
            dayQuery.addOrderByAscending(ScheduleDay.COL_NUMBER);

            m_days = (List<ScheduleDay>) m_broker.getCollectionByQuery(dayQuery);

            /*
             * Load periods
             */
            Criteria periodCriteria = new Criteria();
            periodCriteria.addEqualTo(SchedulePeriod.COL_SCHEDULE_OID, m_schedule.getTimeScheduleOid());

            QueryByCriteria periodQuery = new QueryByCriteria(SchedulePeriod.class, periodCriteria);
            periodQuery.addOrderByAscending(SchedulePeriod.COL_NUMBER);

            m_periods = (List<SchedulePeriod>) m_broker.getCollectionByQuery(periodQuery);
        }

        /**
         * Does student have special ESl/ESD program applied?.
         *
         * @param student SisStudent
         * @return boolean
         */
        private boolean isESLESD(SisStudent student) {
            if (m_studentESLPrograms == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getEslPrograms());
                criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_endDate);

                X2Criteria criteriaEndDateIsGE = new X2Criteria();
                criteriaEndDateIsGE.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_startDate);

                X2Criteria criteriaEndDate = new X2Criteria();
                criteriaEndDate.addEmpty(StudentProgramParticipation.COL_END_DATE, m_broker.getPersistenceKey());
                criteriaEndDate.addOrCriteria(criteriaEndDateIsGE);

                criteria.addAndCriteria(criteriaEndDate);

                m_studentESLPrograms =
                        m_broker.getMapByQuery(new BeanQuery(StudentProgramParticipation.class, criteria),
                                StudentProgramParticipation.COL_STUDENT_OID, 1000);
            }

            StudentProgramParticipation program = m_studentESLPrograms.get(student.getOid());

            return program == null ? false : true;
        }

        /**
         * Does student has gifted special codes?.
         *
         * @param student SisStudent
         * @return boolean
         */
        private boolean isGifted(SisStudent student) {
            String code = getDesignationStateCode(student);

            return GIFTED_CATEGORY_CODES.contains(code);
        }

        /**
         * Does special needs applied for student?.
         *
         * @param student SisStudent
         * @return boolean
         */
        private boolean isSpecialNeeds(SisStudent student) {
            String code = getDesignationStateCode(student);

            return SPED_CATEGORY_CODES.contains(code);
        }

        /**
         * Returns the disability code.
         *
         * @param student SisStudent
         * @return String
         */
        private String getDesignationStateCode(SisStudent student) {
            String designationCode = "";

            String designation = m_studentDesignationsMap.containsKey(student.getOid())
                    ? (String) student.getFieldValueByAlias(ALIAS_STUDENT_DESIGNATION) : null;
            if (!StringUtils.isEmpty(designation) && m_disabilityCodes.containsKey(designation)) {
                ReferenceCode disabilityRef = m_disabilityCodes.get(designation);
                designationCode = disabilityRef.getStateCode();
            }

            return designationCode;
        }

        /**
         * Loads the disability code map.
         *
         * @param includeGifted boolean
         */
        public void loadDesignations(boolean includeGifted) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            DataDictionaryField studentDesigField =
                    dictionary.findDataDictionaryFieldByAlias(ALIAS_STUDENT_DESIGNATION);
            if (studentDesigField != null) {
                X2Criteria studentCriteria = new X2Criteria();
                studentCriteria.addNotEmpty(studentDesigField.getJavaName(), m_broker.getPersistenceKey());

                if (!includeGifted) {
                    studentCriteria.addNotEqualTo(studentDesigField.getJavaName(), GIFTED_STUDENT_DISABILITY_CODE);
                }

                /*
                 * Load the designation for primary students.
                 */
                X2Criteria primaryCriteria = new X2Criteria();
                primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, m_schedule.getSchoolOid());
                primaryCriteria.addNotNull(SisStudent.COL_SCHOOL_OID);
                primaryCriteria.addAndCriteria(studentCriteria);

                QueryByCriteria primaryQuery = new QueryByCriteria(SisStudent.class, primaryCriteria);
                m_studentDesignationsMap = m_broker.getMapByQuery(primaryQuery, X2BaseBean.COL_OID, 1000);

                /*
                 * Load the designation for secondary students.
                 */
                X2Criteria secondaryCriteria = new X2Criteria();
                secondaryCriteria.addEqualTo(
                        SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_SCHOOL_OID,
                        m_schedule.getSchoolOid());
                secondaryCriteria.addNotNull(SisStudent.COL_SCHOOL_OID);

                secondaryCriteria.addEqualTo(
                        SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_DISTRICT_CONTEXT_OID,
                        m_schedule.getDistrictContextOid());
                secondaryCriteria.addEqualTo(SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_TYPE,
                        Integer.valueOf(StudentSchool.SECONDARY));

                QueryByCriteria secondaryQuery = new QueryByCriteria(SisStudent.class, secondaryCriteria);
                m_studentDesignationsMap.putAll(m_broker.getMapByQuery(secondaryQuery, X2BaseBean.COL_OID, 1000));
            }
        }

        /**
         * Parses the section's schedule display to get the day/period display (the first ID of
         * each).
         *
         * @param section void
         */
        private void setDayPeriodFields(MasterSchedule section) {
            String stdp = m_sectionStdpMap.get(section.getOid());

            if (!StringUtils.isEmpty(stdp)) {
                List<String> components = StringUtils.convertDelimitedStringToList(stdp, STRING_DELIMITER);

                if (components.size() == 4) {
                    String day = components.get(2);
                    String period = components.get(3);

                    set(EXPORT_FIELDS.FIELD_DAY.getFieldId(), day);
                    set(EXPORT_FIELDS.FIELD_PERIOD.getFieldId(), period);
                }
            }
        }

        /**
         * Validate the head count for the group. All tallies use the same basic validation.
         *
         * @param count int
         * @param target Object
         * @param group String
         */
        private void validateCount(int count, Object target, String group) {
            // Determine the enrollment total if the validation target is a Section
            int enrollmentTotal = 0;
            if (target instanceof MasterSchedule) {
                enrollmentTotal = ((MasterSchedule) target).getEnrollmentTotal();
            }

            if (count > 300) {
                addValidation(target, VALIDATION_RULES.COUNT_300_ERROR, group + " ");
            } else if (count > 100 && !group.equals(VALIDATION_ESL_GROUP) &&
                    !group.equals(VALIDATION_GIFTED_GROUP) && !group.equals(VALIDATION_NEEDS_GROUP)) {
                addValidation(target, VALIDATION_RULES.COUNT_100_WARN, group + " ");
            } else if (enrollmentTotal > 0 && count >= enrollmentTotal &&
                    (group.equals(VALIDATION_ESL_GROUP) || group.equals(VALIDATION_GIFTED_GROUP)
                            || group.equals(VALIDATION_NEEDS_GROUP))) {
                addValidation(target, VALIDATION_RULES.COUNT_CLASS_SIZE_WARN, group + " ");
            } else if (count > 3 && group.equals(VALIDATION_NEEDS_GROUP)) {
                addValidation(target, VALIDATION_RULES.COUNT_NEEDS_WARN, "");
            }
        }

        /**
         * Validates the homeroom information based on the validation rules.
         *
         * @param homeroom String
         * @param statistic Map<String,Integer>
         * @param staffList Collection<SisStaff>
         */
        private void validateHomeroom(String homeroom, Map<String, Integer> statistic, Collection<SisStaff> staffList) {
            // Check school code
            String schoolCode = m_schedule.getSchool().getSchoolId();
            if (StringUtils.isEmpty(schoolCode)) {
                addValidation(homeroom, VALIDATION_RULES.SCHOOL_CODE, "");
            }

            // Check class/division name
            if (StringUtils.isEmpty(homeroom)) {
                addValidation(homeroom, VALIDATION_RULES.CLASS_DIVISION_NAME, "");
            }

            // Check Teacher
            if (CollectionUtils.isEmpty(staffList)) {
                addValidation(homeroom, VALIDATION_RULES.TEACHER, "");
            }

            // Check K-Half/Full headcounts
            int count = statistic.get(HEADCOUNT_GRADE_KINDERGARTEN_HALFDAY).intValue();
            validateCount(count, homeroom, VALIDATION_KH_GROUP);

            count = statistic.get(HEADCOUNT_GRADE_KINDERGARTEN_FULLDAY).intValue();
            validateCount(count, homeroom, VALIDATION_KF_GROUP);

            // Check grade 1-12 headcounts
            for (int index = HEADCOUNT_GRADE_INITIAL_1; index <= HEADCOUNT_GRADE_END_12; index++) {
                String key = String.valueOf(index);

                count = statistic.get(key).intValue();
                validateCount(count, homeroom, "Grade " + key);
            }

            // Check special headcount
            count = statistic.get(HEADCOUNT_SPECIAL_NEEDS).intValue();
            validateCount(count, homeroom, VALIDATION_NEEDS_GROUP);

            // Check gifted headcount
            count = statistic.get(HEADCOUNT_GIFTED).intValue();
            validateCount(count, homeroom, VALIDATION_GIFTED_GROUP);

            // Check ESL headcount
            count = statistic.get(HEADCOUNT_ESL_ESD).intValue();
            validateCount(count, homeroom, VALIDATION_ESL_GROUP);
        }

        /**
         * Validates the section according to the validation rules.
         *
         * @param section MasterSchedule
         * @param sectionStatistic Map<String,Integer>
         */
        private void validateSection(MasterSchedule section, Map<String, Integer> sectionStatistic) {
            SchoolCourse schoolCourse = section.getSchoolCourse();
            Course course = schoolCourse.getCourse();
            Course rootCourse = course.getRootCourse();

            // Check school code
            String schoolCode = section.getSchedule().getSchool().getSchoolId();
            if (StringUtils.isEmpty(schoolCode)) {
                addValidation(section, VALIDATION_RULES.SCHOOL_CODE, "");
            }

            // Check subject code
            String subjectCode = (String) rootCourse.getFieldValueByAlias(ALIAS_SUBJECT_CODE);

            subjectCode = StringUtils.coalesce(subjectCode, "");
            String subjectArea = getSubjectArea(subjectCode);

            if (StringUtils.isEmpty(subjectArea)) {
                addValidation(section, VALIDATION_RULES.SUBJECT_CODE, "", SUBJECT_KEYWORD, subjectCode);
            }

            // Check class/division name
            String divisionCourseName = "";
            if (m_courseDescription == COURSE_DESCRIPTION_TYPE.DISTRICT) {
                divisionCourseName = course.getDescription();
            } else {
                divisionCourseName = schoolCourse.getDescription();
            }

            if (StringUtils.isEmpty(divisionCourseName)) {
                addValidation(section, VALIDATION_RULES.CLASS_DIVISION_NAME, "");
            }

            // Check Teacher
            SisStaff staff = section.getPrimaryStaff();
            if (staff == null) {
                addValidation(section, VALIDATION_RULES.TEACHER, "");
            }

            // Check K-Half/Full headcounts
            int count = sectionStatistic.get(HEADCOUNT_GRADE_KINDERGARTEN_HALFDAY).intValue();
            validateCount(count, section, VALIDATION_KH_GROUP);

            count = sectionStatistic.get(HEADCOUNT_GRADE_KINDERGARTEN_FULLDAY).intValue();
            validateCount(count, section, VALIDATION_KF_GROUP);

            // Check grade 1-12 headcounts
            for (int index = HEADCOUNT_GRADE_INITIAL_1; index <= HEADCOUNT_GRADE_END_12; index++) {
                String key = String.valueOf(index);

                count = sectionStatistic.get(key).intValue();
                validateCount(count, section, "Grade " + key);
            }

            // Check special headcount
            count = sectionStatistic.get(HEADCOUNT_SPECIAL_NEEDS).intValue();
            validateCount(count, section, VALIDATION_NEEDS_GROUP);

            // Check gifted headcount
            count = sectionStatistic.get(HEADCOUNT_GIFTED).intValue();
            validateCount(count, section, VALIDATION_GIFTED_GROUP);

            // Check ESL headcount
            count = sectionStatistic.get(HEADCOUNT_ESL_ESD).intValue();
            validateCount(count, section, VALIDATION_ESL_GROUP);
        }

        /**
         * Wraps the value in double-quotes if this is being run as an export and the value contains
         * a comma.
         *
         * @param value String
         * @return String
         */
        private String wrap(String value) {
            String formattedValue = value;

            if (m_wrapValues && !StringUtils.isEmpty(formattedValue) && formattedValue.contains(STRING_DELIMITER)) {
                formattedValue = "\"" + formattedValue + "\"";
            }

            return formattedValue;
        }
    }

    /**
     * The Enum STUDENT_STATUS_TYPE.
     */
    enum STUDENT_STATUS_TYPE {
        ADMITTED(SisPreferenceConstants.STUDENT_PREREG_CODE), REGISTERED(
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        private String m_studentStatus;

        /**
         * Instantiates a new student status type.
         *
         * @param studentStatus String
         */
        STUDENT_STATUS_TYPE(String studentStatus) {
            m_studentStatus = studentStatus;
        }

        /**
         * Gets the student status.
         *
         * @return String
         */
        public String getStudentStatus() {
            return m_studentStatus;
        }

        /**
         * Gets the by ordinal.
         *
         * @param ordinal int
         * @return student status type
         */
        public static STUDENT_STATUS_TYPE getByOrdinal(int ordinal) {
            STUDENT_STATUS_TYPE studentStatusType = null;

            switch (ordinal) {
                case 0:
                    studentStatusType = STUDENT_STATUS_TYPE.ADMITTED;
                    break;
                case 1:
                    studentStatusType = STUDENT_STATUS_TYPE.REGISTERED;
                    break;
            }

            return studentStatusType;
        }
    }

    /**
     * The Enum COURSE_DESCRIPTION_TYPE.
     */
    enum COURSE_DESCRIPTION_TYPE {
        DISTRICT, SCHOOL;

        /**
         * Gets the by ordinal.
         *
         * @param ordinal int
         * @return course description type
         */
        public static COURSE_DESCRIPTION_TYPE getByOrdinal(int ordinal) {
            COURSE_DESCRIPTION_TYPE courseDescriptionType = null;

            switch (ordinal) {
                case 0:
                    courseDescriptionType = COURSE_DESCRIPTION_TYPE.DISTRICT;
                    break;
                case 1:
                    courseDescriptionType = COURSE_DESCRIPTION_TYPE.SCHOOL;
                    break;
            }

            return courseDescriptionType;
        }
    }

    /*
     * Input parameters
     */
    public static final String PARAM_EXCLUDE_COURSES = "excludeCourses";
    public static final String PARAM_REPORT_EXPORT_TYPE = "exportType";
    public static final String PARAM_SCHEDULE = "scheduleOid";
    public static final String PARAM_SCHEDULE_TERM = "scheduleTermOid";
    public static final String PARAM_STUDENT_STATUS = "studentStatus";
    public static final String PARAM_COURSE_DESCRIPTION = "courseDescription";

    /*
     * Sub-report constants
     */
    private static final String COL_SUBREPORT_DATA_EXPORT = "subreportExportData";
    private static final String COL_SUBREPORT_DATA_EXCEPTIONS = "subreportExceptionsData";
    private static final String REPORT_FORMAT_EXPORT = "subreportExportFormat";
    private static final String REPORT_FORMAT_EXCEPTIONS = "subreportExceptionsFormat";
    private static final String SUBREPORT_ID_EXPORT = "BC-CLS-001-EXP";
    private static final String SUBREPORT_ID_EXCEPTIONS = "BC-CLS-001-EXC";

    /*
     * Other constants
     */
    private static final String SUBJECT_KEYWORD = ":SUBJECT:";

    /**
     * Default constructor for use in tools that use this as an external source.
     */
    public ClassSizeVerificationData() {
        super();
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        ClassSizeVerificationDataGrid exportGrid = new ClassSizeVerificationDataGrid(getBroker(), getParameters());
        exportGrid.evaluateGrid();
        exportGrid.sortExceptionGrid();
        ReportDataGrid exceptionsGrid = exportGrid.getExceptionGrid();

        exceptionsGrid.beforeTop();
        exportGrid.beforeTop();

        grid.append();
        grid.set(COL_SUBREPORT_DATA_EXPORT, exportGrid);
        grid.set(COL_SUBREPORT_DATA_EXCEPTIONS, exceptionsGrid);

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        collectReportEntities();
    }

    /**
     * Loading appropriate sub-report formats.
     */
    private void collectReportEntities() {
        // Collect report entities
        Report exportSubreport = ReportUtils.getReport(SUBREPORT_ID_EXPORT, getBroker());
        Report exceptionsSubreport = ReportUtils.getReport(SUBREPORT_ID_EXCEPTIONS, getBroker());

        // Send them to report parameters as byte-streams
        addParameter(REPORT_FORMAT_EXPORT, new ByteArrayInputStream(exportSubreport.getCompiledFormat()));
        addParameter(REPORT_FORMAT_EXCEPTIONS, new ByteArrayInputStream(exceptionsSubreport.getCompiledFormat()));
    }
}
