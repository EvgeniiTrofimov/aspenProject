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
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "SADE" report.
 *
 * @author X2 Development Corporation
 */
public class StudentAchievementDataExtractData extends ReportJavaSourceNet {

    /**
     * Fields used in export operations / reported.
     */
    enum EXPORT_FIELDS {
        FIELD_SCHOOL_ID("colSchoolID", "School", true), FIELD_PEN("colPen", "PEN", true), FIELD_PUPIL_NO("colPupilNo",
                "Pupil No", false), FIELD_STUDENT_NAME("colStudentName", "Student Name", false), FIELD_COURSE_CODE(
                        "colCourseCode", "Course Code", true), FIELD_SCHOOL_COURSE_CODE("colSchoolCourseCode",
                                "School Course Code", false), FIELD_SCHOOL_COURSE_DESCRIPTION("colCourseDescription",
                                        "School Course Description", false), FIELD_ACTIVE_DATE("colActiveDate",
                                                "Active Date", true), FIELD_CLASS_IDENTIFIER("colClassIdentifier",
                                                        "Class Identifier", true), FIELD_CREDITS("colPotentialCredits",
                                                                "Credits", true), FIELD_SCHOOL_MARK_TYPE(
                                                                        "colSchoolMarkType", "School Mark Type",
                                                                        true), FIELD_SCHOOL_MARK_VALUE(
                                                                                "colSchoolMarkValue",
                                                                                "School Mark Value",
                                                                                true), FIELD_EQUIVALENCY(
                                                                                        "colEquivalencyChallange",
                                                                                        "Equivalency / Challenge",
                                                                                        true), FIELD_COMPLETION_DATE(
                                                                                                "colCompletionDate",
                                                                                                "Completion Date",
                                                                                                true);

        private String m_fieldId;
        private String m_fieldName;
        private boolean m_inExtract;

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
         * Gets the in extract.
         *
         * @return boolean
         */
        public boolean getInExtract() {
            return m_inExtract;
        }

        /**
         * Instantiates a new export fields.
         *
         * @param fieldId String
         * @param fieldName String
         * @param inExtract boolean
         */
        private EXPORT_FIELDS(String fieldId, String fieldName, boolean inExtract) {
            m_fieldId = fieldId;
            m_fieldName = fieldName;
            m_inExtract = inExtract;
        }
    }

    /**
     * Generate data set that can be used for export and reporting.
     * This class is common and is copied to other SADE classes
     *
     * @author X2 Development Corporation
     */
    static class SADEReportDataGrid extends ReportDataGrid {
        /*
         * Transcript aliases
         */
        private static final String ALIAS_COURSE_ACTIVE_DATE = "trn-course-active-date";
        private static final String ALIAS_COMPLETION_DATE = "trn-completion-date";
        private static final String ALIAS_END_DATE = "trn-end-date";
        private static final String ALIAS_START_DATE = "trn-start-date";
        private static final String ALIAS_TRAX_OVERRIDE = "trn-trax-override";
        private static final String ALIAS_TRAX_CODE = "rcd-exam-trax";

        /*
         * Organization aliases
         */
        private static final String ALIAS_SADE_CURRENT_START_DATE = "org-sade-date-start_current";
        private static final String ALIAS_SADE_CURRENT_END_DATE = "org-sade-date-end_current";
        private static final String ALIAS_SADE_PREVIOUS_START_DATE = "org-sade-date-start_previous";
        private static final String ALIAS_SADE_PREVIOUS_END_DATE = "org-sade-date-end_previous";

        /*
         * Other aliases
         */
        private static final String ALIAS_EXTERNAL_CODE = "crs-external-code";

        /*
         * General constants
         */
        private static final String COURSE_CODE_PREFIX = "J";
        private static final String DL_SCHOOL_TYPE = "DL";
        private static final String ORGANIZATION_ID_200 = "200";
        private static final String PERIOD_TYPE_CURRENT = "C";
        private static final String PERIOD_TYPE_PREVIOUS = "P";
        public static final String PERIOD_TYPE_COLUMN = "periodType";
        private static final String STUDENT_OID_COLUMN = "studentOid";

        /*
         * Mark type grades
         */
        private static final String[] FOUR_POINT_GRADES = new String[] {"01", "02", "03"};
        private static final String[] THREE_POINT_GRADES = new String[] {"KF"};

        /*
         * Formatting constants
         */
        private DecimalFormat m_bigDecimalFormatter = new DecimalFormat("###0");
        private DateAsStringConverter m_converter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, Locale.getDefault(), true);

        /*
         * Members
         */
        private X2Broker m_broker;
        private StudentContextReportHelper m_contextHelper;
        private int m_currentRecords;
        private int m_currentStudents;
        private DataDictionary m_dictionary;
        private SimpleDateFormat m_formatter;
        private Collection<String> m_gradeLevelsFourPoint;
        private Collection<String> m_gradeLevelsThreePoint;
        private boolean m_gridLimitGrades = false;
        private Collection<SisSchool> m_gridSchools;
        private boolean m_includePrevious;
        private Collection<String> m_limitedGradeList;
        private int m_previousRecords;
        private int m_previousStudents;
        private int m_totalStudents;

        /*
         * SADE date ranges
         */
        private PlainDate m_currentEndDate;
        private PlainDate m_currentStartDate;
        private PlainDate m_previousEndDate;
        private PlainDate m_previousStartDate;

        /*
         * Reference lookups
         */
        private Collection<String> m_allowedTypes;
        private Map<String, DataDictionary> m_extendedReferenceDictionaries;
        private Collection<String> m_localCourseTypes;
        private Map<String, ReferenceCode> m_traxCodes;

        /*
         * Schedule data
         */
        private Map<String, List<String>> m_dayIdMap;
        private Map<String, Boolean> m_periodFirstDisplay;
        private Map<String, List<String>> m_periodIdMap;
        private Map<String, Collection<String>> m_sectionsByStudent;
        private Map<String, KeyValuePair<PlainDate, PlainDate>> m_termDateRanges;

        /**
         * Instantiates SADE report grid.
         *
         * @param helper StudentContextReportHelper
         * @param broker X2Broker
         */
        public SADEReportDataGrid(StudentContextReportHelper helper, X2Broker broker) {
            m_broker = broker;
            m_contextHelper = helper;
            m_dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            m_formatter = new SimpleDateFormat("dd-MMM-yy");
            m_gradeLevelsFourPoint = Arrays.asList(FOUR_POINT_GRADES);
            m_gradeLevelsThreePoint = Arrays.asList(THREE_POINT_GRADES);

            /*
             * Load reference lookups
             */
            m_extendedReferenceDictionaries = new HashMap<String, DataDictionary>(64);
            m_traxCodes = loadCodeMap(m_dictionary.findDataDictionaryFieldByAlias(ALIAS_TRAX_OVERRIDE));
            loadAllowedCourseTypes();
            loadLocalCourseTypes();

            /*
             * Initialize schedule data maps
             */
            m_dayIdMap = new HashMap<String, List<String>>(2048);
            m_periodFirstDisplay = new HashMap<String, Boolean>(2048);
            m_periodIdMap = new HashMap<String, List<String>>(2048);
            m_sectionsByStudent = new HashMap<String, Collection<String>>(2048);
            m_termDateRanges = new HashMap<String, KeyValuePair<PlainDate, PlainDate>>(2048);

            /*
             * Initialize counts (set to -1 for error checking)
             */
            m_totalStudents = -1;
            m_currentStudents = -1;
            m_previousStudents = -1;
            m_currentRecords = -1;
            m_previousRecords = -1;
        }

        /**
         * Populates the grid with information for the schools and grade term provided.
         */
        public void evaluateGrid() {
            if (m_gridSchools != null) {
                Collection<String> schoolOids = getOidList(m_gridSchools);

                processTranscripts(schoolOids);
                processSchedules(schoolOids);
            }
        }

        /**
         * Returns the current period's end date of the SADE extract based on the organization.
         *
         * @return PlainDate
         */
        public PlainDate getCurrentEndDate() {
            return m_currentEndDate;
        }

        /**
         * Provides the count of current records represented in the grid.
         *
         * @return int
         */
        public int getCurrentRecordCount() {
            return m_currentRecords;
        }

        /**
         * Provides the count of current term students represented in the grid.
         *
         * @return int
         */
        public int getCurrentStudentCount() {
            return m_currentStudents;
        }

        /**
         * Returns the current period's start date of the SADE extract based on the organization.
         *
         * @return PlainDate
         */
        public PlainDate getCurrentStartDate() {
            return m_currentStartDate;
        }

        /**
         * Returns the previous period's end date of the SADE extract based on the organization.
         *
         * @return PlainDate
         */
        public PlainDate getPreviousEndDate() {
            return m_previousEndDate;
        }

        /**
         * Provides the count of previous records represented in the grid.
         *
         * @return int
         */
        public int getPreviousRecordCount() {
            return m_previousRecords;
        }

        /**
         * Returns the previous period's start date of the SADE extract based on the organization.
         *
         * @return PlainDate
         */
        public PlainDate getPreviousStartDate() {
            return m_previousStartDate;
        }

        /**
         * Provides the count of previous term students represented in the grid.
         *
         * @return int
         */
        public int getPreviousStudentCount() {
            return m_previousStudents;
        }

        /**
         * Provides the count of distinct students represented in the grid.
         *
         * @return int
         */
        public int getTotalStudentCount() {
            return m_totalStudents;
        }

        /**
         * Organization setter. Also pulls the SADE start/end dates.
         *
         * @param organization Organization
         */
        public void initializeDateRanges(Organization organization) {
            Organization rootOrganization = organization.getRootOrganization();
            m_currentStartDate = parseSystemDate(
                    (String) rootOrganization.getFieldValueByAlias(SADEReportDataGrid.ALIAS_SADE_CURRENT_START_DATE));
            m_currentEndDate = parseSystemDate(
                    (String) rootOrganization.getFieldValueByAlias(SADEReportDataGrid.ALIAS_SADE_CURRENT_END_DATE));
            m_previousStartDate = parseSystemDate(
                    (String) rootOrganization.getFieldValueByAlias(SADEReportDataGrid.ALIAS_SADE_PREVIOUS_START_DATE));
            m_previousEndDate = parseSystemDate(
                    (String) rootOrganization.getFieldValueByAlias(SADEReportDataGrid.ALIAS_SADE_PREVIOUS_END_DATE));
        }

        /**
         * Iterate through the grid and remove any invalid rows. Invalid rows include:
         * <ul>
         * <li>Entries without an active date
         * </ul>
         * Once complete the grid will be set to "beforeTop".
         */
        public void removeInvalidRows() {
            beforeTop();

            while (next()) {
                Object activeDate = get(EXPORT_FIELDS.FIELD_ACTIVE_DATE.getFieldId());
                if (activeDate == null) {
                    deleteRow();
                }
            }

            beforeTop();
        }

        /**
         * Sets the date formatter with the desired pattern.
         *
         * @param pattern void
         */
        public void setDateFormat(String pattern) {
            m_formatter = new SimpleDateFormat(pattern);
        }

        /**
         * Flags export whether or not the previous date range needs to be included.
         *
         * @param include void
         */
        public void setIncludePrevious(boolean include) {
            m_includePrevious = include;
        }

        /**
         * Provides a list of grades to limit the transcript selection for the grid.
         *
         * @param limitedGradeList void
         */
        public void setLimitedGradeList(Collection<String> limitedGradeList) {
            m_limitedGradeList = limitedGradeList;
        }

        /**
         * Sets the limit grade list flag which determines if the grid is limited to a list of grade
         * levels.
         *
         * @param limitGrades void
         */
        public void setLimitGrades(Boolean limitGrades) {
            m_gridLimitGrades = limitGrades.booleanValue();
        }

        /**
         * School setter used when the grid is created for a single school.
         *
         * @param school void
         */
        public void setSchool(SisSchool school) {
            m_gridSchools = new LinkedList<SisSchool>();
            m_gridSchools.add(school);
        }

        /**
         * School setter used when the grid is created for a list of schools.
         *
         * @param schools void
         */
        public void setSchools(Collection<SisSchool> schools) {
            m_gridSchools = schools;
        }

        /**
         * If necessary, iterate over the grid and tally the student/record counts. When complete
         * the grid will be
         * set to "beforeTop".
         */
        public void tallyCounts() {
            if (m_totalStudents == -1 || m_currentStudents == -1 || m_previousStudents == -1 ||
                    m_currentRecords == -1 || m_previousRecords == -1) {
                m_currentRecords = 0;
                m_previousRecords = 0;

                Set<String> distinctStudents = new HashSet<String>();
                Set<String> distinctStudentsCurrent = new HashSet<String>();
                Set<String> distinctStudentsPrevious = new HashSet<String>();

                beforeTop();

                while (next()) {
                    String type = (String) get(PERIOD_TYPE_COLUMN);

                    distinctStudents.add((String) get(STUDENT_OID_COLUMN));

                    if (PERIOD_TYPE_PREVIOUS.equals(type)) {
                        m_previousRecords++;
                        distinctStudentsPrevious.add((String) get(STUDENT_OID_COLUMN));
                    } else {
                        m_currentRecords++;
                        distinctStudentsCurrent.add((String) get(STUDENT_OID_COLUMN));
                    }
                }

                beforeTop();

                m_totalStudents = distinctStudents.size();
                m_currentStudents = distinctStudentsCurrent.size();
                m_previousStudents = distinctStudentsPrevious.size();
            }
        }

        /**
         * Adds the data as a new row in the grid.
         *
         * @param school School
         * @param student SisStudent
         * @param schedule StudentSchedule
         * @param course SchoolCourse
         * @param periodType String
         */
        private void addSADERow(School school,
                                SisStudent student,
                                StudentSchedule schedule,
                                SchoolCourse course,
                                String periodType) {
            append();

            if (school != null) {
                set(EXPORT_FIELDS.FIELD_SCHOOL_ID.getFieldId(),
                        String.format("%8s", school.getSchoolId()).replace(' ', '0'));
            }

            if (student != null) {
                set(EXPORT_FIELDS.FIELD_PEN.getFieldId(), student.getStateId());
                set(EXPORT_FIELDS.FIELD_PUPIL_NO.getFieldId(), student.getLocalId());
                set(EXPORT_FIELDS.FIELD_STUDENT_NAME.getFieldId(), student.getNameView());
            }

            if (course != null) {
                set(EXPORT_FIELDS.FIELD_SCHOOL_COURSE_CODE.getFieldId(), course.getNumber());
                set(EXPORT_FIELDS.FIELD_SCHOOL_COURSE_DESCRIPTION.getFieldId(), course.getDescription());

                Course rootCourse = course.getCourse().getRootCourse();
                set(EXPORT_FIELDS.FIELD_COURSE_CODE.getFieldId(), getCourseCode(rootCourse));
                set(EXPORT_FIELDS.FIELD_CREDITS.getFieldId(), formatCredits(rootCourse));
            }

            MasterSchedule masterSchedule = schedule.getSection();
            set(EXPORT_FIELDS.FIELD_CLASS_IDENTIFIER.getFieldId(), COURSE_CODE_PREFIX + getClassId(masterSchedule));

            /*
             * Get Active/Completion dates
             */
            PlainDate activeDate = getTermDate(masterSchedule.getScheduleTerm(), true);
            if (activeDate != null) {
                set(EXPORT_FIELDS.FIELD_ACTIVE_DATE.getFieldId(), m_formatter.format(activeDate));
            }

            PlainDate completionDate = getTermDate(masterSchedule.getScheduleTerm(), false);
            if (completionDate != null) {
                set(EXPORT_FIELDS.FIELD_COMPLETION_DATE.getFieldId(), m_formatter.format(completionDate));
            }

            /*
             * Additional fields
             */
            set(PERIOD_TYPE_COLUMN, periodType);
            set(STUDENT_OID_COLUMN, student.getOid());

            /*
             * If the completion date is greater than the reporting period end date, SADE extract
             * should display the
             * Completion Date, Mark Type and Mark Value as NULL (blank).
             */
            PlainDate reportEnd = m_currentEndDate;
            if (PERIOD_TYPE_PREVIOUS.equals(periodType)) {
                reportEnd = m_previousEndDate;
            }

            if (completionDate != null && completionDate.after(reportEnd)) {
                set(EXPORT_FIELDS.FIELD_COMPLETION_DATE.getFieldId(), "");
                set(EXPORT_FIELDS.FIELD_SCHOOL_MARK_TYPE.getFieldId(), "");
                set(EXPORT_FIELDS.FIELD_SCHOOL_MARK_VALUE.getFieldId(), "");
            }
        }

        /**
         * Adds the data as a new row in the grid.
         *
         * @param school School
         * @param student SisStudent
         * @param transcript Transcript
         * @param course SchoolCourse
         * @param periodType String
         */
        private void addSADERow(School school,
                                SisStudent student,
                                Transcript transcript,
                                SchoolCourse course,
                                String periodType) {
            append();

            if (school != null) {
                set(EXPORT_FIELDS.FIELD_SCHOOL_ID.getFieldId(),
                        String.format("%8s", school.getSchoolId()).replace(' ', '0'));
            }

            if (student != null) {
                set(EXPORT_FIELDS.FIELD_PEN.getFieldId(), student.getStateId());
                set(EXPORT_FIELDS.FIELD_PUPIL_NO.getFieldId(), student.getLocalId());
                set(EXPORT_FIELDS.FIELD_STUDENT_NAME.getFieldId(), student.getNameView());
            }

            if (course != null) {
                set(EXPORT_FIELDS.FIELD_SCHOOL_COURSE_CODE.getFieldId(), course.getNumber());
                set(EXPORT_FIELDS.FIELD_SCHOOL_COURSE_DESCRIPTION.getFieldId(), course.getDescription());

                Course rootCourse = course.getCourse().getRootCourse();
                set(EXPORT_FIELDS.FIELD_COURSE_CODE.getFieldId(), getCourseCode(rootCourse));
                set(EXPORT_FIELDS.FIELD_CREDITS.getFieldId(), formatCredits(rootCourse));
            }

            MasterSchedule masterSchedule = transcript.getMasterSchedule();
            if (masterSchedule != null) {
                set(EXPORT_FIELDS.FIELD_CLASS_IDENTIFIER.getFieldId(), COURSE_CODE_PREFIX + getClassId(masterSchedule));
            }

            /*
             * Get Active/Completion dates
             */
            PlainDate activeDate = getTranscriptStartDate(transcript);
            if (activeDate != null) {
                set(EXPORT_FIELDS.FIELD_ACTIVE_DATE.getFieldId(), m_formatter.format(activeDate));
            }

            PlainDate completionDate = getTranscriptEndDate(transcript);
            if (completionDate != null) {
                set(EXPORT_FIELDS.FIELD_COMPLETION_DATE.getFieldId(), m_formatter.format(completionDate));
            }

            /*
             * Set mark and mark type fields
             */
            String schoolMarkValue = transcript.getFinalGrade();
            String schoolMarkType = getMarkType(schoolMarkValue, course);

            set(EXPORT_FIELDS.FIELD_SCHOOL_MARK_TYPE.getFieldId(), schoolMarkType);
            set(EXPORT_FIELDS.FIELD_SCHOOL_MARK_VALUE.getFieldId(), schoolMarkValue);

            /*
             * Pull Equivalency/Challenge value based on the trax override field
             */
            String trax = (String) transcript.getFieldValueByAlias(ALIAS_TRAX_OVERRIDE);
            set(EXPORT_FIELDS.FIELD_EQUIVALENCY.getFieldId(), lookupTraxValue(trax));

            /*
             * Additional fields
             */
            set(PERIOD_TYPE_COLUMN, periodType);
            set(STUDENT_OID_COLUMN, student.getOid());

            /*
             * If the completion date is greater than the reporting period end date, SADE extract
             * should display the
             * Completion Date, Mark Type and Mark Value as NULL (blank).
             */
            PlainDate reportEnd = m_currentEndDate;
            if (PERIOD_TYPE_PREVIOUS.equals(periodType)) {
                reportEnd = m_previousEndDate;
            }

            if (completionDate != null && completionDate.after(reportEnd)) {
                set(EXPORT_FIELDS.FIELD_COMPLETION_DATE.getFieldId(), "");
                set(EXPORT_FIELDS.FIELD_SCHOOL_MARK_TYPE.getFieldId(), "");
                set(EXPORT_FIELDS.FIELD_SCHOOL_MARK_VALUE.getFieldId(), "");
            }
        }

        /**
         * Returns the appropriate mark type based on BC's rules:
         *
         * 1. For students enrolled in course grade of 4-12 and SU, display the Mark Type as "2" if
         * the grade is
         * numeric, otherwise display as "1".
         * 2. For course grades 1-3, display the Mark Type as "4".
         * 3. For course grade KF, display the Mark Type as "3".
         *
         * @param markValue String
         * @param schoolCourse SchoolCourse
         * @return String 1-4
         */
        private String getMarkType(String markValue, SchoolCourse schoolCourse) {
            String type = null;

            if (!StringUtils.isEmpty(markValue)) {
                String courseGrade = schoolCourse.getGradeLevel();
                if (m_gradeLevelsFourPoint.contains(courseGrade)) {
                    type = "4";
                } else if (m_gradeLevelsThreePoint.contains(courseGrade)) {
                    type = "3";
                } else {
                    type = !isNumeric(markValue) ? "1" : "2";
                }
            }

            return type;
        }

        /**
         * Only include courses with a course type of:
         * <ul>
         * <li>AP
         * <li>BA
         * <li>CP
         * <li>EC
         * <li>IB
         * <li>LD
         * <li>MF
         * <li>MI
         * <li>PA
         * <li>PS.
         *
         * @param criteria X2Criteria
         * @param coursePrefix String
         */
        private void applyCourseTypeRestriction(X2Criteria criteria, String coursePrefix) {
            // Need to check both enterprise level-course and district-level course
            Criteria districtCriteria = new Criteria();
            districtCriteria.addIn(coursePrefix + SchoolCourse.REL_COURSE + PATH_DELIMITER + Course.COL_FIELD_B002,
                    m_allowedTypes);

            Criteria enterpriseCriteria = new Criteria();
            enterpriseCriteria.addIn(coursePrefix + SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    Course.REL_PARENT_COURSE + PATH_DELIMITER + Course.COL_FIELD_B002, m_allowedTypes);

            enterpriseCriteria.addOrCriteria(districtCriteria);
            criteria.addAndCriteria(enterpriseCriteria);
        }

        /**
         * Apply the date range criteria to the transcript records (CURRENT vs. CURRENT AND
         * PREVIOUS)
         * <br>
         * Course data will be included in the extract if:
         * <ul>
         * <li>The start date <= Reporting Period End Date
         * <li>The end date is NULL or end date >= Reporting Period Start Date
         * </ul>
         *
         * @param transcriptCriteria X2Criteria
         */
        private void applyDateRangeCriteria(X2Criteria transcriptCriteria) {
            X2Criteria currentCriteria = new X2Criteria();

            /*
             * Only include transcripts with a valid start/end date
             */
            DataDictionaryField fieldActiveDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_ACTIVE_DATE);
            DataDictionaryField fieldStartDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_START_DATE);

            DataDictionaryField fieldCompletionDate =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_COMPLETION_DATE);
            DataDictionaryField fieldEndDate = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_END_DATE);

            /*
             * If no fields are defined with the start/end date then do not include any results
             */
            if (fieldActiveDate == null && fieldStartDate == null && fieldCompletionDate == null
                    && fieldEndDate == null) {
                currentCriteria.addEqualTo(X2BaseBean.COL_OID, "nomatch");
            } else {
                applyStartDateCriteria(currentCriteria, fieldActiveDate, fieldStartDate, m_currentEndDate);
                applyEndDateCriteria(currentCriteria, fieldCompletionDate, fieldEndDate, m_currentStartDate);

                if (m_includePrevious) {
                    X2Criteria previousCriteria = new X2Criteria();

                    applyStartDateCriteria(previousCriteria, fieldActiveDate, fieldStartDate, m_previousEndDate);
                    applyEndDateCriteria(previousCriteria, fieldCompletionDate, fieldEndDate, m_previousStartDate);

                    currentCriteria.addOrCriteria(previousCriteria);
                }
            }

            transcriptCriteria.addAndCriteria(currentCriteria);
        }

        /**
         * Do not include non-DL schools from district 200.
         *
         * @param criteria X2Criteria
         * @param schoolPrefix String
         */
        private void applyDistrictRestriction(X2Criteria criteria, String schoolPrefix) {
            X2Criteria dlCriteria = new X2Criteria();
            dlCriteria.addEqualTo(schoolPrefix + School.COL_SCHOOL_TYPE_CODE, DL_SCHOOL_TYPE);
            dlCriteria.addEqualTo(schoolPrefix + School.REL_ORGANIZATION2 + PATH_DELIMITER + Organization.COL_ID,
                    ORGANIZATION_ID_200);

            X2Criteria nonDlCriteria = new X2Criteria();
            nonDlCriteria.addNotEqualTo(schoolPrefix + School.REL_ORGANIZATION2 + PATH_DELIMITER + Organization.COL_ID,
                    ORGANIZATION_ID_200);

            dlCriteria.addOrCriteria(nonDlCriteria);
            criteria.addAndCriteria(dlCriteria);
        }

        /**
         * Applies the end date criteria. For DL schools Completion date is the "Completion Date" on
         * the transcript.
         * For NON DL schools Completion date is the "Course End Date" from the transcript if exists
         * else, use the
         * end date from the course schedule (term end date). Since transcripts without a related
         * master are allowed,
         * the criteria is also checked against the end date of the school year.
         *
         * @param criteria X2Criteria
         * @param fieldCompletionDate DataDictionaryField
         * @param fieldEndDate DataDictionaryField
         * @param date PlainDate
         */
        private void applyEndDateCriteria(X2Criteria criteria,
                                          DataDictionaryField fieldCompletionDate,
                                          DataDictionaryField fieldEndDate,
                                          PlainDate date) {
            /*
             * For Non DL Schools the Course End Date "trn-end-date" from student transcript if
             * exists if not, use
             * the end date from the course schedule
             */
            X2Criteria nonDlTermCriteria = new X2Criteria();
            nonDlTermCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + School.COL_SCHOOL_TYPE_CODE,
                    DL_SCHOOL_TYPE);
            nonDlTermCriteria.addGreaterOrEqualThan(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_END_DATE, date);

            if (fieldEndDate != null) {
                nonDlTermCriteria.addEmpty(fieldEndDate.getJavaName(), m_broker.getPersistenceKey());

                X2Criteria nonDlDateCriteria = new X2Criteria();
                nonDlDateCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + School.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);
                nonDlDateCriteria.addNotEmpty(fieldEndDate.getJavaName(), m_broker.getPersistenceKey());
                nonDlDateCriteria.addGreaterOrEqualThan(fieldEndDate.getJavaName(), date);

                nonDlTermCriteria.addOrCriteria(nonDlDateCriteria);
            }

            if (fieldCompletionDate != null) {
                /*
                 * For DL schools, Completion date is the "trn-completion-date" (Completion Date).
                 * Empty values allowed
                 */
                X2Criteria dlCriteria = new X2Criteria();
                dlCriteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + School.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);
                dlCriteria.addNotEmpty(fieldCompletionDate.getJavaName(), m_broker.getPersistenceKey());
                dlCriteria.addGreaterOrEqualThan(fieldCompletionDate.getJavaName(), date);

                X2Criteria dlEmptyCriteria = new X2Criteria();
                dlEmptyCriteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + School.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);
                dlEmptyCriteria.addEmpty(fieldCompletionDate.getJavaName(), m_broker.getPersistenceKey());

                nonDlTermCriteria.addOrCriteria(dlCriteria);
                nonDlTermCriteria.addOrCriteria(dlEmptyCriteria);
            }

            criteria.addAndCriteria(nonDlTermCriteria);
        }

        /**
         * Applies the start date criteria. For DL schools, SADE should use 'Active date' on the
         * student transcript.
         * NON DL schools Active Date is the "Course Start Date" from student transcript if exists,
         * else use the
         * start date from the course schedule (Term Start Date). Transcripts without an explicit
         * start date or
         * associataed with a section are not included
         *
         * @param criteria X2Criteria
         * @param fieldActiveDate DataDictionaryField
         * @param fieldStartDate DataDictionaryField
         * @param date PlainDate
         */
        private void applyStartDateCriteria(X2Criteria criteria,
                                            DataDictionaryField fieldActiveDate,
                                            DataDictionaryField fieldStartDate,
                                            PlainDate date) {
            if (fieldActiveDate != null && fieldStartDate != null) {
                /*
                 * For DL schools, SADE should use ?Active date? on the student transcript
                 */
                X2Criteria dlCriteria = new X2Criteria();
                dlCriteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + School.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);

                X2Criteria dlAndCriteria = new X2Criteria();
                dlAndCriteria.addLessOrEqualThan(fieldActiveDate.getJavaName(), date);

                X2Criteria dlOrCriteria = new X2Criteria();
                dlOrCriteria.addIsNull(fieldActiveDate.getJavaName());

                dlAndCriteria.addOrCriteria(dlOrCriteria);
                dlCriteria.addAndCriteria(dlAndCriteria);

                /*
                 * NON DL schools Active Date is the "Course Start Date" from student transcript if
                 * exists
                 */
                X2Criteria nonDlDateCriteria = new X2Criteria();
                nonDlDateCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + School.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);
                nonDlDateCriteria.addNotEmpty(fieldStartDate.getJavaName(), m_broker.getPersistenceKey());
                nonDlDateCriteria.addLessOrEqualThan(fieldStartDate.getJavaName(), date);

                /*
                 * If no start date, use start date from the course schedule (Term Start Date)
                 */
                X2Criteria nonDlTermCriteria = new X2Criteria();
                nonDlTermCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + School.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);
                nonDlTermCriteria.addEmpty(fieldStartDate.getJavaName(), m_broker.getPersistenceKey());
                nonDlTermCriteria.addLessOrEqualThan(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                        ScheduleTermDate.COL_START_DATE, date);

                dlCriteria.addOrCriteria(nonDlDateCriteria);
                dlCriteria.addOrCriteria(nonDlTermCriteria);

                criteria.addAndCriteria(dlCriteria);
            } else {
                criteria.addEqualTo(X2BaseBean.COL_OID, "nomatch");
            }
        }

        /**
         * Formats the passed credits for the report. Only credits > 0 are displayed.
         *
         * @param course Course
         * @return String
         */
        private String formatCredits(Course course) {
            String value = "";

            if (!m_localCourseTypes.contains(course.getFieldB002())) {
                BigDecimal credits = course.getCredit();

                if (credits != null && credits.doubleValue() > 0) {
                    value = m_bigDecimalFormatter.format(credits);
                }
            }

            return value;
        }

        /**
         * Parses the section's schedule display to get the day/period display (the first ID of
         * each).
         *
         * @param section MasterSchedule
         * @return String
         */
        private String getClassId(MasterSchedule section) {
            String classId = "";

            String scheduleDisplay = section.getScheduleDisplay();
            if (!StringUtils.isEmpty(scheduleDisplay)) {
                int index = scheduleDisplay.indexOf('(');

                if (index > 0) {
                    List<String> dayIds = getDayIds(section.getSchedule().getTimeScheduleOid());
                    List<String> periodIds = getPeriodIds(section.getSchedule().getTimeScheduleOid());
                    boolean periodFirst = getPeriodFirst(section.getSchedule());

                    String day = scheduleDisplay.substring(0, index);
                    String period = scheduleDisplay.substring(index + 1, scheduleDisplay.length() - 1);

                    if (periodFirst) {
                        day = scheduleDisplay.substring(index + 1, scheduleDisplay.length() - 1);
                        period = scheduleDisplay.substring(0, index);
                    }

                    // Loop over IDs til we find one that is in the display. The IDs are ordered by
                    // number
                    for (int i = 0; i < dayIds.size(); i++) {
                        String id = dayIds.get(i);

                        if (day.contains(id)) {
                            day = String.valueOf(i + 1);
                            break;
                        }
                    }

                    for (int i = 0; i < periodIds.size(); i++) {
                        String id = periodIds.get(i);

                        if (period.contains(id)) {
                            period = String.valueOf(i + 1);
                            break;
                        }
                    }

                    classId = StringUtils.padLeft((section.getTermView() + day + period), 9, '0');
                }
            }

            return classId;
        }

        /**
         * Returns the course code for the extract. Returns the external course code if exists.
         * Otherwise returns the
         * course number.
         *
         * @param course Course
         * @return String
         */
        private String getCourseCode(Course course) {
            String externalCode = (String) course.getFieldValueByAlias(ALIAS_EXTERNAL_CODE);

            return StringUtils.coalesce(externalCode, course.getNumber());
        }

        /**
         * Looks up the day ID-number map based on the schedule OID. For the first check against a
         * schedule the
         * map is loaded into a map keyed to the schedule OID.
         *
         * @param scheduleOid String
         * @return Map<String, Integer>
         */
        private List<String> getDayIds(String scheduleOid) {
            List<String> dayIds = m_dayIdMap.get(scheduleOid);

            if (dayIds == null) {
                /*
                 * Initialize map
                 */
                dayIds = new LinkedList<String>();
                m_dayIdMap.put(scheduleOid, dayIds);

                /*
                 * Build map
                 */
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ScheduleDay.COL_SCHEDULE_OID, scheduleOid);

                QueryByCriteria query = new QueryByCriteria(ScheduleDay.class, criteria);
                query.addOrderByAscending(ScheduleDay.COL_NUMBER);

                QueryIterator days = m_broker.getIteratorByQuery(query);
                try {
                    while (days.hasNext()) {
                        ScheduleDay day = (ScheduleDay) days.next();
                        dayIds.add(day.getId());
                    }
                } finally {
                    days.close();
                }
            }

            return dayIds;
        }

        /**
         * Looks up the period ID-number map based on the schedule OID. For the first check against
         * a schedule the
         * map is loaded into a map keyed to the schedule OID.
         *
         * @param schedule Schedule
         * @return Map<String, Integer>
         */
        private boolean getPeriodFirst(Schedule schedule) {
            Boolean value = m_periodFirstDisplay.get(schedule.getOid());

            if (value == null) {
                value = Boolean.valueOf(schedule.scheduleExpressionPeriodFirst());
                m_periodFirstDisplay.put(schedule.getOid(), value);
            }

            return value.booleanValue();
        }

        /**
         * Looks up the period ID-number map based on the schedule OID. For the first check against
         * a schedule the
         * map is loaded into a map keyed to the schedule OID.
         *
         * @param scheduleOid String
         * @return Map<String, Integer>
         */
        private List<String> getPeriodIds(String scheduleOid) {
            List<String> periodIds = m_periodIdMap.get(scheduleOid);

            if (periodIds == null) {
                /*
                 * Initialize map
                 */
                periodIds = new LinkedList<String>();
                m_periodIdMap.put(scheduleOid, periodIds);

                /*
                 * Build map
                 */
                Criteria criteria = new Criteria();
                criteria.addEqualTo(SchedulePeriod.COL_SCHEDULE_OID, scheduleOid);

                QueryByCriteria query = new QueryByCriteria(SchedulePeriod.class, criteria);
                query.addOrderByAscending(SchedulePeriod.COL_NUMBER);

                QueryIterator periods = m_broker.getIteratorByQuery(query);
                try {
                    while (periods.hasNext()) {
                        SchedulePeriod period = (SchedulePeriod) periods.next();
                        periodIds.add(period.getId());
                    }
                } finally {
                    periods.close();
                }
            }

            return periodIds;
        }

        /**
         * Iterate over the list of beans and puts the OID into its own collection.
         *
         * @param objects Collection
         * @return Collection<String> of OIDs
         */
        private Collection<String> getOidList(Collection objects) {
            Collection<String> oids = new LinkedList<String>();

            for (X2BaseBean bean : (Collection<X2BaseBean>) objects) {
                oids.add(bean.getOid());
            }

            return oids;
        }

        /**
         * Returns the earliest start or latest end date of the schedule term. If no dates are found
         * NULL is returned.
         * The results are cached for repeated lookup.
         *
         * @param term ScheduleTerm
         * @param isStartDate boolean
         * @return PlainDate
         */
        private PlainDate getTermDate(ScheduleTerm term, boolean isStartDate) {
            PlainDate date = null;

            if (term != null) {
                KeyValuePair<PlainDate, PlainDate> dateRange = m_termDateRanges.get(term.getOid());
                if (dateRange == null) {
                    PlainDate startDate = null;
                    PlainDate endDate = null;

                    for (ScheduleTermDate termDate : term.getScheduleTermDates()) {
                        PlainDate termStart = termDate.getStartDate();
                        PlainDate termEnd = termDate.getEndDate();

                        if (startDate == null || termStart.before(startDate)) {
                            startDate = termStart;
                        }

                        if (endDate == null || termEnd.after(endDate)) {
                            endDate = termEnd;
                        }
                    }

                    dateRange = new KeyValuePair<PlainDate, PlainDate>(startDate, endDate);
                    m_termDateRanges.put(term.getOid(), dateRange);
                }

                date = isStartDate ? dateRange.getKey() : dateRange.getValue();
            }

            return date;
        }

        /**
         * Looks up the end date of the transcript record:
         * <ul>
         * <li>For DL schools, Completion date is the "trn-completion-date" (Completion Date)
         * <li>For Non-DL Schools the Course End Date "trn-end-date" from student transcript if
         * exists if not, use
         * the end date from the course schedule
         * </ul>
         * .
         *
         * @param transcript Transcript
         * @return PlainDate
         */
        private PlainDate getTranscriptEndDate(Transcript transcript) {
            boolean isDl = DL_SCHOOL_TYPE.equals(transcript.getSchool().getSchoolTypeCode());

            String dateString = isDl ? (String) transcript.getFieldValueByAlias(ALIAS_COMPLETION_DATE)
                    : (String) transcript.getFieldValueByAlias(ALIAS_END_DATE);

            PlainDate date = (PlainDate) m_converter.parseSystemString(dateString);

            if (date == null && !isDl && transcript.getMasterSchedule() != null) {
                date = getTermDate(transcript.getMasterSchedule().getScheduleTerm(), false);
            }

            return date;
        }

        /**
         * Looks up the start date of the transcript record:
         * <ul>
         * <li>For DL schools, use course active date (alias "trn-course-active-date") on the
         * student transcript
         * <li>For Non-DL schools active date should be the course start date "trn-start-date" from
         * student transcript
         * if exists other wise use the start date from the course schedule
         * </ul>
         * .
         *
         * @param transcript Transcript
         * @return PlainDate
         */
        private PlainDate getTranscriptStartDate(Transcript transcript) {
            boolean isDl = DL_SCHOOL_TYPE.equals(transcript.getSchool().getSchoolTypeCode());

            String dateString = isDl ? (String) transcript.getFieldValueByAlias(ALIAS_COURSE_ACTIVE_DATE)
                    : (String) transcript.getFieldValueByAlias(ALIAS_START_DATE);

            PlainDate date = (PlainDate) m_converter.parseSystemString(dateString);

            if (date == null && !isDl && transcript.getMasterSchedule() != null) {
                date = getTermDate(transcript.getMasterSchedule().getScheduleTerm(), true);
            }

            return date;
        }

        /**
         * Checks if the passed string is numeric.
         *
         * @param str String
         * @return boolean
         */
        private boolean isNumeric(String str) {
            boolean numeric = true;

            for (char c : str.toCharArray()) {
                if (!Character.isDigit(c)) {
                    numeric = false;
                    break;
                }
            }

            return numeric;
        }

        /**
         * Loads the course type codes allowed in the export.
         */
        private void loadAllowedCourseTypes() {
            m_allowedTypes = new LinkedList<String>();

            DataDictionaryField field =
                    m_dictionary.findDataDictionaryField(Course.class.getName(), Course.COL_FIELD_B002);
            if (field != null) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                criteria.addIn(ReferenceCode.COL_LOCAL_CODE,
                        Arrays.asList("AP", "BA", "CP", "EC", "IB", "LD", "MF", "MI", "PA", "PS"));

                SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                m_allowedTypes = m_broker.getSubQueryCollectionByQuery(query);
            }
        }

        /**
         * Returns a map of the reference codes associated with the passed field.
         *
         * @param field DataDictionaryField
         * @return Map<String, ReferenceCode>
         */
        private Map<String, ReferenceCode> loadCodeMap(DataDictionaryField field) {
            Map<String, ReferenceCode> codes = new HashMap<String, ReferenceCode>(64);

            if (field != null) {
                ReferenceTable table = field.getReferenceTable();
                if (table != null) {
                    /*
                     * If table is found, build code map and track the extended dictionary
                     */
                    codes = table.getCodeMap(m_broker);

                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(table.getExtendedDataDictionary(),
                            m_broker.getPersistenceKey());
                    m_extendedReferenceDictionaries.put(table.getOid(), dictionary);
                }
            }

            return codes;
        }

        /**
         * Loads the course type codes with a local code of "LD".
         */
        private void loadLocalCourseTypes() {
            m_localCourseTypes = new LinkedList<String>();

            DataDictionaryField field =
                    m_dictionary.findDataDictionaryField(Course.class.getName(), Course.COL_FIELD_B002);
            if (field != null) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, "LD");

                SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                m_localCourseTypes = m_broker.getSubQueryCollectionByQuery(query);
            }
        }

        /**
         * Returns the Trax Code (UDF on reference code) based on the value set on the transcript
         * record.
         *
         * @param traxCode String
         * @return String
         */
        private String lookupTraxValue(String traxCode) {
            String value = "";

            ReferenceCode code = m_traxCodes.get(traxCode);
            if (code != null) {
                DataDictionary dictionary = m_extendedReferenceDictionaries.get(code.getReferenceTableOid());
                value = (String) code.getFieldValueByAlias(ALIAS_TRAX_CODE, dictionary);
            }

            return value;
        }

        /**
         * Returns the PlainDate object of the passed system date.
         *
         * @param dateAsString String
         * @return PlainDate
         */
        private PlainDate parseSystemDate(String dateAsString) {
            PlainDate date = null;

            if (!StringUtils.isEmpty(dateAsString)) {
                date = (PlainDate) m_converter.parseSystemString(dateAsString);
            }

            return date;
        }

        /**
         * Query for and iterate over student schedule records to be added to the grid.
         *
         * @param schoolOids Collection<String>
         */
        private void processSchedules(Collection<String> schoolOids) {
            X2Criteria scheduleCriteria = new X2Criteria();
            scheduleCriteria.addIn(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, schoolOids);

            /*
             * Only include schedules for non-DL students
             */
            scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_TYPE_CODE, DL_SCHOOL_TYPE);

            /*
             * Restrict specific grade levels and schools, if necessary
             */
            applyDistrictRestriction(scheduleCriteria,
                    StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER);
            if (m_gridLimitGrades && !CollectionUtils.isEmpty(m_limitedGradeList)) {
                scheduleCriteria.addIn(
                        StudentSchedule.REL_STUDENT + PATH_DELIMITER + m_contextHelper.getGradeLevelField(),
                        m_limitedGradeList);
            }


            /*
             * Only include specific course types
             */
            applyCourseTypeRestriction(scheduleCriteria, StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHOOL_COURSE + PATH_DELIMITER);

            /*
             * Current reporting period
             */
            X2Criteria currentCriteria = new X2Criteria();
            currentCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_START_DATE, m_currentEndDate);
            currentCriteria.addGreaterOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_END_DATE, m_currentStartDate);

            /*
             * Previous reporting period
             */
            if (m_includePrevious) {
                X2Criteria previousCriteria = new X2Criteria();
                previousCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                        ScheduleTermDate.COL_START_DATE, m_previousEndDate);
                previousCriteria.addGreaterOrEqualThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHEDULE_TERM + PATH_DELIMITER +
                        ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                        ScheduleTermDate.COL_END_DATE, m_previousStartDate);

                currentCriteria.addOrCriteria(previousCriteria);
            }

            scheduleCriteria.addAndCriteria(currentCriteria);

            QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, scheduleCriteria);
            QueryIterator iterator = m_broker.getIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    StudentSchedule schedule = (StudentSchedule) iterator.next();
                    SisStudent student = schedule.getStudent();
                    SisSchool school = schedule.getSchedule().getSchool();
                    MasterSchedule section = schedule.getSection();
                    SchoolCourse schoolCourse = section.getSchoolCourse();

                    Collection<String> sectionOids = m_sectionsByStudent.get(student.getOid());
                    if (sectionOids == null) {
                        sectionOids = new LinkedList<String>();
                    }

                    if (student != null && schoolCourse != null &&
                            (sectionOids == null || !sectionOids.contains(section.getOid()))) {
                        String type = PERIOD_TYPE_CURRENT;

                        PlainDate endDate = getTermDate(section.getScheduleTerm(), false);
                        if (endDate != null && m_currentStartDate != null && endDate.before(m_currentStartDate)) {
                            type = PERIOD_TYPE_PREVIOUS;
                        }

                        if (PERIOD_TYPE_CURRENT.equals(type) || m_includePrevious) {
                            addSADERow(school, student, schedule, schoolCourse, type);
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * Query for and iterate over transcripts to be added to the grid.
         *
         * @param schoolOids Collection<String>
         */
        private void processTranscripts(Collection<String> schoolOids) {
            X2Criteria transcriptCriteria = new X2Criteria();
            transcriptCriteria.addIn(Transcript.COL_SCHOOL_OID, schoolOids);

            /*
             * Restrict specific grade levels and schools, if necessary
             */
            restrictTranscriptGradeLevels(transcriptCriteria);
            applyDistrictRestriction(transcriptCriteria, Transcript.REL_SCHOOL + PATH_DELIMITER);

            /*
             * Only include specific course types
             */
            applyCourseTypeRestriction(transcriptCriteria, Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER);

            /*
             * Apply criteria for restricing in the current/previous date range
             */
            applyDateRangeCriteria(transcriptCriteria);
            BeanQuery transcriptQuery = new BeanQuery(Transcript.class, transcriptCriteria);
            QueryIterator transcripts = m_broker.getIteratorByQuery(transcriptQuery);

            try {
                while (transcripts.hasNext()) {
                    Transcript transcript = (Transcript) transcripts.next();
                    SisStudent student = transcript.getStudent();
                    SisSchool school = transcript.getSchool();
                    SchoolCourse schoolCourse = transcript.getSchoolCourse();

                    if (student != null && schoolCourse != null && transcript.getDistrictContext() != null) {
                        String type = PERIOD_TYPE_CURRENT;
                        PlainDate endDate = getTranscriptEndDate(transcript);
                        if (endDate != null && m_currentStartDate != null && endDate.before(m_currentStartDate)) {
                            type = PERIOD_TYPE_PREVIOUS;
                        }

                        if (PERIOD_TYPE_CURRENT.equals(type) || m_includePrevious) {
                            addSADERow(school, student, transcript, schoolCourse, type);
                        }
                    }

                    /*
                     * Track section so it is not repeated with the student schedule records
                     */
                    if (student != null && !StringUtils.isEmpty(transcript.getMasterScheduleOid())) {
                        Collection<String> sectionOids = m_sectionsByStudent.get(student.getOid());

                        if (sectionOids == null) {
                            sectionOids = new LinkedList<String>();
                            m_sectionsByStudent.put(student.getOid(), sectionOids);
                        }

                        sectionOids.add(transcript.getMasterScheduleOid());
                    }
                }
            } finally {
                transcripts.close();
            }
        }

        /**
         * Restrict by grade level -- Only limit for "Non-DL" schools.
         *
         * @param transcriptCriteria X2Criteria
         */
        private void restrictTranscriptGradeLevels(X2Criteria transcriptCriteria) {
            if (m_gridLimitGrades && !CollectionUtils.isEmpty(m_limitedGradeList)) {
                X2Criteria nondlCriteria = new X2Criteria();
                nondlCriteria.addIn(Transcript.COL_GRADE_LEVEL, m_limitedGradeList);
                nondlCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);

                X2Criteria dlCriteria = new X2Criteria();
                dlCriteria.addEmpty(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_TYPE_CODE,
                        m_broker.getPersistenceKey());
                dlCriteria.addOrEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_TYPE_CODE,
                        DL_SCHOOL_TYPE);

                nondlCriteria.addOrCriteria(dlCriteria);
                transcriptCriteria.addAndCriteria(nondlCriteria);
            }
        }
    }

    /**
     * Restricted grade levels for Non-DL schools
     */
    public static final String[] LIMITED_GRADES = new String[] {"08", "09", "10", "11", "12"};

    /*
     * Input parameters
     */
    private static final String PARAM_PERIODS_INCLUDED = "includePeriod";
    private static final String PARAM_LIMIT_GRADES = "limitGrades";
    private static final String PARAM_SORT_ORDER = "sortOrder";
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";

    /*
     * Additional report fields
     */
    private static final String FIELD_CURRENT_END = "currentEndDate";
    private static final String FIELD_CURRENT_START = "currentStartDate";
    private static final String FIELD_PREVIOUS_END = "previousEndDate";
    private static final String FIELD_PREVIOUS_START = "previousStartDate";

    /*
     * Members
     */
    private boolean m_includePreviousDates;
    private Boolean m_limitGrades = Boolean.FALSE;
    private Collection<SisSchool> m_schools = null;
    private String[] m_sortOrder = null;

    private String[] m_defaultSortOrder = new String[] {EXPORT_FIELDS.FIELD_SCHOOL_ID.getFieldId(),
            SADEReportDataGrid.PERIOD_TYPE_COLUMN,
            EXPORT_FIELDS.FIELD_PEN.getFieldId(),
            EXPORT_FIELDS.FIELD_COURSE_CODE.getFieldId(),
            EXPORT_FIELDS.FIELD_ACTIVE_DATE.getFieldId(),
            EXPORT_FIELDS.FIELD_CLASS_IDENTIFIER.getFieldId()};

    /**
     * Default constructor for use in tools that use this as an external source.
     */
    public StudentAchievementDataExtractData() {
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

        if (m_schools != null && !m_schools.isEmpty()) {
            for (SisSchool school : m_schools) {
                // get grid for current grade term period
                ReportDataGrid exportSadeGrid = getSADEGrid(school);
                grid.append(exportSadeGrid);
            }
        }

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
        m_schools = getSchools();

        String includedPeriods = (String) getParameter(PARAM_PERIODS_INCLUDED);
        if ("previous".equals(includedPeriods)) {
            m_includePreviousDates = true;
        } else {
            m_includePreviousDates = false;
        }

        if (getParameter(PARAM_LIMIT_GRADES) != null) {
            m_limitGrades = (Boolean) getParameter(PARAM_LIMIT_GRADES);
        }

        if (getParameter(PARAM_SORT_ORDER) != null) {
            m_sortOrder = getUserSortOrderAsStringArray((String) getParameter(PARAM_SORT_ORDER));
        } else {
            m_sortOrder = m_defaultSortOrder;
        }
    }

    /**
     * Sets additional information on the report.
     *
     * @param sadeGrid SADEReportDataGrid
     * @param school SisSchool
     */
    private void addReportAdditionalInfo(SADEReportDataGrid sadeGrid, SisSchool school) {
        sadeGrid.setAll("school", school);

        sadeGrid.setAll("currPeriodNoOfStudents", Integer.valueOf(sadeGrid.getCurrentStudentCount()));
        sadeGrid.setAll("currPeriodTotalRecordsNo", Integer.valueOf(sadeGrid.getCurrentRecordCount()));
        sadeGrid.setAll("prevPeriodNoOfStudents", Integer.valueOf(sadeGrid.getPreviousStudentCount()));
        sadeGrid.setAll("prevPeriodTotalRecordsNo", Integer.valueOf(sadeGrid.getPreviousRecordCount()));

        sadeGrid.setAll(FIELD_CURRENT_END, sadeGrid.getCurrentEndDate());
        sadeGrid.setAll(FIELD_CURRENT_START, sadeGrid.getCurrentStartDate());
        sadeGrid.setAll(FIELD_PREVIOUS_END, sadeGrid.getPreviousEndDate());
        sadeGrid.setAll(FIELD_PREVIOUS_START, sadeGrid.getPreviousStartDate());
    }

    /**
     * Sets the SADE information for the passed school.
     *
     * @param school SisSchool
     * @return ReportDataGrid
     */
    private ReportDataGrid getSADEGrid(SisSchool school) {
        StudentContextReportHelper helper =
                new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        SADEReportDataGrid sadeGrid = new SADEReportDataGrid(helper, getBroker());
        sadeGrid.initializeDateRanges(getOrganization());
        sadeGrid.setSchool(school);
        sadeGrid.setIncludePrevious(m_includePreviousDates);
        sadeGrid.setLimitGrades(m_limitGrades);
        sadeGrid.setLimitedGradeList(Arrays.asList(LIMITED_GRADES));

        sadeGrid.evaluateGrid();
        sadeGrid.removeInvalidRows();
        sadeGrid.tallyCounts();
        sadeGrid.sort(Arrays.asList(m_sortOrder), false);

        if (sadeGrid.isEmpty()) {
            // Always include an empty row for formatting purposes
            sadeGrid.append();
        }

        addReportAdditionalInfo(sadeGrid, school);

        return sadeGrid;
    }

    /**
     * Loads the schools used in the export.
     *
     * @return Collection<SisSchool>
     */
    private Collection<SisSchool> getSchools() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return getBroker().getCollectionByQuery(new QueryByCriteria(SisSchool.class, criteria));
    }
}
