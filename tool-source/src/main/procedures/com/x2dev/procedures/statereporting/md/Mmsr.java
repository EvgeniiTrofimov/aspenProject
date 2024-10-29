/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: MMSR export.
 * This class implements the data export for MD MMSR export.
 *
 * @author X2 Development Corporation
 */
public class Mmsr extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD MMSR export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class MmsrEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        Mmsr m_mmsrData = null;
        EnrollmentSnapshot m_snapshot = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public MmsrEntity() {
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
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";

            return name;
        }

        /**
         * Return the enrollment snapshot that is used by some
         * fieldRetrievers to get enrollment data.
         *
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot() {
            return m_snapshot;
        }

        /**
         * Returns an error message or null indicating if this entity should be filtered out of the
         * export or included.
         * Implementing classes can perform run time filtering on entities.
         *
         * @return a validation error if this entity should not be exported.
         *         a null indicates the entity should be exported.
         */
        @Override
        public StateReportValidationError filterEntity() {
            Mmsr mmsr = (Mmsr) getData();
            StateReportValidationError error = null;
            if (!StudentManager.isActiveStudent(mmsr.getOrganization(), m_snapshot.getEnrollmentStatus())) {
                error = new StateReportValidationError(this, getData().getFieldDefinition(3), "Student not active",
                        null);
            }

            return error;
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

            m_mmsrData = (Mmsr) data;

            // Get enrollment snapshot on report as of date.
            SisStudent student = (SisStudent) bean;
            m_snapshot = getSnapshot(student, m_mmsrData.m_reportDate, data.getFieldDefinition(DOE_02_SCHOOL_ID));
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
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate, FieldDefinition field) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            if (!snapshot.isPrecise()) {
                addRetrievalError(field.getFieldId(), new StateReportValidationError(this, field,
                        "WARNING: Enrollment information (enrollment status, school, and/or YOG) is not precise", ""));
            }

            return snapshot;
        }
    }

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the report format parameter. This indicated CSV or Column delimited report.
     */
    public static final String REPORT_FORMAT_PARAM = "reportFormat";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_PERIOD_PARAM = "reportPeriod";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * DOE field alias constants. Most of these field aliases are all for the STUDENT
     *
     * Fields without aliases must have a bean path or retriever specified in the Field Definition.
     */

    private static final String DOE_01_LEA_ID = "DOE DISTRICT CODE";
    private static final String DOE_02_SCHOOL_ID = "DOE SCHOOL CODE";
    private static final String DOE_03_LAST_NAME = "DOE LAST NAME";
    private static final String DOE_04_FIRST_NAME = "DOE FIRST NAME";
    private static final String DOE_05_MIDDLE_NAME = "DOE MIDDLE NAME";
    private static final String DOE_06_STATE_ID = "DOE SASID";
    private static final String DOE_07_GENDER = "DOE GENDER";
    private static final String DOE_08_RACE = "DOE RACE";
    private static final String DOE_09_ELL = "DOE ELL";
    private static final String DOE_10_SPED_STATUS = "DOE SPED STATUS";
    private static final String DOE_11_TITLE1 = "DOE TITLE1";
    private static final String DOE_12_LUNCH = "DOE LUNCH";
    private static final String DOE_13_BIRTH_DATE = "DOE DOB";
    private static final String DOE_14_PRIOR_CARE = "DOE PRIOR CARE";
    private static final String DOE_15_HOME_CARE = "DOE HOME CARE";
    private static final String DOE_16_HEAD_START = "DOE HEAD START";
    private static final String DOE_17_PREK = "DOE PREK";
    private static final String DOE_18_CHILD_CARE_CENTER = "DOE CHILD CARE CENTER";
    private static final String DOE_19_FAMILY_CHILD_CARE = "DOE_FAMILY CHILD CARE";
    private static final String DOE_20_NONPUB_NURSERY = "DOE NONPUB NURSERY";
    private static final String DOE_21_EVEN_START = "DOE EVEN START";
    private static final String DOE_22_HIPPY = "DOE HIPPY";
    private static final String DOE_23_PARENT_TEACHER = "DOE PARENT TEACHER";
    private static final String DOE_24_PRESCHOOL_SPED = "DOE PRESCHOOL SPED";
    private static final String DOE_25_PRESCHOOL_OTHER = "DOE PRESCHOOL OTHER";
    private static final String DOE_26_TEACHER_NAME = "DOE TEACHER";

    /*
     * Field aliases for "Student Program Participation" program type code
     */
    private static final String DOE_PROG_CODE_LUNCH = "DOE PR LUNCH";
    private static final String DOE_PROG_CODE_TITLE1 = "DOE PR TITLE1";
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";
    private static final String DOE_PROG_CODE_504 = "DOE PR 504";
    private static final String DOE_SPED_BEGIN = "DOE SPED BEGIN";
    private static final String DOE_SPED_END = "DOE SPED END";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_STATUS_FIELD = "DOE Status";
    private static final String DOE_STATUS_FIELD_REPORT_CODE = "Report";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String DATE_FORMAT = "yyyyMMdd";
    private static final String DATE_INNER_FORMAT = "yyyy-MM-dd";
    private static final String REGEX_NUMERIC = "[0123456789]*";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected SimpleDateFormat m_dateFormat;
    protected SimpleDateFormat m_dateInnerFormat;
    protected String m_doeStatusField;
    protected Pattern m_illegalNameCharacters;
    protected PlainDate m_reportDate;
    protected Integer m_reportFormat;
    protected int m_reportPeriod;
    protected String m_schoolCodeField;
    protected String m_spedStartDate;
    protected String m_spedEndDate;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentPrograms;
    protected Map<String, Collection<SisStaff>> m_teacherMap;

    /**
     * Retrieve a field value as a boolean indicator "0"/"1".
     * If the field is populated, it recieves a "1".
     * if it is empty or null or "0", it recieves a "0".
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAsBoolean implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (StringUtils.isEmpty(value) || "0".equals(value)) {
                value = "0";
            } else {
                value = "1";
            }
            return value;
        }
    }

    /**
     * Retrieve the gender code for the student.
     * Translate into report required values (1 or 2).
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGender implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            String gender = "0";
            if (value != null) {
                value = value.toUpperCase();
                if ("M".equals(value)) {
                    gender = "1";
                }
                if ("F".equals(value)) {
                    gender = "2";
                }
            }
            return gender;
        }
    }

    /**
     * Returns an indicator as to whether the student is participating in a program.
     *
     * The program is identified by the field.getParameter() value matching the State code value
     * of the reference code behind the program code.
     *
     * To match, the student program must have a start date before the report date
     * and the end date must be after the report date or must be empty.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SisStudent student = (SisStudent) entity.getBean();
            String programAlias = (String) field.getParameter();
            PlainDate reportDate = ((Mmsr) data).m_reportDate;
            Collection<StudentProgramParticipation> programs = m_studentPrograms.get(student.getOid());
            String inProgram = "0";
            if (programs != null && !StringUtils.isEmpty(programAlias)) {
                for (StudentProgramParticipation program : programs) {

                    String code = program.getProgramCode();
                    String alias = lookupStateValue(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, code);
                    if (programAlias.equals(alias)) {
                        if (program.getStartDate() != null &&
                                ((reportDate.after(program.getStartDate()) ||
                                        reportDate.equals(program.getStartDate())) &&
                                        (program.getEndDate() == null ||
                                                reportDate.before(program.getEndDate()) ||
                                                reportDate.before(program.getEndDate())))) {
                            inProgram = "1";
                            break;
                        }
                    }
                }
            }
            return inProgram;
        }
    }

    /**
     * Returns the school code for the given student.
     * <p>
     * Students can attend multiple schools in a year. Get the snapshot record
     * for the current student to get the school.
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            MmsrEntity mmsrEnt = (MmsrEntity) entity;
            EnrollmentSnapshot snapshot = mmsrEnt.getSnapshot();
            SisSchool school = snapshot.getSchool();
            String schoolCode = "XXXX";
            if (school != null) {
                // Left pad with zeros.
                schoolCode = "0000" + getProperty(school, m_schoolCodeField);
                schoolCode = schoolCode.substring(schoolCode.length() - 4);

            } else {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field,
                                "Could not find School Code", null));
            }

            return schoolCode;
        }
    }

    /**
     * Returns the sped placement info value. If the "retrieve sped values" input parameter
     * was selected, the following logic is used to derive the return value:
     * <p>
     * <table border="1">
     * <tr>
     * <th>Returned state code equivalent</th>
     * <th>Circumstance</th>
     * </tr>
     * <tr>
     * <td>0</td>
     * <td>No Special education</td>
     * </tr>
     * <tr>
     * <td>1</td>
     * <td>Special education</td>
     * </tr>
     * <tr>
     * <td>2</td>
     * <td>Student is in 504</td>
     * </tr>
     * </table>
     *
     */
    protected class RetrieveSpedStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String spedStatus = "0";

            // Get sped dates from student fields.
            String spedStartDateStr = (String) getProperty(entity.getBean(), m_spedStartDate);
            PlainDate spedStartDate = null;
            if (!StringUtils.isEmpty(spedStartDateStr)) {
                try {
                    Date parseDate;
                    parseDate = m_dateInnerFormat.parse(spedStartDateStr);
                    spedStartDate = new PlainDate(parseDate);
                } catch (ParseException e) {
                    // Do nothing, there is no date or invalid date.
                }
            }
            PlainDate spedEndDate = (PlainDate) getProperty(entity.getBean(), m_spedEndDate);

            // Get 504 status from field or program.
            String stdTblAlias = translateAliasToJavaName(DOE_09_ELL, false);
            String stat504 = "0";
            if (!StringUtils.isEmpty(stdTblAlias)) {
                stat504 = (String) getProperty(entity.getBean(), stdTblAlias);
            } else {
                /**
                 * For program, find a program with program code state value DOE_PROG_CODE_ELL
                 */
                FieldDefinition field504 = new FieldDefinition(DOE_PROG_CODE_504,
                        LABEL_PREFIX_CHAR + DOE_PROG_CODE_504, null, 0, 1, 1, "[YN]",
                        null, new RetrieveProgram(), null, DOE_PROG_CODE_504);
                FieldRetriever retriever = new RetrieveProgram();
                stat504 = (String) retriever.getFieldValue(data, entity, field504);
            }

            // Determine sped status value.
            if (spedStartDate != null && (spedStartDate.before(m_reportDate) || spedStartDate.equals(m_reportDate)) &&
                    (spedEndDate == null || spedEndDate.after(m_reportDate) || spedEndDate.equals(m_reportDate))) {
                // Check sped status, in sped program.
                spedStatus = "1";
            } else if (stat504.equals("1")) {
                spedStatus = "2";
            }

            return spedStatus;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names, and middle initial.
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
            Boolean upper = (Boolean) field.getParameter();
            // Strip illegal characters (punctuation).
            if (value != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(value);
                value = matcher.replaceAll("");

                // Map to upper case if required.
                if (upper != null && upper.booleanValue()) {
                    value = value.toUpperCase();
                }

                // Trim to valid field length.
                if (value.length() > field.getMaxLength()) {
                    value = value.substring(0, field.getMaxLength());
                }
            } else {
                value = "";
            }
            // Trim the field to max length.
            if (value.length() > field.getMaxLength()) {
                value = value.substring(0, field.getMaxLength());
            }
            return value;
        }
    }

    /**
     * Retriever to find the name of the teacher of the student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacherName implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String name = null;

            MmsrEntity mmsrEnt = (MmsrEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();
            EnrollmentSnapshot snapshot = mmsrEnt.getSnapshot();
            SisSchool school = snapshot.getSchool();
            String homeroom = student.getHomeroom();

            if (school != null && !StringUtils.isEmpty(homeroom)) {
                String key = school.getOid() + "-" + homeroom;
                Collection<SisStaff> teachers = m_teacherMap.get(key);
                if (teachers != null) {
                    SisStaff teacher = teachers.iterator().next();
                    if (teacher != null) {
                        name = (teacher.getPerson().getFirstName() + " " + teacher.getPerson().getLastName()).trim();
                    }

                    if (teachers.size() > 1) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "Teacher name inexact. Multiple teachers have the same homeroom.",
                                "Homeroom=" + homeroom);
                        entity.addRetrievalError(field.getFieldId(), error);
                    }
                }
            }

            // Trim the field to max length.
            if (name != null && name.length() > field.getMaxLength()) {
                name = name.substring(0, field.getMaxLength());
            }

            return name;
        }
    }

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "MMSR Export";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Sets the value delimiter in the export.
     * The user parameter "Report format" determines whether to include delimiters (CSV)
     * or use column delimited.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getUseValueWrappers()
     */
    @Override
    public boolean getUseValueDelimiters() {
        boolean useDelims = false;
        if (m_reportFormat != null && m_reportFormat.intValue() == 1) {
            useDelims = true;
        }
        return useDelims;
    }

    /**
     * Sets the value wrapper in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getUseValueWrappers()
     */
    @Override
    public boolean getUseValueWrappers() {
        return false;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {

        // Load initialization data
        initializeFields();

        /*
         * Get core parameters
         */
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_reportFormat = (Integer) getParameter(REPORT_FORMAT_PARAM);
        m_reportPeriod = ((Integer) getParameter(REPORT_PERIOD_PARAM)).intValue();

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        m_dateInnerFormat = new SimpleDateFormat(DATE_INNER_FORMAT);
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        // Set the field definition array.
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(52);
        fieldDefinitions.add(getDOE01_leaid());
        fieldDefinitions.add(getDOE02_schoolid());
        fieldDefinitions.add(getDOE03_lastName());
        fieldDefinitions.add(getDOE04_firstName());
        fieldDefinitions.add(getDOE05_middleName());
        fieldDefinitions.add(getDOE06_studentStateId());
        // EOY only fields.
        if (m_reportPeriod > 0) {
            fieldDefinitions.add(getDOE07_gender());
            fieldDefinitions.add(getDOE08_race());
            fieldDefinitions.add(getDOE09_ell());
            fieldDefinitions.add(getDOE10_spedStatus());
            fieldDefinitions.add(getDOE11_title1());
            fieldDefinitions.add(getDOE12_lunch());
            fieldDefinitions.add(getDOE13_birthDate());
            fieldDefinitions.add(getDOE14_priorCare());
            fieldDefinitions.add(getDOE15_homeCare());
            fieldDefinitions.add(getDOE16_headStart());
            fieldDefinitions.add(getDOE17_prek());
            fieldDefinitions.add(getDOE18_childCareCenter());
            fieldDefinitions.add(getDOE19_familyChildCare());
            fieldDefinitions.add(getDOE20_nonpubNursery());
            fieldDefinitions.add(getDOE21_evenStart());
            fieldDefinitions.add(getDOE22_hippy());
            fieldDefinitions.add(getDOE23_parentTeacher());
            fieldDefinitions.add(getDOE24_preschoolSped());
            fieldDefinitions.add(getDOE25_other());
        }
        fieldDefinitions.add(getDOE26_teacherName());
        setFieldDefinitions(fieldDefinitions);
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 3: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(MmsrEntity.class);

            int count = getBroker().getCount(studentQuery);

            // Load student programs map and teacher map.
            loadStudentPrograms(studentCriteria, count);
            loadTeacherMap();
        }
    }

    /**
     * Build Field definition for DOE 01, district identifier (LEA).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE01_leaid() {
        String leaId = null;
        String districtIdField = translateAliasToJavaName(DOE_01_LEA_ID, true);
        if (districtIdField != null) {
            leaId = (String) getOrganization().getFieldValueByBeanPath(districtIdField);
        }
        FieldDefinition field = new FieldDefinition(DOE_01_LEA_ID,
                LABEL_PREFIX_CHAR + DOE_01_LEA_ID,
                leaId, 0, 2, 2, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 02, school identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE02_schoolid() {
        FieldDefinition field = new FieldDefinition(DOE_02_SCHOOL_ID,
                SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_schoolCodeField,
                null, 0, 4, 4, REGEX_NUMERIC,
                null, new RetrieveSchoolCode(), null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 03, student last name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE03_lastName() {
        FieldDefinition field = new FieldDefinition(DOE_03_LAST_NAME,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                null, 0, 1, 14, null, null,
                new RetrieveStripNameChar(),
                null, Boolean.FALSE);
        return field;
    }

    /**
     * Build Field definition for DOE 04, student first name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE04_firstName() {
        FieldDefinition field = new FieldDefinition(DOE_04_FIRST_NAME,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                null, 0, 1, 9, null, null,
                new RetrieveStripNameChar(),
                null, Boolean.FALSE);
        return field;
    }

    /**
     * Build Field definition for DOE 05, student middle initial.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE05_middleName() {
        FieldDefinition field = new FieldDefinition(DOE_05_MIDDLE_NAME,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_MIDDLE_NAME,
                null, 0, 0, 1, null, null,
                new RetrieveStripNameChar(), null, Boolean.TRUE);
        return field;
    }


    /**
     * Build Field definition for DOE 06, SASID Student state identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE06_studentStateId() {
        FieldDefinition field = new FieldDefinition(DOE_06_STATE_ID, SisStudent.COL_STATE_ID,
                null, 0, 1, 11, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 07, student gender.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE07_gender() {
        FieldDefinition field = new FieldDefinition(DOE_07_GENDER,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_GENDER_CODE,
                null, 0, 1, 1, "[12]",
                null, new RetrieveGender(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 08, student race code.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE08_race() {
        FieldDefinition field = new FieldDefinition(DOE_08_RACE,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + translateAliasToJavaName(DOE_08_RACE, true),
                null, 1,
                1, 1, "[12345]", null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 09, ELL Indicator.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE09_ell() {
        FieldDefinition field = null;
        String stdTblAlias = translateAliasToJavaName(DOE_09_ELL, false);
        if (!StringUtils.isEmpty(stdTblAlias)) {
            // For a field on the student record, retrieve the value directly.
            field = new FieldDefinition(DOE_09_ELL,
                    stdTblAlias, null, 0, 1, 1, "[01]",
                    null, null, null, null);
        } else {
            /**
             * For program, find a program with program code state value DOE_PROG_CODE_ELL
             */
            field = new FieldDefinition(DOE_09_ELL,
                    LABEL_PREFIX_CHAR + DOE_PROG_CODE_ELL, null, 0, 1, 1, "[01]",
                    null, new RetrieveProgram(), null, DOE_PROG_CODE_ELL);
        }
        return field;
    }


    /**
     * Build Field definition for DOE 10, student special ed. status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE10_spedStatus() {
        FieldDefinition field = new FieldDefinition(DOE_10_SPED_STATUS, LABEL_PREFIX_CHAR + DOE_10_SPED_STATUS,
                "0", 0, 1, 1, "[012]", null,
                new RetrieveSpedStatus(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 11, student Title I Status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE11_title1() {
        FieldDefinition field = null;
        String stdTblAlias = translateAliasToJavaName(DOE_11_TITLE1, false);
        if (!StringUtils.isEmpty(stdTblAlias)) {
            // For a field on the student record, retrieve the value directly.
            field = new FieldDefinition(DOE_11_TITLE1, stdTblAlias,
                    null, 0, 1, 1, "[01]",
                    null, null, null, null);
        } else {
            /**
             * For program, find a program with program code state value DOE_PROG_CODE_TITLE1
             */
            field = new FieldDefinition(DOE_11_TITLE1, LABEL_PREFIX_CHAR + DOE_11_TITLE1,
                    null, 0, 1, 1, "[01]",
                    null, new RetrieveProgram(), null, DOE_PROG_CODE_TITLE1);
        }
        return field;
    }

    /**
     * Build Field definition for DOE 12, student Free/Reduced Lunch status
     * This field can be stored in one of two places:
     * 1. Field on the student record.
     * 2. Field on the Student program record.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE12_lunch() {
        FieldDefinition field = null;
        String stdTblAlias = translateAliasToJavaName(DOE_12_LUNCH, false);
        if (!StringUtils.isEmpty(stdTblAlias)) {
            // For a field on the student record, retrieve the value directly.
            field = new FieldDefinition(DOE_12_LUNCH, stdTblAlias,
                    "0", 0, 1, 1, "[01]",
                    null, new RetrieveAsBoolean(), null, null);
        } else {
            /**
             * For program, find a program with program code state value DOE_PROG_CODE_LUNCH
             */
            field = new FieldDefinition(DOE_12_LUNCH, LABEL_PREFIX_CHAR + DOE_12_LUNCH,
                    "0", 0, 1, 1, "[01]",
                    null, new RetrieveProgram(), null, DOE_PROG_CODE_LUNCH);
        }
        return field;
    }

    /**
     * Build Field definition for DOE 13, student birth date.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE13_birthDate() {
        FieldDefinition field = new FieldDefinition(DOE_13_BIRTH_DATE,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_DOB,
                null, 0, 8, 8, null, m_dateFormat, null,
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 14, Predominant prior care.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE14_priorCare() {
        FieldDefinition field =
                new FieldDefinition(DOE_14_PRIOR_CARE, translateAliasToJavaName(DOE_14_PRIOR_CARE, true),
                        " ", 1, 1, 1, "[123456]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 15, Home care flag.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE15_homeCare() {
        FieldDefinition field = new FieldDefinition(DOE_15_HOME_CARE, translateAliasToJavaName(DOE_15_HOME_CARE, true),
                "0", 0, 1, 1, "[01]", null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 16, Head Start.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE16_headStart() {
        FieldDefinition field =
                new FieldDefinition(DOE_16_HEAD_START, translateAliasToJavaName(DOE_16_HEAD_START, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 17, Pre-K flag.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE17_prek() {
        FieldDefinition field = new FieldDefinition(DOE_17_PREK, translateAliasToJavaName(DOE_17_PREK, true),
                "0", 0, 1, 1, "[01]", null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 18, Child care center flag.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE18_childCareCenter() {
        FieldDefinition field =
                new FieldDefinition(DOE_18_CHILD_CARE_CENTER, translateAliasToJavaName(DOE_18_CHILD_CARE_CENTER, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 19, Family care flag.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE19_familyChildCare() {
        FieldDefinition field =
                new FieldDefinition(DOE_19_FAMILY_CHILD_CARE, translateAliasToJavaName(DOE_19_FAMILY_CHILD_CARE, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 20, Non-Public nursery flag.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE20_nonpubNursery() {
        FieldDefinition field =
                new FieldDefinition(DOE_20_NONPUB_NURSERY, translateAliasToJavaName(DOE_20_NONPUB_NURSERY, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 21, Even start flag.
     *
     * @return a FieldDefinition
     */

    protected FieldDefinition getDOE21_evenStart() {
        FieldDefinition field =
                new FieldDefinition(DOE_21_EVEN_START, translateAliasToJavaName(DOE_21_EVEN_START, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 22, Hippy flag.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE22_hippy() {
        FieldDefinition field = new FieldDefinition(DOE_22_HIPPY, translateAliasToJavaName(DOE_22_HIPPY, true),
                "0", 0, 1, 1, "[01]", null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 23, parent as teacher.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE23_parentTeacher() {
        FieldDefinition field =
                new FieldDefinition(DOE_23_PARENT_TEACHER, translateAliasToJavaName(DOE_23_PARENT_TEACHER, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 24, Preschool Sped.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE24_preschoolSped() {
        FieldDefinition field =
                new FieldDefinition(DOE_24_PRESCHOOL_SPED, translateAliasToJavaName(DOE_24_PRESCHOOL_SPED, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 25, Other preschool care not listed.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE25_other() {
        FieldDefinition field =
                new FieldDefinition(DOE_25_PRESCHOOL_OTHER, translateAliasToJavaName(DOE_25_PRESCHOOL_OTHER, true),
                        "0", 0, 1, 1, "[01]", null,
                        null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 26, teacher name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE26_teacherName() {
        FieldDefinition field = new FieldDefinition(DOE_26_TEACHER_NAME, LABEL_PREFIX_CHAR + DOE_26_TEACHER_NAME,
                null, 0, 1, 14, null, null,
                new RetrieveTeacherName(),
                null, null);
        return field;
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */

    private Criteria getReportingCriteria() {
        /*
         * Use the grade level reference code to find grade levels that are K and PreK.
         * These reference codes would have state values of 91-96.
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);
        ReferenceTable refTbl = field.getReferenceTable();
        ArrayList kinderCodes = new ArrayList<String>();
        if (refTbl != null) {
            Map<String, ReferenceCode> codes = refTbl.getCodeMap(getBroker());
            for (ReferenceCode code : codes.values()) {
                if (code.getStateCode() != null && code.getStateCode().matches("9[123456]")) {
                    kinderCodes.add(code.getCode());
                }
            }
        }

        /*
         * Who should be included? Primary Kindergarten and preschool students.
         *
         * The students that belong to each group depend upon how this export is being run:
         *
         * The export is being run for either (A) the entire district or (B) a single school
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case A:
         *
         * Students in an active, non-archived school in the district
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case B:
         *
         * Students with enrollment activity (E,W) within the year.
         *
         */

        /*
         * Primary students
         */
        // Select students with primary school, or students with
        // enrollment activity (E,W) in the school this year.
        Criteria enrollCriteria = new Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, StudentEnrollment.WITHDRAWAL);
        Criteria enrollCriteria2 = new Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        // With Enrollment records within the active date range.
        Criteria activityCriteria = new Criteria();
        PlainDate startDate = getCurrentContext().getStartDate();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        activityCriteria.addAndCriteria(enrollCriteria2);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        Criteria primaryCriteria = new Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }
        primaryCriteria.addOrCriteria(enrollCriteria);

        // Add grade level requirement.
        primaryCriteria.addIn(SisStudent.COL_GRADE_LEVEL, kinderCodes);

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(primaryCriteria);

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
        if (requireReportStatus.booleanValue()) {
            userCriteria.addEqualTo(m_doeStatusField, DOE_STATUS_FIELD_REPORT_CODE);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_doeStatusField = translateAliasToJavaName(DOE_STATUS_FIELD, true);

        m_spedStartDate = translateAliasToJavaName(DOE_SPED_BEGIN, true);
        m_spedEndDate = translateAliasToJavaName(DOE_SPED_END, true);
        m_schoolCodeField = translateAliasToJavaName(DOE_02_SCHOOL_ID, true);
    }

    /**
     * Load student programs into a map by student Oid.
     *
     * @param studentCriteria Criteria
     * @param count int
     */
    private void loadStudentPrograms(Criteria studentCriteria, int count) {
        /*
         * Load student program participation records.
         */
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria programCriteria = new Criteria();
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);

        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        m_studentPrograms = getBroker().getGroupedCollectionByQuery(programQuery,
                StudentProgramParticipation.COL_STUDENT_OID, count);
    }

    /**
     * Load teacher map.
     */
    private void loadTeacherMap() {
        String substType = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_SUBSTITUTE_CODE);
        String active =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

        Criteria criteria = new Criteria();
        criteria.addNotNull(SisStaff.COL_HOMEROOM);
        criteria.addNotNull(SisStaff.COL_SCHOOL_OID);
        criteria.addEqualTo(SisStaff.COL_STATUS, active);
        criteria.addNotEqualTo(SisStaff.COL_STAFF_TYPE, substType);
        QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        m_teacherMap = new HashMap<String, Collection<SisStaff>>();
        try {
            while (iterator.hasNext()) {
                SisStaff teacher = (SisStaff) iterator.next();
                String homeroom = teacher.getHomeroom();
                String homeroom2 = teacher.getHomeroom2();
                String schoolOid = teacher.getSchoolOid();

                // Check the teachers primary homeroom.
                String key = schoolOid + "-" + homeroom;
                Collection teachersInHomeroom = m_teacherMap.get(key);
                if (teachersInHomeroom == null) {
                    teachersInHomeroom = new ArrayList<SisStaff>();
                    m_teacherMap.put(key, teachersInHomeroom);
                }
                teachersInHomeroom.add(teacher);

                // Check the teachers secondary homeroom, if present.
                if (!StringUtils.isEmpty(homeroom2)) {
                    key = schoolOid + "-" + homeroom;
                    teachersInHomeroom = m_teacherMap.get(key);
                    if (teachersInHomeroom == null) {
                        teachersInHomeroom = new ArrayList<SisStaff>();
                        m_teacherMap.put(key, teachersInHomeroom);
                    }
                    teachersInHomeroom.add(teacher);
                }
            }
        } finally {
            iterator.close();
        }
    }
}
