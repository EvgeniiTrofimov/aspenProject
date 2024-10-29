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
package com.x2dev.procedures.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class MDStudentReportData.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MDStudentReportData extends StateReportData {

    /**
     * The Class MDStudentReportEntity.
     */
    public static abstract class MDStudentReportEntity extends StateReportEntity {

        /**
         * Gets the school.
         *
         * @param data MDStudentReportData
         * @return Sis school
         */
        public abstract SisSchool getSchool(MDStudentReportData data);
    }

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String INPUT_PARAM_QUERY_BY = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String INPUT_PARAM_QUERY_STRING = "queryString";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String INPUT_PARAM_REPORT_DATE = "reportDate";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String INPUT_PARAM_SORT = "sort";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String INPUT_PARAM_SUBMISSION_DATE = "submissionDate";

    /**
     * Name for "Two year cutoff date" parameter. This indicates the two year period for SPED, ELL
     * exit.
     */
    public static final String INPUT_PARAM_TWO_YEAR_CUTOFF = "twoYearCutoff";

    // Used by Allegany on student to override their school code.
    protected static final String DOE_SCHOOL_CODE = "DOE SCHOOL CODE";

    /*
     * Reference codes aliases for the student program code
     */
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";
    private static final String DOE_PROG_CODE_HOMELESS = "DOE PR HOMELESS";
    private static final String DOE_SCHOOL_OVERRIDE = "DOE SCHOOL OVERRIDE";

    /*
     * Aliases for the student direct cert and foster status
     */
    private static final String DOE_LUNCH = "DOE LUNCH";
    private static final String DOE_FOSTER_STATUS = "all-std-FosterCareStatus";
    private static final String DOE_DIRECT_CERTIFICATION = "all-std-DirectCertification";

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
        private boolean m_checkCutoff = false;
        private String m_fieldToRetreive = null;
        private String m_programStateCode = null;
        private Map<String, Collection<StudentProgramParticipation>> m_studentPrograms;

        /**
         * Constructor loads a map of programs by student.
         *
         * @param programStateCode The state code of the reference code for the program code.
         * @param fieldToRetreive String
         * @param subQuery A subquery of student oids to use for selecting programs into a map.
         * @param checkCutoff boolean
         */
        public RetrieveProgram(String programStateCode, String fieldToRetreive, SubQuery subQuery,
                boolean checkCutoff) {
            m_fieldToRetreive = fieldToRetreive;
            m_programStateCode = programStateCode;
            m_checkCutoff = checkCutoff;

            // Load programs for use in the retriever.
            Criteria progCriteria = new Criteria();
            progCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
            QueryByCriteria progQuery = new QueryByCriteria(StudentProgramParticipation.class, progCriteria);
            m_studentPrograms = getBroker().getGroupedCollectionByQuery(progQuery,
                    StudentProgramParticipation.COL_STUDENT_OID, 100);
        }

        /**
         * Retrieve programs that match the field parameter as a state code for the program code,
         * and
         * included in the program start/end date range.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Collection<StudentProgramParticipation> programs = m_studentPrograms.get(entity.getBean().getOid());
            Object value = null;
            if (programs != null && !StringUtils.isEmpty(m_programStateCode)) {
                for (StudentProgramParticipation program : programs) {
                    String code = program.getProgramCode();
                    String alias = lookupStateValue(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, code);
                    if (m_programStateCode.equals(alias)) {
                        if (!StringUtils.isEmpty(m_fieldToRetreive)) {
                            value = getProperty(program, m_fieldToRetreive);
                        } else if (program.getStartDate() != null &&
                                ((m_reportDate.after(program.getStartDate()) ||
                                        m_reportDate.equals(program.getStartDate())) &&
                                        (program.getEndDate() == null ||
                                                m_reportDate.before(program.getEndDate()) ||
                                                m_reportDate.equals(program.getEndDate())))) {

                            value = "Y";
                        } else if (m_checkCutoff &&
                                program.getEndDate() != null && m_twoYearCutoff != null &&
                                m_twoYearCutoff.before(program.getEndDate())) {
                            value = "E";
                        }
                        break;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a string value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with one character:
     * The reference code state code value in the reference table for race codes.
     * In MD, this is:
     * "1" - Indian/Native/Alaskan
     * "2" - Asian
     * "3" - Black
     * "4" - Pacific
     * "5" - White
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {
        private Map<String, Collection<Race>> m_raceCodeMap;
        private Map<String, ReferenceCode> m_raceCodes;

        /**
         * Instantiates a new retrieve race.
         *
         * @param subQuery SubQuery
         */
        public RetrieveRace(SubQuery subQuery) {
            super();

            // Get race code reference codes for use in the race retriever.
            DataDictionaryField raceCodeField = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                    .findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
            if (raceCodeField != null && raceCodeField.getReferenceTableOid() != null) {
                X2Criteria raceCriteria = new X2Criteria();
                raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, raceCodeField.getReferenceTableOid());
                raceCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
                m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);
            } else {
                m_raceCodes = new HashMap();
            }

            Criteria raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String yesCode = param.substring(0, 1);
            String noCode = param.substring(1, 2);
            String stateCode = param.substring(2);
            String raceCode = noCode;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());
            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(stateCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = yesCode;
                        break;
                    }
                }
            }

            return raceCode;
        }
    }

    /**
     * Returns the school code for the given student.
     * <p>
     * Students can attend multiple schools in a year. Get the MembershipAttendance record
     * for the current student instance to get the school
     * <p>
     * There are two modes:
     * parameter = "alias" -> Retrieve the aliased field value.
     * parameter = "alias=value" -> Check if the value in the aliased field
     * is equal to the value supplied. Return true/false.
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            MDStudentReportEntity stuEntity = (MDStudentReportEntity) entity;

            School school = stuEntity.getSchool((MDStudentReportData) data);

            // Get the parameter to find and comparison value.
            String param = (String) field.getParameter();
            String equalValue = null;
            int equalPos = param.indexOf('=');
            if (equalPos > -1) {
                equalValue = param.substring(equalPos + 1);
                param = param.substring(0, equalPos);

            }
            Object value = null;

            if (school != null) {
                value = school.getFieldValueByAlias(param);
                // If an equals exists, check if the value retrieved equals the equal value.
                if (equalPos > -1) {
                    if (equalValue.equals(value)) {
                        value = Boolean.TRUE;
                    } else {
                        value = Boolean.FALSE;
                    }
                }
            }

            return value;
        }
    }


    /**
     * Returns the Free/Reduced Meals for the given student.
     */
    protected class RetrieveMeals implements FieldRetriever {
        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            MDStudentReportEntity stuEntity = (MDStudentReportEntity) entity;
            Student student = (Student) stuEntity.getBean();
            String param = (String) field.getParameter();

            String lunch = (String) student.getFieldValueByAlias(DOE_LUNCH);
            boolean directCert =
                    StringUtils.isEqual((String) student.getFieldValueByAlias(DOE_DIRECT_CERTIFICATION),
                            BooleanAsStringConverter.TRUE);
            boolean fosterStatus =
                    StringUtils.isEqual((String) student.getFieldValueByAlias(DOE_FOSTER_STATUS),
                            BooleanAsStringConverter.TRUE);

            String value;
            if (StringUtils.isEqual(lunch, "C")) {
                if (directCert || fosterStatus) {
                    value = "F";
                } else {
                    value = "N";
                }
            } else {
                value = (String) student.getFieldValueByAlias(param);
                value = data.lookupReferenceCodeByAlias(param, value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return value;
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
     * <td>N</td>
     * <td>Special education status is not active and student did not exit special education during
     * the last two school years</td>
     * </tr>
     * <tr>
     * <td>Y</td>
     * <td>Special education status is active</td>
     * </tr>
     * <tr>
     * <td>E</td>
     * <td>Special education status is not active, the student exited special education during the
     * last two school years</td>
     * </tr>
     * <tr>
     * <td>2</td>
     * <td>Special education status is not active, the student is in 504</td>
     * </tr>
     * <tr>
     * <td>3</td>
     * <td>Special education status is exited, the student is in 504</td>
     * </tr>
     * </table>
     *
     */
    protected class RetrieveSped implements FieldRetriever {
        private String m_sped504Alias = null;
        private String m_spedActiveCode;

        /**
         * Instantiates a new retrieve sped.
         */
        public RetrieveSped() {
            super();

            // Check Alleganey custom IEP/504 alias.
            m_sped504Alias = translateAliasToJavaName("DOE SPED 504", false);
            if (m_sped504Alias == null) {
                m_sped504Alias = "";
            }
            m_spedActiveCode =
                    PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = "N";
            PlainDate spedEndDate = (PlainDate) getProperty(entity.getBean(), SisStudent.COL_SPED_EXIT_DATE);

            if (StringUtils.isEmpty(m_sped504Alias)) {
                // Standard sped and 504 status fields.
                // Get 504 and sped status and dates from student fields.
                String spedStatusCode = (String) getProperty(entity.getBean(), SisStudent.COL_SPED_STATUS_CODE);
                String status504Code = (String) getProperty(entity.getBean(), SisStudent.COL_SECTION504_STATUS_CODE);

                if (m_spedActiveCode.equals(spedStatusCode)) {
                    value = "Y";
                } else if (spedEndDate != null
                        && (spedEndDate.before(m_reportDate) || spedEndDate.equals(m_reportDate))) {
                    // Check exited sped status, in 504 program.
                    if (m_spedActiveCode.equals(status504Code)) {
                        value = "3";
                    }
                    // check if exited sped within the last two years.
                    else if (m_twoYearCutoff != null && spedEndDate.after(m_twoYearCutoff)) {
                        value = "E";
                    }
                } else if (m_spedActiveCode.equals(status504Code)) {
                    value = "2";
                }
            } else {
                // Alleganey uses alternate sped and 504 status field.
                String sped504Value = (String) getProperty(entity.getBean(), m_sped504Alias);

                if ("IEP".equals(sped504Value)) {
                    value = "Y";
                } else if (spedEndDate != null
                        && (spedEndDate.before(m_reportDate) || spedEndDate.equals(m_reportDate))) {
                    if ("504".equals(sped504Value)) {
                        value = "3";
                    }
                    // check if exited sped within the last two years.
                    else if (m_twoYearCutoff != null && spedEndDate.after(m_twoYearCutoff)) {
                        value = "E";
                    }
                } else if ("504".equals(sped504Value)) {
                    value = "2";
                }
            }

            return value;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * Trim the resulting string to the field maximum length.
     * For first and last names, and middle initial.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";

        private Pattern m_illegalNameCharacters;

        /**
         * Instantiates a new retrieve strip name char.
         */
        public RetrieveStripNameChar() {
            super();
            m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
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
            // Strip illegal characters (punctuation).
            if (value != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(value);
                value = matcher.replaceAll("");

                // Trim to valid field length.
                if (value.length() > field.getMaxLength()) {
                    value = value.substring(0, field.getMaxLength());
                }
            } else {
                value = "";
            }
            return value;
        }

    }


    protected PlainDate m_reportDate;
    protected PlainDate m_twoYearCutoff;

    private Map<String, String> m_schoolCodeMap;
    private Map<String, SisSchool> m_schoolCodeSchoolMap;
    private Criteria m_studentCriteria;

    /**
     * Gets the override school.
     *
     * @param student SisStudent
     * @return Sis school
     */
    protected SisSchool getOverrideSchool(SisStudent student) {
        SisSchool overrideSchool = null;
        if (m_schoolCodeMap != null) {
            String studentSchoolOverride = (String) student.getFieldValueByAlias(DOE_SCHOOL_OVERRIDE);
            if (!StringUtils.isEmpty(studentSchoolOverride)) {
                String overrideSchoolCode = m_schoolCodeMap.get(studentSchoolOverride);
                if (!StringUtils.isEmpty(overrideSchoolCode)) {
                    overrideSchool = m_schoolCodeSchoolMap.get(overrideSchoolCode);
                }
            }
        }
        return overrideSchool;
    }

    /**
     * Override method to create the reporting criteria
     * Default contains all.
     *
     * @return Criteria
     */
    protected Criteria getReportingCriteria() {
        return new X2Criteria();
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    protected Criteria getStudentCriteria() {
        if (m_studentCriteria == null) {
            /*
             * First build the criteria based on the user's input
             */
            X2Criteria userCriteria = new X2Criteria();

            String queryString = (String) getParameter(INPUT_PARAM_QUERY_STRING);
            int queryBy = getParameter(INPUT_PARAM_QUERY_BY) != null
                    ? ((Integer) getParameter(INPUT_PARAM_QUERY_BY)).intValue()
                    : -1;
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
            m_studentCriteria = new Criteria();
            if (queryBy > 0) {
                m_studentCriteria.addAndCriteria(userCriteria);
            }
            m_studentCriteria.addAndCriteria(getReportingCriteria());
        }

        return m_studentCriteria;
    }

    /**
     * Gets the student query.
     *
     * @return Query by criteria
     */
    protected QueryByCriteria getStudentQuery() {
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, getStudentCriteria());

        int sort = ((Integer) getParameter(INPUT_PARAM_SORT)).intValue();
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
        return studentQuery;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        m_twoYearCutoff = (PlainDate) getParameter(INPUT_PARAM_TWO_YEAR_CUTOFF);

        initializeFields();

        if (getSetupErrors().isEmpty()) {
            // Add any retrievers or validators.
            SubQuery subQueryStudentOid = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());
            SubQuery subQueryPersonOid = new SubQuery(SisStudent.class,
                    SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, getStudentCriteria());
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ATT-CLEAN", new RetrieveStripNameChar());
            calcs.put("ATT-ELL-END", new RetrieveProgram(DOE_PROG_CODE_ELL, "endDate", subQueryStudentOid, true));
            calcs.put("ATT-ELL-START", new RetrieveProgram(DOE_PROG_CODE_ELL, "startDate", subQueryStudentOid, true));
            calcs.put("ATT-ELLIND", new RetrieveProgram(DOE_PROG_CODE_ELL, null, subQueryStudentOid, true));
            calcs.put("ATT-HOMELESS-PRG", new RetrieveProgram(DOE_PROG_CODE_HOMELESS, null, subQueryStudentOid, false));
            calcs.put("ATT-RACE", new RetrieveRace(subQueryPersonOid));
            calcs.put("ATT-SCHOOL", new RetrieveSchool());
            calcs.put("ATT-SPED", new RetrieveSped());
            calcs.put("ATT_FR_MEALS", new RetrieveMeals());
            super.addCalcs(calcs);
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        // If there is a school override on the student, get the override reference codes to state
        // code mapping.
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(DOE_SCHOOL_OVERRIDE);
        if (field != null) {
            ReferenceTable refTbl = field.getReferenceTable();
            if (refTbl != null) {
                m_schoolCodeMap = new HashMap<String, String>();
                for (ReferenceCode refCode : refTbl.getReferenceCodes()) {
                    m_schoolCodeMap.put(refCode.getCode(), refCode.getStateCode());
                }

                // Also need a mapping from school code back to school.
                m_schoolCodeSchoolMap = new HashMap<String, SisSchool>();
                for (School school : getOrganization().getSchools()) {
                    String schoolCode = (String) school.getFieldValueByAlias(DOE_SCHOOL_CODE);
                    if (!StringUtils.isEmpty(schoolCode)) {
                        m_schoolCodeSchoolMap.put(schoolCode, (SisSchool) school);
                    }
                }
            }
        }
    }

}
