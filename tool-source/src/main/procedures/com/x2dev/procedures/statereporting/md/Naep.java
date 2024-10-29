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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: NAEP export.
 * This class implements the data export for MD NAEP export.
 *
 * @author X2 Development Corporation
 */
public class Naep extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD ELL export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NaepEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NaepEntity() {
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
     * Field aliases for "Student Program Participation" program type code
     */
    private static final String DOE_SCHOOL_CODE = "DOE SCHOOL CODE";
    private static final String DOE_SCHOOL_OVERRIDE = "DOE SCHOOL OVERRIDE";

    /*
     * Student alias for ELL indicator.
     */
    private static final String DOE_STD_ELL = "DOE ELL";
    private static final String DOE_ELL_SCORE = "DOE LEP SCORE";

    /*
     * Student program aliases for ELL.
     */
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";
    private static final String DOE_PROG_ELL_SCORE = "DOE PR LEP SCORE";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_studentEllAliasField;
    protected String m_studentEllScoreAliasField;
    protected Pattern m_illegalNameCharacters;
    protected String m_overrideSchoolCodeField;
    protected String m_progEllScoreAliasField;
    protected PlainDate m_reportDate;
    protected String m_schoolCodeField;

    /**
     * Simple field retriever, but uses constructed path rather than field path.
     * This is useful for field values where the alias may not exist for all customers.
     * The initialize method can determine whether or which field to pass to the
     * constructor for the retriever.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveField implements FieldRetriever {
        String m_fieldPath = null;

        /**
         * Instantiates a new retrieve field.
         *
         * @param fieldPath String
         */
        public RetrieveField(String fieldPath) {
            m_fieldPath = fieldPath;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return getProperty(entity.getBean(), m_fieldPath);
        }
    }

    /**
     * Perform normal field lookup and reference code lookup, but retrieve the federal
     * code rather than the state code from the reference code bean.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFedRefcode implements FieldRetriever {
        Map<String, Map<String, ReferenceCode>> tableMap = new HashMap<String, Map<String, ReferenceCode>>(5);

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = getProperty(entity.getBean(), field.getBeanPath());
            String fedCode = null;
            if (value != null && value instanceof String) {
                String valueString = (String) value;
                Map<String, ReferenceCode> codeMap = tableMap.get(valueString);
                if (codeMap == null) {
                    DataDictionary dictionary =
                            DataDictionary.getDistrictDictionary(data.getBroker().getPersistenceKey());
                    DataDictionaryField ddField = dictionary
                            .findDataDictionaryField(entity.getBean().getClass().getName(), field.getBeanPath());
                    if (ddField != null) {
                        ReferenceTable table = ddField.getReferenceTable();
                        if (table != null) {
                            codeMap = table.getCodeMap(data.getBroker());
                            tableMap.put(valueString, codeMap);
                        }
                    }
                }

                if (codeMap != null) {
                    ReferenceCode code = codeMap.get(valueString);
                    if (code != null) {
                        fedCode = code.getFederalCode();
                    }
                }
            }
            return fedCode;
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
        Map<String, ReferenceCode> m_raceCodes = new HashMap<String, ReferenceCode>();
        Map<String, Collection<Race>> m_personRaceCodes = null;

        /**
         * Instantiates a new retrieve race.
         *
         * @param subQuery SubQuery
         */
        public RetrieveRace(SubQuery subQuery) {
            // Get race code reference codes for use in the race retriever.
            Criteria raceCriteria = new Criteria();
            raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

            raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_personRaceCodes = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String codeChar = param.substring(2);
            String raceCode = falseChar;

            Student student = (Student) entity.getBean();
            Collection<Race> races = m_personRaceCodes.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(codeChar);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = trueChar;
                        break;
                    }
                }
            }

            return raceCode;
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
     * Returns the school code for the given student.
     * <p>
     * Students can attend multiple schools in a year. Get the membership attendance record
     * for the current student instance to get the school
     */
    protected class RetrieveSchoolCode implements FieldRetriever {
        Map<String, String> m_schoolCodeMap;

        /**
         * Build a map of school id to school code.
         *
         * @param broker X2Broker
         * @param codeField String
         */
        public RetrieveSchoolCode(X2Broker broker, String codeField) {
            m_schoolCodeMap = new HashMap<String, String>();
            if (!StringUtils.isEmpty(codeField)) {
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
                DataDictionaryField field = dictionary.findDataDictionaryField(SisStudent.class.getName(), codeField);
                if (field != null) {
                    ReferenceTable refTbl = field.getReferenceTable();
                    if (refTbl != null) {
                        for (ReferenceCode refCode : refTbl.getReferenceCodes()) {
                            m_schoolCodeMap.put(refCode.getCode(), refCode.getStateCode());
                        }
                    }
                }
            }
        }

        /**
         * Retrieve the school code.
         * Check student school override, and translate that to school code if present.
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
            SisStudent student = (SisStudent) entity.getBean();
            String schoolCode = null;

            // Look for an override.
            if (m_overrideSchoolCodeField != null) {
                String schoolId = (String) getProperty(entity.getBean(), ((Naep) data).m_overrideSchoolCodeField);
                if (!StringUtils.isEmpty(schoolId)) {
                    schoolCode = m_schoolCodeMap.get(schoolId);
                    if (StringUtils.isEmpty(schoolCode)) {
                        schoolCode = schoolId;
                    }
                }
            }

            if (!StringUtils.isEmpty(schoolCode)) {
                // A school code is found. format and use it.
                schoolCode = "0000" + schoolCode;
                schoolCode = schoolCode.substring(schoolCode.length() - 4);
            } else if (student.getSchool() != null) {
                // Left pad with zeros.
                schoolCode = "0000" + getProperty(student.getSchool(), m_schoolCodeField);
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
     * Retrieve a program participation for a given program code state code value
     * and program start and end date.
     * Retreive a value from in program bean.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {
        Map<String, Collection<StudentProgramParticipation>> m_studentPrograms = null;
        String m_fieldToRetreive = null;
        String m_programStateCode = null;

        /**
         * Constructor loads a map of programs by student.
         *
         * @param programStateCode The state code of the reference code for the program code.
         * @param fieldToRetreive String
         * @param subQuery A subquery of student oids to use for selecting programs into a map.
         */
        public RetrieveProgram(String programStateCode, String fieldToRetreive, SubQuery subQuery) {
            m_fieldToRetreive = fieldToRetreive;
            m_programStateCode = programStateCode;

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
            PlainDate reportDate = new PlainDate();
            Collection<StudentProgramParticipation> programs = m_studentPrograms.get(entity.getBean().getOid());
            Object value = null;
            if (programs != null && !StringUtils.isEmpty(m_programStateCode)) {
                for (StudentProgramParticipation program : programs) {

                    String code = program.getProgramCode();
                    String alias = lookupStateValue(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, code);
                    if (m_programStateCode.equals(alias)) {
                        if (program.getStartDate() != null &&
                                ((reportDate.after(program.getStartDate()) ||
                                        reportDate.equals(program.getStartDate())) &&
                                        (program.getEndDate() == null ||
                                                reportDate.before(program.getEndDate()) ||
                                                reportDate.equals(program.getEndDate())))) {

                            if (!StringUtils.isEmpty(m_fieldToRetreive)) {
                                value = getProperty(program, m_fieldToRetreive);
                            } else {
                                value = Boolean.TRUE;
                            }
                            break;
                        }
                    }
                }
            }

            return value;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        // Load initialization data
        initializeFields();

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

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
            setEntityClass(NaepEntity.class);

            SubQuery subQuery = null;

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("NAEP-CLEAN", new RetrieveStripNameChar());
            calcs.put("NAEP-GENDER", new RetrieveGender());
            subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
            calcs.put("NAEP-RACE", new RetrieveRace(subQuery));
            calcs.put("NAEP-REFCODEFED", new RetrieveFedRefcode());
            calcs.put("NAEP-SCHOOL", new RetrieveSchoolCode(getBroker(), m_overrideSchoolCodeField));

            subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            if (StringUtils.isEmpty(m_studentEllAliasField)) {
                calcs.put("NAEP-ELLIND", new RetrieveProgram(DOE_PROG_CODE_ELL, null, subQuery));
            } else {
                calcs.put("NAEP-ELLIND", new RetrieveField(m_studentEllAliasField));
            }
            super.addCalcs(calcs);
        }

        /*
         * HashMap validators = new HashMap<String, FieldRetriever>();
         * validators.put("FTE-RESIDENT", new ValidateResidentStatus());
         * super.addValidators(validators);
         */
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
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */

    private Criteria getReportingCriteria() {
        /*
         * Primary students
         */
        Criteria reportCriteria = new Criteria();
        reportCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

        if (isSchoolContext()) {
            reportCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            reportCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            reportCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }
        return reportCriteria;
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
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_SCHOOL_OVERRIDE, false);
        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL_CODE, true);
        m_studentEllAliasField = translateAliasToJavaName(DOE_STD_ELL, false);
        m_studentEllScoreAliasField = translateAliasToJavaName(DOE_ELL_SCORE, false);
        m_progEllScoreAliasField = translateAliasToJavaName(DOE_PROG_ELL_SCORE, false);
    }
}
