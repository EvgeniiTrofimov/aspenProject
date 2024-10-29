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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
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
public class MmsrEnrollmentWic extends StateReportData {
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
        MmsrEnrollmentWic m_mmsrData = null;
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
            MmsrEnrollmentWic mmsr = (MmsrEnrollmentWic) getData();
            StateReportValidationError error = null;
            if (m_snapshot == null ||
                    m_snapshot.getEnrollmentStatus() == null ||
                    !StudentManager.isActiveStudent(mmsr.getOrganization(), m_snapshot.getEnrollmentStatus())) {
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

            m_mmsrData = (MmsrEnrollmentWic) data;

            // Get enrollment snapshot on report as of date.
            SisStudent student = (SisStudent) bean;
            m_snapshot = getSnapshot(student, m_mmsrData.m_reportDate);
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
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            return snapshot;
        }
    }

    /**
     * Name for the grade term Oid parameter. The value is a String.
     */
    public static final String GRADE_TERM_OID_PARAM = "gradeTermOid";

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
     * Field aliases for codes
     */
    private static final String DOE_SCHOOL_CODE = "DOE SCHOOL CODE";
    private static final String DOE_STAFF_ID = "DOE STAFF ID";
    private static final String DOE_STD_ELL = "DOE ELL";
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";
    private static final String DOE_MMSR_SCORE = "DOE MMSR SCORE";
    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_STATUS_FIELD = "DOE Status";
    private static final String DOE_STATUS_FIELD_REPORT_CODE = "Report";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";

    /*
     * Wicomico uses programs to store prior care information.
     */
    private static final String PROGRAM_CODE = "MMSR";
    private static final String PROGRAM_HOME_CARE_CODE = "mmsr-cared-for";
    private static final String PROGRAM_PRIOR_CARE_ALIAS = "mmsr-early-care";
    private static final String PROGRAM_CODE_HEAD_START = "Head Start";
    private static final String PROGRAM_CODE_PREK = "Prekindergarten";
    private static final String PROGRAM_CODE_CCC = "Child Care Center";
    private static final String PROGRAM_CODE_FCC = "Family Child Care";
    private static final String PROGRAM_CODE_NPUB_NURSERY = "Non-Public Nursery School";
    private static final String PROGRAM_CODE_KIND = "Kindergarten";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_doeMmsrScoreField;
    protected String m_doeStatusField;
    protected String m_progCaredForField;
    protected String m_progEarlyCareField;
    protected Pattern m_illegalNameCharacters;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected PlainDate m_reportDate;
    protected String m_schoolCodeField;
    protected String m_staffIdField;
    protected String m_studentEllAliasField;
    protected Map<String, Collection<SisStaff>> m_teacherMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentMmsrPrograms = null;

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
     * The Class RetrieveFarms.
     */
    protected class RetrieveFarms implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            String farms = "0";
            if (!StringUtils.isEmpty(value)) {
                farms = "1";
            }
            return farms;
        }
    }

    /**
     * Retrieve a grade code for the requested grade value.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRubricGrade implements FieldRetriever {
        private Map<String, Map<String, Collection<RubricAssessmentPerformance>>> m_scoreMap = null;

        /**
         * Build a map of Rubric Assessment Performance by student and criterion based on
         * the selected grade term and the current context year.
         *
         * @param gradeTermOid String
         * @param studentSubquery SubQuery
         */
        public RetrieveRubricGrade(String gradeTermOid, SubQuery studentSubquery) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(RubricAssessmentPerformance.REL_RUBRIC_ASSESSMENT + PATH_DELIMITER +
                    RubricAssessment.REL_TRANSCRIPT_RUBRICS + PATH_DELIMITER +
                    TranscriptRubric.REL_TRANSCRIPT + PATH_DELIMITER +
                    Transcript.COL_STUDENT_OID, studentSubquery);
            criteria.addEqualTo(RubricAssessmentPerformance.REL_RUBRIC_ASSESSMENT + PATH_DELIMITER +
                    RubricAssessment.REL_TRANSCRIPT_RUBRICS + PATH_DELIMITER +
                    TranscriptRubric.REL_TRANSCRIPT + PATH_DELIMITER +
                    Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            criteria.addEqualTo(RubricAssessmentPerformance.REL_RUBRIC_ASSESSMENT + PATH_DELIMITER +
                    RubricAssessment.REL_TRANSCRIPT_RUBRICS + PATH_DELIMITER +
                    TranscriptRubric.REL_TRANSCRIPT_COLUMN_DEFINITION + PATH_DELIMITER +
                    TranscriptColumnDefinition.COL_GRADE_TERM_OID, gradeTermOid);
            criteria.addNotEmpty(RubricAssessmentPerformance.REL_RUBRIC_CRITERION + PATH_DELIMITER +
                    m_doeMmsrScoreField, getBroker().getPersistenceKey());
            QueryByCriteria query = new QueryByCriteria(RubricAssessmentPerformance.class, criteria);

            /*
             * available in 3.0+
             * m_scoreMap = getBroker().getGroupedCollectionByQuery(query,
             * new String[] {RubricAssessmentPerformance.REL_RUBRIC_ASSESSMENT +
             * ModelProperty.PATH_DELIMITER +
             * RubricAssessment.COL_PARENT_OID,
             * RubricAssessmentPerformance.REL_RUBRIC_CRITERION + ModelProperty.PATH_DELIMITER +
             * m_doeMmsrScoreField},
             * new int[] {100,100});
             * 3.0+
             */

            /* available in 2.12 (Wicomico) */
            m_scoreMap = new HashMap<String, Map<String, Collection<RubricAssessmentPerformance>>>();
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    RubricAssessmentPerformance rap = (RubricAssessmentPerformance) iterator.next();
                    String stdOid = rap.getRubricAssessment().getParentOid();
                    String scoreId = (String) rap.getRubricCriterion().getFieldValueByBeanPath(m_doeMmsrScoreField);

                    if (stdOid != null && scoreId != null) {
                        Map<String, Collection<RubricAssessmentPerformance>> stdMap = m_scoreMap.get(stdOid);
                        if (stdMap == null) {
                            stdMap = new HashMap<String, Collection<RubricAssessmentPerformance>>();
                            m_scoreMap.put(stdOid, stdMap);
                        }
                        Collection<RubricAssessmentPerformance> rapMap = stdMap.get(scoreId);
                        if (rapMap == null) {
                            rapMap = new HashSet<RubricAssessmentPerformance>();
                            stdMap.put(scoreId, rapMap);
                        }
                        rapMap.add(rap);
                    }
                }
            } finally {
                iterator.close();
            }
            /* 2.12 */
        }

        /**
         * Retrieve the grade from the grade map based on student and criterion.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String grade = " ";
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();
            Map<String, Collection<RubricAssessmentPerformance>> rapMap = m_scoreMap.get(student.getOid());
            if (rapMap != null && param != null) {
                Collection<RubricAssessmentPerformance> rapList = rapMap.get(param);
                if (rapList != null && rapList.size() > 0) {
                    RubricAssessmentPerformance rap = rapList.iterator().next();
                    if (rap != null) {
                        BigDecimal points = rap.getPoints();
                        if (points != null) {
                            grade = Integer.toString(points.intValue());
                        }
                    }
                }
            }

            return grade;
        }
    }

    /**
     * Retrieve home care flag, as interpreted from prior care flag.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveHomeCare implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = "0";
            String priorCare = entity.getFieldValue("PriorCare");
            if (StringUtils.isEmpty(priorCare) || "7".equals(priorCare)) {
                value = "1";
            }
            return value;
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
     * Wicomico uses custom student program participations to store MMSR early care information.
     * Custom retriever for those values.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMmsrProgram implements FieldRetriever {

        /**
         * Retrieve a boolean indicating if the field in the program contains the expected value.
         * The parameter should be the expected string value of the early-care field.
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
            String valueToRetreive = (String) field.getParameter();
            Collection<StudentProgramParticipation> programs = m_studentMmsrPrograms.get(entity.getBean().getOid());
            Boolean value = Boolean.FALSE;
            String err = valueToRetreive;

            if (programs != null && !StringUtils.isEmpty(m_progEarlyCareField)
                    && !StringUtils.isEmpty(m_progCaredForField)) {
                for (StudentProgramParticipation program : programs) {
                    err += "++" + program.getOid() + "|";
                    String schoolYear = program.getFieldA001();
                    int pgmYear = 0;
                    try {
                        // String of the form "2010-2011", substring after "-" and convert to int.
                        pgmYear = Integer.parseInt(schoolYear.substring(schoolYear.indexOf('-') + 1));
                    } catch (Exception e) {
                        // didn't substring or parse properly.
                    }
                    err += Integer.toString(pgmYear) + "|";

                    if (getCurrentContext().getSchoolYear() == pgmYear) {
                        if ("0".equals(valueToRetreive)) // Home care
                        {
                            String fieldValue = (String) getProperty(program, m_progCaredForField);
                            if (fieldValue != null && fieldValue.equals("Y")) {
                                value = Boolean.TRUE;
                            }
                        } else {
                            String fieldValue = (String) getProperty(program, m_progEarlyCareField);
                            err += "EarlyCare=" + fieldValue + "|";
                            String otherValue =
                                    (String) getProperty(program, StudentProgramParticipation.COL_FIELD_D001);
                            err += "OtherCare=" + otherValue + "|";
                            String otherState = null;
                            if (!StringUtils.isEmpty(otherValue)) {
                                otherState = lookupStateValue(StudentProgramParticipation.class,
                                        StudentProgramParticipation.COL_FIELD_D001, otherValue);
                            }
                            if (valueToRetreive.equals(otherState)) {
                                value = Boolean.TRUE;
                            } else if ("1".equals(valueToRetreive) && PROGRAM_CODE_HEAD_START.equals(fieldValue)) {
                                value = Boolean.TRUE;
                            } else if ("2".equals(valueToRetreive) && PROGRAM_CODE_PREK.equals(fieldValue)) {
                                value = Boolean.TRUE;
                            } else if ("3".equals(valueToRetreive) && PROGRAM_CODE_CCC.equals(fieldValue)) {
                                value = Boolean.TRUE;
                            } else if ("4".equals(valueToRetreive) && PROGRAM_CODE_FCC.equals(fieldValue)) {
                                value = Boolean.TRUE;
                            } else if ("5".equals(valueToRetreive) && PROGRAM_CODE_NPUB_NURSERY.equals(fieldValue)) {
                                value = Boolean.TRUE;
                            } else if ("6".equals(valueToRetreive) && PROGRAM_CODE_KIND.equals(fieldValue)) {
                                value = Boolean.TRUE;
                            }
                        }
                    }
                }
            }
            entity.addRetrievalError(field.getFieldId(), new StateReportValidationError(entity, field, err, err));
            return value;
        }
    }

    /**
     * Retrieve the home care flag and the pror care value.
     * Examine the
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMmsrProgram2 implements FieldRetriever {

        /**
         * Retrieve a boolean indicating if the field in the program contains the expected value.
         * The parameter should be the expected string value of the early-care field.
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
            Collection<StudentProgramParticipation> programs = m_studentMmsrPrograms.get(entity.getBean().getOid());
            String value = " ";
            String err = "";
            if (programs != null && !StringUtils.isEmpty(m_progCaredForField)) {
                for (StudentProgramParticipation program : programs) {
                    err += "++" + program.getOid() + "|";
                    String schoolYear = program.getFieldA001();
                    int pgmYear = 0;
                    try {
                        // String of the form "2010-2011", substring after "-" and convert to int.
                        pgmYear = Integer.parseInt(schoolYear.substring(schoolYear.indexOf('-') + 1));
                    } catch (Exception e) {
                        // didn't substring or parse properly.
                    }
                    err += Integer.toString(pgmYear) + "|";

                    if (getCurrentContext().getSchoolYear() == pgmYear) {
                        String fieldValue = (String) getProperty(program, m_progCaredForField);
                        err += "CareFor=" + fieldValue + "|";
                        if ("Y".equals(fieldValue)) {
                            value = " ";
                        } else {
                            fieldValue = (String) getProperty(program, m_progEarlyCareField);
                            err += "EarlyCare=" + fieldValue + "|";
                            if (PROGRAM_CODE_HEAD_START.equals(fieldValue)) {
                                value = "1";
                            }
                            if (PROGRAM_CODE_PREK.equals(fieldValue)) {
                                value = "2";
                            }
                            if (PROGRAM_CODE_CCC.equals(fieldValue)) {
                                value = "3";
                            }
                            if (PROGRAM_CODE_FCC.equals(fieldValue)) {
                                value = "4";
                            }
                            if (PROGRAM_CODE_NPUB_NURSERY.equals(fieldValue)) {
                                value = "5";
                            }
                            if (PROGRAM_CODE_KIND.equals(fieldValue)) {
                                value = "6";
                            }
                        }
                    }
                }
            }
            entity.addRetrievalError(field.getFieldId(), new StateReportValidationError(entity, field, err, err));
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

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String raceCode = "0";

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(param);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = param;
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
     * Retriever to find the teacher of the student and retrieve information.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacher implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;

            MmsrEntity mmsrEnt = (MmsrEntity) entity;
            MmsrEnrollmentWic enrData = (MmsrEnrollmentWic) data;
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();
            EnrollmentSnapshot snapshot = mmsrEnt.getSnapshot();
            SisSchool school = snapshot.getSchool();
            String homeroom = student.getHomeroom();

            if (school != null && !StringUtils.isEmpty(homeroom)) {
                String key = school.getOid() + "-" + homeroom.trim();
                Collection<SisStaff> teachers = m_teacherMap.get(key);
                if (teachers != null) {
                    SisStaff teacher = teachers.iterator().next();
                    if (teacher != null) {
                        if ("ID".equals(param)) {
                            value = (String) teacher.getFieldValueByBeanPath(enrData.m_staffIdField);
                        } else if ("NAME".equals(param)) {
                            value = (teacher.getPerson().getFirstName() + " " + teacher.getPerson().getLastName())
                                    .trim();
                            if (value != null && value.length() > field.getMaxLength()) {
                                value = value.substring(0, field.getMaxLength());
                            }
                        }
                    }

                    if (teachers.size() > 1) {
                        StateReportValidationError error = new StateReportValidationError(entity, field,
                                "Teacher inexact. Multiple teachers have the same homeroom.", "Homeroom=" + homeroom);
                        entity.addRetrievalError(field.getFieldId(), error);
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
         * Get core parameters
         */
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        String gradeTermOid = (String) getParameter(GRADE_TERM_OID_PARAM);

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

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

            // Load teacher and race maps.
            loadMaps(studentCriteria);

            // Add any retrievers or validators.
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("MMSR-CLEAN", new RetrieveStripNameChar());
            calcs.put("MMSR-GENDER", new RetrieveGender());
            calcs.put("MMSR-GRADE", new RetrieveRubricGrade(gradeTermOid, subQuery));
            calcs.put("MMSR-RACE", new RetrieveRace());
            calcs.put("MMSR-SCHOOL", new RetrieveSchoolCode());
            calcs.put("MMSR-TEACHER", new RetrieveTeacher());
            calcs.put("MMSR-HOMECARE", new RetrieveHomeCare());
            calcs.put("MMSR-EARLYCARE", new RetrieveMmsrProgram());
            calcs.put("MMSR-EARLYCARE2", new RetrieveMmsrProgram2());
            calcs.put("MMSR-FARMS", new RetrieveFarms());

            if (StringUtils.isEmpty(m_studentEllAliasField)) {
                calcs.put("MMSR-ELLIND", new RetrieveProgram(DOE_PROG_CODE_ELL, null, subQuery));
            } else {
                calcs.put("MMSR-ELLIND", new RetrieveField(m_studentEllAliasField));
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
                if (code.getStateCode() != null
                        && (code.getStateCode().equals("91") || code.getStateCode().equals("92"))) {
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
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
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
        if (requireReportStatus.booleanValue() && !StringUtils.isEmpty(m_doeStatusField)) {
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
        m_doeStatusField = translateAliasToJavaName(DOE_STATUS_FIELD, false);
        m_doeMmsrScoreField = translateAliasToJavaName(DOE_MMSR_SCORE, true);
        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL_CODE, true);
        m_staffIdField = translateAliasToJavaName(DOE_STAFF_ID, true);
        m_studentEllAliasField = translateAliasToJavaName(DOE_STD_ELL, false);

        m_progCaredForField = translateAliasToJavaName(PROGRAM_HOME_CARE_CODE, false);
        m_progEarlyCareField = translateAliasToJavaName(PROGRAM_PRIOR_CARE_ALIAS, false);

        // Get race code reference codes for use in the race retriever.
        Criteria raceCriteria = new Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);
    }

    /**
     * Load a maps of teachers by homeroom and race codes by personOid.
     *
     * @param studentCriteria Criteria
     */
    private void loadMaps(Criteria studentCriteria) {
        String substType = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_SUBSTITUTE_CODE);
        String active =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

        X2Criteria criteria = new X2Criteria();
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
                String key = schoolOid + "-" + homeroom.trim();
                Collection teachersInHomeroom = m_teacherMap.get(key);
                if (teachersInHomeroom == null) {
                    teachersInHomeroom = new ArrayList<SisStaff>();
                    m_teacherMap.put(key, teachersInHomeroom);
                }
                teachersInHomeroom.add(teacher);

                // Check the teachers secondary homeroom, if present.
                if (!StringUtils.isEmpty(homeroom2)) {
                    key = schoolOid + "-" + homeroom2.trim();
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

        /*
         * Map of race codes by personOid.
         */
        SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

        /*
         * Map of student program participation for MMSR.
         */
        subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria progCriteria = new Criteria();
        progCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_CODE);
        progCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);

        QueryByCriteria progQuery = new QueryByCriteria(StudentProgramParticipation.class, progCriteria);
        m_studentMmsrPrograms =
                getBroker().getGroupedCollectionByQuery(progQuery, StudentProgramParticipation.COL_STUDENT_OID, 100);

    }
}
