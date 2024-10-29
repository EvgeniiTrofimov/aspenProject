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
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;


/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class CTEStateReportData extends StateReportData {


    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class CTEEmptyEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";


        /**
         * Empty no argument constructor for dynamic instantiation.
         */
        public CTEEmptyEntity() {
            // Empty no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the display name of the represented entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder buffer = new StringBuilder();
            buffer.append("Graduation Program Name: " + ((GraduationProgram) getBean()).getName());
            return buffer.toString();
        }

        /**
         * Return errors only if Graduation Program gprObsoleteInd is false.
         *
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValidations()
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidations() {
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            if (!((GraduationProgram) getBean()).getObsoleteIndicator()) {
                errors.addAll(super.getFieldValidations());
            }
            return errors;
        }

        /**
         * initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);
        }
    }

    /**
     * Retrieve district id.
     *
     * @author Follett Software Company
     */
    class RetrieveDistrictId implements FieldRetriever {
        public static final String CALC_ID = "CALC-DIST-ID";

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
            String value = null;

            GraduationProgram program = (GraduationProgram) entity.getBean();
            value = (String) program.getFieldValueByBeanPath(m_cteProgramDistrictId);
            if (StringUtils.isEmpty(value)) {
                value = (String) getOrganization().getFieldValueByBeanPath(m_districtCode);
            }

            return value;
        }
    }

    /*
     * Aliases
     */
    // GRADUATION_PROGRAM table
    protected static final String ALIAS_CTE_PROGRAM_DISTRICT_ID = "DOE CTE PROGRAM DISTRICT ID";
    protected static final String ALIAS_DOE_RI_CTE_PROGRAM_TYPE = "DOE RI CTE PROGRAM TYPE";
    protected static final String ALIAS_DOE_RI_DISTRICT_CODE = "RI Reporting District Code";
    protected static final String ALIAS_GSR_NO_LONGER_PARTICIPATE = "all-gsr-NoLongerParticipating";

    // assessment_column_definition table
    protected static final String ALIAS_RI_CTE_APPRENTICESHIP = "RI CTE Apprenticeship";
    protected static final String ALIAS_RI_CTE_CAREER_PROGRAM_CATAGORY = "RI CTE Career Program Catagory";
    protected static final String ALIAS_RI_CTE_POSTSECONDARY_CREDIT = "RI CTE POSTSECONDARY CREDIT";
    protected static final String ALIAS_RI_CTE_CREDENTIAL_EARNED = "RI CTE REC Credential";

    // Procedure Id
    protected static final String PROCEDURE_ID_EXPDATA_RI_CTE_APRPR = "EXPDATA-RI-CTE-APRPR";
    protected static final String PROCEDURE_ID_EXPDATA_RI_CTE_CRS = "EXPDATA-RI-CTE-CRS";
    protected static final String PROCEDURE_ID_EXPDATA_RI_CTE_PRG = "EXPDATA-RI-CTE-PRG";
    protected static final String PROCEDURE_ID_EXPDATA_RI_CTE_PSCC = "EXPDATA-RI-CTE-PSCC";
    protected static final String PROCEDURE_ID_EXPDATA_RI_CTE_RCGNZ = "EXPDATA-RI-CTE-RCGNZ";
    protected static final String PROCEDURE_ID_EXPDATA_RI_CTE_STDPR = "EXPDATA-RI-CTE-STDPR";

    // param
    protected static final String PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS = "includeExcludeStudents";
    protected static final String PARAM_PROCEDURE_ID = "procedureId";
    // SCHOOL table
    protected static final String ALIAS_STATE_SCHOOL_ID = "State School Id";

    /*
     * others
     */
    protected static final String ASSMT_DEF_ID_CTE = "CTE";

    // errors
    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /*
     * Maps
     */
    // key Retriever Id, value FieldRetriever
    protected Map<String, FieldRetriever> m_calcs;
    // key GraduationProgramOid, value CourseOids
    protected Map<String, Set<String>> m_graduationPrgmCourseMap;
    // key GraduationProgramOid, value SchoolCourseOids
    protected Map<String, Set<String>> m_graduationPrgmSchoolCourseMap;
    // key GraduationProgramOid, value StudentOid + MasterScheduleOid
    protected Map<String, TreeSet<String>> m_graduationPrgmStdCectionMap;
    // key GraduationProgramOid, value StudentOids
    protected Map<String, Set<String>> m_graduationPrgmStudentsMap;
    // key ProgramType, value GraduationProgramOid
    protected Map<String, String> m_programTypeGraduationPrgmOidMap;
    // key cteProgramType + schoolCode, value StudentAssessmentOids
    protected Map<String, Set<String>> m_studentAssesmentOidsMap;
    // key StudentAssessmentOid, value StudentAssessment
    protected Map<String, StudentAssessment> m_studentAssesmentMap;
    /*
     * //key studentsAssesmentOid, value StudentOid
     * protected Map<String, String> m_studentAssesmentStudentMap;
     */
    // key SchoolCourseOid, value SchoolCourse
    protected Map<String, SchoolCourse> m_schoolCourseMap;
    // key MasterScheduleOid, value MasterSchedule
    protected Map<String, MasterSchedule> m_sectionMap;
    // key StudentOid, value Student
    protected Map<String, Student> m_studentMap;
    // key StudentOid, value MasterScheduleOid
    protected Map<String, Set<String>> m_studentSectionsMap;

    /*
     * others
     */
    protected String m_assessmentProgramType = null;
    protected String m_assessmentFieldJavaName = null;
    protected String m_assessmentFieldReferenceTableOid = null;
    protected QueryByCriteria m_byCriteria;
    protected X2Criteria m_criteriaActiveStudentOid;
    protected Class m_cteEntityClass;
    protected String m_cteProgram;
    protected String m_cteProgramDistrictId;
    protected String m_districtCode;
    protected String m_fieldGsrNoLongerParticipate;
    protected X2Criteria m_graduationPrgrmCriteria = null;
    protected String m_schoolCode;
    protected SubQuery m_subQueryActiveStudentOid;

    /**
     * Post process.
     *
     * @return Collection
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

        if (PROCEDURE_ID_EXPDATA_RI_CTE_CRS.equals(getProcedureId())
                || PROCEDURE_ID_EXPDATA_RI_CTE_PRG.equals(getProcedureId())) {
            QueryByCriteria gpQuery = new QueryByCriteria(GraduationProgram.class, getGraduationPrgrmCriteria());
            Map<String, GraduationProgram> programsMap = getBroker().getMapByQuery(gpQuery, X2BaseBean.COL_OID, 10);

            for (Entry<String, GraduationProgram> entry : programsMap.entrySet()) {
                String programOid = entry.getKey();
                GraduationProgram gp = entry.getValue();

                Collection<String> coursesOids = m_graduationPrgmCourseMap.get(programOid);

                if (coursesOids == null || coursesOids.isEmpty()) {
                    StateReportValidationError error =
                            new StateReportValidationError("Program Name: " + gp.getName(),
                                    "", "",
                                    "Active CTE Program has no courses assigned for reporting year.");
                    errors.add(error);
                }
            }
        }

        errors.addAll(super.postProcess());

        return errors;
    }

    /**
     * Gets the active std oids.
     *
     * @return active students
     */
    protected List<String> getActiveStdOids() {
        List<String> activeStudentOids = new ArrayList<>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(getActiveStdOidsSubQuery());

        try {
            while (iterator.hasNext()) {
                Object[] item = (Object[]) iterator.next();
                activeStudentOids.add((String) item[0]);
            }
        } finally {
            iterator.close();
        }

        return activeStudentOids;
    }

    /**
     * Gets the active std oids sub query.
     *
     * @return Sub query
     */
    protected SubQuery getActiveStdOidsSubQuery() {
        if (m_subQueryActiveStudentOid == null) {
            m_subQueryActiveStudentOid =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getActiveStudentOidsCriteria());
        }
        return m_subQueryActiveStudentOid;
    }

    /**
     * get criteria based on GraduationProgram class.
     *
     * @return X 2 criteria
     */
    protected X2Criteria getGraduationPrgrmCriteria() {
        if (m_graduationPrgrmCriteria == null) {
            m_graduationPrgrmCriteria = new X2Criteria();
            m_graduationPrgrmCriteria.addNotEmpty(m_cteProgram, getBroker().getPersistenceKey());
            m_graduationPrgrmCriteria.addNotEqualTo(GraduationProgram.COL_OBSOLETE_INDICATOR, Boolean.TRUE);
        }
        return m_graduationPrgrmCriteria;
    }

    /**
     * find student assessment column definition from alias.
     *
     * @param alias String
     * @return Assessment column definition
     */
    protected AssessmentColumnDefinition getStdAssesmentColumnByAlias(String alias) {
        AssessmentColumnDefinition assessmentColumnDefinition = null;
        if (alias != null) {
            Criteria assesmColumnCriteria = new X2Criteria();
            assesmColumnCriteria.addEqualTo(AssessmentColumnDefinition.COL_ALIAS, alias);
            QueryIterator queryIterator = getBroker()
                    .getIteratorByQuery(new QueryByCriteria(AssessmentColumnDefinition.class, assesmColumnCriteria));
            if (queryIterator.hasNext()) {
                assessmentColumnDefinition = (AssessmentColumnDefinition) queryIterator.next();
            }
        }
        return assessmentColumnDefinition;
    }

    /**
     * find stdAssesmentField in data dictionary by assessmentColumnDefinition alias.
     *
     * @param alias String
     * @param isRequired boolean
     * @return String
     */
    protected String getStdAssesmentFieldByAlias(String alias, boolean isRequired) {
        String returnValue = null;
        AssessmentColumnDefinition assessmentColumnDefinition = getStdAssesmentColumnByAlias(alias);
        DataFieldConfig dataFieldConfig = null;
        if (assessmentColumnDefinition != null) {
            dataFieldConfig = assessmentColumnDefinition.getDataFieldConfig();

            if (dataFieldConfig != null) {
                DataDictionaryField dataDictionaryField =
                        getDataDictionary().findDataDictionaryField(dataFieldConfig.getDataFieldOid());
                returnValue = dataDictionaryField.getSystemDataField().getJavaName();
            }
        }

        if (isRequired && returnValue == null) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }
        return returnValue;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for graduationProgram to load.
     * Define list of field definitions for the export.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    protected void initialize() {
        initializeField();
        X2Criteria graduationPrgrmCriteria = getGraduationPrgrmCriteria();
        if (PROCEDURE_ID_EXPDATA_RI_CTE_CRS.equals(getProcedureId())
                || PROCEDURE_ID_EXPDATA_RI_CTE_PRG.equals(getProcedureId())) {
            graduationPrgrmCriteria.addIn(GraduationProgram.REL_STUDENT_PROGRAM_STUDIES + ModelProperty.PATH_DELIMITER
                    + GraduationStudentProgram.COL_STUDENT_OID, getActiveStdOidsSubQuery());
        }
        initializeMaps(graduationPrgrmCriteria);
        initializeQuery();
        initializeEntityClass();
        if (getSetupErrors().size() == 0) {
            setQuery(m_byCriteria);
            setEntityClass(m_cteEntityClass);
            initializeCalcs();
            super.addCalcs(m_calcs);
        }
    }

    /**
     * initialize m_calcs <br>
     * m_calcs using like custom calculation map <br>
     * by default create empty HashMap. <br>
     * Override this method for initialize calculation map
     */
    protected void initializeCalcs() {
        m_calcs = new HashMap<String, FieldRetriever>();
        m_calcs.put(RetrieveDistrictId.CALC_ID, new RetrieveDistrictId());
    }

    /**
     * initialize m_entityClass <br>
     * m_entityClass using for sets the StateReportEntity subclass for this export
     * by default set CTEEmptyEntity.class
     * Override this method for initialize entity class
     */
    protected void initializeEntityClass() {
        m_cteEntityClass = CTEEmptyEntity.class;
    }

    /**
     * initialize field.
     */
    protected void initializeField() {
        m_cteProgram = translateAliasToJavaName(ALIAS_DOE_RI_CTE_PROGRAM_TYPE, true);
        m_schoolCode = translateAliasToJavaName(ALIAS_STATE_SCHOOL_ID, true);
        m_assessmentProgramType = getStdAssesmentFieldByAlias(ALIAS_RI_CTE_CAREER_PROGRAM_CATAGORY, true);
        m_cteProgramDistrictId = translateAliasToJavaName(ALIAS_CTE_PROGRAM_DISTRICT_ID, true);
        m_districtCode = translateAliasToJavaName(ALIAS_DOE_RI_DISTRICT_CODE, true);
        m_fieldGsrNoLongerParticipate = translateAliasToJavaName(ALIAS_GSR_NO_LONGER_PARTICIPATE, true);
    }

    /**
     * initialize m_graduationPrgmSchoolCourseMap <br>
     * key GraduationProgramOid <br>
     * value SchoolCourseOids <br>
     * .
     *
     * @param graduationPrgrmCriteria X2Criteria
     */
    protected void initializeGraduationPrgmCourseMap(X2Criteria graduationPrgrmCriteria) {
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addIn(GraduationCourseRequirement.REL_REQUIREMENT + ModelProperty.PATH_DELIMITER +
                GraduationRequirement.COL_PROGRAM_STUDIES_OID,
                new SubQuery(GraduationProgram.class, X2BaseBean.COL_OID, graduationPrgrmCriteria));
        courseCriteria.addEqualTo(GraduationCourseRequirement.REL_COURSE + ModelProperty.PATH_DELIMITER +
                Course.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                getCurrentContext().getOid());

        String[] columns = new String[] {
                GraduationCourseRequirement.REL_COURSE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                GraduationCourseRequirement.REL_REQUIREMENT + ModelProperty.PATH_DELIMITER +
                        GraduationRequirement.COL_PROGRAM_STUDIES_OID
        };

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(GraduationCourseRequirement.class, columns, courseCriteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        m_graduationPrgmCourseMap = new HashMap<String, Set<String>>();
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String coursesOid = (String) row[0];
                String graduationProgramOid = (String) row[1];

                Set<String> coursesOids = m_graduationPrgmCourseMap.get(graduationProgramOid);
                if (coursesOids == null) {
                    coursesOids = new HashSet<String>();
                    m_graduationPrgmCourseMap.put(graduationProgramOid, coursesOids);
                }
                coursesOids.add(coursesOid);

            }
        } finally {
            iterator.close();
        }

    }

    /**
     * initialize m_graduationPrgmSchoolCourseMap <br>
     * key GraduationProgramOid <br>
     * value SchoolCourseOids <br>
     * .
     *
     * @param graduationPrgrmCriteria X2Criteria
     */
    protected void initializeGraduationPrgmSchoolCourseMap(X2Criteria graduationPrgrmCriteria) {
        X2Criteria courseCriteria = new X2Criteria();
        courseCriteria.addIn(GraduationCourseRequirement.REL_REQUIREMENT + ModelProperty.PATH_DELIMITER +
                GraduationRequirement.COL_PROGRAM_STUDIES_OID,
                new SubQuery(GraduationProgram.class, X2BaseBean.COL_OID, graduationPrgrmCriteria));


        String[] columns = new String[] {GraduationCourseRequirement.REL_COURSE + ModelProperty.PATH_DELIMITER +
                Course.REL_SCHOOL_COURSES + ModelProperty.PATH_DELIMITER +
                X2BaseBean.COL_OID,

                GraduationCourseRequirement.REL_REQUIREMENT + ModelProperty.PATH_DELIMITER +
                        GraduationRequirement.COL_PROGRAM_STUDIES_OID,

        };

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(GraduationCourseRequirement.class, columns, courseCriteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        m_graduationPrgmSchoolCourseMap = new HashMap<String, Set<String>>();
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolCoursesOid = (String) row[0];
                String graduationProgramOid = (String) row[1];

                Set<String> schoolCoursesOids = m_graduationPrgmSchoolCourseMap.get(graduationProgramOid);
                if (schoolCoursesOids == null) {
                    schoolCoursesOids = new HashSet<String>();
                    m_graduationPrgmSchoolCourseMap.put(graduationProgramOid, schoolCoursesOids);
                }
                schoolCoursesOids.add(schoolCoursesOid);
            }
        } finally {
            iterator.close();
        }

    }


    /**
     * initialize m_graduationPrgmCectionMap <br>
     * notes: prepare m_graduationPrgmStudentsMap before call this method <br>
     * key GraduationProgramOid <br>
     * value MasterScheduleOids <br>
     * .
     */
    protected void initializeGraduationPrgmSectionMap() {
        m_graduationPrgmStdCectionMap = new HashMap<String, TreeSet<String>>();
        for (String key : m_graduationPrgmStudentsMap.keySet()) {
            Set<String> students = m_graduationPrgmStudentsMap.get(key);
            for (String stdOid : students) {
                Set<String> sections = m_studentSectionsMap.get(stdOid);
                if (sections != null) {
                    for (String sectionOid : sections) {

                        TreeSet<String> stdSectionOids = m_graduationPrgmStdCectionMap.get(key);
                        if (stdSectionOids == null) {
                            stdSectionOids = new TreeSet<String>();
                            m_graduationPrgmStdCectionMap.put(key, stdSectionOids);
                        }
                        stdSectionOids.add(stdOid + sectionOid);
                    }
                }
            }
        }
    }


    /**
     * initialize m_graduationPrgmStudentsMap <br>
     * key GraduationProgramOid <br>
     * value StudentOids <br>
     * .
     *
     * @param graduationPrgmCriteria X2Criteria
     */
    protected void initializeGraduationPrgmStudentsMap(X2Criteria graduationPrgmCriteria) {
        String[] columns = new String[] {X2BaseBean.COL_OID,
                GraduationProgram.REL_STUDENT_PROGRAM_STUDIES + ModelProperty.PATH_DELIMITER +
                        GraduationStudentProgram.COL_STUDENT_OID,
                m_cteProgram
        };

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(GraduationProgram.class, columns, graduationPrgmCriteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        m_graduationPrgmStudentsMap = new HashMap<String, Set<String>>();
        m_programTypeGraduationPrgmOidMap = new HashMap<String, String>();
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String graduationPgrmOid = (String) row[0];
                String stdOid = (String) row[1];
                String progType = (String) row[2];

                if (!m_programTypeGraduationPrgmOidMap.containsKey(progType)) {
                    m_programTypeGraduationPrgmOidMap.put(progType, graduationPgrmOid);
                }

                Set<String> students = m_graduationPrgmStudentsMap.get(graduationPgrmOid);
                if (students == null) {
                    students = new HashSet<String>();
                    m_graduationPrgmStudentsMap.put(graduationPgrmOid, students);
                }
                students.add(stdOid);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * initialize m_studentsAssesmMap <br>
     * key cteProgramType + schoolCode <br>
     * value StudentAssessmentOids <br>
     * .
     */
    protected void initializeStudentsAssesmMap() {
        Criteria studentsAssesmCriteria = new X2Criteria();
        studentsAssesmCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                AssessmentDefinition.COL_ID, ASSMT_DEF_ID_CTE);
        studentsAssesmCriteria.addNotNull(m_assessmentFieldJavaName);
        String[] columns = new String[] {X2BaseBean.COL_OID, m_assessmentProgramType,
                StudentAssessment.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_schoolCode};

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(StudentAssessment.class, columns, studentsAssesmCriteria);
        query.addOrderByAscending(m_assessmentFieldJavaName);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        m_studentAssesmentOidsMap = new HashMap<String, Set<String>>();
        List<String> allstdAssessmentOids = new ArrayList<String>();
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String stdAssessmentOid = (String) row[0];
                String cteProgramType = (String) row[1];
                String key = cteProgramType;
                Set<String> stdAssessmentOids;
                allstdAssessmentOids.add(stdAssessmentOid);
                if (!m_studentAssesmentOidsMap.containsKey(key)) {
                    stdAssessmentOids = new HashSet<String>();
                    m_studentAssesmentOidsMap.put(key, stdAssessmentOids);
                } else {
                    stdAssessmentOids = m_studentAssesmentOidsMap.get(key);
                }
                stdAssessmentOids.add(stdAssessmentOid);
            }
        } finally {
            iterator.close();
        }

        if (!allstdAssessmentOids.isEmpty()) {
            Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, allstdAssessmentOids);
            QueryByCriteria byCriteria = new QueryByCriteria(StudentAssessment.class, criteria);
            m_studentAssesmentMap = getBroker().getMapByQuery(byCriteria, X2BaseBean.COL_OID, 100);
        }

    }

    /**
     * initialize maps.
     *
     * @param graduationPrgrmCriteria X2Criteria
     */
    protected void initializeMaps(X2Criteria graduationPrgrmCriteria) {
        initializeGraduationPrgmCourseMap(graduationPrgrmCriteria);
    }


    /**
     * initialize m_schoolCourseMap <br>
     * key schoolCourseOid <br>
     * value schoolCourse.
     */
    protected void initializeSchoolCourseMap() {
        X2Criteria courseCriteria = new X2Criteria();
        Collection<String> schoolCourseOids = new HashSet<String>();
        for (String key : m_graduationPrgmSchoolCourseMap.keySet()) {
            schoolCourseOids.addAll(m_graduationPrgmSchoolCourseMap.get(key));
        }
        courseCriteria.addIn(X2BaseBean.COL_OID, schoolCourseOids);
        QueryByCriteria byCriteria = new QueryByCriteria(SchoolCourse.class, courseCriteria);
        m_schoolCourseMap = getBroker().getMapByQuery(byCriteria, X2BaseBean.COL_OID, 100);
    }

    /**
     * initialize m_sectionMap <br>
     * notes: prepare m_studentSectionsMap before call this method <br>
     * key MasterScheduleOid <br>
     * value MasterSchedule.
     */
    protected void initializeSectionMap() {
        Set<String> sectionOidCollection = new HashSet<String>();

        for (String key : m_studentSectionsMap.keySet()) {
            sectionOidCollection.addAll(m_studentSectionsMap.get(key));
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(X2BaseBean.COL_OID, sectionOidCollection);
        QueryByCriteria byCriteria = new QueryByCriteria(MasterSchedule.class, criteria);
        m_sectionMap = getBroker().getMapByQuery(byCriteria, X2BaseBean.COL_OID, 100);
    }

    /**
     * initialize m_studentMap <br>
     * notes: prepare m_graduationPrgmStudentsMap before call this method <br>
     * key StudentOid <br>
     * value Student.
     */
    protected void initializeStudentMap() {
        Set<String> stdCollection = new HashSet<String>();
        for (String key : m_graduationPrgmStudentsMap.keySet()) {
            stdCollection.addAll(m_graduationPrgmStudentsMap.get(key));
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(X2BaseBean.COL_OID, stdCollection);
        QueryByCriteria byCriteria = new QueryByCriteria(Student.class, criteria);
        m_studentMap = getBroker().getMapByQuery(byCriteria, X2BaseBean.COL_OID, 500);
    }

    /**
     * initialize m_studentSectionsMap <br>
     * notes: prepare m_graduationPrgmStudentsMap before call this method <br>
     * key StudentOid <br>
     * value MasterScheduleOids.
     */
    protected void initializeStudentSections() {
        Set<String> stdCollection = new HashSet<String>();
        for (String key : m_graduationPrgmStudentsMap.keySet()) {
            stdCollection.addAll(m_graduationPrgmStudentsMap.get(key));
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentSchedule.COL_STUDENT_OID, stdCollection);

        String[] columns = new String[] {StudentSchedule.COL_SECTION_OID,
                StudentSchedule.COL_STUDENT_OID
        };
        ReportQueryByCriteria query = new ReportQueryByCriteria(StudentSchedule.class, columns, criteria);
        query.addOrderByAscending(X2BaseBean.COL_OID);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        m_studentSectionsMap = new HashMap<String, Set<String>>();
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String sectionOid = (String) row[0];
                String stdOid = (String) row[1];

                Set<String> sections = m_studentSectionsMap.get(stdOid);
                if (sections == null) {
                    sections = new HashSet<String>();
                    m_studentSectionsMap.put(stdOid, sections);
                }
                sections.add(sectionOid);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * initialize m_byCriteria <br>
     * this query to be used to gather data for this export.
     */
    protected void initializeQuery() {
        m_byCriteria = new QueryByCriteria(GraduationProgram.class, getGraduationPrgrmCriteria());
    }

    /**
     * Gets the active student oids criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getActiveStudentOidsCriteria() {
        if (m_criteriaActiveStudentOid == null) {
            StudentHistoryHelper helper = new StudentHistoryHelper(this);
            helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) != null
                    && getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) instanceof Boolean) {
                Boolean exclude =
                        Boolean.valueOf(!((Boolean) getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS)).booleanValue());
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, exclude);
            }
            m_criteriaActiveStudentOid = helper.getStudentCriteria();
        }
        return m_criteriaActiveStudentOid;
    }

}
