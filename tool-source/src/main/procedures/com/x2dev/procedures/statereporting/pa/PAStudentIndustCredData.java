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

package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Class for Student Industry Credentials export.
 */
public class PAStudentIndustCredData extends StateReportData {
    /**
     * Entity class for Student Industry Credentials.
     */
    public static class PAStudentSICEntity extends StateReportEntity {
        private static final String GRD_CODE_AG_COURSES = "Ag Courses";

        private PAStudentIndustCredData m_data = null;
        private Map<SisSchool, Map<String, Collection<Transcript>>> m_posTranscriptsMap = null;
        private SisStudent m_student = null;

        /**
         * Gets the cte std assessment.
         *
         * @return student assessment with CTE Industry Credentials
         */
        public StudentAssessment getCteStdAssessment() {
            return m_data.getCteAssessmByStdOidMap(m_student.getOid());
        }

        /**
         * Gets the cte std assessment data dictionary.
         *
         * @return extendent data dictionary for CTE Assessment.
         */
        public DataDictionary getCteStdAssessmentDataDictionary() {
            DataDictionary extendedDataDictionary = null;
            StudentAssessment cteAssessment = getCteStdAssessment();
            if (cteAssessment != null) {
                extendedDataDictionary = DataDictionary.getDistrictDictionary(cteAssessment.getExtendedDataDictionary(),
                        m_data.getBroker().getPersistenceKey());
            }
            return extendedDataDictionary;
        }

        /**
         * Gets the program of study.
         *
         * @return student's program of study.
         */
        public GraduationProgram getProgramOfStudy() {
            String currentStudentPosOid = m_data.getPosOidByStudentOid(m_student.getOid());
            GraduationProgram pos = m_data.getPosByOid(currentStudentPosOid);
            return pos;
        }

        /**
         * Gets the student.
         *
         * @return student.
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (SisStudent) bean;

            m_data = (PAStudentIndustCredData) data;

            m_posTranscriptsMap = initSchoolTranscriptsMap();

            boolean studentHasPosCourses = (m_posTranscriptsMap.size() > 0);
            if (!(studentHasPosCourses && studentHasCteAssessmentWithCPC() && studentTakenAgCoursesThisYear())) {
                setRowCount(0);
            }
        }

        /**
         * Student has cte assessment with CPC.
         *
         * @return true if student has CTE assessment with Career Program Category and PA CTE
         *         Credentials Earned
         *         populated, otherwise false.
         */
        public boolean studentHasCteAssessmentWithCPC() {
            boolean studHasCteAssessWithCip = false;

            StudentAssessment stdAssessment = getCteStdAssessment();
            if (stdAssessment != null) {
                studHasCteAssessWithCip = (stdAssessment.getFieldValueByAlias(ALIAS_DOE_CAREER_PROGRAM,
                        getCteStdAssessmentDataDictionary()) != null &&
                        stdAssessment.getFieldValueByAlias(ALIAS_DOE_CREDENTIAL_EARNED,
                                getCteStdAssessmentDataDictionary()) != null);
            }

            return studHasCteAssessWithCip;
        }

        /**
         * Inits the school transcripts map.
         *
         * @return maps of transcripts keyed on requirements oids grouped by schools.
         */
        private Map<SisSchool, Map<String, Collection<Transcript>>> initSchoolTranscriptsMap() {
            Map<SisSchool, Map<String, Collection<Transcript>>> transcriptsByReqOidBySklMap =
                    new HashMap<SisSchool, Map<String, Collection<Transcript>>>();
            // Get Program of study of the student.
            String posOfCurStudent = m_data.getPosOidByStudentOid(m_student.getOid());
            // Get courses of the Program of study grouped by requirement oid.
            Map<String, Collection<String>> posCoursesOidsByReqOid = m_data.getCoursesOidsByPosOid(posOfCurStudent);

            Collection<Transcript> allTranscripts = m_student.getTranscripts();

            // Find transcripts for Program of study, group them by schools and by requirements.
            for (Transcript curTranscript : allTranscripts) {
                String courseOid = curTranscript.getSchoolCourse().getCourseOid();
                for (String reqOid : posCoursesOidsByReqOid.keySet()) {
                    boolean transcriptIsPos = false;
                    if (posCoursesOidsByReqOid.get(reqOid).contains(courseOid)) {
                        transcriptIsPos = true;
                    }

                    if (transcriptIsPos) {
                        SisSchool curSchool = curTranscript.getSchool();
                        Map<String, Collection<Transcript>> transcriptsByReqOid =
                                transcriptsByReqOidBySklMap.get(curSchool);
                        if (transcriptsByReqOid == null) {
                            transcriptsByReqOid = new HashMap<String, Collection<Transcript>>();
                            transcriptsByReqOidBySklMap.put(curSchool, transcriptsByReqOid);
                        }

                        Collection<Transcript> transcripts = transcriptsByReqOid.get(reqOid);

                        if (transcripts == null) {
                            transcripts = new ArrayList<Transcript>();
                            transcriptsByReqOid.put(reqOid, transcripts);
                        }

                        transcripts.add(curTranscript);
                    }
                }
            }

            return transcriptsByReqOidBySklMap;
        }

        /**
         * Student taken ag courses this year.
         *
         * @return true if student has taken ag courses this year, otherwise false.
         */
        private boolean studentTakenAgCoursesThisYear() {
            boolean studentTakenAgCoursesThisYear = false;

            Map<String, Collection<Transcript>> transcriptByReqMap = new HashMap<String, Collection<Transcript>>();
            for (SisSchool school : m_posTranscriptsMap.keySet()) {
                Map<String, Collection<Transcript>> curMap = m_posTranscriptsMap.get(school);
                transcriptByReqMap.putAll(curMap);
            }

            // Get all student's transcript with AG course.
            for (String reqOid : transcriptByReqMap.keySet()) {
                // Check whether requirement with 'AG Courses'
                GraduationRequirement req = m_data.getRequirementByOid(reqOid);

                if (GRD_CODE_AG_COURSES.equals(req.getCode())) {
                    // Check whether student has 'AG Courses' transcripts for current year.
                    Collection<Transcript> transcripts = transcriptByReqMap.get(reqOid);
                    for (Transcript curTranscript : transcripts) {
                        SisDistrictSchoolYearContext context = curTranscript.getDistrictContext();

                        if (context != null) {
                            if (context.equals(m_data.getCurrentContext())) {
                                studentTakenAgCoursesThisYear = true;
                            }
                        }
                    }
                }
            }

            return studentTakenAgCoursesThisYear;
        }
    }

    /**
     * Field retriever.
     */
    public static class RetrieveSICInfo implements FieldRetriever {
        private static final String CALC_PARAM_CIP_CODE = "CIP_CODE";
        private static final String CALC_PARAM_CIP_LOCATION_CODE = "LOC_CODE";
        private static final String CALC_PARAM_INDUSTRY_CREDENTIAL = "CRED_EARNED";

        private static final String STRING_EMPTY = "";
        private static final String STRING_POINT = ".";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            PAStudentIndustCredData sicData = (PAStudentIndustCredData) data;
            PAStudentSICEntity sicEntity = (PAStudentSICEntity) entity;

            StudentAssessment cteAssessment = sicEntity.getCteStdAssessment();
            boolean stdHasCteAssessment = (cteAssessment != null);
            DataDictionary extendedDataDictionary = null;
            if (stdHasCteAssessment) {
                extendedDataDictionary = DataDictionary.getDistrictDictionary(cteAssessment.getExtendedDataDictionary(),
                        sicData.getBroker().getPersistenceKey());
            }

            Object value = null;

            if (field.getParameter().equals(CALC_PARAM_CIP_LOCATION_CODE)) {
                value = sicEntity.getStudent().getSchool().getSchoolId();
            } else if (field.getParameter().equals(CALC_PARAM_CIP_CODE)) {
                String cipCode =
                        (String) cteAssessment.getFieldValueByAlias(ALIAS_DOE_CAREER_PROGRAM, extendedDataDictionary);
                value = cipCode.replace(STRING_POINT, STRING_EMPTY);
            } else if (field.getParameter().equals(CALC_PARAM_INDUSTRY_CREDENTIAL)) {
                value = cteAssessment.getFieldValueByAlias(ALIAS_DOE_CREDENTIAL_EARNED, extendedDataDictionary);
            }

            return value;
        }

    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    public static final String ALIAS_CIP_NUMBER = "DOE CIP NUMBER";
    public static final String ALIAS_DOE_CAREER_PROGRAM = "DOE CAREER PROGRAM";
    public static final String ALIAS_DOE_CREDENTIAL_EARNED = "DOE CREDENTIAL EARNED";

    private static final String CTE_INDUSTRY_CREDENTIALS = "CTE CREDENTIALS";

    private static final String GENERAL_FAMILY_CONSUMER_SCIENCE = "19.0101";

    private static final String PARAM_REPORT_DATE = "reportDate";

    private static final String SIC_CALC = "SIC_CALC";

    protected String m_fieldCipCode = null;

    /**
     * Instance variables.
     */
    Map<String, Collection<StudentAssessment>> m_assessmentsByStdOidMap = null;
    Map<String, Map<String, Collection<String>>> m_coursesByReqOidByPosOidMap = null;
    StudentHistoryHelper m_helper = null;
    Map<String, Collection<GraduationProgram>> m_posByOidMap = null;
    Map<String, String> m_posOidByStdOidMap = null;
    PlainDate m_reportDate = null;
    Map<String, Collection<GraduationRequirement>> m_requirementsByOidMap = null;

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
        initializeFields();

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        m_coursesByReqOidByPosOidMap = getProgramsWithCourses();
        m_posByOidMap = getAllProgramsOfStudy();
        m_posOidByStdOidMap = getStudentsWithPrograms(m_posByOidMap.keySet());

        m_posByOidMap = getAllProgramsOfStudy();
        m_requirementsByOidMap = getRequirements();

        m_helper = new StudentHistoryHelper(this);

        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        Collection<String> studentsHavingPosOids = m_posOidByStdOidMap.keySet();
        studentCriteria.addIn(X2BaseBean.COL_OID, studentsHavingPosOids);

        m_assessmentsByStdOidMap = getCteAssessmentsOfStudents(studentCriteria);

        setQuery(m_helper.getStudentQuery(true));

        setEntityClass(PAStudentSICEntity.class);

        registerFieldRetrievers();
    }

    /**
     * Gets the cte assessm by std oid map.
     *
     * @param stdOid String
     * @return StudentAssessment by student oid.
     */
    protected StudentAssessment getCteAssessmByStdOidMap(String stdOid) {
        StudentAssessment stdAssessment = null;

        Collection<StudentAssessment> stdAssessments = m_assessmentsByStdOidMap.get(stdOid);

        if (stdAssessments != null) {
            for (StudentAssessment firstAssessment : stdAssessments) {
                stdAssessment = firstAssessment;
                break;
            }
        }

        return stdAssessment;
    }

    /**
     * Gets the courses oids by pos oid.
     *
     * @param posOid String
     * @return map of course oids keyed on requirement oid by passed POS oid.
     */
    protected Map<String, Collection<String>> getCoursesOidsByPosOid(String posOid) {
        return m_coursesByReqOidByPosOidMap.get(posOid);
    }

    /**
     * Gets the pos by oid.
     *
     * @param posOid String
     * @return Graduation program
     */
    protected GraduationProgram getPosByOid(String posOid) {
        GraduationProgram pos = null;
        Collection<GraduationProgram> programs = m_posByOidMap.get(posOid);
        if (programs != null) {
            for (GraduationProgram firstPos : programs) {
                pos = firstPos;
                break;
            }
        }

        return pos;
    }

    /**
     * Gets the pos oid by student oid.
     *
     * @param studentOid String
     * @return oid of program of study by passed student oid.
     */
    protected String getPosOidByStudentOid(String studentOid) {
        return m_posOidByStdOidMap.get(studentOid);
    }

    /**
     * Gets the requirement by oid.
     *
     * @param reqOid String
     * @return graduation requirement by graduation requirement oid.
     */
    protected GraduationRequirement getRequirementByOid(String reqOid) {
        GraduationRequirement req = null;
        Collection<GraduationRequirement> reqs = m_requirementsByOidMap.get(reqOid);
        if (reqs != null) {
            for (GraduationRequirement firstRequirement : reqs) {
                req = firstRequirement;
                break;
            }
        }

        return req;
    }

    /**
     * Gets the all programs of study.
     *
     * @return all programs of study keyed on oids.
     */
    private Map<String, Collection<GraduationProgram>> getAllProgramsOfStudy() {
        X2Criteria posCriteria = new X2Criteria();
        posCriteria.addNotEmpty(m_fieldCipCode, getBroker().getPersistenceKey());
        posCriteria.addNotEqualTo(m_fieldCipCode, GENERAL_FAMILY_CONSUMER_SCIENCE);
        QueryByCriteria posQuery = new QueryByCriteria(GraduationProgram.class, posCriteria);

        Map<String, Collection<GraduationProgram>> programsByOidMap =
                getBroker().getGroupedCollectionByQuery(posQuery, X2BaseBean.COL_OID, 16);

        return programsByOidMap;
    }

    /**
     * Gets the cte assessments of students.
     *
     * @param studentCriteria X2Criteria
     * @return Map
     */
    private Map<String, Collection<StudentAssessment>> getCteAssessmentsOfStudents(X2Criteria studentCriteria) {
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria assessmentsCriteria = new X2Criteria();
        assessmentsCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentsSubQuery);
        assessmentsCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getStartDate());
        assessmentsCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, m_reportDate);
        assessmentsCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION +
                ModelProperty.PATH_DELIMITER +
                AssessmentDefinition.COL_ID, CTE_INDUSTRY_CREDENTIALS);

        QueryByCriteria assessmentsQuery = new QueryByCriteria(StudentAssessment.class, assessmentsCriteria);

        return getBroker().getGroupedCollectionByQuery(assessmentsQuery, StudentAssessment.COL_STUDENT_OID, 256);
    }

    /**
     * Get all graduation requirement with appropriate courses.
     *
     * @return Map
     */
    private Map<String, Map<String, Collection<String>>> getProgramsWithCourses() {
        HashMap<String, Map<String, Collection<String>>> programReqCourses =
                new HashMap<String, Map<String, Collection<String>>>();

        String[] fields = {GraduationCourseRequirement.COL_COURSE_OID,
                GraduationCourseRequirement.REL_REQUIREMENT +
                        ModelProperty.PATH_DELIMITER +
                        GraduationRequirement.COL_PROGRAM_STUDIES_OID,
                GraduationCourseRequirement.REL_REQUIREMENT +
                        ModelProperty.PATH_DELIMITER +
                        X2BaseBean.COL_OID};

        X2Criteria coursesWithPosCriteria = new X2Criteria();
        ReportQueryByCriteria coursesWithPosQuery = new ReportQueryByCriteria(GraduationCourseRequirement.class,
                fields,
                coursesWithPosCriteria);

        ReportQueryIterator coursesWithPosOids = getBroker().getReportQueryIteratorByQuery(coursesWithPosQuery);

        try {
            final int GRAD_FIELD_COURSE_OID = 0;
            final int GRAD_FIELD_PGM_OID = 1;
            final int GRAD_FIELD_REQ_OID = 2;

            while (coursesWithPosOids.hasNext()) {
                Object[] currentFields = (Object[]) coursesWithPosOids.next();

                String currentCourseOid = (String) currentFields[GRAD_FIELD_COURSE_OID];
                String programOid = (String) currentFields[GRAD_FIELD_PGM_OID];
                String reqOid = (String) currentFields[GRAD_FIELD_REQ_OID];

                Map<String, Collection<String>> reqCourses = programReqCourses.get(programOid);

                if (reqCourses == null) {
                    reqCourses = new HashMap<String, Collection<String>>();
                    programReqCourses.put(programOid, reqCourses);
                }

                Collection<String> courses = reqCourses.get(reqOid);

                if (courses == null) {
                    courses = new ArrayList<String>();
                    reqCourses.put(reqOid, courses);
                }

                courses.add(currentCourseOid);
            }
        } finally {
            coursesWithPosOids.close();
        }

        return programReqCourses;
    }

    /**
     * Gets the requirements.
     *
     * @return map of graduation requirements by graduation requirement oid.
     */
    private Map<String, Collection<GraduationRequirement>> getRequirements() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(GraduationRequirement.COL_PROGRAM_STUDIES_OID, m_posByOidMap.keySet());
        QueryByCriteria reqQuery = new QueryByCriteria(GraduationRequirement.class, criteria);

        return getBroker().getGroupedCollectionByQuery(reqQuery, X2BaseBean.COL_OID, 256);
    }

    /**
     * Get all students with Pos.
     *
     * @param posOids Collection<String>
     * @return Hash map
     */
    private HashMap<String, String> getStudentsWithPrograms(Collection<String> posOids) {
        HashMap<String, String> stdPos = new HashMap<String, String>();

        X2Criteria stdWithPosCriteria = new X2Criteria();

        stdWithPosCriteria.addIn(GraduationStudentProgram.REL_PROGRAM_STUDIES +
                ModelProperty.PATH_DELIMITER +
                X2BaseBean.COL_OID, posOids);

        String[] fields = {GraduationStudentProgram.COL_STUDENT_OID,
                GraduationStudentProgram.COL_PROGRAM_STUDIES_OID};

        ReportQueryByCriteria studentWithPosQuery = new ReportQueryByCriteria(GraduationStudentProgram.class,
                fields, stdWithPosCriteria);

        ReportQueryIterator stdWithPosIterator = getBroker().getReportQueryIteratorByQuery(studentWithPosQuery);

        try {
            final int FIELD_STUDENT_OID = 0;
            final int FIELD_PROGRAM_OID = 1;

            while (stdWithPosIterator.hasNext()) {
                Object[] currentFields = (Object[]) stdWithPosIterator.next();

                String studentOid = (String) currentFields[FIELD_STUDENT_OID];
                String programOid = (String) currentFields[FIELD_PROGRAM_OID];

                stdPos.put(studentOid, programOid);
            }
        } finally {
            stdWithPosIterator.close();
        }

        return stdPos;
    }

    /**
     * Lookup field aliases and paths.
     * Get data from input definition
     */
    private void initializeFields() {
        m_fieldCipCode = translateAliasToJavaName(ALIAS_CIP_NUMBER, true);
    }

    /**
     * Register custom fieldRetrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(SIC_CALC, new RetrieveSICInfo());
        super.addCalcs(calcs);
    }
}
