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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.sis.model.beans.GraduationStudentProgram;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;


/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class CTEStudentPrograms extends CTEStateReportData {

    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class CTEStudentProgramEntity extends StateReportEntity {

        /*
         * Aliases
         */
        // STUDENT table
        private static final String ALIAS_DOE_APRENTICESHIP_CODE = "DOE APRENTICESHIP CODE";
        private static final String ALIAS_TECH_CENTER_CODE = "Tech Center Code";

        /*
         * others
         */
        private CTEStudentPrograms m_CTEStudentPrograms;
        private List<String> m_studentsWithCourse;

        /**
         * Empty no argument constructor for dynamic instantiation.
         */
        public CTEStudentProgramEntity() {
            // Empty no argument constructor for dynamic instantiation.
        }

        /**
         * get value from Student by alias DOE APRENTICESHIP CODE.
         *
         * @return String
         */
        public String getApprenticeshipCode() {
            String studentOid = m_studentsWithCourse.get(getCurrentRow());
            Student student = m_CTEStudentPrograms.m_studentMap.get(studentOid);
            return (String) student.getFieldValueByAlias(ALIAS_DOE_APRENTICESHIP_CODE);
        }

        /**
         * get value from Student by alias Tech Center Code.
         *
         * @return String
         */
        public String getCareerTechCenterSchoolCode() {
            Student student = getCurrentStudent();
            return (String) student.getFieldValueByAlias(ALIAS_TECH_CENTER_CODE);
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
            buffer.append(getBean().getClass());
            buffer.append(" ");
            buffer.append(getBean().getOid());
            return buffer.toString();
        }

        /**
         * get State Id from Student.
         *
         * @return String
         */
        public String getSasid() {
            Student student = getCurrentStudent();
            return student.getStateId();

        }

        /**
         * get value from School by alias State School Id.
         *
         * @return String
         */
        public String getSchoolCode() {
            Student student = getCurrentStudent();
            return (String) student.getSchool().getFieldValueByAlias(ALIAS_STATE_SCHOOL_ID);
        }

        /**
         * initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            GraduationProgram graduationProgram = (GraduationProgram) bean;
            m_CTEStudentPrograms = (CTEStudentPrograms) data;
            if (graduationProgram.getFieldValueByBeanPath(m_CTEStudentPrograms.m_cteProgram) == null) {
                return;
            }
            ArrayList<String> studentOids = new ArrayList<String>();
            if (m_CTEStudentPrograms.m_graduationPrgmStudentsMap.containsKey(graduationProgram.getOid())) {
                studentOids.addAll(m_CTEStudentPrograms.m_graduationPrgmStudentsMap.get(graduationProgram.getOid()));
            }
            studentOids.retainAll(m_CTEStudentPrograms.m_activeStudentOids);

            Collection<GraduationStudentProgram> gsrList = graduationProgram.getStudentProgramStudies();
            Collection<String> stdOisToRemove = gsrList
                    .stream()
                    .filter(gsr -> BooleanAsStringConverter.TRUE
                            .equals(gsr.getFieldValueByBeanPath(m_CTEStudentPrograms.m_fieldGsrNoLongerParticipate)))
                    .map(gsrToConsider -> gsrToConsider.getStudentOid())
                    .collect(Collectors.toList());

            if (stdOisToRemove != null) {
                studentOids.removeAll(stdOisToRemove);
            }
            m_studentsWithCourse = new ArrayList<String>();
            m_studentsWithCourse.addAll(studentOids);
            setRowCount(m_studentsWithCourse.size());
        }

        /**
         * get current Student.
         *
         * @return Student
         */
        private Student getCurrentStudent() {
            String studentOid = m_studentsWithCourse.get(getCurrentRow());
            return m_CTEStudentPrograms.m_studentMap.get(studentOid);
        }
    }

    /**
     * The Class RetrieveGSRValues.
     */
    class RetrieveGSRValues implements FieldRetriever {

        private static final String CALC_ID = "CALC-GSR";

        /*
         * Retriever Parameters
         */
        private static final String PARAM_COURSEWORK = "COURSEWORK";
        private static final String PARAM_CREDENTIAL = "CREDENTIAL";
        private static final String PARAM_CTEPROGRAM = "CTEPROGRAM";
        private static final String PARAM_IRCONLY = "IRCONLY";
        private static final String PARAM_WBL = "WBL";

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
            CTEStudentProgramEntity courseEntity = (CTEStudentProgramEntity) entity;
            GraduationProgram graduationProgram = (GraduationProgram) courseEntity.getBean();
            String param = (String) field.getParameter();
            Collection<GraduationStudentProgram> gsrList = graduationProgram.getStudentProgramStudies();
            Student student = courseEntity.getCurrentStudent();
            if (gsrList != null && !gsrList.isEmpty()) {
                if (PARAM_COURSEWORK.equals(param)) {
                    value = getGsrByBeanPath(gsrList, m_fieldGsrCrsWork, student.getOid()) != null ? "Y" : "N";
                } else if (PARAM_CREDENTIAL.equals(param)) {
                    value = getGsrByBeanPath(gsrList, m_fieldGsrCredential, student.getOid()) != null ? "Y" : "N";
                } else if (PARAM_CTEPROGRAM.equals(param)) {
                    value = getGsrByBeanPath(gsrList, m_fieldGsrCTEPgm, student.getOid()) != null ? "Y" : "N";
                } else if (PARAM_IRCONLY.equals(param)) {
                    value = getGsrByBeanPath(gsrList, m_fieldGsrIRCOnly, student.getOid()) != null ? "Y" : "N";
                } else if (PARAM_WBL.equals(param)) {
                    value = getGsrByBeanPath(gsrList, m_fieldGsrWBL, student.getOid()) != null ? "Y" : "N";
                }
            }
            return value;
        }

        /**
         * Gets the gsr by bean path.
         *
         * @param gsrList Collection<GraduationStudentProgram>
         * @param beanPath String
         * @param stdOid String
         * @return Graduation student program
         */
        private GraduationStudentProgram getGsrByBeanPath(Collection<GraduationStudentProgram> gsrList,
                                                          String beanPath,
                                                          String stdOid) {
            GraduationStudentProgram gsrFound = gsrList
                    .stream()
                    .filter(gsr -> BooleanAsStringConverter.TRUE.equals(gsr.getFieldValueByBeanPath(beanPath))
                            && gsr.getStudentOid().equals(stdOid))
                    .findFirst()
                    .orElse(null);

            return gsrFound;
        }

    }

    /**
     * The Class RetrieveStudent.
     */
    class RetrieveStudent implements FieldRetriever {

        /*
         * Retriever Parameters
         */
        private static final String PARAM_APPRENTICESHIPCODE = "APPRENTICESHIPCODE";
        private static final String PARAM_CTECHCTR = "CTECHCTR";
        private static final String PARAM_SASID = "SASID";
        private static final String PARAM_SCHCODE = "SCHCODE";

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
            Object value = null;

            CTEStudentProgramEntity courseEntity = (CTEStudentProgramEntity) entity;

            String param = (String) field.getParameter();
            if (PARAM_SASID.equals(param)) {
                value = courseEntity.getSasid();
            } else if (PARAM_APPRENTICESHIPCODE.equals(param)) {
                value = courseEntity.getApprenticeshipCode();
            } else if (PARAM_SCHCODE.equals(param)) {
                value = courseEntity.getSchoolCode();
            } else if (PARAM_CTECHCTR.equals(param)) {
                value = courseEntity.getCareerTechCenterSchoolCode();
            }
            return value;
        }

    }

    /*
     * Retrievers id
     */
    private static final String RETRIVE_ID_STUDENT = "STUDENT";

    /*
     * Aliases
     */
    private static final String ALIAS_GSR_COMPLETED_IRC_ONLY = "all-gsr-CompletedIRCOnly";
    private static final String ALIAS_GSR_COURSEWORK_COMPLETED = "all-gsr-CourseWorkCompleted";
    private static final String ALIAS_GSR_CREDENTIAL_COMPLETED = "all-gsr-CredentialCompleted";
    private static final String ALIAS_GSR_CTE_PROGRAM_COMPLETER = "all-gsr-CTEProgramCompleter";
    private static final String ALIAS_GSR_WBL_COMPLETED = "all-gsr-WBLCompleted";

    protected List<String> m_activeStudentOids = new ArrayList<String>();
    protected PlainDate m_endDate = null;
    protected String m_fieldGsrCredential;
    protected String m_fieldGsrCrsWork;
    protected String m_fieldGsrCTEPgm;
    protected String m_fieldGsrIRCOnly;
    protected String m_fieldGsrWBL;

    protected StudentHistoryHelper m_scheduleHelper;
    protected PlainDate m_startDate = null;

    /**
     * Initialize.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initialize()
     */
    @Override
    protected void initialize() {
        m_startDate = getCurrentContext().getStartDate();
        m_endDate = getCurrentContext().getEndDate();

        m_scheduleHelper = new StudentHistoryHelper(this);
        m_scheduleHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_ACTIVE);

        if (getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) != null
                && getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS) instanceof Boolean) {
            Boolean exclude =
                    Boolean.valueOf(!((Boolean) getParameter(PARAM_INCLUDE_DOE_EXCLUDE_STUDENTS)).booleanValue());
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, exclude);
        }

        super.initialize();
        m_activeStudentOids = getActiveStdOids();
    }

    /**
     * Initialize calcs.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeCalcs()
     */
    @Override
    protected void initializeCalcs() {
        super.initializeCalcs();
        m_calcs.put(RETRIVE_ID_STUDENT, new RetrieveStudent());
        m_calcs.put(RetrieveGSRValues.CALC_ID, new RetrieveGSRValues());
    }

    /**
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeField()
     */
    @Override
    protected void initializeField() {
        super.initializeField();
        m_fieldGsrCredential = translateAliasToJavaName(ALIAS_GSR_CREDENTIAL_COMPLETED, true);
        m_fieldGsrCrsWork = translateAliasToJavaName(ALIAS_GSR_COURSEWORK_COMPLETED, true);
        m_fieldGsrCTEPgm = translateAliasToJavaName(ALIAS_GSR_CTE_PROGRAM_COMPLETER, true);
        m_fieldGsrIRCOnly = translateAliasToJavaName(ALIAS_GSR_COMPLETED_IRC_ONLY, true);
        m_fieldGsrWBL = translateAliasToJavaName(ALIAS_GSR_WBL_COMPLETED, true);
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        m_cteEntityClass = CTEStudentProgramEntity.class;
    }

    /**
     * initialize Maps.
     * // *
     *
     * @param graduationPrgrmCriteria X2Criteria
     */
    @Override
    protected void initializeMaps(X2Criteria graduationPrgrmCriteria) {
        initializeGraduationPrgmStudentsMap(graduationPrgrmCriteria);
        initializeStudentMap();
        initializeGraduationPrgmCourseMap(getGraduationPrgrmCriteria());
        initializeGraduationPrgmSchoolCourseMap(getGraduationPrgrmCriteria());
        initializeSchoolCourseMap();
    }
}
