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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class CTECourses extends CTEStateReportData {

    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class CTECourseEntity extends StateReportEntity {
        /*
         * Aliases
         */
        // COURSE table
        private static final String DOE_COURSE_SEQUENCE = "DOE COURSE SEQUENCE";

        /*
         * Other
         */
        private CTECourses m_CTECourses;
        private List<String> m_filteredSchoolCourses;

        /**
         * Empty no argument constructor for dynamic instantiation.
         */
        public CTECourseEntity() {
            // Empty no argument constructor for dynamic instantiation.
        }

        /**
         * get Course number from schoolCourse.
         *
         * @return String
         */
        public String getCourseId() {
            SchoolCourse schoolCourse = getCurrentSchoolCourse();
            return schoolCourse.getNumber();
        }

        /**
         * get value by alias DOE COURSE SEQUENCE from Course.
         *
         * @return String
         */
        public String getCourseSequence() {
            SchoolCourse schoolCourse = getCurrentSchoolCourse();
            return (String) schoolCourse.getCourse().getFieldValueByAlias(DOE_COURSE_SEQUENCE);
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
            buffer.append(", ");
            buffer.append("Course Number: " + getCourseId());
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
            GraduationProgram graduationProgram = (GraduationProgram) bean;
            m_CTECourses = (CTECourses) data;
            m_filteredSchoolCourses = new ArrayList<String>();

            String gprOid = graduationProgram.getOid();
            if (m_CTECourses.m_graduationPrgmSchoolCourseMap.containsKey(gprOid)) {
                Collection<String> schoolCourseOids =
                        m_CTECourses.m_graduationPrgmSchoolCourseMap.get(gprOid);
                for (String schoolCourseOid : schoolCourseOids) {
                    SchoolCourse currentSchoolCourse = m_CTECourses.m_schoolCourseMap.get(schoolCourseOid);
                    String key = currentSchoolCourse.getNumber();
                    if (currentSchoolCourse.getCourse().getDistrictContext().equals(m_CTECourses.getCurrentContext())
                            && !m_CTECourses.m_exportedCourses.contains(key)) {
                        m_CTECourses.m_exportedCourses.add(key);
                        m_filteredSchoolCourses.add(currentSchoolCourse.getOid());
                    }
                }
            }
            setRowCount(m_filteredSchoolCourses.size());
        }

        /**
         * get current SchoolCourse.
         *
         * @return School course
         */
        private SchoolCourse getCurrentSchoolCourse() {
            String schoolCourseOid = m_filteredSchoolCourses.get(getCurrentRow());
            return m_CTECourses.m_schoolCourseMap.get(schoolCourseOid);
        }
    }

    /**
     * Retrieve by MasterSchedule record.
     *
     * @author Follett Software Company
     */
    class RetrieveSection implements FieldRetriever {

        /*
         * Retriever parameters
         */
        private static final String PARAM_COURSEORDERPROGRSEQ = "COURSEORDERPROGRSEQ";
        private static final String PARAM_LOCALCOURSEID = "LOCALCOURSEID";

        /**
         * Gets the field value.
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
            Object value = null;

            CTECourseEntity courseEntity = (CTECourseEntity) entity;

            String param = (String) field.getParameter();
            if (PARAM_LOCALCOURSEID.equals(param)) {
                value = courseEntity.getCourseId();
            } else if (PARAM_COURSEORDERPROGRSEQ.equals(param)) {
                value = courseEntity.getCourseSequence();
            }

            return value;
        }

    }

    /*
     * Retriever Id
     */
    private static final String RETRIVE_ID_SECTION = "SECTION";

    /**
     * Class members
     */
    protected PlainDate m_endDate = null;
    protected Set<String> m_exportedCourses = new HashSet<>();
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

        super.initialize();
    }

    /**
     * Initialize calcs.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeCalcs()
     */
    @Override
    protected void initializeCalcs() {
        super.initializeCalcs();
        m_calcs.put(RETRIVE_ID_SECTION, new RetrieveSection());
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        m_cteEntityClass = CTECourseEntity.class;

    }

    /**
     * initialize maps.
     *
     * @param graduationPrgrmCriteria X2Criteria
     */
    @Override
    protected void initializeMaps(X2Criteria graduationPrgrmCriteria) {
        initializeGraduationPrgmCourseMap(graduationPrgrmCriteria);
        initializeGraduationPrgmSchoolCourseMap(graduationPrgrmCriteria);
        initializeSchoolCourseMap();
    }

}
