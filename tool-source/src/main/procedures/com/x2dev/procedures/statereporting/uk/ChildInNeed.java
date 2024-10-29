/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.StateReportModel;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.StudentProgramDetail;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import org.jdom.Element;

/**
 * State report for UK's Child In Need Census.
 *
 * @author Follett Software Company
 */
public class ChildInNeed extends XMLStateReportData {
    public static final String PARAM_CURRENT_BEAN = "currentBean";

    private static final String MODEL_NAME = "CBDS";
    private static final String MODEL_PROCEDURE_LOAD_DATES = "loadDates";
    private static final String MODEL_PARAM_CENSUS_DATE = "censusDate";
    private static final String MODEL_PARAM_PROGRAM_START_DATE = "programStartDate";
    private static final String MODEL_PARAM_PROGRAM_END_DATE = "programEndDate";
    private static final String MODEL_RETRIEVER_SYSTEM = "SYSTEM";

    private static final String ELEMENT_REFERENCE_DATE = "ReferenceDate";

    private static final String QUERY_ID_ORGANIZATION = "organization";
    private static final String QUERY_ID_STUDENTS = "students";
    private static final String QUERY_ID_STUDENT_DISABILITIES = "studentDisabilities";
    private static final String QUERY_ID_STUDENT_PROGRAM_PARTICIPATIONS = "studentProgramParticipations";
    private static final String QUERY_ID_STUDENT_PROGRAM_INITIAL_ASSESSMENTS = "studentProgramInitialAssessements";
    private static final String QUERY_ID_STUDENT_PROGRAM_CORE_ASSESSMENTS = "studentProgramCoreAssessements";
    private static final String QUERY_ID_STUDENT_PROGRAM_SECTION_47S = "studentProgramSection47s";
    private static final String QUERY_ID_STUDENT_PROGRAM_CHILD_PROTECTION_PLANS = "studentProgramChildProtectionPlans";

    private static final String PROGRAM_CODE_CHILD_IN_NEED = "Child in Need";

    private static final String PROGRAM_DETAIL_TYPE_IA = "IA";
    private static final String PROGRAM_DETAIL_TYPE_CA = "CA";
    private static final String PROGRAM_DETAIL_TYPE_S47 = "S47";
    private static final String PROGRAM_DETAIL_TYPE_CPP = "CPP";

    /**
     * The UK model
     */
    private StateReportModel m_model;
    private PlainDate m_censusDate;
    private PlainDate m_programStartDate;
    private PlainDate m_programEndDate;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_model = loadModel(MODEL_NAME);
        m_model.execute(MODEL_PROCEDURE_LOAD_DATES);
        m_censusDate = (PlainDate) m_model.get(MODEL_PARAM_CENSUS_DATE);
        m_programStartDate = (PlainDate) m_model.get(MODEL_PARAM_PROGRAM_START_DATE);
        m_programEndDate = (PlainDate) m_model.get(MODEL_PARAM_PROGRAM_END_DATE);

        setElement(ELEMENT_REFERENCE_DATE, new Element(ELEMENT_REFERENCE_DATE).setText(m_censusDate.toString()));

        X2Criteria studentProgramCriteria = getStudentProgramCriteria();
        SubQuery sfpStdProgQuery = getStudentProgramSubQuery(studentProgramCriteria);
        SubQuery sfpStdSubQuery = getStudentSubQuery(studentProgramCriteria);

        loadOrganization();

        loadStudents(sfpStdSubQuery);

        loadStudentDisabilities(sfpStdSubQuery);

        loadStudentProgramParticipation(sfpStdProgQuery);

        loadStudentProgramDetails(sfpStdProgQuery, PROGRAM_DETAIL_TYPE_IA,
                QUERY_ID_STUDENT_PROGRAM_INITIAL_ASSESSMENTS);
        loadStudentProgramDetails(sfpStdProgQuery, PROGRAM_DETAIL_TYPE_CA, QUERY_ID_STUDENT_PROGRAM_CORE_ASSESSMENTS);
        loadStudentProgramDetails(sfpStdProgQuery, PROGRAM_DETAIL_TYPE_S47, QUERY_ID_STUDENT_PROGRAM_SECTION_47S);
        loadStudentProgramDetails(sfpStdProgQuery, PROGRAM_DETAIL_TYPE_CPP,
                QUERY_ID_STUDENT_PROGRAM_CHILD_PROTECTION_PLANS);

        // Load Retrievers
        if (getSetupErrors().isEmpty()) {
            // Organization level calculated fields
            m_model.initializeRetriever(MODEL_RETRIEVER_SYSTEM);

            super.addCalcs(m_model.getRetrievers());
        }
    }

    /**
     * Sets up the X2Criteria for Students Programs.
     *
     * @return sfpStdSubQuery
     */
    private X2Criteria getStudentProgramCriteria() {
        X2Criteria studentProgramParticipationCriteria1 = new X2Criteria();
        studentProgramParticipationCriteria1.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE,
                PROGRAM_CODE_CHILD_IN_NEED);

        X2Criteria studentProgramParticipationCriteria2 = new X2Criteria();

        // Include the Program if its Start Date is between the selected Start and End Dates
        X2Criteria studentProgramParticipationCriteria3 = new X2Criteria();
        studentProgramParticipationCriteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                m_programStartDate);
        studentProgramParticipationCriteria3.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                m_programEndDate);
        studentProgramParticipationCriteria2.addOrCriteria(studentProgramParticipationCriteria3);

        // Include the Program if its End Date is between the selected Start and End Dates
        X2Criteria studentProgramParticipationCriteria4 = new X2Criteria();
        studentProgramParticipationCriteria4.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                m_programStartDate);
        studentProgramParticipationCriteria4.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                m_programEndDate);
        studentProgramParticipationCriteria2.addOrCriteria(studentProgramParticipationCriteria4);

        // Include the Program if its Start Date is between before the selected Start Date and and
        // its End Date is null
        X2Criteria studentProgramParticipationCriteria5 = new X2Criteria();
        studentProgramParticipationCriteria5.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                m_programStartDate);
        studentProgramParticipationCriteria5.addIsNull(StudentProgramParticipation.COL_END_DATE);
        studentProgramParticipationCriteria2.addOrCriteria(studentProgramParticipationCriteria5);

        studentProgramParticipationCriteria1.addAndCriteria(studentProgramParticipationCriteria2);

        return studentProgramParticipationCriteria1;
    }

    /**
     * Sets up the SubQuery for Students Programs.
     *
     * @param studentProgramParticipationCriteria X2Criteria
     * @return sfpStdSubQuery
     */
    private SubQuery getStudentProgramSubQuery(X2Criteria studentProgramParticipationCriteria) {
        SubQuery sfpStdProgSubQuery = new SubQuery(StudentProgramParticipation.class, X2BaseBean.COL_OID,
                studentProgramParticipationCriteria);

        return sfpStdProgSubQuery;
    }

    /**
     * Sets up the SubQuery for Students.
     *
     * @param studentProgramParticipationCriteria X2Criteria
     * @return sfpStdSubQuery
     */
    private SubQuery getStudentSubQuery(X2Criteria studentProgramParticipationCriteria) {
        SubQuery sfpStdSubQuery = new SubQuery(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_STUDENT_OID, studentProgramParticipationCriteria);

        return sfpStdSubQuery;
    }

    /**
     * load the Organization.
     */
    private void loadOrganization() {
        setBeanToQuery(QUERY_ID_ORGANIZATION, getOrganization());
    }

    /**
     * load the Students.
     *
     * @param sfpStdSubQuery SubQuery
     */
    private void loadStudents(SubQuery sfpStdSubQuery) {
        X2Criteria studentCriteria = new X2Criteria();

        // We are include all students for the entire LA
        studentCriteria.addIn(X2BaseBean.COL_OID, sfpStdSubQuery);

        BeanQuery studentQuery = new BeanQuery(Student.class, studentCriteria);
        Collection<Student> students = getBroker().getCollectionByQuery(studentQuery);

        setCollectionToQuery(QUERY_ID_STUDENTS, students);
    }

    /**
     * load the Students Disabilities.
     *
     * @param sfpStdSubQuery SubQuery
     */
    private void loadStudentDisabilities(SubQuery sfpStdSubQuery) {
        X2Criteria studentDisabilityCriteria = new X2Criteria();
        studentDisabilityCriteria.addIn(IepDisability.COL_STUDENT_OID, sfpStdSubQuery);

        BeanQuery studentDisabilityQuery = new BeanQuery(IepDisability.class, studentDisabilityCriteria);
        HashMap<String, Collection<IepDisability>> studentDisabilities =
                (HashMap<String, Collection<IepDisability>>) getBroker()
                        .getGroupedCollectionByQuery(studentDisabilityQuery, IepDisability.COL_STUDENT_OID, 64);

        setGroupedCollectionToQuery(QUERY_ID_STUDENT_DISABILITIES, studentDisabilities);
    }

    /**
     * load the Student Program Participation.
     *
     * @param sfpStdProgQuery SubQuery
     */
    private void loadStudentProgramParticipation(SubQuery sfpStdProgQuery) {
        X2Criteria studentProgramParticipationCriteria = new X2Criteria();
        studentProgramParticipationCriteria.addIn(X2BaseBean.COL_OID, sfpStdProgQuery);

        BeanQuery studentProgramParticipationQuery =
                new BeanQuery(StudentProgramParticipation.class, studentProgramParticipationCriteria);
        HashMap<String, Collection<StudentProgramParticipation>> studentProgramParticipations =
                (HashMap<String, Collection<StudentProgramParticipation>>) getBroker().getGroupedCollectionByQuery(
                        studentProgramParticipationQuery, StudentProgramParticipation.COL_STUDENT_OID, 64);

        setGroupedCollectionToQuery(QUERY_ID_STUDENT_PROGRAM_PARTICIPATIONS, studentProgramParticipations);
    }

    /**
     * load the Student Program Details.
     *
     * @param sfpStdProgQuery SubQuery
     * @param programDetailType String
     * @param queryId String
     */
    private void loadStudentProgramDetails(SubQuery sfpStdProgQuery, String programDetailType, String queryId) {
        X2Criteria studentProgramDetailCriteria = new X2Criteria();
        studentProgramDetailCriteria.addIn(StudentProgramDetail.COL_PROGRAM_OID, sfpStdProgQuery);
        studentProgramDetailCriteria.addEqualTo(StudentProgramDetail.COL_TYPE, programDetailType);

        BeanQuery studentProgramDetailQuery = new BeanQuery(StudentProgramDetail.class, studentProgramDetailCriteria);
        HashMap<String, Collection<StudentProgramDetail>> studentProgramDetails =
                (HashMap<String, Collection<StudentProgramDetail>>) getBroker().getGroupedCollectionByQuery(
                        studentProgramDetailQuery, StudentProgramDetail.COL_PROGRAM_OID, 64);

        setGroupedCollectionToQuery(queryId, studentProgramDetails);
    }

}
