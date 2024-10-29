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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for Program Enrollment export.
 *
 * @author X2 Development Corporation
 */

public class ProgramEnrollment extends StateReportData {
    /**
     * Entity class for Program Enrollment export.
     *
     * @author X2 Development Corporation
     */

    public static class ProgramEnrollmentEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ProgramEnrollmentEntity() {
            // no argument constructor
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = ((StudentProgramParticipation) getBean()).getStudent();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            return name;
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }
    }

    /**
     * Constants for reporting information.
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Local variables for reporting information.
     */
    protected PlainDate m_reportDate;

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        Criteria studentCriteria = getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria programCriteria = getProgramCriteria();
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        applyInputCriteria(programCriteria, true, null);
        if (!isSchoolContext()) {
            programCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            programCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                    + Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);


        applyInputSort(query, null);

        setQuery(query);

        setEntityClass(ProgramEnrollmentEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();

        HashMap validators = new HashMap<String, FieldRetriever>();

        super.addCalcs(calcs);
        super.addValidators(validators);
    }

    /**
     * Returns the criteria that retrieves all programs that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getProgramCriteria() {
        Criteria criteria = new Criteria();
        criteria.addNotNull(StudentProgramParticipation.COL_PROGRAM_CODE);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
        criteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        Criteria endDateCriteria = new Criteria();
        endDateCriteria.addNotNull(StudentProgramParticipation.COL_PROGRAM_CODE);
        endDateCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
        endDateCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
        endDateCriteria.addLessThan(StudentProgramParticipation.COL_START_DATE,
                StudentProgramParticipation.COL_END_DATE);

        criteria.addOrCriteria(endDateCriteria);
        return criteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        List<String> enrollTypes = new ArrayList<String>();
        enrollTypes.add(StudentEnrollment.WITHDRAWAL);
        enrollTypes.add(StudentEnrollment.ENTRY);

        // With Enrollment records within the active date range and of the type E,W.
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        enrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollTypes);
        SubQuery subQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_SCHOOL_OID, enrollmentCriteria);

        applyInputCriteria(enrollmentCriteria, false, StudentEnrollment.REL_STUDENT);
        if (isSchoolContext()) {
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            enrollmentCriteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            enrollmentCriteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        Criteria orCriteria = new Criteria();
        orCriteria.addIn(X2BaseBean.COL_OID, subQuery);

        Criteria criteria = new Criteria();
        criteria.addOrCriteria(studentCriteria);
        criteria.addOrCriteria(orCriteria);

        return criteria;
    }
}
