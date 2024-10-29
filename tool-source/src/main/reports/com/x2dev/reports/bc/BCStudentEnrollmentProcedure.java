/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.SchoolCapacity;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.StudentEnrollmentProcedure;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The class customize the student registration for BC. If a student is registered into aspen live
 * school from non-aspen live school, it adds withdraw record and drop the schedules for the student
 *
 * @author X2 Development Corporation
 */
public class BCStudentEnrollmentProcedure implements StudentEnrollmentProcedure {
    public static final String PUBLIC_SCHOOL_INDS = "Public School InDs";
    private X2Broker m_broker;
    private PrivilegeSet m_privilegeSet;

    /**
     * Initialize.
     *
     * @param broker X2Broker
     * @param privilegeSet PrivilegeSet
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#initialize(com.follett.fsc.core.k12.
     *      business.X2Broker, com.follett.fsc.core.k12.business.PrivilegeSet)
     */
    @Override
    public void initialize(X2Broker broker, PrivilegeSet privilegeSet) {
        m_broker = broker;
        m_privilegeSet = privilegeSet;
    }

    /**
     * After change enrollment status.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterChangeEnrollmentStatus(com.x2dev
     *      .sis.model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterChangeEnrollmentStatus(SisStudent student, StudentEnrollment enrollment) {
        return null;
    }

    /**
     * After change year of graduation.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterChangeYearOfGraduation(com.x2dev
     *      .sis.model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterChangeYearOfGraduation(SisStudent student, StudentEnrollment enrollment) {
        return null;
    }

    /**
     * After registrater student.
     *
     * @param student SisStudent
     * @param entryEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterRegistraterStudent(com.x2dev.sis
     *      .model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterRegistraterStudent(SisStudent student, StudentEnrollment entryEnrollment) {
        Criteria studentEnrollmentCriteria = new Criteria();
        studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        // Exclude the enrollment record just created to only include previous enrollment record
        studentEnrollmentCriteria.addNotEqualTo(X2BaseBean.COL_OID, entryEnrollment.getOid());

        QueryByCriteria stdEnrollmentQuery = new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
        stdEnrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        stdEnrollmentQuery.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);

        Collection<StudentEnrollment> studentEnrollmentList = m_broker.getCollectionByQuery(stdEnrollmentQuery);

        StudentEnrollment mostRecentEnrollment = null;
        /*
         * To determine if it needs to perform inserting W record and deleting schedule records for
         * the student, check if the most recent enrollment record is not a Withdraw
         */
        if (!studentEnrollmentList.isEmpty()) {
            Iterator<StudentEnrollment> studentEnrollmentIterator = studentEnrollmentList.iterator();

            mostRecentEnrollment = studentEnrollmentIterator.next();

            if (mostRecentEnrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                /*
                 * this student is not from the non-apsen live school
                 */
                mostRecentEnrollment = null;
            }
        }

        Criteria secondarySchoolCriteria = new Criteria();
        secondarySchoolCriteria.addEqualTo(StudentSchool.COL_STUDENT_OID, student.getOid());
        secondarySchoolCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                student.getSchool().getCurrentContext().getOid());
        secondarySchoolCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

        SubQuery secondarySchoolQuery =
                new SubQuery(StudentSchool.class, StudentSchool.COL_SCHOOL_OID, secondarySchoolCriteria);

        Criteria studentScheduleCriteria = new Criteria();
        studentScheduleCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, student.getOid());
        studentScheduleCriteria.addEqualTo(
                StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_DISTRICT_CONTEXT
                        + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                student.getSchool().getCurrentContext().getOid());
        studentScheduleCriteria.addNotEqualTo(
                StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                student.getSchoolOid());
        studentScheduleCriteria.addNotIn(
                StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_SCHOOL_OID,
                secondarySchoolQuery);

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);
        Collection studentSchedulesToDrop = m_broker.getCollectionByQuery(query);

        ScheduleManager manager = new ScheduleManager(m_broker);
        manager.dropStudentSchedules(MasterSchedule.class, studentSchedulesToDrop, m_privilegeSet.getUser(),
                new ArrayList(0), null);

        StudentEnrollment withdrawal = new StudentEnrollment(m_broker.getPersistenceKey());
        withdrawal.setStudentOid(student.getOid());

        if (mostRecentEnrollment != null) {
            withdrawal.setSchoolOid(mostRecentEnrollment.getSchoolOid());
            withdrawal.setTimestamp(mostRecentEnrollment.getTimestamp() + 1);
            withdrawal.setYog(mostRecentEnrollment.getYog());
            withdrawal.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
            withdrawal.setEnrollmentCode(PUBLIC_SCHOOL_INDS);
            withdrawal.setReasonCode("");
            withdrawal.setEnrollmentDate(DateUtils.add(new PlainDate(), -1));
            m_broker.saveBeanForced(withdrawal);
        }

        if (mostRecentEnrollment != null) {
            StudentSchool schoolAssociation = X2BaseBean.newInstance(StudentSchool.class, m_broker.getPersistenceKey());
            schoolAssociation.setDistrictContextOid(student.getSchool().getCurrentContext().getOid());
            schoolAssociation.setEndDate(DateUtils.add(new PlainDate(), -1));
            schoolAssociation.setSchoolOid(mostRecentEnrollment.getSchoolOid());
            schoolAssociation.setStartDate(mostRecentEnrollment.getEnrollmentDate());
            schoolAssociation.setStudentOid(student.getOid());
            schoolAssociation.setType(StudentSchool.FORMER);

            m_broker.saveBeanForced(schoolAssociation);
        }

        return null;
    }

    /**
     * After transfer student.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @param entryEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterTransferStudent(com.x2dev.sis.
     *      model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterTransferStudent(SisStudent student,
                                                      StudentEnrollment withdrawEnrollment,
                                                      StudentEnrollment entryEnrollment) {
        return null;
    }

    /**
     * After withdraw student.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterWithdrawStudent(com.x2dev.sis.
     *      model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterWithdrawStudent(SisStudent student, StudentEnrollment withdrawEnrollment) {
        return null;
    }

    /**
     * Validate change enrollment status.
     *
     * @param student SisStudent
     * @param date PlainDate
     * @param reason String
     * @param status String
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateChangeEnrollmentStatus(com.
     *      x2dev.sis.model.beans.SisStudent, com.x2dev.utils.types.PlainDate, java.lang.String,
     *      java.lang.String, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateChangeEnrollmentStatus(SisStudent student,
                                                                PlainDate date,
                                                                String reason,
                                                                String status,
                                                                StudentEnrollment studentEnrollment) {
        return null;
    }

    /**
     * Validate change year of graduation.
     *
     * @param student SisStudent
     * @param date PlainDate
     * @param reason String
     * @param yog int
     * @param gradeLevel String
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateChangeYearOfGraduation(com.
     *      x2dev.sis.model.beans.SisStudent, com.x2dev.utils.types.PlainDate, java.lang.String,
     *      int,
     *      java.lang.String, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateChangeYearOfGraduation(SisStudent student,
                                                                PlainDate date,
                                                                String reason,
                                                                int yog,
                                                                String gradeLevel,
                                                                StudentEnrollment studentEnrollment) {
        return null;
    }

    /**
     * Validate register student.
     *
     * @param student SisStudent
     * @param yog int
     * @param status String
     * @param school SisSchool
     * @param schoolCapacity SchoolCapacity
     * @param enrollmentPropertyValues Map<ModelProperty,Object>
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateRegisterStudent(com.x2dev.sis
     *      .model.beans.SisStudent, int, java.lang.String, com.x2dev.sis.model.beans.SisSchool,
     *      com.follett.fsc.core.k12.beans.SchoolCapacity, java.util.Map)
     */
    @Override
    public List<ValidationError> validateRegisterStudent(SisStudent student,
                                                         int yog,
                                                         String status,
                                                         SisSchool school,
                                                         SchoolCapacity schoolCapacity,
                                                         Map<ModelProperty, Object> enrollmentPropertyValues) {
        return null;
    }

    /**
     * Validate transfer student.
     *
     * @param student SisStudent
     * @param user SisUser
     * @param withdrawalDate PlainDate
     * @param withdrawalCode String
     * @param withdrawalReason String
     * @param entryDate PlainDate
     * @param entryCode String
     * @param entryReason String
     * @param destinationSchool SisSchool
     * @param destinationSchoolCapacity SchoolCapacity
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateTransferStudent(com.x2dev.sis
     *      .model.beans.SisStudent, com.x2dev.sis.model.beans.SisUser,
     *      com.x2dev.utils.types.PlainDate,
     *      java.lang.String, java.lang.String, com.x2dev.utils.types.PlainDate, java.lang.String,
     *      java.lang.String, com.x2dev.sis.model.beans.SisSchool,
     *      com.follett.fsc.core.k12.beans.SchoolCapacity,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateTransferStudent(SisStudent student,
                                                         SisUser user,
                                                         PlainDate withdrawalDate,
                                                         String withdrawalCode,
                                                         String withdrawalReason,
                                                         PlainDate entryDate,
                                                         String entryCode,
                                                         String entryReason,
                                                         SisSchool destinationSchool,
                                                         SchoolCapacity destinationSchoolCapacity,
                                                         StudentEnrollment studentEnrollment) {
        return null;
    }

    /**
     * Validate withdraw student.
     *
     * @param student SisStudent
     * @param user User
     * @param date PlainDate
     * @param code String
     * @param reason String
     * @param status String
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateWithdrawStudent(com.x2dev.sis
     *      .model.beans.SisStudent, com.follett.fsc.core.k12.beans.User,
     *      com.x2dev.utils.types.PlainDate, java.lang.String, java.lang.String, java.lang.String,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateWithdrawStudent(SisStudent student,
                                                         User user,
                                                         PlainDate date,
                                                         String code,
                                                         String reason,
                                                         String status,
                                                         StudentEnrollment studentEnrollment) {
        return null;
    }
}
