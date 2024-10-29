/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.va;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.List;
import java.util.TreeMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure will take a list to snapshot of students
 * and roll back their enrollment and grade level to indicate they
 * withdrew at the end of the previous school year.
 * <p>
 * It should be run from the context of a student list.
 * It will process all students in the list, so the list should only contain students that you want
 * to process.
 * <p>
 * <ol>
 * <li>Use context to identify the current student.</li>
 * <li>Determine if the student already has a withdrawal sometime during the summer after the end of
 * school date.</li>
 * <li>Set the YOG and grade level back to the way it was on the end of school date.</li>
 * <li>Set student enrollment status to the last withdrawal record status or to "Inactive".</li>
 * <li>Create or update a withdrawal record to proper exit code and status.</il>
 * </ol>
 *
 * 
 *
 * @author X2 Development Corporation
 */
public class StudentNoShow extends ProcedureJavaSource {
    /**
     * Parameter name for the end of school date for previous year.
     */
    public static final String PARAM_END_OF_SCHOOL = "endOfSchool";
    public static final String PARAM_UPDATE = "update";

    private PlainDate m_endOfSchool;
    private int m_lastSchoolYear;
    private TreeMap<Integer, List<String>> m_gradeLevelMap;
    private boolean m_update;
    private SisStudent m_student;
    private SisUser m_user;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_endOfSchool = (PlainDate) getParameter(PARAM_END_OF_SCHOOL);
        m_update = ((Boolean) getParameter(PARAM_UPDATE)).booleanValue();

        // Lookup last school year. This is the year in the end of school date.
        Calendar calendar = Calendar.getInstance(OrganizationManager.getTimeZone(getOrganization()));
        calendar.setTime(m_endOfSchool);
        m_lastSchoolYear = calendar.get(Calendar.YEAR);

        // Get grade level map for grade level lookup.
        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());

        // Preload student enrollment records for the students.
        if (m_student != null) {
            X2Criteria enrCriteria = new X2Criteria();
            enrCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, m_student.getOid());
            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, enrCriteria);
            query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
            query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
            List<StudentEnrollment> enrollments = (List<StudentEnrollment>) getBroker().getCollectionByQuery(query);

            if (enrollments != null) {
                // rollback and withdraw the student
                withdrawStudent(m_student, enrollments);

                if (m_update) {
                    logMessage("");
                    logMessage(" *** Changes were made but are not yet displayed.");
                    logMessage("     You must click on the Enrollment side tab again to see the changes.");
                } else {
                    logMessage("");
                    logMessage(
                            " *** Changes were not made. Check the update student option to make changes permanent.");
                }

                /*
                 * // Determine if the student changes schools from last spring.
                 * // Find any withdrawal after the end of school.
                 * StudentEnrollment lastEnrollment = null;
                 * StudentEnrollment withdrawal = null;
                 * for (StudentEnrollment enrollment : enrollments)
                 * {
                 * if (m_endOfSchool.before(enrollment.getEnrollmentDate()))
                 * {
                 * if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType()))
                 * {
                 * withdrawal = enrollment;
                 * }
                 * }
                 * else
                 * {
                 * lastEnrollment = enrollment;
                 * break;
                 * }
                 * }
                 * 
                 * // make sure the earliest withdrawal is in the same school as the student
                 * // was in at the end of the year.
                 * if (lastEnrollment != null &&
                 * withdrawal != null &&
                 * !lastEnrollment.getSchoolOid().equals(withdrawal.getSchoolOid()))
                 * {
                 * withdrawal = null;
                 * }
                 * 
                 * // Rollback the student.
                 * if (withdrawal != null)
                 * {
                 * // A withdrawal happened. Rollback the student to that withdrawal record.
                 * rollbackStudent(m_student, enrollments, withdrawal);
                 * }
                 * else
                 * {
                 * // Create a withdrawal at end of school.
                 * withdrawStudent(m_student, enrollments);
                 * }
                 */
            }
        } else {
            logMessage("This procedure can only be run from a student detail or sub list.");
        }
    }

    /**
     * Identify the student to process from the current context.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_student = userData.getCurrentRecord(SisStudent.class);
        m_user = (SisUser) userData.getUser();
    }

    /**
     * A student has not been withdrawn at any time since the end of school.
     * This will rollback the student to the end of school and generate a withdrawal.
     * <p>
     * <ol>
     * <li>Set student status, YOG, grade level back to end of school date.</li>
     * <li>Remove enrollment records after end of school date.</li>
     * <li>Create withdrawal record for end of school date.
     * </ol>
     *
     * @param student SisStudent
     * @param enrollments List<StudentEnrollment>
     */
    private void withdrawStudent(SisStudent student, List<StudentEnrollment> enrollments) {
        // Find last enrollment record before end of school date. Use that to reset student info.
        StringBuilder buffer = new StringBuilder();
        buffer.append(student.getNameView());

        int newYog = student.getYog();

        StudentEnrollment lastEnrollment = null;
        for (StudentEnrollment enrollment : enrollments) {
            if (!m_endOfSchool.before(enrollment.getEnrollmentDate())) {
                lastEnrollment = enrollment;
                break;
            }
        }
        if (lastEnrollment != null) {
            if (lastEnrollment.getSchoolOid() != null
                    && !lastEnrollment.getSchoolOid().equals(student.getSchoolOid())) {
                buffer.append(" : School => ").append(lastEnrollment.getSchool().getName());
                if (m_update) {
                    student.setSchoolOid(lastEnrollment.getSchoolOid());
                }
            }

            if (lastEnrollment.getYog() > 2000) {
                newYog = lastEnrollment.getYog();
                if (student.getYog() != newYog) {
                    buffer.append(" : YOG => ").append(newYog);
                    if (m_update) {
                        student.setYog(newYog);
                    }
                }
            }

            String newGradeLevel = getGradeLevel(newYog, student.getGradeLevel());
            if (!student.getGradeLevel().equals(newGradeLevel)) {
                buffer.append(" : Grade level => ").append(newGradeLevel);
                if (m_update) {
                    student.setGradeLevel(newGradeLevel);
                }
            }
            if (!"Inactive".equals(student.getEnrollmentStatus())) {
                buffer.append(" : Enrollment status => Inactive");
                if (m_update) {
                    student.setEnrollmentStatus("Inactive");
                }
            }

            if (m_update) {
                getBroker().saveBeanForced(student);
            }
            logMessage(buffer.toString());

            // Remove enrollment records after the end of school if any.
            for (StudentEnrollment enrollment : enrollments) {
                if (m_endOfSchool.before(enrollment.getEnrollmentDate())) {
                    buffer = new StringBuilder();
                    buffer.append("   Delete Enrollment ").append(enrollment.getEnrollmentType())
                            .append(" ").append(enrollment.getEnrollmentDate())
                            .append(" ").append(enrollment.getEnrollmentCode());
                    logMessage(buffer.toString());
                    if (m_update) {
                        getBroker().deleteBean(enrollment);
                    }
                } else {
                    break;
                }
            }


            // Create a withdrawal record.
            if (!StudentEnrollment.WITHDRAWAL.equals(lastEnrollment.getEnrollmentType()) ||
                    !m_endOfSchool.equals(lastEnrollment.getEnrollmentDate())) {
                if (m_update) {
                    StudentEnrollment withdrawalEnrollment =
                            X2BaseBean.newInstance(StudentEnrollment.class, getBroker().getPersistenceKey());
                    withdrawalEnrollment.setStudentOid(student.getOid());
                    withdrawalEnrollment.setSchoolOid(lastEnrollment.getSchoolOid());
                    withdrawalEnrollment.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
                    withdrawalEnrollment.setEnrollmentDate(m_endOfSchool);
                    withdrawalEnrollment.setEnrollmentCode("W201");
                    withdrawalEnrollment.setStatusCode("Inactive");
                    withdrawalEnrollment.setYog(newYog);
                    getBroker().saveBeanForced(withdrawalEnrollment);
                }
                buffer = new StringBuilder();
                buffer.append("      Add Enrollment W ")
                        .append(m_endOfSchool)
                        .append(" W201");
                logMessage(buffer.toString());
            } else if ("Active".equals(lastEnrollment.getStatusCode())) {
                lastEnrollment.setStatusCode("Inactive");
                getBroker().saveBeanForced(lastEnrollment);
            }

            if (m_update) {
                dropStudentSchedule(student);
            }
            buffer = new StringBuilder();
            buffer.append("   Drop scheduled classes");
            logMessage(buffer.toString());
        } else {
            buffer.append(student.getNameView()).append(" : No enrollment record found prior to ")
                    .append(m_endOfSchool);
            logMessage(buffer.toString());
        }
    }

    /**
     * A student has been withdrawn at some time since the end of school.
     * This will rollback the student to that withdrawal.
     *
     * @param yog int
     * @param studentGradeLevel String
     * @return String
     */
    /*
     * private void rollbackStudent(SisStudent student, List<StudentEnrollment> enrollments,
     * StudentEnrollment withdrawal)
     * {
     * StringBuilder buffer = new StringBuilder();
     * buffer.append(student.getNameView());
     * 
     * int newYog = student.getYog();
     * 
     * if (withdrawal.getYog() > 2000 && withdrawal.getYog() != student.getYog())
     * {
     * newYog = withdrawal.getYog();
     * buffer.append(" : YOG => ").append(newYog);
     * if (m_update)
     * {
     * student.setYog(newYog);
     * }
     * }
     * if ("Active".equals(withdrawal.getStatusCode()))
     * {
     * buffer.append(" : Enrollment status => Inactive");
     * if (m_update)
     * {
     * student.setEnrollmentStatus("Inactive");
     * }
     * }
     * else if (!withdrawal.getStatusCode().equals(student.getEnrollmentStatus()))
     * {
     * buffer.append(" : Enrollment status => ").append(withdrawal.getStatusCode());
     * if (m_update)
     * {
     * student.setEnrollmentStatus(withdrawal.getStatusCode());
     * }
     * }
     * 
     * String newGradeLevel = getGradeLevel(newYog, student.getGradeLevel());
     * if (!student.getGradeLevel().equals(newGradeLevel))
     * {
     * buffer.append(" : Grade level => ").append(newGradeLevel);
     * if (m_update)
     * {
     * student.setGradeLevel(newGradeLevel);
     * }
     * }
     * 
     * logMessage(buffer.toString());
     * if (m_update)
     * {
     * getBroker().saveBeanForced(student);
     * }
     * 
     * // Remove enrollment records after the last withdrawal record if any.
     * for (StudentEnrollment enrollment : enrollments)
     * {
     * if (m_endOfSchool.before(enrollment.getEnrollmentDate()) &&
     * !withdrawal.getOid().equals(enrollment.getOid()))
     * {
     * buffer = new StringBuilder();
     * buffer.append("   Delete Enrollment ").append(enrollment.getEnrollmentType())
     * .append(" ").append(enrollment.getEnrollmentDate())
     * .append(" ").append(enrollment.getEnrollmentCode());
     * logMessage(buffer.toString());
     * if (m_update)
     * {
     * getBroker().deleteBean(enrollment);
     * }
     * }
     * else
     * {
     * break;
     * }
     * }
     * 
     * // Set up the withdrawal record if necessary.
     * buffer = new StringBuilder();
     * buffer.append("   Update Enrollment W ").append(withdrawal.getEnrollmentDate());
     * boolean dirty = false;
     * if (!withdrawal.getEnrollmentCode().equals("W201"))
     * {
     * buffer.append(" Code => W201");
     * dirty = true;
     * if (m_update)
     * {
     * withdrawal.setEnrollmentCode("W201");
     * }
     * }
     * else
     * {
     * buffer.append(" W201");
     * }
     * 
     * if ("Active".equals(withdrawal.getStatusCode()))
     * {
     * buffer.append(" Status => Inactive");
     * dirty = true;
     * if (m_update)
     * {
     * withdrawal.setStatusCode("Inactive");
     * }
     * }
     * 
     * if (dirty)
     * {
     * logMessage(buffer.toString());
     * if (m_update)
     * {
     * getBroker().saveBeanForced(withdrawal);
     * }
     * }
     * }
     */


    /**
     * Calculate the proper grade level for the student/yog/year.
     *
     * @param yog
     *
     * @return String gradeLevel
     */
    private String getGradeLevel(int yog, String studentGradeLevel) {
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
        List<String> gradeLevels =
                StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, m_lastSchoolYear, m_gradeLevelMap);
        if (gradeLevels != null && gradeLevels.size() > 0) {
            studentGradeLevel = gradeLevels.get(0);
        }
        return studentGradeLevel;
    }

    /**
     * Drop the students schedules as of the student withdrawal action.
     *
     * @param student SisStudent
     */
    private void dropStudentSchedule(SisStudent student) {
        ScheduleManager manager = new ScheduleManager(getBroker());
        manager.dropStudentScheduleRelatedInfo(student, m_user, null);
    }
}
