/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Populates the Student Program Participation table with records for students
 * taking CATE courses.
 * 1. A student may take more than one CATE course.
 * 2. Each CATE course has a program ID.
 * 3. This procedure should create a StudentProgramParticipation for
 * each scheduled class that is a CATE course.
 * 4. This procedure should add/remove/merge based on existing CATE
 * program ID and new/removed CATE program ID.
 *
 * SELECT STUDENT.* FROM STUDENT
 * INNER JOIN STUDENT_SCHEDULE ON SSC_STD_OID = STD_OID
 * INNER JOIN SCHEDULE_MASTER ON MST_OID = SSC_MST_OID
 * INNER JOIN COURSE_SCHOOL ON CSK_OID = MST_CSK_OID
 * INNER JOIN COURSE ON CRS_OID = CSK_CRS_OID
 * WHERE COURSE.'i4see 1740' IS NOT NULL
 *
 * @author X2 Development Corporation
 */
public class I4SeeProgramCreation extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String I4SEE_162_PRIMARY_PROGRAM = "i4see 162";
    private static final String I4SEE_1700_CATE_ENROLLMENT_STATUS = "i4see 1700";
    private static final String I4SEE_1710_PRIMARY_PROGRAM_ID = "i4see 1710"; // CIP
    private static final String I4SEE_1720_PROGRAM_COMPLETER = "i4see 1720";
    private static final String I4SEE_1730_TRAN_MODE = "i4see 1730";
    private static final String I4SEE_1740_PROGRAM_ID = "i4see 1740"; // CIP
    private static final String I4SEE_240_CATE_ENTRY_CODE = "i4see 240 CATE";

    private static final String I4SEE_STATUS_FIELD = "i4see Status";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";

    private static final String I4SEE_CATE_CONTEXT = "i4see CATE CONTEXT";
    private static final String I4SEE_CATE_PROGRAM_CODE = "CATE";
    private static final String I4SEE_CATE_PROGRAM_DESCRIPTION = "CATE Enrollment";

    private static final Map<String, String> m_javaNames = new HashMap();

    /*
     * INPUT PARAMETERS
     */
    private static final String INPUT_START_DATE = "startDate";
    private static final String INPUT_ENTRY_CODE = "entryCode";
    private static final String INPUT_PROGRAM_COMPLETER = "programCompleter";
    private static final String INPUT_PRIMARY_PROGRAM_ID = "primaryProgramId";
    private static final String INPUT_TRAN_MODE = "tranMode";
    private static final String INPUT_SET_CATE_REPORT = "setCateReport";
    private static final String INPUT_CATE_ENTROLLMENT_STATUS = "cateEnrollmentStatus";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";

    private PlainDate m_startDate;
    private String m_entryCode;
    private String m_programCompleter;
    private int m_primaryProgramId;
    private String m_tranMode;
    private boolean m_setCateReport;
    private String m_cateEnrollmentStatus;

    /*
     * Collected data
     */
    private HashSet<String> m_activeScheduleOids;
    private Map<String, Set<String>> m_programIdMap = new HashMap<String, Set<String>>();
    protected Map<String, Schedule> m_scheduleMap;
    protected Map<String, SisSchool> m_schoolMap;
    private Map<String, Collection<StudentProgramParticipation>> m_existingPrograms;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        int count = 0;
        int updated = 0;
        int deleted = 0;

        m_startDate = (PlainDate) getParameter(INPUT_START_DATE);
        m_entryCode = (String) getParameter(INPUT_ENTRY_CODE);
        m_programCompleter = (String) getParameter(INPUT_PROGRAM_COMPLETER);
        m_primaryProgramId = ((Integer) getParameter(INPUT_PRIMARY_PROGRAM_ID)).intValue();
        m_tranMode = (String) getParameter(INPUT_TRAN_MODE);
        m_setCateReport = ((Boolean) getParameter(INPUT_SET_CATE_REPORT)).booleanValue();
        m_cateEnrollmentStatus = (String) getParameter(INPUT_CATE_ENTROLLMENT_STATUS);

        loadJavaNameMap();
        loadSchools();
        loadActiveSchedules();
        loadProgramIds();
        loadCATEStudentProgramParticipation();

        createReferenceCode();

        X2Criteria criteria = new X2Criteria();

        criteria.addIn(SisStudent.REL_STUDENT_SCHEDULES + PATH_DELIMITER +
                StudentSchedule.COL_SCHEDULE_OID, m_activeScheduleOids);
        criteria.addNotNull(SisStudent.REL_STUDENT_SCHEDULES + PATH_DELIMITER +
                StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_javaNames.get(I4SEE_1740_PROGRAM_ID));

        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
        Criteria studentCriteria = new Criteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, subQuery);

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                studentCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                studentCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                studentCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            String currentContextId = getCurrentContext().getContextId();

            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                /*
                 * For a student, find out how many programs were identified in the student
                 * schedule.
                 * Insure the same number of StudentProgramParticipation records are created for
                 * them.
                 * If requested, copy the program Ids into the program partcipation records.
                 */
                Set<String> studentProgramIds = m_programIdMap.get(student.getOid());
                Collection<StudentProgramParticipation> programs = m_existingPrograms.get(student.getOid());
                if (studentProgramIds == null) {
                    studentProgramIds = new HashSet<String>();
                    // m_programIdMap.put(student.getOid(), studentProgramIds);
                }
                Set<String> tempStudentProgramIds = new HashSet<String>();
                tempStudentProgramIds.addAll(studentProgramIds);
                if (programs == null) {
                    programs = new ArrayList<StudentProgramParticipation>();
                    // m_existingPrograms.put(student.getOid(), programs);
                }
                int progIdcount = studentProgramIds.size();
                int programPartCount = programs.size();

                if (progIdcount > programPartCount) {
                    // Make some more program records.
                    for (int newP = 0; newP < (progIdcount - programPartCount); newP++) {
                        count++;
                        StudentProgramParticipation program =
                                new StudentProgramParticipation(getBroker().getPersistenceKey());
                        program.setProgramCode(I4SEE_CATE_PROGRAM_CODE);
                        program.setStudentOid(student.getOid());
                        program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_CATE_CONTEXT), currentContextId);

                        programs.add(program);
                    }
                } else if (progIdcount < programPartCount) {
                    // Remove some program records.
                    // First, look for program IDs not in the schedule program Id collection.
                    while (progIdcount < programPartCount) {
                        boolean removedOne = false;
                        for (StudentProgramParticipation p : programs) {
                            String programProgramId =
                                    (String) p.getFieldValueByBeanPath(m_javaNames.get(I4SEE_1710_PRIMARY_PROGRAM_ID));
                            if (!tempStudentProgramIds.contains(programProgramId)) {
                                // Found a program participation with a program ID that does not
                                // match.
                                // Remove it and remove the program ID from the schedule set so it
                                // won't match again.
                                programs.remove(p);
                                tempStudentProgramIds.remove(programProgramId);
                                removedOne = true;
                                getBroker().deleteBean(p);
                                deleted++;
                                break;
                            }

                            // That program participation ID matched a program Id from the schedule.
                            // keep the program participation and remove the schedule program id so
                            // it won't match again.
                            tempStudentProgramIds.remove(programProgramId);
                        }
                        // None were removed by ID and we still need to remove some. Remove at
                        // random.
                        if (!removedOne) {
                            int left = progIdcount - programPartCount;
                            for (int i = 0; i < left; i++) {
                                Iterator<StudentProgramParticipation> pIterator = programs.iterator();
                                StudentProgramParticipation p = pIterator.next();
                                pIterator.remove();
                                getBroker().deleteBean(p);
                                deleted++;
                            }
                        }
                        programPartCount = programs.size();
                    }
                }

                // set the primary program ID from the student schedule record Program ID
                if (m_primaryProgramId == 1) {
                    tempStudentProgramIds = new HashSet<String>();
                    tempStudentProgramIds.addAll(studentProgramIds);
                    for (StudentProgramParticipation program : programs) {
                        String programProgramId = (String) program
                                .getFieldValueByBeanPath(m_javaNames.get(I4SEE_1710_PRIMARY_PROGRAM_ID));
                        if (tempStudentProgramIds.contains(programProgramId)) {
                            tempStudentProgramIds.remove(programProgramId);
                        } else {
                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1710_PRIMARY_PROGRAM_ID), null);
                        }
                    }
                    for (StudentProgramParticipation program : programs) {
                        String programProgramId = (String) program
                                .getFieldValueByBeanPath(m_javaNames.get(I4SEE_1710_PRIMARY_PROGRAM_ID));
                        if (StringUtils.isEmpty(programProgramId)) {
                            String progId = tempStudentProgramIds.iterator().next();
                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1710_PRIMARY_PROGRAM_ID), progId);
                            tempStudentProgramIds.remove(progId);
                        }
                    }
                }

                // Set all other values as necessary.
                for (StudentProgramParticipation program : programs) {
                    program.setStartDate(m_startDate);

                    /*
                     * Only set/update fields where data has been provided. Blank values leave
                     * fields
                     * unaltered.
                     */
                    if (!StringUtils.isEmpty(m_entryCode)) {
                        program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_240_CATE_ENTRY_CODE), m_entryCode);
                    }
                    if (!StringUtils.isEmpty(m_programCompleter)) {
                        program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1720_PROGRAM_COMPLETER),
                                m_programCompleter);
                    }
                    if (!StringUtils.isEmpty(m_tranMode)) {
                        program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1730_TRAN_MODE), m_tranMode);
                    }
                    // Set all as primary. Multiple programs must be cleaned up by hand.
                    String primary =
                            (String) program.getFieldValueByBeanPath(m_javaNames.get(I4SEE_162_PRIMARY_PROGRAM));
                    if (primary == null) {
                        program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_162_PRIMARY_PROGRAM), "1");
                    }

                    getBroker().saveBeanForced(program);
                }

                /*
                 * Update fields on the student record
                 */

                if (!StringUtils.isEmpty(m_cateEnrollmentStatus)) {
                    WebUtils.setProperty(student, m_javaNames.get(I4SEE_1700_CATE_ENROLLMENT_STATUS),
                            m_cateEnrollmentStatus);
                }
                if (m_setCateReport) {
                    WebUtils.setProperty(student, m_javaNames.get(I4SEE_STATUS_FIELD), I4SEE_STATUS_FIELD_REPORT_CODE);
                }
                getBroker().saveBeanForced(student);

                updated += progIdcount;
            }
        } finally {
            iterator.close();
        }

        logMessage("Created " + count + " Program records.  Updated " + updated + " Program records. Deleted " + deleted
                + " Program records.");
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Load java name map.
     *
     * @return void
     */
    private void loadJavaNameMap() {
        m_javaNames.put(I4SEE_1700_CATE_ENROLLMENT_STATUS, translateAliasToJavaName(I4SEE_1700_CATE_ENROLLMENT_STATUS));
        m_javaNames.put(I4SEE_1710_PRIMARY_PROGRAM_ID, translateAliasToJavaName(I4SEE_1710_PRIMARY_PROGRAM_ID));
        m_javaNames.put(I4SEE_1720_PROGRAM_COMPLETER, translateAliasToJavaName(I4SEE_1720_PROGRAM_COMPLETER));
        m_javaNames.put(I4SEE_1730_TRAN_MODE, translateAliasToJavaName(I4SEE_1730_TRAN_MODE));
        m_javaNames.put(I4SEE_1740_PROGRAM_ID, translateAliasToJavaName(I4SEE_1740_PROGRAM_ID));
        m_javaNames.put(I4SEE_240_CATE_ENTRY_CODE, translateAliasToJavaName(I4SEE_240_CATE_ENTRY_CODE));
        m_javaNames.put(I4SEE_STATUS_FIELD, translateAliasToJavaName(I4SEE_STATUS_FIELD));
        m_javaNames.put(I4SEE_CATE_CONTEXT, translateAliasToJavaName(I4SEE_CATE_CONTEXT));
        m_javaNames.put(I4SEE_162_PRIMARY_PROGRAM, translateAliasToJavaName(I4SEE_162_PRIMARY_PROGRAM));
    }

    /**
     * loads existing Student Program Participation records for this year's CATE program.
     */
    private void loadCATEStudentProgramParticipation() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, I4SEE_CATE_PROGRAM_CODE);
        criteria.addEqualTo(m_javaNames.get(I4SEE_CATE_CONTEXT), getCurrentContext().getContextId());
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        m_existingPrograms = getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                getBroker().getCount(query));
    }

    /**
     * Builds a map of Program Ids for each student. The results are grouped by student, so
     * the program ID will be random if there are multiple program IDs for the student
     *
     * @throws SQLException exception
     */
    private void loadProgramIds() throws SQLException {
        String programIdDatabaseName = translateAliasToDatabaseName(I4SEE_1740_PROGRAM_ID);
        String sql = "SELECT STD_OID, " + programIdDatabaseName + " AS PROGRAMID FROM STUDENT" +
                " INNER JOIN STUDENT_SCHEDULE ON SSC_STD_OID=STD_OID" +
                " INNER JOIN SCHEDULE_MASTER ON MST_OID=SSC_MST_OID" +
                " INNER JOIN COURSE_SCHOOL ON MST_CSK_OID=CSK_OID" +
                " INNER JOIN COURSE ON CSK_CRS_OID=CRS_OID" +
                " INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT ON CRS_CTX_OID=CTX_OID" +
                " WHERE " + programIdDatabaseName + " IS NOT NULL" +
                " AND CTX_CONTEXT_ID='" + getCurrentContext().getContextId() + "'" +
                " GROUP BY STD_OID, " + programIdDatabaseName;

        ResultSet results = null;
        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;

        try {
            statement = connection.prepareStatement(sql);
            results = statement.executeQuery();

            while (results.next()) {
                String studentOid = results.getString("STD_OID");
                String programId = results.getString("PROGRAMID");
                Set<String> programIds = m_programIdMap.get(studentOid);
                if (programIds == null) {
                    programIds = new HashSet<String>();
                    m_programIdMap.put(studentOid, programIds);
                }
                programIds.add(programId);
            }
        } finally {
            if (results != null) {
                try {
                    results.close();
                } catch (SQLException e) {
                    // ignore
                }
            }

            if (statement != null) {
                try {
                    statement.close();
                } catch (SQLException se) {
                    // General statement close.
                }
            }

            getBroker().returnConnection();
        }
    }

    /**
     * Creates a reference code in the Student Program reference table for CATE.
     */
    private void createReferenceCode() {
        /*
         * ensure the CATE code exists for the Student Program reference codes
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_CODE, I4SEE_CATE_PROGRAM_CODE);
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbStdProgram");
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        if (getBroker().getCount(query) == 0) {
            ReferenceCode referenceCode = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
            referenceCode.setCode(I4SEE_CATE_PROGRAM_CODE);
            referenceCode.setDescription(I4SEE_CATE_PROGRAM_DESCRIPTION);
            referenceCode.setReferenceTableOid("rtbStdProgram");
            getBroker().saveBean(referenceCode);
        }
    }

    /**
     * Translate alias to java name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        String javaName = null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }

        return javaName;
    }

    /**
     * Translate alias to database name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToDatabaseName(String alias) {
        String javaName = null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getDatabaseName();
        }

        return javaName;
    }

    /**
     * Loads the active schedule for each school, creating a list of schedule OIDs
     * and a map of schools and schedules.
     */
    private void loadActiveSchedules() {
        m_activeScheduleOids = new HashSet<String>();
        m_scheduleMap = new HashMap();
        Collection<SisSchool> schools = m_schoolMap.values();
        for (SisSchool school : schools) {
            m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
            if (school.getActiveSchedule() != null) {
                m_activeScheduleOids.add(school.getActiveSchedule().getOid());
            }
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

}
