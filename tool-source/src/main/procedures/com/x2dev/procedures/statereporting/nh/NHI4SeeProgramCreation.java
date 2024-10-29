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
package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
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
public class NHI4SeeProgramCreation extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String I4SEE_030_DISTRICT_CODE = "i4see 030";
    private static final String I4SEE_225_CODE = "i4see 225";
    private static final String I4SEE_220_CODE = "i4see 220";
    private static final String ALIAS_PRIMARY_PROGRAM_FLAG = "all-pgm-PrimaryProgramIndicator";
    private static final String I4SEE_1700_CATE_ENROLLMENT_STATUS = "i4see 1700";
    private static final String I4SEE_1710_PRIMARY_PROGRAM_ID = "i4see 1710"; // CIP
    private static final String I4SEE_1720_PROGRAM_COMPLETER = "i4see 1720";
    private static final String I4SEE_1730_TRAN_MODE = "i4see 1730";
    private static final String I4SEE_1740_PROGRAM_ID = "i4see 1740"; // CIP
    private static final String I4SEE_240_CATE_ENTRY_CODE = "i4see 240 CATE";
    private static final String I4SEE_260_CATE_EXIT_CODE = "i4see 260 CATE";
    private static final String I4SEE_AFTER_MOY = "i4see AFTER MOY";
    private static final String I4SEE_CATE_CONTEXT = "i4see CATE CONTEXT";
    private static final String I4SEE_CATE_PROGRAM_CODE = "CATE";
    private static final String I4SEE_CATE_PROGRAM_DESCRIPTION = "CATE Enrollment";
    private static final String I4SEE_STATUS_FIELD = "i4see Status";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";

    /*
     * INPUT PARAMETERS
     */
    private static final String INPUT_CATE_ENTROLLMENT_STATUS = "cateEnrollmentStatus";
    private static final String INPUT_ENTRY_CODE = "entryCode";
    private static final String INPUT_REPORT_DATE = "reportDate";
    private static final String INPUT_PROGRAM_COMPLETER = "programCompleter";
    private static final String INPUT_SET_CATE_REPORT = "setCateReport";
    private static final String INPUT_TRAN_MODE = "tranMode";
    private static final String INPUT_UPDATE_BEGIN = "updateBeginDate";
    private static final String INPUT_UPDATE_END = "updateEndDate";
    private static final String INPUT_UPDATE_EXIT = "updateExitCode";
    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";

    private boolean m_beginDateOverride;
    private String m_cateEnrollmentStatus;
    private HelperReportData m_data;
    private boolean m_endDateOverride;
    private String m_entryCode;
    private boolean m_exitCodeOverride;
    private static final Map<String, String> m_javaNames = new HashMap();
    private String m_programCompleter;
    private PlainDate m_reportDate;
    private boolean m_setCateReport;
    private String m_tranMode;

    /*
     * Collected data
     */
    private HashSet<String> m_activeScheduleOids;
    private Map<String, Collection<StudentProgramParticipation>> m_existingPrograms;
    private Map<String, Set<String>> m_programIdMap = new HashMap<String, Set<String>>();
    protected Map<String, Schedule> m_scheduleMap;
    protected Map<String, SisSchool> m_schoolMap;
    private Collection AFTER_MOY_EXCLUDE = Arrays.asList("S2");

    /**
     * The Class HelperReportData.
     */
    class HelperReportData extends StateReportData {
        /*
         * Constants: Parameters, Constants
         */
        protected static final String PARAM_END_DATE = "endDate";

        /*
         * Instance variables.
         */
        protected PlainDate m_endDate;
        protected StudentHistoryHelper m_helper;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

            String fieldProgramId = translateAliasToJavaName("i4see 1740", true);

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_ACTIVE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);

            X2Criteria scheduleCriteria = m_helper.getStudentScheduleCriteria();
            scheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    fieldProgramId, getBroker().getPersistenceKey());

            X2Criteria scheduleChangeCriteria = m_helper.getStudentScheduleChangeCriteria();
            scheduleChangeCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    fieldProgramId, getBroker().getPersistenceKey());
        }
    }

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
        boolean excludeFlag;

        m_reportDate = (PlainDate) getParameter(INPUT_REPORT_DATE);
        m_beginDateOverride = ((Boolean) getParameter(INPUT_UPDATE_BEGIN)).booleanValue();
        m_endDateOverride = ((Boolean) getParameter(INPUT_UPDATE_END)).booleanValue();
        m_entryCode = (String) getParameter(INPUT_ENTRY_CODE);
        m_exitCodeOverride = ((Boolean) getParameter(INPUT_UPDATE_EXIT)).booleanValue();
        m_programCompleter = (String) getParameter(INPUT_PROGRAM_COMPLETER);
        m_tranMode = (String) getParameter(INPUT_TRAN_MODE);
        m_cateEnrollmentStatus = (String) getParameter(INPUT_CATE_ENTROLLMENT_STATUS);
        m_setCateReport = ((Boolean) getParameter(INPUT_SET_CATE_REPORT)).booleanValue();

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

        m_data = new HelperReportData();
        m_data.setBroker(getBroker());
        m_data.setCurrentContext(getCurrentContext());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(isSchoolContext());
        m_data.setSchool(getSchool());
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, m_data.m_helper.getStudentCriteria());
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        String organizationDistrictResponsible =
                getOrganization().getFieldValueByAlias(I4SEE_030_DISTRICT_CODE).toString();
        boolean databaseDistrictReponsibleCodePresent = !organizationDistrictResponsible.isEmpty();
        String districtResponsible = null;
        String townResponsible = null;

        ArrayList<String> studentNames = new ArrayList<String>();

        try {
            // String currentContextId = getOrganization().getCurrentContext().getContextId();

            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                List<StudentScheduleSpan> spans = m_data.m_helper.getStudentScheduleSpans(student);
                Collections.sort(spans, new Comparator<StudentScheduleSpan>() {
                    @Override
                    public int compare(StudentScheduleSpan span1, StudentScheduleSpan span2) {
                        return span1.getEntryDate().compareTo(span2.getEntryDate());
                    }
                });

                /*
                 * For a student, find out how many programs were identified in the student
                 * schedule.
                 * Insure the same number of StudentProgramParticipation records are created for
                 * them.
                 * If requested, copy the program Ids into the program partcipation records.
                 */
                Set<String> studentProgramIds = m_programIdMap.get(student.getOid());
                studentProgramIds = (studentProgramIds == null) ? new HashSet<String>() : studentProgramIds;
                Set<String> tempStudentProgramIds = new HashSet<String>();
                Iterator<StudentScheduleSpan> spanIterator = spans.iterator();
                String termView = "";
                MasterSchedule section;
                Course course;
                excludeFlag = true;
                boolean enrolled = false;
                boolean isEmptyPgms = false;

                tempStudentProgramIds.addAll(studentProgramIds);
                Collection<StudentProgramParticipation> programs = m_existingPrograms.get(student.getOid());

                if (programs == null) {
                    programs = new ArrayList<StudentProgramParticipation>();
                    isEmptyPgms = true;
                    // m_existingPrograms.put(student.getOid(), programs);
                }
                int studentProgramParticipationCount = programs.size();
                int singlePrimary = 0;
                for (StudentProgramParticipation program : programs) {
                    Object primaryProgram =
                            program.getFieldValueByBeanPath(m_javaNames.get(ALIAS_PRIMARY_PROGRAM_FLAG));
                    if (primaryProgram != null && BooleanAsStringConverter.TRUE.equals(primaryProgram.toString())) {
                        singlePrimary++;
                    }
                }
                Collection<String> operatedPgmIds = new ArrayList<String>();
                while (spanIterator.hasNext()) {
                    boolean programUpdated = false;
                    StudentScheduleSpan currentSpan = spanIterator.next();
                    if (!currentSpan.getEntryDate().equals(currentSpan.getExitDate()) ||
                            getCurrentContext().getStartDate().equals(currentSpan.getExitDate())) {
                        section = currentSpan.getSection();
                        termView = section.getTermView();
                        if (!AFTER_MOY_EXCLUDE.contains(termView)) {
                            excludeFlag = false;
                        }
                        course = section.getSchoolCourse().getCourse();

                        if (!enrolled) {
                            enrolled = currentSpan.getExitDate() == null;
                        }

                        // Set all other values as necessary.
                        // boolean override = false;

                        StudentEnrollment entry =
                                m_data.m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, "E");
                        if (databaseDistrictReponsibleCodePresent) {
                            Object drValue = entry.getFieldValueByAlias(I4SEE_225_CODE);
                            districtResponsible = (drValue != null) ? drValue.toString() : null;
                            Object trValue = entry.getFieldValueByAlias(I4SEE_220_CODE);
                            townResponsible = (drValue != null) ? trValue.toString() : null;
                        }

                        boolean outsideAreaStudent = false;
                        // If the values are not empty or they do not match the inputs then they are
                        // not local
                        if (!(districtResponsible == null || !districtResponsible.isEmpty() || townResponsible.isEmpty()
                                || townResponsible == null ||
                                districtResponsible.equals(organizationDistrictResponsible) ||
                                (districtResponsible.equals("335") &&
                                        (organizationDistrictResponsible.equals("079") ||
                                                organizationDistrictResponsible.equals("029") ||
                                                organizationDistrictResponsible.equals("261"))))) {
                            outsideAreaStudent = true;
                        }
                        String courseProgramId = (String) course.getFieldValueByAlias(I4SEE_1740_PROGRAM_ID);
                        if (isEmptyPgms && !StringUtils.isEmpty(courseProgramId)
                                && !operatedPgmIds.contains(courseProgramId)) {
                            operatedPgmIds.add(courseProgramId);
                            programs = new ArrayList<>();
                            StudentProgramParticipation program =
                                    new StudentProgramParticipation(getBroker().getPersistenceKey());
                            program.setProgramCode(I4SEE_CATE_PROGRAM_CODE);
                            program.setStudentOid(student.getOid());
                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_CATE_CONTEXT),
                                    getCurrentContext().getContextId());
                            programs.add(program);
                            count++;
                        }
                        for (StudentProgramParticipation program : programs) {
                            programUpdated = false;
                            String programProgramId = (String) program
                                    .getFieldValueByBeanPath(m_javaNames.get(I4SEE_1710_PRIMARY_PROGRAM_ID));
                            if (isEmptyPgms || (programProgramId != null && programProgramId.equals(courseProgramId))) {
                                program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1710_PRIMARY_PROGRAM_ID),
                                        courseProgramId);
                                if ((program.getStartDate() == null
                                        || program.getStartDate().after(currentSpan.getEntryDate()))
                                        || (m_beginDateOverride
                                                && program.getStartDate().after(currentSpan.getEntryDate()))) {
                                    program.setStartDate(currentSpan.getEntryDate());
                                    programUpdated = true;
                                }

                                PlainDate dateToSet = null;
                                Collection<ScheduleTermDate> schDates =
                                        section.getScheduleTerm().getScheduleTermDates();
                                for (ScheduleTermDate schDate : schDates) {
                                    if (dateToSet == null || dateToSet.before(schDate.getEndDate())) {
                                        dateToSet = schDate.getEndDate();
                                    }
                                }
                                if (dateToSet != null
                                        && (program.getEndDate() == null || program.getEndDate().before(dateToSet))) {
                                    program.setEndDate(dateToSet);
                                }
                                if (m_endDateOverride) {
                                    if (program.getEndDate() == null || (currentSpan.getExitDate() != null
                                            && program.getEndDate().before(currentSpan.getExitDate()))) {
                                        program.setEndDate(currentSpan.getExitDate());
                                    }
                                    programUpdated = true;
                                }

                                if (excludeFlag && program.getFieldValueByAlias(I4SEE_AFTER_MOY) == null) {
                                    program.setFieldValueByAlias(I4SEE_AFTER_MOY, "1");
                                    programUpdated = true;
                                }

                                /*
                                 * Only set/update fields where data has been provided. If the
                                 * record is empty we will use logic unless the user specifically
                                 * wants the logic to be implemented
                                 */
                                if (!StringUtils.isEmpty(m_entryCode) && !"-1".equals(m_entryCode)) {
                                    program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_240_CATE_ENTRY_CODE),
                                            m_entryCode);
                                    programUpdated = true;
                                } else if (databaseDistrictReponsibleCodePresent) {
                                    if (program
                                            .getFieldValueByBeanPath(m_javaNames.get(I4SEE_240_CATE_ENTRY_CODE)) == null
                                            || "-1".equals(m_entryCode)) {
                                        if (outsideAreaStudent) {
                                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_240_CATE_ENTRY_CODE),
                                                    "V1");
                                        } else {
                                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_240_CATE_ENTRY_CODE),
                                                    "V3");
                                        }
                                        programUpdated = true;
                                    }
                                }

                                if (program
                                        .getFieldValueByBeanPath(m_javaNames.get(I4SEE_260_CATE_EXIT_CODE)) == null) {
                                    StudentScheduleChange exitChange = currentSpan.getExitChange();
                                    if (exitChange != null) {
                                        String exitCode = exitChange.getChangeTypeCode();
                                        if (exitCode != null || m_exitCodeOverride) {
                                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_260_CATE_EXIT_CODE),
                                                    exitCode);
                                            programUpdated = true;
                                        }
                                    }
                                }

                                if (!StringUtils.isEmpty(m_programCompleter) && !"-1".equals(m_programCompleter)) {
                                    program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1720_PROGRAM_COMPLETER),
                                            m_programCompleter);
                                    programUpdated = true;
                                }

                                if (!StringUtils.isEmpty(m_tranMode) && !"-1".equals(m_tranMode)) {
                                    program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1730_TRAN_MODE), m_tranMode);
                                    programUpdated = true;
                                } else if (databaseDistrictReponsibleCodePresent) {
                                    if (program.getFieldValueByBeanPath(m_javaNames.get(I4SEE_1730_TRAN_MODE)) == null
                                            || "-1".equals(m_programCompleter)) {
                                        if (outsideAreaStudent) {
                                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1730_TRAN_MODE), "6");
                                        } else {
                                            program.setFieldValueByBeanPath(m_javaNames.get(I4SEE_1730_TRAN_MODE), "8");
                                        }
                                        programUpdated = true;
                                    }
                                }
                                Object primaryProgram =
                                        program.getFieldValueByBeanPath(m_javaNames.get(ALIAS_PRIMARY_PROGRAM_FLAG));
                                if (primaryProgram == null) {
                                    program.setFieldValueByBeanPath(m_javaNames.get(ALIAS_PRIMARY_PROGRAM_FLAG),
                                            BooleanAsStringConverter.FALSE);
                                    programUpdated = true;
                                }

                                if (singlePrimary != 1) {
                                    if (singlePrimary == -1) {
                                        program.setFieldValueByBeanPath(m_javaNames.get(ALIAS_PRIMARY_PROGRAM_FLAG),
                                                BooleanAsStringConverter.FALSE);
                                        programUpdated = true;
                                    } else {
                                        program.setFieldValueByBeanPath(m_javaNames.get(ALIAS_PRIMARY_PROGRAM_FLAG),
                                                BooleanAsStringConverter.TRUE);
                                        studentNames.add(student.getNameView());
                                        singlePrimary = -1;
                                        programUpdated = true;
                                    }
                                }
                                getBroker().saveBeanForced(program);
                                updated += (programUpdated && !isEmptyPgms) ? 1 : 0;
                            }
                        }
                        // We still want to add out ID to make sure if there are too many that we
                        // drop it
                        studentProgramIds.add(courseProgramId);
                    }
                }


                int studentProgramIDCount = studentProgramIds.size();

                if (studentProgramIDCount < studentProgramParticipationCount) {
                    // Remove some program records.
                    // First, look for program IDs not in the schedule program Id collection.
                    while (studentProgramIDCount < studentProgramParticipationCount) {
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
                            int left = studentProgramIDCount - studentProgramParticipationCount;
                            for (int i = 0; i < left; i++) {
                                Iterator<StudentProgramParticipation> pIterator = programs.iterator();
                                StudentProgramParticipation p = pIterator.next();
                                pIterator.remove();
                                getBroker().deleteBean(p);
                                deleted++;
                            }
                        }
                        studentProgramParticipationCount = programs.size();
                    }
                }
                /*
                 * Update fields on the student record
                 */

                if (!StringUtils.isEmpty(m_cateEnrollmentStatus) && !"-1".equals(m_cateEnrollmentStatus)) {
                    WebUtils.setProperty(student, m_javaNames.get(I4SEE_1700_CATE_ENROLLMENT_STATUS),
                            m_cateEnrollmentStatus);
                } else if (!enrolled && "-1".equals(m_cateEnrollmentStatus)) {
                    WebUtils.setProperty(student, m_javaNames.get(I4SEE_1700_CATE_ENROLLMENT_STATUS), "9");
                }
                if (m_setCateReport) {
                    WebUtils.setProperty(student, m_javaNames.get(I4SEE_STATUS_FIELD), I4SEE_STATUS_FIELD_REPORT_CODE);
                }
                getBroker().saveBeanForced(student);
            }
        } finally {
            iterator.close();
        }

        String multipleProgramMessage = (studentNames.size() < 1) ? "There were no students with multiple programs.\n"
                : "The following students had multiple programs and each one was assigned a different number.\n";

        logMessage(multipleProgramMessage);

        Collections.sort(studentNames);
        for (String name : studentNames) {
            logMessage(name);
        }

        logMessage("Created " + count + " Program records.  Updated " + updated + " Program records. Deleted " + deleted
                + " Program records.");
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
        m_javaNames.put(I4SEE_260_CATE_EXIT_CODE, translateAliasToJavaName(I4SEE_260_CATE_EXIT_CODE));
        m_javaNames.put(I4SEE_STATUS_FIELD, translateAliasToJavaName(I4SEE_STATUS_FIELD));
        m_javaNames.put(I4SEE_CATE_CONTEXT, translateAliasToJavaName(I4SEE_CATE_CONTEXT));
        m_javaNames.put(ALIAS_PRIMARY_PROGRAM_FLAG, translateAliasToJavaName(ALIAS_PRIMARY_PROGRAM_FLAG));
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
        PreparedStatement statement = connection.prepareStatement(sql);

        try {
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
                } catch (SQLException e) {
                    // ignore
                }
            }

            getBroker().returnConnection();
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
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
}
