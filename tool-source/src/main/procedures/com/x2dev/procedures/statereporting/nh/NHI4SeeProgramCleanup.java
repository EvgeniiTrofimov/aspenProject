/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2021 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;


public class NHI4SeeProgramCleanup extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String ALIAS_CRS_PROGRAM_ID = "i4see 1740";
    private static final String ALIAS_PGM_PROGRAM_ID = "i4see 1710"; // CIP
    private static final String ALIAS_PGM_CATE_CONTEXT = "i4see CATE CONTEXT";

    private static final String I4SEE_CATE_PROGRAM_CODE = "CATE";

    /*
     * INPUT PARAMETERS
     */
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

    private String m_fieldCateContext;
    private String m_fieldCrsProgId;
    private String m_fieldPgmProgId;

    /*
     * Collected data
     */
    private Map<String, Collection<StudentProgramParticipation>> m_existingPrograms;

    /**
     * The Class HelperReportData.
     */
    class HelperReportData extends StateReportData {
        /*
         * Constants: Parameters, Constants
         */
        protected static final String PARAM_SCHOOLS = "schoolOids";

        /*
         * Instance variables.
         */
        protected StudentHistoryHelper m_helper;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            String fieldProgramId = translateAliasToJavaName(ALIAS_CRS_PROGRAM_ID, true);

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);

            Object objSchools = getParameter(PARAM_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                m_helper.getStudentCriteria().addIn(Student.COL_SCHOOL_OID, oids);
            }


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
        int updated = 0;

        m_fieldCateContext = translateAliasToJavaName(ALIAS_PGM_CATE_CONTEXT);
        m_fieldCrsProgId = translateAliasToJavaName(ALIAS_CRS_PROGRAM_ID);
        m_fieldPgmProgId = translateAliasToJavaName(ALIAS_PGM_PROGRAM_ID);

        loadCATEStudentProgramParticipation();

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

        ArrayList<String> studentNames = new ArrayList<String>();

        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                List<StudentScheduleSpan> spans = getFilteredScheduleSpans(student);
                for (StudentScheduleSpan span : spans) {
                    String crsProgId = (String) span.getSection().getSchoolCourse().getCourse()
                            .getFieldValueByBeanPath(m_fieldCrsProgId);
                    if (!StringUtils.isEmpty(crsProgId) && span.getExitChange() != null) {
                        Collection<StudentProgramParticipation> programs = m_existingPrograms.get(student.getOid());
                        if (programs != null) {
                            for (StudentProgramParticipation pgm : programs) {
                                String pgmProgId = (String) pgm.getFieldValueByBeanPath(m_fieldPgmProgId);
                                if (StringUtils.isEqual(pgmProgId, crsProgId) && pgm.getEndDate() == null) {
                                    pgm.setEndDate(span.getExitChange().getEffectiveDate());
                                    getBroker().saveBeanForced(pgm);
                                    if (!studentNames.contains(student.getNameView())) {
                                        studentNames.add(student.getNameView());
                                    }
                                    updated++;
                                }
                            }
                        }
                    }
                }

            }
        } finally {
            iterator.close();
        }

        if (updated > 0) {
            logMessage("The following students have programs that have been updated.\n");

            Collections.sort(studentNames);
            for (String name : studentNames) {
                logMessage(name);
            }

            logMessage("\nUpdated " + updated + " Program records.");
        }
    }

    /**
     * Gets the filtered schedule spans.
     *
     * @param student SisStudent
     * @return List
     */
    private List<StudentScheduleSpan> getFilteredScheduleSpans(SisStudent student) {
        List<StudentScheduleSpan> spans = m_data.m_helper.getStudentScheduleSpans(student);
        List<StudentScheduleSpan> spansToRemove = new ArrayList<StudentScheduleSpan>();
        Map<String, StudentScheduleSpan> map = new HashMap<String, StudentScheduleSpan>();

        for (StudentScheduleSpan span : spans) {
            StudentScheduleSpan prevSpan = map.get(span.getSection().getSchoolCourse().getCourse().getOid());
            if (prevSpan == null) {
                map.put(span.getSection().getSchoolCourse().getCourse().getOid(), span);
            } else {
                if (span.getEntryDate().after(prevSpan.getEntryDate())) {
                    spansToRemove.add(prevSpan);
                    map.put(span.getSection().getSchoolCourse().getCourse().getOid(), span);
                } else {
                    spansToRemove.add(span);
                }
            }
        }
        spans.removeAll(spansToRemove);
        return spans;
    }


    /**
     * loads existing Student Program Participation records for this year's CATE program.
     */
    private void loadCATEStudentProgramParticipation() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, I4SEE_CATE_PROGRAM_CODE);
        criteria.addEqualTo(m_fieldCateContext, getCurrentContext().getContextId());
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        m_existingPrograms = getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                getBroker().getCount(query));
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
}
