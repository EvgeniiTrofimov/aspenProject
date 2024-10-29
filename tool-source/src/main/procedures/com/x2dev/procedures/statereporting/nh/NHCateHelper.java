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

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class is helper for CATE exports.
 *
 * @author X2 Development Corporation
 */
public class NHCateHelper {
    /**
     * Helper class to hold student enrollment status.
     *
     * @author X2 Development Corporation
     */
    protected class CateEnrollmentSpan {
        private String m_cateEnrStatus;
        private PlainDate m_entryDate;
        private String m_entryCode;
        private PlainDate m_exitDate;
        private String m_exitCode;
        private String m_gradeLevel;
        private StudentProgramParticipation m_program;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CateEnrollmentSpan() {
            // no arguments
        }

        /**
         * @return the m_cateEnrStatus
         */
        public String getCateEnrStatus() {
            return m_cateEnrStatus;
        }

        /**
         * @param cateEnrStatus the cateEnrStatus to set
         */
        public void setCateEnrStatus(String cateEnrStatus) {
            this.m_cateEnrStatus = cateEnrStatus;
        }

        /**
         * @return the m_entryDate
         */
        public PlainDate getEntryDate() {
            return m_entryDate;
        }

        /**
         * @param entryDate the m_entryDate to set
         */
        public void setEntryDate(PlainDate entryDate) {
            this.m_entryDate = entryDate;
        }

        /**
         * @return the m_entryCode
         */
        public String getEntryCode() {
            return m_entryCode;
        }

        /**
         * @param entryCode the m_entryCode to set
         */
        public void setEntryCode(String entryCode) {
            this.m_entryCode = entryCode;
        }

        /**
         * @return the m_exitDate
         */
        public PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * @param exitDate the m_exitDate to set
         */
        public void setExitDate(PlainDate exitDate) {
            this.m_exitDate = exitDate;
        }

        /**
         * @return the m_exitCode
         */
        public String getExitCode() {
            return m_exitCode;
        }

        /**
         * @param exitCode the m_exitCode to set
         */
        public void setExitCode(String exitCode) {
            this.m_exitCode = exitCode;
        }

        /**
         * @return the m_gradeLevel
         */
        public String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * @param gradeLevel the m_gradeLevel to set
         */
        public void setGradeLevel(String gradeLevel) {
            this.m_gradeLevel = gradeLevel;
        }

        /**
         * @return the program
         */
        public StudentProgramParticipation getProgram() {
            return m_program;
        }

        /**
         * @param program the m_program to set
         */
        public void setProgram(StudentProgramParticipation program) {
            this.m_program = program;
        }
    }

    private static final String ALIAS_CATE_ENR_STATUS = "all-std-CATEEnrollmentStatus";
    private static final String ALIAS_CATE_ENTRY_CODE = "all-pgm-EntryCode";
    private static final String ALIAS_CATE_ENTRY_DATE = "all-pgm-EntryDate";
    private static final String ALIAS_CATE_EXIT_CODE = "all-pgm-ExitCode";
    private static final String ALIAS_CATE_EXIT_DATE = "all-pgm-ExitDate";
    private static final String ALIAS_PROGRAM_COMPLETER = "all-pgm-ProgramCompleter";

    protected EnrollmentManager m_enrollManager;
    protected StudentHistoryHelper m_stdHelper;
    protected StateReportData m_stateReportData;
    protected X2Broker m_broker;
    protected Map<String, List<StudentProgramParticipation>> m_existingPrograms;
    protected Map<String, List<CateEnrollmentSpan>> m_cateSpans;
    Map<String, HashMap<String, PlainDate>> m_lastInSessionDateMap = new HashMap<String, HashMap<String, PlainDate>>();

    private Set<String> m_grade12Codes;

    /**
     * Public no argument constructor for dynamic instantiation.
     *
     * @param data
     * @param broker
     * @param stdProgramCode
     * @param contextBeanPath
     */
    public NHCateHelper(StateReportData data, X2Broker broker, String stdProgramCode, String contextBeanPath) {
        m_stdHelper = new StudentHistoryHelper(data);
        m_stateReportData = data;
        m_broker = broker;
        m_enrollManager = new EnrollmentManager(data.getBroker(), data.getPrivilegeSet(), data.getOrganization());

        loadCATEStudentProgramParticipation(stdProgramCode, contextBeanPath);
    }

    /**
     * @return the m_stdHelper
     */
    public StudentHistoryHelper getStdHelper() {
        return m_stdHelper;
    }

    /**
     * Return codes of CATE Enrollment Status. Students with codes should not be included.
     *
     * @param stateCode
     * @param alias
     *
     * @return
     */
    public Collection<String> getCodesByStateAndAlias(String stateCode, String alias) {
        DataDictionaryField stdCateStatusField =
                m_stateReportData.getDataDictionary().findDataDictionaryFieldByAlias(alias);
        Collection<String> stdCateStatusCodes = new ArrayList<String>();

        if (stdCateStatusField.getReferenceTable() != null) {
            Collection<ReferenceCode> refCodes = stdCateStatusField.getReferenceTable().getReferenceCodes();

            for (ReferenceCode refCode : refCodes) {
                if (stateCode.equals(refCode.getStateCode())) {
                    stdCateStatusCodes.add(refCode.getCode());
                }
            }
        }

        return stdCateStatusCodes;
    }

    /**
     * Return codes of CATE Enrollment Status. Students with codes should not be included.
     *
     * @param stateCode
     * @return
     */
    public Collection<String> getProgramCodesByStateCode(String stateCode) {
        DataDictionaryField pgmCodeField =
                m_stateReportData.getDataDictionary().findDataDictionaryField(
                        StudentProgramParticipation.class.getName(), StudentProgramParticipation.COL_PROGRAM_CODE);
        Collection<String> pgmCodes = new ArrayList<String>();

        if (pgmCodeField.getReferenceTable() != null) {
            Collection<ReferenceCode> refCodes = pgmCodeField.getReferenceTable().getReferenceCodes();

            for (ReferenceCode refCode : refCodes) {
                if (stateCode.equals(refCode.getStateCode())) {
                    pgmCodes.add(refCode.getCode());
                }
            }
        }

        return pgmCodes;
    }

    /**
     * Returns enrollment spans for the given student.
     *
     * @param student
     * @param includeSummerWithdrawals
     * @return
     */
    public List<CateEnrollmentSpan> getCateEnrollmentSpans(SisStudent student) {
        List<CateEnrollmentSpan> cateSpans = new ArrayList<NHCateHelper.CateEnrollmentSpan>();
        List<StudentProgramParticipation> pgms = m_existingPrograms.get(student.getOid());
        List<StudentEnrollmentSpan> enrSpans = m_stdHelper.getStudentEnrollmentSpans(student, true);

        if (pgms != null) {
            for (StudentProgramParticipation pgm : pgms) {
                PlainDate entryDate = null;
                String entryCode = null;
                PlainDate exitDate = null;
                String exitCode = null;

                entryDate = (PlainDate) pgm.getFieldValueByAlias(ALIAS_CATE_ENTRY_DATE);

                if (entryDate != null) {
                    for (StudentEnrollmentSpan enrSpan : enrSpans) {
                        if (!enrSpan.getFirstActiveEnrollment().getEnrollmentDate().after(entryDate)
                                && (enrSpan.getLastActiveDate() == null
                                        || enrSpan.getLastActiveDate().after(entryDate))) {
                            exitDate = getCateExitDate(pgm);
                            entryCode = getCateEntryExitCode(pgm, ALIAS_CATE_ENTRY_CODE);
                            exitCode = getCateEntryExitCode(pgm, ALIAS_CATE_EXIT_CODE);

                            if (m_stateReportData.getParameter("reportType") != null
                                    && ((Integer) m_stateReportData.getParameter("reportType")).intValue() == 1
                                    && StringUtils.isEmpty(exitCode)) {
                                exitCode = "W31"; // default value for Student
                                if (isGrade12(student.getGradeLevel())) {
                                    if (BooleanAsStringConverter.TRUE
                                            .equals(pgm.getFieldValueByAlias(ALIAS_PROGRAM_COMPLETER))) {
                                        exitCode = "W32";
                                    } else {
                                        exitCode = "W33";
                                    }
                                }
                            }

                            CateEnrollmentSpan cateSpan = new CateEnrollmentSpan();
                            cateSpan.setEntryDate(entryDate);
                            cateSpan.setExitDate(exitDate);
                            cateSpan.setEntryCode(entryCode);
                            cateSpan.setExitCode(exitCode);
                            cateSpan.setCateEnrStatus(getCateEnrStatus(pgm.getStudent(), ALIAS_CATE_ENR_STATUS));
                            cateSpan.setGradeLevel(pgm.getStudent().getGradeLevel());
                            cateSpan.setProgram(pgm);

                            cateSpans.add(cateSpan);
                            break;
                        }
                    }
                }
            }
        }

        return cateSpans;
    }

    /**
     * Returns entry or exit state code for the program.
     *
     * @param program
     * @param alias
     * @return
     */
    private String getCateEntryExitCode(StudentProgramParticipation program, String alias) {
        String stateCode = null;
        String progCateEntryCodeField = m_stateReportData.translateAliasToJavaName(alias, true);
        String code = (String) program.getFieldValueByBeanPath(progCateEntryCodeField);

        if (code != null) {
            stateCode =
                    m_stateReportData.lookupStateValue(StudentProgramParticipation.class, progCateEntryCodeField, code);

            if (stateCode == null) {
                stateCode = code;
            }
        }

        return stateCode;
    }

    /**
     * Returns enrollment status state code for the program.
     *
     * @param student
     * @param alias
     * @return
     */
    private String getCateEnrStatus(SisStudent student, String alias) {
        String stateCode = null;
        String stdCateEnrStatus = m_stateReportData.translateAliasToJavaName(alias, true);
        String code = (String) student.getFieldValueByBeanPath(stdCateEnrStatus);

        if (code != null) {
            stateCode = m_stateReportData.lookupStateValue(SisStudent.class, stdCateEnrStatus, code);

            if (stateCode == null) {
                stateCode = code;
            }
        }

        return stateCode;
    }

    /**
     * Returns exit date of the program.
     *
     * @param program
     * @return
     */
    private PlainDate getCateExitDate(StudentProgramParticipation program) {
        PlainDate programExitDate = null;
        SisStudent student = program.getStudent();

        if (program != null) {
            programExitDate = (PlainDate) program.getFieldValueByAlias(ALIAS_CATE_EXIT_DATE);
            if (programExitDate != null) {
                return programExitDate;
            }
            // Default to last in-session date of student calendar for EOY. If
            // no calendar exists, use district end date for current school
            // year.
            // These values are lazy loaded into m_lastInSessionDateMap using
            // the format HashMap<schoolOid, HashMap<calendarId, PlainDate>>
            else if (m_stateReportData.getParameter("reportType") != null
                    && ((Integer) m_stateReportData.getParameter("reportType")).intValue() == 1
                    && programExitDate == null) {
                PlainDate lastInSessionDay = null;

                String calendarId = student.getCalendarCode();
                if (m_lastInSessionDateMap.get(student.getSchoolOid()) != null
                        && m_lastInSessionDateMap.get(student.getSchoolOid()).get(calendarId) != null) {
                    programExitDate = m_lastInSessionDateMap.get(student.getSchoolOid()).get(calendarId);
                } else {
                    Map<String, Set<PlainDate>> calendarDatesMap =
                            m_enrollManager.getCalendarLookup(student.getSchool(), m_stateReportData
                                    .getCurrentContext().getStartDate(), m_stateReportData.getCurrentContext()
                                            .getEndDate(),
                                    m_stateReportData.getCurrentContext().getOid());
                    for (PlainDate date : calendarDatesMap.get(calendarId)) {
                        if (lastInSessionDay == null || lastInSessionDay.before(date)) {
                            lastInSessionDay = date;
                        }
                    }
                    if (lastInSessionDay != null) {
                        programExitDate = lastInSessionDay;
                    } else {
                        programExitDate = m_stateReportData.getCurrentContext().getEndDate();
                    }

                    // Add schoolOid to map if it doesn't already exist
                    if (!m_lastInSessionDateMap.containsKey(student.getSchoolOid())) {
                        HashMap<String, PlainDate> lastDateByCalendar = new HashMap<String, PlainDate>();
                        m_lastInSessionDateMap.put(student.getSchoolOid(), lastDateByCalendar);
                    }

                    HashMap<String, PlainDate> lastDateByCalendar = m_lastInSessionDateMap.get(student.getSchoolOid());
                    if (!lastDateByCalendar.containsKey(calendarId)) {
                        lastDateByCalendar.put(calendarId, programExitDate);
                    }
                }
            }
        }
        return programExitDate;
    }

    /**
     * @param gradeLevel
     * @return
     */
    private boolean isGrade12(String gradeLevel) {
        if (m_grade12Codes == null) {
            m_grade12Codes = new HashSet();
            DataDictionaryField dictionaryField =
                    m_stateReportData.getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                ReferenceTable rtb = dictionaryField.getReferenceTable();
                if (rtb.getExtendedDataDictionary() != null) {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(rtb.getExtendedDataDictionary(),
                            m_broker.getPersistenceKey());
                    DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias("NumericGradeLevel");
                    if (field != null) {
                        X2Criteria criteria = new X2Criteria();
                        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtb.getOid());
                        criteria.addEqualTo(field.getJavaName(), "12");

                        String[] columns = new String[] {ReferenceCode.COL_CODE};
                        ColumnQuery query = new ColumnQuery(ReferenceCode.class, columns, criteria);
                        ReportQueryIterator queryItr = m_broker.getReportQueryIteratorByQuery(query);
                        try {
                            while (queryItr.hasNext()) {
                                Object[] row = (Object[]) queryItr.next();
                                String code = (String) row[0];
                                m_grade12Codes.add(code);
                            }
                        } finally {
                            queryItr.close();
                        }
                    }
                }
            }
        }
        return m_grade12Codes.contains(gradeLevel);
    }

    /**
     * loads existing Student Program Participation records for the CATE program
     */
    private void loadCATEStudentProgramParticipation(String stateProgramCode, String contextBeanPath) {
        String contextId = m_stateReportData.getCurrentContext().getContextId();

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, getProgramCodesByStateCode(stateProgramCode));
        criteria.addEqualTo(contextBeanPath, contextId);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        m_existingPrograms =
                m_broker.getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                        m_broker.getCount(query));
    }

}
