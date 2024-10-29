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

package com.x2dev.procedures.statereporting.wa;

import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

/**
 * Export Data Module for WA District Student.
 *
 * @author X2 Development Corporation
 */
public class WALearningAssistanceProgram extends WAStudentProgram {
    /**
     * Entity class for WA District Student export.
     *
     */
    public static class WALearningAssistanceProgramEntity extends StateReportEntity {
        List<StudentInfo> m_statistics;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId();

            return name;
        }

        /**
         * Initialize method.
         * Determine if the program overlaps an active span to get the appropriate school at the
         * time.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_statistics = new ArrayList<StudentInfo>();

            WALearningAssistanceProgram lapData = (WALearningAssistanceProgram) data;

            SisStudent student = (SisStudent) bean;

            // Skip the program if the student is not in the District Student file.
            List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, lapData);
            if (districtSpans.size() == 0) {
                setRowCount(0);
                return;
            }

            Collection<StudentProgramParticipation> pgms = lapData.m_pgmMap.get(student.getOid());
            List<StudentEnrollmentSpan> enrollmentSpans = lapData.m_helper.getStudentEnrollmentSpans(student, true);

            if (pgms != null) {
                for (StudentProgramParticipation program : pgms) {

                    String pgmStateCode = data.lookupStateValue(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE,
                            program.getProgramCode());
                    if (pgmStateCode == null || !lapData.m_reportedPgmCodes.contains(pgmStateCode)) {
                        continue;
                    }

                    SisSchool school = null;

                    // Stop matching when any rule returns a result
                    // Rule 1 - find school based on enrolled school when program started.
                    if (program.getStartDate() != null) {
                        school = withinSpans(program.getStartDate(), enrollmentSpans);
                    }

                    // Rule 2 - find the school based on enrolled school when program ended.
                    if (school == null && program.getEndDate() != null) {
                        school = withinSpans(program.getStartDate(), enrollmentSpans);
                    }

                    // Rule 3 - find the school based on the most current enrollment
                    if (school == null) {
                        for (StudentEnrollmentSpan span : enrollmentSpans) {
                            if (span.getSchool() != null) {
                                StudentEnrollment enrollment = getLastActiveEnrollment(span);
                                school = (enrollment != null && enrollment.getSchool() != null) ? enrollment.getSchool()
                                        : span.getSchool();
                            }
                        }
                    }

                    if (school == null) {
                        school = student.getSchool();
                    }

                    if (!lapData.includeSchool(school.getOid())) {
                        break;
                    }

                    StudentInfo stdInfo = lapData.new StudentInfo();
                    stdInfo.setProgram(program);
                    stdInfo.setSchool(school);

                    m_statistics.add(stdInfo);
                }
            }

            setRowCount(m_statistics.size());
        }

        /**
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return DistrictEnrollmentSpan
         */
        public StudentInfo getStdInfo() {
            return m_statistics.get(getCurrentRow());
        }


        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. in district
         * transfer.
         *
         * @param student Student
         * @param sdData WALearningAssistanceProgram
         * @return List<DistrictEnrollmentSpan>
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WALearningAssistanceProgram sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans = sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                /*
                 * Check if the span is a no-show span. Do not include no-show spans.
                 * A no-show span represents an enrollment where the student never showed up.
                 * It is identified by a withdrawal code that has NS in the local code of the
                 * reference table.
                 */
                boolean noShow = false;
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    String withdrawalCode = enrollment.getEnrollmentCode();
                    withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode, withdrawalCode,
                            ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (CODE_NO_SHOW.equals(withdrawalCode)) {
                        noShow = true;
                    }
                }

                if (!noShow) {
                    // Check the span for entry type (internal or external)
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        String code = enrollment.getEnrollmentCode();
                        String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if ("1".equals(stateCode) || "2".equals(stateCode)) {
                            if (currentSpan != null &&
                                    (currentSpan.m_exitDate == null ||
                                            !sdData.getOrganization().getCurrentContext().getStartDate()
                                                    .after(currentSpan.m_exitDate))) {
                                districtSpans.add(currentSpan);
                            }
                            currentSpan = null;

                            currentSpan = new DistrictEnrollmentSpan();
                            currentSpan.m_entryEnrollment = enrollment;
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                        } else {
                            if (currentSpan == null) {
                                currentSpan = new DistrictEnrollmentSpan();
                                currentSpan.m_entryEnrollment = enrollment;
                            }
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                        }
                    }
                }
            }
            if (currentSpan != null &&
                    (currentSpan.m_exitDate == null ||
                            !sdData.getOrganization().getCurrentContext().getStartDate()
                                    .after(currentSpan.m_exitDate))) {
                districtSpans.add(currentSpan);
            }

            return districtSpans;
        }

        /**
         * Gets the last active enrollment.
         *
         * @param span StudentEnrollmentSpan
         * @return Student enrollment
         */
        private StudentEnrollment getLastActiveEnrollment(StudentEnrollmentSpan span) {
            StudentEnrollment value = span.getFirstActiveEnrollment();
            for (StudentEnrollment enrollment : span.getEnrollments()) {
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())
                        && "Active".equals(enrollment.getStatusCode())) {
                    value = enrollment;
                }
            }
            return value;
        }

        /**
         * Within spans.
         *
         * @param date PlainDate
         * @param spans List<StudentEnrollmentSpan>
         * @return SisSchool
         */
        private SisSchool withinSpans(PlainDate date, List<StudentEnrollmentSpan> spans) {
            SisSchool school = null;
            for (StudentEnrollmentSpan span : spans) {
                if (!span.getFirstActiveDate().after(date)
                        && (span.getLastActiveDate() == null || !span.getLastActiveDate().before(date))) {
                    StudentEnrollment enrollment = getLastActiveEnrollment(span);
                    if (enrollment != null && enrollment.getSchool() != null) {
                        school = enrollment.getSchool();
                        break;
                    }
                    if (span.getSchool() != null) {
                        school = span.getSchool();
                        break;
                    }
                }
            }
            return school;
        }
    }

    /**
     * Returns calculated values for the program from the entity.
     * <p>
     * Param:
     * <br>
     * SCHOOL: The school id (LocationId)
     * <br>
     * PGM_CODE: The school id (ProgramCode)
     * <br>
     * START_DATE: The school id (StartDate)
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            WALearningAssistanceProgram lapData = (WALearningAssistanceProgram) data;
            WALearningAssistanceProgramEntity lapEntity = (WALearningAssistanceProgramEntity) entity;

            StudentInfo stdInfo = lapEntity.getStdInfo();

            String parameter = (String) field.getParameter();

            if (PARAM_SCHOOL.equals(parameter)) {
                StudentEnrollment enrollment = null;
                List<StudentEnrollmentSpan> spans =
                        lapData.m_helper.getStudentEnrollmentSpans((SisStudent) entity.getBean(), true);
                for (StudentEnrollmentSpan span : spans) {
                    enrollment = span.getFirstActiveEnrollment();
                }
                if (null != enrollment) {
                    value = enrollment.getFieldValueByBeanPath(m_fieldLocationOverride);
                }
                if (null == value) {
                    value = stdInfo.getSchool().getSchoolId();
                }
            } else if (PARAM_PGM_CODE.equals(parameter)) {
                value = data.lookupStateValue(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE,
                        ((StudentProgramParticipation) stdInfo.getProgram()).getProgramCode());

            } else if (PARAM_START_DATE.equals(parameter)) {
                if (stdInfo.getProgram() != null) {
                    value = ((StudentProgramParticipation) stdInfo.getProgram()).getStartDate();
                }
            }

            return value;
        }
    }

    /**
     * Returns calculated values for the program from the entity.
     * <p>
     * Param:
     * <br>
     * SCHOOL: The school id (LocationId)
     * <br>
     * PGM_CODE: The school id (ProgramCode)
     * <br>
     * START_DATE: The school id (StartDate)
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgramAlias implements FieldRetriever {
        private static final String DATE_FORMAT_DB = "yyyy-MM-dd";

        private static final String FIELD_NAME_DATE_BEGINNING = "DateBeginning";
        private static final String FIELD_NAME_DATE_END = "DateEnd";

        private SimpleDateFormat m_dateFormatDB;

        /**
         * Instantiates a new retrieve program alias.
         */
        public RetrieveProgramAlias() {
            m_dateFormatDB = new SimpleDateFormat(DATE_FORMAT_DB);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            WALearningAssistanceProgramEntity lapEntity = (WALearningAssistanceProgramEntity) entity;

            StudentInfo stdInfo = lapEntity.getStdInfo();

            String parameter = (String) field.getParameter();

            if (m_aliases.contains(parameter)) {
                value = ((StudentProgramParticipation) stdInfo.getProgram()).getFieldValueByAlias(parameter);
                if (!StringUtils.isEmpty((String) value)) {
                    // state codes
                    if (m_aliasesState.contains(parameter)) {
                        value = lookupReferenceCodeByAlias(parameter, (String) value,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                    // dates
                    if (m_aliasesDates.contains(parameter)) {
                        try {
                            value = m_dateFormatDB.parse((String) value);
                        } catch (ParseException e) {
                            e.printStackTrace();
                        }
                    }
                    // numeric
                    if (m_aliasesNums.contains(parameter)) {
                        if ((parameter.equals(ALIAS_BEGIN_SCORE)
                                && !StringUtils.isEmpty(entity.getFieldValue(FIELD_NAME_DATE_BEGINNING))) ||
                                (parameter.equals(ALIAS_END_SCORE)
                                        && !StringUtils.isEmpty(entity.getFieldValue(FIELD_NAME_DATE_END)))) {
                            value = BigDecimal.valueOf(Double.parseDouble((String) value));
                        } else {
                            value = null;
                        }
                    }
                }
            }

            return value;
        }
    }

    protected Collection<String> m_reportedPgmCodes = null;

    private static final String ALIAS_ACD_GROWTH = "DOE LAP ACADEMIC GROWTH";
    private static final String ALIAS_BEGIN_SCORE = "DOE LAP BEGINNING SCORE";
    private static final String ALIAS_DATE_BEGIN_SCORE = "DOE LAP DATE OF BEGINNING SCORE";
    private static final String ALIAS_DATE_END_SCORE = "DOE LAP DATE OF END SCORE";
    private static final String ALIAS_END_SCORE = "DOE LAP END SCORE";
    private static final String ALIAS_EXT_LRN_TIME = "DOE LAP EXTENDED LEARNING TIME";
    private static final String ALIAS_ID_ASM = "DOE LAP IDENTIFICATION ASSESSMENT";
    private static final String ALIAS_MON_ASM = "DOE LAP MONITORING ASSESSMENT";
    private static final String ALIAS_STD_MET_GOAL = "DOE LAP STUDENT MET GOAL";
    private static final String ALIAS_TUTOR_INTERVENT = "DOE LAP TUTORING INTERVENTION";

    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    private static final String[] REPORTED_PROGRAM_CODES = {"6", "7", "37", "38", "44"};

    protected List<String> m_aliases =
            Arrays.asList(ALIAS_ACD_GROWTH, ALIAS_BEGIN_SCORE, ALIAS_DATE_BEGIN_SCORE, ALIAS_DATE_END_SCORE,
                    ALIAS_END_SCORE, ALIAS_EXT_LRN_TIME, ALIAS_ID_ASM, ALIAS_MON_ASM, ALIAS_STD_MET_GOAL,
                    ALIAS_TUTOR_INTERVENT);
    protected List<String> m_aliasesState = Arrays.asList(ALIAS_ACD_GROWTH, ALIAS_EXT_LRN_TIME, ALIAS_ID_ASM,
            ALIAS_MON_ASM, ALIAS_STD_MET_GOAL, ALIAS_TUTOR_INTERVENT);
    protected List<String> m_aliasesDates = Arrays.asList(ALIAS_DATE_BEGIN_SCORE, ALIAS_DATE_END_SCORE);
    protected List<String> m_aliasesNums = Arrays.asList(ALIAS_BEGIN_SCORE, ALIAS_END_SCORE);

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        validateAliases();
        if (getSetupErrors().size() == 0) {
            m_reportedPgmCodes = Arrays.asList(REPORTED_PROGRAM_CODES);

            setEntityClass(WALearningAssistanceProgramEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PGM-INFO", new RetrieveProgram());
            calcs.put("PGM-INFO-ALIAS", new RetrieveProgramAlias());
            super.addCalcs(calcs);
        }
    }

    /**
     * Validate aliases.
     */
    private void validateAliases() {
        for (String alias : m_aliases) {
            DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
            if (field == null) {
                String aliasMsg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
                addSetupError(aliasMsg, alias);
            }
        }
    }
}
