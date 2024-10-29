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
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: Attendance export.
 * This class implements the data export for MD Attendance export.
 *
 * @author X2 Development Corporation
 */
public class MDPTECHOutcomeExportData extends MDStudentReportData {

    /**
     * The Class HighSchoolDataCollectionEntity.
     */
    public static class MDPTECHOutcomeExportDataEntity extends MDStudentReportEntity {

        /**
         * The Class CollegeRequirements.
         */
        public class CollegeRequirements {
            private BigDecimal m_advancedTechCredit = BigDecimal.ZERO;
            private BigDecimal m_foreignLanguageCredit = BigDecimal.ZERO;
            private BigDecimal m_mathCredit = BigDecimal.ZERO;

            /**
             * Increment.
             *
             * @param type String
             * @param amount BigDecimal
             */
            public void increment(String type, BigDecimal amount) {
                if ("Math".equals(type)) {
                    m_mathCredit = m_mathCredit.add(amount);
                } else if ("Advanced Technology".equals(type)) {
                    m_advancedTechCredit = m_advancedTechCredit.add(amount);
                } else if ("Foreign Language".equals(type)) {
                    m_foreignLanguageCredit = m_foreignLanguageCredit.add(amount);
                }
            }

            /**
             * Checks if is met.
             *
             * @return true, if is met
             */
            public boolean isMet() {
                return BIG_DECIMAL_FOUR.compareTo(m_mathCredit) <= 0
                        || BIG_DECIMAL_TWO.compareTo(m_advancedTechCredit) <= 0
                        || BIG_DECIMAL_TWO.compareTo(m_foreignLanguageCredit) <= 0;
            }
        }

        public final BigDecimal BIG_DECIMAL_TWO = BigDecimal.valueOf(2.0);
        public final BigDecimal BIG_DECIMAL_FOUR = BigDecimal.valueOf(4.0);

        private BigDecimal m_creditDualEnr = BigDecimal.ZERO;
        private BigDecimal m_creditTotal = BigDecimal.ZERO;
        private MDPTECHOutcomeExportData m_data;
        private CollegeRequirements m_colledgeReq;
        private EnrollmentSnapshot m_snapshot = null;
        private boolean m_updated = false;

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (MDPTECHOutcomeExportData) data;
            List<Transcript> transcripts = m_data.getTranscriptsByStd((Student) bean);
            if (transcripts != null) {
                m_colledgeReq = new CollegeRequirements();
                for (Transcript trn : transcripts) {
                    BigDecimal trnCredit = trn.getTotalCredit();
                    if (trnCredit != null) {
                        m_creditTotal = m_creditTotal.add(trnCredit);
                        if (BooleanAsStringConverter.TRUE.equals(
                                trn.getSchoolCourse().getCourse()
                                        .getFieldValueByBeanPath(m_data.m_fieldCrsFieldDualEnr))) {
                            m_creditDualEnr = m_creditDualEnr.add(trnCredit);
                        }
                        String reqType = (String) trn.getSchoolCourse().getCourse()
                                .getFieldValueByBeanPath(m_data.m_fieldCrsCollegeReq);
                        m_colledgeReq.increment(reqType, trnCredit);
                    }
                }
            }
            m_snapshot = getSnapshot((SisStudent) bean, m_data.getReportContext().getEndDate(),
                    data.getFieldDefinition(DOE_14_ENTRY_STATUS));
        }

        /**
         * Post process.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            super.postProcess();
            SisStudent student = (SisStudent) getBean();
            if (m_data.isUpdateStudent() && m_updated) {
                getData().getBroker().saveBeanForced(student);
            }
        }

        /**
         * Gets dual enrollment credit
         *
         * @return BigDecimal
         */
        public BigDecimal getCreditDualEnr() {
            return m_creditDualEnr;
        }

        /**
         * Gets total credit
         *
         * @return BigDecimal
         */
        public BigDecimal getCreditTotal() {
            return m_creditTotal;
        }

        /**
         * Return the enrollment snapshot that is used by some
         * fieldRetrievers to get enrollment data.
         *
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot() {
            return m_snapshot;
        }

        /**
         * Gets the school.
         *
         * @param data MDStudentReportData
         * @return Sis school
         * @see com.x2dev.procedures.statereporting.md.MDStudentReportData.MDStudentReportEntity#getSchool(com.follett.fsc.core.k12.tools.stateexports.StateReportData)
         */
        @Override
        public SisSchool getSchool(MDStudentReportData data) {
            SisStudent student = (SisStudent) getBean();
            return student.getSchool();
        }

        /**
         * Is Met College Requirements
         *
         * @return boolean
         */
        public boolean isMetCollegeReq() {
            return m_colledgeReq != null && m_colledgeReq.isMet();
        }

        /**
         *
         */
        public void setUpdated() {
            m_updated = true;
        }

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate, FieldDefinition field) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            if (!snapshot.isPrecise()) {
                addRetrievalError(DOE_14_ENTRY_STATUS, new StateReportValidationError(this, field,
                        "WARNING: Enrollment information (enrollment status, school, and/or YOG) is not precise", ""));
            }

            return snapshot;
        }

    }

    /**
     * Field retriever for Credit Earned.
     */
    protected class CreditEarnedRetriever implements FieldRetriever {

        protected static final String CALC_ID = "CREDIT-EARNED";

        private static final String PARAM_CREDIT_HS = "CREDIT-HS";
        private static final String PARAM_CREDIT_COLLEGE = "CREDIT-COLLEGE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            MDPTECHOutcomeExportDataEntity repEntity = (MDPTECHOutcomeExportDataEntity) entity;
            String param = (String) field.getParameter();
            if (PARAM_CREDIT_HS.equals(param)) {
                value = repEntity.getCreditTotal().subtract(repEntity.getCreditDualEnr());
            } else if (PARAM_CREDIT_COLLEGE.equals(param)) {
                value = repEntity.getCreditDualEnr();
            }
            return value;
        }
    }

    /**
     * Returns the grade level for the YOG in the given snapshot.
     */
    protected class RetrieveGradeLevel implements FieldRetriever {

        public static final String CALC_ID = "GRADE-LEVEL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            EnrollmentSnapshot snapshot = ((MDPTECHOutcomeExportDataEntity) entity).getSnapshot();
            SisStudent student = (SisStudent) entity.getBean();
            int schoolYear = ((MDPTECHOutcomeExportData) data).getReportContext().getSchoolYear();

            int yog = student.getYog();
            if (snapshot.getYog() != yog) {
                yog = snapshot.getYog();
            }
            List gradeLevels =
                    StudentManager.getMatchingGradeLevels(StudentManager.getMaxGradeLevel(getBroker()),
                            yog, schoolYear, m_gradeLevelMap);

            String gradeLevel = (String) gradeLevels.get(0);

            if (gradeLevels.size() > 1) {
                entity.addRetrievalError(DOE_03_GRADE, new StateReportValidationError(entity, field,
                        "WARNING: Calculated grade level is not precise",
                        DOE_03_GRADE + "=" + STYLE_BOLD + gradeLevels.toString() + STYLE_END));
            }

            return gradeLevel;
        }
    }

    /**
     * Field retriever for Report Date.
     */
    protected class RetrieveReportData implements FieldRetriever {

        public static final String CALC_ID = "CALC-REP-DATA";

        private static final String PARAM_YEAR = "YEAR";
        private static final String PARAM_DATE = "DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            if (PARAM_YEAR.equals(param)) {
                value = getReportContext().getSchoolYear();
            } else if (PARAM_DATE.equals(param)) {
                value = ((MDPTECHOutcomeExportData) data).m_reportDate;
            }
            return value;
        }
    }

    /**
     * Field retriever for PTECH Program Year.
     */
    protected class RetrieveStudentAlias implements FieldRetriever {

        public static final String CALC_ID = "STD-ALIAS";

        private static final String ALIAS_CTE_CONCENTRATOR = "all-std-CTEConcentrator";
        private static final String ALIAS_HIGH_SCH_COMPLETION = "all-std-HighSchoolProgramCompletionStatus";
        private static final String ALIAS_HS_ON_TRACK_4 = "all-std-HSonTrack4Years";
        private static final String ALIAS_MET_LOC_GRAD_REQ_PTECH = "all-std-MetLocalGraduationRequirementsPTECH";
        private static final String ALIAS_PTECH_EXIT_STATUS = "all-std-PTECHExitStatus";
        private static final String ALIAS_PTECH_ON_TRACK_4 = "all-std-PTECHonTrack4Years";
        private static final String ALIAS_PTECH_ON_TRACK_5 = "all-std-PTECHonTrack5Years";
        private static final String ALIAS_PTECH_ON_TRACK_6 = "all-std-PTECHonTrack6Years";
        private static final String ALIAS_PTECH_YEAR = "all-std-PTECHYear";

        private static final String CHAR_0 = "0";
        private static final String CHAR_C = "C";
        private static final String CHAR_T = "T";
        private static final String CHAR_W = "W";
        private static final String CHAR_N = "N";
        private static final String CHAR_Y = "Y";

        private static final String COMPLETION_STATUS_00 = "00";
        private static final String COMPLETION_STATUS_01 = "01";
        private static final String COMPLETION_STATUS_02 = "02";
        private static final String COMPLETION_STATUS_03 = "03";

        private static final String GRADE_LEVEL_13 = "13";
        private static final String GRADE_LEVEL_14 = "14";

        private static final String FIELD_NAME_CTE_CONCENTRATOR = "CTE Concentrator";
        private static final String FIELD_NAME_LOC_GRAD_REQ = "Met Local Graduation Requirements PTECH";

        private static final int PTECH_PATHWAY_YEARS_4 = 4;
        private static final int PTECH_PATHWAY_YEARS_5 = 5;
        private static final int PTECH_PATHWAY_YEARS_6 = 6;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            MDPTECHOutcomeExportData reportData = (MDPTECHOutcomeExportData) data;
            SisStudent student = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();

            String fieldPath = data.translateAliasToJavaName((String) field.getParameter(), false);

            String fieldValue = (String) student.getFieldValueByBeanPath(fieldPath);

            if (param.equals(ALIAS_PTECH_YEAR)) {

                String groupYear = (String) student.getFieldValueByBeanPath(reportData.m_fieldStdGroupYear);

                if (!StringUtils.isEmpty(groupYear)) {
                    Integer groupYearInt = Integer.parseInt(groupYear);
                    value = String.valueOf(reportData.getReportContext().getSchoolYear() - groupYearInt + 1);
                }
            } else if (param.equals(ALIAS_CTE_CONCENTRATOR)) {
                value = CHAR_N;
                List<Transcript> trns = reportData.getTranscriptsByStd(student);
                if (trns != null) {
                    value = trns.stream()
                            .filter(trn -> isCTEConcentratorCourse(trn.getSchoolCourse().getCourse()))
                            .findFirst()
                            .map(trn -> CHAR_Y)
                            .orElse(CHAR_N);
                }
            } else if (param.equals(ALIAS_HS_ON_TRACK_4) && fieldPath != null) {
                value = (String) student.getFieldValueByBeanPath(fieldPath);
                if (StringUtils.isEmpty(value)) {
                    value = CHAR_Y;
                } else {
                    // skip update if the field is populated
                    fieldPath = null;
                }
            } else if (param.equals(ALIAS_PTECH_ON_TRACK_4)) {
                value = isPTECHPathwayYearsMatched(student, PTECH_PATHWAY_YEARS_4);
            } else if (param.equals(ALIAS_PTECH_ON_TRACK_5)) {
                value = isPTECHPathwayYearsMatched(student, PTECH_PATHWAY_YEARS_5);
            } else if (param.equals(ALIAS_PTECH_ON_TRACK_6)) {
                value = isPTECHPathwayYearsMatched(student, PTECH_PATHWAY_YEARS_6);
            } else if (param.equals(ALIAS_PTECH_EXIT_STATUS)) {
                value = getPTECHExitStatus(reportData, student);
            } else if (param.equals(ALIAS_MET_LOC_GRAD_REQ_PTECH)) {
                String gradeLevel = student.getGradeLevel();
                if (GRADE_LEVEL_13.equals(gradeLevel) || GRADE_LEVEL_14.equals(gradeLevel)) {
                    value = CHAR_Y;
                } else {
                    value = CHAR_N;
                }
            } else if (param.equals(ALIAS_HIGH_SCH_COMPLETION)) {
                MDPTECHOutcomeExportDataEntity repEntity = (MDPTECHOutcomeExportDataEntity) entity;
                boolean isMetCollegeReq = repEntity.isMetCollegeReq();
                String cteConcentrator = repEntity.getFieldValue(FIELD_NAME_CTE_CONCENTRATOR);
                String locGradReq = repEntity.getFieldValue(FIELD_NAME_LOC_GRAD_REQ);
                value = COMPLETION_STATUS_00;
                if (CHAR_Y.equals(cteConcentrator)) {
                    value = COMPLETION_STATUS_02;
                }
                if (isMetCollegeReq && CHAR_Y.equals(locGradReq) && CHAR_N.equals(cteConcentrator)) {
                    value = COMPLETION_STATUS_01;
                }
                if (isMetCollegeReq && CHAR_Y.equals(locGradReq) && CHAR_Y.equals(cteConcentrator)) {
                    value = COMPLETION_STATUS_03;
                }
                if (!isMetCollegeReq && CHAR_Y.equals(locGradReq) && CHAR_N.equals(cteConcentrator)) {
                    value = COMPLETION_STATUS_03;
                }
            }

            Boolean stdFreeze = (Boolean) data.getPropertyAsJavaType(student, m_fieldStdFreezeData);

            if ((stdFreeze == null || !stdFreeze.booleanValue()) && reportData.isUpdateStudent()) {
                if (!StringUtils.isEmpty(fieldPath) && !StringUtils.isEmpty(value)) {
                    String currentValue = (String) student.getFieldValueByBeanPath(fieldPath);
                    if (!value.equals(currentValue)) {
                        ((MDPTECHOutcomeExportDataEntity) entity).setUpdated();
                        student.setFieldValueByBeanPath(fieldPath, value);
                    }
                }
            } else if (!StringUtils.isEmpty(fieldValue)) {
                value = fieldValue;
            }

            return value;
        }

        private String getPTECHExitStatus(MDPTECHOutcomeExportData data, Student student) {
            String value = null;
            if (StudentManager.isActiveStudent(student.getOrganization1(), student.getEnrollmentStatus())) {
                value = CHAR_0;
            } else {
                Collection<StudentEnrollment> enrollments = data.getStudentEnrollmentsByStd(student);
                if (enrollments != null) {
                    StudentEnrollment latestWithdrawal = enrollments.stream()
                            .filter(enr -> StudentEnrollment.WITHDRAWAL.equals(enr.getEnrollmentType())).findFirst()
                            .orElse(null);
                    if (latestWithdrawal != null) {
                        String withdrawalCode = latestWithdrawal.getEnrollmentCode();
                        if (!StringUtils.isEmpty(withdrawalCode)) {
                            String firstChar = withdrawalCode.substring(0, 1);
                            if (CHAR_C.equals(firstChar) || CHAR_T.equals(firstChar) || CHAR_W.equals(firstChar)) {
                                value = firstChar;
                            }
                        }
                    }
                }
            }
            return value;
        }

        private boolean isCTEConcentratorCourse(Course course) {
            String fieldValue = (String) course.getFieldValueByBeanPath(m_fieldDOECTEConcentrator);
            if (!StringUtils.isEmpty(fieldValue)) {
                fieldValue = lookupStateValue(Course.class, m_fieldDOECTEConcentrator, fieldValue);
            }
            return CHAR_Y.equals(fieldValue);
        }

        private String isPTECHPathwayYearsMatched(Student student, int valueToMatch) {
            Integer stdPTECHPathwayYears =
                    Integer.valueOf((String) student.getFieldValueByBeanPath(m_fieldStdPTECHPathwayYears));
            return stdPTECHPathwayYears != null && valueToMatch == stdPTECHPathwayYears.intValue() ? CHAR_Y : CHAR_N;
        }

    }

    public static final String ALIAS_COLLEGE_REQ = "all-crs-CollegeRequirements";
    public static final String ALIAS_DOE_CTE_CONCENTRATOR = "DOE CTE CONCENTRATOR";
    public static final String ALIAS_DOE_DUAL_ENR = "DOE DUAL ENROLLMENT";
    public static final String ALIAS_FREEZE_DATA_FOR_PTECH = "all-std-FreezeDataforPTECH";
    public static final String ALIAS_PTECH_PATHWAY_YEARS = "all-std-PTECHPathwayYears";
    public static final String ALIAS_STD_GROUP = "all-std-Group";
    public static final String ALIAS_STD_GROUP_YEAR = "all-std-GroupYear";

    private static final String DOE_03_GRADE = "DOE GRADE";
    private static final String DOE_14_ENTRY_STATUS = "DOE ENTRY STATUS";

    public static final String INPUT_PARAM_CONTEXT_OID = "contextOid";
    public static final String INPUT_PARAM_REPORT_TYPE = "reportType";
    public static final String INPUT_PARAM_UPDATE_STUDENT = "updateStudent";

    public static final String VAL_GROUP_PTECH = "PTECH";
    public static final String VAL_REPORT_TYPE_ENROLLMENT = "enrollment";
    public static final String VAL_REPORT_TYPE_FALL_ENROLLMENT = "fallEnrollment";

    protected TreeMap m_gradeLevelMap;
    private Map<String, Collection<StudentEnrollment>> m_enrMap;
    private String m_fieldCrsCollegeReq;
    private String m_fieldCrsFieldDualEnr;
    private String m_fieldDOECTEConcentrator;
    private String m_fieldStdFreezeData;
    private String m_fieldStdGroup;
    private String m_fieldStdGroupYear;
    private String m_fieldStdPTECHPathwayYears;
    private DistrictSchoolYearContext m_reportContext;
    private Map<String, List<Transcript>> m_stdTranscriptsMap;
    private QueryByCriteria m_studentQuery;
    private boolean m_updateStudentIndicator;

    /**
     * Builds a criteria for the students that should be reported.
     *
     * @return Criteria
     */
    @Override
    protected Criteria getReportingCriteria() {
        Criteria activityCriteria = new Criteria();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                getReportContext().getStartDate());
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getReportContext().getEndDate());
        activityCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE,
                Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        Criteria enrollCriteria = new Criteria();

        enrollCriteria = new Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        X2Criteria reportingCriteria = new X2Criteria();

        reportingCriteria.addAndCriteria(enrollCriteria);

        if (isSchoolContext()) {
            reportingCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            reportingCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            reportingCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        List<String> codes = getRefCodesWithStateValue(ALIAS_STD_GROUP, Arrays.asList(VAL_GROUP_PTECH)).stream()
                .map(ReferenceCode::getCode).collect(Collectors.toList());
        if (codes.isEmpty()) {
            codes.add("--NotInList--");
        }
        reportingCriteria.addIn(m_fieldStdGroup, codes);
        reportingCriteria.addNotEmpty(m_fieldStdGroupYear, getBroker().getPersistenceKey());
        reportingCriteria.addLessOrEqualThan(m_fieldStdGroupYear, getReportContext().getSchoolYear());

        return reportingCriteria;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.md.MDStudentReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {

        m_fieldCrsCollegeReq = translateAliasToJavaName(ALIAS_COLLEGE_REQ, true);
        m_fieldCrsFieldDualEnr = translateAliasToJavaName(ALIAS_DOE_DUAL_ENR, true);
        m_fieldStdGroup = translateAliasToJavaName(ALIAS_STD_GROUP, true);
        m_fieldStdGroupYear = translateAliasToJavaName(ALIAS_STD_GROUP_YEAR, true);
        m_fieldDOECTEConcentrator = translateAliasToJavaName(ALIAS_DOE_CTE_CONCENTRATOR, true);
        m_fieldStdFreezeData = translateAliasToJavaName(ALIAS_FREEZE_DATA_FOR_PTECH, true);
        m_fieldStdPTECHPathwayYears = translateAliasToJavaName(ALIAS_PTECH_PATHWAY_YEARS, true);

        super.initialize();

        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        Object parameter = getParameter(INPUT_PARAM_UPDATE_STUDENT);
        m_updateStudentIndicator =
                parameter != null && parameter instanceof Boolean && ((Boolean) parameter).booleanValue() ? true
                        : false;

        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());

        if (getSetupErrors().size() == 0) {

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CreditEarnedRetriever.CALC_ID, new CreditEarnedRetriever());
            calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
            calcs.put(RetrieveReportData.CALC_ID, new RetrieveReportData());
            calcs.put(RetrieveStudentAlias.CALC_ID, new RetrieveStudentAlias());
            super.addCalcs(calcs);


            loadStudentEnrollments(getStudentCriteria());
            m_studentQuery = getStudentQuery();

            setQuery(m_studentQuery);
            setEntityClass(MDPTECHOutcomeExportDataEntity.class);
        }

    }

    /**
     * Gets the ref codes with state value.
     *
     * @param alias String
     * @param includedCodes Collection<String>
     * @return List
     */
    private List<ReferenceCode> getRefCodesWithStateValue(String alias,
                                                          Collection<String> includedCodes) {
        DataDictionary dictionary = this.getDataDictionary();
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        List<ReferenceCode> codesWithStateValue = new ArrayList<ReferenceCode>();
        if (field != null) {
            String refTableOid = field.getReferenceTableOid();
            if (!StringUtils.isEmpty(refTableOid)) {
                Map<String, ReferenceCode> refCodes = getReferenceCodes(refTableOid);
                for (Entry<String, ReferenceCode> refCodeEntry : refCodes.entrySet()) {
                    if (!StringUtils.isEmpty(refCodeEntry.getValue().getStateCode()) &&
                            (includedCodes == null
                                    || includedCodes.contains(refCodeEntry.getValue().getStateCode()))) {
                        codesWithStateValue.add(refCodeEntry.getValue());
                    }
                }
            }
        }
        return codesWithStateValue;
    }

    /**
     * Gets the report context.
     *
     * @return District school year context
     */
    private DistrictSchoolYearContext getReportContext() {
        if (m_reportContext == null) {
            Object parameter = getParameter(INPUT_PARAM_CONTEXT_OID);
            if (parameter != null && parameter instanceof String) {
                m_reportContext = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        (String) parameter);
            }
        }
        return m_reportContext;
    }

    /**
     * Gets Studen Enrollment list by student
     *
     * @return List
     */
    private Collection<StudentEnrollment> getStudentEnrollmentsByStd(Student student) {
        return m_enrMap.get(student.getOid());
    }

    /**
     * Gets Transcript map
     *
     * @return List
     */
    private Map<String, List<Transcript>> getTranscriptMap() {
        if (m_stdTranscriptsMap == null) {
            X2Criteria transcriptCriteria = new X2Criteria();
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentQuery.getCriteria());
            transcriptCriteria.addIn(Transcript.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    stdSubQuery);
            transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getReportContext().getOid());
            if (isSchoolContext()) {
                transcriptCriteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
            }
            QueryByCriteria transcriptQuery = new QueryByCriteria(Transcript.class, transcriptCriteria);
            transcriptQuery.addOrderByAscending(X2BaseBean.COL_OID);
            m_stdTranscriptsMap =
                    getBroker().getGroupedCollectionByQuery(transcriptQuery, Transcript.COL_STUDENT_OID, 100);
        }
        return m_stdTranscriptsMap;
    }

    /**
     * Gets Transcript list by student
     *
     * @return List
     */
    private List<Transcript> getTranscriptsByStd(Student student) {
        return getTranscriptMap().get(student.getOid());
    }

    /**
     * Checks if is update student.
     *
     * @return true, if is update student
     */
    private boolean isUpdateStudent() {
        return m_updateStudentIndicator;
    }

    /**
     * Load collection of students' enrollments keyed on students' oids.
     *
     * @param stdCriteria Criteria
     */
    private void loadStudentEnrollments(Criteria stdCriteria) {
        X2Criteria enrCriteria = new X2Criteria();
        SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);
        enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, stdSubQuery);
        QueryByCriteria enrQuery = new QueryByCriteria(StudentEnrollment.class, enrCriteria);
        enrQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);

        m_enrMap = getBroker().getGroupedCollectionByQuery(enrQuery, StudentEnrollment.COL_STUDENT_OID, 1024);
    }
}
