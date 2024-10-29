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
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
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
public class MDPTECHExportData extends MDStudentReportData {

    /**
     * The Class HighSchoolDataCollectionEntity.
     */
    public static class MDPTECHExportDataEntity extends MDStudentReportEntity {

        private MDPTECHExportData m_data;
        private EnrollmentSnapshot m_snapshot = null;
        private List<SchoolCourse> m_stdCourseList;
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
            SisStudent std = (SisStudent) bean;
            m_data = (MDPTECHExportData) data;
            PlainDate endSnapshotDate = null;
            if (m_data.m_reportType.equals(VAL_REPORT_TYPE_ENROLLMENT)) {
                List<Transcript> stdTranscriptList = m_data.getTranscriptsByStd(std);
                if (stdTranscriptList == null) {
                    setRowCount(0);
                } else {
                    setRowCount(stdTranscriptList.size());
                    m_stdCourseList = stdTranscriptList.stream().map(trn -> trn.getSchoolCourse())
                            .collect(Collectors.toList());
                }
                endSnapshotDate = m_data.getReportContext().getEndDate();
            } else {
                List<StudentSchedule> stdSchList = m_data.getStudentScheduleMap().get(std.getOid());
                if (stdSchList == null) {
                    setRowCount(0);
                } else {
                    setRowCount(stdSchList.size());
                    m_stdCourseList = stdSchList.stream().map(sch -> sch.getSection().getSchoolCourse())
                            .collect(Collectors.toList());
                }
                endSnapshotDate = m_data.m_reportDate;
            }
            m_snapshot = getSnapshot(std, endSnapshotDate, data.getFieldDefinition(DOE_14_ENTRY_STATUS));

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
         * Gets the Course record.
         *
         * @return Course
         */
        public SchoolCourse getCurrentCourse() {
            return m_stdCourseList != null ? m_stdCourseList.get(getCurrentRow()) : null;
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
            return getCurrentCourse().getSchool();
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
     * Field retriever for Course details.
     */
    protected class CourseDetailsRetriever implements FieldRetriever {

        protected static final String CALC_ID = "CRS-DETAIL";

        private static final String PARAM_SCED = "CRS-SCED";
        private static final String PARAM_NUMBER = "CRS-NUMBER";

        private String m_fieldScedCode;

        /**
         * Instantiates a new retrieve course details.
         */
        public CourseDetailsRetriever() {
            m_fieldScedCode = translateAliasToJavaName(ALIAS_SCED_CODE, true);
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
            String param = (String) field.getParameter();
            Course course = ((MDPTECHExportDataEntity) entity).getCurrentCourse().getCourse();
            if (course != null) {
                if (PARAM_SCED.equals(param)) {
                    value = lookupStateValue(Course.class, m_fieldScedCode,
                            (String) course.getFieldValueByBeanPath(m_fieldScedCode));
                } else if (PARAM_NUMBER.equals(param)) {
                    value = course.getNumber();
                }
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
            EnrollmentSnapshot snapshot = ((MDPTECHExportDataEntity) entity).getSnapshot();
            SisStudent student = (SisStudent) entity.getBean();
            int schoolYear = ((MDPTECHExportData) data).getReportContext().getSchoolYear();

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
     * Field retriever for PTECH Year.
     */
    protected class RetrievePTECHYear implements FieldRetriever {

        public static final String CALC_ID = "PTECH-YEAR";

        private static final String ALIAS_FREEZE_DATA_FOR_PTECH = "all-std-FreezeDataforPTECH";
        private static final String ALIAS_PTECH_YEAR = "all-std-PTECHYear";

        private String m_fieldStudentFreezeData;
        private String m_fieldStudentPTECHYear;

        /**
         * Instantiates a new retrieve P-TECH Year.
         */
        public RetrievePTECHYear() {
            m_fieldStudentFreezeData =
                    MDPTECHExportData.this.translateAliasToJavaName(ALIAS_FREEZE_DATA_FOR_PTECH, true);
            m_fieldStudentPTECHYear = MDPTECHExportData.this.translateAliasToJavaName(ALIAS_PTECH_YEAR, true);
        }

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
            MDPTECHExportData reportData = (MDPTECHExportData) data;
            if (!StringUtils.isEmpty(m_fieldStudentPTECHYear)) {
                SisStudent student = (SisStudent) entity.getBean();

                String groupYear = (String) student.getFieldValueByBeanPath(reportData.m_fieldStdGroupYear);

                String fieldValue = (String) student.getFieldValueByBeanPath(m_fieldStudentPTECHYear);

                Boolean stdFreeze = (Boolean) data.getPropertyAsJavaType(student, m_fieldStudentFreezeData);

                if (!StringUtils.isEmpty(groupYear)) {
                    Integer groupYearInt = Integer.parseInt(groupYear);
                    value = String.valueOf(reportData.getReportContext().getSchoolYear() - groupYearInt + 1);
                }

                if ((stdFreeze == null || !stdFreeze.booleanValue()) && reportData.isUpdateStudent()) {
                    if (!StringUtils.isEmpty(value) && !value.equals(fieldValue)) {
                        ((MDPTECHExportDataEntity) entity).setUpdated();
                        student.setFieldValueByBeanPath(m_fieldStudentPTECHYear, value);
                    }
                } else if (!StringUtils.isEmpty(fieldValue)) {
                    value = fieldValue;
                }
            }
            return value;
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
                value = ((MDPTECHExportData) data).m_reportDate;
            }
            return value;
        }
    }

    private static final String ALIAS_PTECH_QUALIFYING_COURSE = "all-crs-PTECHQualifyingCourse";
    private static final String ALIAS_SCED_CODE = "DOE SCED CODE";
    private static final String ALIAS_STD_GROUP = "all-std-Group";
    private static final String ALIAS_STD_GROUP_YEAR = "all-std-GroupYear";

    private static final String DOE_03_GRADE = "DOE GRADE";
    private static final String DOE_14_ENTRY_STATUS = "DOE ENTRY STATUS";

    private static final String INPUT_PARAM_CONTEXT_OID = "contextOid";
    private static final String INPUT_PARAM_REPORT_TYPE = "reportType";
    private static final String INPUT_PARAM_UPDATE_STUDENT = "updateStudent";

    private static final String VAL_GROUP_PTECH = "PTECH";
    private static final String VAL_REPORT_TYPE_ENROLLMENT = "enrollment";

    protected TreeMap m_gradeLevelMap;
    private String m_fieldPTECHQualifyingCours;
    private String m_fieldStdGroup;
    private String m_fieldStdGroupYear;
    private DistrictSchoolYearContext m_reportContext;
    private String m_reportType;
    private Map<String, List<StudentSchedule>> m_stdStudentScheduleMap;
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

        X2Criteria studentEnrOrActiveCriteria = new X2Criteria();
        studentEnrOrActiveCriteria.addOrCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        studentEnrOrActiveCriteria.addOrCriteria(enrollCriteria);

        X2Criteria reportingCriteria = new X2Criteria();

        reportingCriteria.addAndCriteria(studentEnrOrActiveCriteria);

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

        m_fieldPTECHQualifyingCours = translateAliasToJavaName(ALIAS_PTECH_QUALIFYING_COURSE, true);
        m_fieldStdGroup = translateAliasToJavaName(ALIAS_STD_GROUP, true);
        m_fieldStdGroupYear = translateAliasToJavaName(ALIAS_STD_GROUP_YEAR, true);

        super.initialize();

        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        Object parameter = getParameter(INPUT_PARAM_UPDATE_STUDENT);
        m_updateStudentIndicator =
                parameter != null && parameter instanceof Boolean && ((Boolean) parameter).booleanValue() ? true
                        : false;

        m_reportType = (String) getParameter(INPUT_PARAM_REPORT_TYPE);

        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());

        if (getSetupErrors().size() == 0) {

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
            calcs.put(RetrieveReportData.CALC_ID, new RetrieveReportData());
            calcs.put(CourseDetailsRetriever.CALC_ID, new CourseDetailsRetriever());
            calcs.put(RetrievePTECHYear.CALC_ID, new RetrievePTECHYear());
            super.addCalcs(calcs);

            m_studentQuery = getStudentQuery();

            setQuery(m_studentQuery);
            setEntityClass(MDPTECHExportDataEntity.class);
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
     * Gets Course map
     *
     * @return List
     */
    private Map<String, List<StudentSchedule>> getStudentScheduleMap() {
        if (m_stdStudentScheduleMap == null) {
            Criteria courseCriteria = new Criteria();
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentQuery.getCriteria());
            courseCriteria.addIn(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    stdSubQuery);
            courseCriteria.addEqualTo(
                    StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.REL_SCHEDULE
                            + ModelProperty.PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                    getReportContext().getOid());
            courseCriteria.addEqualTo(
                    StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE
                            + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER
                            + m_fieldPTECHQualifyingCours,
                    BooleanAsStringConverter.TRUE);
            courseCriteria.addLessOrEqualThan(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER
                    + MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER
                    + ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER
                    + ScheduleTermDate.COL_START_DATE, m_reportDate);
            courseCriteria
                    .addGreaterOrEqualThan(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER
                            + MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER
                            + ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER
                            + ScheduleTermDate.COL_END_DATE, m_reportDate);
            QueryByCriteria courseQuery = new QueryByCriteria(StudentSchedule.class, courseCriteria);
            courseQuery.addOrderByAscending(X2BaseBean.COL_OID);
            m_stdStudentScheduleMap =
                    getBroker().getGroupedCollectionByQuery(courseQuery, StudentSchedule.COL_STUDENT_OID, 100);
        }
        return m_stdStudentScheduleMap;
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
            transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldPTECHQualifyingCours,
                    BooleanAsStringConverter.TRUE);
            transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getReportContext().getOid());
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
}
