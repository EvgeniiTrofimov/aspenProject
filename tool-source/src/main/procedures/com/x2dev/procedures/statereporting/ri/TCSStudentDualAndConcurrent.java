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

package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for the TCS Student Dual and Concurrent Export.
 * This class implements the data export for the TCS Student Dual and Concurrent Export.
 *
 * @author X2 Development Corporation
 */
public class TCSStudentDualAndConcurrent extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the TCS Student Dual and Concurrent Export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class TCSStudentDualAndConcurrentEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        /*
         * Cached values for retrievers to share.
         */
        TCSStudentDualAndConcurrent m_tcsData = null;
        List<DualAndConcurrentEnrollmentInfo> m_dualConcurrentInfos = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TCSStudentDualAndConcurrentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            DualAndConcurrentEnrollmentInfo info = getDualConcurrentInfo();
            if (info != null) {
                name += info.getInstitutionCourseName();
            }

            return name;
        }

        /**
         * Returns the schedule info record for the schedule record(s)
         * based in the current row value.
         *
         * @return ScheduleInfo
         */
        public DualAndConcurrentEnrollmentInfo getDualConcurrentInfo() {
            DualAndConcurrentEnrollmentInfo info = null;
            if (m_dualConcurrentInfos != null && getCurrentRow() < m_dualConcurrentInfos.size()
                    && getCurrentRow() >= 0) {
                info = m_dualConcurrentInfos.get(getCurrentRow());
            }
            return info;
        }

        /**
         * Initialize the entity for the student bean provided.
         * The entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            Map<String, DualAndConcurrentEnrollmentInfo> scheduleMap =
                    new HashMap<String, DualAndConcurrentEnrollmentInfo>();
            // Sort records by course view.
            TreeSet<DualAndConcurrentEnrollmentInfo> infos =
                    new TreeSet<DualAndConcurrentEnrollmentInfo>(new Comparator<DualAndConcurrentEnrollmentInfo>() {
                        @Override
                        public int compare(DualAndConcurrentEnrollmentInfo info1,
                                           DualAndConcurrentEnrollmentInfo info2) {
                            return info1.getCourseView().compareTo(info2.getCourseView());
                        }
                    });
            m_tcsData = (TCSStudentDualAndConcurrent) data;
            SisStudent student = (SisStudent) bean;

            Collection<StudentSchedule> schedules = m_tcsData.m_scheduleMap.get(student.getOid());
            Collection<Transcript> transcripts = m_tcsData.m_transcriptMap.get(student.getOid());
            Collection<StudentEnrollment> enrollments = m_tcsData.m_enrollmentMap.get(student.getOid());
            /*
             * Remove student if withdrew from school before the current year's start date
             */
            PlainDate orgStartDate = null;
            if (student != null &&
                    student.getOrganization1() != null &&
                    student.getOrganization1().getCurrentContext() != null &&
                    student.getOrganization1().getCurrentContext().getStartDate() != null) {
                orgStartDate = student.getOrganization1().getCurrentContext().getStartDate();
            }
            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    PlainDate enrollmentDate = enrollment.getEnrollmentDate();
                    String enrollmentType = StringUtils.coalesce(enrollment.getEnrollmentType(), "");
                    if ("W".equals(enrollmentType) &&
                            orgStartDate != null &&
                            enrollmentDate != null &&
                            orgStartDate.after(enrollmentDate)) {
                        setRowCount(0);
                        return;
                    }
                    break;
                }
            } else {
                setRowCount(0);
                return;
            }

            // To save memory at the end of the report, remove the collection from the map as we go.
            // Noted large memory usage of report.
            m_tcsData.m_scheduleMap.remove(student.getOid());
            m_tcsData.m_transcriptMap.remove(student.getOid());

            // Add current schedule sections to the map.
            if (schedules != null) {
                for (StudentSchedule schedule : schedules) {
                    DualAndConcurrentEnrollmentInfo info = new DualAndConcurrentEnrollmentInfo(schedule, m_tcsData);
                    scheduleMap.put(schedule.getSectionOid(), info);
                }
            }

            // try to match schedule to transcript on master schedule oid.
            if (transcripts != null) {
                for (Transcript currentTranscript : transcripts) {
                    /*
                     * For each transcript, find a scheduleMap entry with the same section.
                     * If a TRN record is associated with an SSC record on Section,
                     * the fields on the SSC record are primary (only one record should be included
                     * for a SSC record
                     * and its related TRN record).
                     */
                    Transcript associatedTranscript = null;
                    for (DualAndConcurrentEnrollmentInfo info : scheduleMap.values()) {
                        if (info.getSection().getOid().equals(currentTranscript.getMasterScheduleOid())) {
                            associatedTranscript = currentTranscript;
                            info.setTranscript(associatedTranscript);
                        }
                    }
                    // If there is no associated SSC, create new DualAndConcurrentEnrollmentInfo
                    // from current transcript.
                    if (associatedTranscript == null) {
                        String enrollmentType =
                                (String) currentTranscript.getFieldValueByBeanPath(m_tcsData.m_trnEnrollType);
                        if (m_tcsData.m_enrollmentTypes.contains(enrollmentType)) {
                            DualAndConcurrentEnrollmentInfo info =
                                    new DualAndConcurrentEnrollmentInfo(currentTranscript, m_tcsData);
                            infos.add(info);
                        }
                    }
                }
            }

            infos.addAll(scheduleMap.values());
            m_dualConcurrentInfos = new ArrayList<DualAndConcurrentEnrollmentInfo>(infos);

            /*
             * With m_schedules populated, we now know how many schedule records for this student.
             */
            setRowCount(m_dualConcurrentInfos.size());
            setCurrentRow(0);
        }
    }

    /**
     * A container class for record information for one Dual/Concurrent enrollment.
     *
     * @author X2 Development Corporation
     */
    protected static class DualAndConcurrentEnrollmentInfo {
        private String m_enrollmentType;
        private String m_instCourseCredits;
        private String m_instCourseID;
        private String m_instCourseName;
        private String m_institutionID;
        private MasterSchedule m_section;
        private StudentSchedule m_studentSchedule;
        private Transcript m_transcript;

        /**
         * Instantiates a new dual and concurrent enrollment info.
         *
         * @param studentSchedule StudentSchedule
         * @param data TCSStudentDualAndConcurrent
         */
        public DualAndConcurrentEnrollmentInfo(StudentSchedule studentSchedule, TCSStudentDualAndConcurrent data) {
            m_studentSchedule = studentSchedule;
            m_section = m_studentSchedule.getSection();
            m_enrollmentType = (String) m_studentSchedule.getFieldValueByBeanPath(data.m_sscEnrollType);
            m_instCourseCredits = (String) m_studentSchedule.getFieldValueByBeanPath(data.m_sscInstitutionCrsCredits);
            m_instCourseID = (String) m_studentSchedule.getFieldValueByBeanPath(data.m_sscInstitutionCrsId);
            m_instCourseName = (String) m_studentSchedule.getFieldValueByBeanPath(data.m_sscInstitutionCrsName);
            m_institutionID = (String) m_studentSchedule.getFieldValueByBeanPath(data.m_sscInstitutionId);
            m_institutionID = data.lookupReferenceCodeByBeanPath(StudentSchedule.class,
                    data.m_sscInstitutionId, m_institutionID,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

        }

        /**
         * Instantiates a new dual and concurrent enrollment info.
         *
         * @param transcript Transcript
         * @param data TCSStudentDualAndConcurrent
         */
        public DualAndConcurrentEnrollmentInfo(Transcript transcript, TCSStudentDualAndConcurrent data) {
            m_transcript = transcript;
            m_section = m_transcript.getMasterSchedule();
            m_enrollmentType = (String) m_transcript.getFieldValueByBeanPath(data.m_trnEnrollType);
            m_instCourseCredits = (String) m_transcript.getFieldValueByBeanPath(data.m_trnInstitutionCrsCredits);
            m_instCourseID = (String) m_transcript.getFieldValueByBeanPath(data.m_trnInstitutionCrsId);
            m_instCourseName = (String) m_transcript.getFieldValueByBeanPath(data.m_trnInstitutionCrsName);
            m_institutionID = (String) m_transcript.getFieldValueByBeanPath(data.m_trnInstitutionId);
            m_institutionID = data.lookupReferenceCodeByBeanPath(Transcript.class,
                    data.m_trnInstitutionId, m_institutionID,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
        }

        /**
         * Return the name of the section for use in sorting.
         *
         * @return String
         */
        protected String getCourseView() {
            String name = "";
            if (m_section != null) {
                name = m_section.getCourseView();
            } else {
                name += m_enrollmentType + m_instCourseCredits + m_instCourseID + m_instCourseName + m_institutionID;
            }
            return name;
        }

        /**
         * Gets the enroll type.
         *
         * @return String
         */
        protected String getEnrollType() {
            return m_enrollmentType;
        }

        /**
         * Gets the institution course credits.
         *
         * @return String
         */
        protected String getInstitutionCourseCredits() {
            return m_instCourseCredits;
        }

        /**
         * Gets the institution course ID.
         *
         * @return String
         */
        protected String getInstitutionCourseID() {
            return m_instCourseID;
        }

        /**
         * Gets the institution course name.
         *
         * @return String
         */
        protected String getInstitutionCourseName() {
            return m_instCourseName;
        }

        /**
         * Gets the institution ID.
         *
         * @return String
         */
        protected String getInstitutionID() {
            return m_institutionID;
        }

        /**
         * Gets the section.
         *
         * @return Master schedule
         */
        protected MasterSchedule getSection() {
            return m_section;
        }

        /**
         * Gets the transcript.
         *
         * @return Transcript
         */
        protected Transcript getTranscript() {
            return m_transcript;
        }

        /**
         * Sets the transcript.
         *
         * @param transcript void
         */
        protected void setTranscript(Transcript transcript) {
            m_transcript = transcript;
        }
    }

    /**
     * Returns the district code or school code for the given class section.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDistrictSchool implements FieldRetriever {
        private final String PARAM_DISTRICT = "DISTRICT";
        private final String PARAM_SCHOOL = "SCHOOL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            // Check the student adjusted school code.
            Object value = null;
            String param = (String) field.getParameter();
            DualAndConcurrentEnrollmentInfo info = ((TCSStudentDualAndConcurrentEntity) entity).getDualConcurrentInfo();

            if (PARAM_DISTRICT.equals(param)) {
                if (info.getSection() == null) {
                    String adjustedDistrCode =
                            (String) info.getTranscript().getSchool().getFieldValueByBeanPath(m_sklFieldAdjDistr);
                    value = !StringUtils.isEmpty(adjustedDistrCode) ? adjustedDistrCode
                            : info.getTranscript().getSchool().getOrganization1()
                                    .getFieldValueByBeanPath(m_districtIdField);
                } else {
                    String adjustedDistrCode = (String) info.getSection().getSchedule().getSchool()
                            .getFieldValueByBeanPath(m_sklFieldAdjDistr);
                    value = !StringUtils.isEmpty(adjustedDistrCode) ? adjustedDistrCode
                            : info.getSection().getSchedule().getSchool().getOrganization1()
                                    .getFieldValueByBeanPath(m_districtIdField);
                }
            } else if (PARAM_SCHOOL.equals(param)) {
                value = info.getSection() == null
                        ? info.getTranscript().getSchool().getFieldValueByBeanPath(m_sklIdField)
                        : info.getSection().getSchedule().getSchool().getFieldValueByBeanPath(m_sklIdField);
            }

            return value;
        }
    }

    /**
     * Returns data about Dual\Concurrent enrollment in PS institutions.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDualConcurrentData implements FieldRetriever {
        private final String PARAM_ENR_TYPE = "ENR-TYPE";
        private final String PARAM_INST_ID = "INST-ID";
        private final String PARAM_INST_CRS_ID = "INST-CRS-ID";
        private final String PARAM_INST_CRS_NAME = "INST-CRS-NAME";
        private final String PARAM_INST_CRS_CREDITS = "INST-CRS-CRED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            DualAndConcurrentEnrollmentInfo info = ((TCSStudentDualAndConcurrentEntity) entity).getDualConcurrentInfo();

            if (PARAM_ENR_TYPE.equals(param)) {
                value = info.getEnrollType();
            } else if (PARAM_INST_ID.equals(param)) {
                value = info.getInstitutionID();
            } else if (PARAM_INST_CRS_ID.equals(param)) {
                value = info.getInstitutionCourseID();
            } else if (PARAM_INST_CRS_NAME.equals(param)) {
                value = info.getInstitutionCourseName();
            } else if (PARAM_INST_CRS_CREDITS.equals(param)) {
                try {
                    value = new BigDecimal(info.getInstitutionCourseCredits());
                } catch (Exception e) {
                    // String representation of number is expected.
                }
            }

            return value;
        }
    }

    /**
     * Returns credit value for the section course.
     *
     * This returns a String object.
     * The field definition should use a character
     * formatter to format the value appropriately.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSection implements FieldRetriever {
        private final String PARAM_CREDIT = "CREDIT";
        private final String PARAM_GRADE_LETTER = "LETTER_GRADE";
        private final String PARAM_GRADE_NUMERIC = "NUMERIC_GRADE";
        private final String PARAM_ID = "ID";

        private GradesManager m_gradesManager;
        private Map<String, GradeScale> m_gradeScales;

        /**
         * Constructor. Initialize GradesManager and grade scales map
         */
        public RetrieveSection() {
            m_gradesManager = new GradesManager(getBroker());

            m_gradeScales = new HashMap<String, GradeScale>();
            X2Criteria gradeScaleCriteria = new X2Criteria();
            gradeScaleCriteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                    Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
            BeanQuery gradeScaleQuery = new BeanQuery(TranscriptColumnDefinition.class, gradeScaleCriteria);
            Collection<TranscriptColumnDefinition> tcds = getBroker().getCollectionByQuery(gradeScaleQuery);
            for (TranscriptColumnDefinition tcd : tcds) {
                m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
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
            DualAndConcurrentEnrollmentInfo info = ((TCSStudentDualAndConcurrentEntity) entity).getDualConcurrentInfo();
            MasterSchedule section = info.getSection();
            Transcript transcript = info.getTranscript();
            if (PARAM_ID.equals(param)) {
                value = section == null ? null : section.getCourseView();
            } else if (PARAM_GRADE_LETTER.equals(param)) {
                if (transcript != null) {
                    String finalGrade = transcript.getFinalGrade();
                    value = finalGrade;

                    GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                    String letterGrade = null;

                    if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                        BigDecimal numericGradeBD = m_gradesManager.getNumericValue(finalGrade, scale,
                                transcript.getSchool(), transcript.getSchoolCourseOid());
                        if (numericGradeBD != null) {
                            letterGrade = finalGrade;
                        }

                        if (letterGrade == null) {
                            // Try the final grade as a number.
                            BigDecimal gradeAsNumber = null;
                            try {
                                gradeAsNumber = new BigDecimal(finalGrade);
                            } catch (NumberFormatException nfe) {
                                // nothing. The grade is not numeric.
                            }

                            if (gradeAsNumber != null) {
                                letterGrade = m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                        transcript.getSchool(), transcript.getSchoolCourseOid());
                            }
                        }

                        if (!StringUtils.isEmpty(letterGrade)) {
                            value = lookupStateValue(GradeScaleGradeDefinition.class,
                                    GradeScaleGradeDefinition.COL_GRADE_CODE,
                                    letterGrade);
                        }
                    }
                }
            } else if (PARAM_GRADE_NUMERIC.equals(param)) {
                if (transcript != null) {
                    String finalGrade = transcript.getFinalGrade();
                    GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());

                    if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                        // If the final grade is a letter value, convert it to a number.
                        value = m_gradesManager.getNumericValue(finalGrade, scale,
                                transcript.getSchool(), transcript.getSchoolCourseOid());

                        // Otherwise try to convert the finalGrade to a BigDecimal.
                        if (value == null) {
                            try {
                                if (finalGrade.length() > 5) {
                                    finalGrade = finalGrade.substring(0, 5);
                                }
                                value = new BigDecimal(finalGrade);
                            } catch (NumberFormatException nfe) {
                                // nothing. The grade is not numeric.
                            }
                        } else {
                            String valueAsString = value + "";
                            if (valueAsString.length() > 5) {
                                value = new BigDecimal(valueAsString.substring(0, 5));
                            }
                        }
                    }
                }
            } else if (PARAM_CREDIT.equals(param)) {
                if (transcript != null) {
                    String psCreditsEarned = (String) transcript.getFieldValueByBeanPath(m_trnCreditsEarned);
                    if (psCreditsEarned != null) {
                        BigDecimal totalCredit = new BigDecimal(psCreditsEarned);
                        value = totalCredit;
                    }
                }
            }

            return value;
        }

    }

    /**
     * "Credit" can either be 0.00 or equal to "Institution Crs Cred"
     */
    protected class ValidateCredits implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            TCSStudentDualAndConcurrentEntity dcEntity = (TCSStudentDualAndConcurrentEntity) entity;
            String instCourseCredits = dcEntity.getDualConcurrentInfo().getInstitutionCourseCredits();
            if (!"0.00".equals(value) &&
                    ((instCourseCredits != null && !instCourseCredits.equals(value)) ||
                            (instCourseCredits == null && value != null))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Credit is not 0.00 and not equal to Institution Crs Cred",
                        "Credit = " + STYLE_BOLD + String.valueOf(value) + STYLE_END +
                                ", Institution Crs Cred = " + STYLE_BOLD + String.valueOf(instCourseCredits)
                                + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "requireSced" parameter. The value is an Boolean.
     */
    public static final String REQUIRE_SCED_PARAM = "requireSced";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the active students only parameter. The value is a boolean.
     */
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";

    /**
     * Include State Exclusion. The value is a boolean.
     */
    public static final String INCLUDE_STATE_EXCLUSION = "includeStateExclusion";

    /**
     * Alias fields
     */
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_SECTION = "DOE EXCLUDE MST";
    protected static final String ALIAS_SCED_CODE = "RI Course ID";

    protected static final String ALIAS_SSC_PSENROLLTYPE = "all-ssc-PSEnrollmentType";
    protected static final String ALIAS_SSC_INSTITUTIONID = "all-ssc-PSInstitutionId";
    protected static final String ALIAS_SSC_INSTITUTIONCOURSEID = "all-ssc-PSInstitutionCourseId";
    protected static final String ALIAS_SSC_INSTITUTCOURSENAME = "all-ssc-PSInstitutionCourseName";
    protected static final String ALIAS_SSC_INSTCOURSECREDITS = "all-ssc-PSInstitutionCourseCredit";

    protected static final String ALIAS_TRN_PSCREDTSEARNED = "all-trn-PSCredtsEarned";
    protected static final String ALIAS_TRN_PSENROLLTYPE = "all-trn-PSEnrollmentType";
    protected static final String ALIAS_TRN_INSTITUTIONID = "all-trn-PSInstitutionId";
    protected static final String ALIAS_TRN_INSTITUTIONCOURSEID = "all-trn-PSInstitutionCourseId";
    protected static final String ALIAS_TRN_INSTITUTCOURSENAME = "all-trn-PSInstitutionCourseName";
    protected static final String ALIAS_TRN_INSTCOURSECREDITS = "all-trn-PSInstitutionCourseCredit";


    public String m_sscEnrollType;
    public String m_sscInstitutionId;
    public String m_sscInstitutionCrsId;
    public String m_sscInstitutionCrsName;

    public String m_sscInstitutionCrsCredits;

    public String m_trnCreditsEarned;
    public String m_trnEnrollType;
    public String m_trnInstitutionId;
    public String m_trnInstitutionCrsId;
    public String m_trnInstitutionCrsName;
    public String m_trnInstitutionCrsCredits;

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected X2Criteria m_enrollmentCriteria;
    protected Map<String, Collection<StudentEnrollment>> m_enrollmentMap;
    protected Set<String> m_enrollmentTypes;
    protected String m_excludeCourseField;
    protected String m_excludeSectionField;
    protected String m_excludeStudentField;
    protected boolean m_includeStateExclusion;
    protected String m_orgFieldStr;
    protected String m_orgOid;
    protected PlainDate m_reportDate;
    protected boolean m_sasidStudentsOnly;
    protected X2Criteria m_scheduleCriteria;
    protected Map<String, Collection<StudentSchedule>> m_scheduleMap;
    protected Map<String, Collection<Transcript>> m_transcriptMap;

    /**
     * Returns the heading with an end of line character appended.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        String heading = super.getHeading();
        if (heading != null && !StringUtils.isEmpty(heading) && !heading.endsWith("\n")) {
            heading += "\r\n";
        }
        return heading;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        // Load initialization data
        initializeFields();
        initEnrollmentTypes();
        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Load a map of student transcript records my student.
            // Must load before student criteria is generated. That depends on schedule and change
            // criteria.
            loadStudentSchedules();
            loadTranscripts();

            loadEnrollments();

            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort == null ? 0 : sort.intValue()) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(TCSStudentDualAndConcurrentEntity.class);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TCS-DISTRICT-SCHOOL", new RetrieveDistrictSchool());
            calcs.put("TCS-SECTION", new RetrieveSection());
            calcs.put("TCS-DC", new RetrieveDualConcurrentData());

            addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("VAL-CREDITS", new ValidateCredits());

            addValidators(validators);
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER +
                RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(StudentSchedule.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Gets a list of the master schedules that are used for post-secondary credit.
     *
     * @return Sets the
     */
    private Set<String> getMasterSchedules() {
        Set<String> values = new HashSet();
        for (Entry<String, Collection<StudentSchedule>> entry : m_scheduleMap.entrySet()) {
            for (StudentSchedule item : entry.getValue()) {
                values.add(item.getSectionOid());
            }
        }
        return values;
    }

    /**
     * Builds a criteria for the students that should be reported.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addIn(X2BaseBean.COL_OID, m_scheduleMap.keySet());
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIn(X2BaseBean.COL_OID, m_transcriptMap.keySet());

        primaryCriteria.addOrCriteria(orCriteria);

        return primaryCriteria;
    }

    /**
     * Build the criteria to query transcripts.
     *
     * @param isSectionRequired boolean
     * @return X 2 criteria
     */
    private X2Criteria getTranscriptCriteria(boolean isSectionRequired) {
        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        // check school or organization selection.
        if (isSchoolContext()) {
            transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check include exclusion flag as well as exclusion flags for student, section and student
        // schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            transcriptCriteria.addNotEmpty(Transcript.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        // Check if the section exclusion custom field is present.
        if (isSectionRequired && !StringUtils.isEmpty(m_excludeSectionField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    m_excludeSectionField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (isSectionRequired && !StringUtils.isEmpty(m_excludeCourseField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (isSectionRequired && requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                transcriptCriteria.addNotEmpty(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        // Add user entered selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // YOG
                transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(transcriptCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }
        return transcriptCriteria;
    }

    /**
     * Find the set of post-secondary enrollment types.
     */
    private void initEnrollmentTypes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField enrollTypeField = dictionary.findDataDictionaryField(StudentSchedule.class.getName(),
                m_sscEnrollType);
        m_enrollmentTypes = enrollTypeField.getReferenceTable().getCodeMap().keySet();
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        // Lookup potential exclude fields. Not required.
        m_excludeCourseField = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, false);
        m_excludeStudentField = translateAliasToJavaName(ALIAS_EXCLUDE_STUDENT, false);
        m_excludeSectionField = translateAliasToJavaName(ALIAS_EXCLUDE_SECTION, false);

        m_sasidStudentsOnly = true;
        Boolean sasidStudentsOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentsOnly != null) {
            m_sasidStudentsOnly = sasidStudentsOnly.booleanValue();
        }
        m_includeStateExclusion = true;
        Boolean includeStateExclusion = (Boolean) getParameter(INCLUDE_STATE_EXCLUSION);
        if (includeStateExclusion != null) {
            m_includeStateExclusion = includeStateExclusion.booleanValue();
        }

        m_sscEnrollType = translateAliasToJavaName(ALIAS_SSC_PSENROLLTYPE, true);
        m_sscInstitutionId = translateAliasToJavaName(ALIAS_SSC_INSTITUTIONID, true);
        m_sscInstitutionCrsId = translateAliasToJavaName(ALIAS_SSC_INSTITUTIONCOURSEID, true);
        m_sscInstitutionCrsName = translateAliasToJavaName(ALIAS_SSC_INSTITUTCOURSENAME, true);
        m_sscInstitutionCrsCredits = translateAliasToJavaName(ALIAS_SSC_INSTCOURSECREDITS, true);

        m_trnEnrollType = translateAliasToJavaName(ALIAS_TRN_PSENROLLTYPE, true);
        m_trnInstitutionId = translateAliasToJavaName(ALIAS_TRN_INSTITUTIONID, true);
        m_trnInstitutionCrsId = translateAliasToJavaName(ALIAS_TRN_INSTITUTIONCOURSEID, true);
        m_trnInstitutionCrsName = translateAliasToJavaName(ALIAS_TRN_INSTITUTCOURSENAME, true);
        m_trnInstitutionCrsCredits = translateAliasToJavaName(ALIAS_TRN_INSTCOURSECREDITS, true);
        m_trnCreditsEarned = translateAliasToJavaName(ALIAS_TRN_PSCREDTSEARNED, true);
    }

    /**
     * Loads a map of sets of Student enrollment records by studentOid for students in the export.
     */
    private void loadEnrollments() {
        m_enrollmentCriteria = new X2Criteria();

        // check school or organization selection.
        if (isSchoolContext()) {
            m_enrollmentCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL,
                    getSchool().getOid());
        } else if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
            m_enrollmentCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    m_orgFieldStr,
                    m_orgOid);
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        } else {
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check include exclusion falg and exclusion flags for student, section and student
        // schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            m_enrollmentCriteria.addNotEmpty(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        X2Criteria matchingStudentCriteria = new X2Criteria();
        matchingStudentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, m_scheduleMap.keySet());
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, m_transcriptMap.keySet());
        matchingStudentCriteria.addOrCriteria(orCriteria);
        m_enrollmentCriteria.addAndCriteria(matchingStudentCriteria);


        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, m_enrollmentCriteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);

        m_enrollmentMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 500);
    }

    /**
     * Load a map of sets of student schedule records by student oid for the students in the export.
     * NOTE : Do not exclude courses based on [DOE EXCLUDE CRS] or [DOE EXCLUDE MST]. Instructions
     * to districts
     * require inclusion of all records with [all.trn.PSEnrollmentType].
     */
    private void loadStudentSchedules() {
        m_scheduleCriteria = new X2Criteria();

        // From active Schedule
        m_scheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Only with valid value in [DOE SSC PSENROLLTYPE]
        m_scheduleCriteria.addIn(m_sscEnrollType, m_enrollmentTypes);

        // Check include exclusion flag as well as exclusion flags for student, section and student
        // schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            m_scheduleCriteria.addNotEmpty(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        // Add user entered selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // YOG
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(m_scheduleCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, m_scheduleCriteria);
        m_scheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Loads a map of sets of transcript records by studentOid for students in the export.
     * In certain condition transcripts could be duplicated in collection for a single student
     */
    private void loadTranscripts() {
        // load matching transcripts
        X2Criteria transcriptCriteria = getTranscriptCriteria(true);
        transcriptCriteria.addIn(Transcript.COL_MASTER_SCHEDULE_OID, getMasterSchedules());
        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, m_scheduleMap.keySet());

        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        query.addOrderBy(Transcript.COL_STUDENT_OID, true);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500);

        // load post-secondary transcripts
        transcriptCriteria = getTranscriptCriteria(false);
        // Only with valid value in [DOE TRN PSENROLLTYPE]
        transcriptCriteria.addIn(m_trnEnrollType, m_enrollmentTypes);
        query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        query.addOrderBy(Transcript.COL_STUDENT_OID, true);
        m_transcriptMap.putAll(getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500));
    }
}
