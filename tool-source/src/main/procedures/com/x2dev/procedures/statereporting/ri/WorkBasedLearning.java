/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.ParameterSelectionHandler;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
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
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.beans.StudentScheduleChange;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for Work Based Learning export.
 *
 * @author Follett Software Company
 */
public class WorkBasedLearning extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Work Based Learning export.
     *
     * @author Follett Software Company
     */
    public static class WorkBasedLearningEntity extends StateReportEntity {

        /**
         * List of DTO
         */
        List<ReportEntityDTO> m_entityDTOList;

        /**
         * WorkBasedLearning object
         */
        WorkBasedLearning m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WorkBasedLearningEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets current DTO.
         *
         * @return ReportEntityDTO
         */
        public ReportEntityDTO getCurrentDto() {
            return m_entityDTOList.get(getCurrentRow());
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
            return name;
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (WorkBasedLearning) data;
            SisStudent student = (SisStudent) bean;

            List<StudentScheduleSpan> scheduleSpans = m_data.m_helper.getStudentScheduleSpans(student);

            m_entityDTOList = getDTOListFromScheduleSpans(scheduleSpans);

            Collection<StudentAssessment> assessments = m_data.m_assesmentByStdMap.get(student.getOid());
            if (assessments != null && !assessments.isEmpty()) {
                for (StudentAssessment asm : assessments) {
                    m_entityDTOList.add(m_data.new ReportEntityDTO(asm));
                }
            }

            String stdWBL = (String) student.getFieldValueByBeanPath(m_data.m_stdWBLTypeField);
            if (!StringUtils.isEmpty(stdWBL)) {
                m_entityDTOList.add(m_data.new ReportEntityDTO(student));
            }

            setRowCount(m_entityDTOList.size());

        }

        /**
         * Converts list of StudentScheduleSpan into list of ReportEntityDTO.
         *
         * @param scheduleSpans List of StudentScheduleSpan
         * @return List of ReportEntityDTO
         */
        private List<ReportEntityDTO> getDTOListFromScheduleSpans(List<StudentScheduleSpan> scheduleSpans) {
            List<ReportEntityDTO> filteredSpans = scheduleSpans.stream()
                    .map(span -> spanToDto(span))
                    .filter(span -> span != null)
                    .collect(Collectors.toList());
            Map<String, List<ReportEntityDTO>> spanMap = new HashMap<>();
            for (ReportEntityDTO span : filteredSpans) {
                String key = span.m_section + ModelProperty.PATH_DELIMITER + span.m_sector
                        + ModelProperty.PATH_DELIMITER + span.m_type;
                List<ReportEntityDTO> recordsForSectorAndType = spanMap.get(key);
                if (recordsForSectorAndType == null) {
                    recordsForSectorAndType = new ArrayList<>();
                    spanMap.put(key, recordsForSectorAndType);
                }
                recordsForSectorAndType.add(span);
            }
            List<ReportEntityDTO> reportSpans = new ArrayList<>();
            for (Map.Entry<String, List<ReportEntityDTO>> entry : spanMap.entrySet()) {
                List<ReportEntityDTO> recordsForSectorAndType = entry.getValue();
                ReportEntityDTO greatestActive = null;
                ReportEntityDTO greatestInactive = null;
                for (ReportEntityDTO recordToCompare : recordsForSectorAndType) {
                    if (recordToCompare.m_scc == null) {
                        if (greatestActive == null || greatestActive.m_hours.compareTo(recordToCompare.m_hours) <= 0) {
                            greatestActive = recordToCompare;
                        }
                    } else {
                        if (greatestInactive == null
                                || greatestInactive.m_hours.compareTo(recordToCompare.m_hours) <= 0) {
                            greatestInactive = recordToCompare;
                        }
                    }
                }
                if (greatestActive != null) {
                    reportSpans.add(greatestActive);
                } else if (greatestInactive != null) {
                    reportSpans.add(greatestInactive);
                }
            }
            return reportSpans;
        }

        /**
         * Checks if schedule record is active.
         *
         * @param scheduleSpan
         * @return boolean
         */
        private boolean spanIsActive(StudentScheduleSpan scheduleSpan) {
            boolean value = false;
            if (scheduleSpan.getSchedule() != null) {
                String hours = (String) scheduleSpan.getSchedule().getFieldValueByBeanPath(m_data.m_sscWBLHoursField);
                value = scheduleSpan != null && scheduleSpan.getExitChange() == null
                        && !StringUtils.isEmpty(hours);
            }
            return value;
        }

        /**
         * Check if schedule record is dropped
         *
         * @param scheduleSpan
         * @return boolean
         */
        private boolean spanIsValidDropped(StudentScheduleSpan scheduleSpan) {
            if (scheduleSpan == null || scheduleSpan.getExitChange() == null) {
                return false;
            }
            StudentScheduleChange scc = scheduleSpan.getExitChange();
            String hours = (String) scc.getFieldValueByBeanPath(m_data.m_sccWBLHoursField);
            return StudentScheduleChange.CODE_DROP.equals(scc.getChangeTypeCode())
                    && !StringUtils.isEmpty((String) scc.getFieldValueByBeanPath(m_data.m_sccWBLHoursField))
                    && !StringUtils.isEmpty(hours);
        }

        /**
         * Converts single StudentScheduleSpan into ReportEntityDTO.
         *
         * @param scheduleSpan
         * @return ReportEntityDTO
         */
        private ReportEntityDTO spanToDto(StudentScheduleSpan scheduleSpan) {
            if (spanIsActive(scheduleSpan)) {
                return m_data.new ReportEntityDTO(scheduleSpan.getSchedule());
            }
            if (spanIsValidDropped(scheduleSpan)) {
                return m_data.new ReportEntityDTO(scheduleSpan.getExitChange());
            }
            return null;
        }
    }

    /**
     * Retrieve the WBL data from current DTO item.
     *
     * @author Follett Software Company
     */
    protected class RetrieveWBLData implements FieldRetriever {

        public static final String CALC_ID = "WBL-DATA";
        private static final String PARAM_HOURS = "HOURS";
        private static final String PARAM_HOURS_PAID = "HOURS-PAID";
        private static final String PARAM_LOCAL_SECTION_ID = "LOCAL-SECTION-ID";
        private static final String PARAM_PARTNER = "PARTNER";
        private static final String PARAM_SCHCODE = "SCHCODE";
        private static final String PARAM_SECTOR = "SECTOR";
        private static final String PARAM_SETTING = "SETTING";
        private static final String PARAM_TYPE = "TYPE";

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
            WorkBasedLearningEntity wblEntity = (WorkBasedLearningEntity) entity;
            String param = (String) field.getParameter();
            ReportEntityDTO dto = wblEntity.getCurrentDto();
            Object value = null;
            if (PARAM_HOURS.equals(param)) {
                value = dto.m_hours;
            } else if (PARAM_HOURS_PAID.equals(param)) {
                value = dto.m_hoursPaid;
            } else if (PARAM_PARTNER.equals(param)) {
                value = dto.m_partner;
            } else if (PARAM_SCHCODE.equals(param)) {
                value = dto.m_schCode;
            } else if (PARAM_SECTOR.equals(param)) {
                value = dto.m_sector;
            } else if (PARAM_SETTING.equals(param)) {
                value = dto.m_setting;
            } else if (PARAM_TYPE.equals(param)) {
                value = dto.m_type;
            } else if (PARAM_LOCAL_SECTION_ID.equals(param)) {
                value = dto.m_section;
            }
            return value;
        }
    }

    /**
     * Helper DTO class used for calculations.
     *
     * @author Follett Software Company
     */
    private class ReportEntityDTO {

        String m_hours;
        String m_hoursPaid;
        String m_partner;
        String m_schCode;
        String m_section;
        String m_sector;
        String m_setting;
        String m_type;
        StudentScheduleChange m_scc;

        /**
         * Public constructor for instantiation based on StudentScheduleChange object.
         *
         * @param scc StudentScheduleChange
         */
        public ReportEntityDTO(StudentScheduleChange scc) {
            m_hours = (String) scc.getFieldValueByBeanPath(m_sccWBLHoursField);
            if (m_hours == null) {
                m_hours = "";
            }
            m_hoursPaid = (String) scc.getFieldValueByBeanPath(m_sccWBLHoursPaidField);
            m_partner = (String) scc.getFieldValueByBeanPath(m_sccWBLPartnerField);
            if (scc.getMasterSchedule() != null && scc.getMasterSchedule().getSchoolCourse() != null
                    && scc.getMasterSchedule().getSchoolCourse().getSchool() != null) {
                m_schCode = (String) scc.getMasterSchedule().getSchoolCourse().getSchool()
                        .getFieldValueByBeanPath(m_sklIdField);
            } else if (scc.getStudent() != null && scc.getStudent().getSchool() != null) {
                m_schCode = (String) scc.getStudent().getSchool()
                        .getFieldValueByBeanPath(m_sklIdField);
            }
            m_sector = lookupStateValue(StudentScheduleChange.class, m_sccWBLSectorField,
                    (String) scc.getFieldValueByBeanPath(m_sccWBLSectorField));
            m_type = lookupStateValue(StudentScheduleChange.class, m_sccWBLTypeField,
                    (String) scc.getFieldValueByBeanPath(m_sccWBLTypeField));
            m_section = formatCourseSectionName(scc.getMasterSchedule());
            m_setting = WBL_SETTING_DEFAULT;
            m_scc = scc;
        }

        /**
         * Public constructor for instantiation based on StudentSchedule object.
         *
         * @param ssc StudentSchedule
         */
        public ReportEntityDTO(StudentSchedule ssc) {
            m_hours = (String) ssc.getFieldValueByBeanPath(m_sscWBLHoursField);
            if (m_hours == null) {
                m_hours = "";
            }
            m_hoursPaid = (String) ssc.getFieldValueByBeanPath(m_sscWBLHoursPaidField);
            m_partner = (String) ssc.getFieldValueByBeanPath(m_sscWBLPartnerField);
            if (ssc.getSection() != null && ssc.getSection().getSchoolCourse() != null
                    && ssc.getSection().getSchoolCourse().getSchool() != null) {
                m_schCode = (String) ssc.getSection().getSchoolCourse().getSchool()
                        .getFieldValueByBeanPath(m_sklIdField);
            } else if (ssc.getStudent() != null && ssc.getStudent().getSchool() != null) {
                m_schCode = (String) ssc.getStudent().getSchool()
                        .getFieldValueByBeanPath(m_sklIdField);
            }
            m_sector = lookupStateValue(StudentSchedule.class, m_sscWBLSectorField,
                    (String) ssc.getFieldValueByBeanPath(m_sscWBLSectorField));
            m_type = lookupStateValue(StudentSchedule.class, m_sscWBLTypeField,
                    (String) ssc.getFieldValueByBeanPath(m_sscWBLTypeField));
            m_section = formatCourseSectionName(ssc.getSection());
            m_setting = WBL_SETTING_DEFAULT;
        }

        /**
         * Public constructor for instantiation based on SisStudent object.
         *
         * @param student SisStudent
         */
        public ReportEntityDTO(SisStudent student) {
            m_hours = (String) student.getFieldValueByBeanPath(m_stdWBLHoursField);
            if (m_hours == null) {
                m_hours = "";
            }
            m_hoursPaid = (String) student.getFieldValueByBeanPath(m_stdWBLHoursPaidField);
            m_partner = (String) student.getFieldValueByBeanPath(m_stdWBLPartnerField);
            if (student.getSchool() != null) {
                m_schCode = (String) student.getSchool().getFieldValueByBeanPath(m_sklIdField);
            }
            m_sector = (String) student.getFieldValueByBeanPath(m_stdWBLSectorField);
            m_type = (String) student.getFieldValueByBeanPath(m_stdWBLTypeField);
            m_setting = lookupStateValue(SisStudent.class, m_stdWBLSettingField,
                    (String) student.getFieldValueByBeanPath(m_stdWBLSettingField));
        }

        /**
         * Public constructor for instantiation based on StudentAssessment object.
         *
         * @param asm StudentAssessment
         * @param courseView
         */
        public ReportEntityDTO(StudentAssessment asm) {
            m_hours = (String) asm.getFieldValueByBeanPath(m_asmWBLHoursField.getJavaName());
            if (m_hours == null) {
                m_hours = "";
            }
            m_hoursPaid = (String) asm.getFieldValueByBeanPath(m_asmWBLHoursPaidField.getJavaName());
            m_partner = (String) asm.getFieldValueByBeanPath(m_asmWBLPartnerField.getJavaName());
            if (asm.getSchool() != null) {
                m_schCode = (String) asm.getSchool().getFieldValueByBeanPath(m_sklIdField);
            }
            m_section = (String) asm.getFieldValueByBeanPath(m_asmWBLSectionField.getJavaName());
            m_sector = (String) asm.getFieldValueByBeanPath(m_asmWBLSectorField.getJavaName());
            if (m_asmWBLSettingField.hasReferenceTable()) {
                m_setting = (String) asm.getFieldValueByBeanPath(m_asmWBLSettingField.getJavaName());
                if (!StringUtils.isEmpty(m_setting)) {
                    m_setting = lookupReferenceCodeByRefTbl(m_asmWBLSettingField.getReferenceTableOid(), m_setting,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            m_type = (String) asm.getFieldValueByBeanPath(m_asmWBLTypeField.getJavaName());
        }
    }

    private static final String ALIAS_EXCLUDE_SKL = "all-skl-ExcludefromReporting";
    private static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";

    private static final String ALIAS_ASM_WBL_HOURS = "all-asm-WBLHours";
    private static final String ALIAS_ASM_WBL_HOURS_PAID = "all-asm-WBLHoursPaid";
    private static final String ALIAS_ASM_WBL_PARTNER = "all-asm-WBLPartner";
    private static final String ALIAS_ASM_WBL_SECTION = "all-asm-WBLSection";
    private static final String ALIAS_ASM_WBL_SECTOR = "all-asm-WBLSector";
    private static final String ALIAS_ASM_WBL_SETTING = "all-asm-WBLSetting";
    private static final String ALIAS_ASM_WBL_TYPE = "all-asm-WBLType";

    private static final String ALIAS_SCC_WBL_HOURS = "all-scc-WBLHours";
    private static final String ALIAS_SCC_WBL_HOURS_PAID = "all-scc-WBLHoursPaid";
    private static final String ALIAS_SCC_WBL_PARTNER = "all-scc-WBLPartner";
    private static final String ALIAS_SCC_WBL_SECTOR = "all-scc-WBLSector";
    private static final String ALIAS_SCC_WBL_TYPE = "all-scc-WBLType";

    private static final String ALIAS_SSC_WBL_HOURS = "all-ssc-WBLHours";
    private static final String ALIAS_SSC_WBL_HOURS_PAID = "all-ssc-WBLHoursPaid";
    private static final String ALIAS_SSC_WBL_PARTNER = "all-ssc-WBLPartner";
    private static final String ALIAS_SSC_WBL_SECTOR = "all-ssc-WBLSector";
    private static final String ALIAS_SSC_WBL_TYPE = "all-ssc-WBLType";

    private static final String ALIAS_STD_WBL_HOURS = "all-std-WBLHours";
    private static final String ALIAS_STD_WBL_HOURS_PAID = "all-std-WBLHoursPaid";
    private static final String ALIAS_STD_WBL_PARTNER = "all-std-WBLPartner";
    private static final String ALIAS_STD_WBL_SECTOR = "all-std-WBLSector";
    private static final String ALIAS_STD_WBL_SETTING = "all-std-WBLSetting";
    private static final String ALIAS_STD_WBL_TYPE = "all-std-WBLType";

    private static final String ASSMT_DEF_ID = "WBL";

    private static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";

    private static final String SECTION_SEPARATOR = "-";

    private static final String WBL_SETTING_DEFAULT = "CLASSSECTION";

    protected Map<String, Collection<StudentAssessment>> m_assesmentByStdMap = null;
    protected DataDictionaryField m_asmWBLHoursField;
    protected DataDictionaryField m_asmWBLHoursPaidField;
    protected DataDictionaryField m_asmWBLPartnerField;
    protected DataDictionaryField m_asmWBLSectionField;
    protected DataDictionaryField m_asmWBLSectorField;
    protected DataDictionaryField m_asmWBLSettingField;
    protected DataDictionaryField m_asmWBLTypeField;
    protected String m_excludeSklField;
    protected String m_excludeStdField;
    protected StudentHistoryHelper m_helper;
    protected boolean m_sasidStudentsOnly;
    protected String m_sccWBLHoursField;
    protected String m_sccWBLHoursPaidField;
    protected String m_sccWBLPartnerField;
    protected String m_sccWBLSectorField;
    protected String m_sccWBLTypeField;
    protected String m_sscWBLHoursField;
    protected String m_sscWBLHoursPaidField;
    protected String m_sscWBLPartnerField;
    protected String m_sscWBLSectorField;
    protected String m_sscWBLTypeField;
    protected String m_stdWBLHoursField;
    protected String m_stdWBLHoursPaidField;
    protected String m_stdWBLPartnerField;
    protected String m_stdWBLSectorField;
    protected String m_stdWBLSettingField;
    protected String m_stdWBLTypeField;

    /**
     * Close.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
     */
    @Override
    public void close() {
        super.close();
        if (m_helper != null) {
            m_helper.close();
        }
    }

    /**
     * Initialize the export.
     *
     * @throws X2BaseException exception
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        m_sasidStudentsOnly = true;
        Boolean sasidStudentsOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentsOnly != null) {
            m_sasidStudentsOnly = sasidStudentsOnly.booleanValue();
        }

        // Use history helper to discover students with a populated WBLTypeField in schedule
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);

        X2Criteria scheduleCriteria = m_helper.getStudentScheduleCriteria();
        scheduleCriteria.addNotEqualTo(
                StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL
                        + ModelProperty.PATH_DELIMITER + m_excludeSklField,
                BooleanAsStringConverter.TRUE);
        scheduleCriteria.addNotEmpty(m_sscWBLHoursField, getBroker().getPersistenceKey());

        X2Criteria scheduleChangeCriteria = m_helper.getStudentScheduleChangeCriteria();
        scheduleChangeCriteria.addNotEqualTo(
                StudentScheduleChange.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL
                        + ModelProperty.PATH_DELIMITER + m_excludeSklField,
                BooleanAsStringConverter.TRUE);
        scheduleChangeCriteria.addNotEmpty(m_sscWBLHoursField, getBroker().getPersistenceKey());


        X2Criteria studentWBLCriteria = new X2Criteria();

        studentWBLCriteria.addNotEqualTo(
                SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_excludeSklField,
                BooleanAsStringConverter.TRUE);
        studentWBLCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                Student.COL_ENROLLMENT_STATUS));
        studentWBLCriteria.addNotEqualTo(m_excludeStdField, BooleanAsStringConverter.TRUE);
        studentWBLCriteria.addNotEmpty(m_stdWBLTypeField, getBroker().getPersistenceKey());

        initializeStudentAssessments();
        X2Criteria asmWBLCriteria = new X2Criteria();
        ParameterSelectionHandler.addParameterSafeOIDList(asmWBLCriteria, getBroker(), m_assesmentByStdMap.keySet(), 0,
                X2BaseBean.COL_OID);

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addOrCriteria(studentWBLCriteria);
        studentCriteria.addOrCriteria(asmWBLCriteria);


        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            studentCriteria.addNotEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        applyInputCriteria(studentCriteria, false, null);

        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);

        setQuery(studentQuery);
        setEntityClass(WorkBasedLearningEntity.class);

        // Add any retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveWBLData.CALC_ID, new RetrieveWBLData());
        super.addCalcs(calcs);

    }

    private String formatCourseSectionName(MasterSchedule ms) {
        String value = null;
        if (ms != null) {
            value = String.format("%s%s%s", ms.getSchoolCourse().getNumber(), SECTION_SEPARATOR,
                    ms.getSectionNumber());
        }
        return value;
    }

    /**
     * Get Field Java Name according given alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @param isRequired boolean
     * @return String
     */
    private DataDictionaryField getAsmJavaName(String alias, DataDictionary dataDictionary, boolean isRequired) {
        DataDictionaryField dictField = dataDictionary.findDataDictionaryFieldByAlias(alias);
        if (dictField == null && isRequired) {
            addSetupError("Setup Error", "Assessment column for " + alias + " is not defined");
        }
        return dictField;
    }

    /**
     * Initialize asd.
     */
    private void initializeAsd() {
        X2Criteria asdCriteria = new X2Criteria();
        asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASSMT_DEF_ID);

        AssessmentDefinition asd = (AssessmentDefinition) getBroker()
                .getBeanByQuery(new QueryByCriteria(AssessmentDefinition.class, asdCriteria));

        if (asd == null) {
            addSetupError("Setup Error", "Required assessment definition with ID = " + ASSMT_DEF_ID
                    + " could not be found");
        } else {
            DataDictionary dataDictionary =
                    DataDictionary.getDistrictDictionary(asd, getBroker().getPersistenceKey());

            if (dataDictionary == null) {
                addSetupError("Setup Error", "Extended Dictinary for assessment could not be loaded");
            } else {
                m_asmWBLHoursField = getAsmJavaName(ALIAS_ASM_WBL_HOURS, dataDictionary, true);
                m_asmWBLHoursPaidField = getAsmJavaName(ALIAS_ASM_WBL_HOURS_PAID, dataDictionary, true);
                m_asmWBLPartnerField = getAsmJavaName(ALIAS_ASM_WBL_PARTNER, dataDictionary, true);
                m_asmWBLSectionField = getAsmJavaName(ALIAS_ASM_WBL_SECTION, dataDictionary, true);
                m_asmWBLSectorField = getAsmJavaName(ALIAS_ASM_WBL_SECTOR, dataDictionary, true);
                m_asmWBLSettingField = getAsmJavaName(ALIAS_ASM_WBL_SETTING, dataDictionary, true);
                m_asmWBLTypeField = getAsmJavaName(ALIAS_ASM_WBL_TYPE, dataDictionary, true);
            }
        }
    }

    /**
     * Initialize fields. Translate aliases to java names.
     */
    private void initializeFields() {
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, true);

        m_sccWBLHoursField = translateAliasToJavaName(ALIAS_SCC_WBL_HOURS, true);
        m_sccWBLHoursPaidField = translateAliasToJavaName(ALIAS_SCC_WBL_HOURS_PAID, true);
        m_sccWBLPartnerField = translateAliasToJavaName(ALIAS_SCC_WBL_PARTNER, true);
        m_sccWBLSectorField = translateAliasToJavaName(ALIAS_SCC_WBL_SECTOR, true);
        m_sccWBLTypeField = translateAliasToJavaName(ALIAS_SCC_WBL_TYPE, true);
        m_sscWBLHoursField = translateAliasToJavaName(ALIAS_SSC_WBL_HOURS, true);
        m_sscWBLHoursPaidField = translateAliasToJavaName(ALIAS_SSC_WBL_HOURS_PAID, true);
        m_sscWBLPartnerField = translateAliasToJavaName(ALIAS_SSC_WBL_PARTNER, true);
        m_sscWBLSectorField = translateAliasToJavaName(ALIAS_SSC_WBL_SECTOR, true);
        m_sscWBLTypeField = translateAliasToJavaName(ALIAS_SSC_WBL_TYPE, true);

        m_stdWBLHoursField = translateAliasToJavaName(ALIAS_STD_WBL_HOURS, true);
        m_stdWBLHoursPaidField = translateAliasToJavaName(ALIAS_STD_WBL_HOURS_PAID, true);
        m_stdWBLPartnerField = translateAliasToJavaName(ALIAS_STD_WBL_PARTNER, true);
        m_stdWBLSectorField = translateAliasToJavaName(ALIAS_STD_WBL_SECTOR, true);
        m_stdWBLTypeField = translateAliasToJavaName(ALIAS_STD_WBL_TYPE, true);
        m_stdWBLSettingField = translateAliasToJavaName(ALIAS_STD_WBL_SETTING, true);

        initializeAsd();
    }

    /**
     * Inits the students assesm map.
     */
    private void initializeStudentAssessments() {
        X2Criteria asmCriteria = new X2Criteria();
        asmCriteria
                .addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                        AssessmentDefinition.COL_ID, ASSMT_DEF_ID);
        asmCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE,
                getCurrentContext().getStartDate());
        asmCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getEndDate());
        if (isSchoolContext()) {
            asmCriteria.addEqualTo(StudentAssessment.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria byCriteria = new QueryByCriteria(StudentAssessment.class, asmCriteria);
        m_assesmentByStdMap =
                getBroker().getGroupedCollectionByQuery(byCriteria, StudentAssessment.COL_STUDENT_OID, 100);

    }

}
