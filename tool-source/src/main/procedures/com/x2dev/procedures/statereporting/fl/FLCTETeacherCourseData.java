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

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLCTEStudentCourseScheduleData.RetrieveCteFields;
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper.MasterScheduleInfo;
import com.x2dev.procedures.statereporting.fl.FLStaffHelper.ScheduleTeacherInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * FL Title I Supplemental Educational Services report.
 */
public class FLCTETeacherCourseData extends FLStateReportData {

    /**
     * The Class FLCTETeacherCourseEntity.
     */
    public static class FLCTETeacherCourseEntity extends FLStateReportEntity {
        private FLCTETeacherCourseData m_data;
        private List<ScheduleTeacherInfo> m_records;

        /**
         * Instantiates a new FLCTE teacher course entity.
         */
        public FLCTETeacherCourseEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         */
        @Override
        public String getEntityName() {
            SisStaff staff = (SisStaff) getBean();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    "] ";
            return name;
        }

        /**
         * Gets the schedule info.
         *
         * @return Schedule teacher info
         */
        public ScheduleTeacherInfo getScheduleInfo() {
            return m_records.get(getCurrentRow());
        }

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

            m_data = (FLCTETeacherCourseData) data;

            List<ScheduleTeacher> schedules = m_data.getStaffHelper().getTeacherSchedules(bean.getOid());
            if (schedules != null && !schedules.isEmpty()) {
                m_records = new ArrayList(schedules.size());
                for (ScheduleTeacher schedule : schedules) {
                    if (!StringUtils.isEmpty(schedule.getSectionOid())
                            && m_data.m_sectionsWithCteOids.contains(schedule.getSectionOid())) {
                        ScheduleTeacherInfo info = m_data.getStaffHelper().new ScheduleTeacherInfo(schedule);
                        m_records.add(info);
                    }
                }

                setRowCount(m_records.size());
            } else

            {
                setRowCount(0);
            }
        }
    }

    /**
     * Field retriever for Career and Technical Education Teacher Course fields.
     */
    protected class RetrieveCTETeacherCoursFields implements FieldRetriever {

        public static final String CALC_ID = "CALC_CTETC";

        private static final String PARAM_CTETC_CRS_ID = "CTETC_CRS_ID";
        private static final String PARAM_CTETC_ED_CERT_ID = "CTETC_ED_CERT_ID";
        private static final String PARAM_CTETC_FACIL_TYPE = "CTETC_FACIL_TYPE";
        private static final String PARAM_CTETC_PERIOD_ID = "CTETC_PERIOD_ID";
        private static final String PARAM_CTETC_SCHOOL_ID = "CTETC_SCHOOL_ID";
        private static final String PARAM_CTETC_SECT_ID = "CTETC_SECT_ID";
        private static final String PARAM_CTETC_SSN = "CTETC_SSN";
        private static final String PARAM_CTETC_TERM = "CTETC_TERM";

        private DataDictionaryField m_fieldSheduleTermCode;

        /**
         * Instantiates a new retrieve CTE teacher cours fields.
         */
        public RetrieveCTETeacherCoursFields() {
            m_fieldSheduleTermCode =
                    getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
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
            ScheduleTeacherInfo info = ((FLCTETeacherCourseEntity) entity).getScheduleInfo();
            FLCTETeacherCourseData flData = (FLCTETeacherCourseData) data;

            String parameter = (String) field.getParameter();
            Object value = null;

            if (info != null) {
                MasterScheduleInfo masterScheduleInfo = info.getMasterScheduleInfo();
                if (masterScheduleInfo != null) {
                    switch (parameter) {
                        case PARAM_CTETC_SCHOOL_ID:
                            value = masterScheduleInfo.getSchoolNumber();
                            break;
                    }
                    switch (parameter) {
                        case PARAM_CTETC_CRS_ID:
                            value = masterScheduleInfo.getCourseNumber();
                            break;
                    }
                    switch (parameter) {
                        case PARAM_CTETC_SECT_ID:
                            value = masterScheduleInfo.getSectionNumber();
                            break;
                    }
                    switch (parameter) {
                        case PARAM_CTETC_PERIOD_ID:
                            value = masterScheduleInfo.getPeriodNumber();
                            break;
                    }
                    switch (parameter) {
                        case PARAM_CTETC_TERM:
                            value = flData.getFieldValue(masterScheduleInfo.getSection().getScheduleTerm(),
                                    m_fieldSheduleTermCode);

                            break;
                    }
                    switch (parameter) {
                        case PARAM_CTETC_FACIL_TYPE:
                            value = masterScheduleInfo.getFacilityType();
                            break;
                    }
                    switch (parameter) {
                        case PARAM_CTETC_ED_CERT_ID:
                            value = info.getStaffInfo().getCertificationNumber();
                            break;
                    }
                    switch (parameter) {
                        case PARAM_CTETC_SSN:
                            value = info.getStaffInfo().getSSN();
                            break;
                    }
                }
            }

            return value;
        }
    }

    protected static final List<String> CTETC_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_5);

    private Collection<String> m_sectionsWithCteOids = null;

    /**
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        DataDictionaryField field = translateAliasToDictionaryField(FLScheduleHelper.ALIAS_CTE_PROGRAM_CODE_CRS, true);
        if (getSetupErrors().size() != 0) {
            return;
        }

        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_BEGIN_DATE, getSurveyPeriod().getStartDate());
        getStaffHelper().setSelectionProperty(FLStaffHelper.PROPERTY_END_DATE, getSurveyPeriod().getEndDate());

        getStaffHelper().getTeacherScheduleCriteria()
                .addNotEmpty(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER +
                        field.getJavaName(), getBroker().getPersistenceKey());

        setQuery(getStaffHelper().getStaffQuery(false));
        setEntityClass(FLCTETeacherCourseEntity.class);

        initializeSectionsWithCTEStudents();

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the valid survey periods.
     *
     * @return Collection
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return CTETC_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Initialize sections with CTE students.
     *
     * @throws X2BaseException exception
     */
    private void initializeSectionsWithCTEStudents() throws X2BaseException {
        m_sectionsWithCteOids = new HashSet<String>();

        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID,
                getStudentHelper().getStudentProgramDataset(
                        RetrieveCteFields.DDX_ID,
                        getSurveyPeriod()).getStudentSubQuery());

        FLScheduleHelper scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        StudentScheduleHelper studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());

        Collection<SisStudent> students = getBroker().getCollectionByQuery(getStudentHelper().getStudentQuery(false));
        for (SisStudent student : students) {
            Collection<StudentScheduleInfo> infos = studentScheduleHelper.getStudentScheduleInfo(student);
            for (StudentScheduleInfo info : infos) {
                m_sectionsWithCteOids.add(info.getSection().getOid());
            }
        }
    }

    /**
     * Register custom field Retrievers.
     *
     * @throws X2BaseException exception
     */

    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveCTETeacherCoursFields.CALC_ID, new RetrieveCTETeacherCoursFields());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */

    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
