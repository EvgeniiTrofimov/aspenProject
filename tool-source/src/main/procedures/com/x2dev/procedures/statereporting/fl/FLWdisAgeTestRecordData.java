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

/*
 * TODO: Complete the following items as soon as possible so that my testing can continue
 * 1) Review Appendix L. You will see there that each assessment has an associated test code type.
 * These must be added to a FL Test Codes reference table and associated with a column definition
 * for each of the assessment definitions. These fields should not be editable and should default to
 * the correct value. (done)
 * You will also need to load an additional reference table named FL Test Content Code which will be
 * dependent on the test code. This will automatically restrict the availability of choices bases on
 * the value in the test code field. (done)
 *
 * 2) Correct the key indicator in the export format definition. Rows are currently being excluded
 * because of unique key matches. (done)
 *
 * 3) Correct the test date format in the output. (done)
 *
 * 4) Adjust other TODOs in the code (done)
 *
 */

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentProgramDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLWdisAgeTestRecordData.
 */
public class FLWdisAgeTestRecordData extends FLStateReportData implements WdisEntity {


    /**
     * The Enum ASD_TYPES.
     */
    enum ASD_TYPES implements AsdTypeInterface {
        BSL("BSL", "BSL"), BSP("BSP", "BSP"), CAS("CAS", "CAS"), TAB("TAB", "TAB"), TBE("TBE", "TBE");

        private String m_id;
        private String m_testName;


        /**
         * Instantiates a new asd types.
         *
         * @param id String
         * @param testName String
         */
        ASD_TYPES(String id, String testName) {
            m_id = id;
            m_testName = testName;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.AsdTypeInterface#getId()
         */
        @Override
        public String getId() {
            return m_id;
        }


        /**
         * Gets the ids.
         *
         * @return Collection
         */
        public static Collection<String> getIds() {
            Collection<String> ids = new ArrayList<String>();
            for (ASD_TYPES value : values()) {
                ids.add(value.getId());
            }
            return ids;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.AsdTypeInterface#getTestName()
         */
        @Override
        public String getTestName() {
            return m_testName;
        }
    }


    /**
     * The Class FLAgeTestRecordEntity.
     */
    public static class FLAgeTestRecordEntity extends FLStateReportEntity implements WdisEntity {

        FLWdisAgeTestRecordData m_data;
        SisStudent m_student;


        /**
         * Instantiates a new FL age test record entity.
         */
        public FLAgeTestRecordEntity() {
            // Public no argument constructor for dynamic instantiation.
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            SisStudent student = studentAssessment.getStudent();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", ASDID: " + studentAssessment.getAssessmentDefinition().getId() +
                    ", ASMDATE: " + studentAssessment.getDate().toString() +
                    "] ";

            return name;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity#getProgram()
         */
        @Override
        public StudentProgramParticipation getProgram() {
            StudentProgramParticipation pgm = null;

            List<StudentProgramParticipation> pgms =
                    m_data.m_studentProgramDataset.getPrograms(m_student.getOid());
            if (pgms != null && pgms.size() > 0) {
                pgm = pgms.get(pgms.size() - 1);
            }

            return pgm;
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (FLWdisAgeTestRecordData) data;
            StudentAssessment studentAssessment = (StudentAssessment) bean;
            m_student = studentAssessment.getStudent();
            List<StudentScheduleInfo> infos = m_data.m_studentScheduleHelper.getStudentScheduleInfo(m_student);
            if (infos.size() == 0 || getProgram() == null) {
                setRowCount(0);
            }
        }
    }


    /**
     * The Class RetrieveStudentAssessmentFieldWDIS.
     */
    protected class RetrieveStudentAssessmentFieldWDIS extends RetrieveStudentAssessmentField {


        /**
         * Instantiates a new retrieve student assessment field WDIS.
         *
         * @throws X2BaseException exception
         */
        public RetrieveStudentAssessmentFieldWDIS() throws X2BaseException {
            super();
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.RetrieveAsmFieldInterface#getAsdTypes()
         */
        @Override
        public AsdTypeInterface[] getAsdTypes() {
            return ASD_TYPES.values();
        }

    }


    /**
     * The Class RetrieveTestName.
     */
    protected class RetrieveTestName implements FieldRetriever {
        public static final String CALC_ID = "Test Name";


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentAssessment asm = (StudentAssessment) entity.getBean();
            return ASD_TYPES.valueOf(asm.getAssessmentDefinition().getId()).getTestName();
        }

    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_F, FLStateReportData.SURVEY_PERIOD_W,
                    FLStateReportData.SURVEY_PERIOD_S);

    private final static String DDX_ID_AGE = "FL-PGM-AGE";

    protected FLScheduleHelper m_scheduleHelper;
    protected StudentScheduleHelper m_studentScheduleHelper;
    protected StudentProgramDataset m_studentProgramDataset;


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportEntity.WdisEntity#getProgram()
     */
    @Override
    public StudentProgramParticipation getProgram() {
        // TODO Auto-generated method stub
        return null;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#getValidSurveyPeriods()
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return CUSTOM_SURVEY_PERIOD_VALID_CODES;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLStateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() != 0) {
            return;
        }

        getStudentHelper().setWdisMode(WDIS_MODE_ADULT_GENERAL_EDUCATION);
        X2Criteria studentCriteria = getStudentHelper().getStudentCriteria();
        SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria studentAssessmentCriteria = new X2Criteria();
        studentAssessmentCriteria
                .addIn(StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, studentQuery);
        studentAssessmentCriteria.addIn(
                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                        + AssessmentDefinition.COL_ID,
                ASD_TYPES.getIds());

        studentAssessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getStartDate());
        studentAssessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, getSurveyPeriod().getEndDate());

        QueryByCriteria studentAssessmentQuery =
                new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria, false);

        m_scheduleHelper =
                new FLScheduleHelper(this, this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                this.getSurveyPeriod().getStartDate(), this.getSurveyPeriod().getEndDate());
        m_studentProgramDataset = getStudentHelper().getStudentProgramDataset(DDX_ID_AGE, getSurveyPeriod());

        setQuery(studentAssessmentQuery);
        setEntityClass(FLAgeTestRecordEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }


    /**
     * Register field retrievers.
     *
     * @throws X2BaseException exception
     */
    private void registerFieldRetrievers() throws X2BaseException {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveWdisYear.CALC_ID, new RetrieveWdisYear());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveStudentAssessmentField.CALC_ID, new RetrieveStudentAssessmentFieldWDIS());
        calcs.put(RetrieveTestName.CALC_ID, new RetrieveTestName());
        super.addCalcs(calcs);
    }



    /**
     * Register field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
