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
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * FL Title I Supplemental Educational Services report.
 */
public class FLStudentAssessmentData extends FLStateReportData {

    /**
     * The Enum ASD_TYPES.
     */
    enum ASD_TYPES implements AsdTypeInterface {
        AICE("AICE", "CAI"), AP("AP", "APT"), IB("IB", "IBP");

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
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.AsdTypeInterface#getTestName()
         */
        @Override
        public String getTestName() {
            return m_testName;
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
    }

    /**
     * The Class FLStudentAssessmentEntity.
     */
    public static class FLStudentAssessmentEntity extends FLStateReportEntity {

        /**
         * Instantiates a new FL student assessment entity.
         */
        public FLStudentAssessmentEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
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
    }

    /**
     * The Class RetrieveAssessmentTestName.
     */
    protected class RetrieveTestName implements FieldRetriever {
        public static final String CALC_ID = "ASM_TEST_NAME";

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
            StudentAssessment asm = (StudentAssessment) entity.getBean();
            return ASD_TYPES.valueOf(asm.getAssessmentDefinition().getId()).getTestName();
        }

    }

    /**
     * The Class RetrieveStudentAssessmentFieldSIS.
     */
    protected class RetrieveStudentAssessmentFieldSIS extends RetrieveStudentAssessmentField {

        /**
         * Instantiates a new retrieve student assessment field SIS.
         *
         * @throws X2BaseException exception
         */
        public RetrieveStudentAssessmentFieldSIS() throws X2BaseException {
            super();
        }

        /**
         * Gets the asm types.
         *
         * @return Asm type interface[]
         * @see com.x2dev.procedures.statereporting.fl.FLStateReportData.RetrieveAsmFieldInterface#getAsdTypes()
         */
        @Override
        public AsdTypeInterface[] getAsdTypes() {
            return ASD_TYPES.values();
        }

    }

    protected static final List<String> CUSTOM_SURVEY_PERIOD_VALID_CODES =
            Arrays.asList(FLStateReportData.SURVEY_PERIOD_5);

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

        if (getSetupErrors().size() != 0) {
            return;
        }

        X2Criteria studentCriteria = getStudentHelper().getStudentCriteria();
        SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria studentAssessmentCriteria = new X2Criteria();
        studentAssessmentCriteria
                .addIn(StudentAssessment.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, studentQuery);
        studentAssessmentCriteria.addIn(
                StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER
                        + AssessmentDefinition.COL_ID,
                ASD_TYPES.getIds());

        // TODO: add additional criteria based on the start and end date of the selected survey
        // periods (done)
        studentAssessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getSurveyPeriod().getStartDate());
        studentAssessmentCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, getSurveyPeriod().getEndDate());

        QueryByCriteria studentAssessmentQuery =
                new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria, false);

        setQuery(studentAssessmentQuery);
        setEntityClass(FLStudentAssessmentEntity.class);

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
        return CUSTOM_SURVEY_PERIOD_VALID_CODES;
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
        calcs.put(RetrieveStudentAssessmentField.CALC_ID, new RetrieveStudentAssessmentFieldSIS());
        calcs.put(RetrieveTestName.CALC_ID, new RetrieveTestName());
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
