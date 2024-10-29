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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Assessment Export export.
 *
 * @author X2 Development Corporation
 */

public class NYAssessmentModExport extends StateReportData {
    /**
     * Entity class for Assessment Export export.
     *
     * @author X2 Development Corporation
     */

    public static class AssessmentModExportEntity extends StateReportEntity {

        ArrayList<String> assessmentMods;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public AssessmentModExportEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            NYAssessmentModExport amData = (NYAssessmentModExport) data;
            StudentAssessment assessment = (StudentAssessment) bean;
            // TODO Table replace
            assessmentMods = new ArrayList<String>();
            for (int a = 0; a < 5; a++) {
                String element = (String) assessment.getFieldValueByBeanPath(amData.m_aliasModifications.get(a));
                if (element != null) {
                    assessmentMods.add(element);
                }
            }
            setRowCount(assessmentMods.size());
        }

        /**
         * Gets the current mod.
         *
         * @return String
         */
        // TODO Table replace
        public String getCurrentMod() {
            return assessmentMods.get(getCurrentRow());
        }
    }

    /**
     * Constants for reporting information.
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_ASSESS_MOD = "assessMod";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";

    protected static final String ALIAS_NUMERIC_SCORE = "DOE NUMERIC SCORE";
    protected static final String ALIAS_SCALE_SCORE = "DOE SCALE SCORE";
    protected static final String ALIAS_RAW_SCORE = "DOE RAW SCORE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_FROM_SIRS = "NY EXCLUDE SIRS";

    protected static final String ASSESSMENT_RCT = "RCT";

    public static final char PATH_DELIMITER = '.';

    /**
     * Validation errors
     */
    protected static final String VAL_ERROR_AA6201 =
            "AA6201 v7.04 There are records in this file that contain the following district code: \"Bad District\". This district code\r\n"
                    +
                    "does not match your selected district which is: \"Selected District\". The validation process cannot continue\r\n"
                    +
                    "until all the records in the import file match your selected district. If you chose the wrong district at the top of\r\n"
                    +
                    "this page please select the district and perform the import process again.";
    protected static final String VAL_ERROR_AA6208 =
            "Missing or invalid Accomodation Modification & Acc Mod Type Code combination: %s";

    protected StudentHistoryHelper m_helper;

    /**
     * Local variables for reporting information.
     */
    protected PlainDate m_reportDate;

    protected Boolean m_assessMod;

    public String m_excludeSchool;
    public String m_excludeFromSirs;

    // TODO Table Replace
    protected ArrayList<String> m_aliasModifications;

    /**
     * Retrieves the current date.
     */
    protected class RetrieveSnapshotDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return new PlainDate();
        }
    }

    /**
     * Retrieves the current code.
     */
    protected class RetrieveModCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            AssessmentModExportEntity amEntity = (AssessmentModExportEntity) entity;

            value = lookupReferenceCodeByBeanPath(StudentAssessment.class,
                    m_aliasModifications.get(entity.getCurrentRow()),
                    amEntity.getCurrentMod(),
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return value;
        }
    }

    /**
     * Validate the Instruction Dates.
     */
    protected class ValidateAccomodationModification implements FieldValidator {
        private static final String VAL_ID = "VAL-ACC-MOD";

        /**
         * Fields' IDs
         */
        private static final String FIELD_ACC_MOD_CODE = "Acc Mod Code";
        private final List<String> VALID_ACC_MOD_CODES = Arrays.asList("01", "02", "03", "04", "05", "06", "07", "08",
                "09", "10", "11", "12");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String accModeCode = entity.getFieldValue(FIELD_ACC_MOD_CODE);

            if (!com.x2dev.utils.StringUtils.isEmpty(accModeCode) && !VALID_ACC_MOD_CODES.contains(accModeCode)) {
                errors.add(new StateReportValidationError(entity, field, "AA6208", String.format(VAL_ERROR_AA6208,
                        accModeCode)));
            }

            return errors;
        }
    }

    /**
     * Overrided getHeading Method. The reason is so that the user can decide if the header is
     * included or not at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if ((Boolean) getParameter(PARAM_REMOVE_HEADER) == Boolean.TRUE) {
            return null;
        }
        return super.getHeading();
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {

            if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
                m_helper.getStudentCriteria().addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + m_excludeSchool,
                        BooleanAsStringConverter.TRUE);
            }

            SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

            Criteria assessmentCriteria = new Criteria();

            assessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
            assessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getStartDate());
            // assessmentCriteria.addNotEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION +
            // PATH_DELIMITER + m_excludeFromSirs, BooleanAsStringConverter.TRUE);

            assessmentCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION
                    + PATH_DELIMITER + AssessmentDefinition.COL_ID, ASSESSMENT_RCT);

            // … create Query …
            QueryByCriteria query = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);

            applyInputSort(query, null);
            setQuery(query);
            setEntityClass(AssessmentModExportEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("AR-SSDATE", new RetrieveSnapshotDate());
            calcs.put("AM-MODCODE", new RetrieveModCode());

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateAccomodationModification.VAL_ID, new ValidateAccomodationModification());

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Initializes fields to be used in the export.
     */
    private void initializeFields() {
        // TODO Table replace
        m_aliasModifications = new ArrayList<String>();
        for (int a = 0; a < 5; a++) {
            m_aliasModifications.add(translateAliasToJavaName("DOE MOD " + (a + 1), true));
        }
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_excludeFromSirs = translateAliasToJavaName(ALIAS_EXCLUDE_FROM_SIRS, true);
    }

}
