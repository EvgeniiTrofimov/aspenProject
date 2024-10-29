/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.wa;

import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;

/**
 * This class provides the data source for the Washington immunization report 348-014.
 *
 * @author X2 Development Corporation
 */
public class WAImmunizationSummaryPreK extends ReportJavaSourceNet {

    /**
     * A helper class containing the counts for all vaccines for a particular class of students.
     */
    private class VaccineCounts {
        int[] m_counts;
        int m_size;

        /**
         * Instantiates a new vaccine counts.
         *
         * @param size int
         */
        VaccineCounts(int size) {
            m_size = size;
            m_counts = new int[size];
        }

        /**
         * Increment.
         *
         * @param entity StateReportEntity
         * @param match String
         */
        public void increment(StateReportEntity entity, String match) {
            for (int i = 0; i < m_size; ++i) {
                String code = entity.getFieldValue(m_vaccinePositions[i]);
                if (code != null && code.matches(match)) {
                    m_counts[i]++;
                }
            }
        }

        /**
         * Gets the count.
         *
         * @param index int
         * @return Integer
         */
        public Integer getCount(int index) {
            return Integer.valueOf(m_counts[index]);
        }
    }

    private static final String DATA_FIELD_DT = "Diptheria/Tetanus";
    private static final String DATA_FIELD_GRADE = "Grade level";
    private static final String DATA_FIELD_HEP_B = "Hep B";
    private static final String DATA_FIELD_HIB = "HIB";
    private static final String DATA_FIELD_MMR = "MMR";
    private static final String DATA_FIELD_P = "Pertussis";
    private static final String DATA_FIELD_PCV = "Pneumococcal";
    private static final String DATA_FIELD_POLIO = "Polio";
    private static final String DATA_FIELD_SCHOOL = "School";
    private static final String DATA_FIELD_SCHOOL_ADDRESS = "School Address";
    private static final String DATA_FIELD_SCHOOL_CITY = "School City";
    private static final String DATA_FIELD_SCHOOL_CITY_STATE = "School City/State";
    private static final String DATA_FIELD_SCHOOL_COUNTY = "School County";
    private static final String DATA_FIELD_SCHOOL_ZIP = "School Zip";
    private static final String DATA_FIELD_VARICELLA = "Varicella";

    private static final String FIELD_ENROLLED = "enrolled";
    private static final String FIELD_IMMUNE_TOTAL = "immuneTotal";
    private static final String FIELD_EXEMPT_TOTAL = "exemptTotal";
    private static final String FIELD_EXEMPT_MEDICAL = "exemptMedical";
    private static final String FIELD_EXEMPT_PERSONAL = "exemptPersonal";
    private static final String FIELD_EXEMPT_RELIGIOUS = "exemptReligious";
    private static final String FIELD_EXEMPT_RELIGIOUS_MEMBERSHIP = "exemptReligiousMemb";
    private static final String FIELD_CONDITIONAL = "conditional";
    private static final String FIELD_OUT_OF_COMPLIANCE = "outOfCompliance";

    private static final String FIELD_SCHOOL_ADDRESS = "schoolAddress";
    private static final String FIELD_SCHOOL_CITY = "schoolCity";
    private static final String FIELD_SCHOOL_CITY_STATE = "schoolCityState";
    private static final String FIELD_SCHOOL_COUNTY = "schoolCounty";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_SCHOOL_ZIP = "schoolZip";

    private static final String FIELD_VACCINE_NOT_IMMUNE_DT = "vaccineNotImmuneDT";
    private static final String FIELD_VACCINE_NOT_IMMUNE_P = "vaccineNotImmuneP";
    private static final String FIELD_VACCINE_NOT_IMMUNE_POLIO = "vaccineNotImmunePolio";
    private static final String FIELD_VACCINE_NOT_IMMUNE_MMR = "vaccineNotImmuneMMR";
    private static final String FIELD_VACCINE_NOT_IMMUNE_HEP_B = "vaccineNotImmuneHepB";
    private static final String FIELD_VACCINE_NOT_IMMUNE_VARICELLA = "vaccineNotImmuneVaricella";
    private static final String FIELD_VACCINE_NOT_IMMUNE_HIB = "vaccineNotImmuneHIB";
    private static final String FIELD_VACCINE_NOT_IMMUNE_PCV = "vaccineNotImmunePCV";

    private static int INDEX_DT = 0;
    private static int INDEX_P = 1;
    private static int INDEX_POLIO = 2;
    private static int INDEX_MMR = 3;
    private static int INDEX_HEP_B = 4;
    private static int INDEX_VARICELLA = 5;
    private static int INDEX_HIB = 6;
    private static int INDEX_PCV = 7;

    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";

    private static String MATCH_ANY_EXEMPT_STATUS = ".*[MPRG].*";
    private static String MATCH_ANY_NOT_IMMUNE_STATUS = ".*[DMNPRG].*";
    private static String MATCH_GRADE_PK = "^PK$";

    // Maintain list of unused codes
    // private static String MATCH_STATUS_COMPLIANT = "C";
    private static String MATCH_STATUS_CONDITIONAL = ".*D.*";
    private static String MATCH_STATUS_MEDICAL_EXEMPT = ".*M.*";
    private static String MATCH_STATUS_NON_COMPLIANT = ".*N.*";
    private static String MATCH_STATUS_PERSONAL_EXEMPT = ".*P.*";
    private static String MATCH_STATUS_RELIGIOUS_EXEMPT = ".*R.*";
    private static String MATCH_STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT = ".*G.*";

    private static int NUM_FIELDS = 46;
    private static int NUM_VACCINES = 8;

    private static final String PARAM_DISTRICT_NAME = "districtName";
    private static final String PARAM_START_SCHOOL_YEAR = "startSchoolYear";
    private static final String PARAM_REPORT_DATE = "reportDate";

    private static final String PROCEDURE_ID = "procedureId";

    protected int m_countPKConditional;
    protected int m_countPKEnrolled;
    protected int m_countPKExempt;
    protected int m_countPKMedicalExempt;
    protected int m_countPKPersonalExempt;
    protected int m_countPKReligiousExempt;
    protected int m_countPKReligiousMembExempt;
    protected int m_countPKImmune;
    protected int m_countPKNotCompliant;

    protected VaccineCounts m_countsPK;

    protected int m_gradePosition = -1;
    protected int m_schoolPosition = -1;
    protected int m_schoolAddressPosition = -1;
    protected int m_schoolCityPosition = -1;
    protected int m_schoolCityStatePosition = -1;
    protected int m_schoolCountyPosition = -1;
    protected int m_schoolZipPosition = -1;
    protected int[] m_vaccinePositions = new int[NUM_VACCINES];

    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;
    private String m_schoolName = "";
    private String m_schoolAddress = "";
    private String m_schoolCity = "";
    private String m_schoolCityState = "";
    private String m_schoolCounty = "";
    private String m_schoolZIP = "";


    /**
     * Provides the report data source. The input data source is delievered
     * by the WAImmunization class and the EXP-WA-HIS-RPT procedure and export format
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        String procedureId = (String) getParameter(PROCEDURE_ID);
        m_initErrors = new ArrayList<StateReportValidationError>();

        // Lookup State report source data procedure
        m_reportData = StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);

        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                String init_msg = "Failure initializing data structure in WAImmunizationSummaryPreK";
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
        ReportDataGrid dataGrid = new ReportDataGrid(NUM_FIELDS);

        if (m_reportData != null && m_reportData.open()) {
            try {
                StateReportEntity entity = null;
                while ((entity = m_reportData.next()) != null) {
                    // initializeFieldPositions() should be called after calling next() to switch
                    // current export format definition to
                    // initialize additional field positions if CSV export was selected. Switching
                    // going by method
                    // next() because getCurrentFormatDefinitionId() is overridden in entity class.
                    if (!initializeFieldPositions()) {
                        continue;
                    }
                    entity.preProcess();
                    if (!m_schoolName.equals(entity.getFieldValue(m_schoolPosition))) {
                        // output grid row on school change
                        if (m_schoolName.length() > 0) {
                            outputGrid(dataGrid);
                        }
                        m_schoolName = entity.getFieldValue(m_schoolPosition);
                        m_schoolAddress = entity.getFieldValue(m_schoolAddressPosition);
                        m_schoolCityState = entity.getFieldValue(m_schoolCityStatePosition);
                        m_schoolCity = entity.getFieldValue(m_schoolCityPosition);
                        m_schoolZIP = entity.getFieldValue(m_schoolZipPosition);
                        m_schoolCounty = entity.getFieldValue(m_schoolCountyPosition);
                        m_countsPK = new VaccineCounts(NUM_VACCINES);
                        initCounts();
                    }
                    String gradeLevel = entity.getFieldValue(m_gradePosition);
                    String allStatus = getAllStatus(entity);
                    if (gradeLevel != null && gradeLevel.matches(MATCH_GRADE_PK)) {
                        // Add counts for students K-12
                        m_countsPK.increment(entity, MATCH_ANY_NOT_IMMUNE_STATUS);
                        ++m_countPKEnrolled;

                        if (allStatus.matches(MATCH_ANY_EXEMPT_STATUS)) {
                            ++m_countPKExempt;
                        }
                        if (allStatus.matches(MATCH_STATUS_MEDICAL_EXEMPT)) {
                            ++m_countPKMedicalExempt;
                        } else if (allStatus.matches(MATCH_STATUS_PERSONAL_EXEMPT)) {
                            ++m_countPKPersonalExempt;
                        } else if (allStatus.matches(MATCH_STATUS_RELIGIOUS_EXEMPT)) {
                            ++m_countPKReligiousExempt;
                        } else if (allStatus.matches(MATCH_STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT)) {
                            ++m_countPKReligiousMembExempt;
                        } else if (allStatus.matches(MATCH_STATUS_NON_COMPLIANT)) {
                            ++m_countPKNotCompliant;
                        } else if (allStatus.matches(MATCH_STATUS_CONDITIONAL)) {
                            ++m_countPKConditional;
                        } else {
                            ++m_countPKImmune;
                        }
                    }
                    entity.postProcess();
                }
                if (m_schoolName.length() > 0) {
                    outputGrid(dataGrid);
                }
            } finally {
                m_reportData.close();
            }
        }
        addParameter(PARAM_REPORT_DATE, getParameter(PARAM_REPORT_DATE));
        addParameter(PARAM_DISTRICT_NAME, getOrganization().getName());
        addParameter(PARAM_START_SCHOOL_YEAR, Integer.valueOf(getCurrentContext().getSchoolYear() - 1));
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Provides a String containing a concatenated list of all immunization status codes.
     *
     * @param entity StateReportEntity
     * @return String
     */
    private String getAllStatus(StateReportEntity entity) {
        StringBuilder codes = new StringBuilder(m_vaccinePositions.length + 1);
        for (int i = 0; i < m_vaccinePositions.length; ++i) {
            codes.append(entity.getFieldValue(m_vaccinePositions[i]));
        }
        return codes.toString();
    }

    /**
     * Reset accumulators to zero.
     */
    private void initCounts() {
        m_countPKConditional = 0;
        m_countPKEnrolled = 0;
        m_countPKMedicalExempt = 0;
        m_countPKPersonalExempt = 0;
        m_countPKReligiousExempt = 0;
        m_countPKReligiousMembExempt = 0;
        m_countPKImmune = 0;
        m_countPKNotCompliant = 0;

        m_countPKConditional = 0;
        m_countPKExempt = 0;
        m_countPKImmune = 0;
        m_countPKNotCompliant = 0;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        initReportsFormat();
    }

    /**
     *
     * Sets the field position array for the input data.
     *
     * @return true, if successful
     */
    private boolean initializeFieldPositions() {
        for (int i = 0; i < m_vaccinePositions.length; ++i) {
            m_vaccinePositions[i] = -1;
        }
        for (int pos = 0; pos < m_reportData.getFieldCount(); pos++) {
            FieldDefinition field = m_reportData.getFieldDefinition(pos);
            String fieldName = field.getFieldId();
            if (DATA_FIELD_DT.equals(fieldName)) {
                m_vaccinePositions[INDEX_DT] = pos;
            } else if (DATA_FIELD_P.equals(fieldName)) {
                m_vaccinePositions[INDEX_P] = pos;
            } else if (DATA_FIELD_POLIO.equals(fieldName)) {
                m_vaccinePositions[INDEX_POLIO] = pos;
            } else if (DATA_FIELD_MMR.equals(fieldName)) {
                m_vaccinePositions[INDEX_MMR] = pos;
            } else if (DATA_FIELD_HEP_B.equals(fieldName)) {
                m_vaccinePositions[INDEX_HEP_B] = pos;
            } else if (DATA_FIELD_VARICELLA.equals(fieldName)) {
                m_vaccinePositions[INDEX_VARICELLA] = pos;
            } else if (DATA_FIELD_HIB.equals(fieldName)) {
                m_vaccinePositions[INDEX_HIB] = pos;
            } else if (DATA_FIELD_PCV.equals(fieldName)) {
                m_vaccinePositions[INDEX_PCV] = pos;
            } else if (DATA_FIELD_SCHOOL.equals(fieldName)) {
                m_schoolPosition = pos;
            } else if (DATA_FIELD_SCHOOL_ADDRESS.equals(fieldName)) {
                m_schoolAddressPosition = pos;
            } else if (DATA_FIELD_SCHOOL_CITY_STATE.equals(fieldName)) {
                m_schoolCityStatePosition = pos;
            } else if (DATA_FIELD_GRADE.equals(fieldName)) {
                m_gradePosition = pos;
            } else if (DATA_FIELD_SCHOOL_CITY.equals(fieldName)) {
                m_schoolCityPosition = pos;
            } else if (DATA_FIELD_SCHOOL_ZIP.equals(fieldName)) {
                m_schoolZipPosition = pos;
            } else if (DATA_FIELD_SCHOOL_COUNTY.equals(fieldName)) {
                m_schoolCountyPosition = pos;
            }
        }
        if (m_schoolPosition < 0 || m_gradePosition < 0) {
            return false;
        }
        for (int i = 0; i < m_vaccinePositions.length; ++i) {
            if (m_vaccinePositions[i] < 0) {
                return false;
            }
        }
        return true;
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        String formatPDF = (String) getParameter(INPUT_REPORT_ID_PDF);
        String formatCSV = (String) getParameter(INPUT_REPORT_ID_CSV);
        ToolJob job = this.getJob();

        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
                this.setFormatId(formatCSV);
                getParameters().put("format", formatCSV);
                break;
            case ToolInput.HTML_FORMAT:
                this.setFormatId(formatPDF);
                getParameters().put("format", formatPDF);
                break;
            case ToolInput.PDF_FORMAT:
                this.setFormatId(formatPDF);
                getParameters().put("format", formatPDF);
                break;
            case ToolInput.XLS_FORMAT:
                this.setFormatId(formatPDF);
                getParameters().put("format", formatPDF);
                break;
        }
    }

    /**
     * Outputs the information for a school.
     *
     * @param dataGrid DataGrid
     */
    private void outputGrid(DataGrid dataGrid) {
        dataGrid.append();
        dataGrid.set(FIELD_SCHOOL_ADDRESS, m_schoolAddress);
        dataGrid.set(FIELD_SCHOOL_CITY_STATE, m_schoolCityState);
        dataGrid.set(FIELD_SCHOOL_NAME, m_schoolName);

        dataGrid.set(FIELD_SCHOOL_CITY, m_schoolCity);
        dataGrid.set(FIELD_SCHOOL_ZIP, m_schoolZIP);
        dataGrid.set(FIELD_SCHOOL_COUNTY, m_schoolCounty);

        dataGrid.set(FIELD_ENROLLED, Integer.valueOf(m_countPKEnrolled));
        dataGrid.set(FIELD_IMMUNE_TOTAL, Integer.valueOf(m_countPKImmune));

        dataGrid.set(FIELD_EXEMPT_TOTAL, Integer.valueOf(m_countPKExempt));
        dataGrid.set(FIELD_EXEMPT_MEDICAL, Integer.valueOf(m_countPKMedicalExempt));
        dataGrid.set(FIELD_EXEMPT_PERSONAL, Integer.valueOf(m_countPKPersonalExempt));
        dataGrid.set(FIELD_EXEMPT_RELIGIOUS, Integer.valueOf(m_countPKReligiousExempt));
        dataGrid.set(FIELD_EXEMPT_RELIGIOUS_MEMBERSHIP, Integer.valueOf(m_countPKReligiousMembExempt));

        dataGrid.set(FIELD_CONDITIONAL, Integer.valueOf(m_countPKConditional));
        dataGrid.set(FIELD_OUT_OF_COMPLIANCE, Integer.valueOf(m_countPKNotCompliant));

        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_DT, m_countsPK.getCount(INDEX_DT));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_P, m_countsPK.getCount(INDEX_P));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_POLIO, m_countsPK.getCount(INDEX_POLIO));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_MMR, m_countsPK.getCount(INDEX_MMR));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_HEP_B, m_countsPK.getCount(INDEX_HEP_B));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_VARICELLA, m_countsPK.getCount(INDEX_VARICELLA));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_HIB, m_countsPK.getCount(INDEX_HIB));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_PCV, m_countsPK.getCount(INDEX_PCV));
    }

}
