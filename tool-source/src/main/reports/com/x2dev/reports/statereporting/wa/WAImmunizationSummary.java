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
public class WAImmunizationSummary extends ReportJavaSourceNet {


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
            String tdap611Code = entity.getFieldValue(m_vaccinePositions[INDEX_TDAP611]);
            String pertussisCode = entity.getFieldValue(m_vaccinePositions[INDEX_P]);
            String diptheriaTetanusCode = entity.getFieldValue(m_vaccinePositions[INDEX_DT]);

            if (tdap611Code != null && tdap611Code.matches(match)) {
                if (pertussisCode == null || !pertussisCode.matches(match)) {
                    m_counts[INDEX_P]++;
                }
                if (diptheriaTetanusCode == null || !diptheriaTetanusCode.matches(match)) {
                    m_counts[INDEX_DT]++;
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

    protected static int INDEX_DT = 0;
    protected static int INDEX_P = 1;
    protected static int INDEX_POLIO = 2;
    protected static int INDEX_MMR = 3;
    protected static int INDEX_HEP_B = 4;
    protected static int INDEX_VARICELLA = 5;
    protected static int INDEX_TDAP611 = 6;


    private static final String DATA_FIELD_DT = "Diptheria/Tetanus";
    private static final String DATA_FIELD_GRADE = "Grade level";
    private static final String DATA_FIELD_HEP_B = "Hep B";
    private static final String DATA_FIELD_HIGHEST_GRADE = "Highest Grade";
    private static final String DATA_FIELD_LOWEST_GRADE = "Lowest Grade";
    private static final String DATA_FIELD_MMR = "MMR";
    private static final String DATA_FIELD_P = "Pertussis";
    private static final String DATA_FIELD_POLIO = "Polio";
    private static final String DATA_FIELD_SCHOOL = "School";
    private static final String DATA_FIELD_SCHOOL_ADDRESS = "School Address";
    private static final String DATA_FIELD_SCHOOL_CITY = "School City";
    private static final String DATA_FIELD_SCHOOL_CITY_STATE = "School City/State";
    private static final String DATA_FIELD_SCHOOL_CODE = "School Code";
    private static final String DATA_FIELD_SCHOOL_COUNTY = "School County";
    private static final String DATA_FIELD_SCHOOL_DISTRICT_NAME = "School District Name";
    private static final String DATA_FIELD_SCHOOL_ZIP = "School Zip";
    private static final String DATA_FIELD_TD611 = "Tdap6-11";
    private static final String DATA_FIELD_VARICELLA = "Varicella";

    private static final String FIELD_CONDITIONAL = "conditionalK12";
    private static final String FIELD_CONDITIONAL_6 = "conditional6";
    private static final String FIELD_CONDITIONAL_K = "conditionalK";
    private static final String FIELD_ENROLLED = "enrolled";
    private static final String FIELD_ENROLLED_6 = "enrolled6";
    private static final String FIELD_ENROLLED_K = "enrolledK";
    private static final String FIELD_EXEMPT_6 = "exempt6";
    private static final String FIELD_EXEMPT_6_MEDICAL = "exempt6Medical";
    private static final String FIELD_EXEMPT_6_PERSONAL = "exempt6Personal";
    private static final String FIELD_EXEMPT_6_RELIGIOUS = "exempt6Religious";
    private static final String FIELD_EXEMPT_6_RELIGIOUS_MEMBERSHIP = "exempt6ReligiousMemb";
    private static final String FIELD_EXEMPT_K = "exemptK";
    private static final String FIELD_EXEMPT_K_MEDICAL = "exemptKMedical";
    private static final String FIELD_EXEMPT_K_PERSONAL = "exemptKPersonal";
    private static final String FIELD_EXEMPT_K_RELIGIOUS = "exemptKReligious";
    private static final String FIELD_EXEMPT_K_RELIGIOUS_MEMBERSHIP = "exemptKReligiousMemb";
    private static final String FIELD_EXEMPT_MEDICAL = "exemptMedical";
    private static final String FIELD_EXEMPT_PERSONAL = "exemptPersonal";
    private static final String FIELD_EXEMPT_RELIGIOUS = "exemptReligious";
    private static final String FIELD_EXEMPT_RELIGIOUS_MEMBERSHIP = "exemptReligiousMemb";
    private static final String FIELD_EXEMPT_TOTAL = "exemptTotal";
    private static final String FIELD_IMMUNE_TOTAL = "immuneTotal";
    private static final String FIELD_IMMUNE_6 = "immune6";
    private static final String FIELD_IMMUNE_K = "immuneK";
    private static final String FIELD_OUT_OF_COMPLIANCE = "outOfComplianceK12";
    private static final String FIELD_OUT_OF_COMPLIANCE_6 = "outOfCompliance6";
    private static final String FIELD_OUT_OF_COMPLIANCE_K = "outOfComplianceK";

    private static final String FIELD_SCHOOL_ADDRESS = "schoolAddress";
    private static final String FIELD_SCHOOL_CITY = "schoolCity";
    private static final String FIELD_SCHOOL_CITY_STATE = "schoolCityState";
    private static final String FIELD_SCHOOL_CODE = "schoolCode";
    private static final String FIELD_SCHOOL_COUNTY = "schoolCounty";
    private static final String FIELD_SCHOOL_DISTRICT_NAME = "districtName";
    private static final String FIELD_SCHOOL_HIGHEST_GRADE = "highestGrade";
    private static final String FIELD_SCHOOL_LOWEST_GRADE = "lowestGrade";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String FIELD_SCHOOL_ZIP = "schoolZip";

    private static final String FIELD_VACCINE_EXEMPT_DT = "vaccineExemptDT";
    private static final String FIELD_VACCINE_EXEMPT_P = "vaccineExemptP";
    private static final String FIELD_VACCINE_EXEMPT_POLIO = "vaccineExemptPolio";
    private static final String FIELD_VACCINE_EXEMPT_MMR = "vaccineExemptMMR";
    private static final String FIELD_VACCINE_EXEMPT_HEP_B = "vaccineExemptHepB";
    private static final String FIELD_VACCINE_EXEMPT_VARICELLA = "vaccineExemptVaricella";

    private static final String FIELD_VACCINE_NOT_IMMUNE_DT = "vaccineNotImmuneDT";
    private static final String FIELD_VACCINE_NOT_IMMUNE_P = "vaccineNotImmuneP";
    private static final String FIELD_VACCINE_NOT_IMMUNE_POLIO = "vaccineNotImmunePolio";
    private static final String FIELD_VACCINE_NOT_IMMUNE_MMR = "vaccineNotImmuneMMR";
    private static final String FIELD_VACCINE_NOT_IMMUNE_HEP_B = "vaccineNotImmuneHepB";
    private static final String FIELD_VACCINE_NOT_IMMUNE_VARICELLA = "vaccineNotImmuneVaricella";

    private static final String FIELD_VACCINE_NOT_IMMUNE_K_DT = "vaccineNotImmuneKDT";
    private static final String FIELD_VACCINE_NOT_IMMUNE_K_P = "vaccineNotImmuneKP";
    private static final String FIELD_VACCINE_NOT_IMMUNE_K_POLIO = "vaccineNotImmuneKPolio";
    private static final String FIELD_VACCINE_NOT_IMMUNE_K_MMR = "vaccineNotImmuneKMMR";
    private static final String FIELD_VACCINE_NOT_IMMUNE_K_HEP_B = "vaccineNotImmuneKHepB";
    private static final String FIELD_VACCINE_NOT_IMMUNE_K_VARICELLA = "vaccineNotImmuneKVaricella";

    private static final String FIELD_VACCINE_NOT_IMMUNE_6_DT = "vaccineNotImmune6DT";
    private static final String FIELD_VACCINE_NOT_IMMUNE_6_P = "vaccineNotImmune6P";
    private static final String FIELD_VACCINE_NOT_IMMUNE_6_POLIO = "vaccineNotImmune6Polio";
    private static final String FIELD_VACCINE_NOT_IMMUNE_6_MMR = "vaccineNotImmune6MMR";
    private static final String FIELD_VACCINE_NOT_IMMUNE_6_HEP_B = "vaccineNotImmune6HepB";
    private static final String FIELD_VACCINE_NOT_IMMUNE_6_VARICELLA = "vaccineNotImmune6Varicella";

    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";

    private static String MATCH_ANY_EXEMPT_STATUS = ".*[MPRG].*";
    private static String MATCH_ANY_NOT_IMMUNE_STATUS = ".*[DMNPRG].*";
    private static String MATCH_GRADE_K = "^K1$|^K2$";
    private static String MATCH_GRADE_K12 =
            "^K1$|^K2$|^1$|^01$|^2$|^02$|^3$|^03$|^4$|^04$|^5$|^05$|^6$|^06$|^7$|^07$|^8$|^08$|^9$|^09$|^10$|^11$|^12$";
    private static String MATCH_GRADE_6 = "^6$|^06$";

    // Maintain list of unused codes
    // private static String MATCH_STATUS_COMPLIANT = "C";
    private static String MATCH_STATUS_CONDITIONAL = ".*D.*";
    private static String MATCH_STATUS_MEDICAL_EXEMPT = ".*M.*";
    private static String MATCH_STATUS_NON_COMPLIANT = ".*N.*";
    private static String MATCH_STATUS_PERSONAL_EXEMPT = ".*P.*";
    private static String MATCH_STATUS_RELIGIOUS_EXEMPT = ".*R.*";
    private static String MATCH_STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT = ".*G.*";

    private static int NUM_FIELDS = 46;
    private static int NUM_VACCINES = 7;

    private static final String PARAM_DISTRICT_NAME = "districtName";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SCHOOL_YEAR = "school Year";


    private static final String PROCEDURE_ID = "procedureId";

    protected int m_countK12Conditional;
    protected int m_countK12Enrolled;
    protected int m_countK12Exempt;
    protected int m_countK12MedicalExempt;
    protected int m_countK12PersonalExempt;
    protected int m_countK12ReligiousExempt;
    protected int m_countK12ReligiousMembExempt;
    protected int m_countK12Immune;
    protected int m_countK12NotCompliant;

    protected int m_count6Conditional;
    protected int m_count6Exempt;
    protected int m_count6ExemptMedical;
    protected int m_count6ExemptPersonal;
    protected int m_count6ExemptReligious;
    protected int m_count6ExemptReligiousMemb;
    protected int m_count6Immune;
    protected int m_count6NotCompliant;

    protected int m_countKConditional;
    protected int m_countKExempt;
    protected int m_countKExemptMedical;
    protected int m_countKExemptPersonal;
    protected int m_countKExemptReligious;
    protected int m_countKExemptReligiousMemb;
    protected int m_countKImmune;
    protected int m_countKNotCompliant;

    protected VaccineCounts m_counts6;
    protected VaccineCounts m_countsK;
    protected VaccineCounts m_countsK12;
    protected VaccineCounts m_countsNotImmune;

    protected int m_gradePosition = -1;
    protected int m_schoolDistrictNamePosition = -1;

    protected int m_schoolAddressPosition = -1;
    protected int m_schoolCityPosition = -1;
    protected int m_schoolCityStatePosition = -1;
    protected int m_schoolCodePosition = -1;
    protected int m_schoolCountyPosition = -1;
    protected int m_schoolHighestGradePosition = -1;
    protected int m_schoolLowestGradePosition = -1;
    protected int m_schoolPosition = -1;
    protected int m_schoolZipPosition = -1;

    protected int[] m_vaccinePositions = new int[NUM_VACCINES];

    private String m_districtName = "";
    private Collection<StateReportValidationError> m_initErrors = null;
    private StateReportData m_reportData = null;

    private String m_schoolAddress = "";
    private String m_schoolCity = "";
    private String m_schoolCityState = "";
    private String m_schoolCode = "";
    private String m_schoolCounty = "";
    private String m_schoolHighestGrade = "";
    private String m_schoolLowestGrade = "";
    private String m_schoolName = "";
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
                String init_msg = "Failure initializing data structure in WAImmunizationSummary";
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
                        m_schoolCode = entity.getFieldValue(m_schoolCodePosition);
                        m_schoolCity = entity.getFieldValue(m_schoolCityPosition);
                        m_schoolZIP = entity.getFieldValue(m_schoolZipPosition);
                        m_schoolCounty = entity.getFieldValue(m_schoolCountyPosition);
                        m_districtName = entity.getFieldValue(m_schoolDistrictNamePosition);
                        m_schoolLowestGrade = entity.getFieldValue(m_schoolLowestGradePosition);
                        m_schoolHighestGrade = entity.getFieldValue(m_schoolHighestGradePosition);
                        m_countsNotImmune = new VaccineCounts(NUM_VACCINES);
                        m_countsK12 = new VaccineCounts(NUM_VACCINES);
                        m_countsK = new VaccineCounts(NUM_VACCINES);
                        m_counts6 = new VaccineCounts(NUM_VACCINES);
                        initCounts();
                    }
                    String gradeLevel = entity.getFieldValue(m_gradePosition);
                    String allStatus = getAllStatus(entity);

                    if (gradeLevel != null && gradeLevel.matches(MATCH_GRADE_K12)) {
                        // Add counts for students K-12
                        m_countsK12.increment(entity, MATCH_ANY_EXEMPT_STATUS);
                        m_countsNotImmune.increment(entity, MATCH_ANY_NOT_IMMUNE_STATUS);
                        ++m_countK12Enrolled;
                        if (allStatus.matches(MATCH_ANY_EXEMPT_STATUS)) {
                            ++m_countK12Exempt;
                            if (allStatus.matches(MATCH_STATUS_MEDICAL_EXEMPT)) {
                                ++m_countK12MedicalExempt;
                            }
                            if (allStatus.matches(MATCH_STATUS_PERSONAL_EXEMPT)) {
                                ++m_countK12PersonalExempt;
                            }
                            if (allStatus.matches(MATCH_STATUS_RELIGIOUS_EXEMPT)) {
                                ++m_countK12ReligiousExempt;
                            }
                            if (allStatus.matches(MATCH_STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT)) {
                                ++m_countK12ReligiousMembExempt;
                            }
                        } else if (allStatus.matches(MATCH_STATUS_CONDITIONAL)) {
                            ++m_countK12Conditional;
                        } else if (allStatus.matches(MATCH_STATUS_NON_COMPLIANT)) {
                            ++m_countK12NotCompliant;
                        } else {
                            ++m_countK12Immune;
                        }
                    }
                    if (gradeLevel != null && gradeLevel.matches(MATCH_GRADE_K)) {
                        // Add counts for K students
                        m_countsK.increment(entity, MATCH_ANY_NOT_IMMUNE_STATUS);
                        if (allStatus.matches(MATCH_ANY_EXEMPT_STATUS)) {
                            ++m_countKExempt;
                            if (allStatus.matches(MATCH_STATUS_MEDICAL_EXEMPT)) {
                                ++m_countKExemptMedical;
                            }
                            if (allStatus.matches(MATCH_STATUS_PERSONAL_EXEMPT)) {
                                ++m_countKExemptPersonal;
                            }
                            if (allStatus.matches(MATCH_STATUS_RELIGIOUS_EXEMPT)) {
                                ++m_countKExemptReligious;
                            }
                            if (allStatus.matches(MATCH_STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT)) {
                                ++m_countKExemptReligiousMemb;
                            }
                        } else if (allStatus.matches(MATCH_STATUS_CONDITIONAL)) {
                            ++m_countKConditional;
                        } else if (allStatus.matches(MATCH_STATUS_NON_COMPLIANT)) {
                            ++m_countKNotCompliant;
                        } else {
                            ++m_countKImmune;
                        }
                    }
                    if (gradeLevel != null && gradeLevel.matches(MATCH_GRADE_6)) {
                        // Add counts for grade 6 students
                        m_counts6.increment(entity, MATCH_ANY_NOT_IMMUNE_STATUS);
                        if (allStatus.matches(MATCH_ANY_EXEMPT_STATUS)) {
                            ++m_count6Exempt;
                            if (allStatus.matches(MATCH_STATUS_MEDICAL_EXEMPT)) {
                                ++m_count6ExemptMedical;
                            }
                            if (allStatus.matches(MATCH_STATUS_PERSONAL_EXEMPT)) {
                                ++m_count6ExemptPersonal;
                            }
                            if (allStatus.matches(MATCH_STATUS_RELIGIOUS_EXEMPT)) {
                                ++m_count6ExemptReligious;
                            }
                            if (allStatus.matches(MATCH_STATUS_RELIGIOUS_MEMBERSHIP_EXEMPT)) {
                                ++m_count6ExemptReligiousMemb;
                            }
                        } else if (allStatus.matches(MATCH_STATUS_CONDITIONAL)) {
                            ++m_count6Conditional;
                        } else if (allStatus.matches(MATCH_STATUS_NON_COMPLIANT)) {
                            ++m_count6NotCompliant;
                        } else {
                            ++m_count6Immune;
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
        addParameter(PARAM_SCHOOL_YEAR, getCurrentContext().getContextId());
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
        m_countK12Conditional = 0;
        m_countK12Enrolled = 0;
        m_countK12Exempt = 0;
        m_countK12MedicalExempt = 0;
        m_countK12PersonalExempt = 0;
        m_countK12ReligiousExempt = 0;
        m_countK12ReligiousMembExempt = 0;
        m_countK12Immune = 0;
        m_countK12NotCompliant = 0;

        m_count6Conditional = 0;
        m_count6Exempt = 0;
        m_count6Immune = 0;
        m_count6NotCompliant = 0;
        m_count6ExemptMedical = 0;
        m_count6ExemptPersonal = 0;
        m_count6ExemptReligious = 0;
        m_count6ExemptReligiousMemb = 0;

        m_countKConditional = 0;
        m_countKExempt = 0;
        m_countKImmune = 0;
        m_countKNotCompliant = 0;
        m_countKExemptMedical = 0;
        m_countKExemptPersonal = 0;
        m_countKExemptReligious = 0;
        m_countKExemptReligiousMemb = 0;
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
            } else if (DATA_FIELD_TD611.equals(fieldName)) {
                m_vaccinePositions[INDEX_TDAP611] = pos;
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
            } else if (DATA_FIELD_SCHOOL_CODE.equals(fieldName)) {
                m_schoolCodePosition = pos;
            } else if (DATA_FIELD_SCHOOL_ZIP.equals(fieldName)) {
                m_schoolZipPosition = pos;
            } else if (DATA_FIELD_SCHOOL_COUNTY.equals(fieldName)) {
                m_schoolCountyPosition = pos;
            } else if (DATA_FIELD_SCHOOL_DISTRICT_NAME.equals(fieldName)) {
                m_schoolDistrictNamePosition = pos;
            } else if (DATA_FIELD_LOWEST_GRADE.equals(fieldName)) {
                m_schoolLowestGradePosition = pos;
            } else if (DATA_FIELD_HIGHEST_GRADE.equals(fieldName)) {
                m_schoolHighestGradePosition = pos;
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

        dataGrid.set(FIELD_SCHOOL_CODE, m_schoolCode);
        dataGrid.set(FIELD_SCHOOL_CITY, m_schoolCity);
        dataGrid.set(FIELD_SCHOOL_ZIP, m_schoolZIP);
        dataGrid.set(FIELD_SCHOOL_COUNTY, m_schoolCounty);
        dataGrid.set(FIELD_SCHOOL_DISTRICT_NAME, m_districtName);
        dataGrid.set(FIELD_SCHOOL_LOWEST_GRADE, m_schoolLowestGrade);
        dataGrid.set(FIELD_SCHOOL_HIGHEST_GRADE, m_schoolHighestGrade);

        dataGrid.set(FIELD_ENROLLED, Integer.valueOf(m_countK12Enrolled));

        dataGrid.set(FIELD_EXEMPT_TOTAL, Integer.valueOf(m_countK12Exempt));
        dataGrid.set(FIELD_IMMUNE_TOTAL, Integer.valueOf(m_countK12Immune));
        dataGrid.set(FIELD_EXEMPT_MEDICAL, Integer.valueOf(m_countK12MedicalExempt));
        dataGrid.set(FIELD_EXEMPT_PERSONAL, Integer.valueOf(m_countK12PersonalExempt));
        dataGrid.set(FIELD_EXEMPT_RELIGIOUS, Integer.valueOf(m_countK12ReligiousExempt));
        dataGrid.set(FIELD_EXEMPT_RELIGIOUS_MEMBERSHIP, Integer.valueOf(m_countK12ReligiousMembExempt));
        dataGrid.set(FIELD_CONDITIONAL, Integer.valueOf(m_countK12Conditional));
        dataGrid.set(FIELD_OUT_OF_COMPLIANCE, Integer.valueOf(m_countK12NotCompliant));

        dataGrid.set(FIELD_ENROLLED_K,
                Integer.valueOf(m_countKImmune + m_countKExempt + m_countKConditional + m_countKNotCompliant));
        dataGrid.set(FIELD_IMMUNE_K, Integer.valueOf(m_countKImmune));
        dataGrid.set(FIELD_EXEMPT_K, Integer.valueOf(m_countKExempt));
        dataGrid.set(FIELD_EXEMPT_K_MEDICAL, Integer.valueOf(m_countKExemptMedical));
        dataGrid.set(FIELD_EXEMPT_K_PERSONAL, Integer.valueOf(m_countKExemptPersonal));
        dataGrid.set(FIELD_EXEMPT_K_RELIGIOUS, Integer.valueOf(m_countKExemptReligious));
        dataGrid.set(FIELD_EXEMPT_K_RELIGIOUS_MEMBERSHIP, Integer.valueOf(m_countKExemptReligiousMemb));
        dataGrid.set(FIELD_CONDITIONAL_K, Integer.valueOf(m_countKConditional));
        dataGrid.set(FIELD_OUT_OF_COMPLIANCE_K, Integer.valueOf(m_countKNotCompliant));

        dataGrid.set(FIELD_ENROLLED_6,
                Integer.valueOf(m_count6Immune + m_count6Exempt + m_count6Conditional + m_count6NotCompliant));
        dataGrid.set(FIELD_IMMUNE_6, Integer.valueOf(m_count6Immune));
        dataGrid.set(FIELD_EXEMPT_6, Integer.valueOf(m_count6Exempt));
        dataGrid.set(FIELD_EXEMPT_6_MEDICAL, Integer.valueOf(m_count6ExemptMedical));
        dataGrid.set(FIELD_EXEMPT_6_PERSONAL, Integer.valueOf(m_count6ExemptPersonal));
        dataGrid.set(FIELD_EXEMPT_6_RELIGIOUS, Integer.valueOf(m_count6ExemptReligious));
        dataGrid.set(FIELD_EXEMPT_6_RELIGIOUS_MEMBERSHIP, Integer.valueOf(m_count6ExemptReligiousMemb));
        dataGrid.set(FIELD_CONDITIONAL_6, Integer.valueOf(m_count6Conditional));
        dataGrid.set(FIELD_OUT_OF_COMPLIANCE_6, Integer.valueOf(m_count6NotCompliant));

        dataGrid.set(FIELD_VACCINE_EXEMPT_DT, m_countsK12.getCount(INDEX_DT));
        dataGrid.set(FIELD_VACCINE_EXEMPT_P, m_countsK12.getCount(INDEX_P));
        dataGrid.set(FIELD_VACCINE_EXEMPT_POLIO, m_countsK12.getCount(INDEX_POLIO));
        dataGrid.set(FIELD_VACCINE_EXEMPT_MMR, m_countsK12.getCount(INDEX_MMR));
        dataGrid.set(FIELD_VACCINE_EXEMPT_HEP_B, m_countsK12.getCount(INDEX_HEP_B));
        dataGrid.set(FIELD_VACCINE_EXEMPT_VARICELLA, m_countsK12.getCount(INDEX_VARICELLA));

        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_DT, m_countsNotImmune.getCount(INDEX_DT));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_P, m_countsNotImmune.getCount(INDEX_P));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_POLIO, m_countsNotImmune.getCount(INDEX_POLIO));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_MMR, m_countsNotImmune.getCount(INDEX_MMR));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_HEP_B, m_countsNotImmune.getCount(INDEX_HEP_B));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_VARICELLA, m_countsNotImmune.getCount(INDEX_VARICELLA));

        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_K_DT, m_countsK.getCount(INDEX_DT));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_K_P, m_countsK.getCount(INDEX_P));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_K_POLIO, m_countsK.getCount(INDEX_POLIO));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_K_MMR, m_countsK.getCount(INDEX_MMR));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_K_HEP_B, m_countsK.getCount(INDEX_HEP_B));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_K_VARICELLA, m_countsK.getCount(INDEX_VARICELLA));

        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_6_DT, m_counts6.getCount(INDEX_DT));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_6_P, m_counts6.getCount(INDEX_P));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_6_POLIO, m_counts6.getCount(INDEX_POLIO));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_6_MMR, m_counts6.getCount(INDEX_MMR));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_6_HEP_B, m_counts6.getCount(INDEX_HEP_B));
        dataGrid.set(FIELD_VACCINE_NOT_IMMUNE_6_VARICELLA, m_counts6.getCount(INDEX_VARICELLA));
    }

}
