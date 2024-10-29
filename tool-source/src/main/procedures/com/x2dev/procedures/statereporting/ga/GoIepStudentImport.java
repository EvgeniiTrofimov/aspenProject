/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2022 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisReferenceTable;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * GO IEP Student Data Import for Laurens County, GA
 */
public class GoIepStudentImport extends TextImportJavaSource {
    private static final long serialVersionUID = 1L;

    /*
     * Constants
     */
    protected static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";

    /*
     * Aliases
     */
    private static final String ALIAS_STD_GTID = "GTID";
    private static final String STUDENT_PRIM_EXCP_ALIAS = "DOE Primary Exceptionality";
    private static final String STUDENT_TOT_SVC_M_ALIAS = "DOE Total Service Minutes";
    private static final String STUDENT_GA_TESTID_ALIAS = "GTID";
    private static final String STUDENT_GAA_FLAG = "DOE GAA Flag";
    private static final String STUDENT_GAA_START_DATE = "all-std-GAAEligibilityDate";
    private static final String STUDENT_GAA_END_DATE = "all-std-GAAEligibilityEndDate";
    private static final String STUDENT_SPED_ENVRMNT = "DOE SPED Environment";

    /*
     * Input parameters
     */
    private static final String PREVIEW_MODE_PARAM = "previewMode";
    private static final String VERBOSE_OUTPUT_PARAM = "verboseOutput";

    /*
     * Record indexes
     */
    private static final int INDEX_SYSTEMCODE = 0;
    private static final int INDEX_FISCAL_YEAR = 1;
    private static final int INDEX_SCHOOL_CODE = 2;
    private static final int INDEX_SVC_MINS_PER_WEEK = 3;
    private static final int INDEX_REPORT_TYPE = 4;
    private static final int INDEX_MILD_INTL_DISAB_P = 5;
    private static final int INDEX_MOD_INTL_DISAB_Q = 6;
    private static final int INDEX_SEV_INTL_DISAB_R = 7;
    private static final int INDEX_PROF_INTL_DISAB_S = 8;
    private static final int INDEX_EMOT_BEHAV_DISOR_T = 9;
    private static final int INDEX_SPEC_LEARN_DISAB_U = 10;
    private static final int INDEX_ORTHOP_IMPAIRMENT_V = 11;
    private static final int INDEX_HEAR_IMPAIRMENT_W = 12;
    private static final int INDEX_DEAF_X = 13;
    private static final int INDEX_OTHER_HEALTH_IMP_Y = 14;
    private static final int INDEX_VISUAL_IMPAIRMENT_Z = 15;
    private static final int INDEX_DEAF_BLIND_2 = 16;
    private static final int INDEX_SPEECH_LANG_IMP_3 = 17;
    private static final int INDEX_AUTISM_6 = 18;
    private static final int INDEX_TRM_BRAIN_INJURY_7 = 19;
    private static final int INDEX_PRE_SKL_SPED_SDD_8 = 20;
    private static final int INDEX_PRIMARY_DISABILITY = 21;
    private static final int INDEX_SECONDARY_DISABILITY = 22;
    private static final int INDEX_PRIM_DISAB_CODE = 23;
    private static final int INDEX_DISAB_PROGRAM_CODE = 24;
    private static final int INDEX_FTE_PROGRAM_CODE = 25;
    private static final int INDEX_RELATED_SERVICES = 26;
    private static final int INDEX_ADAPTED_PE_A = 27;
    private static final int INDEX_AUDIOLOGY_B = 28;
    private static final int INDEX_COUNSELING_C = 29;
    private static final int INDEX_DIAGNOSTIC_SVCS_D = 30;
    private static final int INDEX_OCC_THERAPY_E = 31;
    private static final int INDEX_PHYS_THERAPY_F = 32;
    private static final int INDEX_PSYCHOLOGY_SVCS_G = 33;
    private static final int INDEX_INTERPRETER_H = 34;
    private static final int INDEX_SKL_HEALTH_NURSE_I = 35;
    private static final int INDEX_SKL_SOCIAL_WORK_J = 36;
    private static final int INDEX_SPECIAL_TRANSP_K = 37;
    private static final int INDEX_ORIENT_MOBILITY_L = 38;
    private static final int INDEX_SPEECH_THERAPY_3 = 39;
    private static final int INDEX_IEP_TYPE = 40;
    private static final int INDEX_GTID = 41;
    private static final int INDEX_EXIT_EVENT = 42;
    private static final int INDEX_EXIT_DATE = 43;
    private static final int INDEX_GAA = 44;
    private static final int INDEX_GAA_ENTERED_DATE = 45;
    private static final int INDEX_GAA_EXITED_DATE = 46;
    private static final int INDEX_LAST_NAME = 47;
    private static final int INDEX_FIRST_NAME = 48;
    private static final int INDEX_GRADE = 49;
    private static final int INDEX_CASE_MANAGER = 50;
    private static final int INDEX_SPED_ENVIRNMNT_CODE = 51;

    /*
     * Member variables
     */
    private X2Broker m_broker;
    private int m_errorCount = 0;
    private int m_insertCount = 0;
    private int m_matchCount = 0;
    private List<String> m_messages;
    private boolean m_previewMode;
    private int m_skipCount = 0;
    private File m_sourceFile;
    private boolean m_verboseOutput;
    private int m_updateCount = 0;

    /**
     * Maps
     */
    private Map<String, ReferenceCode> m_disabilityCodesByStateCode;
    private Map<String, ReferenceCode> m_hoursPerWeekCodesByStateCode;
    private Map<String, ReferenceCode> m_spedEnvironmentCodesByStateCode;
    private Map<String, Collection<IepDisability>> m_studentDisabilityRecordsByGtid;
    private Map<String, SisStudent> m_studentRecordsByGtid;

    /**
     * Print out the logs to the user
     *
     * @throws X2BaseException
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        buffer.append("  Results" + '\n');
        buffer.append("------------------------------------------------\n");
        buffer.append("   File name: " + m_sourceFile.toString() + '\n');
        buffer.append("   Match count: " + m_matchCount + '\n');
        buffer.append("   Update count: " + m_updateCount + '\n');
        buffer.append("   Insert count: " + m_insertCount + '\n');
        buffer.append("   Skipped count: " + m_skipCount + '\n');
        buffer.append("   Error count: " + m_errorCount + '\n');
        buffer.append("------------------------------------------------\n");

        buffer.append(StringUtils.convertCollectionToDelimitedString(m_messages, "\n"));

        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_broker = getBroker();
        m_messages = new LinkedList<String>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField fieldStdGTID = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_GTID);
        if (fieldStdGTID != null) {
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class);
            m_studentRecordsByGtid = m_broker.getMapByQuery(studentQuery, fieldStdGTID.getJavaName(), 4096);

            X2Criteria studentDisabilityCriteria = new X2Criteria();
            studentDisabilityCriteria.addIn(IepDisability.REL_STUDENT + PATH_DELIMITER + fieldStdGTID.getJavaName(),
                    m_studentRecordsByGtid.keySet());
            QueryByCriteria studentDisabilityQuery =
                    new QueryByCriteria(IepDisability.class, studentDisabilityCriteria);
            m_studentDisabilityRecordsByGtid = m_broker.getGroupedCollectionByQuery(studentDisabilityQuery,
                    IepDisability.REL_STUDENT + PATH_DELIMITER + fieldStdGTID.getJavaName(), 4096);

            Criteria serviceHoursCriteria = new Criteria();
            serviceHoursCriteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + SisReferenceTable.COL_USER_NAME,
                    "Hours Codes");
            QueryByCriteria serviceHoursQuery = new QueryByCriteria(ReferenceCode.class, serviceHoursCriteria);
            m_hoursPerWeekCodesByStateCode =
                    m_broker.getMapByQuery(serviceHoursQuery, ReferenceCode.COL_STATE_CODE, 16);

            Criteria primaryDisabilityCriteria = new Criteria();
            primaryDisabilityCriteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + SisReferenceTable.COL_USER_NAME,
                    "DOE Exceptionality Codes");
            QueryByCriteria primaryDisabilityQuery =
                    new QueryByCriteria(ReferenceCode.class, primaryDisabilityCriteria);
            m_disabilityCodesByStateCode =
                    m_broker.getMapByQuery(primaryDisabilityQuery, ReferenceCode.COL_STATE_CODE, 32);
            /// TODO: find most-recent IEP

            Criteria spedEnvironmentCriteria = new Criteria();
            spedEnvironmentCriteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + SisReferenceTable.COL_USER_NAME,
                    "Instructional Setting Codes");
            QueryByCriteria spedEnvironmentQuery = new QueryByCriteria(ReferenceCode.class, spedEnvironmentCriteria);
            m_spedEnvironmentCodesByStateCode =
                    m_broker.getMapByQuery(spedEnvironmentQuery, ReferenceCode.COL_STATE_CODE, 32);

            m_previewMode = ((Boolean) getParameter(PREVIEW_MODE_PARAM)).booleanValue();
            m_messages.add("Preview Mode: " + m_previewMode + "\n");
            m_verboseOutput = ((Boolean) getParameter(VERBOSE_OUTPUT_PARAM)).booleanValue();
            m_messages.add("Verbose Output: " + m_verboseOutput + "\n");
            m_messages.add("\n");
        } else {
            m_messages
                    .add(LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP));
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 52;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        setStartingLine(2);
        setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
        // setUseValueWrappers(false);
        setValueWrapper('\"');

        m_sourceFile = sourceFile;
        super.importData(m_sourceFile);
    }

    /**
     * Import resource records
     *
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        // skip header row
        if (lineNumber > 1) {
            String studentGTID = record.get(INDEX_GTID);
            if (studentGTID != null) {
                studentGTID = studentGTID.trim();
            }

            if (StringUtils.isEmpty(studentGTID)) {
                m_errorCount++;
                m_messages.add("Missing Student GTID on line: " + lineNumber + "\n");
            } else if (m_studentRecordsByGtid.get(studentGTID) == null) {
                m_errorCount++;
                m_messages
                        .add("Unable to recognize student with GTID " + studentGTID + " on line: " + lineNumber + "\n");
            } else {
                m_matchCount++;

                SisStudent student = m_studentRecordsByGtid.get(studentGTID);

                String updatedFields = "";
                updatedFields += updateServiceHoursColumnP(student, record.get(INDEX_MILD_INTL_DISAB_P));
                updatedFields += updateServiceHoursColumnQ(student, record.get(INDEX_MOD_INTL_DISAB_Q));
                updatedFields += updateServiceHoursColumnR(student, record.get(INDEX_SEV_INTL_DISAB_R));
                updatedFields += updateServiceHoursColumnS(student, record.get(INDEX_PROF_INTL_DISAB_S));
                updatedFields += updateServiceHoursColumnT(student, record.get(INDEX_EMOT_BEHAV_DISOR_T));
                updatedFields += updateServiceHoursColumnU(student, record.get(INDEX_SPEC_LEARN_DISAB_U));
                updatedFields += updateServiceHoursColumnV(student, record.get(INDEX_ORTHOP_IMPAIRMENT_V));
                updatedFields += updateServiceHoursColumnW(student, record.get(INDEX_HEAR_IMPAIRMENT_W));
                updatedFields += updateServiceHoursColumnX(student, record.get(INDEX_DEAF_X));
                updatedFields += updateServiceHoursColumnY(student, record.get(INDEX_OTHER_HEALTH_IMP_Y));
                updatedFields += updateServiceHoursColumnZ(student, record.get(INDEX_VISUAL_IMPAIRMENT_Z));
                // updatedFields += updateServiceHoursColumn1(student, record.get(
                updatedFields += updateServiceHoursColumn2(student, record.get(INDEX_DEAF_BLIND_2));
                updatedFields += updateServiceHoursColumn3(student, record.get(INDEX_SPEECH_LANG_IMP_3));
                updatedFields += updateServiceHoursColumn6(student, record.get(INDEX_AUTISM_6));
                updatedFields += updateServiceHoursColumn7(student, record.get(INDEX_TRM_BRAIN_INJURY_7));
                updatedFields += updateServiceHoursColumn8(student, record.get(INDEX_PRE_SKL_SPED_SDD_8));
                updatedFields += updateServiceHoursColumnA(student, record.get(INDEX_ADAPTED_PE_A));
                updatedFields += updateServiceHoursColumnB(student, record.get(INDEX_AUDIOLOGY_B));
                updatedFields += updateServiceHoursColumnC(student, record.get(INDEX_COUNSELING_C));
                updatedFields += updateServiceHoursColumnD(student, record.get(INDEX_DIAGNOSTIC_SVCS_D));
                updatedFields += updateServiceHoursColumnE(student, record.get(INDEX_OCC_THERAPY_E));
                updatedFields += updateServiceHoursColumnF(student, record.get(INDEX_PHYS_THERAPY_F));
                updatedFields += updateServiceHoursColumnG(student, record.get(INDEX_PSYCHOLOGY_SVCS_G));
                updatedFields += updateServiceHoursColumnH(student, record.get(INDEX_INTERPRETER_H));
                updatedFields += updateServiceHoursColumnI(student, record.get(INDEX_SKL_HEALTH_NURSE_I));
                updatedFields += updateServiceHoursColumnJ(student, record.get(INDEX_SKL_SOCIAL_WORK_J));
                updatedFields += updateServiceHoursColumnK(student, record.get(INDEX_SPECIAL_TRANSP_K));
                updatedFields += updateServiceHoursColumnL(student, record.get(INDEX_ORIENT_MOBILITY_L));
                updatedFields += updateServiceHoursColumn3R(student, record.get(INDEX_SPEECH_THERAPY_3));

                String primaryExceptionalityStateCode = record.get(INDEX_PRIM_DISAB_CODE);
                updatedFields +=
                        updatePrimaryExceptionality(student, primaryExceptionalityStateCode, STUDENT_PRIM_EXCP_ALIAS);
                // updatedFields += updateCreatePrimaryDisabilityRecord(student,
                // primaryExceptionalityStateCode);
                /// TODO: updateCreateSeceondaryDisabilityRecord

                updatedFields += updateGaaFlag(student, record.get(INDEX_GAA), STUDENT_GAA_FLAG);

                updatedFields += updateGaaDate(student, record.get(INDEX_GAA_ENTERED_DATE), STUDENT_GAA_START_DATE);
                updatedFields += updateGaaDate(student, record.get(INDEX_GAA_EXITED_DATE), STUDENT_GAA_END_DATE);
                updatedFields +=
                        updateSpedEnvironment(student, record.get(INDEX_SPED_ENVIRNMNT_CODE), STUDENT_SPED_ENVRMNT);

                updatedFields += updateWeeklyServiceMinutes(student, record.get(INDEX_SVC_MINS_PER_WEEK),
                        STUDENT_TOT_SVC_M_ALIAS);

                if (!StringUtils.isEmpty(updatedFields)) {
                    updatedFields = updatedFields.trim();
                    if (updatedFields.endsWith(",")) {
                        updatedFields = updatedFields.substring(0, updatedFields.length() - 1);
                    }
                    m_updateCount++;
                    m_messages.add("Updated the following fields for student " + studentGTID + ": " + updatedFields);
                } else {
                    m_skipCount++;
                    m_messages.add("No updates needed for student " + studentGTID);
                }

                if (!m_previewMode && student.isDirty()) {
                    m_broker.saveBeanForced(student, true, false);
                }

            }
        }
        m_messages.add("\n");
    }

    private String updateCreatePrimaryDisabilityRecord(SisStudent student, String primaryDisablityStateCode) {
        String changedCode = "";

        ReferenceCode primaryDisabilityRefCode = m_disabilityCodesByStateCode.get(primaryDisablityStateCode);
        String newPrimaryDisabilityCode = "";
        if (primaryDisabilityRefCode != null) {
            newPrimaryDisabilityCode = primaryDisabilityRefCode.getCode();
        }
        if (newPrimaryDisabilityCode == null) {
            newPrimaryDisabilityCode = "";
        }

        if (student != null) {
            String studentGaTestId = (String) student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS);
            if (studentGaTestId != null) {
                Collection<IepDisability> studentDisabilityRecords =
                        m_studentDisabilityRecordsByGtid.get(studentGaTestId);
                boolean primaryIepDisabilityFound = false;
                if (studentDisabilityRecords != null && !studentDisabilityRecords.isEmpty()) {
                    for (IepDisability studentDisabilityRecord : studentDisabilityRecords) {
                        if (studentDisabilityRecord.getPrimaryIndicator()) {
                            m_matchCount++;
                            String oldPrimaryDisabilityCode = studentDisabilityRecord.getDisabilityCode();
                            if (oldPrimaryDisabilityCode == null) {
                                oldPrimaryDisabilityCode = "";
                            }

                            if (!oldPrimaryDisabilityCode.equalsIgnoreCase(newPrimaryDisabilityCode)) {
                                if (!m_previewMode) {
                                    studentDisabilityRecord.setDisabilityCode(newPrimaryDisabilityCode);
                                    m_broker.saveBeanForced(studentDisabilityRecord, true, false);
                                }
                                m_messages.add("Updated Primary Disablity record for student:" + studentGaTestId
                                        + ", new code:" + newPrimaryDisabilityCode + ", old code:"
                                        + oldPrimaryDisabilityCode);
                                m_updateCount++;
                                changedCode = "Student Disability record, ";
                            } else {
                                m_skipCount++;
                            }

                            primaryIepDisabilityFound = true;
                            break;
                        }
                    }
                }

                if (!primaryIepDisabilityFound) {
                    if (!m_previewMode) {
                        IepDisability primaryDisability =
                                X2BaseBean.newInstance(IepDisability.class, m_broker.getPersistenceKey());
                        primaryDisability.setDisabilityCode(newPrimaryDisabilityCode);
                        m_broker.saveBeanForced(primaryDisability, true, false);
                    }
                    m_messages.add("Created new Primary Disablity record for student:" + studentGaTestId + ", code:"
                            + newPrimaryDisabilityCode);
                    m_insertCount++;
                    changedCode = "Student Disability record, ";
                }
            }
        }

        return changedCode;
    }

    private String updateGaaDate(SisStudent student, String gaaDate, String columnAlias) {
        String changedCode = "";

        if (student != null) {
            String oldCode = (String) student.getFieldValueByAlias(columnAlias);
            if (oldCode == null) {
                oldCode = "";
            }

            String newCode = "";
            if (!StringUtils.isEmpty(gaaDate)) {
                try {
                    SimpleDateFormat gaaDateFormat = new SimpleDateFormat("yyyyMMdd");
                    PlainDate gaaPlainDate = new PlainDate(gaaDateFormat.parse(gaaDate));
                    newCode = gaaPlainDate.toString();
                } catch (Exception e) {
                    m_messages.add("Error processing GAA Date " + gaaDate + "; no changes made");
                    newCode = oldCode;
                }
            }

            if (m_verboseOutput) {
                m_messages.add("Student:" + (String) student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS) + ", field:"
                        + columnAlias + ", old GAA Flag:" + oldCode + ", new GAA Flag:" + newCode);
            }

            if (!oldCode.equals(newCode)) {
                student.setFieldValueByAlias(columnAlias, newCode);
                changedCode = columnAlias + ", ";
            }
        }

        return changedCode;
    }

    private String updateGaaFlag(SisStudent student, String gaaCode, String columnAlias) {
        String changedCode = "";

        if (student != null) {
            String oldCode = (String) student.getFieldValueByAlias(columnAlias);
            if (oldCode == null) {
                oldCode = "";
            }

            String newCode = "";
            if (!StringUtils.isEmpty(gaaCode)) {
                if (gaaCode.equals("Y")) {
                    newCode = "Yes";
                } else if (gaaCode.equals("N")) {
                    newCode = "No";
                }
            }

            if (m_verboseOutput) {
                m_messages.add("Student:" + (String) student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS) + ", field:"
                        + columnAlias + ", old GAA Flag:" + oldCode + ", new GAA Flag:" + newCode);
            }

            if (!oldCode.equals(newCode)) {
                student.setFieldValueByAlias(columnAlias, newCode);
                changedCode = columnAlias + ", ";
            }
        }

        return changedCode;
    }

    private String updatePrimaryExceptionality(SisStudent student, String serviceCode, String columnAlias) {
        String changedCode = "";

        if (student != null) {
            String oldCode = (String) student.getFieldValueByAlias(columnAlias);
            if (oldCode == null) {
                oldCode = "";
            }

            String newCode = "";
            if (!StringUtils.isEmpty(serviceCode)) {
                ReferenceCode refCode = m_disabilityCodesByStateCode.get(serviceCode);
                if (refCode != null) {
                    newCode = refCode.getCode();
                }
            }

            if (m_verboseOutput) {
                m_messages.add("Student:" + (String) student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS) + ", field:"
                        + columnAlias + ", old disability code:" + oldCode + ", new disability code:" + newCode);
            }

            if (!oldCode.equals(newCode)) {
                student.setFieldValueByAlias(columnAlias, newCode);
                changedCode = columnAlias + ", ";
            }
        }

        return changedCode;
    }

    private String updateServiceHours(SisStudent student, String serviceCode, String columnAlias) {
        String changedCode = "";

        if (student != null) {
            String oldCode = (String) student.getFieldValueByAlias(columnAlias);
            if (oldCode == null) {
                oldCode = "";
            }

            String newCode = "0";
            if (serviceCode != null && !StringUtils.isEmpty(serviceCode.trim())
                    && !serviceCode.trim().equalsIgnoreCase("N")) {
                newCode = "1";
            }

            if (m_verboseOutput) {
                // m_messages.add("Student:" +
                // (String)student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS) + ", field:" +
                // columnAlias + ", old service code:" + ((StringUtils.isEmpty(oldCode) ||
                // oldCode.equals("0")) ? "false" : "true") + ", new service code:" +
                // ((StringUtils.isEmpty(newCode) || newCode.equals("0")) ? "false" : "true"));
                m_messages.add("Student:" + (String) student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS) + ", field:"
                        + columnAlias + ", old service code:" + oldCode + ", new service code:" + newCode);
            }

            if (((StringUtils.isEmpty(oldCode) || oldCode.equals("0")) && newCode.equals("1"))
                    || (!StringUtils.isEmpty(oldCode) && oldCode.equals("1") && newCode.equals("0"))) {
                student.setFieldValueByAlias(columnAlias, newCode);
                changedCode = columnAlias + ", ";
            }
        }

        return changedCode;
    }

    private String updateServiceHoursColumn1(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services 1");
    }

    private String updateServiceHoursColumn2(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services 2");
    }

    private String updateServiceHoursColumn3(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services 3");
    }

    private String updateServiceHoursColumn3R(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser 3");
    }

    private String updateServiceHoursColumn6(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services 6");
    }

    private String updateServiceHoursColumn7(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services 7");
    }

    private String updateServiceHoursColumn8(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services 8");
    }

    private String updateServiceHoursColumnA(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser A");
    }

    private String updateServiceHoursColumnB(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser B");
    }

    private String updateServiceHoursColumnC(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser C");
    }

    private String updateServiceHoursColumnD(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser D");
    }

    private String updateServiceHoursColumnE(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser E");
    }

    private String updateServiceHoursColumnF(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser F");
    }

    private String updateServiceHoursColumnG(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser G");
    }

    private String updateServiceHoursColumnH(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser H");
    }

    private String updateServiceHoursColumnI(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser I");
    }

    private String updateServiceHoursColumnJ(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser J");
    }

    private String updateServiceHoursColumnK(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser K");
    }

    private String updateServiceHoursColumnL(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Related Ser L");
    }

    private String updateServiceHoursColumnP(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services P");
    }

    private String updateServiceHoursColumnQ(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services Q");
    }

    private String updateServiceHoursColumnR(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services R");
    }

    private String updateServiceHoursColumnS(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services S");
    }

    private String updateServiceHoursColumnT(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services T");
    }

    private String updateServiceHoursColumnU(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services U");
    }

    private String updateServiceHoursColumnV(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services V");
    }

    private String updateServiceHoursColumnW(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services W");
    }

    private String updateServiceHoursColumnX(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services X");
    }

    private String updateServiceHoursColumnY(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services Y");
    }

    private String updateServiceHoursColumnZ(SisStudent student, String serviceCode) {
        return updateServiceHours(student, serviceCode, "DOE Services Z");
    }

    /**
     * Updates the SPED environment.
     *
     * @param student
     * @param environmentStateCode
     * @param columnAlias
     * @return String
     */
    private String updateSpedEnvironment(SisStudent student, String environmentStateCode, String columnAlias) {
        String changedCode = "";
        /*
         * if (student != null) {
         * String oldCode = (String)student.getFieldValueByAlias(columnAlias);
         * if (oldCode == null) {
         * oldCode = "";
         * }
         *
         * String newCode = "";
         * if (!StringUtils.isEmpty(environmentStateCode)) {
         * ReferenceCode refCode =
         * (ReferenceCode)m_spedEnvironmentCodesByStateCode.get(environmentStateCode);
         * if (refCode != null) {
         * newCode = (String)refCode.getCode();
         * }
         * }
         *
         * if (m_verboseOutput) {
         * m_messages.add("Student:" + (String)student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS)
         * + ", field:" + columnAlias + ", old code:" + oldCode + ", new code:" + newCode);
         * }
         *
         * if (!oldCode.equals(newCode)) {
         * student.setFieldValueByAlias(columnAlias, newCode);
         * changedCode = columnAlias + ", " ;
         * }
         * }
         */
        if (m_verboseOutput) {
            m_messages.add("Temporarily ignoring SpedEnvironment codes");
        }

        return changedCode;
    }

    private String updateWeeklyServiceMinutes(SisStudent student, String weeklyServiceMinutes, String columnAlias) {
        String changedCode = "";

        if (student != null) {
            String oldCode = (String) student.getFieldValueByAlias(columnAlias);
            if (oldCode == null) {
                oldCode = "";
            }

            String newCode = "";
            if (!StringUtils.isEmpty(weeklyServiceMinutes)) {
                try {
                    int weeklyMinutesInteger = Integer.parseInt(weeklyServiceMinutes);
                    newCode = Integer.toString(weeklyMinutesInteger);
                } catch (Exception e) {
                    newCode = "";
                }
            }

            if (m_verboseOutput) {
                m_messages.add("Student:" + (String) student.getFieldValueByAlias(STUDENT_GA_TESTID_ALIAS) + ", field:"
                        + columnAlias + ", old value:" + oldCode + ", new value:" + newCode);
            }

            if (!oldCode.equals(newCode)) {
                student.setFieldValueByAlias(columnAlias, newCode);
                changedCode = columnAlias + ", ";
            }
        }

        return changedCode;
    }
}
