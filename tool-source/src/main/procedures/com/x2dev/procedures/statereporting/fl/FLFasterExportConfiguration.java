/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ExtendedDataTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.Restriction;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.RuleSet;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.StudentTransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObjectRecord;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterExportConfiguration.
 */
@SuppressWarnings("unused")
public class FLFasterExportConfiguration extends FLExportConfiguration {


    /**
     * The Interface FasterExport.
     */
    public interface FasterExport {


        /**
         * Gets the validation rules.
         *
         * @param helper FLFasterExportConfiguration
         * @param validations FLFasterValidations
         * @return List
         */
        List<FLValidationRule> getValidationRules(FLFasterExportConfiguration helper, FLFasterValidations validations);


        /**
         * Gets the record type.
         *
         * @return String
         */
        String getRecordType();
    }


    /**
     * The Enum FL_EXPORT.
     */
    public enum FL_EXPORT implements FLExport, FasterExport {
        RECORD00(TransferObjectRecord.RECORD_TYPE_00, "EXPDATA-FL-FST-00",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD01(TransferObjectRecord.RECORD_TYPE_01, "EXPDATA-FL-FST-01",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD02(TransferObjectRecord.RECORD_TYPE_02, "EXPDATA-FL-FST-02",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD03(TransferObjectRecord.RECORD_TYPE_03, "EXPDATA-FL-FST-03",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD04(TransferObjectRecord.RECORD_TYPE_04, "EXPDATA-FL-FST-04",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD05(TransferObjectRecord.RECORD_TYPE_05, "EXPDATA-FL-FST-05",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD06(TransferObjectRecord.RECORD_TYPE_06, "EXPDATA-FL-FST-06",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD07(TransferObjectRecord.RECORD_TYPE_07, "EXPDATA-FL-FST-07",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD08(TransferObjectRecord.RECORD_TYPE_08, "EXPDATA-FL-FST-08",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD09(TransferObjectRecord.RECORD_TYPE_09, "EXPDATA-FL-FST-09",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD10(TransferObjectRecord.RECORD_TYPE_10, "EXPDATA-FL-FST-10",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD11(TransferObjectRecord.RECORD_TYPE_11, "EXPDATA-FL-FST-11",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD99(TransferObjectRecord.RECORD_TYPE_99, "EXPDATA-FL-FST-99",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD99ATV(TransferObjectRecord.RECORD_TYPE_99ATV, "EXPDATA-FL-FST-99ATV",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD99HC(TransferObjectRecord.RECORD_TYPE_99HC, "EXPDATA-FL-FST-99HC",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD99HS(TransferObjectRecord.RECORD_TYPE_99HS, "EXPDATA-FL-FST-99HS",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                })),
        //
        RECORD99IMM(TransferObjectRecord.RECORD_TYPE_99IMM, "EXPDATA-FL-FST-99IMM",
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", FIELD_STD_NUM_ID_FL)
                }));

        private Class<?> m_baseClass;
        private String m_code;
        private String m_exportId;
        private HashMap<String, FLRelationshipKey> m_mapRelationshipByClassName;
        private HashMap<String, FLRelationshipKey> m_mapRelationshipByFieldName;
        private String m_procedureId;
        private List<FLRelationshipKey> m_relationships;


        /**
         * Instantiates a new fl export.
         *
         * @param code String
         * @param procedureId String
         * @param relationships List<FLRelationshipKey>
         */
        private FL_EXPORT(String code, String procedureId, List<FLRelationshipKey> relationships) {
            m_code = code;
            m_procedureId = procedureId;
            m_relationships = relationships;
        }


        /**
         * Find export by record type.
         *
         * @param recordType String
         * @return FLExport
         */
        public static FLExport findExportByRecordType(String recordType) {
            for (FLExport export : values()) {
                if (((FasterExport) export).getRecordType().equals(recordType)) {
                    return export;
                }
            }
            return null;
        }


        /**
         * Gets the base class.
         *
         * @return Class
         */
        public Class<?> getBaseClass() {
            return m_baseClass;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getCode()
         */
        @Override
        public String getCode() {
            return m_code;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getExportId()
         */
        @Override
        public String getExportId() {
            return m_exportId;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getFileNo()
         */
        @Override
        public String getFileNo() {
            return null;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getProcedureId()
         */
        @Override
        public String getProcedureId() {
            return m_procedureId;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FasterExport#getRecordType()
         */
        @Override
        public String getRecordType() {
            return m_code;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getRelationshipKeyByClassName(java.lang.String)
         */
        @Override
        public FLRelationshipKey getRelationshipKeyByClassName(String className) {
            if (m_mapRelationshipByClassName == null) {
                m_mapRelationshipByClassName = new HashMap();
                for (FLRelationshipKey relationship : m_relationships) {
                    m_mapRelationshipByClassName.put(relationship.getRelationshipClass().getName(), relationship);
                }
            }
            return m_mapRelationshipByClassName.get(className);
        }


        /**
         * Gets the relationship key by field name.
         *
         * @param fieldName String
         * @return FL relationship key
         */
        public FLRelationshipKey getRelationshipKeyByFieldName(String fieldName) {
            if (m_mapRelationshipByFieldName == null) {
                m_mapRelationshipByFieldName = new HashMap();
                for (FLRelationshipKey relationship : m_relationships) {
                    m_mapRelationshipByFieldName.put(relationship.getFieldName(), relationship);
                }
            }
            return m_mapRelationshipByFieldName.get(fieldName);
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport#getValidationRules()
         */
        @Override
        public List<FLValidationRule> getValidationRules() {
            return null;
        }


        /**
         * @see com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FasterExport#getValidationRules(com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLFasterValidations)
         */
        @Override
        public List<FLValidationRule> getValidationRules(FLFasterExportConfiguration helper,
                                                         FLFasterValidations validations) {
            List<FLValidationRule> rules = new ArrayList<>();
            for (RuleAssociation ruleAssociation : validations.getRuleAssociations(getRecordType())) {
                if (ruleAssociation.isAppliedForRecordsType(helper.getRecordsType())
                        && ruleAssociation.isAppliedForTransferType(helper.getTransferType())) {
                    FLValidationRule rule = ruleAssociation.getRule(getCode());
                    if (rule != null) {
                        rules.add(rule);
                    }
                }
            }
            return rules;
        }


        /**
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return "FL_EXPORT " + getProcedureId();
        }
    }


    /**
     * The Class RuleAssociation.
     */
    static class RuleAssociation {
        private HashMap<String, String> m_associations = null;
        private FLValidationProcessor m_processor = null;
        private List<String> m_recordsTypesApplyTo = new ArrayList<>();
        private List<String> m_transferTypesApplyTo = new ArrayList<>();


        /**
         * Instantiates a new rule association.
         */
        public RuleAssociation() {
            // public constructor without parameters
        }


        /**
         * Instantiates a new rule association.
         *
         * @param exportIdSuffix String
         * @param ruleNumber String
         */
        public RuleAssociation(String exportIdSuffix, String ruleNumber) {
            addAssociation(exportIdSuffix, ruleNumber);
        }


        /**
         * Instantiates a new rule association.
         *
         * @param transferTypesApplyTo List<String>
         * @param recordsTypesApplyTo List<String>
         */
        public RuleAssociation(List<String> transferTypesApplyTo, List<String> recordsTypesApplyTo) {
            if (recordsTypesApplyTo != null) {
                m_recordsTypesApplyTo.addAll(recordsTypesApplyTo);
            }
            if (transferTypesApplyTo != null) {
                m_transferTypesApplyTo.addAll(transferTypesApplyTo);
            }
        }


        /**
         * Adds the association.
         *
         * @param exportIdSuffix String
         * @param ruleNumber String
         */
        public void addAssociation(String exportIdSuffix, String ruleNumber) {
            if (m_associations == null) {
                m_associations = new HashMap<>();
            }
            m_associations.put(exportIdSuffix, ruleNumber);
        }


        /**
         * Gets the rule.
         *
         * @param exportIdSuffix String
         * @return FL validation rule
         */
        public FLValidationRule getRule(String exportIdSuffix) {
            FLValidationRule rule = null;
            if (exportIdSuffix != null) {
                for (Entry<String, String> association : m_associations.entrySet()) {
                    if (exportIdSuffix.equals(association.getKey())) {
                        rule = new FLValidationRule(exportIdSuffix, association.getValue(), m_processor);
                        break;
                    }
                }
            }
            return rule;
        }


        /**
         * Checks if is applied for records type.
         *
         * @param recordsType String
         * @return true, if is applied for records type
         */
        public boolean isAppliedForRecordsType(String recordsType) {
            return m_recordsTypesApplyTo.size() == 0 || m_recordsTypesApplyTo.contains(recordsType);
        }


        /**
         * Checks if is applied for transfer type.
         *
         * @param transferType String
         * @return true, if is applied for transfer type
         */
        public boolean isAppliedForTransferType(String transferType) {
            return m_transferTypesApplyTo.size() == 0 || m_transferTypesApplyTo.contains(transferType);
        }


        /**
         * Sets the processor.
         *
         * @param processor void
         */
        public void setProcessor(FLValidationProcessor processor) {
            m_processor = processor;
        }


        /**
         * Sets the processor.
         *
         * @param message String
         * @param rules ValidationRule[]
         */
        public void setProcessor(String message, ValidationRule... rules) {
            m_processor = new FLValidationRuleSet(new RuleSet(rules), message);
        }


        /**
         * Sets the processor.
         *
         * @param message String
         * @param if_ Restriction
         * @param then Restriction
         */
        public void setProcessor(String message, Restriction if_, Restriction then) {
            m_processor = new FLValidationRuleSet(new RuleSet(ValidationRule.testIf(if_).testThen(then)), message);
        }


        /**
         * Sets the processor.
         *
         * @param message String
         * @param restriction Restriction
         */
        public void setProcessor(String message, Restriction restriction) {
            m_processor = new FLValidationRuleSet(
                    new RuleSet(ValidationRule.testIf(Restriction.alwaysTrue()).testThen(restriction)), message);
        }


        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return String.valueOf(m_associations);
        }
    }

    public static final String ALIAS_VALIDATION_ERR_REC_OID = "err-rec-oid";

    // field names of fields from FASTER export formats
    public static final String FIELD_1ST_ENC_DATE = "1st Enc Date";
    public static final String FIELD_1ST_ENC_DATE_FORMAT = "1st Enc Date Format";
    public static final String FIELD_ACCESS_DATE = "Access Date";
    public static final String FIELD_ACCESS_TIME = "Access Time";
    public static final String FIELD_ACTION_CODE = "Action Code";
    public static final String FIELD_ACTION_DATE = "Action Date";
    public static final String FIELD_ACTION_DST_NUMBER = "Action Dst Number";
    public static final String FIELD_ACTION_DURATION = "Action Duration";
    public static final String FIELD_ACTION_SKL_NUM = "Action Skl Num";
    public static final String FIELD_ADDL_SKL_YEAR = "Add'l Skl Year";
    public static final String FIELD_ADDRESSED_DST_NUM = "Addressed Dst Num";
    public static final String FIELD_ADDRESSED_DST_NUM_B = "Addressed Dst Num B";
    public static final String FIELD_ADDRESSED_SKL_NUM = "Addressed Skl Num";
    public static final String FIELD_ADDRESSED_SKL_NUM_B = "Addressed Skl Num B";
    public static final String FIELD_ADDRESS_APT_NUM = "Address Apt Num";
    public static final String FIELD_ADDRESS_CITY = "Address City";
    public static final String FIELD_ADDRESS_STATE = "Address State";
    public static final String FIELD_ADDRESS_STREET = "Address Street";
    public static final String FIELD_ADDRESS_ZIP = "Address Zip";
    public static final String FIELD_ADULT_FEE_STATUS_1ST = "Adult Fee Status 1st";
    public static final String FIELD_AGE_PROGRAM_CODE = "AGE Program Code";
    public static final String FIELD_AICE_DIPLOMA = "AICE Diploma";
    public static final String FIELD_AIC_PROGRAM_COMPLETE = "AIC Program Complete";
    public static final String FIELD_AKA_NAME_1 = "aka Name 1";
    public static final String FIELD_AKA_NAME_2 = "aka Name 2";
    public static final String FIELD_AKA_NAME_3 = "aka Name 3";
    public static final String FIELD_AKA_NAME_4 = "aka Name 4";
    public static final String FIELD_APPENDAGE = "Appendage";
    public static final String FIELD_APPENDAGE_B = "Appendage B";
    public static final String FIELD_ASM_MET_ALG_1 = "Asm Met ALG 1";
    public static final String FIELD_ASM_MET_ELA = "Asm Met ELA";
    public static final String FIELD_ASM_PASSED_GEOMET = "Asm Passed GEOMET";
    public static final String FIELD_ASM_PASSED_USHISTORY = "Asm Passed USHISTORY";
    public static final String FIELD_ASM_PASSED_ALG_2 = "Asm Passed ALG 2";
    public static final String FIELD_ASM_PASSED_BIOLOGY = "Asm Passed BIOLOGY";
    public static final String FIELD_BILITER_SEAL_DESIGN = "Biliter Seal Design";
    public static final String FIELD_BIRTH_PLACE = "Birth Place";
    public static final String FIELD_CERT_OF_COMPL_DATE = "Cert of Compl Date";
    public static final String FIELD_CERT_OF_COMPL_TYPE = "Cert of Compl Type";
    public static final String FIELD_CLASS_RANK_EFF_DATE = "Class Rank, Eff Date";
    public static final String FIELD_CLASS_RANK_NUM_POS = "Class Rank, Num Pos";
    public static final String FIELD_CLASS_RANK_PERCENT = "Class Rank, Percent";
    public static final String FIELD_CLASS_RANK_TOTAL = "Class Rank, Total";
    public static final String FIELD_COLLEGE_READY_DIPLOM = "College Ready Diplom";
    public static final String FIELD_COMMENT = "Comment";
    public static final String FIELD_COMMENT_LINE_1 = "Comment Line 1";
    public static final String FIELD_COMMENT_LINE_10 = "Comment Line 10";
    public static final String FIELD_COMMENT_LINE_11 = "Comment Line 11";
    public static final String FIELD_COMMENT_LINE_12 = "Comment Line 12";
    public static final String FIELD_COMMENT_LINE_2 = "Comment Line 2";
    public static final String FIELD_COMMENT_LINE_3 = "Comment Line 3";
    public static final String FIELD_COMMENT_LINE_4 = "Comment Line 4";
    public static final String FIELD_COMMENT_LINE_5 = "Comment Line 5";
    public static final String FIELD_COMMENT_LINE_6 = "Comment Line 6";
    public static final String FIELD_COMMENT_LINE_7 = "Comment Line 7";
    public static final String FIELD_COMMENT_LINE_8 = "Comment Line 8";
    public static final String FIELD_COMMENT_LINE_9 = "Comment Line 9";
    public static final String FIELD_COMVOL_SERVICE_HRS = "ComVol Service Hrs";
    public static final String FIELD_CONSENT_DATE_EV = "Consent Date (EV) ";
    public static final String FIELD_COUNTRY_OF_BIRTH = "Country of Birth";
    public static final String FIELD_COURSE_FLAG = "Course Flag";
    public static final String FIELD_COURSE_GRADE = "Course Grade";
    public static final String FIELD_COURSE_NUMBER = "Course Number";
    public static final String FIELD_COURSE_TITLE_ABBREV = "Course Title Abbrev";
    public static final String FIELD_CPP = "CPP ";
    public static final String FIELD_CREDIT_ATTEMPTED_CRS = "Credit Attempted Crs";
    public static final String FIELD_CREDIT_EARNED_CRS = "Credit Earned Crs";
    public static final String FIELD_CREDS_NEEDED = "Creds Needed ";
    public static final String FIELD_CRIT_HLTH_INFO_CONT = "Crit Hlth Info Cont";
    public static final String FIELD_CRIT_HLTH_INFO_IND = "Crit Hlth Info Ind";
    public static final String FIELD_CRS_ABSENCES = "Crs Absences";
    public static final String FIELD_CRS_ASM_STATUS = "Crs Asm Status";
    public static final String FIELD_CRS_IN_PROGRESS_HRS = "Crs In Progress Hrs";
    public static final String FIELD_CRS_STATE_SUBJ_REQS = "Crs State Subj Reqs";
    public static final String FIELD_CTE_ACADEMY_ID = "CTE Academy ID ";
    public static final String FIELD_CTE_FULL_PGM_COMPL = "CTE Full Pgm Compl ";
    public static final String FIELD_CTE_INDUS_CERT_ID = "CTE Indus Cert ID ";
    public static final String FIELD_CTE_INDUS_CERT_OUT = "CTE Indus Cert Out ";
    public static final String FIELD_CTE_PGM_CODE = "CTE Pgm Code ";
    public static final String FIELD_CTE_PGM_CERT_EARNED_DATE = "CTE Cert Earn Date ";
    public static final String FIELD_CTE_PGM_NAME = "CTE Pgm Name ";
    public static final String FIELD_DATE_1 = "Date 1";
    public static final String FIELD_DATE_2 = "Date 2";
    public static final String FIELD_DATE_FORMAT_1 = "Date Format 1";
    public static final String FIELD_DATE_FORMAT_2 = "Date Format 2";
    public static final String FIELD_DATE_OF_BIRTH = "Date of Birth";
    public static final String FIELD_DATE_OF_BIRTH_B = "Date of Birth B";
    public static final String FIELD_DATE_QUALIFIER_1 = "Date Qualifier 1";
    public static final String FIELD_DATE_QUALIFIER_2 = "Date Qualifier 2";
    public static final String FIELD_DAYS_ABSENT_ANNUAL = "Days Absent Annual";
    public static final String FIELD_DAYS_ABSENT_SUMMER = "Days Absent Summer";
    public static final String FIELD_DAYS_PRESENT_ANNUAL = "Days Present Annual";
    public static final String FIELD_DAYS_PRESENT_SUMMER = "Days Present Summer";
    public static final String FIELD_DIFF_DIPLOMA = "Diff Diploma";
    public static final String FIELD_DIPLOMA_DATE = "Diploma Date";
    public static final String FIELD_DIPLOMA_DESIGNATION = "Diploma Designation";
    public static final String FIELD_DIPLOMA_TYPE = "Diploma Type";
    public static final String FIELD_DISMISSAL_DATE = "Dismissal Date ";
    public static final String FIELD_DISTRICT_NUMBER_CE = "District Number, CE";
    public static final String FIELD_DIS_COND_RESOLUTION = "Dis Cond Resolution";
    public static final String FIELD_DIS_COND_TYPE_CODE = "Dis Cond Type Code";
    public static final String FIELD_DOB_VERIFICATION = "DOB Verification";
    public static final String FIELD_DRP_OUTCOMES = "DRP Outcomes ";
    public static final String FIELD_DRP_PLACEMNT_REASONS = "DRP Place Reasons ";
    public static final String FIELD_DRP_PROGRAM_CODE = "DRP Program Code ";
    public static final String FIELD_DST_COM_VOL_REQ_MET = "Dst Com Vol Req Met";
    public static final String FIELD_DST_NUM_CRED_EARNED = "Dst Num Cred Earned";
    public static final String FIELD_EARLY_ADM_STUDENT = "Early Adm Student";
    public static final String FIELD_ELIG_DETERM_DATE = "Elig Determ Date ";
    public static final String FIELD_ELL_CLASSIF_DATE = "ELL Classif Date";
    public static final String FIELD_ELL_ENTRY_BASIS = "ELL Entry Basis";
    public static final String FIELD_ELL_ENTRY_DATE = "ELL Entry Date";
    public static final String FIELD_ELL_EXIT_BASIS = "ELL Exit Basis ";
    public static final String FIELD_ELL_EXIT_DATE = "ELL Exit Date";
    public static final String FIELD_ELL_INSTRUCT_EXT = "ELL Instruct Ext";
    public static final String FIELD_ELL_PK_12 = "ELL PK-12";
    public static final String FIELD_ELL_RECLAS_DATE = "ELL Reclas Date";
    public static final String FIELD_ELL_RECLAS_EXIT_DATE = "ELL Reclas Exit Date";
    public static final String FIELD_ELL_REEVAL_DATE = "ELL Reeval Date";
    public static final String FIELD_ELL_REPORT_CARD_1 = "ELL Report Card 1";
    public static final String FIELD_ELL_SEMIANN_REVIEW = "ELL Semiann Review ";
    public static final String FIELD_ELL_STD_PLAN_DATE = "ELL Std Plan Date";
    public static final String FIELD_ELL_YEAR_2_END = "ELL Year 2 end";
    public static final String FIELD_ERROR_COUNT = "Error Count";
    public static final String FIELD_ETHNICITY = "Ethnicity";
    public static final String FIELD_EVAL_COMPL_DATE = "Eval Compl Date ";
    public static final String FIELD_EVAL_REEVAL_DATE = "Eval/Reeval Date";
    public static final String FIELD_EVEN_START_FAMIL_LIT = "Even Start Famil Lit";
    public static final String FIELD_EXCEPTIONALITY = "Exceptionality ";
    public static final String FIELD_EXCEPT_PLAN_DATE = "Except Plan Date";
    public static final String FIELD_EXCEPT_PRIMARY = "Except Primary";
    public static final String FIELD_FEFP_PGM_NUM = "FEFP Pgm Num";
    public static final String FIELD_FILLER_FACTS = "Filler FACTS";
    public static final String FIELD_FILLER_LOCAL_USE = "Filler Local Use";
    public static final String FIELD_FILLER_MSIX = "Filler MSIX";
    public static final String FIELD_FIRST_NAME = "First Name";
    public static final String FIELD_FIRST_NAME_B = "First Name B";
    public static final String FIELD_FLEID = "FLEID";
    public static final String FIELD_FL_FIRST_START_PGM = "FL First Start Pgm";
    public static final String FIELD_FORMER_LAST_NAME_1 = "Former Last Name 1";
    public static final String FIELD_FORMER_LAST_NAME_1_B = "Former Last Name 1 B";
    public static final String FIELD_FORMER_LAST_NAME_2 = "Former Last Name 2";
    public static final String FIELD_FORMER_LAST_NAME_2_B = "Former Last Name 2 B";
    public static final String FIELD_GENDER = "Gender";
    public static final String FIELD_GENDER_B = "Gender B";
    public static final String FIELD_GPA_DISTRICT = "GPA, District";
    public static final String FIELD_GPA_DST_CUMULATIVE = "GPA Dst Cumulative";
    public static final String FIELD_GPA_DST_TERM = "GPA Dst Term";
    public static final String FIELD_GPA_STATE = "GPA, State";
    public static final String FIELD_GPA_STATE_CUMULATIVE = "GPA State Cumulative";
    public static final String FIELD_GPA_STATE_TERM = "GPA State Term";
    public static final String FIELD_GRADE_LEVEL = "Grade Level";
    public static final String FIELD_GRADE_PROMO_STATUS = "Grade Promo Status";
    public static final String FIELD_GRADING_CYCLE_WEEKS = "Grading Cycle Weeks";
    public static final String FIELD_GRADUATION_OPTION = "Graduation Option";
    public static final String FIELD_GRAD_BL_1_CUM_GRADE = "Grad Bl 1 Cum Grade";
    public static final String FIELD_GRAD_BL_1_EXAM_GRADE = "Grad Bl 1 Exam Grade";
    public static final String FIELD_GRAD_BL_1_PER_1 = "Grad Bl 1 Per 1";
    public static final String FIELD_GRAD_BL_1_PER_2 = "Grad Bl 1 Per 2";
    public static final String FIELD_GRAD_BL_1_PER_3 = "Grad Bl 1 Per 3";
    public static final String FIELD_GRAD_BL_2_CUM_GRADE = "Grad Bl 2 Cum Grade";
    public static final String FIELD_GRAD_BL_2_EXAM_GRADE = "Grad Bl 2 Exam Grade";
    public static final String FIELD_GRAD_BL_2_PER_1 = "Grad Bl 2 Per 1";
    public static final String FIELD_GRAD_BL_2_PER_2 = "Grad Bl 2 Per 2";
    public static final String FIELD_GRAD_BL_2_PER_3 = "Grad Bl 2 Per 3";
    public static final String FIELD_GRAD_BL_3_CUM_GRADE = "Grad Bl 3 Cum Grade";
    public static final String FIELD_GRAD_BL_3_EXAM_GRADE = "Grad Bl 3 Exam Grade";
    public static final String FIELD_GRAD_BL_3_PER_1 = "Grad Bl 3 Per 1";
    public static final String FIELD_GRAD_BL_3_PER_2 = "Grad Bl 3 Per 2";
    public static final String FIELD_GRAD_BL_3_PER_3 = "Grad Bl 3 Per 3";
    public static final String FIELD_GRAD_BL_4_CUM_GRADE = "Grad Bl 4 Cum Grade";
    public static final String FIELD_GRAD_BL_4_EXAM_GRADE = "Grad Bl 4 Exam Grade";
    public static final String FIELD_GRAD_BL_4_PER_1 = "Grad Bl 4 Per 1";
    public static final String FIELD_GRAD_BL_4_PER_2 = "Grad Bl 4 Per 2";
    public static final String FIELD_GRAD_BL_4_PER_3 = "Grad Bl 4 Per 3";
    public static final String FIELD_GRAD_BL_5_CUM_GRADE = "Grad Bl 5 Cum Grade";
    public static final String FIELD_GRAD_BL_5_EXAM_GRADE = "Grad Bl 5 Exam Grade";
    public static final String FIELD_GRAD_BL_5_PER_1 = "Grad Bl 5 Per 1";
    public static final String FIELD_GRAD_BL_5_PER_2 = "Grad Bl 5 Per 2";
    public static final String FIELD_GRAD_BL_5_PER_3 = "Grad Bl 5 Per 3";
    public static final String FIELD_GRAD_BL_6_CUM_GRADE = "Grad Bl 6 Cum Grade";
    public static final String FIELD_GRAD_BL_6_EXAM_GRADE = "Grad Bl 6 Exam Grade";
    public static final String FIELD_GRAD_BL_6_PER_1 = "Grad Bl 6 Per 1";
    public static final String FIELD_GRAD_BL_6_PER_2 = "Grad Bl 6 Per 2";
    public static final String FIELD_GRAD_BL_6_PER_3 = "Grad Bl 6 Per 3";
    public static final String FIELD_GRAD_PURP_DATE_COMM = "Grad Purp Date Comm";
    public static final String FIELD_GRAD_PURP_DATE_MATH = "Grad Purp Date Math";
    public static final String FIELD_GRAD_REQ_BASIS = "Grad Req Basis";
    public static final String FIELD_HEARING_PROBLEMS = "Hearing Problems";
    public static final String FIELD_HLTH_EXAM_SKL_ENTRY = "Hlth Exam, Skl Entry";
    public static final String FIELD_HLTH_SCR_DATE = "Hlth Scr Date";
    public static final String FIELD_HLTH_SCR_DATE_FORMAT = "Hlth Scr Date Format";
    public static final String FIELD_HLTH_SCR_RESULTS = "Hlth Scr Results";
    public static final String FIELD_HLTH_SCR_TYPE_CODE = "Hlth Scr Type Code";
    public static final String FIELD_HOME_LANG_SURV_DATE = "Home Lang Surv Date";
    public static final String FIELD_HS_GRADUATION_DATE = "HS Graduation Date";
    public static final String FIELD_HS_GRADUATION_DATE_B = "HS Graduation Date B";
    public static final String FIELD_IB_DIPLOMA = "IB Diploma";
    public static final String FIELD_ID_TYPE = "ID Type";
    public static final String FIELD_ID_TYPE_B = "ID Type B";
    public static final String FIELD_IMMUNIZATION_STATUS = "Immunization Status";
    public static final String FIELD_IMM_DATE = "Imm Date";
    public static final String FIELD_IMM_DATE_FORMAT = "Imm Date Format";
    public static final String FIELD_IMM_SOURCE_CODE = "Imm Source Code";
    public static final String FIELD_IMM_STATUS_CODE = "Imm Status Code";
    public static final String FIELD_IMM_TYPE_CODE = "Imm Type Code";
    public static final String FIELD_INCIDENT_DATE = "Incident Date";
    public static final String FIELD_INCIDENT_TYPE = "Incident Type";
    public static final String FIELD_INSTNAL_STD_NUM = "Inst'nal Std Num";
    public static final String FIELD_INSTNAL_STD_NUM_B = "Inst'nal Std Num B";
    public static final String FIELD_INVOLVED_TIME = "Involved Time";
    public static final String FIELD_LAST_NAME = "Last Name";
    public static final String FIELD_LAST_NAME_B = "Last Name B";
    public static final String FIELD_MED_TREATMENT_TYPE = "Med Treatment Type";
    public static final String FIELD_MESSAGE_TYPE = "Message Type";
    public static final String FIELD_MESSAGE_TYPE_B = "Message Type B";
    public static final String FIELD_MIDDLE_NAME = "Middle Name";
    public static final String FIELD_MIDDLE_NAME_B = "Middle Name B";
    public static final String FIELD_MIGRANT_ANNUAL_TERM = "Migrant Annual Term";
    public static final String FIELD_MIGRANT_BIRTH_STATE = "Migrant Birth State";
    public static final String FIELD_MIGRANT_CONTINUATION = "Migrant Continuation";
    public static final String FIELD_MIGRANT_DST_ATTENDED = "Migrant Dst Attended";
    public static final String FIELD_MIGRANT_ENR_DATE = "Migrant Enr Date";
    public static final String FIELD_MIGRANT_PRIORITY = "Migrant Priority";
    public static final String FIELD_MIGRANT_RESID_DATE = "Migrant Resid Date";
    public static final String FIELD_MIGRANT_SKL_ATTENDED = "Migrant Skl Attended";
    public static final String FIELD_MIGRANT_STATUS_TERM = "Migrant Status Term";
    public static final String FIELD_MIGRANT_SUMMER_TERM = "Migrant Summer Term";
    public static final String FIELD_MIGRANT_WDRAWAL_DATE = "Migrant Wdrawal Date";
    public static final String FIELD_MILITARY_FAMILY = "Military Family";
    public static final String FIELD_MIN_EXCEPT_DATE = "Min Except, Date";
    public static final String FIELD_MSIX_ID = "MSIX ID";
    public static final String FIELD_MULTIPLE_BIRTH_STD = "Multiple Birth Std";
    public static final String FIELD_NAME = "Name";
    public static final String FIELD_NATIONAL_RECORD_TYPE = "National Record Type";
    public static final String FIELD_NATIVE_LANGUAGE = "Native Language";
    public static final String FIELD_NICK_NAME = "Nick Name";
    public static final String FIELD_NICK_NAME_B = "Nick Name B";
    public static final String FIELD_NTL_ACHV_SCHOLAR = "Ntl Achv Scholar";
    public static final String FIELD_NTL_HISPANIC_SCHOLAR = "Ntl Hispanic Scholar";
    public static final String FIELD_NTL_MERIT_SCHOLAR = "Ntl Merit Scholar";
    public static final String FIELD_ONLINE_COURSE_EXEMPT = "Online Course Exempt";
    public static final String FIELD_ONLINE_CRS_INDICATOR = "Online Crs Indicator";
    public static final String FIELD_PARENT_GUARD_CODE = "Parent/Guard Code";
    public static final String FIELD_PARENT_GUARD_CODE_F = "Parent/Guard Code F";
    public static final String FIELD_PARENT_GUARD_CODE_M = "Parent/Guard Code M";
    public static final String FIELD_PARENT_GUARD_NAME = "Parent/Guard Name";
    public static final String FIELD_PARENT_GUARD_NAME_F = "Parent/Guard Name F";
    public static final String FIELD_PARTICIPATION_LVL = "Participation Lvl";
    public static final String FIELD_PAS_NUMBER = "PAS Number";
    public static final String FIELD_PHYS_EDU_WAIVER = "Phys Edu Waiver";
    public static final String FIELD_PIWLRI = "PIWLRI";
    public static final String FIELD_PLACEMENT_DATE = "Placement Date ";
    public static final String FIELD_PLACEMENT_STATUS = "Placement Status ";
    public static final String FIELD_PRIMARY_LANGUAGE = "Primary Language";
    public static final String FIELD_PRIOR_TO_KINDERGART = "Prior to Kindergart";
    public static final String FIELD_QA_DATE = "QA Date ";
    public static final String FIELD_QA_FROM_CITY = "QA From City ";
    public static final String FIELD_QA_FROM_COUNTRY = "QA From Country ";
    public static final String FIELD_QA_FROM_STATE = "QA From State ";
    public static final String FIELD_QA_TO_CITY = "QA To City ";
    public static final String FIELD_QA_TO_STATE = "QA To State ";
    public static final String FIELD_QP_DST_CUMULATIVE = "QP Dst Cumulative";
    public static final String FIELD_QP_DST_TERM = "QP Dst Term";
    public static final String FIELD_QP_STATE_CUMULATIVE = "QP State Cumulative";
    public static final String FIELD_QP_STATE_TERM = "QP State Term";
    public static final String FIELD_RACE_AFRICAN_BLACK = "African Black";
    public static final String FIELD_RACE_AMERICAALASKAINDIAN = "AmericaAlaskaIndian";
    public static final String FIELD_RACE_ASIAN = "Asian";
    public static final String FIELD_RACE_PACIFIC_ISLANDER = "Pacific Islander";
    public static final String FIELD_RACE_WHITE = "White";
    public static final String FIELD_RACIAL_ETHNIC_CAT = "Racial/Ethnic Cat";
    public static final String FIELD_RACIAL_ETHNIC_CAT_B = "Racial/Ethnic Cat B";
    public static final String FIELD_RECORD_TYPE = "Record Type";
    public static final String FIELD_REFERRAL_DATE = "Referral Date ";
    public static final String FIELD_RESIDENT_STATUS = "Resident Status";
    public static final String FIELD_RESID_TUITION_PURPOS = "Resid Tuition Purpos";
    public static final String FIELD_SBT_CRS_NUMBER = "Sbt Crs Number";
    public static final String FIELD_SBT_STATE_SUBJ_REQS = "Sbt State Subj Reqs";
    public static final String FIELD_SCHOOL_NUMBER_CE = "School Number, CE";
    public static final String FIELD_SCHOOL_SPONSORED = "School Sponsored";
    public static final String FIELD_SECTION_504_ELIGIBLE = "Section 504 Eligible";
    public static final String FIELD_SENDING_DST_NUM = "Sending Dst Number";
    public static final String FIELD_SENDING_DST_NUM_B = "Sending Dst Number B";
    public static final String FIELD_SENDING_INST_ID = "Sending Inst Id";
    public static final String FIELD_SENDING_INST_ID_B = "Sending Inst Id B";
    public static final String FIELD_SENDING_SKL_NUM = "Sending Skl Number";
    public static final String FIELD_SENDING_SKL_NUM_B = "Sending Skl Number B";
    public static final String FIELD_SKL_CRED_CODE = "Skl Cred Code";
    public static final String FIELD_SKL_CRED_CODE_TYPE = "Skl Cred Code Type";
    public static final String FIELD_SKL_NAME_CRED_EARNED = "Skl Name Cred Earned";
    public static final String FIELD_SKL_NAME_CRS_TAKEN = "Skl Name Crs Taken";
    public static final String FIELD_SKL_NUM_CRS_TAKEN = "Skl Num Crs Taken";
    public static final String FIELD_SKL_NUM_EARNED_TAKEN = "Skl Num Earned/Taken";
    public static final String FIELD_SKL_YEAR_WITH_CENT = "Skl Year With Cent";
    public static final String FIELD_SPEEDE_EXPRESS_INSTITUTION_ID = "Institution Id";
    public static final String FIELD_SPEEDE_EXPRESS_INSTITUTION_ID_B = "Institution Id B";
    public static final String FIELD_SPONSOR_NAME = "Sponsor Name";
    public static final String FIELD_STATUS_DATE = "Status Date ";
    public static final String FIELD_STD_NUM_ID_ALIAS = "Std Num ID Alias";
    public static final String FIELD_STD_NUM_ID_FL = "Std Num Id FL";
    public static final String FIELD_STD_NUM_ID_FL_B = "Std Num Id FL B";
    public static final String FIELD_STD_SCHOOL_ADDRESS_1 = "Std School Address 1";
    public static final String FIELD_STD_SCHOOL_ADDRESS_2 = "Std School Address 2";
    public static final String FIELD_STD_SCHOOL_NAME = "Std School Name";
    public static final String FIELD_STD_SKL_PHONE_NUMBER = "Std Skl Phone Number";
    public static final String FIELD_STD_WAS_PAID = "Std Was Paid";
    public static final String FIELD_STD_WAS_RECRUITED = "Std Was Recruited";
    public static final String FIELD_TERM = "Term";
    public static final String FIELD_TERM_DESIGNATOR = "Term Designator";
    public static final String FIELD_TERM_DESIGNATOR_B = "Term Designator B";
    public static final String FIELD_TERM_END_DATE = "Term End Date";
    public static final String FIELD_TERM_NAME = "Term Name";
    public static final String FIELD_TERM_START_DATE = "Term Start Date";
    public static final String FIELD_TEST_DATE = "Test Date ";
    public static final String FIELD_TEST_FORM = "Test Form ";
    public static final String FIELD_TEST_GRADE_LEVEL = "Test Grade Level ";
    public static final String FIELD_TEST_LEVEL = "Test Level ";
    public static final String FIELD_TEST_NAME = "Test Name ";
    public static final String FIELD_TEST_PAS_NUM = "Test PAS Number ";
    public static final String FIELD_TEST_PROD_IND = "Test Prod Ind";
    public static final String FIELD_TEST_PROD_IND_B = "Test Prod Ind B";
    public static final String FIELD_TEST_S = "Test S ";
    public static final String FIELD_TEST_SC = "Test SC ";
    public static final String FIELD_TEST_ST = "Test ST ";
    public static final String FIELD_TEST_SUBJECT_CONTENT = "Test Subject Content";
    public static final String FIELD_TIME_QUALIFYING_CODE = "Time Qualifying Code";
    public static final String FIELD_TRANSACTION_ID = "Transaction Id";
    public static final String FIELD_TRANSACTION_ID_B = "Transaction Id B";
    public static final String FIELD_TRANSMIT_DATE = "Transmit Date";
    public static final String FIELD_TRANSMIT_DATE_B = "Transmit Date B";
    public static final String FIELD_TYPE_CODE = "Type Code";
    public static final String FIELD_TYPE_CODE_QUALIFIER = "Type Code Qualifier";
    public static final String FIELD_VAC_CERT_EXP_DATE = "Vac Cert Exp Date";
    public static final String FIELD_VISION_PROBLEM = "Vision Problem";
    public static final String FIELD_WDRAWAL_CODE = "Wdrawal Code";
    public static final String FIELD_WDRAWAL_DATE = "Wdrawal Date";
    public static final String FIELD_YEAR_ENTERED_9_GRADE = "Year Entered 9 Grade";

    public static final String PROCEDURE_ID_LIKE = "EXPDATA-FL-FST-%%%%%";

    public static final List<String> s_genericRecordTypes =
            Arrays.asList(TransferObjectRecord.RECORD_TYPE_99, TransferObjectRecord.RECORD_TYPE_99ATV,
                    TransferObjectRecord.RECORD_TYPE_99HC, TransferObjectRecord.RECORD_TYPE_99HS,
                    TransferObjectRecord.RECORD_TYPE_99IMM);

    public static Map<String, String> s_sectionAToBFields = null;

    static {
        s_sectionAToBFields = new HashMap<String, String>();
        s_sectionAToBFields.put(FIELD_STD_NUM_ID_FL, FIELD_STD_NUM_ID_FL_B);
        s_sectionAToBFields.put(FIELD_ADDRESSED_DST_NUM, FIELD_ADDRESSED_DST_NUM_B);
        s_sectionAToBFields.put(FIELD_ADDRESSED_SKL_NUM, FIELD_ADDRESSED_SKL_NUM_B);
        s_sectionAToBFields.put(FIELD_SENDING_DST_NUM, FIELD_SENDING_DST_NUM_B);
        s_sectionAToBFields.put(FIELD_SENDING_SKL_NUM, FIELD_SENDING_SKL_NUM_B);
        s_sectionAToBFields.put(FIELD_MESSAGE_TYPE, FIELD_MESSAGE_TYPE_B);
        s_sectionAToBFields.put(FIELD_TEST_PROD_IND, FIELD_TEST_PROD_IND_B);
        s_sectionAToBFields.put(FIELD_TERM_DESIGNATOR, FIELD_TERM_DESIGNATOR_B);
        s_sectionAToBFields.put(FIELD_INSTNAL_STD_NUM, FIELD_INSTNAL_STD_NUM_B);
        s_sectionAToBFields.put(FIELD_LAST_NAME, FIELD_LAST_NAME_B);
        s_sectionAToBFields.put(FIELD_APPENDAGE, FIELD_APPENDAGE_B);
        s_sectionAToBFields.put(FIELD_FIRST_NAME, FIELD_FIRST_NAME_B);
        s_sectionAToBFields.put(FIELD_MIDDLE_NAME, FIELD_MIDDLE_NAME_B);
        s_sectionAToBFields.put(FIELD_FORMER_LAST_NAME_1, FIELD_FORMER_LAST_NAME_1_B);
        s_sectionAToBFields.put(FIELD_FORMER_LAST_NAME_2, FIELD_FORMER_LAST_NAME_2_B);
        s_sectionAToBFields.put(FIELD_NICK_NAME, FIELD_NICK_NAME_B);
        s_sectionAToBFields.put(FIELD_GENDER, FIELD_GENDER_B);
        s_sectionAToBFields.put(FIELD_RACIAL_ETHNIC_CAT, FIELD_RACIAL_ETHNIC_CAT_B);
        s_sectionAToBFields.put(FIELD_ID_TYPE, FIELD_ID_TYPE_B);
        s_sectionAToBFields.put(FIELD_SENDING_INST_ID, FIELD_SENDING_INST_ID_B);
        s_sectionAToBFields.put(FIELD_HS_GRADUATION_DATE, FIELD_HS_GRADUATION_DATE_B);
        s_sectionAToBFields.put(FIELD_DATE_OF_BIRTH, FIELD_DATE_OF_BIRTH_B);
        s_sectionAToBFields.put(FIELD_TRANSACTION_ID, FIELD_TRANSACTION_ID_B);
        s_sectionAToBFields.put(FIELD_SPEEDE_EXPRESS_INSTITUTION_ID, FIELD_SPEEDE_EXPRESS_INSTITUTION_ID_B);
        s_sectionAToBFields.put(FIELD_TRANSMIT_DATE, FIELD_TRANSMIT_DATE_B);
    }

    private static final String DDX_VALIDATION_ERROR_ID = "FL-FASTER-ERROR";
    private static final String VALIDATION_ERROR_TABLE_NAME = "FASTER Transfer Record Error";

    private ExtendedDataDictionary m_ddxValidationError = null;
    private TransferObjectHelper m_helper = null;
    private Map<String, String> m_localStdOid = new HashMap<>();
    private Map<String, Collection<ExportFormatRow>> m_mapExportFormatRows = new HashMap();
    private Map<String, ExportFormatDefinition> m_recordTypeDefinitions = new HashMap<>();
    private Map<String, List<ExportFormatField>> m_recordTypeFields = new HashMap<>();
    private Map<String, List<String>> m_recordTypeColumnNames = new HashMap<>();
    private ExtendedDataTable m_tbxValidationError = null;
    private String m_transferObjectOid = null;
    private DataDictionary m_validationErrorDictionary;


    /**
     * Instantiates a new FL faster export configuration.
     *
     * @param currentContext DistrictSchoolYearContext
     * @param broker X2Broker
     */
    public FLFasterExportConfiguration(DistrictSchoolYearContext currentContext, X2Broker broker) {
        this(currentContext, null, broker);
    }


    /**
     * Instantiates a new FL faster export configuration.
     *
     * @param currentContext DistrictSchoolYearContext
     * @param transferObjectOid String
     * @param broker X2Broker
     */
    public FLFasterExportConfiguration(DistrictSchoolYearContext currentContext, String transferObjectOid,
            X2Broker broker) {
        super(currentContext, broker);
        m_transferObjectOid = transferObjectOid;
        m_helper = new TransferObjectHelper(broker);


        X2Criteria effCriteria = new X2Criteria();
        String idColumn = ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                + ExportFormatDefinition.COL_PROCEDURE_ID;
        effCriteria.addLike(idColumn, "EXPDATA-FL-FST-%%%%%");
        QueryByCriteria effQuery = new QueryByCriteria(ExportFormatField.class, effCriteria);
        effQuery.addOrderByAscending(ExportFormatField.COL_POSITION);
        Map<String, List<ExportFormatField>> procIdFields =
                getBroker().getGroupedCollectionByQuery(effQuery, idColumn, 17);

        for (Entry<String, List<ExportFormatField>> entry : procIdFields.entrySet()) {
            String procId = entry.getKey();
            String recordType = procId.replaceAll("EXPDATA-FL-FST-", "");
            m_recordTypeFields.put(recordType, entry.getValue());
        }

        for (Entry<String, List<ExportFormatField>> entry : m_recordTypeFields.entrySet()) {
            String recordType = entry.getKey();
            List<ExportFormatField> fields = entry.getValue();
            if (m_recordTypeDefinitions.get(recordType) == null) {
                m_recordTypeDefinitions.put(recordType, fields.iterator().next().getDefinition());
            }
        }

        for (Entry<String, List<ExportFormatField>> entry : m_recordTypeFields.entrySet()) {
            String recordType = entry.getKey();
            List<String> columnNames = m_recordTypeColumnNames.get(recordType);
            if (columnNames == null) {
                columnNames = new ArrayList<>();
                m_recordTypeColumnNames.put(recordType, columnNames);
            }
            for (ExportFormatField field : entry.getValue()) {
                columnNames.add(field.getName());
            }
        }
    }


    /**
     * Gets the column names by record type.
     *
     * @param recordType String
     * @return List
     */
    public List<String> getColumnNamesByRecordType(String recordType) {
        return m_recordTypeColumnNames.get(recordType);
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#getExportFormatRowFieldValue(com.follett.fsc.core.k12.beans.ExportFormatRow,
     *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport, java.lang.String)
     */
    @Override
    public String getExportFormatRowFieldValue(ExportFormatRow row, FLExport export, String fieldName) {
        String value = null;

        String recordType = ((FasterExport) export).getRecordType();
        List<ExportFormatField> fields = getFieldDefinitionsByRecordType(recordType);
        int startPosition = 0;
        int fieldLength = 0;
        for (ExportFormatField field : fields) {
            if (field.getName().equals(fieldName)) {
                fieldLength = field.getMaximumLength();
                break;
            }
            startPosition += field.getMaximumLength();
        }

        if (fieldLength > 0 && startPosition < 1020) {
            String plainRow = getPlainRow(row);

            value = plainRow.substring(startPosition, startPosition + fieldLength);
        }

        if (value == null) {
            value = super.getExportFormatRowFieldValue(row, export, fieldName);
        }

        return value;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#getExportFormatRowFieldValue(com.follett.fsc.core.k12.beans.ExportFormatRow,
     *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport,
     *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLRelationshipKey)
     */
    @Override
    public String getExportFormatRowFieldValue(ExportFormatRow row, FLExport export, FLRelationshipKey relKey) {
        return super.getExportFormatRowFieldValue(row, export, relKey.getFieldName());
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#getExportFormatRows(com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport)
     */
    @Override
    public Collection<ExportFormatRow> getExportFormatRows(FLExport export) {
        Collection<ExportFormatRow> values = null;
        if (!m_mapExportFormatRows.containsKey(export.getCode())) {
            ExportFormatResult result = getExportFormatResult();
            if (result != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, result.getOid());
                criteria.addEqualTo(ExportFormatRow.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                        + ExportFormatDefinition.COL_PROCEDURE_ID, export.getProcedureId());
                BeanQuery query = new BeanQuery(ExportFormatRow.class, criteria);
                query.addOrderBy(ExportFormatRow.COL_SORT_ORDER, true);
                values = getBroker().getCollectionByQuery(query);
            } else {
                values = new LinkedList();
            }
            m_mapExportFormatRows.put(export.getCode(), values);
        }
        return m_mapExportFormatRows.get(export.getCode());
    }


    /**
     * Gets the export format rows.
     *
     * @param export FLExport
     * @param studentTransferOid String
     * @return Collection
     */
    public Collection<ExportFormatRow> getExportFormatRows(FLExport export, String studentTransferOid) {
        Collection<ExportFormatRow> studentRows = null;
        if (!m_mapExportFormatRows.containsKey(export.getCode())) {
            StudentTransferObject studentTransfer = m_helper.findStudentTransfer(studentTransferOid);
            studentRows = new ArrayList<ExportFormatRow>();
            ExportFormatResult result = getExportFormatResult();
            if (result != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, result.getOid());
                criteria.addEqualTo(ExportFormatRow.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                        + ExportFormatDefinition.COL_PROCEDURE_ID, export.getProcedureId());
                BeanQuery query = new BeanQuery(ExportFormatRow.class, criteria);
                query.addOrderBy(ExportFormatRow.COL_SORT_ORDER, true);
                Collection<ExportFormatRow> rows = getBroker().getCollectionByQuery(query);
                for (ExportFormatRow row : rows) {
                    String stdOid = getStudentOidByRow(row);
                    if (studentTransfer.getStudentOid().equals(stdOid)) {
                        studentRows.add(row);
                    }
                }
            } else {
                studentRows = new LinkedList();
            }
            m_mapExportFormatRows.put(export.getCode(), studentRows);
        }
        return m_mapExportFormatRows.get(export.getCode());
    }


    /**
     * Gets the field definitions by record type.
     *
     * @param recordType String
     * @return List
     */
    public List<ExportFormatField> getFieldDefinitionsByRecordType(String recordType) {
        return m_recordTypeFields.get(recordType);
    }


    /**
     * Gets the field java name.
     *
     * @param exportFormatDefinitionOid String
     * @param fieldName String
     * @return String
     */
    public String getFieldJavaName(String exportFormatDefinitionOid, String fieldName) {
        String fieldPath = null;
        for (Entry<String, ExportFormatDefinition> entry : m_recordTypeDefinitions.entrySet()) {
            ExportFormatDefinition efd = entry.getValue();
            if (efd.getOid().equals(exportFormatDefinitionOid)) {
                String recordType = entry.getKey();
                for (ExportFormatField field : getFieldDefinitionsByRecordType(recordType)) {
                    if (field.getName().equals(fieldName)) {
                        fieldPath = field.getDataFieldConfig().getDataField().getJavaName();
                    }
                }
            }
        }
        return fieldPath;
    }


    /**
     * Gets the field length.
     *
     * @param recordType String
     * @param fieldName String
     * @return int
     */
    public int getFieldLength(String recordType, String fieldName) {
        List<ExportFormatField> fields = getFieldDefinitionsByRecordType(recordType);
        int fieldLength = 0;
        for (ExportFormatField field : fields) {
            if (field.getName().equals(fieldName)) {
                fieldLength = field.getMaximumLength();
                break;
            }
        }
        return fieldLength;
    }


    /**
     * Gets the field start position.
     *
     * @param recordType String
     * @param fieldName String
     * @return int
     */
    public int getFieldStartPosition(String recordType, String fieldName) {
        List<ExportFormatField> fields = getFieldDefinitionsByRecordType(recordType);
        boolean isFound = false;
        int startPosition = 0;
        for (ExportFormatField field : fields) {
            if (field.getName().equals(fieldName)) {
                isFound = true;
                break;
            }
            startPosition += field.getMaximumLength();
        }
        if (!isFound) {
            throw new X2RuntimeException();
        }
        return startPosition;
    }


    /**
     * Gets the format definition by record type.
     *
     * @param recordType String
     * @return Export format definition
     */
    public ExportFormatDefinition getFormatDefinitionByRecordType(String recordType) {
        if (m_recordTypeDefinitions.get(recordType) == null) {
            m_recordTypeDefinitions.put(recordType,
                    m_recordTypeFields.get(recordType).iterator().next().getDefinition());
        }
        return m_recordTypeDefinitions.get(recordType);
    }


    /**
     * Gets the plain row.
     *
     * @param row ExportFormatRow
     * @return String
     */
    public String getPlainRow(ExportFormatRow row) {
        return m_helper.getPlainRow(row);
    }


    /**
     * Gets the plain row field value.
     *
     * @param plainRow String
     * @param export FLExport
     * @param fieldName String
     * @return String
     */
    public String getPlainRowFieldValue(String plainRow, FLExport export, String fieldName) {
        String value = null;
        String recordType = ((FasterExport) export).getRecordType();
        List<ExportFormatField> fields = getFieldDefinitionsByRecordType(recordType);
        int startPosition = 0;
        int fieldLength = 0;
        for (ExportFormatField field : fields) {
            if (field.getName().equals(fieldName)) {
                fieldLength = field.getMaximumLength();
                break;
            }
            startPosition += field.getMaximumLength();
        }

        if (fieldLength > 0 && startPosition < 1020) {
            value = plainRow.substring(startPosition, startPosition + fieldLength);
        }

        return value;
    }


    /**
     * Gets the records type.
     *
     * @return String
     */
    public String getRecordsType() {
        if (m_transferObjectOid == null) {
            throw new X2RuntimeException();
        }
        return m_helper.findTransferObject(m_transferObjectOid).getRecordsType();
    }


    /**
     * Gets the relationship key.
     *
     * @param recordType String
     * @param fieldName String
     * @return FL relationship key
     */
    public static FLRelationshipKey getRelationshipKey(String recordType, String fieldName) {
        FL_EXPORT flExport = null;
        for (FL_EXPORT currentFlExport : FL_EXPORT.values()) {
            if (currentFlExport.getRecordType().equals(recordType)) {
                flExport = currentFlExport;
            }
        }
        FLRelationshipKey relationshipKey = flExport.getRelationshipKeyByFieldName(fieldName);
        return relationshipKey;
    }


    /**
     * Gets the student oid by row.
     *
     * @param row ExportFormatRow
     * @return String
     */
    public String getStudentOidByRow(ExportFormatRow row) {
        String procedureId = row.getDefinition().getProcedureId();
        for (FLExport export : FL_EXPORT.values()) {
            if (export.getProcedureId().equals(procedureId)) {
                String localId = super.getExportFormatRowFieldValue(row, export, FIELD_STD_NUM_ID_FL);
                String stdOid = m_localStdOid.get(localId);
                if (stdOid == null) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addBeginsWith(SisStudent.COL_LOCAL_ID, localId.replaceFirst("^0+(?!$)", "").trim());
                    BeanQuery query = new BeanQuery(SisStudent.class, criteria);
                    stdOid = getBroker().getBeanByQuery(query).getOid();
                    m_localStdOid.put(localId, stdOid);
                }
                return stdOid;
            }
        }
        return null;
    }


    /**
     * Gets the transfer object.
     *
     * @return Transfer object
     */
    public TransferObject getTransferObject() {
        return m_helper.findTransferObject(m_transferObjectOid);
    }


    /**
     * Gets the transfer object helper.
     *
     * @return Transfer object helper
     */
    public TransferObjectHelper getTransferObjectHelper() {
        return m_helper;
    }


    /**
     * Gets the transfer type.
     *
     * @return String
     */
    public String getTransferType() {
        if (m_transferObjectOid == null) {
            throw new X2RuntimeException();
        }
        return m_helper.findTransferObject(m_transferObjectOid).getTransferType();
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#getValidationErrorDictionary()
     */
    @Override
    public DataDictionary getValidationErrorDictionary() {
        if (m_validationErrorDictionary == null) {
            ExtendedDataDictionary ddx = getValidationErrorExtendedDataDictionary();
            m_validationErrorDictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
        }
        return m_validationErrorDictionary;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#persistValidationErrors(java.util.List)
     */
    @Override
    public List<X2BaseBean> persistValidationErrors(List<FLValidationError> errors) throws X2BaseException {
        ArrayList<X2BaseBean> errorBeans = new ArrayList<>();
        if (errors != null && !errors.isEmpty()) {
            ExtendedDataTable tbx = getValidationErrorExtendedDataTable();
            DataDictionary dictionary = getValidationErrorDictionary();

            for (FLValidationError error : errors) {
                X2BaseBean errorBean =
                        X2BaseBean.newInstance(tbx.getDataTableConfig().getDataTable().getDataClass(),
                                getBroker().getPersistenceKey());
                String ruleNumber = error.getRuleNumber();
                if (ruleNumber.length() > 10) {
                    ruleNumber = ruleNumber.substring(0, 10);
                }
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_RULE_NUMBER, ruleNumber, dictionary);
                String errorType = error.getError();
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_ERROR, errorType, dictionary);
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_VALUE, error.getValue(), dictionary);
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_FIELD_NAME, error.getFieldName(), dictionary);
                String messageOutput = error.getMessage();
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_MESSAGE,
                        StringUtils.isEmpty(messageOutput) ? errorType : messageOutput, dictionary);
                TransferObjectRecord tor = m_helper.getTransferObjectRecord(error.getRow());
                tor.setStatus(StudentTransferObject.STATUS_ERROR);
                tor.persist();
                String transferRecordOid = tor.getOid();
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_REC_OID, transferRecordOid, dictionary);

                ExtendedDataDictionary ddx = getValidationErrorExtendedDataDictionary();
                errorBean.setFieldValueByAlias("err-ddx-oid", ddx.getOid(), dictionary);

                getBroker().saveBean(errorBean);

                errorBeans.add(errorBean);
            }
        }
        return errorBeans;
    }


    /**
     * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration#getExports()
     */
    @Override
    protected FLExport[] getExports() {
        return FL_EXPORT.values();
    }


    /**
     * Gets the export format result.
     *
     * @return Export format result
     */
    private ExportFormatResult getExportFormatResult() {
        if (m_transferObjectOid == null) {
            throw new X2RuntimeException();
        }
        TransferObject to = m_helper.findTransferObject(m_transferObjectOid);
        return to.getResult();
    }


    /**
     * Gets the validation error extended data dictionary.
     *
     * @return Extended data dictionary
     */
    private ExtendedDataDictionary getValidationErrorExtendedDataDictionary() {
        if (m_ddxValidationError == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_VALIDATION_ERROR_ID);
            BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
            m_ddxValidationError = (ExtendedDataDictionary) getBroker().getBeanByQuery(query);
            if (m_ddxValidationError == null) {
                throw new IllegalStateException(
                        "The extended dictionary " + DDX_VALIDATION_ERROR_ID + " must exist");
            }
        }
        return m_ddxValidationError;
    }


    /**
     * Gets the validation error extended data table.
     *
     * @return Extended data table
     */
    private ExtendedDataTable getValidationErrorExtendedDataTable() {
        if (m_tbxValidationError == null) {
            ExtendedDataDictionary ddx = getValidationErrorExtendedDataDictionary();
            for (ExtendedDataTable table : ddx.getExtendedDataTables()) {
                if (VALIDATION_ERROR_TABLE_NAME.equals(table.getUserName())) {
                    m_tbxValidationError = table;
                    break;
                }
            }
            if (m_tbxValidationError == null) {
                throw new IllegalStateException(
                        "The validation error table with name " + VALIDATION_ERROR_TABLE_NAME
                                + " could not be found");
            }
        }
        return m_tbxValidationError;
    }
}
