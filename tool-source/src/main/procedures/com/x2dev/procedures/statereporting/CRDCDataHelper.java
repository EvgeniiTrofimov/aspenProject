/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.DoubleNestedMap;
import com.x2dev.utils.types.PlainDate;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data helper for CRDC exports. This class:
 * 1) Prepares rows based on selected export results and parameters-filters from export format
 * definition.
 * 2) Returns instances of used by export retrievers and validators.
 *
 * @author Follett Software Company
 */
public class CRDCDataHelper {
    /**
     * Enum of Datasets. Each element has procedure id, name of field that should contain bean oid,
     * collection of
     * CalcParams that can be applied to the Dataset.
     *
     * @author Follett Software Company
     */
    public enum Dataset {
        SCHOOL("EXPDATA-CRDC-SKL", "SchoolOid", Arrays.asList(
                CalcParameter.FIREARMS_USE,
                CalcParameter.HOMICIDE,
                CalcParameter.LEA_NCES_CODE,
                CalcParameter.SKL_NCES_CODE,
                CalcParameter.SKL_IS_OOD,
                CalcParameter.PHS_ATK_FHT_FIRE_EXPL_DVC,
                CalcParameter.PHS_ATK_FHT_WEAPON,
                CalcParameter.PHS_ATK_FHT_WITHOUT_WPN,
                CalcParameter.POSSESS_FIRE_EXPL_DVC,
                CalcParameter.RAPES_OR_ATTEMPTED,
                CalcParameter.ROBB_FIREARM_EXPL_DVC,
                CalcParameter.ROBBERY_WEAPON,
                CalcParameter.ROBBERY_WITHOUT_WEAPON,
                CalcParameter.SEXUAL_BATTERY,
                CalcParameter.THTS_PHS_ATK_FIR_EXPL_DVC,
                CalcParameter.THTS_PHS_ATK_WEAPON,
                CalcParameter.THTS_PHS_ATK_WITHOUT_WPN), false),

        SECTION("EXPDATA-CRDC-MST", "SectionOid", Arrays.asList(
                CalcParameter.SCHED,
                CalcParameter.SCHED_AP,
                CalcParameter.SCHOOL_TYPE,
                CalcParameter.SINGLE_SEX,
                CalcParameter.TEACHER_CERTIFIED), false),

        STAFF("EXPDATA-CRDC-STF", "StaffOid", Arrays.asList(
                CalcParameter.CERTIFIED,
                CalcParameter.STAFF_TYPE,
                CalcParameter.TEACHER_ABSENT,
                CalcParameter.YEARS_TEACHING), false),

        STUDENT("EXPDATA-CRDC-STD", "StudentOid", Arrays.asList(
                CalcParameter.ACT_TEST, /* SCH P2 */
                CalcParameter.ACTIVE_PART1, /* LEA P1, SCH P1 */
                CalcParameter.AGE_YEARS, /* LEA P1, LEA P2 */
                CalcParameter.AGE_SCHOOL, /* SCH P1 */
                CalcParameter.DUAL_ENROLLMENT, /* SCH P1 */
                CalcParameter.ENROLLED_DIST_ED, /* LEA P2 */
                CalcParameter.ENROLLED_GED_PREP, /* LEA P2 */
                CalcParameter.GENDER, /* LEA P1, SCH P1, SCH P2 */
                CalcParameter.GIFTED_TALENTED, /* SCH P1 */
                CalcParameter.GRADE, /* SCH P1, SCH P2 */
                CalcParameter.HARAS_BULL_SEX, /* SCH P2 */
                CalcParameter.HARAS_BULL_RACE, /* SCH P2 */
                CalcParameter.HARAS_BULL_DISAB, /* SCH P2 */
                CalcParameter.HARAS_BULL_SEX_ORIENT, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_ATHIEST, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_BUDDHIST, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_CATHOLIC, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_EAST_ORTHODOX, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_HINDU, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_ISLAMIC, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_JEHOVAH_WITNESS, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_JEWISH, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_MORMON, /* SCH P2 */
                CalcParameter.HARAS_BULL_MULTIPLE_RELIGION, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_OTHER_CHRISTIAN, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_OTHER, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_PROTESTANT, /* SCH P2 */
                CalcParameter.HARAS_BULL_RELIGION_SIKH, /* SCH P2 */
                CalcParameter.HISPANIC, /* LEA P2, SCH P1, SCH P2 */
                CalcParameter.IB_PROGRAM, /* SCH P1 */
                CalcParameter.IDEA, /* LEA P1, LEA P2, SCH P1, SCH P2 */
                CalcParameter.LEP, /* LEA P1, SCH P1, SCH P2 */
                CalcParameter.LEP_ENROLLED, /* SCH P1 */
                CalcParameter.OOD, /* LEA P1 */
                CalcParameter.RACE, /* LEA P2, SCH P1, SCH P2 */
                CalcParameter.SECTION_504, /* SCH P1, SCH P2 */
                CalcParameter.CREDIT_RECOVERY, /* SCH P2 */
                CalcParameter.DAYS_ABSENT, /* SCH P2 */
                CalcParameter.DISCIPLINE_DISB_HARASS, /* SCH P2 */
                CalcParameter.DISCIPLINE_RACE_HARASS, /* SCH P2 */
                CalcParameter.DISCIPLINE_SEX_HARASS, /* SCH P2 */
                CalcParameter.ED_SERV_WHILE_EXPELLED, /* SCH P2 */
                CalcParameter.EXPELLED, /* SCH P2 */
                CalcParameter.EXPELLED_ZERO_TOL, /* SCH P2 */
                CalcParameter.HARASSED_BASED_ON_DISB, /* SCH P2 */
                CalcParameter.HARASSED_BASED_ON_RACE, /* SCH P2 */
                CalcParameter.HARASSED_BASED_ON_SEX, /* SCH P2 */
                CalcParameter.NUM_CORPORAL_PUNISH, /* SCH P2 */
                CalcParameter.NUM_IN_SCHOOL_SUSP, /* SCH P2 */
                CalcParameter.NUM_MECH_RESTRAINT, /* SCH P2 */
                CalcParameter.NUM_OUT_SCHOOL_SUSP, /* SCH P2 */
                CalcParameter.NUM_OUT_SCHOOL_SUSP_DAY, /* SCH P2 */
                CalcParameter.NUM_PHYSICAL_RESTRAINT, /* SCH P2 */
                CalcParameter.NUM_SECLUSION, /* SCH P2 */
                CalcParameter.PASSED_ALGEBRA_I, /* SCH P2 */
                CalcParameter.RECV_HS_EQUIVALENCY, /* SCH P2 */
                CalcParameter.REFERRED_LAW_ENFORCE, /* SCH P2 */
                CalcParameter.RETAINED, /* SCH P2 */
                CalcParameter.SAT_REASONING_TEST, /* SCH P2 */
                CalcParameter.SCHOOL_RELATED_ARREST, /* SCH P2 */
                CalcParameter.UNGRADED_LEVEL, /* SCH P1 */
                CalcParameter.XFER_ALT_SCH_DISCIPLINE, /* SCH P2 */
                CalcParameter.XFER_REG_SCH_DISCIPLINE, /* SCH P2 */
                CalcParameter.ALLEG_HARAS_BULLY_SEX_RACE_NOD_IS, /* SCH P2 */
                CalcParameter.ALLEG_HARAS_BULLY_SEXUAL_OR_RELIG /* SCH P2 */
        ), false),

        STUDENT_SCHEDULE("EXPDATA-CRDC-SSC", "StudentOid-SectionOid", Arrays.asList(
                CalcParameter.QUALIFYING_AP_SCORE,
                CalcParameter.TOOK_AP_TEST), false),

        STUDENT_SCHEDULE_REV("EXPDATA-CRDC-SSC", "StudentOid-SectionOid", Arrays.asList(
                CalcParameter.NO_QUALIFYING_AP_SCORE,
                CalcParameter.NO_AP_TEST), true),

        STAFF_PRIOR_YR("EXPDATA-CRDC-PRYRSTF", "StaffOid", Arrays.asList(
                CalcParameter.STAFF_TYPE_PRIOR_YR), false);

        /**
         * Instantiates a new dataset.
         *
         * @param procedureId String
         * @param oidFieldName String
         * @param calcParams List<CRDCDataHelper.CalcParameter>
         * @param reverseJoin boolean
         */
        Dataset(String procedureId, String oidFieldName, List<CRDCDataHelper.CalcParameter> calcParams,
                boolean reverseJoin) {
            m_calcParams = calcParams;
            m_oidFieldName = oidFieldName;
            m_procedureId = procedureId;
            m_reverseJoin = reverseJoin;
        }

        private List<CRDCDataHelper.CalcParameter> m_calcParams = null;
        private String m_oidFieldName = null;
        private String m_procedureId = null;
        private boolean m_reverseJoin;

        /**
         * Returns dataset by passed CalcParam.
         *
         * @param parameter CalcParameter
         * @return CRDCDataHelper.Dataset
         */
        public static CRDCDataHelper.Dataset findDatasetByParameter(CalcParameter parameter) {
            CRDCDataHelper.Dataset foundedDataset = null;

            for (CRDCDataHelper.Dataset currentDataset : Dataset.values()) {
                if (currentDataset.getCalcParams().contains(parameter)) {
                    foundedDataset = currentDataset;
                    break;
                }
            }

            return foundedDataset;
        }

        /**
         * Returns calc params of the dataset.
         *
         * @return List<CRDCDataHelper.CalcParameter>
         */
        public List<CRDCDataHelper.CalcParameter> getCalcParams() {
            return m_calcParams;
        }

        /**
         * Returns field name of bean oid.
         *
         * @return String
         */
        public String getOidFieldName() {
            return m_oidFieldName;
        }

        /**
         * Returns procedure id.
         *
         * @return String
         */
        public String getProcedureId() {
            return m_procedureId;
        }

        /**
         * Checks if is reverse join.
         *
         * @return true, if is reverse join
         */
        public boolean isReverseJoin() {
            return m_reverseJoin;
        }
    }

    /**
     * Calculator is used to determine logic that need to get filtering value.
     *
     * @author Follett Software Company
     */
    private interface Calculator {

        /**
         * Gets the calculated value.
         *
         * @param helper CRDCDataHelper
         * @param beanPath String
         * @param row ExportFormatRow
         * @return String
         */
        public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row);
    }

    /**
     * Enum of CalcParameters. Each CalcParameter contains name of parameter and name of related
     * field that is used to
     * filter by this field. Also CalcParameter can contain Calculator with special logic if needed.
     *
     * @author Follett Software Company
     */
    public enum CalcParameter {
        ACT_TEST("ACTTest", "ACTTest"),

        ACTIVE_PART1("ActivePart1", "ActivePart1"),

        AGE_YEARS("NumOfYears", "Birthdate", new Calculator() {
            /**
             * Calculator for student's number of years.
             *
             * @return String
             */
            @Override
            public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row) {
                String stringDate = (String) row.getFieldValueByBeanPath(beanPath);
                SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy");
                PlainDate birthDate = null;

                try {
                    birthDate = new PlainDate(format.parse(stringDate));
                } catch (ParseException e) {
                    e.printStackTrace();
                }

                int numOfYears = 0;

                if (birthDate != null) {
                    numOfYears = getDiffYears(birthDate, helper.m_reportDate);
                }

                return String.valueOf(numOfYears);
            }

            /**
             * Returns difference in years between first and last date.
             *
             * @param first
             * @param last
             * @return int
             */
            private int getDiffYears(Date first, Date last) {
                Calendar a = Calendar.getInstance();
                a.setTime(first);
                Calendar b = Calendar.getInstance();
                b.setTime(last);
                int diff = b.get(Calendar.YEAR) - a.get(Calendar.YEAR);
                if (a.get(Calendar.MONTH) > b.get(Calendar.MONTH) ||
                        (a.get(Calendar.MONTH) == b.get(Calendar.MONTH)
                                && a.get(Calendar.DATE) > b.get(Calendar.DATE))) {
                    diff--;
                }
                return diff;
            }
        }),

        AGE_SCHOOL("Age", "Birthdate", new Calculator() {
            /**
             * Calculator for student's age.
             */
            @Override
            public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row) {
                String age = null;

                String stringDate = (String) row.getFieldValueByBeanPath(beanPath);
                SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy");
                PlainDate birthDate = null;

                try {
                    birthDate = new PlainDate(format.parse(stringDate));
                } catch (ParseException e) {
                    e.printStackTrace();
                }

                if (birthDate != null) {
                    int numOfYears = getDiffYears(birthDate, helper.m_reportDate);

                    switch (numOfYears) {
                        case 0:
                        case 1:
                        case 2:
                            age = "2";
                            break;
                        case 3:
                            age = "3";
                            break;
                        case 4:
                            age = "4";
                            break;
                        case 5:
                            age = "5";
                            break;
                        case 6:
                        case 7:
                        case 8:
                        case 9:
                        case 10:
                        case 11:
                            age = "Elem";
                            break;
                        case 12:
                        case 13:
                        case 14:
                            age = "Middle";
                            break;
                        case 15:
                        case 16:
                        case 17:
                        case 18:
                            age = "High";
                            break;
                        default:
                            break;
                    }
                }

                return age;
            }

            /**
             * Returns difference in years between first and last date.
             *
             * @param first
             * @param last
             * @return int
             */
            private int getDiffYears(Date first, Date last) {
                Calendar a = Calendar.getInstance();
                a.setTime(first);
                Calendar b = Calendar.getInstance();
                b.setTime(last);
                int diff = b.get(Calendar.YEAR) - a.get(Calendar.YEAR);
                if (a.get(Calendar.MONTH) > b.get(Calendar.MONTH) ||
                        (a.get(Calendar.MONTH) == b.get(Calendar.MONTH)
                                && a.get(Calendar.DATE) > b.get(Calendar.DATE))) {
                    diff--;
                }
                return diff;
            }
        }),

        DUAL_ENROLLMENT("Dual Enrollment", "DualEnrollment"),

        GENDER("Gender", "Gender"),

        GIFTED_TALENTED("Gifted/Talented", "Gifted/Talented"),

        GRADE("Grade", "Grade"),

        HISPANIC("Hispanic", "Hispanic"),

        IB_PROGRAM("IB Program", "IBProgram"),

        IDEA("IDEA", "IDEA"),

        LEP("LEP", "LEP"),

        LEP_ENROLLED("LEPEnrolled", "LEPEnrolled"),

        OOD("OOD", "OOD"),

        RACE("Race", "Race", new Calculator() {
            @Override
            public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row) {
                String value = null;

                Dataset rowDataset = Dataset.findDatasetByParameter(HISPANIC);
                String hispanicBeanPath = helper.getBeanPath(rowDataset.getProcedureId(), HISPANIC.getFieldName());
                String hispanicValue = HISPANIC.getCalculatedValue(helper, hispanicBeanPath, row);

                if (!"Y".equals(hispanicValue)) {
                    value = (String) row.getFieldValueByBeanPath(beanPath);
                }

                return value;
            }
        }),

        SECTION_504("Section504", "Section504"),

        SCHED("Sched", "Type"),

        SCHED_AP("SchedAP", "APType"),

        SCHOOL_TYPE("SchoolType", "SchoolType"),

        STAFF_TYPE("Type", "Type"),

        SINGLE_SEX("SingleSex", "SexAllowedToAttend"),

        CERTIFIED("Certified", "Certified"),

        YEARS_TEACHING("Years Teaching", "YearsTeaching"),

        CREDIT_RECOVERY("CreditRecovery", "CreditRecovery"),

        DAYS_ABSENT("DaysAbsent", "DaysAbsent"),

        DISCIPLINE_DISB_HARASS("DisciplineDisbHarass", "DisciplineDisbHarass"),

        DISCIPLINE_RACE_HARASS("DisciplineRaceHarass", "DisciplineRaceHarass"),

        DISCIPLINE_SEX_HARASS("DisciplineSexHarass", "DisciplineSexHarass"),

        ED_SERV_WHILE_EXPELLED("EdServWhileExpelled", "EdServWhileExpelled"),

        ENROLLED_DIST_ED("EnrolledDistEd", "EnrolledDistEd"),

        ENROLLED_GED_PREP("EnrolledGEDPrep", "EnrolledGEDPrep"),

        EXPELLED("Expelled", "Expelled"),

        EXPELLED_ZERO_TOL("ExpelledZeroTol", "ExpelledZeroTol"),

        FIREARMS_USE("FirearmsUse", "FirearmsUse"),

        HARAS_BULL_SEX("HarassBullySex", "HarassBullySex"),

        HARAS_BULL_RACE("HarassBullyRace", "HarassBullyRace"),

        HARAS_BULL_DISAB("HarassBullyDisab", "HarassBullyDisab"),

        HARAS_BULL_SEX_ORIENT("HarassBullyOrient", "HarassBullyOrient"),

        HARAS_BULL_RELIGION("HarassBullyReligion", "HarassBullyReligion"),

        HARAS_BULL_RELIGION_ATHIEST("HarassBullyReligionAthiest", "HarassBullyReligionAthiest"),

        HARAS_BULL_RELIGION_BUDDHIST("HarassBullyReligionBuddhist", "HarassBullyReligionBuddhist"),

        HARAS_BULL_RELIGION_CATHOLIC("HarassBullyReligionCatholic", "HarassBullyReligionCatholic"),

        HARAS_BULL_RELIGION_EAST_ORTHODOX("HarassBullyReligionEasternOrthodox", "HarassBullyReligionEasternOrthodox"),

        HARAS_BULL_RELIGION_HINDU("HarassBullyReligionHindu", "HarassBullyReligionHindu"),

        HARAS_BULL_RELIGION_ISLAMIC("HarassBullyReligionIslamic", "HarassBullyReligionIslamic"),

        HARAS_BULL_RELIGION_JEHOVAH_WITNESS("HarassBullyReligionJehovahWitness", "HarassBullyReligionJehovahWitness"),

        HARAS_BULL_RELIGION_JEWISH("HarassBullyReligionJewish", "HarassBullyReligionJewish"),

        HARAS_BULL_RELIGION_MORMON("HarassBullyReligionMormon", "HarassBullyReligionMormon"),

        HARAS_BULL_MULTIPLE_RELIGION("HarassBullyMultipleReligion", "HarassBullyMultipleReligion"),

        HARAS_BULL_RELIGION_OTHER_CHRISTIAN("HarassBullyReligionOtherChristian", "HarassBullyReligionOtherChristian"),

        HARAS_BULL_RELIGION_OTHER("HarassBullyReligionOtherReligion", "HarassBullyReligionOtherReligion"),

        HARAS_BULL_RELIGION_PROTESTANT("HarassBullyReligionProtestant", "HarassBullyReligionProtestant"),

        HARAS_BULL_RELIGION_SIKH("HarassBullyReligionSikh", "HarassBullyReligionSikh"),

        HARASSED_BASED_ON_DISB("HarassedBasedOnDisb", "HarassedBasedOnDisb"),

        HARASSED_BASED_ON_RACE("HarassedBasedOnRace", "HarassedBasedOnRace"),

        HARASSED_BASED_ON_SEX("HarassedBasedOnSex", "HarassedBasedOnSex"),

        HOMICIDE("Homicide", "Homicide"),

        LEA_NCES_CODE("LeaNCESCode", "LeaNCESCode"),

        SKL_NCES_CODE("SchoolNCESCode", "SchoolNCESCode"),

        SKL_IS_OOD("SchoolIsOOD", "SchoolIsOOD", new Calculator() {
            private static final int NES_SCHOOL_ID_MIN_LENGTH = 10;

            @Override
            public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row) {
                String value = null;

                Dataset rowDataset = Dataset.findDatasetByParameter(SKL_NCES_CODE);
                String ncesCodeBeanPath = helper.getBeanPath(rowDataset.getProcedureId(), SKL_NCES_CODE.getFieldName());
                String ncesCode = SKL_NCES_CODE.getCalculatedValue(helper, ncesCodeBeanPath, row);

                value = (ncesCode == null || ncesCode.length() < NES_SCHOOL_ID_MIN_LENGTH) ? "Y" : "N";
                return value;
            }
        }),

        NUM_CORPORAL_PUNISH("NumCorporalPunish", "NumCorporalPunish"),

        NUM_IN_SCHOOL_SUSP("NumIn-schoolSusp", "NumIn-schoolSusp"),

        NUM_MECH_RESTRAINT("NumMechRestraint", "NumMechRestraint"),

        NUM_OUT_SCHOOL_SUSP("NumOut-schoolSusp", "NumOut-schoolSusp"),

        NUM_OUT_SCHOOL_SUSP_DAY("NumOut-schoolSuspDay", "NumOut-schoolSuspDay"),

        NUM_PHYSICAL_RESTRAINT("NumPhysicalRestraint", "NumPhysicalRestraint"),

        NUM_SECLUSION("NumSeclusion", "NumSeclusion"),

        PASSED_ALGEBRA_I("PassedAlgebraI", "PassedAlgebraI"),

        PHS_ATK_FHT_FIRE_EXPL_DVC("PhsAtkFhtFireExplDvc", "PhsAtkFhtFireExplDvc"),

        PHS_ATK_FHT_WEAPON("PhsAtkFhtWeapon", "PhsAtkFhtWeapon"),

        PHS_ATK_FHT_WITHOUT_WPN("PhsAtkFhtWithoutWpn", "PhsAtkFhtWithoutWpn"),

        POSSESS_FIRE_EXPL_DVC("PossessFireExplDvc", "PossessFireExplDvc"),

        RAPES_OR_ATTEMPTED("RapesOrAttempted", "RapesOrAttempted"),

        RECV_HS_EQUIVALENCY("RecvHSEquivalency", "RecvHSEquivalency"),

        REFERRED_LAW_ENFORCE("ReferredLawEnforce", "ReferredLawEnforce"),

        RETAINED("Retained", "Retained"),

        ROBB_FIREARM_EXPL_DVC("RobbFirearmExplDvc", "RobbFirearmExplDvc"),

        ROBBERY_WEAPON("RobberyWeapon", "RobberyWeapon"),

        ROBBERY_WITHOUT_WEAPON("RobberyWithoutWeapon", "RobberyWithoutWeapon"),

        SAT_REASONING_TEST("SATReasoningTest", "SATReasoningTest"),

        STAFF_TYPE_PRIOR_YR("PriorYrType", "Type"),

        NO_QUALIFYING_AP_SCORE("NoQualifyingAPScore", "QualifyingAPScore"),

        QUALIFYING_AP_SCORE("QualifyingAPScore", "QualifyingAPScore"),

        NO_AP_TEST("NoAPTest", "TookAPTest"),

        TOOK_AP_TEST("TookAPTest", "TookAPTest"),

        SCHOOL_RELATED_ARREST("SchoolRelatedArrest", "SchoolRelatedArrest"),

        SEXUAL_BATTERY("SexualBattery", "SexualBattery"),

        TEACHER_CERTIFIED("Teacher Certified", "TeacherCertified"),

        TEACHER_ABSENT("TeacherDaysAbsent", "DaysAbsent"),

        THTS_PHS_ATK_FIR_EXPL_DVC("ThtsPhsAtkFirExplDvc", "ThtsPhsAtkFirExplDvc"),

        THTS_PHS_ATK_WEAPON("ThtsPhsAtkWeapon", "ThtsPhsAtkWeapon"),

        THTS_PHS_ATK_WITHOUT_WPN("ThtsPhsAtkWithoutWpn", "ThtsPhsAtkWithoutWpn"),

        UNGRADED_LEVEL("UngradedLevel", "Birthdate", new Calculator() {
            // TODO: Corrected Ungraded Level calculator
            @Override
            public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row) {
                String stringDate = (String) row.getFieldValueByBeanPath(beanPath);
                SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy");
                PlainDate birthDate = null;

                try {
                    birthDate = new PlainDate(format.parse(stringDate));
                } catch (ParseException e) {
                    e.printStackTrace();
                }

                int numOfYears = 0;

                if (birthDate != null) {
                    numOfYears = getDiffYears(birthDate, helper.m_reportDate) - 5;
                }

                return String.valueOf(numOfYears);
            }

            /**
             * Returns difference in years between first and last date.
             *
             * @param first
             * @param last
             * @return int
             */
            private int getDiffYears(Date first, Date last) {
                Calendar a = Calendar.getInstance();
                a.setTime(first);
                Calendar b = Calendar.getInstance();
                b.setTime(last);
                int diff = b.get(Calendar.YEAR) - a.get(Calendar.YEAR);
                if (a.get(Calendar.MONTH) > b.get(Calendar.MONTH) ||
                        (a.get(Calendar.MONTH) == b.get(Calendar.MONTH)
                                && a.get(Calendar.DATE) > b.get(Calendar.DATE))) {
                    diff--;
                }
                return diff;
            }
        }),

        XFER_ALT_SCH_DISCIPLINE("XferAltSchDiscipline", "XferAltSchDiscipline"),

        XFER_REG_SCH_DISCIPLINE("XferRegSchDiscipline", "XferRegSchDiscipline"),

        ALLEG_HARAS_BULLY_SEX_RACE_NOD_IS("HarassBullySexRace", "HarassBullySexRace"),

        ALLEG_HARAS_BULLY_SEXUAL_OR_RELIG("HarassBullySexRelig", "HarassBullySexRelig");

        /**
         * Instantiates a new calc parameter.
         *
         * @param paramName String
         * @param fieldName String
         */
        private CalcParameter(String paramName, String fieldName) {
            m_fieldName = fieldName;
            m_fieldValueCalculator = new Calculator() {
                @Override
                public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row) {
                    return (String) row.getFieldValueByBeanPath(beanPath);
                }
            };
            m_paramName = paramName;
        }

        /**
         * Instantiates a new calc parameter.
         *
         * @param paramName String
         * @param fieldName String
         * @param fieldCalculator Calculator
         */
        private CalcParameter(String paramName, String fieldName, CRDCDataHelper.Calculator fieldCalculator) {
            m_fieldName = fieldName;
            m_fieldValueCalculator = fieldCalculator;
            m_paramName = paramName;
        }

        private String m_fieldName = null;
        private CRDCDataHelper.Calculator m_fieldValueCalculator = null;
        private String m_paramName = null;

        /**
         * Returns CalcParameter based on parameter name.
         *
         * @param name String
         * @return CRDCDataHelper.CalcParameter
         */
        public static CRDCDataHelper.CalcParameter findCalcParamByName(String name) {
            CRDCDataHelper.CalcParameter foundedParameter = null;

            for (CRDCDataHelper.CalcParameter currentParam : CalcParameter.values()) {
                if (currentParam.getParamName().equals(name)) {
                    foundedParameter = currentParam;
                    break;
                }
            }

            return foundedParameter;
        }

        /**
         * Returns calculated value.
         *
         * @param helper CRDCDataHelper
         * @param beanPath String
         * @param row ExportFormatRow
         * @return String
         */
        public String getCalculatedValue(CRDCDataHelper helper, String beanPath, ExportFormatRow row) {
            return m_fieldValueCalculator.getCalculatedValue(helper, beanPath, row);
        }

        /**
         * Returns appropriate field name that relates to the parameter.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Returns name of the parameter.
         *
         * @return String
         */
        public String getParamName() {
            return m_paramName;
        }
    }

    /**
     * The Interface Filter.
     *
     * @author Follett Software Company
     */
    private interface Filter {

        /**
         * Returns true if row passed this filter, otherwise false.
         *
         * @param row ExportFormatRow
         * @return boolean
         */
        public boolean doesRowPassTheFilter(ExportFormatRow row);

        /**
         * Returns datasets that can be filtered by this filter.
         *
         * @return Collection<Dataset>
         */
        public Collection<Dataset> getDatasets();

        /**
         * Returns simple filters for this filter.
         *
         * @return Collection<SimpleFilter>
         */
        public Collection<SimpleFilter> getSimpleFilters();

        /**
         * Returns true if passed row can be filtered by the filter, otherwise false.
         *
         * @param row ExportFormatRow
         * @return true, if successful
         */
        public boolean rowCanBeFiltered(ExportFormatRow row);
    }

    /**
     * The Class CRDCFinalEntity.
     */
    public static class CRDCFinalEntity extends StateReportEntity {

        /**
         * Gets the field value.
         *
         * @param index int
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValue(int)
         */
        @Override
        public String getFieldValue(int index) {
            String value = super.getFieldValue(index);
            FieldDefinition field = getData().getFieldDefinition(index);
            if (!StringUtils.isEmpty(value)
                    && ((!value.equals(field.getDefaultValue()) && !value.equals("0"))
                            || value.equals("No"))) {
                // valid value found
                ((CRDCFinalData) getData()).addValidFieldDefinition(field);
            }
            return value;
        }

    }

    /**
     * The Class CRDCFinalData.
     */
    public static class CRDCFinalData extends StateReportData {
        private static final Set<String> DESE_FIELDS = new HashSet(Arrays.asList("LEA_ENR",
                "LEA_ENR_NONLEAFAC",
                "LEA_SCHOOLS",
                "LEA_PS_IND",
                "LEA_PSENR_A2",
                "LEA_PSENR_A3",
                "LEA_PSENR_A4",
                "LEA_PSENR_A5",
                "SCH_GRADE_PS",
                "SCH_GRADE_KG",
                "SCH_GRADE_G01",
                "SCH_GRADE_G02",
                "SCH_GRADE_G03",
                "SCH_GRADE_G04",
                "SCH_GRADE_G05",
                "SCH_GRADE_G06",
                "SCH_GRADE_G07",
                "SCH_GRADE_G08",
                "SCH_GRADE_G09",
                "SCH_GRADE_G10",
                "SCH_GRADE_G11",
                "SCH_GRADE_G12",
                "SCH_GRADE_UG",
                "SCH_PSENR_HI_M",
                "SCH_PSENR_HI_F",
                "SCH_PSENR_AM_M",
                "SCH_PSENR_AM_F",
                "SCH_PSENR_AS_M",
                "SCH_PSENR_AS_F",
                "SCH_PSENR_HP_M",
                "SCH_PSENR_HP_F",
                "SCH_PSENR_BL_M",
                "SCH_PSENR_BL_F",
                "SCH_PSENR_WH_M",
                "SCH_PSENR_WH_F",
                "SCH_PSENR_TR_M",
                "SCH_PSENR_TR_F",
                "SCH_PSENR_LEP_M",
                "SCH_PSENR_LEP_F",
                "SCH_PSENR_IDEA_M",
                "SCH_PSENR_IDEA_F",
                "SCH_ENR_HI_M",
                "SCH_ENR_HI_F",
                "SCH_ENR_AM_M",
                "SCH_ENR_AM_F",
                "SCH_ENR_AS_M",
                "SCH_ENR_AS_F",
                "SCH_ENR_HP_M",
                "SCH_ENR_HP_F",
                "SCH_ENR_BL_M",
                "SCH_ENR_BL_F",
                "SCH_ENR_WH_M",
                "SCH_ENR_WH_F",
                "SCH_ENR_TR_M",
                "SCH_ENR_TR_F",
                "SCH_ENR_LEP_M",
                "SCH_ENR_LEP_F",
                "SCH_ENR_IDEA_M",
                "SCH_ENR_IDEA_F",
                "SCH_ENR_504_M",
                "SCH_ENR_504_F",
                "SCH_LEPENR_HI_M",
                "SCH_LEPENR_HI_F",
                "SCH_LEPENR_AM_M",
                "SCH_LEPENR_AM_F",
                "SCH_LEPENR_AS_M",
                "SCH_LEPENR_AS_F",
                "SCH_LEPENR_HP_M",
                "SCH_LEPENR_HP_F",
                "SCH_LEPENR_BL_M",
                "SCH_LEPENR_BL_F",
                "SCH_LEPENR_WH_M",
                "SCH_LEPENR_WH_F",
                "SCH_LEPENR_TR_M",
                "SCH_LEPENR_TR_F",
                "SCH_LEPPROGENR_HI_M",
                "SCH_LEPPROGENR_HI_F",
                "SCH_LEPPROGENR_AM_M",
                "SCH_LEPPROGENR_AM_F",
                "SCH_LEPPROGENR_AS_M",
                "SCH_LEPPROGENR_AS_F",
                "SCH_LEPPROGENR_HP_M",
                "SCH_LEPPROGENR_HP_F",
                "SCH_LEPPROGENR_BL_M",
                "SCH_LEPPROGENR_BL_F",
                "SCH_LEPPROGENR_WH_M",
                "SCH_LEPPROGENR_WH_F",
                "SCH_LEPPROGENR_TR_M",
                "SCH_LEPPROGENR_TR_F",
                "SCH_IDEAENR_HI_M",
                "SCH_IDEAENR_HI_F",
                "SCH_IDEAENR_AM_M",
                "SCH_IDEAENR_AM_F",
                "SCH_IDEAENR_AS_M",
                "SCH_IDEAENR_AS_F",
                "SCH_IDEAENR_HP_M",
                "SCH_IDEAENR_HP_F",
                "SCH_IDEAENR_BL_M",
                "SCH_IDEAENR_BL_F",
                "SCH_IDEAENR_WH_M",
                "SCH_IDEAENR_WH_F",
                "SCH_IDEAENR_TR_M",
                "SCH_IDEAENR_TR_F",
                "SCH_IDEAENR_LEP_M",
                "SCH_IDEAENR_LEP_F",
                "SCH_504ENR_HI_M",
                "SCH_504ENR_HI_F",
                "SCH_504ENR_AM_M",
                "SCH_504ENR_AM_F",
                "SCH_504ENR_AS_M",
                "SCH_504ENR_AS_F",
                "SCH_504ENR_HP_M",
                "SCH_504ENR_HP_F",
                "SCH_504ENR_BL_M",
                "SCH_504ENR_BL_F",
                "SCH_504ENR_WH_M",
                "SCH_504ENR_WH_F",
                "SCH_504ENR_TR_M",
                "SCH_504ENR_TR_F",
                "SCH_504ENR_LEP_M",
                "SCH_504ENR_LEP_F",
                "SCH_DUAL_IND",
                "SCH_DUALENR_HI_M",
                "SCH_DUALENR_HI_F",
                "SCH_DUALENR_AM_M",
                "SCH_DUALENR_AM_F",
                "SCH_DUALENR_AS_M",
                "SCH_DUALENR_AS_F",
                "SCH_DUALENR_HP_M",
                "SCH_DUALENR_HP_F",
                "SCH_DUALENR_BL_M",
                "SCH_DUALENR_BL_F",
                "SCH_DUALENR_WH_M",
                "SCH_DUALENR_WH_F",
                "SCH_DUALENR_TR_M",
                "SCH_DUALENR_TR_F",
                "SCH_DUALENR_LEP_M",
                "SCH_DUALENR_LEP_F",
                "SCH_DUALENR_IDEA_M",
                "SCH_DUALENR_IDEA_F",
                "SCH_ALGCLASSES_GS0708",
                "SCH_ALGCERT_GS0708",
                "SCH_ALGENR_G07_IND",
                "SCH_ALGENR_G08_IND",
                "SCH_ALGENR_G07",
                "SCH_ALGENR_G08_HI_M",
                "SCH_ALGENR_G08_HI_F",
                "SCH_ALGENR_G08_AM_M",
                "SCH_ALGENR_G08_AM_F",
                "SCH_ALGENR_G08_AS_M",
                "SCH_ALGENR_G08_AS_F",
                "SCH_ALGENR_G08_HP_M",
                "SCH_ALGENR_G08_HP_F",
                "SCH_ALGENR_G08_BL_M",
                "SCH_ALGENR_G08_BL_F",
                "SCH_ALGENR_G08_WH_M",
                "SCH_ALGENR_G08_WH_F",
                "SCH_ALGENR_G08_TR_M",
                "SCH_ALGENR_G08_TR_F",
                "SCH_ALGENR_G08_LEP_M",
                "SCH_ALGENR_G08_LEP_F",
                "SCH_ALGENR_G08_IDEA_M",
                "SCH_ALGENR_G08_IDEA_F",
                "SCH_ALGPASS_G07",
                "SCH_ALGPASS_G08_HI_M",
                "SCH_ALGPASS_G08_HI_F",
                "SCH_ALGPASS_G08_AM_M",
                "SCH_ALGPASS_G08_AM_F",
                "SCH_ALGPASS_G08_AS_M",
                "SCH_ALGPASS_G08_AS_F",
                "SCH_ALGPASS_G08_HP_M",
                "SCH_ALGPASS_G08_HP_F",
                "SCH_ALGPASS_G08_BL_M",
                "SCH_ALGPASS_G08_BL_F",
                "SCH_ALGPASS_G08_WH_M",
                "SCH_ALGPASS_G08_WH_F",
                "SCH_ALGPASS_G08_TR_M",
                "SCH_ALGPASS_G08_TR_F",
                "SCH_ALGPASS_G08_LEP_M",
                "SCH_ALGPASS_G08_LEP_F",
                "SCH_ALGPASS_G08_IDEA_M",
                "SCH_ALGPASS_G08_IDEA_F",
                "SCH_GEOMENR_G08_IND",
                "SCH_GEOMENR_G08",
                "SCH_ALGENR_GS0910_HI_M",
                "SCH_ALGENR_GS0910_HI_F",
                "SCH_ALGENR_GS0910_AM_M",
                "SCH_ALGENR_GS0910_AM_F",
                "SCH_ALGENR_GS0910_AS_M",
                "SCH_ALGENR_GS0910_AS_F",
                "SCH_ALGENR_GS0910_HP_M",
                "SCH_ALGENR_GS0910_HP_F",
                "SCH_ALGENR_GS0910_BL_M",
                "SCH_ALGENR_GS0910_BL_F",
                "SCH_ALGENR_GS0910_WH_M",
                "SCH_ALGENR_GS0910_WH_F",
                "SCH_ALGENR_GS0910_TR_M",
                "SCH_ALGENR_GS0910_TR_F",
                "SCH_ALGENR_GS0910_LEP_M",
                "SCH_ALGENR_GS0910_LEP_F",
                "SCH_ALGENR_GS0910_IDEA_M",
                "SCH_ALGENR_GS0910_IDEA_F",
                "SCH_ALGENR_GS1112_HI_M",
                "SCH_ALGENR_GS1112_HI_F",
                "SCH_ALGENR_GS1112_AM_M",
                "SCH_ALGENR_GS1112_AM_F",
                "SCH_ALGENR_GS1112_AS_M",
                "SCH_ALGENR_GS1112_AS_F",
                "SCH_ALGENR_GS1112_HP_M",
                "SCH_ALGENR_GS1112_HP_F",
                "SCH_ALGENR_GS1112_BL_M",
                "SCH_ALGENR_GS1112_BL_F",
                "SCH_ALGENR_GS1112_WH_M",
                "SCH_ALGENR_GS1112_WH_F",
                "SCH_ALGENR_GS1112_TR_M",
                "SCH_ALGENR_GS1112_TR_F",
                "SCH_ALGENR_GS1112_LEP_M",
                "SCH_ALGENR_GS1112_LEP_F",
                "SCH_ALGENR_GS1112_IDEA_M",
                "SCH_ALGENR_GS1112_IDEA_F",
                "SCH_ALGPASS_GS0910_HI_M",
                "SCH_ALGPASS_GS0910_HI_F",
                "SCH_ALGPASS_GS0910_AM_M",
                "SCH_ALGPASS_GS0910_AM_F",
                "SCH_ALGPASS_GS0910_AS_M",
                "SCH_ALGPASS_GS0910_AS_F",
                "SCH_ALGPASS_GS0910_HP_M",
                "SCH_ALGPASS_GS0910_HP_F",
                "SCH_ALGPASS_GS0910_BL_M",
                "SCH_ALGPASS_GS0910_BL_F",
                "SCH_ALGPASS_GS0910_WH_M",
                "SCH_ALGPASS_GS0910_WH_F",
                "SCH_ALGPASS_GS0910_TR_M",
                "SCH_ALGPASS_GS0910_TR_F",
                "SCH_ALGPASS_GS0910_LEP_M",
                "SCH_ALGPASS_GS0910_LEP_F",
                "SCH_ALGPASS_GS0910_IDEA_M",
                "SCH_ALGPASS_GS0910_IDEA_F",
                "SCH_ALGPASS_GS1112_HI_M",
                "SCH_ALGPASS_GS1112_HI_F",
                "SCH_ALGPASS_GS1112_AM_M",
                "SCH_ALGPASS_GS1112_AM_F",
                "SCH_ALGPASS_GS1112_AS_M",
                "SCH_ALGPASS_GS1112_AS_F",
                "SCH_ALGPASS_GS1112_HP_M",
                "SCH_ALGPASS_GS1112_HP_F",
                "SCH_ALGPASS_GS1112_BL_M",
                "SCH_ALGPASS_GS1112_BL_F",
                "SCH_ALGPASS_GS1112_WH_M",
                "SCH_ALGPASS_GS1112_WH_F",
                "SCH_ALGPASS_GS1112_TR_M",
                "SCH_ALGPASS_GS1112_TR_F",
                "SCH_ALGPASS_GS1112_LEP_M",
                "SCH_ALGPASS_GS1112_LEP_F",
                "SCH_ALGPASS_GS1112_IDEA_M",
                "SCH_ALGPASS_GS1112_IDEA_F",
                "SCH_MATHCLASSES_GEOM",
                "SCH_MATHCLASSES_ALG",
                "SCH_MATHCLASSES_ALG2",
                "SCH_MATHCLASSES_ADVM",
                "SCH_MATHCLASSES_CALC",
                "SCH_MATHCERT_GEOM",
                "SCH_MATHCERT_ALG",
                "SCH_MATHCERT_ALG2",
                "SCH_MATHCERT_ADVM",
                "SCH_MATHCERT_CALC",
                "SCH_MATHENR_ALG2_HI_M",
                "SCH_MATHENR_ALG2_HI_F",
                "SCH_MATHENR_ALG2_AM_M",
                "SCH_MATHENR_ALG2_AM_F",
                "SCH_MATHENR_ALG2_AS_M",
                "SCH_MATHENR_ALG2_AS_F",
                "SCH_MATHENR_ALG2_HP_M",
                "SCH_MATHENR_ALG2_HP_F",
                "SCH_MATHENR_ALG2_BL_M",
                "SCH_MATHENR_ALG2_BL_F",
                "SCH_MATHENR_ALG2_WH_M",
                "SCH_MATHENR_ALG2_WH_F",
                "SCH_MATHENR_ALG2_TR_M",
                "SCH_MATHENR_ALG2_TR_F",
                "SCH_MATHENR_ALG2_LEP_M",
                "SCH_MATHENR_ALG2_LEP_F",
                "SCH_MATHENR_ALG2_IDEA_M",
                "SCH_MATHENR_ALG2_IDEA_F",
                "SCH_MATHENR_ADVM_HI_M",
                "SCH_MATHENR_ADVM_HI_F",
                "SCH_MATHENR_ADVM_AM_M",
                "SCH_MATHENR_ADVM_AM_F",
                "SCH_MATHENR_ADVM_AS_M",
                "SCH_MATHENR_ADVM_AS_F",
                "SCH_MATHENR_ADVM_HP_M",
                "SCH_MATHENR_ADVM_HP_F",
                "SCH_MATHENR_ADVM_BL_M",
                "SCH_MATHENR_ADVM_BL_F",
                "SCH_MATHENR_ADVM_WH_M",
                "SCH_MATHENR_ADVM_WH_F",
                "SCH_MATHENR_ADVM_TR_M",
                "SCH_MATHENR_ADVM_TR_F",
                "SCH_MATHENR_ADVM_LEP_M",
                "SCH_MATHENR_ADVM_LEP_F",
                "SCH_MATHENR_ADVM_IDEA_M",
                "SCH_MATHENR_ADVM_IDEA_F",
                "SCH_MATHENR_CALC_HI_M",
                "SCH_MATHENR_CALC_HI_F",
                "SCH_MATHENR_CALC_AM_M",
                "SCH_MATHENR_CALC_AM_F",
                "SCH_MATHENR_CALC_AS_M",
                "SCH_MATHENR_CALC_AS_F",
                "SCH_MATHENR_CALC_HP_M",
                "SCH_MATHENR_CALC_HP_F",
                "SCH_MATHENR_CALC_BL_M",
                "SCH_MATHENR_CALC_BL_F",
                "SCH_MATHENR_CALC_WH_M",
                "SCH_MATHENR_CALC_WH_F",
                "SCH_MATHENR_CALC_TR_M",
                "SCH_MATHENR_CALC_TR_F",
                "SCH_MATHENR_CALC_LEP_M",
                "SCH_MATHENR_CALC_LEP_F",
                "SCH_MATHENR_CALC_IDEA_M",
                "SCH_MATHENR_CALC_IDEA_F",
                "SCH_MATHENR_GEOM_HI_M",
                "SCH_MATHENR_GEOM_HI_F",
                "SCH_MATHENR_GEOM_AM_M",
                "SCH_MATHENR_GEOM_AM_F",
                "SCH_MATHENR_GEOM_AS_M",
                "SCH_MATHENR_GEOM_AS_F",
                "SCH_MATHENR_GEOM_HP_M",
                "SCH_MATHENR_GEOM_HP_F",
                "SCH_MATHENR_GEOM_BL_M",
                "SCH_MATHENR_GEOM_BL_F",
                "SCH_MATHENR_GEOM_WH_M",
                "SCH_MATHENR_GEOM_WH_F",
                "SCH_MATHENR_GEOM_TR_M",
                "SCH_MATHENR_GEOM_TR_F",
                "SCH_MATHENR_GEOM_LEP_M",
                "SCH_MATHENR_GEOM_LEP_F",
                "SCH_MATHENR_GEOM_IDEA_M",
                "SCH_MATHENR_GEOM_IDEA_F",
                "SCH_SCICLASSES_BIOL",
                "SCH_SCICLASSES_CHEM",
                "SCH_SCICLASSES_PHYS",
                "SCH_SCIENR_BIOL_HI_M",
                "SCH_SCIENR_BIOL_HI_F",
                "SCH_SCIENR_BIOL_AM_M",
                "SCH_SCIENR_BIOL_AM_F",
                "SCH_SCIENR_BIOL_AS_M",
                "SCH_SCIENR_BIOL_AS_F",
                "SCH_SCIENR_BIOL_HP_M",
                "SCH_SCIENR_BIOL_HP_F",
                "SCH_SCIENR_BIOL_BL_M",
                "SCH_SCIENR_BIOL_BL_F",
                "SCH_SCIENR_BIOL_WH_M",
                "SCH_SCIENR_BIOL_WH_F",
                "SCH_SCIENR_BIOL_TR_M",
                "SCH_SCIENR_BIOL_TR_F",
                "SCH_SCIENR_BIOL_LEP_M",
                "SCH_SCIENR_BIOL_LEP_F",
                "SCH_SCIENR_BIOL_IDEA_M",
                "SCH_SCIENR_BIOL_IDEA_F",
                "SCH_SCIENR_CHEM_HI_M",
                "SCH_SCIENR_CHEM_HI_F",
                "SCH_SCIENR_CHEM_AM_M",
                "SCH_SCIENR_CHEM_AM_F",
                "SCH_SCIENR_CHEM_AS_M",
                "SCH_SCIENR_CHEM_AS_F",
                "SCH_SCIENR_CHEM_HP_M",
                "SCH_SCIENR_CHEM_HP_F",
                "SCH_SCIENR_CHEM_BL_M",
                "SCH_SCIENR_CHEM_BL_F",
                "SCH_SCIENR_CHEM_WH_M",
                "SCH_SCIENR_CHEM_WH_F",
                "SCH_SCIENR_CHEM_TR_M",
                "SCH_SCIENR_CHEM_TR_F",
                "SCH_SCIENR_CHEM_LEP_M",
                "SCH_SCIENR_CHEM_LEP_F",
                "SCH_SCIENR_CHEM_IDEA_M",
                "SCH_SCIENR_CHEM_IDEA_F",
                "SCH_SCIENR_PHYS_HI_M",
                "SCH_SCIENR_PHYS_HI_F",
                "SCH_SCIENR_PHYS_AM_M",
                "SCH_SCIENR_PHYS_AM_F",
                "SCH_SCIENR_PHYS_AS_M",
                "SCH_SCIENR_PHYS_AS_F",
                "SCH_SCIENR_PHYS_HP_M",
                "SCH_SCIENR_PHYS_HP_F",
                "SCH_SCIENR_PHYS_BL_M",
                "SCH_SCIENR_PHYS_BL_F",
                "SCH_SCIENR_PHYS_WH_M",
                "SCH_SCIENR_PHYS_WH_F",
                "SCH_SCIENR_PHYS_TR_M",
                "SCH_SCIENR_PHYS_TR_F",
                "SCH_SCIENR_PHYS_LEP_M",
                "SCH_SCIENR_PHYS_LEP_F",
                "SCH_SCIENR_PHYS_IDEA_M",
                "SCH_SCIENR_PHYS_IDEA_F",
                "SCH_SCICCERT_BIOL",
                "SCH_SCICCERT_CHEM",
                "SCH_SCICCERT_PHYS",
                "SCH_COMPCLASSES_CSCI",
                "SCH_COMPCCERT_CSCI",
                "SCH_COMPENR_CSCI_HI_M",
                "SCH_COMPENR_CSCI_HI_F",
                "SCH_COMPENR_CSCI_AM_M",
                "SCH_COMPENR_CSCI_AM_F",
                "SCH_COMPENR_CSCI_AS_M",
                "SCH_COMPENR_CSCI_AS_F",
                "SCH_COMPENR_CSCI_HP_M",
                "SCH_COMPENR_CSCI_HP_F",
                "SCH_COMPENR_CSCI_BL_M",
                "SCH_COMPENR_CSCI_BL_F",
                "SCH_COMPENR_CSCI_WH_M",
                "SCH_COMPENR_CSCI_WH_F",
                "SCH_COMPENR_CSCI_TR_M",
                "SCH_COMPENR_CSCI_TR_F",
                "SCH_COMPENR_CSCI_LEP_M",
                "SCH_COMPENR_CSCI_LEP_F",
                "SCH_COMPENR_CSCI_IDEA_M",
                "SCH_COMPENR_CSCI_IDEA_F",
                "SCH_APENR_IND",
                "SCH_APENR_HI_M",
                "SCH_APENR_HI_F",
                "SCH_APENR_AM_M",
                "SCH_APENR_AM_F",
                "SCH_APENR_AS_M",
                "SCH_APENR_AS_F",
                "SCH_APENR_HP_M",
                "SCH_APENR_HP_F",
                "SCH_APENR_BL_M",
                "SCH_APENR_BL_F",
                "SCH_APENR_WH_M",
                "SCH_APENR_WH_F",
                "SCH_APENR_TR_M",
                "SCH_APENR_TR_F",
                "SCH_APENR_LEP_M",
                "SCH_APENR_LEP_F",
                "SCH_APENR_IDEA_M",
                "SCH_APENR_IDEA_F",
                "SCH_APENR_504_M",
                "SCH_APENR_504_F",
                "SCH_APMATHENR_IND",
                "SCH_APMATHENR_HI_M",
                "SCH_APMATHENR_HI_F",
                "SCH_APMATHENR_AM_M",
                "SCH_APMATHENR_AM_F",
                "SCH_APMATHENR_AS_M",
                "SCH_APMATHENR_AS_F",
                "SCH_APMATHENR_HP_M",
                "SCH_APMATHENR_HP_F",
                "SCH_APMATHENR_BL_M",
                "SCH_APMATHENR_BL_F",
                "SCH_APMATHENR_WH_M",
                "SCH_APMATHENR_WH_F",
                "SCH_APMATHENR_TR_M",
                "SCH_APMATHENR_TR_F",
                "SCH_APMATHENR_LEP_M",
                "SCH_APMATHENR_LEP_F",
                "SCH_APMATHENR_IDEA_M",
                "SCH_APMATHENR_IDEA_F",
                "SCH_APSCIENR_IND",
                "SCH_APSCIENR_HI_M",
                "SCH_APSCIENR_HI_F",
                "SCH_APSCIENR_AM_M",
                "SCH_APSCIENR_AM_F",
                "SCH_APSCIENR_AS_M",
                "SCH_APSCIENR_AS_F",
                "SCH_APSCIENR_HP_M",
                "SCH_APSCIENR_HP_F",
                "SCH_APSCIENR_BL_M",
                "SCH_APSCIENR_BL_F",
                "SCH_APSCIENR_WH_M",
                "SCH_APSCIENR_WH_F",
                "SCH_APSCIENR_TR_M",
                "SCH_APSCIENR_TR_F",
                "SCH_APSCIENR_LEP_M",
                "SCH_APSCIENR_LEP_F",
                "SCH_APSCIENR_IDEA_M",
                "SCH_APSCIENR_IDEA_F",
                "SCH_APCOMPENR_IND",
                "SCH_APCOMPENR_HI_M",
                "SCH_APCOMPENR_HI_F",
                "SCH_APCOMPENR_AM_M",
                "SCH_APCOMPENR_AM_F",
                "SCH_APCOMPENR_AS_M",
                "SCH_APCOMPENR_AS_F",
                "SCH_APCOMPENR_HP_M",
                "SCH_APCOMPENR_HP_F",
                "SCH_APCOMPENR_BL_M",
                "SCH_APCOMPENR_BL_F",
                "SCH_APCOMPENR_WH_M",
                "SCH_APCOMPENR_WH_F",
                "SCH_APCOMPENR_TR_M",
                "SCH_APCOMPENR_TR_F",
                "SCH_APCOMPENR_LEP_M",
                "SCH_APCOMPENR_LEP_F",
                "SCH_APCOMPENR_IDEA_M",
                "SCH_APCOMPENR_IDEA_F",
                "SCH_FTECOUNSELORS",
                "SCH_FTESERVICES_NUR",
                "SCH_FTESERVICES_PSY",
                "SCH_FTESERVICES_SOC"));
        private static final String INPUT_PARAM_OMIT_DESE_FIELDS = "omitDESEFields";
        private static final String INPUT_PARAM_OMIT_NOT_SUPPORTED = "omitNotSupported";
        private static final String INPUT_PARAM_OMIT_NO_RESULT = "omitNoResult";

        private Set<FieldDefinition> m_validFieldDefinitions = new HashSet();

        /**
         * Close.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
         */
        @Override
        public void close() {
            super.close();
            if (getParameter(INPUT_PARAM_OMIT_NO_RESULT) != null
                    && getParameter(INPUT_PARAM_OMIT_NO_RESULT) instanceof Boolean
                    && ((Boolean) getParameter(INPUT_PARAM_OMIT_NO_RESULT)).booleanValue()) {
                Iterator<FieldDefinition> iter = getFieldDefinitions().iterator();
                while (iter.hasNext()) {
                    FieldDefinition definition = iter.next();
                    if (!m_validFieldDefinitions.contains(definition)) {
                        iter.remove();
                    }
                }
            }
        }

        /**
         * Gets the heading.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
         */
        @Override
        public String getHeading() {
            StringBuilder heading = new StringBuilder();
            Iterator<FieldDefinition> iter = getFieldDefinitions().iterator();
            while (iter.hasNext()) {
                FieldDefinition definition = iter.next();
                if (heading.length() > 0) {
                    heading.append(",");
                }
                heading.append(definition.getSifPath());
                if (StringUtils.isEmpty(definition.getCalcId())) {
                    iter.remove();
                }
            }
            heading.append(ExportJavaSource.FORMAT_EOL_WINDOWS);
            return heading.toString();
        }

        /**
         * Initialize.
         *
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
         */
        @Override
        protected void initialize() throws X2BaseException {
            super.initialize();
            if (getParameter(INPUT_PARAM_OMIT_NOT_SUPPORTED) != null
                    && getParameter(INPUT_PARAM_OMIT_NOT_SUPPORTED) instanceof Boolean
                    && ((Boolean) getParameter(INPUT_PARAM_OMIT_NOT_SUPPORTED)).booleanValue()) {
                Iterator<FieldDefinition> iter = getFieldDefinitions().iterator();
                while (iter.hasNext()) {
                    FieldDefinition definition = iter.next();
                    if (StringUtils.isEmpty(definition.getCalcId())) {
                        iter.remove();
                    }
                }
            }

            if (getParameter(INPUT_PARAM_OMIT_DESE_FIELDS) != null
                    && getParameter(INPUT_PARAM_OMIT_DESE_FIELDS) instanceof Boolean
                    && ((Boolean) getParameter(INPUT_PARAM_OMIT_DESE_FIELDS)).booleanValue()) {
                Iterator<FieldDefinition> iter = getFieldDefinitions().iterator();
                while (iter.hasNext()) {
                    FieldDefinition definition = iter.next();
                    if (DESE_FIELDS.contains(definition.getSifPath())) {
                        iter.remove();
                    }
                }
            }
        }

        /**
         * Adds the valid field definition.
         *
         * @param field FieldDefinition
         */
        public void addValidFieldDefinition(FieldDefinition field) {
            m_validFieldDefinitions.add(field);
        }

    }
    /**
     * Complex filter.
     * Used to filter rows by several simple filters.
     *
     * @author Follett Software Company
     */
    private class ComplexFilterAlt implements Filter {
        Collection<SimpleFilter> m_altFilters = null;
        Collection<Dataset> m_datasets = null;

        /**
         * Instantiates a new complex filter alt.
         *
         * @param filters Collection<SimpleFilter>
         */
        public ComplexFilterAlt(Collection<SimpleFilter> filters) {
            m_altFilters = filters;
        }

        /**
         * Returns true if row passed this filter, otherwise false.
         *
         * @param row ExportFormatRow
         * @return boolean
         */
        @Override
        public boolean doesRowPassTheFilter(ExportFormatRow row) {
            boolean doesRowPassTheFilter = false;

            for (SimpleFilter simpleFilter : m_altFilters) {
                if (simpleFilter.rowCanBeFiltered(row) && simpleFilter.doesRowPassTheFilter(row)) {
                    doesRowPassTheFilter = true;
                    break;
                }
            }

            return doesRowPassTheFilter;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof ComplexFilterAlt)) {
                return false;
            }
            if (obj == this) {
                return true;
            }

            ComplexFilterAlt rhs = (ComplexFilterAlt) obj;
            return new EqualsBuilder().append(m_altFilters, rhs.m_altFilters).append(m_datasets, rhs.m_datasets)
                    .isEquals();
        }

        /**
         * Returns datasets that can be filtered by this filter.
         *
         * @return Collection<Dataset>
         */
        @Override
        public Collection<Dataset> getDatasets() {
            if (m_datasets == null) {
                m_datasets = new ArrayList<Dataset>();

                for (SimpleFilter simpleFilter : m_altFilters) {
                    m_datasets.addAll(simpleFilter.getDatasets());
                }
            }

            return m_datasets;
        }

        /**
         * Returns simple filters for this filter.
         *
         * @return Collection<SimpleFilter>
         */
        @Override
        public Collection<SimpleFilter> getSimpleFilters() {
            return m_altFilters;
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return new HashCodeBuilder(17, 31).append(m_altFilters).append(m_datasets).toHashCode();
        }

        /**
         * Returns true if row can be filtered by this filter, otherwise false.
         *
         * @param row ExportFormatRow
         * @return boolean
         */
        @Override
        public boolean rowCanBeFiltered(ExportFormatRow row) {
            boolean rowCanBeFiltered = false;

            String procedureId = getProcedureIdByRow(row);

            for (Dataset dataset : getDatasets()) {
                if (dataset.getProcedureId().equals(procedureId)) {
                    rowCanBeFiltered = true;
                    break;
                }
            }
            return rowCanBeFiltered;
        }
    }

    /**
     * Simple filter.
     * Used to filter rows just by one filtered value.
     *
     * @author Follett Software Company
     */
    private class SimpleFilter implements Filter {
        private static final String KEY_SEPARATOR = "|";
        private Collection<Dataset> m_datasets = null;
        private Collection<String> m_filteredValues = null;
        private CalcParameter m_parameter = null;
        private String m_sign = null;

        /**
         * Instantiates a new simple filter.
         *
         * @param parameter CalcParameter
         * @param sign String
         * @param filteredValues Collection<String>
         */
        public SimpleFilter(CalcParameter parameter, String sign, Collection<String> filteredValues) {
            m_parameter = parameter;
            m_sign = sign;
            m_filteredValues = filteredValues;
        }

        /**
         * Returns filtered set of rows for passed datased.
         *
         * @param dataset Dataset
         * @return Set<ExportFormatRow>
         */
        public Set<ExportFormatRow> getFilteredSet(Dataset dataset) {
            Set<ExportFormatRow> values = null;
            if (m_datasets.contains(dataset)) {
                values = CRDCDataHelper.this.getFilteredSet(dataset, this);
            }
            return values;
        }

        /**
         * Returns true if row passed the filter, otherwise false.
         *
         * @param row ExportFormatRow
         * @return boolean
         */
        @Override
        public boolean doesRowPassTheFilter(ExportFormatRow row) {
            // Check if parameter and row owned by the same dataset, otherwise throw exception.
            if (!rowCanBeFiltered(row)) {
                throw new X2RuntimeException();
            }

            // Check if row pass the filter
            Dataset rowDataset = Dataset.findDatasetByParameter(m_parameter);
            String beanPath = getBeanPath(rowDataset.getProcedureId(), m_parameter.getFieldName());
            String value = m_parameter.getCalculatedValue(CRDCDataHelper.this, beanPath, row);

            boolean doesRowPassTheFilter = false;
            switch (m_sign) {
                case "=":
                    doesRowPassTheFilter = checkEqualSign(value);
                    break;

                case ">":
                    doesRowPassTheFilter = checkGreaterThanSign(value);
                    break;

                case ">=":
                    doesRowPassTheFilter = checkGreaterThanOrEqualSign(value);
                    break;

                case "<":
                    doesRowPassTheFilter = checkLessThanSign(value);
                    break;

                case "<=":
                    doesRowPassTheFilter = checkLessThanOrEqualSign(value);
                    break;

                case "!=":
                    doesRowPassTheFilter = checkNotEqualSign(value);
                    break;

                default:
                    break;
            }

            return doesRowPassTheFilter;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof SimpleFilter)) {
                return false;
            }
            if (obj == this) {
                return true;
            }

            SimpleFilter rhs = (SimpleFilter) obj;
            return new EqualsBuilder().append(m_parameter, rhs.m_parameter).append(m_sign, rhs.m_sign)
                    .append(m_filteredValues, rhs.m_filteredValues).isEquals();
        }

        /**
         * Returns datasets that can be filtered by this filter.
         *
         * @return Collection<Dataset>
         */
        @Override
        public Collection<Dataset> getDatasets() {
            if (m_datasets == null) {
                m_datasets = new ArrayList<Dataset>();
                Dataset dataset = Dataset.findDatasetByParameter(m_parameter);
                m_datasets.add(dataset);
            }

            return m_datasets;
        }

        /**
         * Returns list of simple filters for this filter.
         *
         * @return Collection<SimpleFilter>
         */
        @Override
        public Collection<SimpleFilter> getSimpleFilters() {
            return Arrays.asList(this);
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return new HashCodeBuilder(17, 31).append(m_parameter).append(m_sign).append(m_filteredValues).toHashCode();
        }

        /**
         * Returns key constructed from field name, sign and filtered values.
         *
         * @return String
         */
        public String key() {
            StringBuilder key = new StringBuilder();
            key.append(m_parameter.getFieldName());
            key.append(KEY_SEPARATOR);
            key.append(m_sign);
            key.append(KEY_SEPARATOR);
            Set<String> values = new TreeSet<String>(m_filteredValues);
            for (String value : values) {
                key.append(value);
                key.append(KEY_SEPARATOR);
            }
            return key.toString();
        }

        /**
         * Returns true if passed row can be filtered by the filter, otherwise false.
         *
         * @param row ExportFormatRow
         * @return true, if successful
         */
        @Override
        public boolean rowCanBeFiltered(ExportFormatRow row) {
            boolean rowCanBeFiltered = true;

            Dataset parameterDataset = Dataset.findDatasetByParameter(m_parameter);

            if (!parameterDataset.getProcedureId().equals(getProcedureIdByRow(row))) {
                rowCanBeFiltered = false;
            }
            return rowCanBeFiltered;
        }

        /**
         * Returns true if passed value equals to one of the filtered values, otherwise false.
         *
         * @param rowValue String
         * @return boolean
         */
        private boolean checkEqualSign(String rowValue) {
            boolean equal = false;

            if (m_filteredValues.contains(rowValue)) {
                equal = true;
            }

            return equal;
        }

        /**
         * Returns true if passed value greater than filtered value, otherwise false.
         *
         * @param rowValue String
         * @return boolan
         */
        private boolean checkGreaterThanSign(String rowValue) {
            boolean graterThan = false;

            String comparedValue = m_filteredValues.iterator().next();

            BigDecimal comparedValueDecimal = new BigDecimal(comparedValue);
            BigDecimal rowValueDecimal = null;
            try {
                rowValueDecimal = new BigDecimal(rowValue);
            } catch (Exception e) {
                // Conversion failed
            }


            if (rowValueDecimal != null && rowValueDecimal.compareTo(comparedValueDecimal) > 0) {
                graterThan = true;
            }

            return graterThan;
        }

        /**
         * Returns true if passed value greater than or equal to filtered value.
         *
         * @param rowValue String
         * @return boolean
         */
        private boolean checkGreaterThanOrEqualSign(String rowValue) {
            boolean greaterThanOrEqual = false;

            String comparedValue = m_filteredValues.iterator().next();

            BigDecimal comparedValueDecimal = new BigDecimal(comparedValue);
            BigDecimal rowValueDecimal = null;
            try {
                rowValueDecimal = new BigDecimal(rowValue);
            } catch (Exception e) {
                // Conversion failed
            }

            if (rowValueDecimal != null && rowValueDecimal.compareTo(comparedValueDecimal) >= 0) {
                greaterThanOrEqual = true;
            }

            return greaterThanOrEqual;
        }

        /**
         * Returns true if passed value less than filtered value, otherwise false.
         *
         * @param rowValue String
         * @return boolean
         */
        private boolean checkLessThanSign(String rowValue) {
            boolean lessThan = false;

            String comparedValue = m_filteredValues.iterator().next();

            BigDecimal comparedValueDecimal = new BigDecimal(comparedValue);
            BigDecimal rowValueDecimal = null;
            try {
                rowValueDecimal = new BigDecimal(rowValue);
            } catch (Exception e) {
                // Conversion failed
            }

            if (rowValueDecimal != null && rowValueDecimal.compareTo(comparedValueDecimal) < 0) {
                lessThan = true;
            }

            return lessThan;
        }

        /**
         * Returns true if passed value less than or equal to filtered value, otherwise false.
         *
         * @param rowValue String
         * @return boolean
         */
        private boolean checkLessThanOrEqualSign(String rowValue) {
            boolean lessThanOrEqual = false;

            String comparedValue = m_filteredValues.iterator().next();

            BigDecimal comparedValueDecimal = new BigDecimal(comparedValue);
            BigDecimal rowValueDecimal = null;
            try {
                rowValueDecimal = new BigDecimal(rowValue);
            } catch (Exception e) {
                // Conversion failed
            }

            if (rowValueDecimal != null && rowValueDecimal.compareTo(comparedValueDecimal) <= 0) {
                lessThanOrEqual = true;
            }

            return lessThanOrEqual;
        }

        /**
         * Returns true if filtered values don't contain passed value, otherwise false.
         *
         * @param rowValue String
         * @return boolean
         */
        private boolean checkNotEqualSign(String rowValue) {
            boolean notEqual = false;

            if (!m_filteredValues.contains(rowValue)) {
                notEqual = true;
            }

            return notEqual;
        }
    }

    /**
     * Instantiates a new CRDC data helper.
     *
     * @param data StateReportData
     */
    public CRDCDataHelper(StateReportData data) {
        this(data, null);
    }

    /**
     * Instantiates a new CRDC data helper.
     *
     * @param data StateReportData
     * @param crdcResultsCriteria X2Criteria
     */
    public CRDCDataHelper(StateReportData data, X2Criteria crdcResultsCriteria) {
        m_data = data;
        if (crdcResultsCriteria != null) {
            initResults(m_data.getBroker(), crdcResultsCriteria);
            initRows();
            initFieldJavaNameMapping();
        }
    }

    /**
     * Instantiates a new CRDC data helper.
     *
     * WARNING - this helper initializes that dataset for query but cannot be used to generate data
     *
     * @param broker X2Broker
     * @param crdcResultsCriteria X2Criteria
     */
    public CRDCDataHelper(X2Broker broker, X2Criteria crdcResultsCriteria) {
        if (crdcResultsCriteria != null) {
            initResults(broker, crdcResultsCriteria);
            initRows();
            initFieldJavaNameMapping();
        }
    }

    private static final String DELIMITER_AND = ",";
    private static final String DELIMITER_LAST_PARAM = "";
    private static final String DELIMITER_OR = "|";

    private static final int GROUP_PARAM_NAME = 1;
    private static final int GROUP_PARAM_SIGN = 2;
    private static final int GROUP_PARAM_VALUE = 3;
    private static final int GROUP_PARAM_DELIMITER = 6;

    private static final String INTERFACE_NAME_FIELD_RETRIEVER =
            "com.follett.fsc.core.k12.tools.stateexports.FieldRetriever";
    private static final String INTERFACE_NAME_FIELD_VALIDATOR =
            "com.follett.fsc.core.k12.tools.stateexports.FieldValidator";

    private static final String REGEX_ANY_CHAR = ".{1}";
    private static final String REGEX_PATTERN_PARAM_NAME = "(.*?)(>=|<=|!=|=|>|<)([\\{](.*?)[\\}]|(.*?))(!?,|\\||$)";

    private static final String STRING_EMPTY = DELIMITER_LAST_PARAM;

    protected PlainDate m_reportDate = null;

    private StateReportData m_data = null;

    private Pattern m_paramPattern = Pattern.compile(REGEX_PATTERN_PARAM_NAME);

    private Map<String, Map<String, String>> m_procIdFieldNamesBeanPathesMap = null;
    private Map<String, ExportFormatResult> m_resultsIdMap = null;
    private Map<String, List<ExportFormatRow>> m_rowsIdMap = new HashMap<String, List<ExportFormatRow>>();
    private DoubleNestedMap<Map<String, Set<String>>> m_relationshipValuesMap = new DoubleNestedMap();
    private Map<String, Map<String, Map<String, Set<ExportFormatRow>>>> m_baseFilteredSets = new HashMap();
    private Map<String, Map<String, Set<ExportFormatRow>>> m_filteredSets = new HashMap();

    /**
     * Returns bean path by procedure id and field name.
     *
     * @param procedureId String
     * @param fieldName String
     * @return String
     */
    public String getBeanPath(String procedureId, String fieldName) {
        String beanPath = null;

        Map<String, String> fieldNameBeanPath = m_procIdFieldNamesBeanPathesMap.get(procedureId);

        if (fieldNameBeanPath != null) {
            beanPath = fieldNameBeanPath.get(fieldName);
        }

        return beanPath;
    }

    /**
     * Returns filtered rows by passed filters.
     *
     * @param baseDataset Dataset
     * @param calcParams String
     * @param filtersByFieldName Map<CRDCDataHelper.Dataset,String>
     * @return List<ExportFormatRow>
     */
    public List<ExportFormatRow> getFilteredRows(CRDCDataHelper.Dataset baseDataset,
                                                 String calcParams,
                                                 Map<CRDCDataHelper.Dataset, String> filtersByFieldName) {
        List<ExportFormatRow> filteredRows = new ArrayList<ExportFormatRow>();

        Collection<Filter> filters = new ArrayList<Filter>();
        if (!StringUtils.isEmpty(calcParams)) {
            filters = getParamFilter(calcParams);
        }

        Set<Filter> remainingFilters = new HashSet<Filter>();

        List<ExportFormatRow> filteredBaseRows =
                new ArrayList(applyFilters(baseDataset, filters, filtersByFieldName, remainingFilters));

        // If already filtered base rows size is 0, it doesn't make sense to filter and join
        // additional rows.
        if (filteredBaseRows.size() > 0) {
            boolean emptyResult = false;

            Collection<CRDCDataHelper.Dataset> additionalDatasets = getAdditionalDatasets(remainingFilters);

            ArrayList<KeyValuePair<CRDCDataHelper.Dataset, List<ExportFormatRow>>> additionalDatasetsRows =
                    new ArrayList();
            for (CRDCDataHelper.Dataset dataset : additionalDatasets) {
                Set<Filter> nextRemainingFilters = new HashSet<Filter>();

                List<ExportFormatRow> filteredAdditionalRows = new ArrayList(
                        applyFilters(dataset, remainingFilters, filtersByFieldName, nextRemainingFilters));

                remainingFilters = nextRemainingFilters;

                if (!filteredAdditionalRows.isEmpty()) {
                    additionalDatasetsRows.add(new KeyValuePair(dataset, filteredAdditionalRows));
                } else {
                    emptyResult = true;
                    break;
                }
            }

            if (!emptyResult) {
                if (additionalDatasetsRows.size() > 0) {
                    filteredRows = getJoinedBaseRows(baseDataset, filteredBaseRows, additionalDatasetsRows);
                } else {
                    filteredRows = filteredBaseRows;
                }
            }
        }

        return filteredRows;
    }

    /**
     * Get report date.
     *
     * @return Plain date
     */
    public PlainDate getReportDate() {
        return m_reportDate;
    }

    /**
     * Returns instances of retrievers that are used in the export format.
     *
     * @return Map<String, FieldRetriever>
     * @throws X2BaseException exception
     */
    public Map<String, FieldRetriever> getUsedRetrievers() throws X2BaseException {
        Map<String, Object> idObjectsMap = getUsedInnerInstancesByInterface(INTERFACE_NAME_FIELD_RETRIEVER);
        Map<String, FieldRetriever> idRetrieversMap = new HashMap<String, FieldRetriever>();

        for (Entry<String, Object> idObject : idObjectsMap.entrySet()) {
            if (idObject.getValue() instanceof FieldRetriever) {
                idRetrieversMap.put(idObject.getKey(), (FieldRetriever) idObject.getValue());
            }
        }
        return idRetrieversMap;
    }

    /**
     * Returns instances of validators that are used in the export format.
     *
     * @return Map<String, FieldValidator>
     * @throws X2BaseException exception
     */
    public Map<String, FieldValidator> getUsedValidators() throws X2BaseException {
        Map<String, Object> idObjectsMap = getUsedInnerInstancesByInterface(INTERFACE_NAME_FIELD_VALIDATOR);
        Map<String, FieldValidator> idValidatorsMap = new HashMap<String, FieldValidator>();

        for (Entry<String, Object> idObject : idObjectsMap.entrySet()) {
            if (idObject.getValue() instanceof FieldValidator) {
                idValidatorsMap.put(idObject.getKey(), (FieldValidator) idObject.getValue());
            }
        }
        return idValidatorsMap;
    }

    /**
     * Set report date.
     *
     * @param reportDate void
     */
    public void setReportDate(PlainDate reportDate) {
        m_reportDate = reportDate;
    }

    /**
     * Returns filtered rows by passed dataset and filter.
     *
     * @param dataset Dataset
     * @param filter SimpleFilter
     * @return Set<ExportFormatRow>
     */
    protected Set<ExportFormatRow> getFilteredSet(Dataset dataset, SimpleFilter filter) {
        String procedureId = dataset.getProcedureId();
        String key = filter.key();
        Map<String, Set<ExportFormatRow>> filteredSets = m_filteredSets.get(procedureId);
        if (filteredSets == null) {
            filteredSets = new HashMap();
            m_filteredSets.put(procedureId, filteredSets);
        }
        Set<ExportFormatRow> rows = filteredSets.get(key);
        if (rows == null) {
            rows = new HashSet();
            for (ExportFormatRow row : m_rowsIdMap.get(procedureId)) {
                if (filter.doesRowPassTheFilter(row)) {
                    rows.add(row);
                }
            }
            System.out.println(
                    "Creating filtered set " + dataset.getProcedureId() + " key " + key + " size " + rows.size());
            filteredSets.put(key, rows);
        }
        return rows;
    }

    /**
     * Returns procedure id by passed row.
     *
     * @param row ExportFormatRow
     * @return String
     */
    protected String getProcedureIdByRow(ExportFormatRow row) {
        return row.getDefinition().getProcedureId();
    }

    /**
     * Returns filtered rows. Caches filtered rows in appropriate filters.
     * Add filters that cannot be applied to passed rows to collection 'remainingFilters'.
     *
     * @param baseDataset Dataset
     * @param filters Collection<Filter>
     * @param filtersByFieldName Map<Dataset,String>
     * @param remainingFilters Set<Filter>
     * @return Set<ExportFormatRow>
     */
    private Set<ExportFormatRow> applyFilters(Dataset baseDataset,
                                              Collection<Filter> filters,
                                              Map<Dataset, String> filtersByFieldName,
                                              Set<Filter> remainingFilters) {
        List<Set<ExportFormatRow>> sets = new ArrayList();

        if (filtersByFieldName.isEmpty()) {
            Collection<ExportFormatRow> rows = m_rowsIdMap.get(baseDataset.getProcedureId());
            if (rows != null && !rows.isEmpty()) {
                sets.add(new HashSet<ExportFormatRow>(rows));
            }
        } else {
            Map<String, String> fieldNameBeanPathMap =
                    m_procIdFieldNamesBeanPathesMap.get(baseDataset.getProcedureId());
            for (Entry<CRDCDataHelper.Dataset, String> filter : filtersByFieldName.entrySet()) {
                String filteredFieldName = filter.getKey().getOidFieldName();
                String filteredFieldValue = filter.getValue();

                if (!StringUtils.isEmpty(filteredFieldName) && !StringUtils.isEmpty(filteredFieldValue)) {
                    if (fieldNameBeanPathMap.keySet().contains(filteredFieldName)) {
                        Set<ExportFormatRow> filterSet =
                                getBaseFilteredSet(baseDataset, filteredFieldName, filteredFieldValue);
                        if (filterSet != null) {
                            sets.add(filterSet);
                        }
                    }
                }
            }
        }

        for (Filter filter : filters) {
            Collection<SimpleFilter> datasetFilters = new ArrayList();
            for (SimpleFilter simpleFilter : filter.getSimpleFilters()) {
                if (filter.getDatasets().contains(baseDataset)) {
                    datasetFilters.add(simpleFilter);
                } else {
                    remainingFilters.add(simpleFilter);
                }
            }
            if (!datasetFilters.isEmpty()) {
                if (datasetFilters.size() == 1) {
                    Set<ExportFormatRow> set = datasetFilters.iterator().next().getFilteredSet(baseDataset);
                    if (set != null) {
                        sets.add(set);
                    }
                } else {
                    Set<ExportFormatRow> set = new HashSet();
                    for (SimpleFilter datasetFilter : datasetFilters) {
                        Set<ExportFormatRow> datasetSet = datasetFilter.getFilteredSet(baseDataset);
                        if (datasetSet != null) {
                            set.addAll(datasetSet);
                        }
                    }
                    sets.add(set);
                }
            }
        }

        // Sort sets by descending size
        Collections.sort(sets, new Comparator<Set>() {

            @Override
            public int compare(Set o1, Set o2) {
                return o1.size() - o2.size();
            }

        });

        Set<ExportFormatRow> finalSet = new HashSet();
        Iterator<Set<ExportFormatRow>> iterator = sets.iterator();
        if (iterator.hasNext()) {
            finalSet.addAll(iterator.next());
            while (iterator.hasNext()) {
                finalSet.retainAll(iterator.next());
            }
        }
        return finalSet;
    }

    /**
     * Returns datasets that needed to filter rows by passed filters.
     *
     * @param remainingFilters Collection<Filter>
     * @return Set<CRDCDataHelper.Dataset>
     */
    private Set<CRDCDataHelper.Dataset> getAdditionalDatasets(Collection<Filter> remainingFilters) {
        Set<CRDCDataHelper.Dataset> additionalDatasets = new HashSet<CRDCDataHelper.Dataset>();

        for (Filter filter : remainingFilters) {
            additionalDatasets.addAll(filter.getDatasets());
        }

        return additionalDatasets;
    }


    /**
     * Returns filtered by passed field name and value export format rows.
     *
     * @param dataset Dataset
     * @param name String
     * @param value String
     * @return Set<ExportFormatRow>
     */
    private Set<ExportFormatRow> getBaseFilteredSet(Dataset dataset, String name, String value) {
        String procedureId = dataset.getProcedureId();
        Map<String, Map<String, Set<ExportFormatRow>>> filteredSets = m_baseFilteredSets.get(procedureId);
        if (filteredSets == null) {
            filteredSets = new HashMap();
            m_baseFilteredSets.put(procedureId, filteredSets);
        }
        Map<String, Set<ExportFormatRow>> filteredValues = filteredSets.get(name);
        if (filteredValues == null) {
            filteredValues = new HashMap();
            filteredSets.put(name, filteredValues);
        }
        Set<ExportFormatRow> rows = filteredValues.get(value);
        if (rows == null) {
            rows = new HashSet();
            Map<String, String> fieldNameBeanPathMap = m_procIdFieldNamesBeanPathesMap.get(procedureId);
            String filteredFieldBeanPath = fieldNameBeanPathMap.get(name);
            if (!StringUtils.isEmpty(filteredFieldBeanPath)) {
                for (ExportFormatRow row : m_rowsIdMap.get(procedureId)) {
                    if (value.equals(row.getFieldValueByBeanPath(filteredFieldBeanPath))) {
                        rows.add(row);
                    }
                }
            }
            System.out.println("Creating base filtered set " + dataset.getProcedureId() + " name " + name + " value "
                    + value + " size " + rows.size());
            filteredValues.put(value, rows);
        }
        return rows;
    }

    /**
     * Returns joined base rows.
     *
     * @param baseDataset Dataset
     * @param baseRows List<ExportFormatRow>
     * @param additionalDatasetsRows ArrayList<KeyValuePair<Dataset,List<ExportFormatRow>>>
     * @return List<ExportFormatRow>
     */
    private List<ExportFormatRow> getJoinedBaseRows(CRDCDataHelper.Dataset baseDataset,
                                                    List<ExportFormatRow> baseRows,
                                                    ArrayList<KeyValuePair<Dataset, List<ExportFormatRow>>> additionalDatasetsRows) {
        // Base procedure id.
        String baseProcedureId = baseRows.iterator().next().getDefinition().getProcedureId();

        // Base map FieldName-BeanPath for base result.
        Map<String, String> baseFieldNamesBeanPathesMap = m_procIdFieldNamesBeanPathesMap.get(baseProcedureId);

        // Field name of field where is stored Oid of instance of main class of the initial export.
        String baseOidFieldName = baseDataset.getOidFieldName();

        // Field name of field where is stored Oid of instance of main class of the initial export.
        String baseOidBeanPath = baseFieldNamesBeanPathesMap.get(baseOidFieldName);

        // This map stores filters for base rows. Key is bean bath of field and value is collection
        // of filtered value.
        Map<CRDCDataHelper.Dataset, Map<String, Set<String>>> filteredFieldsDataSets = new HashMap();

        for (KeyValuePair<Dataset, List<ExportFormatRow>> pair : additionalDatasetsRows) {
            CRDCDataHelper.Dataset currentDataset = pair.getKey();
            List<ExportFormatRow> curAdditionalRows = pair.getValue();
            String curProcedureId = currentDataset.getProcedureId();
            Map<String, String> curFieldNamesBeanPathesMap = m_procIdFieldNamesBeanPathesMap.get(curProcedureId);
            String curBaseOidFieldBeanPath = curFieldNamesBeanPathesMap.get(baseOidFieldName);
            String curOidFieldName = currentDataset.getOidFieldName();
            String curOidBeanPath = curFieldNamesBeanPathesMap.get(curOidFieldName);

            Map<String, Set<String>> filteredFieldsValues = filteredFieldsDataSets.get(currentDataset);
            if (filteredFieldsValues == null) {
                filteredFieldsValues = new HashMap();
                filteredFieldsDataSets.put(currentDataset, filteredFieldsValues);
            }

            // Try to join by oid field name of dataset of additional row.
            if (baseFieldNamesBeanPathesMap.keySet().contains(curOidFieldName)) {
                String baseRelOidBeanPath = baseFieldNamesBeanPathesMap.get(curOidFieldName);

                // Get collection of values-filters for related field or, if it's null, create one.
                Set<String> baseRelOidFilteredValues = filteredFieldsValues.get(baseRelOidBeanPath);
                if (baseRelOidFilteredValues == null) {
                    baseRelOidFilteredValues = new HashSet<String>();
                    filteredFieldsValues.put(baseRelOidBeanPath, baseRelOidFilteredValues);
                }

                for (ExportFormatRow row : curAdditionalRows) {
                    baseRelOidFilteredValues.add((String) row.getFieldValueByBeanPath(curOidBeanPath));
                }
            }

            // Otherwise try to join by oid field name of the base dataset.
            else if (curFieldNamesBeanPathesMap.keySet().contains(baseOidFieldName)) {
                // Get collection of base field oid filtered values or, if it's null, create one.
                Set<String> baseOidFilteredValues = filteredFieldsValues.get(baseOidBeanPath);
                if (baseOidFilteredValues == null) {
                    baseOidFilteredValues = new HashSet<String>();
                    filteredFieldsValues.put(baseOidBeanPath, baseOidFilteredValues);
                }

                // Add filtered values for base field oid from each additional row of the dataset.
                for (ExportFormatRow row : curAdditionalRows) {
                    baseOidFilteredValues.add((String) row.getFieldValueByBeanPath(curBaseOidFieldBeanPath));
                }
            }

            // Otherwise try to find relation to join datasets.
            else {
                // Find dataset-relation.
                CRDCDataHelper.Dataset datasetRelation = null;

                for (CRDCDataHelper.Dataset dataset : Dataset.values()) {
                    if (dataset.getOidFieldName().contains(currentDataset.getOidFieldName()) &&
                            dataset.getOidFieldName().contains(baseDataset.getOidFieldName())) {
                        datasetRelation = dataset;
                        break;
                    }
                }

                if (datasetRelation != null) {
                    // Get needed relation result.
                    Map<String, String> relFieldNamesBeanPathesMap =
                            m_procIdFieldNamesBeanPathesMap.get(datasetRelation.getProcedureId());

                    // Determine bean path of current and base oid fields in result-relation.
                    String relCurOidBeanPath = relFieldNamesBeanPathesMap.get(currentDataset.getOidFieldName());
                    String relBaseOidBeanPath = relFieldNamesBeanPathesMap.get(baseDataset.getOidFieldName());

                    // Get collection of base field oid filtered values or, if it's null, create
                    // one.
                    Set<String> baseOidFilteredValues = filteredFieldsValues.get(baseOidBeanPath);
                    if (baseOidFilteredValues == null) {
                        baseOidFilteredValues = new HashSet<String>();
                        filteredFieldsValues.put(baseOidBeanPath, baseOidFilteredValues);
                    }

                    // Create filter for relation result by already filtered additional rows.
                    for (ExportFormatRow curAdditionalRow : curAdditionalRows) {
                        String curDatasetOidValue = (String) curAdditionalRow.getFieldValueByBeanPath(curOidBeanPath);
                        Set<String> values = getRelationshipValues(datasetRelation.getProcedureId(),
                                relCurOidBeanPath, relBaseOidBeanPath, curDatasetOidValue);
                        if (values != null && !values.isEmpty()) {
                            baseOidFilteredValues.addAll(values);
                        }
                    }
                }
            }
        }

        List<ExportFormatRow> leftJoinedBaseRows = new ArrayList<ExportFormatRow>();

        for (ExportFormatRow baseRow : baseRows) {
            boolean passedFilter = true;
            for (Entry<Dataset, Map<String, Set<String>>> entry : filteredFieldsDataSets.entrySet()) {
                CRDCDataHelper.Dataset dataset = entry.getKey();
                for (Entry<String, Set<String>> filter : entry.getValue().entrySet()) {
                    String filteredBeanPath = filter.getKey();
                    Collection<String> filteredValues = filter.getValue();

                    if (!dataset.isReverseJoin()
                            && !filteredValues.contains(baseRow.getFieldValueByBeanPath(filteredBeanPath))) {
                        passedFilter = false;
                        break;
                    } else if (dataset.isReverseJoin()
                            && filteredValues.contains(baseRow.getFieldValueByBeanPath(filteredBeanPath))) {
                        passedFilter = false;
                        break;
                    }
                }
            }

            if (passedFilter) {
                leftJoinedBaseRows.add(baseRow);
            }
        }

        return leftJoinedBaseRows;
    }

    /**
     * Parse passed calc param and return founded filters.
     *
     * @param filterParam String
     * @return Collection<Filter>
     */
    private Collection<Filter> getParamFilter(String filterParam) {
        Collection<Filter> filters = new ArrayList<Filter>();

        Matcher matcher = m_paramPattern.matcher(filterParam);

        ArrayList<SimpleFilter> partsOfComplexFilter = new ArrayList<SimpleFilter>();
        while (matcher.find()) {
            String paramName = matcher.group(GROUP_PARAM_NAME); // 1
            String paramValues = matcher.group(GROUP_PARAM_VALUE); // 3
            String sign = matcher.group(GROUP_PARAM_SIGN); // 2
            String delimiter = matcher.group(GROUP_PARAM_DELIMITER); // 6

            if (!StringUtils.isEmpty(paramName) &&
                    (!StringUtils.isEmpty(paramValues))) {
                paramValues = paramValues.replaceAll("\\{|\\}", DELIMITER_LAST_PARAM);

                List<String> filteredValues = Arrays.asList(paramValues.split(DELIMITER_AND));

                CRDCDataHelper.CalcParameter calcParam = CalcParameter.findCalcParamByName(paramName);

                if (delimiter.equals(DELIMITER_AND) || delimiter.equals(DELIMITER_LAST_PARAM)) {
                    if (!partsOfComplexFilter.isEmpty()) {
                        SimpleFilter filter = new SimpleFilter(calcParam, sign, filteredValues);
                        partsOfComplexFilter.add(filter);
                        ComplexFilterAlt complexFilter = new ComplexFilterAlt(partsOfComplexFilter);
                        filters.add(complexFilter);

                        partsOfComplexFilter = new ArrayList<SimpleFilter>();
                    } else {
                        SimpleFilter filter = new SimpleFilter(calcParam, sign, filteredValues);
                        filters.add(filter);
                    }
                } else if (delimiter.equals(DELIMITER_OR)) {
                    SimpleFilter filter = new SimpleFilter(calcParam, sign, filteredValues);
                    partsOfComplexFilter.add(filter);
                }
            }
        }

        return filters;
    }

    /**
     * Return result rows by procedure id.
     *
     * @param procedureId String
     * @return List<ExportFormatRow>
     */
    private List<ExportFormatRow> getRowsByProcedureId(String procedureId) {
        return m_rowsIdMap.get(procedureId);
    }

    /**
     * Gets the set of relationship values with caching.
     *
     * @param procedureId String
     * @param keyBeanPath String
     * @param valueBeanPath String
     * @param lookupValue String
     * @return List
     */
    private Set<String> getRelationshipValues(String procedureId,
                                              String keyBeanPath,
                                              String valueBeanPath,
                                              String lookupValue) {
        Map<String, Set<String>> map = m_relationshipValuesMap.get(procedureId, keyBeanPath, valueBeanPath);
        if (map == null) {
            map = new HashMap();
            List<ExportFormatRow> rows = m_rowsIdMap.get(procedureId);
            if (rows != null && !rows.isEmpty()) {
                for (ExportFormatRow row : rows) {
                    String rowKey = (String) row.getFieldValueByBeanPath(keyBeanPath);
                    Set<String> values = map.get(rowKey);
                    if (values == null) {
                        values = new HashSet();
                        map.put(rowKey, values);
                    }
                    String value = (String) row.getFieldValueByBeanPath(valueBeanPath);
                    values.add(value);
                }
            }
            m_relationshipValuesMap.put(procedureId, keyBeanPath, valueBeanPath, map);
        }
        return map.get(lookupValue);
    }

    /**
     * Determine and create new instances of retrievers and validators based on
     * calc ids and validator ids of field definitions. So only used retrievers and validators
     * should be created.
     *
     * @param fieldInterfaceName String
     * @return Map<String, Object>
     * @throws X2BaseException exception
     */
    private Map<String, Object> getUsedInnerInstancesByInterface(String fieldInterfaceName) throws X2BaseException {
        Collection<String> usedIds = new ArrayList<String>();
        for (int i = 0; i < m_data.getFieldCount(); i++) {
            FieldDefinition currentField = m_data.getFieldDefinition(i);
            String usedId = null;

            if (fieldInterfaceName.equals(INTERFACE_NAME_FIELD_RETRIEVER)) {
                usedId = currentField.getCalcId();
            } else if (fieldInterfaceName.equals(INTERFACE_NAME_FIELD_VALIDATOR)) {
                usedId = currentField.getValidatorId();
            }


            if (!StringUtils.isEmpty(usedId)) {
                usedIds.add(usedId);
            }
        }

        Set<Class> classes = new HashSet();
        classes.addAll(Arrays.asList(m_data.getClass().getDeclaredClasses()));
        classes.addAll(Arrays.asList(m_data.getClass().getSuperclass().getDeclaredClasses()));
        Map<String, Object> usedCalcIdRetrieversMap = new HashMap<String, Object>();

        for (Class currentClass : classes) {
            String interfaceName = null;
            if (currentClass.getInterfaces().length > 0) {
                interfaceName = currentClass.getInterfaces()[0].getName();
            } else if (currentClass.getSuperclass() != null) {
                Class superClass = currentClass.getSuperclass();
                Class subClass = currentClass;

                while (superClass != null) {
                    if (superClass != null && superClass.getInterfaces().length > 0) {
                        interfaceName = superClass.getInterfaces()[0].getName();
                        break;
                    }

                    subClass = superClass;
                    superClass = subClass.getSuperclass();
                }
            }

            if (!StringUtils.isEmpty(interfaceName) && interfaceName.equals(fieldInterfaceName)) {
                String id = null;
                try {
                    id = currentClass.getName().replaceAll(currentClass.getEnclosingClass().getName() + REGEX_ANY_CHAR,
                            STRING_EMPTY);
                    if (usedIds.contains(id)) {
                        Constructor constructor = currentClass.getDeclaredConstructor(currentClass.getEnclosingClass());
                        usedCalcIdRetrieversMap.put(id, constructor.newInstance(m_data));
                    }
                } catch (InvocationTargetException | NoSuchMethodException | SecurityException
                        | IllegalArgumentException | IllegalAccessException | InstantiationException e) {
                    StringWriter sw = new StringWriter();
                    PrintWriter pw = new PrintWriter(sw);
                    e.getCause().printStackTrace(pw);
                    // remove xml-like tags before passing to Jasper
                    throw new ToolRunException(
                            "Retriever: " + id + "\n" + sw.toString().replace("<", "{").replace(">", "}"));
                }
            }
        }

        return usedCalcIdRetrieversMap;
    }

    /**
     * Initialize map where key is procedure id and value is map with pairs
     * ["export field name" : "java name"].
     *
     * NOTE: Need to validate Export Format Definitions before creating and using this map.
     */
    private void initFieldJavaNameMapping() {
        m_procIdFieldNamesBeanPathesMap = new HashMap<String, Map<String, String>>();

        for (ExportFormatResult result : m_resultsIdMap.values()) {
            ExportFormatDefinition definition = result.getDefinition();
            Collection<ExportFormatField> fields = definition.getFields();

            for (ExportFormatField field : fields) {
                Map<String, String> fieldNameBeanPathMap =
                        m_procIdFieldNamesBeanPathesMap.get(definition.getProcedureId());
                if (fieldNameBeanPathMap == null) {
                    fieldNameBeanPathMap = new HashMap<String, String>();
                    m_procIdFieldNamesBeanPathesMap.put(definition.getProcedureId(), fieldNameBeanPathMap);
                }
                if (field.getDataFieldConfig() == null) {
                    m_data.addSetupError("Export Format Definition " + definition.getName(),
                            "Need to validate Export Format Definitions before creating and "
                                    + "using CRDC Initial Export Results.");
                    break;
                }
                try {
                    fieldNameBeanPathMap.put(field.getName(), field.getDataFieldConfig().getDataField().getJavaName());
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Initialize maps with selected results.
     *
     * @param broker X2Broker
     * @param crdcResultsCriteria X2Criteria
     */
    private void initResults(X2Broker broker, X2Criteria crdcResultsCriteria) {
        QueryByCriteria crdcResultsQuery = new QueryByCriteria(ExportFormatResult.class, crdcResultsCriteria);
        m_resultsIdMap = broker.getMapByQuery(crdcResultsQuery, ExportFormatResult.REL_DEFINITION +
                ModelProperty.PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID, 4);
    }

    /**
     * Initialize map where key is procedure id and value is collection of result rows.
     */
    private void initRows() {
        for (Entry<String, ExportFormatResult> entry : m_resultsIdMap.entrySet()) {
            m_rowsIdMap.put(entry.getKey(), new ArrayList(entry.getValue().getRows()));
        }
    }
}
