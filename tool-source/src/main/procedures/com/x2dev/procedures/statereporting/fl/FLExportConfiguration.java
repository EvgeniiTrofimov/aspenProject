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
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLExportConfiguration.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLExportConfiguration {
    public interface FLExport {
        /**
         * Gets the code.
         *
         * @return the code
         */
        public String getCode();

        /**
         * Gets the export id.
         *
         * @return the export id
         */
        public String getExportId();

        /**
         * Gets the file no.
         *
         * @return the file no
         */
        public String getFileNo();

        /**
         * Gets the procedure id.
         *
         * @return the procedure id
         */
        public String getProcedureId();

        /**
         * Gets the relationship key by class name.
         *
         * @param className the class name
         * @return the relationship key by class name
         */
        public FLRelationshipKey getRelationshipKeyByClassName(String className);

        /**
         * Gets the validation rules.
         *
         * @return the validation rules
         */
        public List<FLValidationRule> getValidationRules();

        public String name();
    }

    public interface FLSurvey {
        /**
         * Gets the code.
         *
         * @return the code
         */
        public String getCode();

        /**
         * Gets the exports.
         *
         * @return the exports
         */
        public List<FLExport> getExports();
    }

    /**
     * The Enum FL_EXPORT.
     */
    public enum FL_EXPORT implements FLExport {
        // Student Assessment
        ASM("ASM", "F61096", "EXPDATA-FL-ASM", "EXP-FL-ASM",
                new FLStudentAssessmentValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Industry Certification
        CAPEIC("CAPEIC", "F61096", "EXPDATA-FL-CAPEIC", "EXP-FL-CAPEIC",
                new FLIndustryCertificationValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Compensatory Project Evaluation
        CPE("CPE", "F61096", "EXPDATA-FL-CPE", "EXP-FL-CPE",
                new FLCompensatoryProjectEvaluationValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // CTE Student Course Schedule
        CTESSC("CTESSC", "F61096", "EXPDATA-FL-CTESSC", "EXP-FL-CTESSC",
                new FLCTEStudentCourseScheduleValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local",
                                SisStudent.class, "school.organization1")
                })),
        // CTE Teacher Course
        CTETC("CTETC", "F61096", "EXPDATA-FL-CTETC", "EXP-FL-CTETC",
                new FLCTETeacherCourseValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number CIS"),
                        new FLRelationshipKey(SisStaff.class, "localId", "Staff ID local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Staff ID local",
                                SisStaff.class, "school.organization1")
                })),
        // Dropout Prevention Program
        DRP("DRP", "F61096", "EXPDATA-FL-DRP", "EXP-FL-DRP",
                new FLDropoutPreventionProgramValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // English Language Learners Information
        ELL("ELL", "F61096", "EXPDATA-FL-ELL", "EXP-FL-ELL",
                new FLEnglishLanguageLearnersValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local",
                                SisStudent.class, "school.organization1")
                })),
        // Prior Student Status
        ENR("ENR", "F61096", "EXPDATA-FL-ENR", "EXP-FL-ENR",
                new FLPriorSchoolStatusValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number, E"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Exceptional Student
        EXCEPT("EXCEPT", "F61096", "EXPDATA-FL-EXCEPT", "EXP-FL-EXCEPT",
                new FLExceptionalStudentValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Federal Indicator Status
        FED("FED", "F61096", "EXPDATA-FL-FED", "EXP-FL-FED",
                new FLIndicatorStatusValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Student Additional Funding
        FTE("FTE", "F61096", "EXPDATA-FL-FTE", "EXP-FL-FTE",
                new FLStudentAdditionalFundingValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number IS"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Teacher Course
        MTC("MTC", "F60777", "EXPDATA-FL-MTC", "EXP-FL-MTC",
                new FLTeacherCourseValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStaff.class, "localId", "Staff ID Local")
                })),
        // Staff Additional Job Assignment
        SAJA("SAJA", "F61096", "EXPDATA-FL-SAJA", "EXP-FL-SAJA",
                new FLStaffAdditionalJobAssignmentsValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStaff.class, "localId", "Staff ID Local")
                })),
        // Student Course Transcript Information
        SCTI("SCTI", "F61096", "EXPDATA-FL-SCTI", "EXP-FL-SCTI",
                new FLStudentCourseTranscriptInfValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number CE"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Student Discipline Resultant Action
        SDRA("SDRA", "F61096", "EXPDATA-FL-SDRA", "EXP-FL-SDRA",
                new FLDisciplineResultantValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // School Environmental Safety Incident Report
        SESIR("SESIR", "F61096", "EXPDATA-FL-SESIR", "EXP-FL-SESIR",
                new FLSchoolEnvSafetyIncidentValidation().getValidationRules(),
                new ArrayList()),
        // Student End of Year Status
        SEYS("SEYS", "F61096", "EXPDATA-FL-SEYS", "EXP-FL-SEYS",
                new FLStudentEndOfYearStatusValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Staff Professional Development
        SPD("SPD", "F61096", "EXPDATA-FL-SPD", "EXP-FL-SPD",
                new FLStaffProfessionalDevelopmentValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStaff.class, "localId", "Staff ID Local"),
                })),
        // Student Course Schedule
        SSC("SSC", "F60776", "EXPDATA-FL-SSC", "EXP-FL-SSC",
                new FLStudentCourseScheduleValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Student Demographic Information
        STD("STD", "F60775", "EXPDATA-FL-STD", "EXP-FL-STD",
                new FLStudentExtractValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                })),
        // Staff Demographic Information
        STF("STF", "F61025", "EXPDATA-FL-STF", "EXP-FL-STF",
                new FLStaffExtractValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
                        new FLRelationshipKey(SisStaff.class, "localId", "Staff ID Local")})),
        // Staff Experience
        SXP("SXP", "F61026", "EXPDATA-FL-SXP", "EXP-FL-SXP",
                new FLStaffExperienceValidation().getValidationRules(),
                new ArrayList()),
        // Title I Supplemental Education
        // REPORTING NOT REQUIRED UNTILL FURTHER NOTIFICATION
        // TITLEI("TITLEI", "F60776", "EXPDATA-FL-TITLEI", "EXP-FL-TITLEI",
        // new FLSupplementalEdValidation().getValidationRules(),
        // Arrays.asList(new FLRelationshipKey[] {
        // new FLRelationshipKey(SisSchool.class, "[all-skl-StateId]", "School Number"),
        // new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
        // new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local",
        // SisStudent.class,
        // "school.organization1")
        // })),
        // Student Transportation
        TRN("TRN", "F70496", "EXPDATA-FL-TRN", "EXP-FL-TRN",
                new FLStudentTransportationValidation().getValidationRules(),
                Arrays.asList(new FLRelationshipKey[] {
                        new FLRelationshipKey(SisStudent.class, "localId", "Student ID Local"),
                        new FLRelationshipKey(SisOrganization.class, "localId", "Student ID Local", SisStudent.class,
                                "school.organization1")
                }));

        private String m_code;
        private String m_exportId;
        private String m_fileNo;
        private HashMap<String, FLRelationshipKey> m_mapRelationshipByClassName;
        private String m_procedureId;
        private List<FLRelationshipKey> m_relationships;

        private List<FLValidationRule> m_rules;

        /**
         * Instantiates a new fl export.
         *
         * @param code the code
         * @param fileNo the file no
         * @param procedureId the procedure id
         * @param exportId the export id
         * @param rules the rules
         * @param relationships the relationships
         */
        private FL_EXPORT(String code, String fileNo, String procedureId, String exportId,
                List<FLValidationRule> rules, List<FLRelationshipKey> relationships) {
            m_code = code;
            m_fileNo = fileNo;
            m_procedureId = procedureId;
            m_exportId = exportId;
            m_rules = rules;
            m_relationships = relationships;

        }

        /**
         * Gets the code.
         *
         * @return the code
         */
        @Override
        public String getCode() {
            return m_code;
        }

        /**
         * Gets the export id.
         *
         * @return the export id
         */
        @Override
        public String getExportId() {
            return m_exportId;
        }

        /**
         * Gets the file no.
         *
         * @return the file no
         */
        @Override
        public String getFileNo() {
            return m_fileNo;
        }

        /**
         * Gets the procedure id.
         *
         * @return the procedure id
         */
        @Override
        public String getProcedureId() {
            return m_procedureId;
        }

        /**
         * Gets the relationship key by class name.
         *
         * @param className the class name
         * @return the relationship key by class name
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
         * Gets the validation rules.
         *
         * @return the validation rules
         */
        @Override
        public List<FLValidationRule> getValidationRules() {
            return m_rules;
        }

        /**
         * To string.
         *
         * @return the string
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return "FL_EXPORT " + getProcedureId();
        }
    }


    /**
     * The Enum FL_SURVEY.
     */
    public enum FL_SURVEY implements FLSurvey {
        //
        SURVEY_1("1", Arrays.asList(new FLExport[] {
                FL_EXPORT.STD, FL_EXPORT.SSC, FL_EXPORT.MTC,
                FL_EXPORT.EXCEPT, FL_EXPORT.TRN
        })),
        //
        SURVEY_2("2", Arrays.asList(new FLExport[] {
                FL_EXPORT.STD, FL_EXPORT.SSC, FL_EXPORT.MTC,
                FL_EXPORT.EXCEPT, FL_EXPORT.FED, FL_EXPORT.SDRA,
                FL_EXPORT.ELL, FL_EXPORT.SESIR, FL_EXPORT.ENR, FL_EXPORT.TRN,
                FL_EXPORT.STF, FL_EXPORT.SAJA, FL_EXPORT.SXP
        })),
        //
        SURVEY_3("3", Arrays.asList(new FLExport[] {
                FL_EXPORT.STD, FL_EXPORT.SSC, FL_EXPORT.MTC,
                FL_EXPORT.EXCEPT, FL_EXPORT.FED, FL_EXPORT.SDRA,
                FL_EXPORT.ELL, FL_EXPORT.SESIR, FL_EXPORT.ENR, FL_EXPORT.TRN,
                FL_EXPORT.STF, FL_EXPORT.SAJA, FL_EXPORT.SXP,
        })),
        //
        SURVEY_4("4", Arrays.asList(new FLExport[] {
                FL_EXPORT.STD, FL_EXPORT.SSC, FL_EXPORT.MTC,
                FL_EXPORT.EXCEPT, FL_EXPORT.TRN})),
        //
        SURVEY_5("5", Arrays.asList(new FLExport[] {
                FL_EXPORT.STD, FL_EXPORT.EXCEPT, FL_EXPORT.FED, FL_EXPORT.SEYS, FL_EXPORT.SDRA,
                FL_EXPORT.CTETC, FL_EXPORT.CTESSC, FL_EXPORT.CPE, FL_EXPORT.DRP,
                FL_EXPORT.ELL, FL_EXPORT.SCTI, FL_EXPORT.SESIR, FL_EXPORT.ENR,
                FL_EXPORT.STF, FL_EXPORT.ASM, FL_EXPORT.FTE, FL_EXPORT.CAPEIC
        })),
        //
        SURVEY_6("6", Arrays.asList(new FLExport[] {
                FL_EXPORT.STD
        })),
        //
        SURVEY_8("8", Arrays.asList(new FLExport[] {
                FL_EXPORT.STF, FL_EXPORT.STD
        })),
        //
        SURVEY_9("9", Arrays.asList(new FLExport[] {
                FL_EXPORT.STD
        }));

        private String m_code;
        private List<FLExport> m_exports;

        /**
         * Instantiates a new fl survey.
         *
         * @param code the code
         * @param exports the exports
         */
        private FL_SURVEY(String code, List<FLExport> exports) {
            m_code = code;
            m_exports = exports;
        }

        /**
         * Gets the code.
         *
         * @return the code
         */
        @Override
        public String getCode() {
            return m_code;
        }

        /**
         * Gets the exports.
         *
         * @return the exports
         */
        @Override
        public List<FLExport> getExports() {
            return m_exports;
        }
    }


    /**
     * The Class FLRelationshipKey.
     */
    public static class FLRelationshipKey {
        private String m_beanPath;
        private String m_fieldName;
        private Class m_relatingClass;
        private Class m_relationshipClass;
        private String m_relatingPath;

        /**
         * Instantiates a new FL relationship key.
         *
         * @param relationshipClass the relationship class
         * @param beanPath the bean path
         * @param fieldName the field name
         */
        public FLRelationshipKey(Class relationshipClass, String beanPath, String fieldName) {
            this.m_beanPath = beanPath;
            this.m_fieldName = fieldName;
            this.m_relationshipClass = relationshipClass;
        }

        /**
         * Instantiates a new FL relationship key.
         *
         * @param relationshipClass the relationship class
         * @param beanPath the bean path
         * @param fieldName the field name
         * @param relatingClass the relating class
         * @param relatingPath the relating path
         */
        public FLRelationshipKey(Class relationshipClass, String beanPath, String fieldName, Class relatingClass,
                String relatingPath) {
            m_beanPath = beanPath;
            m_fieldName = fieldName;
            m_relationshipClass = relationshipClass;
            m_relatingClass = relatingClass;
            m_relatingPath = relatingPath;
        }

        /**
         * Gets the bean path.
         *
         * @return the bean path
         */
        public String getBeanPath() {
            return m_beanPath;
        }

        /**
         * Gets the field name.
         *
         * @return the field name
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Gets the query class.
         *
         * @return the query class
         */
        public Class getQueryClass() {
            return m_relatingClass == null ? m_relationshipClass : m_relatingClass;
        }

        /**
         * Gets the relating class.
         *
         * @return the relating class
         */
        public Class getRelatingClass() {
            return m_relatingClass;
        }

        /**
         * Gets the relating path.
         *
         * @return the relating path
         */
        public String getRelatingPath() {
            return m_relatingPath;
        }

        /**
         * Gets the relationship class.
         *
         * @return the relationship class
         */
        public Class getRelationshipClass() {
            return m_relationshipClass;
        }

    }


    /**
     * The Class FLValidationRule.
     */
    public static class FLValidationRule {
        String m_code;
        FLValidationProcessor m_processor;
        String m_ruleNumber;

        /**
         * Instantiates a new FL validation rule.
         *
         * @param code the code
         * @param ruleNumber the rule number
         * @param processor the processor
         */
        public FLValidationRule(String code, String ruleNumber, FLValidationProcessor processor) {
            m_code = code;
            m_processor = processor;
            m_ruleNumber = ruleNumber;
        }

        /**
         * Gets the code.
         *
         * @return the code
         */
        public String getCode() {
            return m_code;
        }

        /**
         * Gets the processor.
         *
         * @return the processor
         */
        public FLValidationProcessor getProcessor() {
            return m_processor;
        }

        /**
         * Gets the rule number.
         *
         * @return the rule number
         */
        public String getRuleNumber() {
            return m_ruleNumber;
        }
    }


    /**
     * The Interface FLValidationProcessor.
     */
    public interface FLValidationProcessor {

        /**
         * Gets the validation errors.
         *
         * @param helper the helper
         * @param export the export
         * @param row the row
         * @return the validation errors
         */
        List<FLValidationError> getValidationErrors(FLExportConfiguration helper,
                                                    FLExport export,
                                                    ExportFormatRow row);
    }


    /**
     * The Class FLValidationError.
     */
    public static class FLValidationError {
        private String m_error;
        private FLExport m_export;
        private String m_fieldName;
        private String m_message;
        private ExportFormatRow m_row;
        private String m_ruleNumber;
        private String m_value;

        /**
         * Instantiates a new FL validation error.
         *
         * @param export the export
         * @param row the row
         * @param error the error
         * @param fieldName
         * @param value the value
         * @param message the message
         */
        public FLValidationError(FLExport export, ExportFormatRow row, String error, String fieldName, String value,
                String message) {
            m_error = error;
            m_export = export;
            m_message = message;
            m_value = value;
            m_row = row;
            m_fieldName = fieldName;
        }

        /**
         * Gets the error.
         *
         * @return the error
         */
        public String getError() {
            return m_error;
        }

        /**
         * Gets the export.
         *
         * @return the export
         */
        public FLExport getExport() {
            return m_export;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Gets the message.
         *
         * @return the message
         */
        public String getMessage() {
            return m_message;
        }

        /**
         * Gets the rule number.
         *
         * @return the rule number
         */
        public String getRuleNumber() {
            return m_ruleNumber;
        }

        /**
         * Gets the row.
         *
         * @return the row
         */
        public ExportFormatRow getRow() {
            return m_row;
        }

        /**
         * Gets the value.
         *
         * @return the value
         */
        public String getValue() {
            return m_value;
        }

        /**
         * Sets the rule number.
         *
         * @param ruleNumber the new rule number
         */
        public void setRuleNumber(String ruleNumber) {
            m_ruleNumber = ruleNumber;
        }
    }


    /**
     * The Class RuntimeParam.
     */
    public static class RuntimeParam {
        public static final String DATE_CERTAIN = "DateCertain";
        public static final String DATE_CERTAIN_OF_SURVEY_2 = "DateCertainSurvey2";
        public static final String DATE_CERTAIN_OF_SURVEY_3 = "DateCertainSurvey3";
        public static final String DATE_CERTAIN_PLUS_90 = "DateCertainPlus90";
        public static final String DATE_CERTAIN_PLUS_2_YEARS = "DateCertainPlus2Years";
        public static final String DATE_CERTAIN_MINUS_2_YEARS = "DateCertainMinus2Years";
        public static final String DATE_WITH_CURRENT_YEAR_PLUS_YEARS = "DateWithCurrentYearPlusYears";
        public static final String DAYS_IN_SURVEY = "DaysInSurvey";
        public static final String DISTRICT_NUMBER = "DistrictNumber";
        public static final String FISCAL_BEGIN_DATE = "FiscalBeginDate";
        public static final String FISCAL_DATE = "FiscalDate";
        public static final String FISCAL_DATE_MINUS_2_YEARS = "FiscalDateMinus2Years";
        public static final String FISCAL_END_DATE = "FiscalEndDate";
        public static final String FISCAL_YEAR = "FiscalYear";
        public static final String PERIOD_END_DATE = "PeriodEndDate";
        private static final String ALIAS_DISTRICT_NUMBER = "all-org-DistrictNumber";
        private Object[] m_params;
        private String m_type;

        /**
         * Instantiates a new runtime param.
         *
         * @param type the type
         * @param params the params
         */
        public RuntimeParam(String type, Object... params) {
            m_params = params;
            m_type = type;
        }

        /**
         * Gets the runtime object.
         *
         * @param helper the helper
         * @return the runtime object
         */
        public Object getRuntimeObject(FLExportConfiguration helper) {
            Object value = null;
            SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("MMddyyyy");

            switch (m_type) {
                case DATE_CERTAIN:
                    value = helper.getSurveyPeriod().getDateCertain();
                    break;
                case DATE_CERTAIN_OF_SURVEY_2:
                    value = helper.new SurveyPeriod("2").getDateCertain();
                    break;
                case DATE_CERTAIN_OF_SURVEY_3:
                    value = helper.new SurveyPeriod("3").getDateCertain();
                    break;
                case DATE_CERTAIN_PLUS_90:
                    Calendar calendar = Calendar.getInstance();
                    calendar.setTime(helper.getSurveyPeriod().getDateCertain());
                    calendar.add(Calendar.DAY_OF_MONTH, 90);
                    value = calendar.getTime();
                    break;
                case DATE_CERTAIN_PLUS_2_YEARS:
                    Calendar calendar2 = Calendar.getInstance();
                    calendar2.setTime(helper.getSurveyPeriod().getDateCertain());
                    calendar2.add(Calendar.YEAR, 2);
                    value = calendar2.getTime();
                    break;
                case DATE_CERTAIN_MINUS_2_YEARS:
                    Calendar calendar3 = Calendar.getInstance();
                    calendar3.setTime(helper.getSurveyPeriod().getDateCertain());
                    calendar3.add(Calendar.YEAR, -2);
                    value = calendar3.getTime();
                    break;
                case DATE_WITH_CURRENT_YEAR_PLUS_YEARS: {
                    int schoolYear = helper.getContext().getSchoolYear();
                    int plusYears = 0;
                    if (m_params.length > 1) {
                        plusYears += ((Integer) m_params[1]).intValue();
                    }
                    int year = schoolYear + plusYears;
                    Date date = (Date) m_params[0];
                    Calendar calendarCurPlus = Calendar.getInstance();
                    calendarCurPlus.setTime(date);
                    calendarCurPlus.set(Calendar.YEAR, year);
                    value = calendarCurPlus.getTime();
                }
                    break;
                case DAYS_IN_SURVEY:
                    Calendar calendar4 = Calendar.getInstance();
                    calendar4.setTime(helper.getSurveyPeriod().getStartDate());
                    long startTime = calendar4.getTimeInMillis();
                    calendar4.setTime(helper.getSurveyPeriod().getEndDate());
                    long endTime = calendar4.getTimeInMillis();
                    value = Double.valueOf(TimeUnit.DAYS.convert((endTime - startTime), TimeUnit.MILLISECONDS));
                    break;
                case DISTRICT_NUMBER:
                    value = helper.getContext().getOrganization1().getFieldValueByAlias(ALIAS_DISTRICT_NUMBER);
                    break;
                case FISCAL_DATE:
                    if (m_params.length > 1) {
                        try {
                            String firstHalfPattern = "^0[7-9]|1[0-2]$";
                            boolean beforeNewYear = m_params[0].toString().matches(firstHalfPattern);
                            value = DATE_FORMAT.parse(m_params[0].toString() + m_params[1].toString() + "20"
                                    + (beforeNewYear ? helper.getFiscalYear().substring(0, 2)
                                            : helper.getFiscalYear().substring(2)));
                        } catch (Exception e) {
                            value = null;
                        }
                    }
                    break;
                case FISCAL_DATE_MINUS_2_YEARS:
                    if (m_params.length > 1) {
                        try {
                            value = DATE_FORMAT.parse(m_params[0].toString() + m_params[1].toString()
                                    + "20" + helper.getFiscalYear().substring(2));

                            Calendar calendar5 = Calendar.getInstance();
                            calendar5.setTime((Date) value);
                            calendar5.add(Calendar.YEAR, -2);
                            value = calendar5.getTime();
                        } catch (Exception e) {
                            value = null;
                        }
                    }
                    break;
                case PERIOD_END_DATE:
                    value = helper.getSurveyPeriod().getEndDate();
                    break;
                case FISCAL_BEGIN_DATE:
                    value = helper.getContext().getStartDate();
                    break;
                case FISCAL_END_DATE:
                    value = helper.getContext().getEndDate();
                    break;
                case FISCAL_YEAR:
                    value = helper.getFiscalYear();
                    break;
                default:
                    break;
            }
            if (value == null) {
                StringBuilder message = new StringBuilder();
                message.append("RuntimeParam should always return a runtime object. ");
                message.append("type = ");
                message.append(m_type);
                message.append(" ");
                for (int i = 0; i < m_params.length; ++i) {
                    message.append("param ");
                    message.append(i);
                    message.append(" = ");
                    message.append(m_params[i]);
                    message.append(" ");
                }
                throw new IllegalStateException(message.toString());
            }
            return value;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            throw new IllegalStateException("RuntimeParam toString() method should never be used");
        }
    }


    /**
     * The Class ValidateFiscalYear.
     */
    public static class ValidateFiscalYear implements FLValidationProcessor {

        private static final String FIELD_MSG =
                "Fiscal Year must be correct for the submission specified by the district";
        private static final String FIELD_NAME = "Fiscal Year";

        /**
         * Gets the validation errors.
         *
         * @param helper FLExportConfiguration
         * @param export FL_EXPORT
         * @param row ExportFormatRow
         * @return List
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor#getValidationErrors(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public List<FLValidationError> getValidationErrors(FLExportConfiguration helper,
                                                           FLExport export,
                                                           ExportFormatRow row) {
            List<FLValidationError> errors = new LinkedList();
            String value = helper.getExportFormatRowFieldValue(row, export, FIELD_NAME);
            if (value != null) {
                if (!value.equals(helper.getFiscalYear())) {
                    errors.add(new FLValidationError(export, row, "Incorrect value", FIELD_NAME,
                            value, FIELD_MSG));
                }
            } else {
                errors.add(new FLValidationError(export, row, "Field not found", FIELD_NAME, "", FIELD_MSG));
            }
            return errors;
        }
    }


    /**
     * The Class ValidateRegularExpression.
     */
    public static class ValidateRegularExpression implements FLValidationProcessor {
        protected String m_fieldName;
        private String m_message;
        private Pattern m_pattern;

        /**
         * Instantiates a new validate regular expression.
         *
         * @param fieldName the field name
         * @param expression the expression
         * @param message the message
         */
        public ValidateRegularExpression(String fieldName, String expression, String message) {
            m_fieldName = fieldName;
            m_pattern = Pattern.compile(expression);
            m_message = message;
        }

        /**
         * Gets the validation errors.
         *
         * @param helper the helper
         * @param export the export
         * @param row the row
         * @return the validation errors
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor#getValidationErrors(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public List<FLValidationError> getValidationErrors(FLExportConfiguration helper,
                                                           FLExport export,
                                                           ExportFormatRow row) {
            List<FLValidationError> errors = new LinkedList();
            String value = helper.getExportFormatRowFieldValue(row, export, m_fieldName);
            if (value != null) {
                Matcher matcher = m_pattern.matcher(value);
                if (!matcher.find()) {
                    errors.add(new FLValidationError(export, row, "Incorrect value", m_fieldName,
                            value, m_message));
                }
            } else {
                errors.add(new FLValidationError(export, row, "Field not found", m_fieldName, "", m_message));
            }
            return errors;
        }

    }


    /**
     * The Class ValidateStudentNumber.
     */
    public static class ValidateStudentNumber implements FLValidationProcessor {
        protected String m_fieldName;
        private String m_message;


        /**
         * Instantiates a new validate student number.
         *
         * @param fieldName String
         * @param message String
         */
        public ValidateStudentNumber(String fieldName, String message) {
            m_fieldName = fieldName;
            m_message = message;
        }

        /**
         * Gets the validation errors.
         *
         * @param helper the helper
         * @param export the export
         * @param row the row
         * @return the validation errors
         * @see com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor#getValidationErrors(com.x2dev.procedures.statereporting.fl.FLExportConfiguration,
         *      com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT,
         *      com.follett.fsc.core.k12.beans.ExportFormatRow)
         */
        @Override
        public List<FLValidationError> getValidationErrors(FLExportConfiguration helper,
                                                           FLExport export,
                                                           ExportFormatRow row) {
            List<FLValidationError> errors = new LinkedList();
            String value = helper.getExportFormatRowFieldValue(row, export, m_fieldName);
            if (value != null) {
                boolean valid = true;

                Matcher matcher = Pattern.compile("^\\d{9}(\\d|X)$").matcher(value);
                if (!matcher.find()) {
                    valid = false;
                }

                if (valid) {
                    Matcher matcher1 = Pattern.compile("^\\d{10}$").matcher(value);
                    if (matcher1.find()) {
                        Matcher matcher2 =
                                Pattern.compile("^(0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]|7[8-9]).{8}$").matcher(value);
                        if (!matcher2.find()) {
                            valid = false;
                        }
                    }
                }

                if (valid) {
                    Matcher matcher1 = Pattern.compile("^\\d{9}X$").matcher(value);
                    if (matcher1.find()) {
                        Matcher matcher2 = Pattern.compile("^(?!000).{10}$").matcher(value);
                        if (!matcher2.find()) {
                            valid = false;
                        }
                    }
                }

                if (!valid) {
                    errors.add(new FLValidationError(export, row, "Incorrect value", m_fieldName,
                            value, m_message));
                }
            } else {
                errors.add(new FLValidationError(export, row, "Field not found", m_fieldName, "", m_message));
            }

            return errors;
        }

    }


    /**
     * The Class ValidateDistrictNumber.
     */
    public static class ValidateDistrictNumber extends ValidateRegularExpression {

        /**
         * Instantiates a new validate district number.
         *
         * @param fieldName String
         */
        public ValidateDistrictNumber(String fieldName) {
            super(fieldName, "^0[1-9]|[1-5][0-9]|6[0-8]|7[1-5]$",
                    "District Number must be numeric, in the range 01-68 or 71-75 and must be correct for the district submitting the data.");
        }
    }

    /**
     * The Class ValidateSSN.
     */
    public static class ValidateSSN extends ValidateRegularExpression {

        /**
         * Instantiates a new validate SSN object.
         */
        public ValidateSSN() {
            super("SSN", "^(?!.*(000000000|999999999))\\d{9}[ ]|CS\\d{7}[ ]$",
                    "Social Security Number (SSN) must be numeric and greater than zero, excluding the value 999999999, unless it is a Staff Number Identifier and the first two positions are 'CS' and the last seven positions are numeric");
        }
    }

    public static final String ALIAS_VALIDATION_ERR_ACT_OID = "err-act-oid";
    public static final String ALIAS_VALIDATION_ERR_CND_OID = "err-cnd-oid";
    public static final String ALIAS_VALIDATION_ERR_CRS_OID = "err-crs-oid";
    public static final String ALIAS_VALIDATION_ERR_DDX_OID = "err-ddx-oid";
    public static final String ALIAS_VALIDATION_ERR_EFW_OID = "err-efw-oid";
    public static final String ALIAS_VALIDATION_ERR_ERROR = "err-error";
    public static final String ALIAS_VALIDATION_ERR_EXPORT_CODE = "err-export-code";
    public static final String ALIAS_VALIDATION_ERR_FIELD_NAME = "err-field-name";
    public static final String ALIAS_VALIDATION_ERR_MESSAGE = "err-message";
    public static final String ALIAS_VALIDATION_ERR_MST_OID = "err-mst-oid";
    public static final String ALIAS_VALIDATION_ERR_ORG1_OID = "err-org1-oid";
    public static final String ALIAS_VALIDATION_ERR_RULE_NUMBER = "err-rule-number";
    public static final String ALIAS_VALIDATION_ERR_SKL_OID = "err-skl-oid";
    public static final String ALIAS_VALIDATION_ERR_STD_OID = "err-std-oid";
    public static final String ALIAS_VALIDATION_ERR_STF_OID = "err-stf-oid";
    public static final String ALIAS_VALIDATION_ERR_SURVEY = "err-survey";
    public static final String ALIAS_VALIDATION_ERR_VALUE = "err-value";
    public static final String ALIAS_VALIDATION_ERR_YEAR = "err-fiscal-year";

    /**
     * The left square bracket ([) prefix character for identifying aliases in bean paths.
     * An Alias must be enclosed in square brackets.
     * <br>
     * [<i>alias</i>]
     */
    private static final String ALIAS_PREFIX_CHAR = "[";

    /**
     * The right square bracket (]) postfix character for identifying aliases in bean paths.
     * An Alias must be enclosed in square brackets.
     * <br>
     * [<i>alias</i>]
     */
    private static final String ALIAS_SUFFIX_CHAR = "]";

    private static final String DDX_VALIDATION_ERROR_ID = "FL-UDA-VAL_ERROR";
    private static final String VALIDATION_ERROR_TABLE_NAME = "Validation Error";

    private X2Broker m_broker = null;
    private DistrictSchoolYearContext m_currentContext = null;
    private ExtendedDataDictionary m_ddxValidationError = null;
    private String m_fiscalYear = null;
    private Map<String, DataDictionary> m_mapDataDictionary = new HashMap();
    private Map<String, ExportFormatDefinition> m_mapExportFormatDefinition = new HashMap();
    private Map<String, Map<String, ExportFormatField>> m_mapExportFormatFieldByName = new HashMap();
    private Map<String, ExportFormatResult> m_mapExportFormatResult = new HashMap();
    private Map<String, Collection<ExportFormatRow>> m_mapExportFormatRows = new HashMap();
    private Map<String, Map<LookupField, Map<LookupField, Collection<ExportFormatRow>>>> m_mapExportFormatRowsByKeyValue =
            new HashMap();
    private Map<String, Map<String, FieldDefinition>> m_mapFieldDefinitionByName = new HashMap();
    private Map<Object, Set> m_mapUniqueSets = new HashMap();
    private String m_surveyCode;
    private SurveyPeriod m_surveyPeriod;
    private ExtendedDataTable m_tbxValidationError = null;
    private DataDictionary m_validationErrorDictionary;

    /**
     * The Class LookupField.
     */
    public class LookupField {
        private List<String> m_fieldNames = new ArrayList();

        /**
         * Adds the.
         *
         * @param s the s
         */
        public void add(String s) {
            m_fieldNames.add(s);
        }

        /**
         * Equals.
         *
         * @param obj the obj
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            LookupField other = (LookupField) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_fieldNames == null) {
                if (other.m_fieldNames != null) {
                    return false;
                }
            } else {
                Iterator<String> iter1 = m_fieldNames.iterator();
                Iterator<String> iter2 = other.m_fieldNames.iterator();
                while (iter1.hasNext()) {
                    String str1 = iter1.next();
                    if (!iter2.hasNext()) {
                        return false;
                    }
                    String str2 = iter2.next();
                    if (!str1.equals(str2)) {
                        return false;
                    }
                }
                if (iter2.hasNext()) {
                    return false;
                }
            }
            return true;
        }

        /**
         * Gets the field names.
         *
         * @return the field names
         */
        public Collection<String> getFieldNames() {
            return m_fieldNames;
        }

        /**
         * Hash code.
         *
         * @return the int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            for (String s : m_fieldNames) {
                result = prime * result + s.hashCode();
            }
            return result;
        }

        /**
         * To string.
         *
         * @return the string
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder value = new StringBuilder();
            value.append("[");
            for (String s : m_fieldNames) {
                if (value.length() > 1) {
                    value.append(",");
                }
                value.append(s);
            }
            value.append("]");
            return value.toString();
        }

        /**
         * Gets the outer type.
         *
         * @return the outer type
         */
        private FLExportConfiguration getOuterType() {
            return FLExportConfiguration.this;
        }
    }

    /**
     * Instantiates a new FL export configuration.
     *
     * @param currentContext the current context
     * @param surveyCode the survey code
     * @param broker the broker
     */
    public FLExportConfiguration(DistrictSchoolYearContext currentContext, X2Broker broker) {
        m_currentContext = currentContext;
        m_broker = broker;
        m_fiscalYear = FLStateReportData.getFiscalYear(Calendar.getInstance(), currentContext);
    }

    /**
     * Instantiates a new FL export configuration.
     *
     * @param currentContext the current context
     * @param surveyCode the survey code
     * @param broker the broker
     */
    public FLExportConfiguration(DistrictSchoolYearContext currentContext, String surveyCode, X2Broker broker) {
        m_currentContext = currentContext;
        m_broker = broker;
        m_surveyCode = surveyCode;
        m_surveyPeriod = new SurveyPeriod(surveyCode);
        m_fiscalYear = FLStateReportData.getFiscalYear(Calendar.getInstance(), currentContext);
    }

    /**
     * Delete validation errors.
     */
    public void deleteValidationErrors() {
        ExtendedDataTable tbx = getValidationErrorExtendedDataTable();
        DataDictionary dictionary = getValidationErrorDictionary();
        DataDictionaryField fieldDdx = dictionary.findDataDictionaryFieldByAlias(ALIAS_VALIDATION_ERR_DDX_OID);
        DataDictionaryField fieldYear = dictionary.findDataDictionaryFieldByAlias(ALIAS_VALIDATION_ERR_YEAR);
        DataDictionaryField fieldSurvey = dictionary.findDataDictionaryFieldByAlias(ALIAS_VALIDATION_ERR_SURVEY);
        if (fieldDdx == null || fieldYear == null || fieldSurvey == null) {
            throw new IllegalStateException(
                    "The validation error table with key name aliases [" + ALIAS_VALIDATION_ERR_DDX_OID + ", "
                            + ALIAS_VALIDATION_ERR_YEAR + ", " + ALIAS_VALIDATION_ERR_SURVEY + "]  could not be found");
        }

        ExtendedDataDictionary ddx = getValidationErrorExtendedDataDictionary();

        X2Criteria deleteCriteria = new X2Criteria();
        deleteCriteria.addEqualTo(fieldDdx.getJavaName(), ddx.getOid());
        deleteCriteria.addEqualTo(fieldYear.getJavaName(), m_fiscalYear);
        deleteCriteria.addEqualTo(fieldSurvey.getJavaName(), m_surveyPeriod.getCode());
        QueryByCriteria deleteQuery =
                new QueryByCriteria(tbx.getDataTableConfig().getDataTable().getDataClass(), deleteCriteria);
        getBroker().deleteByQuery(deleteQuery);

    }

    /**
     * Gets the broker.
     *
     * @return the broker
     */
    public X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Gets the context.
     *
     * @return current district context
     */
    public DistrictSchoolYearContext getContext() {
        return m_currentContext;
    }

    /**
     * Gets the export format definition.
     *
     * @param export the export
     * @return the export format definition
     */
    public ExportFormatDefinition getExportFormatDefinition(FLExport export) {
        ExportFormatDefinition value = null;
        if (!m_mapExportFormatDefinition.containsKey(export.getCode())) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, export.getProcedureId());
            BeanQuery query = new BeanQuery(ExportFormatDefinition.class, criteria);
            value = (ExportFormatDefinition) m_broker.getBeanByQuery(query);
            if (value == null) {
                throw new IllegalStateException("An export format definition is required for " + export);
            }
            m_mapExportFormatDefinition.put(export.getCode(), value);
        }
        return m_mapExportFormatDefinition.get(export.getCode());
    }

    /**
     * Gets the export format row field value.
     *
     * @param row the row
     * @param export the export
     * @param fieldName the field name
     * @return the export format row field value
     */
    public String getExportFormatRowFieldValue(ExportFormatRow row, FLExport export, String fieldName) {
        ExportFormatField effField = getExportFormatFieldByName(export, fieldName);
        if (effField == null) {
            throw new IllegalStateException(
                    "An export format field is required for " + export + " with name " + fieldName);
        }

        DataFieldConfig fieldConfig = effField.getDataFieldConfig();
        if (fieldConfig == null) {
            throw new IllegalStateException(
                    "A data field must be configured for " + export + " field with name " + fieldName);
        }

        FieldDefinition field = getFieldDefinition(export, fieldName);
        if (field == null) {
            throw new IllegalStateException(
                    "A field definition must be found for " + export + " field with name " + fieldName);
        }

        String fieldValue = (String) row.getFieldValueByBeanPath(fieldConfig.getDataField().getJavaName());
        fieldValue = ExportFormatManager.doPadding(fieldValue,
                (field.getResizeMode() == null
                        ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                        : field.getResizeMode().ordinal()),
                field.getPaddingChar(),
                field.getExportLength());
        return fieldValue;
    }

    /**
     * Gets the export format row field value.
     *
     * @param row the row
     * @param export the export
     * @param relKey
     * @return the export format row field value
     */
    public String getExportFormatRowFieldValue(ExportFormatRow row, FLExport export, FLRelationshipKey relKey) {
        return getExportFormatRowFieldValue(row, export, relKey.getFieldName());
    }

    /**
     * Gets the export format rows.
     *
     * @param export the export
     * @return the export format rows
     */
    public Collection<ExportFormatRow> getExportFormatRows(FLExport export) {
        Collection<ExportFormatRow> values = null;
        if (!m_mapExportFormatRows.containsKey(export.getCode())) {
            ExportFormatResult result = getExportFormatResult(export);
            if (result != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, result.getOid());
                BeanQuery query = new BeanQuery(ExportFormatRow.class, criteria);
                query.addOrderBy(ExportFormatRow.COL_SORT_ORDER, true);
                values = m_broker.getCollectionByQuery(query);
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
     * @param export the export
     * @param key the key
     * @param value the value
     * @return the export format rows
     */
    public Collection<ExportFormatRow> getExportFormatRows(FLExport export,
                                                           LookupField key,
                                                           LookupField value) {
        Map<LookupField, Map<LookupField, Collection<ExportFormatRow>>> keyValueMap =
                m_mapExportFormatRowsByKeyValue.get(export.getCode());
        if (keyValueMap == null) {
            keyValueMap = new HashMap();
            m_mapExportFormatRowsByKeyValue.put(export.getCode(), keyValueMap);
        }
        Map<LookupField, Collection<ExportFormatRow>> valueMap = keyValueMap.get(key);
        if (valueMap == null) {
            valueMap = new HashMap();
            keyValueMap.put(key, valueMap);
            for (ExportFormatRow row : getExportFormatRows(export)) {
                LookupField rowValue = new LookupField();
                for (String fieldName : key.getFieldNames()) {
                    ExportFormatField field = getExportFormatFieldByName(export, fieldName);
                    if (field == null) {
                        throw new IllegalStateException(
                                "An export format field is required for " + export + " with name " + fieldName);
                    }
                    DataFieldConfig fieldConfig = field.getDataFieldConfig();
                    if (fieldConfig == null) {
                        throw new IllegalStateException(
                                "A data field must be configured for " + export + " field with name " + fieldName);
                    }
                    String fieldValue = getExportFormatRowFieldValue(row, export, fieldName);
                    rowValue.add(fieldValue);
                }
                Collection<ExportFormatRow> rows = valueMap.get(rowValue);
                if (rows == null) {
                    rows = new LinkedList();
                    valueMap.put(rowValue, rows);
                }
                rows.add(row);
            }
        }
        Collection<ExportFormatRow> values = valueMap.get(value);
        return values;
    }

    /**
     * Gets the export from procedure id.
     *
     * @param procedureId
     *
     * @return the export from procedure id
     */
    public FLExport getExportFromProcedureId(String procedureId) {
        FLExport match = null;
        for (FLExport procedure : getExports()) {
            if (procedureId.equals(procedure.getProcedureId())) {
                match = procedure;
                break;
            }
        }
        return match;
    }

    /**
     * Gets the fiscal year.
     *
     * @return the fiscal year
     */
    public String getFiscalYear() {
        return m_fiscalYear;
    }

    /**
     * Gets the formatted value.
     *
     * @param exportFormatId String
     * @param fieldName String
     * @param value String
     * @return String
     */
    public String getFormattedValue(String exportFormatId, String fieldName, String value) {
        FLExport export = getExportFromProcedureId(exportFormatId);
        ExportFormatField exportFormatField = getExportFormatFieldByName(export, fieldName);
        String formattedValue = ExportFormatManager.doPadding(value,
                (exportFormatField.getPaddingDirection() == 0
                        ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                        : exportFormatField.getPaddingDirection()),
                exportFormatField.getPaddingChar(),
                exportFormatField.getMaximumLength());
        return formattedValue;
    }

    /**
     * Gets the survey code.
     *
     * @return current survey code
     */
    public String getSurveyCode() {
        return m_surveyCode;
    }

    /**
     * Gets the survey from code.
     *
     * @param code the code
     * @return the survey from code
     */
    public FLSurvey getSurveyFromCode(String code) {
        FL_SURVEY match = null;
        for (FL_SURVEY survey : FL_SURVEY.values()) {
            if (code.contentEquals(survey.getCode())) {
                match = survey;
                break;
            }
        }
        return match;
    }

    /**
     * Gets the survey period.
     *
     * @return the survey period
     */
    public SurveyPeriod getSurveyPeriod() {
        return m_surveyPeriod;
    }

    /**
     * Gets the validation error dictionary.
     *
     * @return the validation error dictionary
     */
    public DataDictionary getValidationErrorDictionary() {
        if (m_validationErrorDictionary == null) {
            ExtendedDataDictionary ddx = getValidationErrorExtendedDataDictionary();
            m_validationErrorDictionary = DataDictionary.getDistrictDictionary(ddx, m_broker.getPersistenceKey());
        }
        return m_validationErrorDictionary;
    }

    /**
     * Gets the unique set.
     *
     * @param obj the obj
     * @return the unique set
     */
    public Set getUniqueSet(Object obj) {
        Set set = m_mapUniqueSets.get(obj);
        if (set == null) {
            set = new HashSet();
            m_mapUniqueSets.put(obj, set);
        }
        return set;
    }

    /**
     * Persist validation errors.
     *
     * @param errors the errors
     * @return
     * @throws X2BaseException the x 2 base exception
     */
    public List<X2BaseBean> persistValidationErrors(List<FLValidationError> errors) throws X2BaseException {
        ArrayList<X2BaseBean> errorBeans = new ArrayList<>();
        if (errors != null && !errors.isEmpty()) {
            ExtendedDataTable tbx = getValidationErrorExtendedDataTable();
            DataDictionary dictionary = getValidationErrorDictionary();

            for (FLValidationError error : errors) {
                X2BaseBean errorBean =
                        X2BaseBean.newInstance(tbx.getDataTableConfig().getDataTable().getDataClass(),
                                m_broker.getPersistenceKey());
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_YEAR, m_fiscalYear, dictionary);
                if (m_surveyPeriod != null) {
                    errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_SURVEY, m_surveyPeriod.getCode(), dictionary);
                }
                String errorMsg = error.getError();
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_EXPORT_CODE, error.getExport().getCode(),
                        dictionary);
                String ruleNumber = error.getRuleNumber();
                if (ruleNumber.length() > 3) {
                    ruleNumber = ruleNumber.substring(0, 3);
                }
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_RULE_NUMBER, ruleNumber, dictionary);
                if (errorMsg.length() > 50) {
                    errorMsg = errorMsg.substring(0, 50);
                }
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_ERROR, errorMsg, dictionary);
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_VALUE, error.getValue(), dictionary);
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_FIELD_NAME, error.getFieldName(), dictionary);
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_MESSAGE, error.getMessage(), dictionary);
                errorBean.setFieldValueByAlias(ALIAS_VALIDATION_ERR_EFW_OID, error.getRow().getOid(), dictionary);

                setErrorRelationship(error, errorBean, SisOrganization.class, ALIAS_VALIDATION_ERR_ORG1_OID,
                        dictionary);
                setErrorRelationship(error, errorBean, SisSchool.class, ALIAS_VALIDATION_ERR_SKL_OID, dictionary);
                setErrorRelationship(error, errorBean, SisStudent.class, ALIAS_VALIDATION_ERR_STD_OID, dictionary);
                setErrorRelationship(error, errorBean, SisStaff.class, ALIAS_VALIDATION_ERR_STF_OID, dictionary);
                setErrorRelationship(error, errorBean, Course.class, ALIAS_VALIDATION_ERR_CRS_OID, dictionary);
                setErrorRelationship(error, errorBean, MasterSchedule.class, ALIAS_VALIDATION_ERR_MST_OID, dictionary);
                setErrorRelationship(error, errorBean, ConductIncident.class, ALIAS_VALIDATION_ERR_CND_OID, dictionary);
                setErrorRelationship(error, errorBean, ConductAction.class, ALIAS_VALIDATION_ERR_ACT_OID, dictionary);

                ExtendedDataDictionary ddx = getValidationErrorExtendedDataDictionary();
                errorBean.setFieldValueByAlias("err-ddx-oid", ddx.getOid(), dictionary);

                m_broker.saveBean(errorBean);

                errorBeans.add(errorBean);
            }
        }
        return errorBeans;
    }

    /**
     * Gets the exports.
     *
     * @return FL export[]
     */
    protected FLExport[] getExports() {
        return FL_EXPORT.values();
    }

    /**
     * Decode bean path.
     *
     * @param table the table
     * @param beanPath the bean path
     * @param dictionary the dictionary
     * @return the string
     */
    private String decodeBeanPath(DataTable table, String beanPath, DataDictionary dictionary) {
        boolean last = false;
        StringBuilder resultPath = new StringBuilder();
        List<String> pathElements =
                StringUtils.convertDelimitedStringToList(beanPath, ModelProperty.PATH_DELIMITER, true);
        for (String element : pathElements) {
            if (last) {
                throw new IllegalStateException(
                        "Bean path parsing error. There can be no elements beyond the first field element in the path.");
            } else if (element.startsWith(ALIAS_PREFIX_CHAR) && element.endsWith(ALIAS_SUFFIX_CHAR)) {
                // Alias lookup.
                String alias = element.substring(1, element.length() - 1);
                DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(alias);
                if (dataField == null) {
                    throw new IllegalStateException(
                            "Bean path parsing error. The alias " + alias + " was not found.");
                } else if (dataField != null && !dataField.getDataTable().equals(table)) {
                    throw new IllegalStateException("Bean path parsing error. The alias " + alias
                            + " was is not on table " + table.getDatabaseName());
                } else if (dataField != null) {
                    // Field element is valid. Add it and mark element as last.
                    if (resultPath.length() > 0) {
                        resultPath.append(ModelProperty.PATH_DELIMITER);
                    }
                    resultPath.append(dataField.getJavaName());
                    last = true;
                }
            } else {
                boolean found = false;
                List<DataDictionaryField> dFields = dictionary.getFieldsForContext(table);
                for (DataDictionaryField dField : dFields) {
                    if (dField.getJavaName().equals(element)) {
                        // Field element is valid. Add it and mark element as last.
                        if (resultPath.length() > 0) {
                            resultPath.append(ModelProperty.PATH_DELIMITER);
                        }
                        resultPath.append(element);
                        last = true;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    List<DataDictionaryRelationship> dRelations = dictionary.getRelationshipsForContext(table);
                    for (DataDictionaryRelationship ddRelation : dRelations) {
                        if (ddRelation.getRelatedJavaName().equals(element)) {

                            if (!ddRelation.getRelatedRelationType()
                                    .equals(DataDictionaryRelationship.ONE_TYPE_CODE)) {
                                throw new IllegalStateException(
                                        "Bean path parsing error. The relationship type for "
                                                + element + " is not 1 to 1 or 1 to N.");
                            }
                            // relationship Ok. continue path. set next table in path.
                            if (resultPath.length() > 0) {
                                resultPath.append(ModelProperty.PATH_DELIMITER);
                            }
                            resultPath.append(element);
                            table = ddRelation.getRelatedDataTable();
                            found = true;
                            break;
                        }
                    }
                }
            }
        }
        // Check the path ends in a field, not a relation.
        if (!last) {
            throw new IllegalStateException("Bean path parsing error. The bean path must end in a field element");
        }
        return resultPath.toString();
    }

    /**
     * Decode related table.
     *
     * @param table the table
     * @param relationshipPath the relationship path
     * @param dictionary the dictionary
     * @return the data table
     */
    private DataTable decodeRelatedTable(DataTable table, String relationshipPath, DataDictionary dictionary) {
        List<String> pathElements =
                StringUtils.convertDelimitedStringToList(relationshipPath, ModelProperty.PATH_DELIMITER, true);
        for (String element : pathElements) {
            boolean found = false;
            List<DataDictionaryRelationship> dRelations = dictionary.getRelationshipsForContext(table);
            for (DataDictionaryRelationship ddRelation : dRelations) {
                if (ddRelation.getRelatedJavaName().equals(element)) {

                    if (!ddRelation.getRelatedRelationType().equals(DataDictionaryRelationship.ONE_TYPE_CODE)) {
                        throw new IllegalStateException("Relation path parsing error. The relationship type for "
                                + element + " is not 1 to 1 or 1 to N.");
                    }
                    table = ddRelation.getRelatedDataTable();
                    found = true;
                    break;
                }
            }
            if (!found) {
                throw new IllegalStateException("Relation not found for "
                        + element + " from table " + table.getDatabaseName());
            }
        }
        return table;
    }

    /**
     * Gets the export format dictionary.
     *
     * @param export the export
     * @return the export format dictionary
     */
    private DataDictionary getExportFormatDictionary(FLExport export) {
        if (!m_mapDataDictionary.containsKey(export.getCode())) {
            ExportFormatDefinition definition = getExportFormatDefinition(export);
            ExtendedDictionaryAttributes extendedDictionary = definition.getExtendedDataDictionary();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendedDictionary, m_broker.getPersistenceKey());
            m_mapDataDictionary.put(export.getCode(), dictionary);
        }
        return m_mapDataDictionary.get(export.getCode());
    }

    /**
     * Gets the export format field by name.
     *
     * @param export the export
     * @param fieldName the field name
     * @return the export format field by name
     */
    private ExportFormatField getExportFormatFieldByName(FLExport export, String fieldName) {
        Map<String, ExportFormatField> valueMap = null;
        if (!m_mapExportFormatFieldByName.containsKey(export.getCode())) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER +
                    ExportFormatDefinition.COL_PROCEDURE_ID, export.getProcedureId());
            BeanQuery query = new BeanQuery(ExportFormatField.class, criteria);
            valueMap = m_broker.getMapByQuery(query, ExportFormatField.COL_NAME, 100);
            m_mapExportFormatFieldByName.put(export.getCode(), valueMap);
        }
        valueMap = m_mapExportFormatFieldByName.get(export.getCode());
        return valueMap.get(fieldName);
    }

    /**
     * Gets the export format result.
     *
     * @param export the export
     * @return the export format result
     */
    private ExportFormatResult getExportFormatResult(FLExport export) {
        // TODO This method needs to query based on the name rules defined. For now just using
        // most
        // recent.
        ExportFormatResult value = null;
        if (!m_mapExportFormatResult.containsKey(export.getCode())) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatResult.REL_DEFINITION + ModelProperty.PATH_DELIMITER +
                    ExportFormatDefinition.COL_PROCEDURE_ID, export.getProcedureId());

            StringBuilder name = new StringBuilder();
            name.append(m_fiscalYear);
            name.append("-");
            name.append(m_surveyPeriod.getCode());
            name.append("-");
            name.append(export.getProcedureId());
            criteria.addEqualTo(ExportFormatResult.COL_NAME, name.toString());
            BeanQuery query = new BeanQuery(ExportFormatResult.class, criteria);
            query.addOrderBy(ExportFormatResult.COL_RUN_DATE, false);
            value = (ExportFormatResult) m_broker.getBeanByQuery(query);
            m_mapExportFormatResult.put(export.getCode(), value);
        }
        return m_mapExportFormatResult.get(export.getCode());
    }

    /**
     * Gets the field definition.
     *
     * @param export FL_EXPORT
     * @param fieldName String
     * @return Field definition
     */
    private FieldDefinition getFieldDefinition(FLExport export, String fieldName) {
        Map<String, FieldDefinition> mapFieldDefinition = m_mapFieldDefinitionByName.get(export.getCode());
        if (mapFieldDefinition == null) {
            mapFieldDefinition = new HashMap();
            m_mapFieldDefinitionByName.put(export.getCode(), mapFieldDefinition);

            ExportFormatDefinition definition = getExportFormatDefinition(export);
            Class workingBeanClass = definition.getSourceTable().getDataTable().getDataClass();
            // Load fields in order of position.
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatField.COL_DEFINITION_OID, definition.getOid());
            BeanQuery query = new BeanQuery(ExportFormatField.class, criteria);
            query.addOrderBy(ExportFormatField.COL_POSITION, true);
            QueryIterator fieldIterator = getBroker().getIteratorByQuery(query);
            try {
                // Load fields list and definitions.
                while (fieldIterator.hasNext()) {
                    ExportFormatField field = (ExportFormatField) fieldIterator.next();
                    FieldDefinition newField = null;
                    try {
                        newField = new FieldDefinition(field, workingBeanClass, getBroker());
                    } catch (X2BaseException e) {
                        // Ignore exception - let getFieldDefiniton return null
                    }
                    mapFieldDefinition.put(newField.getFieldId(), newField);
                }
            } finally {
                fieldIterator.close();
            }
        }
        return mapFieldDefinition.get(fieldName);
    }

    /**
     * Gets the related bean from row.
     *
     * @param beanClass the bean class
     * @param export the export
     * @param row the row
     * @return the related bean from row
     * @throws X2BaseException the x 2 base exception
     */
    private X2BaseBean getRelatedBeanFromRow(Class beanClass, FLExport export, ExportFormatRow row)
            throws X2BaseException {
        X2BaseBean bean = null;
        FLRelationshipKey relationship = export.getRelationshipKeyByClassName(beanClass.getName());
        if (relationship != null) {
            String value = getExportFormatRowFieldValue(row, export, relationship);
            if (!StringUtils.isEmpty(value)) {
                value = value.trim();
                DataDictionary dictionary = getExportFormatDictionary(export);
                DataTable table = dictionary.findDataDictionaryTableByClass(relationship.getQueryClass().getName())
                        .getSystemDataTable();
                String beanPath = decodeBeanPath(table, relationship.getBeanPath(), dictionary);
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(beanPath, value);
                BeanQuery query = new BeanQuery(relationship.getQueryClass(), criteria);
                bean = m_broker.getBeanByQuery(query);
                if (bean != null && !StringUtils.isEmpty(relationship.getRelatingPath())) {
                    table = dictionary.findDataDictionaryTableByClass(bean.getClass().getName())
                            .getSystemDataTable();
                    table = decodeRelatedTable(table, relationship.getRelatingPath(), dictionary);
                    beanPath = relationship.getRelatingPath() + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID;
                    if (!beanClass.equals(table.getDataClass())) {
                        throw new IllegalStateException(
                                "Bean path parsing error. The relating path is class " + table.getDataClass().getName()
                                        + " but the expected class is " + beanClass.getName());
                    }
                    criteria = new X2Criteria();
                    value = (String) WebUtils.getProperty(bean, beanPath);
                    criteria.addEqualTo(beanPath, value);
                    query = new BeanQuery(beanClass, criteria);
                    bean = m_broker.getBeanByQuery(query);
                }
            }
        }
        return bean;
    }

    /**
     * Gets the validation error extended data dictionary.
     *
     * @return the validation error extended data dictionary
     */
    private ExtendedDataDictionary getValidationErrorExtendedDataDictionary() {
        if (m_ddxValidationError == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_VALIDATION_ERROR_ID);
            BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
            m_ddxValidationError = (ExtendedDataDictionary) m_broker.getBeanByQuery(query);
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
     * @return the validation error extended data table
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

    /**
     * Sets the error relationship.
     *
     * @param error the error
     * @param errorBean the error bean
     * @param beanClass the bean class
     * @param alias the alias
     * @param dictionary the dictionary
     * @throws X2BaseException the x 2 base exception
     */
    private void setErrorRelationship(FLValidationError error,
                                      X2BaseBean errorBean,
                                      Class beanClass,
                                      String alias,
                                      DataDictionary dictionary)
            throws X2BaseException {
        X2BaseBean bean = getRelatedBeanFromRow(beanClass, error.getExport(), error.getRow());
        if (bean == null && SisOrganization.class.equals(beanClass)) {
            bean = getContext().getOrganization1();
        }
        if (bean != null) {
            errorBean.setFieldValueByAlias(alias, bean.getOid(), dictionary);
        }
    }

    /**
     * The Class SurveyPeriod.
     */
    class SurveyPeriod {
        public static final String ORA_DDX_ID = "FL-ORA-SURVEY";

        private static final String ALIAS_CODE = "ora-survey-code";
        private static final String ALIAS_END_DATE = "ora-survey-end-date";
        private static final String ALIAS_SNAPSHOT_DATE = "ora-survey-snapshot-date";
        private static final String ALIAS_START_DATE = "ora-survey-start-date";
        private static final String ALIAS_YEAR = "ora-survey-year";

        private static final String ERROR_MSG_ALIAS =
                "Alias was not found for extended data dictionary " + ORA_DDX_ID + ",";
        private static final String ERROR_MSG_DDX = "Extended data dictionary " + ORA_DDX_ID + "was not found.";
        private static final String ERROR_MSG_ORA = "Survey period organization attribute was not found.";

        private String m_code;
        private PlainDate m_endDate;
        private PlainDate m_snapshotDate;
        private PlainDate m_startDate;

        /**
         * Instantiates a new survey period.
         *
         * @param code the code
         */
        public SurveyPeriod(String code) {
            m_code = code;

            X2Criteria ddxCriteria = new X2Criteria();

            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DDX_ID);

            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
            if (ddx == null) {
                throw new IllegalStateException(ERROR_MSG_DDX);
            }
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
            String fieldYear = translateAlias(dictionary, ALIAS_YEAR);
            String fieldCode = translateAlias(dictionary, ALIAS_CODE);
            String fieldSnapshotDate = translateAlias(dictionary, ALIAS_SNAPSHOT_DATE);
            String fieldStartDate = translateAlias(dictionary, ALIAS_START_DATE);
            String fieldEndDate = translateAlias(dictionary, ALIAS_END_DATE);

            X2Criteria criteria = new X2Criteria();

            criteria.addEqualTo(OrganizationAttributes.COL_EXTENDED_DATA_DICTIONARY_OID, ddx.getOid());
            criteria.addEqualTo(fieldYear, Integer.toString(m_currentContext.getSchoolYear()));
            criteria.addEqualTo(fieldCode, code);

            QueryByCriteria query = new QueryByCriteria(OrganizationAttributes.class, criteria);
            OrganizationAttributes ora = (OrganizationAttributes) getBroker().getBeanByQuery(query);
            if (ora == null) {
                throw new IllegalStateException(ERROR_MSG_ORA);
            }

            DateAsStringConverter dateConverter = (DateAsStringConverter) ConverterFactory
                    .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
            m_snapshotDate = (PlainDate) dateConverter
                    .parseSystemString((String) ora.getFieldValueByBeanPath(fieldSnapshotDate));

            if (!FLStateReportData.SURVEY_PERIOD_1.equals(code) &&
                    !FLStateReportData.SURVEY_PERIOD_2.equals(code) &&
                    !FLStateReportData.SURVEY_PERIOD_3.equals(code) &&
                    !FLStateReportData.SURVEY_PERIOD_4.equals(code) &&
                    !FLStateReportData.SURVEY_PERIOD_6.equals(code)) {
                m_startDate = m_currentContext.getStartDate();
                m_endDate = m_currentContext.getEndDate();
            } else {
                m_startDate = (PlainDate) dateConverter
                        .parseSystemString((String) ora.getFieldValueByBeanPath(fieldStartDate));
                m_endDate = (PlainDate) dateConverter
                        .parseSystemString((String) ora.getFieldValueByBeanPath(fieldEndDate));
            }
        }

        /**
         * Gets the code.
         *
         * @return the code
         */
        public String getCode() {
            return m_code;
        }

        /**
         * Get the data certain for this period. This is currently set to the end date of the period
         * but may be redefined later.
         *
         * @return the date certain
         */
        public PlainDate getDateCertain() {
            return m_endDate;
        }

        /**
         * Gets the end date.
         *
         * @return the end date
         */
        public PlainDate getEndDate() {
            return m_endDate;
        }

        /**
         * Gets the snapshot date.
         *
         * @return the snapshot date
         */
        public PlainDate getSnapshotDate() {
            return m_snapshotDate;
        }

        /**
         * Gets the start date.
         *
         * @return the start date
         */
        public PlainDate getStartDate() {
            return m_startDate;
        }

        /**
         * Translate alias.
         *
         * @param dictionary the dictionary
         * @param alias the alias
         * @return the string
         */
        private String translateAlias(DataDictionary dictionary, String alias) {
            String javaName = null;

            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                javaName = field.getJavaName();
            } else {
                throw new IllegalStateException(ERROR_MSG_ALIAS);
            }

            return javaName;
        }

    }

}
