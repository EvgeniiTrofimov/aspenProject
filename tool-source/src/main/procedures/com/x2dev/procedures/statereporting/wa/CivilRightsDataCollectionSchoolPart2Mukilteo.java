/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.utils.converters.BooleanAsStringConverter.TRUE;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.apache.ojb.broker.query.QueryByCriteria;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.wa.CivilRightsDataCollectionSchoolPart2Mukilteo.CrdcHelper.Discipline;
import com.x2dev.procedures.statereporting.wa.CivilRightsDataCollectionSchoolPart2Mukilteo.CrdcHelper.IncidentType;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.Section504StatusCode;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;

/**
 * <p>
 * Civil Rights Data Collection - School - Part 2
 * </p>
 *
 * <p>
 * This exports prepares student demographic information for Civil Rights Data Collection.
 * It will output the following tables:
 * </p>
 *
 * <ul>
 * <li>Table 1 - Students who passed Algebra I in grade 7 or 8</li>
 * <li>Table 2.1 - Students who passed Algebra I in grade 9 or 10</li>
 * <li>Table 2.2 - Students who passed Algebra I in grade 11 or 12</li>
 * <li>Table 7 - Students who took the SAT or ACT any time during the 2013-14 school year</li>
 * <li>Table 8.1 - Students who took AP tests for all AP courses taken</li>
 * <li>Table 8.2 - Students who took AP courses but did not take any AP tests</li>
 * <li>Table 9.1 - Students who passed some AP tests taken</li>
 * <li>Table 9.2 - Students who passed no AP tests taken</li>
 * <li>Table 10 - Students absent 15 or more days during school year</li>
 * <li>Table 12.01 - Students retained in kindergarten</li>
 * <li>Table 12.02 - Students retained in grade 1</li>
 * <li>Table 12.03 - Students retained in grade 2</li>
 * <li>Table 12.04 - Students retained in grade 3</li>
 * <li>Table 12.05 - Students retained in grade 4</li>
 * <li>Table 12.06 - Students retained in grade 5</li>
 * <li>Table 12.07 - Students retained in grade 6</li>
 * <li>Table 12.08 - Students retained in grade 7</li>
 * <li>Table 12.09 - Students retained in grade 8</li>
 * <li>Table 12.10 - Students retained in grade 9</li>
 * <li>Table 12.11 - Students retained in grade 10</li>
 * <li>Table 12.12 - Students retained in grade 11</li>
 * <li>Table 12.13 - Students retained in grade 12</li>
 * <li>Table 14 - Single-Sex Inter-scholastic Athletics Sports, Teams and Participants</li>
 * <li>Table 15.1 - Prekindergarten students receiving only one out-of-school suspension</li>
 * <li>Table 15.2 - Prekindergarten students receiving more than one out-of-school suspension</li>
 * <li>Table 15.3 - Prekindergarten students expelled</li>
 * <li>Table 17.1 - Student without disabilities, Corporal Punishment</li>
 * <li>Table 17.2 - Student without disabilities, receiving one or more in-school suspensions</li>
 * <li>Table 17.3 - Student without disabilities, receiving only one out-of-school suspension</li>
 * <li>Table 17.4 - Student without disabilities, receiving more than one out-of-school suspension
 * </li>
 * <li>Table 17.5 - Student without disabilities, Expulsions with educational services</li>
 * <li>Table 17.6 - Student without disabilities, Expulsions without educational services</li>
 * <li>Table 17.7 - Student without disabilities, Expulsions under zero-tolerance policies</li>
 * <li>Table 17.8 - Student without disabilities, Referral to law enforcement</li>
 * <li>Table 17.9 - Student without disabilities, School-related arrest</li>
 * <li>Table 18.1 - Student with disabilities, Corporal Punishment</li>
 * <li>Table 18.2 - Student with disabilities, receiving one or more in-school suspensions</li>
 * <li>Table 18.3 - Student with disabilities, receiving only one out-of-school suspension</li>
 * <li>Table 18.4 - Student with disabilities, receiving more than one out-of-school suspension</li>
 * <li>Table 18.5 - Student with disabilities, Expulsions with educational services</li>
 * <li>Table 18.6 - Student with disabilities, Expulsions without educational services</li>
 * <li>Table 18.7 - Student with disabilities, Expulsions under zero-tolerance policies</li>
 * <li>Table 18.8 - Student with disabilities, Referral to law enforcement</li>
 * <li>Table 18.9 - Student with disabilities, School-related arrest</li>
 * <li>Table 31.1 - Students reported to have been harassed or bullied on the basis of sex</li>
 * <li>Table 31.2 - Students reported to have been harassed or bullied on the basis of race, color
 * or national origin</li>
 * <li>Table 31.3 - Students reported to have been harassed or bullied on the basis of disability
 * </li>
 * <li>Table 32.1 - Students disciplined for engaging in harassment or bullying on the basis of sex
 * </li>
 * <li>Table 32.2 - Students disciplined for engaging in harassment or bullying on the basis of
 * race, color or national origin</li>
 * <li>Table 32.3 - Students disciplined for engaging in harassment or bullying on the basis of
 * disability</li>
 *
 * <li>Note: The following is not displayed until changes have been made in the system to collect
 * this data</li>
 * <li>Table 34.1 - Non-IDEA students subjected to mechanical restraint</li>
 * <li>Table 34.2 - Non-IDEA students subjected to physical restraint</li>
 * <li>Table 34.3 - Non-IDEA students subjected to seclusion</li>
 * <li>Table 35.1 - Students with disabilities (IDEA) subjected to mechanical restraint</li>
 * <li>Table 35.2 - Students with disabilities (IDEA) subjected to physical restraint</li>
 * <li>Table 35.3 - Students with disabilities (IDEA) subjected to seclusion</li>
 * </ul>
 *
 * @author Follett Software Company
 *
 */

public class CivilRightsDataCollectionSchoolPart2Mukilteo extends StateReportData {

    /**
     * The CRDC entity class.
     *
     * @author Follett Software Company
     */
    public static class CivilRightsSchoolEntity extends StateReportEntity {
        /**
         * 2-d grid data to show
         */
        List<List<Object>> m_data;

        /**
         * Get the current field of the current row at position {@code i}.
         *
         * @param i column index at the current row
         * @return Object
         */
        public Object getCurrentField(int i) {
            Object result = null;
            int currentRow = getCurrentRow();
            if (m_data.get(currentRow).size() > i) {
                result = m_data.get(currentRow).get(i);
            }
            return result;
        }

        /**
         * Initialize the 2-dimensional grid to export.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            CivilRightsDataCollectionSchoolPart2Mukilteo crdc = (CivilRightsDataCollectionSchoolPart2Mukilteo) data;
            m_data = new ArrayList<List<Object>>();

            TreeSet<String> tableNames = new TreeSet<String>(crdc.m_tables.keySet());
            for (String tableName : tableNames) {
                DataGrid grid = crdc.m_tables.get(tableName);

                // table name
                m_data.add(Arrays.asList(new Object[] {tableName}));

                // columns
                List<Object> columns = new ArrayList<Object>(grid.getColumns());
                m_data.add(columns);

                // rest of the fields
                List<Map<String, Object>> rows = grid.getRows();
                for (Map<String, Object> row : rows) {
                    List<Object> line = new ArrayList<Object>();
                    for (int j = 0; j < columns.size(); j++) {
                        line.add(row.get(columns.get(j)));
                    }
                    m_data.add(line);
                }
            }

            setRowCount(m_data.size());
        }
    }

    /**
     * Retrieve the results field-by-field.
     *
     * @author Follett Software Company
     */
    protected class RetrieveResults implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            CivilRightsSchoolEntity e = (CivilRightsSchoolEntity) entity;
            return e.getCurrentField(Integer.parseInt(field.getFieldId()));
        }
    }

    /*
     * Parameters
     */
    protected static final String PARAM_ACTION_SCHOOL_ARREST = "actionSchoolArrest";
    protected static final String PARAM_ACTION_REFERRED_LAW_ENFORCEMENT = "actionReferredLawEnforcement";
    protected static final String PARAM_ACTION_EXPELLED_ZERO_TOLERANCE = "actionExpelledZeroTolerance";
    protected static final String PARAM_ACTION_EXPELLED_WITHOUT_SERVICES = "actionExpelledWithoutServices";
    protected static final String PARAM_ACTION_EXPELLED_WITH_SERVICES = "actionExpelledWithServices";
    protected static final String PARAM_ACTION_OSS = "actionOss";
    protected static final String PARAM_ACTION_ISS = "actionIss";
    protected static final String PARAM_ACTION_CORPORAL_PUNISHMENT = "actionCorporalPunishment";
    protected static final String PARAM_ASSESSMENT_OIDS = "asdOids";
    protected static final String PARAM_INCIDENT_SEX = "incidentBullyingSex";
    protected static final String PARAM_INCIDENT_RACE = "incidentBullyingRace";
    protected static final String PARAM_INCIDENT_DISABILITY = "incidentBullyingDisability";
    protected static final String PARAM_RETAINED_STUDENTS_SNAPSHOT_NAME = "retainedStdOids";
    protected static final String PARAM_START_DATE = "startDate";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_RUN_OPTIONS = "runOptions";
    protected static final String PARAM_RUN_OPTIONS_SUMMARY = "summary";
    protected static final String PARAM_RUN_OPTIONS_STUDENTS = "students";

    // CRDC Tables
    public static final String TABLE_1_ALGEBRA_7_OR_8 = "Table 01 - Students who passed Algebra I in grade 7 or 8";
    public static final String TABLE_2_1_ALGEBRA_9_OR_10 =
            "Table 02.1 - Students who passed Algebra I in grade 9 or 10";
    public static final String TABLE_2_2_ALGEBRA_11_OR_12 =
            "Table 02.2 - Students who passed Algebra I in grade 11 or 12";
    public static final String TABLE_7_SAT_OR_ACT =
            "Table 07 - Students who took the SAT or ACT any time during the 2013-14 school year";
    public static final String TABLE_8_1_AP_TESTS_ALL_AP_COURSES =
            "Table 08.1 - Students who took AP tests for all AP courses taken";
    public static final String TABLE_8_2_AP_COURSES_DIDNT_TAKE_TESTS =
            "Table 08.2 - Students who took AP courses but did not take any AP tests";
    public static final String TABLE_9_1_PASSED_SOME_AP_TESTS_TAKEN =
            "Table 09.1 - Students who passed some AP tests taken";
    public static final String TABLE_9_2_PASSED_NO_AP_TESTS_TAKEN =
            "Table 09.2 - Students who passed no AP tests taken";
    public static final String TABLE_10_ABSENT_15_DAYS =
            "Table 10 - Students absent 15 or more days during school year";
    public static final String TABLE_12_1_RETAINED_KINDERGARTEN = "Table 12.01 - Students retained in kindergarten";
    public static final String TABLE_12_2_RETAINED_IN_GRADE_1 = "Table 12.02 - Students retained in grade 1";
    public static final String TABLE_12_3_RETAINED_IN_GRADE_2 = "Table 12.03 - Students retained in grade 2";
    public static final String TABLE_12_4_RETAINED_IN_GRADE_3 = "Table 12.04 - Students retained in grade 3";
    public static final String TABLE_12_5_RETAINED_IN_GRADE_4 = "Table 12.05 - Students retained in grade 4";
    public static final String TABLE_12_6_RETAINED_IN_GRADE_5 = "Table 12.06 - Students retained in grade 5";
    public static final String TABLE_12_7_RETAINED_IN_GRADE_6 = "Table 12.07 - Students retained in grade 6";
    public static final String TABLE_12_8_RETAINED_IN_GRADE_7 = "Table 12.08 - Students retained in grade 7";
    public static final String TABLE_12_9_RETAINED_IN_GRADE_8 = "Table 12.09 - Students retained in grade 8";
    public static final String TABLE_12_10_RETAINED_IN_GRADE_9 = "Table 12.10 - Students retained in grade 9";
    public static final String TABLE_12_11_RETAINED_IN_GRADE_10 = "Table 12.11 - Students retained in grade 10";
    public static final String TABLE_12_12_RETAINED_IN_GRADE_11 = "Table 12.12 - Students retained in grade 11";
    public static final String TABLE_12_13_RETAINED_IN_GRADE_12 = "Table 12.13 - Students retained in grade 12";
    public static final String TABLE_14_SINGLE_SEX_SPORTS =
            "Table 14 - Single-Sex Interscholastic Athletics Sports, Teams and Participants";
    public static final String TABLE_15_1_PREK_ONE_OSS =
            "Table 15.1 - Prekindergarten students receiving only one out-of-school suspension";
    public static final String TABLE_15_2_PREK_MULTI_OSS =
            "Table 15.2 - Prekindergarten students receiving more than one out-of-school suspension";
    public static final String TABLE_15_3_PREK_EXPELLED = "Table 15.3 - Prekindergarten students expelled";
    public static final String TABLE_17_1_CORPORAL_PUNISHMENT =
            "Table 17.1 - Student without disabilities, Corporal Punishment";
    public static final String TABLE_17_2_MULTI_ISS =
            "Table 17.2 - Student without disabilities, receiving one or more in-school suspensions";
    public static final String TABLE_17_3_ONE_OSS =
            "Table 17.3 - Student without disabilities, receiving only one out-of-school suspension";
    public static final String TABLE_17_4_MULTI_OSS =
            "Table 17.4 - Student without disabilities, receiving more than one out-of-school suspension";
    public static final String TABLE_17_5_EXPULSIONS_W_ED =
            "Table 17.5 - Student without disabilities, Expulsions with educational services";
    public static final String TABLE_17_6_EXPULSIONS_WO_ED =
            "Table 17.6 - Student without disabilities, Expulsions without educational services";
    public static final String TABLE_17_7_EXPULSIONS_ZERO_TOL =
            "Table 17.7 - Student without disabilities, Expulsions under zero-tolerance policies";
    public static final String TABLE_17_8_LAW_ENFORCEMENT =
            "Table 17.8 - Student without disabilities, Referral to law enforcement";
    public static final String TABLE_17_9_SCHOOL_RELATED_ARREST =
            "Table 17.9 - Student without disabilities, School-related arrest";
    public static final String TABLE_18_1_CORPORAL_PUNISHMENT =
            "Table 18.1 - Student with disabilities, Corporal Punishment";
    public static final String TABLE_18_2_MULTI_ISS =
            "Table 18.2 - Student with disabilities, receiving one or more in-school suspensions";
    public static final String TABLE_18_3_ONE_OSS =
            "Table 18.3 - Student with disabilities, receiving only one out-of-school suspension";
    public static final String TABLE_18_4_MULTI_OSS =
            "Table 18.4 - Student with disabilities, receiving more than one out-of-school suspension";
    public static final String TABLE_18_5_EXPULSIONS_W_ED =
            "Table 18.5 - Student with disabilities, Expulsions with educational services";
    public static final String TABLE_18_6_EXPULSIONS_WO_ED =
            "Table 18.6 - Student with disabilities, Expulsions without educational services";
    public static final String TABLE_18_7_EXPULSIONS_ZERO_TOL =
            "Table 18.7 - Student with disabilities, Expulsions under zero-tolerance policies";
    public static final String TABLE_18_8_LAW_ENFORCEMENT =
            "Table 18.8 - Student with disabilities, Referral to law enforcement";
    public static final String TABLE_18_9_SCHOOL_RELATED_ARREST =
            "Table 18.9 - Student with disabilities, School-related arrest";
    public static final String TABLE_31_1_REPORTED_SEX =
            "Table 31.1 - Students reported to have been harassed or bullied on the basis of sex";
    public static final String TABLE_31_2_REPORTED_RACE =
            "Table 31.2 - Students reported to have been harassed or bullied on the basis of race, color or national origin";
    public static final String TABLE_31_3_REPORTED_DISABILITY =
            "Table 31.3 - Students reported to have been harassed or bullied on the basis of disability";
    public static final String TABLE_32_1_DISCIPLINED_SEX =
            "Table 32.1 - Students disciplined for engaging in harassment or bullying on the basis of sex";
    public static final String TABLE_32_2_DISCIPLINED_RACE =
            "Table 32.2 - Students disciplined for engaging in harassment or bullying on the basis of race, color or national origin";
    public static final String TABLE_32_3_DISCIPLINED_DISABILITY =
            "Table 32.3 - Students disciplined for engaging in harassment or bullying on the basis of disability";

    // Note: The following is not displayed until changes have been made in the system to collect
    // this data
    public static final String TABLE_34_1_NON_IDEA_MECHANICAL =
            "Table 34.1 - Non-IDEA students subjected to mechanical restraint";
    public static final String TABLE_34_2_NON_IDEA_PHYSICAL =
            "Table 34.2 - Non-IDEA students subjected to physical restraint";
    public static final String TABLE_34_3_NON_IDEA_SECLUSION = "Table 34.3 - Non-IDEA students subjected to seclusion";
    public static final String TABLE_35_1_IDEA_MECHANICAL =
            "Table 35.1 - Students with disabilities (IDEA) subjected to mechanical restraint";
    public static final String TABLE_35_2_IDEA_PHYSICAL =
            "Table 35.2 - Students with disabilities (IDEA) subjected to physical restraint";
    public static final String TABLE_35_3_IDEA_SECLUSION =
            "Table 35.3 - Students with disabilities (IDEA) subjected to seclusion";

    public static final String TABLE_LIST_OF_STUDENTS = "List of Students";

    // State Codes
    public static final String STATE_CODE_CT = "CT";
    public static final String STATE_CODE_FL = "FL";
    public static final String STATE_CODE_GA = "GA";
    public static final String STATE_CODE_IL = "IL";
    public static final String STATE_CODE_MA = "MA";
    public static final String STATE_CODE_MD = "MD";
    public static final String STATE_CODE_NH = "NH";
    public static final String STATE_CODE_NY = "NY";
    public static final String STATE_CODE_RI = "RI";
    public static final String STATE_CODE_VA = "VA";
    public static final String STATE_CODE_WA = "WA";

    // State Aliases
    // Student
    public static final String ALIAS_CT_CRDC_LEP_IND = "PSIS14";
    public static final String ALIAS_GA_CRDC_LEP_IND = "DOE ELL";
    public static final String ALIAS_MA_CRDC_LEP_IND = "DOE 25";
    public static final String ALIAS_MD_CRDC_LEP_IND = "DOE ELL";
    public static final String ALIAS_IL_CRDC_LEP_IND = "DOE ELL STATUS";
    public static final String ALIAS_RI_CRDC_LEP_IND = "ESL Flag";
    public static final String ALIAS_WA_CRDC_LEP_IND = "ELLStatus";
    public static final String ALIAS_DEFAULT_CRDC_LEP_IND = "CRDC LEP IND";

    public static final String ALIAS_CT_CRDC_GIFTED_IND = "PSIS27";
    public static final String ALIAS_FL_CRDC_GIFTED_IND = "FL Gifted";
    public static final String ALIAS_GA_CRDC_GIFTED_IND = "DOE Gift Referral";
    public static final String ALIAS_MA_CRDC_GIFTED_IND = "gifted";
    public static final String ALIAS_VA_CRDC_GIFTED_IND = "DOE GIFTED REFERRAL";
    public static final String ALIAS_WA_CRDC_GIFTED_IND = "HCPStatus";
    public static final String ALIAS_DEFAULT_CRDC_GIFTED_IND = "CRDC GIFTED";

    public static final String ALIAS_VA_CRDC_GED_IND = "DOE GED";
    public static final String ALIAS_DEFAULT_CRDC_GED_IND = "CRDC GED";

    public static final String ALIAS_CT_CRDC_LEP_PROGRAM_IND = "PSIS15";
    public static final String ALIAS_IL_CRDC_LEP_PROGRAM_IND = "DOE CLS PERIODS WEEK";
    public static final String ALIAS_MA_CRDC_LEP_PROGRAM_IND = "DOE 26";
    public static final String ALIAS_DEFAULT_CRDC_LEP_PROGRAM_IND = "CRDC LEP PROGRAM IND";

    public static final String ALIAS_GA_CRDC_RETAINED = "DOE Retained";
    public static final String ALIAS_VA_CRDC_RETAINED = "DOE RETAINED";
    public static final String ALIAS_DEFAULT_CRDC_RETAINED = "CRDC RETAINED";

    // Course
    public static final String ALIAS_CT_CRDC_SCED_CODE = "DOE COURSE CODE";
    public static final String ALIAS_IL_CRDC_SCED_CODE = "DOE STATE COURSE ID";
    public static final String ALIAS_MA_CRDC_SCED_CODE = "WA10-MTC";
    public static final String ALIAS_MD_CRDC_SCED_CODE = "DOE SUBJECT";
    public static final String ALIAS_NY_CRDC_SCED_CODE = "DOE STATE COURSE";
    public static final String ALIAS_RI_CRDC_SCED_CODE = "RI Course ID";
    public static final String ALIAS_VA_CRDC_SCED_CODE = "DOE SCED COURSE";
    public static final String ALIAS_WA_CRDC_SCED_CODE = "DOE STATE COURSE CODE";
    public static final String ALIAS_DEFAULT_CRDC_SCED_CODE = "CRDC COURSE ID";

    public static final String ALIAS_NY_CRDC_EXCLUDE_IND = "DOE EXCLUDE SCGD";
    public static final String ALIAS_DEFAULT_CRDC_EXCLUDE_IND = "DOE EXCLUDE CRDC";

    public static final String ALIAS_NY_CRDC_OVERRIDE = "DOE SCED OVERRIDE";
    public static final String ALIAS_DEFAULT_CRDC_OVERRIDE = "DOE SCED OVERRIDE";

    // ConductAction
    public static final String ALIAS_CT_CRDC_ARRESTED_IND = "DOE ARRESTED";
    public static final String ALIAS_GA_CRDC_EXPULSION_WITH_EDUCATION = "DOE CONTINUATION";
    public static final String ALIAS_GA_CRDC_EXPULSION_WITHOUT_EDUCATION = "DOE CONTINUATION";
    public static final String ALIAS_MA_CRDC_EXPULSION_WITH_EDUCATION = "SSDR 30";
    public static final String ALIAS_MA_CRDC_EXPULSION_WITHOUT_EDUCATION = "SSDR 30";
    public static final String ALIAS_NH_CRDC_ARRESTED_IND = "DOE ARRESTED";
    public static final String ALIAS_NH_CRDC_REFERRAL_TO_LAW = "DOE INC REPORTED";
    public static final String ALIAS_VA_CRDC_EXPULSION_UNDER_0_TOLERANCE = "EXPULSION UNDER 0 TOLERANCE";
    public static final String ALIAS_VA_CRDC_EXPULSION_WITH_ED_SVCS = "EXPULSION WITH ED SVCS";
    public static final String ALIAS_VA_CRDC_REFERRAL_TO_LAW = "DOE INC REPORTED";
    public static final String ALIAS_VA_CRDC_SCHOOL_RELATED_ARREST = "SCHOOL RELATED ARREST";

    // ConductOffense
    public static final String ALIAS_VA_CRDC_BASED_ON_SEX = "BASED ON SEX";
    public static final String ALIAS_VA_CRDC_BASED_ON_RACE = "BASED ON RACE";
    public static final String ALIAS_VA_CRDC_BASE_ON_DISABILITY = "BASED ON DISABILITY";

    // Staff
    public static final String ALIAS_MA_CRDC_OVERALL_FTE = "Overall FTE";
    public static final String ALIAS_VA_CRDC_TEACHER_FTE = "DOE TEACHER FTE";

    // MasterSchedule
    public static final String ALIAS_DEFAULT_CRDC_SUBJECT_AREA = "CRDC SUBJECT AREA";

    // State Reference Codes
    public static final String REF_CODE_FL_CRDC_CAUCASIAN = "W";
    public static final String REF_CODE_FL_CRDC_AFRICAN_AMERICAN = "B";
    public static final String REF_CODE_FL_CRDC_ASIAN = "A";
    public static final String REF_CODE_FL_CRDC_NATIVE_AMERICAN = "I";
    public static final String REF_CODE_FL_CRDC_PACIFIC_ISLANDER = "A";

    public static final String REF_CODE_GA_CRDC_CAUCASIAN = "W";
    public static final String REF_CODE_GA_CRDC_AFRICAN_AMERICAN = "B";
    public static final String REF_CODE_GA_CRDC_ASIAN = "S";
    public static final String REF_CODE_GA_CRDC_NATIVE_AMERICAN = "I";
    public static final String REF_CODE_GA_CRDC_PACIFIC_ISLANDER = "P";

    public static final String REF_CODE_IL_CRDC_CAUCASIAN = "16";
    public static final String REF_CODE_IL_CRDC_AFRICAN_AMERICAN = "14";
    public static final String REF_CODE_IL_CRDC_ASIAN = "13";
    public static final String REF_CODE_IL_CRDC_NATIVE_AMERICAN = "12";
    public static final String REF_CODE_IL_CRDC_PACIFIC_ISLANDER = "15";

    public static final String REF_CODE_MD_CRDC_CAUCASIAN = "5";
    public static final String REF_CODE_MD_CRDC_AFRICAN_AMERICAN = "3";
    public static final String REF_CODE_MD_CRDC_ASIAN = "2";
    public static final String REF_CODE_MD_CRDC_NATIVE_AMERICAN = "1";
    public static final String REF_CODE_MD_CRDC_PACIFIC_ISLANDER = "4";

    public static final String REF_CODE_NH_CRDC_CAUCASIAN = "5";
    public static final String REF_CODE_NH_CRDC_AFRICAN_AMERICAN = "4";
    public static final String REF_CODE_NH_CRDC_ASIAN = "2";
    public static final String REF_CODE_NH_CRDC_NATIVE_AMERICAN = "1";
    public static final String REF_CODE_NH_CRDC_PACIFIC_ISLANDER = "6";

    public static final String REF_CODE_NY_CRDC_CAUCASIAN = "W";
    public static final String REF_CODE_NY_CRDC_AFRICAN_AMERICAN = "B";
    public static final String REF_CODE_NY_CRDC_ASIAN = "A";
    public static final String REF_CODE_NY_CRDC_NATIVE_AMERICAN = "I";
    public static final String REF_CODE_NY_CRDC_PACIFIC_ISLANDER = "P";

    public static final String REF_CODE_RI_CRDC_CAUCASIAN = "E";
    public static final String REF_CODE_RI_CRDC_AFRICAN_AMERICAN = "C";
    public static final String REF_CODE_RI_CRDC_ASIAN = "B";
    public static final String REF_CODE_RI_CRDC_NATIVE_AMERICAN = "A";
    public static final String REF_CODE_RI_CRDC_PACIFIC_ISLANDER = "P";

    public static final String REF_CODE_WA_CRDC_CAUCASIAN = "W";
    public static final String REF_CODE_WA_CRDC_AFRICAN_AMERICAN = "B";
    public static final String REF_CODE_WA_CRDC_ASIAN = "A";
    public static final String REF_CODE_WA_CRDC_NATIVE_AMERICAN = "I";
    public static final String REF_CODE_WA_CRDC_PACIFIC_ISLANDER = "P";

    public static final String REF_CODE_DEFAULT_CRDC_CAUCASIAN = "1";
    public static final String REF_CODE_DEFAULT_CRDC_AFRICAN_AMERICAN = "2";
    public static final String REF_CODE_DEFAULT_CRDC_ASIAN = "4";
    public static final String REF_CODE_DEFAULT_CRDC_NATIVE_AMERICAN = "8";
    public static final String REF_CODE_DEFAULT_CRDC_PACIFIC_ISLANDER = "16";

    public static final String REF_CODE_IL_CRDC_MALE = "01";
    public static final String REF_CODE_IL_CRDC_FEMALE = "02";
    public static final String REF_CODE_MD_CRDC_MALE = "1";
    public static final String REF_CODE_MD_CRDC_FEMALE = "2";
    public static final String REF_CODE_DEFAULT_CRDC_MALE = "M";
    public static final String REF_CODE_DEFAULT_CRDC_FEMALE = "F";

    /*
     * Constants
     */
    protected static final String SAT_ACT_MIN_GRADE_LEVEL = "09";
    protected static final String SAT_ACT_MAX_GRADE_LEVEL = "12";
    protected static final String REPEAT_ENROLLMENT_TYPE = "Y";

    /*
     * Instance variables
     */
    protected Collection<SisStudent> m_students;
    protected CrdcHelper m_crdcHelper;
    protected GradesManager m_gradesManager;
    protected int m_apCoursesTaken;
    protected int m_apTestsFailed;
    protected int m_apTestsPassed;
    protected List<String> m_conductActionIss;
    protected List<String> m_conductActionOss;
    protected List<String> m_conductIncidentSex;
    protected List<String> m_conductIncidentRace;
    protected List<String> m_conductIncidentDisability;
    protected Map<IncidentType, Map<String, List<ConductOffense>>> m_conductIncidentsMap =
            new HashMap<IncidentType, Map<String, List<ConductOffense>>>();
    protected Map<IncidentType, Map<String, List<ConductOffense>>> m_conductIncidentsVictimsMap =
            new HashMap<IncidentType, Map<String, List<ConductOffense>>>();
    protected Map<String, DataGrid> m_tables;
    protected Map<String, GradeScale> m_gradeScaleMap;
    protected Map<String, Integer> m_gradeLevelLookup;
    protected Map<String, List<ConductAction>> m_conductActionsOss;
    protected Map<String, List<ConductAction>> m_conductActionsIss;
    protected Map<String, List<ConductOffense>> m_conductIncidents = new HashMap<String, List<ConductOffense>>();
    protected Map<String, List<ConductOffense>> m_conductIncidentsBasedOnSex =
            new HashMap<String, List<ConductOffense>>();
    protected Map<String, List<ConductOffense>> m_conductIncidentsBasedOnRace =
            new HashMap<String, List<ConductOffense>>();
    protected Map<String, List<ConductOffense>> m_conductIncidentsBasedOnDisability =
            new HashMap<String, List<ConductOffense>>();
    protected Map<String, List<ConductOffense>> m_conductIncidentsVictims = new HashMap<String, List<ConductOffense>>();
    protected Map<String, List<ConductOffense>> m_conductIncidentsVictimsBasedOnSex =
            new HashMap<String, List<ConductOffense>>();
    protected Map<String, List<ConductOffense>> m_conductIncidentsVictimsBasedOnRace =
            new HashMap<String, List<ConductOffense>>();
    protected Map<String, List<ConductOffense>> m_conductIncidentsVictimsBasedOnDisability =
            new HashMap<String, List<ConductOffense>>();
    protected PlainDate m_reportDate;
    protected PlainDate m_schoolYearEndDate;
    protected PlainDate m_schoolYearStartDate;
    protected PlainDate m_startDate;
    protected Set<String> m_assessmentSet;
    protected Set<String> m_retainedStudents;
    protected String m_activeCode;
    protected String m_runOptions;
    protected StudentHistoryHelper m_helper;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();

        // get reporting students
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);

        // set up the CRDC helper
        m_crdcHelper = new CrdcHelper(this);
        m_crdcHelper.setPersonRaceMap(m_helper.getPersonRaceMap());
        m_crdcHelper.setStartDate(m_startDate);
        m_crdcHelper.setReportDate(m_reportDate);

        // get grade level lookup
        m_gradeLevelLookup = StudentManager.buildNumericGradeLevelMap(getBroker());
        m_gradesManager = new GradesManager(getBroker());

        // loads the grade scale to be used for determining if a student passed a course
        loadGradeScales();

        // loads the conduct incidents
        loadConductIncidents();

        // loads conduct actions
        loadConductActions();

        // loads conduct actions (iss-only)
        loadConductActionsIss();

        // loads conduct actions (oss-only)
        loadConductActionsOss();

        // load SAT/ACT assessments
        loadAssessments();

        // load retained students
        loadRetainedStudents();

        // set the tables
        m_tables = new HashMap<String, DataGrid>();
        if (!PARAM_RUN_OPTIONS_STUDENTS.equals(m_runOptions)) {
            m_tables.put(TABLE_1_ALGEBRA_7_OR_8, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_2_1_ALGEBRA_9_OR_10, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_2_2_ALGEBRA_11_OR_12, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_7_SAT_OR_ACT, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_8_1_AP_TESTS_ALL_AP_COURSES, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_8_2_AP_COURSES_DIDNT_TAKE_TESTS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_9_1_PASSED_SOME_AP_TESTS_TAKEN, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_9_2_PASSED_NO_AP_TESTS_TAKEN, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_10_ABSENT_15_DAYS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_1_RETAINED_KINDERGARTEN, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_2_RETAINED_IN_GRADE_1, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_3_RETAINED_IN_GRADE_2, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_4_RETAINED_IN_GRADE_3, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_5_RETAINED_IN_GRADE_4, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_6_RETAINED_IN_GRADE_5, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_7_RETAINED_IN_GRADE_6, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_8_RETAINED_IN_GRADE_7, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_9_RETAINED_IN_GRADE_8, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_10_RETAINED_IN_GRADE_9, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_11_RETAINED_IN_GRADE_10, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_12_RETAINED_IN_GRADE_11, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_12_13_RETAINED_IN_GRADE_12, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_14_SINGLE_SEX_SPORTS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_15_1_PREK_ONE_OSS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_15_2_PREK_MULTI_OSS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_15_3_PREK_EXPELLED, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_1_CORPORAL_PUNISHMENT, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_2_MULTI_ISS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_3_ONE_OSS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_4_MULTI_OSS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_5_EXPULSIONS_W_ED, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_6_EXPULSIONS_WO_ED, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_7_EXPULSIONS_ZERO_TOL, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_8_LAW_ENFORCEMENT, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_17_9_SCHOOL_RELATED_ARREST, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_1_CORPORAL_PUNISHMENT, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_2_MULTI_ISS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_3_ONE_OSS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_4_MULTI_OSS, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_5_EXPULSIONS_W_ED, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_6_EXPULSIONS_WO_ED, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_7_EXPULSIONS_ZERO_TOL, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_8_LAW_ENFORCEMENT, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_18_9_SCHOOL_RELATED_ARREST, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_31_1_REPORTED_SEX, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_31_2_REPORTED_RACE, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_31_3_REPORTED_DISABILITY, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_32_1_DISCIPLINED_SEX, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_32_2_DISCIPLINED_RACE, CrdcHelper.generateDataGrid());
            m_tables.put(TABLE_32_3_DISCIPLINED_DISABILITY, CrdcHelper.generateDataGrid());

            // Note: The following is not displayed until changes have been made in the system to
            // collect this data
            // m_tables.put(TABLE_34_1_NON_IDEA_MECHANICAL, CrdcHelper.generateDataGrid());
            // m_tables.put(TABLE_34_2_NON_IDEA_PHYSICAL, CrdcHelper.generateDataGrid());
            // m_tables.put(TABLE_34_3_NON_IDEA_SECLUSION, CrdcHelper.generateDataGrid());
            // m_tables.put(TABLE_35_1_IDEA_MECHANICAL, CrdcHelper.generateDataGrid());
            // m_tables.put(TABLE_35_2_IDEA_PHYSICAL, CrdcHelper.generateDataGrid());
            // m_tables.put(TABLE_35_3_IDEA_SECLUSION, CrdcHelper.generateDataGrid());

            // special tables
            // m_tables.put(TABLE_74_REPORTED_HARASSMENT, getHarassmentBullyingTable());
            // m_tables.put(TABLE_88_TEACHER_ABSENTEEISM, getTeacherAbsenteeismTable());
        }

        if (!PARAM_RUN_OPTIONS_SUMMARY.equals(m_runOptions)) {
            m_tables.put(TABLE_LIST_OF_STUDENTS, new DataGrid());
        }

        // get list of students
        m_students = getBroker().getCollectionByQuery(m_helper.getStudentQuery(false));

        DataGrid listOfStudents = m_tables.get(TABLE_LIST_OF_STUDENTS);

        // go through every student
        for (SisStudent student : m_students) {
            StudentEnrollment mostRecentEnrollment =
                    m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, "EWS");
            if (mostRecentEnrollment == null || !m_activeCode.equals(mostRecentEnrollment.getStatusCode())) {
                // skip this student if he/she does not have an active enrollment record
                continue;
            }

            // If in school Context then select only the students in the selected school as of the
            // report Date.
            if (isSchoolContext()) {
                String selectedSchoolOid = getSchool().getOid();
                String lastSchoolOid = mostRecentEnrollment.getSchoolOid();

                if (!selectedSchoolOid.equals(lastSchoolOid)) {
                    continue;
                }
            }

            if (!PARAM_RUN_OPTIONS_SUMMARY.equals(m_runOptions)) {
                listOfStudents.append();
                listOfStudents.set("Student Name", student.getNameView());
                listOfStudents.set("SASID", student.getStateId());
                listOfStudents.set("School", mostRecentEnrollment.getSchool().getName());
            }

            Integer gradeLevel = m_gradeLevelLookup.get(student.getGradeLevel());
            List<ConductAction> conductActionsIss = m_conductActionsIss.get(student.getOid());
            List<ConductAction> conductActionsOss = m_conductActionsOss.get(student.getOid());

            if (gradeLevel != null) {
                // Table 1 - Students who passed Algebra I in grade 7 or 8
                if ((gradeLevel.intValue() == 7 || gradeLevel.intValue() == 8) &&
                        passedCourse(student, CrdcHelper.ALGEBRA_1_COURSES)) {
                    include(TABLE_1_ALGEBRA_7_OR_8, student);
                }

                // Table 2.1 - Students who passed Algebra I in grade 9 or 10
                if ((gradeLevel.intValue() == 9 || gradeLevel.intValue() == 10) &&
                        passedCourse(student, CrdcHelper.ALGEBRA_1_COURSES)) {
                    include(TABLE_2_1_ALGEBRA_9_OR_10, student);
                }

                // Table 2.2 - Students who passed Algebra I in grade 11 or 12
                if ((gradeLevel.intValue() == 11 || gradeLevel.intValue() == 12) &&
                        passedCourse(student, CrdcHelper.ALGEBRA_1_COURSES)) {
                    include(TABLE_2_2_ALGEBRA_11_OR_12, student);
                }
            }

            // Table 7 - Students who took the SAT or ACT any time during the 2013-14 school year
            if (m_assessmentSet.contains(student.getOid())) {
                include(TABLE_7_SAT_OR_ACT, student);
            }

            /*
             * Calculate the AP test variables now for table 18.x and 19.x
             */
            calculateAp(student);

            // Table 8.1 - Students who took AP tests for all AP courses taken
            if (m_apCoursesTaken > 0 && m_apCoursesTaken == (m_apTestsPassed + m_apTestsFailed)) {
                include(TABLE_8_1_AP_TESTS_ALL_AP_COURSES, student);
            }

            // Table 8.2 - Students who took AP courses but did not take any AP tests
            if ((m_apCoursesTaken > 0) && ((m_apTestsPassed + m_apTestsFailed) == 0)) {
                include(TABLE_8_2_AP_COURSES_DIDNT_TAKE_TESTS, student);
            }

            // Table 9.1 - Students who passed some AP tests taken
            if (m_apTestsPassed > 0 && m_apTestsFailed > 0) {
                include(TABLE_9_1_PASSED_SOME_AP_TESTS_TAKEN, student);
            }

            // Table 9.2 - Students who passed no AP tests taken
            if (m_apTestsPassed == 0 && m_apTestsFailed > 0) {
                include(TABLE_9_2_PASSED_NO_AP_TESTS_TAKEN, student);
            }

            // Table 10 - Students absent 15 or more days during school year
            if (absentDays(student) >= 15) {
                include(TABLE_10_ABSENT_15_DAYS, student);
            }

            /*
             * Retained students for table 12.x
             */
            if (m_retainedStudents.contains(student.getOid())) {
                // Table 12.1 - Students retained in kindergarten
                if (gradeLevel.intValue() == 0) {
                    include(TABLE_12_1_RETAINED_KINDERGARTEN, student);
                }

                // Table 12.2 - Students retained in grade 1
                if (gradeLevel.intValue() == 1) {
                    include(TABLE_12_2_RETAINED_IN_GRADE_1, student);
                }

                // Table 12.3 - Students retained in grade 2
                if (gradeLevel.intValue() == 2) {
                    include(TABLE_12_3_RETAINED_IN_GRADE_2, student);
                }

                // Table 12.4 - Students retained in grade 3
                if (gradeLevel.intValue() == 3) {
                    include(TABLE_12_4_RETAINED_IN_GRADE_3, student);
                }

                // Table 12.5 - Students retained in grade 4
                if (gradeLevel.intValue() == 4) {
                    include(TABLE_12_5_RETAINED_IN_GRADE_4, student);
                }

                // Table 12.6 - Students retained in grade 5
                if (gradeLevel.intValue() == 5) {
                    include(TABLE_12_6_RETAINED_IN_GRADE_5, student);
                }

                // Table 12.7 - Students retained in grade 6
                if (gradeLevel.intValue() == 6) {
                    include(TABLE_12_7_RETAINED_IN_GRADE_6, student);
                }

                // Table 12.8 - Students retained in grade 7
                if (gradeLevel.intValue() == 7) {
                    include(TABLE_12_8_RETAINED_IN_GRADE_7, student);
                }

                // Table 12.9 - Students retained in grade 8
                if (gradeLevel.intValue() == 8) {
                    include(TABLE_12_9_RETAINED_IN_GRADE_8, student);
                }

                // Table 12.10 - Students retained in grade 9
                if (gradeLevel.intValue() == 9) {
                    include(TABLE_12_10_RETAINED_IN_GRADE_9, student);
                }

                // Table 12.11 - Students retained in grade 10
                if (gradeLevel.intValue() == 10) {
                    include(TABLE_12_11_RETAINED_IN_GRADE_10, student);
                }

                // Table 12.12 - Students retained in grade 11
                if (gradeLevel.intValue() == 11) {
                    include(TABLE_12_12_RETAINED_IN_GRADE_11, student);
                }

                // Table 12.13 - Students retained in grade 12
                if (gradeLevel.intValue() == 12) {
                    include(TABLE_12_13_RETAINED_IN_GRADE_12, student);
                }
            }

            /*
             * Only for pre-kindergarten students
             * Tables 15.x
             */
            if (gradeLevel != null && gradeLevel.intValue() < 0) {
                // Table 15.1 - Prekindergarten students receiving only one out-of-school suspension
                if (conductActionsOss != null && conductActionsOss.size() == 1) {
                    include(TABLE_15_1_PREK_ONE_OSS, student);
                }

                // Table 15.2 - Prekindergarten students receiving more than one out-of-school
                // suspension
                if (conductActionsOss != null && conductActionsOss.size() > 1) {
                    include(TABLE_15_2_PREK_MULTI_OSS, student);
                }

                // Table 15.3 - Prekindergarten students expelled
                if (m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_WITH_EDUCATION_SERVICES) ||
                        m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_WITHOUT_EDUCATION_SERVICES) ||
                        m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_UNDER_ZERO_TOLERANCE)) {
                    include(TABLE_15_3_PREK_EXPELLED, student);
                }
            }

            /*
             * Discipline of students without disabilities
             * Tables 17.x
             */
            if (!m_crdcHelper.isSped(student)) {
                // Table 17.1 - Student without disabilities, Corporal Punishment
                if (m_crdcHelper.hasConductAction(student, Discipline.CORPORAL_PUNISHMENT)) {
                    include(TABLE_17_1_CORPORAL_PUNISHMENT, student);
                }

                // Table 17.2 - Student without disabilities, receiving one or more in-school
                // suspensions
                if (conductActionsIss != null && conductActionsIss.size() > 0) {
                    include(TABLE_17_2_MULTI_ISS, student);
                }

                // Table 17.3 - Student without disabilities, receiving only one out-of-school
                // suspension
                if (conductActionsOss != null && conductActionsOss.size() == 1) {
                    include(TABLE_17_3_ONE_OSS, student);
                }

                // Table 17.4 - Student without disabilities, receiving more than one out-of-school
                // suspension
                if (conductActionsOss != null && conductActionsOss.size() > 1) {
                    include(TABLE_17_4_MULTI_OSS, student);
                }

                // Table 17.5 - Student without disabilities, Expulsions with educational services
                if (m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_WITH_EDUCATION_SERVICES)) {
                    include(TABLE_17_5_EXPULSIONS_W_ED, student);
                }

                // Table 17.6 - Student without disabilities, Expulsions without educational
                // services
                if (m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_WITHOUT_EDUCATION_SERVICES)) {
                    include(TABLE_17_6_EXPULSIONS_WO_ED, student);
                }

                // Table 17.7 - Student without disabilities, Expulsions under zero-tolerance
                // policies
                if (m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_UNDER_ZERO_TOLERANCE)) {
                    include(TABLE_17_7_EXPULSIONS_ZERO_TOL, student);
                }

                // Table 17.8 - Student without disabilities, Referral to law enforcement
                if (m_crdcHelper.hasConductAction(student, Discipline.REFERRAL_TO_LAW_ENFORCEMENT)) {
                    include(TABLE_17_8_LAW_ENFORCEMENT, student);
                }

                // Table 17.9 - Student without disabilities, School-related arrest
                if (m_crdcHelper.hasConductAction(student, Discipline.SCHOOL_RELATED_ARREST)) {
                    include(TABLE_17_9_SCHOOL_RELATED_ARREST, student);
                }
            }

            /*
             * Disabilities of students WITH disabilities
             * Tables 18.x
             */
            if (m_crdcHelper.isSped(student)) {
                // Table 18.1 - Student with disabilities, Corporal Punishment
                if (m_crdcHelper.hasConductAction(student, Discipline.CORPORAL_PUNISHMENT)) {
                    include(TABLE_18_1_CORPORAL_PUNISHMENT, student);
                }

                // Table 18.2 - Student with disabilities, receiving one or more in-school
                // suspensions
                if (conductActionsIss != null && conductActionsIss.size() > 0) {
                    include(TABLE_18_2_MULTI_ISS, student);
                }

                // Table 18.3 - Student with disabilities, receiving only one out-of-school
                // suspension
                if (conductActionsOss != null && conductActionsOss.size() == 1) {
                    include(TABLE_18_3_ONE_OSS, student);
                }

                // Table 18.4 - Student with disabilities, receiving more than one out-of-school
                // suspension
                if (conductActionsOss != null && conductActionsOss.size() > 1) {
                    include(TABLE_18_4_MULTI_OSS, student);
                }

                // Table 18.5 - Student with disabilities, Expulsions with educational services
                if (m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_WITH_EDUCATION_SERVICES)) {
                    include(TABLE_18_5_EXPULSIONS_W_ED, student);
                }

                // Table 18.6 - Student with disabilities, Expulsions without educational services
                if (m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_WITHOUT_EDUCATION_SERVICES)) {
                    include(TABLE_18_6_EXPULSIONS_WO_ED, student);
                }

                // Table 18.7 - Student with disabilities, Expulsions under zero-tolerance policies
                if (m_crdcHelper.hasConductAction(student, Discipline.EXPULSION_UNDER_ZERO_TOLERANCE)) {
                    include(TABLE_18_7_EXPULSIONS_ZERO_TOL, student);
                }

                // Table 18.8 - Student with disabilities, Referral to law enforcement
                if (m_crdcHelper.hasConductAction(student, Discipline.REFERRAL_TO_LAW_ENFORCEMENT)) {
                    include(TABLE_18_8_LAW_ENFORCEMENT, student);
                }

                // Table 18.9 - Student with disabilities, School-related arrest
                if (m_crdcHelper.hasConductAction(student, Discipline.SCHOOL_RELATED_ARREST)) {
                    include(TABLE_18_9_SCHOOL_RELATED_ARREST, student);
                }
            }

            // Table 31.1 - Students reported to have been harassed or bullied on the basis of sex
            if (isVictim(student, m_conductIncidentSex, IncidentType.BASED_ON_SEX)) {
                include(TABLE_31_1_REPORTED_SEX, student);
            }

            // Table 31.2 - Students reported to have been harassed or bullied on the basis of race,
            // color or national origin
            if (isVictim(student, m_conductIncidentRace, IncidentType.BASED_ON_RACE)) {
                include(TABLE_31_2_REPORTED_RACE, student);
            }

            // Table 31.3 - Students reported to have been harassed or bullied on the basis of
            // disability
            if (isVictim(student, m_conductIncidentDisability, IncidentType.BASED_ON_DISABILITY)) {
                include(TABLE_31_3_REPORTED_DISABILITY, student);
            }

            // Table 32.1 - Students disciplined for engaging in harassment or bullying on the basis
            // of sex
            if (isAggressor(student, m_conductIncidentSex, IncidentType.BASED_ON_SEX)) {
                include(TABLE_32_1_DISCIPLINED_SEX, student);
            }

            // Table 32.2 - Students disciplined for engaging in harassment or bullying on the basis
            // of race, color or national origin
            if (isAggressor(student, m_conductIncidentRace, IncidentType.BASED_ON_RACE)) {
                include(TABLE_32_2_DISCIPLINED_RACE, student);
            }

            // Table 32.3 - Students disciplined for engaging in harassment or bullying on the basis
            // of disability
            if (isAggressor(student, m_conductIncidentDisability, IncidentType.BASED_ON_DISABILITY)) {
                include(TABLE_32_3_DISCIPLINED_DISABILITY, student);
            }
        }

        if (!PARAM_RUN_OPTIONS_SUMMARY.equals(m_runOptions)) {
            listOfStudents.sort(Arrays.asList(new String[] {"SASID", "School", "Student Name"}), false);
        }

        // set organization as the bean
        X2Criteria orgCrit = new X2Criteria();
        orgCrit.addEqualTo(SisOrganization.REL_ORGANIZATION_DEFINITION + PATH_DELIMITER +
                OrganizationDefinition.COL_LEVEL, Integer.valueOf(0));
        QueryByCriteria orgQuery = new QueryByCriteria(SisOrganization.class, orgCrit);
        setQuery(orgQuery);

        setEntityClass(CivilRightsSchoolEntity.class);
        loadFieldDefinitions();
    }

    /**
     * Initialize fields.
     */
    protected void initializeFields() {
        // System Parameters
        m_schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        m_schoolYearEndDate = getOrganization().getCurrentContext().getEndDate();
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        // Load Input Definition Parameters
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_runOptions = (String) getParameter(PARAM_RUN_OPTIONS);

        // set parameters
        String issRcds = (String) getParameter(PARAM_ACTION_ISS);
        m_conductActionIss = translateRcdOidsToCodes(issRcds);
        String ossRcds = (String) getParameter(PARAM_ACTION_OSS);
        m_conductActionOss = translateRcdOidsToCodes(ossRcds);
        String disabilityIncidentRcds = (String) getParameter(PARAM_INCIDENT_DISABILITY);
        m_conductIncidentDisability = translateRcdOidsToCodes(disabilityIncidentRcds);
        String raceIncidentRcds = (String) getParameter(PARAM_INCIDENT_RACE);
        m_conductIncidentRace = translateRcdOidsToCodes(raceIncidentRcds);
        String sexIncidentRcds = (String) getParameter(PARAM_INCIDENT_SEX);
        m_conductIncidentSex = translateRcdOidsToCodes(sexIncidentRcds);
    }

    /**
     * Return number of days absent.
     *
     * @param student SisStudent
     * @return absentDays
     */
    private int absentDays(SisStudent student) {
        int absentDays = 0;
        List<StudentAttendance> studentAttendances = m_helper.getStudentAttendances(student.getOid());

        if (studentAttendances != null && studentAttendances.size() > 0) {
            for (StudentAttendance studentAttendance : studentAttendances) {
                if (studentAttendance.getAbsentIndicator()) {
                    absentDays++;
                }
            }
        }

        return absentDays;
    }

    /**
     * Goes through the {@code student}'s Transcript records and counts up all the
     * the ones that has a SCED course in {@link #AP_ALL}
     * <p>
     * <ul>
     * <li>{@link #m_apCoursesTaken} = number of AP courses this student has taken</li>
     * <li>{@link #m_apTestsPassed} = number of AP courses passed</li>
     * <li>{@link #m_apTestsFailed} = number of AP courses failed</li>
     * </ul>
     * .
     *
     * @param student SisStudent
     */
    private void calculateAp(SisStudent student) {
        m_apCoursesTaken = 0;
        m_apTestsPassed = 0;
        m_apTestsFailed = 0;

        List<Transcript> transcripts = m_helper.getStudentTranscripts(student);
        String scedCourseCodeFieldName = m_crdcHelper.getScedCourseCodeFieldName();
        String scedCourseCodeOverrideFieldName = m_crdcHelper.getScedCourseCodeOverrideFieldName();

        if (!StringUtils.isEmpty(scedCourseCodeFieldName)) {
            if (transcripts != null) {
                for (Transcript transcript : transcripts) {
                    SchoolCourse schoolCourse = transcript.getSchoolCourse();

                    if (schoolCourse != null) {
                        Course course = schoolCourse.getCourse();

                        if (course != null) {
                            String courseSCEDCode = (String) course.getFieldValueByBeanPath(scedCourseCodeFieldName);

                            // If SCED Code Override is present then use it.
                            String courseSCEDOverrideCode = null;
                            if (scedCourseCodeOverrideFieldName != null) {
                                courseSCEDOverrideCode =
                                        (String) course.getFieldValueByBeanPath(scedCourseCodeOverrideFieldName);

                                if (!StringUtils.isEmpty(courseSCEDOverrideCode)) {
                                    courseSCEDCode = courseSCEDOverrideCode;
                                }
                            }

                            if (!StringUtils.isEmpty(courseSCEDCode)) {
                                courseSCEDCode = StringUtils.padRight(courseSCEDCode, 5);
                                boolean isAp = CrdcHelper.arrayContains(CrdcHelper.AP_MATH, courseSCEDCode) ||
                                        CrdcHelper.arrayContains(CrdcHelper.AP_OTHER, courseSCEDCode) ||
                                        CrdcHelper.arrayContains(CrdcHelper.AP_SCIENCE, courseSCEDCode);
                                if (isAp) {
                                    m_apCoursesTaken++;
                                    String apTestResult = (String) transcript.getFieldValueByAlias("AP test result");

                                    if ("Pass".equals(apTestResult)) {
                                        m_apTestsPassed++;
                                    } else if ("Fail".equals(apTestResult)) {
                                        m_apTestsFailed++;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Returns the grade value for the passed term code.
     *
     * @param transcript Transcript
     * @param termCode String
     * @return String
     */
    private String getGrade(Transcript transcript, String termCode) {
        String grade = "";

        if ("T1".equals(termCode) || "SS".equals(termCode) || "Y0".equals(termCode) || "Q1".equals(termCode)) {
            grade = transcript.getFieldA020();
        } else if ("T2".equals(termCode) || "Q2".equals(termCode)) {
            grade = transcript.getFieldA025();
        } else if ("S1".equals(termCode) || "T3".equals(termCode)) {
            grade = transcript.getFieldA030();
        } else if ("S2".equals(termCode)) {
            grade = transcript.getFieldA045();
        } else if ("Q3".equals(termCode)) {
            grade = transcript.getFieldA035();
        } else if ("Q4".equals(termCode)) {
            grade = transcript.getFieldA040();
        }

        if (grade == null) {
            grade = "";
        }

        return grade;
    }

    /**
     * Get the Transcript Term Code.
     *
     * @param transcript Transcript
     * @return String
     */
    private String getTranscriptTermCode(Transcript transcript) {
        String termCode = null;

        if (transcript.getMasterSchedule() != null &&
                transcript.getMasterSchedule().getScheduleTerm() != null &&
                !StringUtils.isEmpty(transcript.getMasterSchedule().getScheduleTerm().getCode())) {
            termCode = transcript.getMasterSchedule().getScheduleTerm().getCode();
        } else {
            termCode = transcript.getTermCode();
        }

        if (StringUtils.isEmpty(termCode) && transcript.getMasterSchedule() != null) {
            termCode = transcript.getMasterSchedule().getTermView();
        }

        return termCode;
    }

    /**
     * Increment a count in the {@code table} in the following categories:
     * <p>
     * <ul>
     * <li><strong>Race</strong> - depends on {@link #getStudentRace(SisStudent)}</li>
     * <li><strong>Special Ed</strong> - depends on {@link #isSped(SisStudent)}</li>
     * <li><strong>Section 504</strong> - depends on {@link #isSection504(SisStudent)}</li>
     * <li><strong>LEP</strong> - depends on {@link #isConsideredLep(SisStudent)}</li>
     * </ul>
     * .
     *
     * @param table String
     * @param student SisStudent
     */
    private void include(String table, SisStudent student) {
        if (!PARAM_RUN_OPTIONS_STUDENTS.equals(m_runOptions)) {
            DataGrid grid = m_tables.get(table);
            m_crdcHelper.includeStudent(grid, student);
        }
    }

    /**
     * Were there any Conduct Offenses with Incidents involving {@code incidentCode} in which
     * the {@code student} was the <strong>aggressor</strong>?.
     *
     * @param student SisStudent
     * @param incidentCodes List<String>
     * @param incidentType IncidentType
     * @return true, if is aggressor
     * @see #isVictim(SisStudent, String)
     */
    private boolean isAggressor(SisStudent student, List<String> incidentCodes, IncidentType incidentType) {
        boolean isEngagedIn = false;
        String studentOid = student.getOid();
        Map<String, List<ConductOffense>> conductIncidents = null;

        // This scenario was done specifically for VA.
        if (m_conductIncidentsMap.containsKey(incidentType)) {
            conductIncidents = m_conductIncidentsMap.get(incidentType);
        }

        // All other states will come in this scenario, as they are saved in the map for the key
        // IncidentType.OTHER
        if (conductIncidents == null) {
            if (m_conductIncidentsMap.containsKey(IncidentType.OTHER)) {
                conductIncidents = m_conductIncidentsMap.get(IncidentType.OTHER);
            }
        }

        if (conductIncidents != null) {
            Collection<ConductOffense> offenses = conductIncidents.get(studentOid);
            if (offenses != null) {
                for (ConductOffense offense : offenses) {
                    for (String code : incidentCodes) {
                        if (code.equals(offense.getIncidentCode())) {
                            isEngagedIn = true;
                            break;
                        }
                    }
                }
            }
        }

        // TODO: may have to revisit for IL as they use OFFENSECODE2 and OFFENSECODE3 on the
        // incident record

        return isEngagedIn;
    }

    /**
     * Were there any Conduct Offenses with Incidents involving {@code incidentCode} in which
     * the {@code student} was the <strong>victim</strong>?.
     *
     * @param student SisStudent
     * @param incidentCodes List<String>
     * @param incidentType IncidentType
     * @return true, if is victim
     * @see #isAggressor(SisStudent, String)
     */
    private boolean isVictim(SisStudent student, List<String> incidentCodes, IncidentType incidentType) {
        boolean isVictim = false;
        String studentOid = student.getOid();
        Map<String, List<ConductOffense>> conductIncidentVictims = null;

        if (m_conductIncidentsVictimsMap.containsKey(incidentType))// This scenario was done
                                                                   // specifically for VA.
        {
            conductIncidentVictims = m_conductIncidentsVictimsMap.get(incidentType);
        }
        if (conductIncidentVictims == null) // All other states will come in this scenario, as they
                                            // are saved in the map for the key IncidentType.OTHER
        {
            if (m_conductIncidentsVictimsMap.containsKey(IncidentType.OTHER)) {
                conductIncidentVictims = m_conductIncidentsVictimsMap.get(IncidentType.OTHER);
            }
        }

        if (conductIncidentVictims != null) {
            Collection<ConductOffense> offenses = conductIncidentVictims.get(studentOid);
            if (offenses != null) {
                for (ConductOffense offense : offenses) {
                    for (String code : incidentCodes) {
                        if (code.equals(offense.getIncidentCode())) {
                            isVictim = true;
                            break;
                        }
                    }
                }
            }
        }
        return isVictim;
    }

    /**
     * Load all student assessment records that have a assessment definition named "ACT" or "SAT".
     */
    private void loadAssessments() {
        m_assessmentSet = new HashSet<String>();

        // get the oids from user input
        String asdOidsAsString = (String) getParameter(PARAM_ASSESSMENT_OIDS);
        List<String> asdOids = StringUtils.convertDelimitedStringToList(asdOidsAsString, ',');

        if (asdOids.size() > 0) {
            X2Criteria assessmentCriteria = new X2Criteria();
            assessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, m_schoolYearStartDate);
            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, m_schoolYearEndDate);
            X2Criteria endDateCriteriaB = new X2Criteria();
            endDateCriteriaB.addNotNull(StudentAssessment.COL_DATE);
            endDateCriteria.addOrCriteria(endDateCriteriaB);
            assessmentCriteria.addAndCriteria(endDateCriteria);

            assessmentCriteria.addIn(StudentAssessment.COL_ASSESSMENT_DEFINITION_OID, asdOids);

            SubQuery assessmentQuery =
                    new SubQuery(StudentAssessment.class, StudentAssessment.COL_STUDENT_OID, assessmentCriteria);
            List<String> studentOids = (List<String>) getBroker().getSubQueryCollectionByQuery(assessmentQuery);
            m_assessmentSet.addAll(studentOids);
        }
    }

    /**
     * Load all the conduct actions that are dated between {@code m_startDate} and {@code
     * m_reportDate}
     * <p>
     * This loads all the conduct actions that the (1) user specifies in the input definition, and
     * (2) excluding ISS and OSS.
     */
    private void loadConductActions() {
        String state = PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STATE);
        String rcds = null;
        Collection<String> actionCodes = null;
        X2Criteria actionCriteria = null;
        BeanQuery actionQuery = null;
        Map actionMap = null;

        // Load corporal punishment action codes
        rcds = (String) getParameter(PARAM_ACTION_CORPORAL_PUNISHMENT);
        actionCodes = translateRcdOidsToCodes(rcds);
        if (actionCodes.size() > 0) {
            actionCriteria = new X2Criteria();
            actionCriteria.addIn(ConductAction.COL_ACTION_CODE, actionCodes);
            actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            actionQuery = new BeanQuery(ConductAction.class, actionCriteria);
            actionMap = getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_STUDENT_OID, 128);
            m_crdcHelper.setConductActions(Discipline.CORPORAL_PUNISHMENT, actionMap, actionCodes);
        }

        // Load expulsions with edu services action codes
        rcds = (String) getParameter(PARAM_ACTION_EXPELLED_WITH_SERVICES);
        actionCodes = translateRcdOidsToCodes(rcds);
        if (actionCodes.size() > 0) {
            actionCriteria = new X2Criteria();
            actionCriteria.addIn(ConductAction.COL_ACTION_CODE, actionCodes);
            actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            if (STATE_CODE_VA.equals(state)) {
                String expulsionWithEdSvcs = translateAliasToJavaName(ALIAS_VA_CRDC_EXPULSION_WITH_ED_SVCS, true);
                actionCriteria.addEqualTo(expulsionWithEdSvcs, BooleanAsStringConverter.TRUE);
            }
            actionQuery = new BeanQuery(ConductAction.class, actionCriteria);
            actionMap = getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_STUDENT_OID, 128);
            m_crdcHelper.setConductActions(Discipline.EXPULSION_WITH_EDUCATION_SERVICES, actionMap, actionCodes);
        }

        // Load expulsions without edu services
        rcds = (String) getParameter(PARAM_ACTION_EXPELLED_WITHOUT_SERVICES);
        actionCodes = translateRcdOidsToCodes(rcds);
        if (actionCodes.size() > 0) {
            actionCriteria = new X2Criteria();
            actionCriteria.addIn(ConductAction.COL_ACTION_CODE, actionCodes);
            actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            if (STATE_CODE_VA.equals(state)) {
                String expulsionWithEdSvcs = translateAliasToJavaName(ALIAS_VA_CRDC_EXPULSION_WITH_ED_SVCS, true);
                actionCriteria.addEqualTo(expulsionWithEdSvcs, BooleanAsStringConverter.FALSE);
            }
            actionQuery = new BeanQuery(ConductAction.class, actionCriteria);
            actionMap = getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_STUDENT_OID, 128);
            m_crdcHelper.setConductActions(Discipline.EXPULSION_WITHOUT_EDUCATION_SERVICES, actionMap, actionCodes);
        }

        // Load expulsions with zero tolerance
        rcds = (String) getParameter(PARAM_ACTION_EXPELLED_ZERO_TOLERANCE);
        actionCodes = translateRcdOidsToCodes(rcds);
        if (actionCodes.size() > 0) {
            actionCriteria = new X2Criteria();
            actionCriteria.addIn(ConductAction.COL_ACTION_CODE, actionCodes);
            actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            if (STATE_CODE_VA.equals(state)) {
                String expelZeroTolerance = translateAliasToJavaName(ALIAS_VA_CRDC_EXPULSION_UNDER_0_TOLERANCE, true);
                actionCriteria.addEqualTo(expelZeroTolerance, BooleanAsStringConverter.TRUE);
            }
            actionQuery = new BeanQuery(ConductAction.class, actionCriteria);
            actionMap = getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_STUDENT_OID, 128);
            m_crdcHelper.setConductActions(Discipline.EXPULSION_UNDER_ZERO_TOLERANCE, actionMap, actionCodes);
        }

        // Load referral to law enforcement (or if state is VA, load all)
        rcds = (String) getParameter(PARAM_ACTION_REFERRED_LAW_ENFORCEMENT);
        actionCodes = translateRcdOidsToCodes(rcds);
        if (actionCodes.size() > 0 || STATE_CODE_VA.equals(state)) {
            actionCriteria = new X2Criteria();
            actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            if (!STATE_CODE_VA.equals(state)) {
                actionCriteria.addIn(ConductAction.COL_ACTION_CODE, actionCodes);
            }
            actionQuery = new BeanQuery(ConductAction.class, actionCriteria);
            actionMap = getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_STUDENT_OID, 128);
            m_crdcHelper.setConductActions(Discipline.REFERRAL_TO_LAW_ENFORCEMENT, actionMap, actionCodes);
        }

        // Load school-related arrests
        rcds = (String) getParameter(PARAM_ACTION_SCHOOL_ARREST);
        actionCodes = translateRcdOidsToCodes(rcds);
        if (actionCodes.size() > 0 || STATE_CODE_CT.equals(state)) {
            actionCriteria = new X2Criteria();
            actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            if (!STATE_CODE_CT.equals(state)) {
                actionCriteria.addIn(ConductAction.COL_ACTION_CODE, actionCodes);
                if (STATE_CODE_VA.equals(state)) {
                    String sklRelatedArrest = translateAliasToJavaName(ALIAS_VA_CRDC_SCHOOL_RELATED_ARREST, true);
                    actionCriteria.addEqualTo(sklRelatedArrest, BooleanAsStringConverter.TRUE);
                }
            }
            actionQuery = new BeanQuery(ConductAction.class, actionCriteria);
            actionMap = getBroker().getGroupedCollectionByQuery(actionQuery, ConductAction.COL_STUDENT_OID, 128);
            m_crdcHelper.setConductActions(Discipline.SCHOOL_RELATED_ARREST, actionMap, actionCodes);
        }
    }

    /**
     * Load all the ISS conduct actions that are dated between {@code m_startDate} and {@code
     * m_reportDate}.
     */
    private void loadConductActionsIss() {
        m_conductActionsIss = new HashMap<String, List<ConductAction>>();
        if (m_conductActionIss.size() > 0) {
            X2Criteria conductActionCriteria = new X2Criteria();
            conductActionCriteria.addIn(ConductAction.COL_ACTION_CODE, m_conductActionIss);
            conductActionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            conductActionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            BeanQuery conductActionQuery = new BeanQuery(ConductAction.class, conductActionCriteria);
            m_conductActionsIss =
                    getBroker().getGroupedCollectionByQuery(conductActionQuery, ConductAction.COL_STUDENT_OID, 2048);
        }
    }

    /**
     * Load all the OSS conduct actions that are dated between {@code m_startDate} and {@code
     * m_reportDate}.
     */
    private void loadConductActionsOss() {
        m_conductActionsOss = new HashMap<String, List<ConductAction>>();
        if (m_conductActionOss.size() > 0) {
            X2Criteria conductActionCriteria = new X2Criteria();
            conductActionCriteria.addIn(ConductAction.COL_ACTION_CODE, m_conductActionOss);
            conductActionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
            conductActionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
            BeanQuery conductActionQuery = new BeanQuery(ConductAction.class, conductActionCriteria);
            m_conductActionsOss =
                    getBroker().getGroupedCollectionByQuery(conductActionQuery, ConductAction.COL_STUDENT_OID, 2048);
        }
    }

    /**
     * Load all conduct incidents that are dated between {@code m_startDate} and {@code
     * m_reportDate}.
     */
    private void loadConductIncidents() {
        String state = PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STATE);

        List<String> allCodes = new ArrayList<String>();
        allCodes.addAll(m_conductIncidentSex);
        allCodes.addAll(m_conductIncidentRace);
        allCodes.addAll(m_conductIncidentDisability);

        if (allCodes.size() > 0 && !STATE_CODE_VA.equals(state)) {
            X2Criteria offenseCriteria = new X2Criteria();
            offenseCriteria.addIn(ConductOffense.COL_INCIDENT_CODE, allCodes);
            offenseCriteria.addGreaterOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                    ConductIncident.COL_INCIDENT_DATE, m_startDate);
            offenseCriteria.addLessOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                    ConductIncident.COL_INCIDENT_DATE, m_reportDate);
            BeanQuery offenseQuery = new BeanQuery(ConductOffense.class, offenseCriteria);
            m_conductIncidents = getBroker().getGroupedCollectionByQuery(offenseQuery,
                    ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                            ConductIncident.COL_STUDENT_OID,
                    128);

            m_conductIncidentsVictims = getBroker().getGroupedCollectionByQuery(offenseQuery,
                    ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                            ConductIncident.COL_VICTIM_OID,
                    128);
            m_conductIncidentsMap.put(IncidentType.OTHER, m_conductIncidents);
            m_conductIncidentsVictimsMap.put(IncidentType.OTHER, m_conductIncidentsVictims);
        } else if (STATE_CODE_VA.equals(state)) {
            // Based on Sex
            if (m_conductIncidentSex != null && m_conductIncidentSex.size() > 0) {
                X2Criteria offenceBasedOnSexCriteria = new X2Criteria();
                String basedOnSex = translateAliasToJavaName(ALIAS_VA_CRDC_BASED_ON_SEX, true);
                offenceBasedOnSexCriteria.addIn(ConductOffense.COL_INCIDENT_CODE, m_conductIncidentSex);
                offenceBasedOnSexCriteria.addGreaterOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        ConductIncident.COL_INCIDENT_DATE, m_startDate);
                offenceBasedOnSexCriteria.addLessOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        ConductIncident.COL_INCIDENT_DATE, m_reportDate);
                offenceBasedOnSexCriteria.addEqualTo(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        basedOnSex, BooleanAsStringConverter.TRUE);
                BeanQuery offenceBasedOnSexQuery = new BeanQuery(ConductOffense.class, offenceBasedOnSexCriteria);
                m_conductIncidentsBasedOnSex = getBroker().getGroupedCollectionByQuery(offenceBasedOnSexQuery,
                        ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                                ConductIncident.COL_STUDENT_OID,
                        128);
                m_conductIncidentsVictimsBasedOnSex = getBroker().getGroupedCollectionByQuery(offenceBasedOnSexQuery,
                        ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                                ConductIncident.COL_VICTIM_OID,
                        128);
                m_conductIncidentsMap.put(IncidentType.BASED_ON_SEX, m_conductIncidentsBasedOnSex);
                m_conductIncidentsVictimsMap.put(IncidentType.BASED_ON_SEX, m_conductIncidentsVictimsBasedOnSex);
            }

            // Based on Race
            if (m_conductIncidentRace != null && m_conductIncidentRace.size() > 0) {
                X2Criteria offenceBasedOnRaceCriteria = new X2Criteria();
                String basedOnRace = translateAliasToJavaName(ALIAS_VA_CRDC_BASED_ON_RACE, true);
                offenceBasedOnRaceCriteria.addIn(ConductOffense.COL_INCIDENT_CODE, m_conductIncidentRace);
                offenceBasedOnRaceCriteria.addGreaterOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        ConductIncident.COL_INCIDENT_DATE, m_startDate);
                offenceBasedOnRaceCriteria.addLessOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        ConductIncident.COL_INCIDENT_DATE, m_reportDate);
                offenceBasedOnRaceCriteria.addEqualTo(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        basedOnRace, BooleanAsStringConverter.TRUE);
                BeanQuery offenceBasedOnRaceQuery = new BeanQuery(ConductOffense.class, offenceBasedOnRaceCriteria);
                m_conductIncidentsBasedOnRace = getBroker().getGroupedCollectionByQuery(offenceBasedOnRaceQuery,
                        ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                                ConductIncident.COL_STUDENT_OID,
                        128);
                m_conductIncidentsVictimsBasedOnRace = getBroker().getGroupedCollectionByQuery(offenceBasedOnRaceQuery,
                        ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                                ConductIncident.COL_VICTIM_OID,
                        128);
                m_conductIncidentsMap.put(IncidentType.BASED_ON_RACE, m_conductIncidentsBasedOnRace);
                m_conductIncidentsVictimsMap.put(IncidentType.BASED_ON_RACE, m_conductIncidentsVictimsBasedOnRace);
            }

            // Based on Disability
            if (m_conductIncidentDisability != null && m_conductIncidentDisability.size() > 0) {
                X2Criteria offenceBasedOnDisabilityCriteria = new X2Criteria();
                String basedOnDisability = translateAliasToJavaName(ALIAS_VA_CRDC_BASE_ON_DISABILITY, true);
                offenceBasedOnDisabilityCriteria.addIn(ConductOffense.COL_INCIDENT_CODE, m_conductIncidentDisability);
                offenceBasedOnDisabilityCriteria.addGreaterOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        ConductIncident.COL_INCIDENT_DATE, m_startDate);
                offenceBasedOnDisabilityCriteria.addLessOrEqualThan(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        ConductIncident.COL_INCIDENT_DATE, m_reportDate);
                offenceBasedOnDisabilityCriteria.addEqualTo(ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                        basedOnDisability, BooleanAsStringConverter.TRUE);
                BeanQuery offenceBasedOnDisabilityQuery =
                        new BeanQuery(ConductOffense.class, offenceBasedOnDisabilityCriteria);
                m_conductIncidentsBasedOnDisability =
                        getBroker().getGroupedCollectionByQuery(offenceBasedOnDisabilityQuery,
                                ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                                        ConductIncident.COL_STUDENT_OID,
                                128);
                m_conductIncidentsVictimsBasedOnDisability =
                        getBroker().getGroupedCollectionByQuery(offenceBasedOnDisabilityQuery,
                                ConductOffense.REL_INCIDENT + PATH_DELIMITER +
                                        ConductIncident.COL_VICTIM_OID,
                                128);
                m_conductIncidentsMap.put(IncidentType.BASED_ON_DISABILITY, m_conductIncidentsBasedOnDisability);
                m_conductIncidentsVictimsMap.put(IncidentType.BASED_ON_DISABILITY,
                        m_conductIncidentsVictimsBasedOnDisability);
            }
        }
    }

    /**
     * Set up the fields for this export
     * <p>
     * All fields with use {@link RetrieveResults} as a FieldRetriever
     * </ul>
     * .
     */
    private void loadFieldDefinitions() {
        List<FieldDefinition> definitions = new ArrayList<FieldDefinition>();
        int max = 3;
        for (int i = 0; i < max; i++) {
            definitions.add(new FieldDefinition(String.valueOf(i), "", "", false, 0, 100, "", null,
                    new RetrieveResults(), null, null));
        }
        super.setFieldDefinitions(definitions);
    }


    /**
     * Pre-load grade scales.
     */
    private void loadGradeScales() {
        m_gradeScaleMap = new HashMap<String, GradeScale>();

        X2Criteria criteria = new X2Criteria();

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        QueryByCriteria gtcQuery = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        QueryIterator gtcIterator = getBroker().getIteratorByQuery(gtcQuery);
        try {
            while (gtcIterator.hasNext()) {
                TranscriptColumnDefinition tcd = (TranscriptColumnDefinition) gtcIterator.next();
                m_gradeScaleMap.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
        } finally {
            gtcIterator.close();
        }
    }

    /**
     * Load data for "Table 74 - Reported allegations of harassment and bullying"
     * <p>
     * <ul>
     * <li>
     * Get total count of conduct incidents from {@link #m_startDate} to
     * {@link #m_reportDate} which the incident code falls within
     * {@link #m_conductIncidentSex}.
     * </li>
     *
     * <li>
     * Get total count of conduct incidents from {@link #m_startDate} to
     * {@link #m_reportDate} which the incident code falls within
     * {@link #m_conductIncidentRace}.
     * </li>
     *
     * <li>
     * Get total count of conduct incidents from {@link #m_startDate} to
     * {@link #m_reportDate} which the incident code falls within
     * {@link #m_conductIncidentDisability}.
     * </li>
     *
     * </ul>
     */
    /*
     * private DataGrid getHarassmentBullyingTable()
     * {
     * String state = PreferenceManager.getPreferenceValue(getOrganization(),
     * SystemPreferenceDefinition.STATE);
     *
     * DataGrid grid = new DataGrid();
     *
     * String schoolOid = getSchool().getOid();
     * X2Criteria criteria;
     * BeanQuery query;
     *
     * grid.append();
     * grid.set("Category", "Allegations of harassment or bullying on the basis of sex");
     * if (m_conductIncidentSex.size() > 0)
     * {
     * criteria = new X2Criteria();
     * criteria.addIn(ConductIncident.REL_CONDUCT_OFFENSES + PATH_DELIMITER +
     * ConductOffense.COL_INCIDENT_CODE, m_conductIncidentSex);
     * criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);
     * criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_reportDate);
     * criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, schoolOid);
     * if (STATE_CODE_VA.equals(state))
     * {
     * String basedOnSex = translateAliasToJavaName(ALIAS_VA_CRDC_BASED_ON_SEX, true);
     * criteria.addEqualTo(basedOnSex, BooleanAsStringConverter.TRUE);
     * }
     * query = new BeanQuery(ConductIncident.class, criteria, true, false);
     * int sexIncidentCount = getBroker().getCount(query);
     * grid.set("Number of incidents", Integer.valueOf(sexIncidentCount));
     * }
     * else
     * {
     * // output blank
     * grid.set("Number of incidents", "");
     * }
     *
     * grid.append();
     * grid.set("Category",
     * "Allegations of harassment or bullying on the basis of race, color, or national origin");
     * if (m_conductIncidentRace.size() > 0)
     * {
     * criteria = new X2Criteria();
     * criteria.addIn(ConductIncident.REL_CONDUCT_OFFENSES + PATH_DELIMITER +
     * ConductIncident.COL_INCIDENT_CODE, m_conductIncidentRace);
     * criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);
     * criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_reportDate);
     * criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, schoolOid);
     * if (STATE_CODE_VA.equals(state))
     * {
     * String basedOnRace = translateAliasToJavaName(ALIAS_VA_CRDC_BASED_ON_RACE, true);
     * criteria.addEqualTo(basedOnRace, BooleanAsStringConverter.TRUE);
     * }
     * query = new BeanQuery(ConductIncident.class, criteria, true, false);
     * int raceIncidentCount = getBroker().getCount(query);
     * grid.set("Number of incidents", Integer.valueOf(raceIncidentCount));
     * }
     * else
     * {
     * // output blank
     * grid.set("Number of incidents", "");
     * }
     *
     * grid.append();
     * grid.set("Category", "Allegations of harassment or bullying on the basis of disability");
     * if (m_conductIncidentDisability.size() > 0)
     * {
     * criteria = new X2Criteria();
     * criteria.addIn(ConductIncident.REL_CONDUCT_OFFENSES + PATH_DELIMITER +
     * ConductIncident.COL_INCIDENT_CODE, m_conductIncidentDisability);
     * criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);
     * criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_reportDate);
     * criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, schoolOid);
     * if (STATE_CODE_VA.equals(state))
     * {
     * String basedOnDisability = translateAliasToJavaName(ALIAS_VA_CRDC_BASE_ON_DISABILITY, true);
     * criteria.addEqualTo(basedOnDisability, BooleanAsStringConverter.TRUE);
     * }
     * query = new BeanQuery(ConductIncident.class, criteria, true, false);
     * int disabilityIncidentCount = getBroker().getCount(query);
     * grid.set("Number of incidents", Integer.valueOf(disabilityIncidentCount));
     * }
     * else
     * {
     * // output blank
     * grid.set("Number of incidents", "");
     * }
     *
     * return grid;
     * }
     */

    /**
     * Create set of student oids that are retained
     */
    private void loadRetainedStudents() {
        m_retainedStudents = new HashSet<String>();

        // is there a "retained" field on the student?
        String retainedField = m_crdcHelper.getRetainedCode();

        if (!StringUtils.isEmpty(retainedField)) // if yes, use it
        {
            X2Criteria studentCriteria = m_helper.getStudentCriteria().copy();
            studentCriteria.addEqualTo(retainedField, BooleanAsStringConverter.TRUE);
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            List<String> studentOids = (List<String>) getBroker().getSubQueryCollectionByQuery(subQuery);
            m_retainedStudents.addAll(studentOids);
        } else // if no, use snapshot of retained students
        {
            String snapshotName = (String) getParameter(PARAM_RETAINED_STUDENTS_SNAPSHOT_NAME);
            if (!StringUtils.isEmpty(snapshotName)) {
                SubQuery retainedStudentsSubQuery =
                        ReportUtils.getRecordSetSubQuery(snapshotName, getUser(), getSchool());
                List<String> retainedStudentsList =
                        (List<String>) getBroker().getSubQueryCollectionByQuery(retainedStudentsSubQuery);
                m_retainedStudents.addAll(retainedStudentsList);
            }
        }
    }

    /**
     * Load data for "Table 88 - Teacher Absenteeism".
     *
     * @param student SisStudent
     * @param courseSCEDCodeGroup String[]
     * @return true, if successful
     */
    /*
     * private DataGrid getTeacherAbsenteeismTable()
     * {
     * DataGrid grid = new DataGrid();
     * String stfFte = m_crdcHelper.getStaffFteCode();
     *
     * if (!StringUtils.isEmpty(stfFte))
     * {
     * X2Criteria teacherCriteria = new X2Criteria();
     * String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
     * SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
     * teacherCriteria.addEqualTo(SisStaff.COL_STATUS, activeCode);
     * teacherCriteria.addEqualTo(SisStaff.COL_STAFF_TYPE, "Teacher");
     * teacherCriteria.addNotEmpty(stfFte, getBroker().getPersistenceKey());
     * if (isSchoolContext())
     * {
     * teacherCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
     * }
     *
     * BeanQuery staffQuery = new BeanQuery(SisStaff.class, teacherCriteria);
     * Map<String, SisStaff> staffMap = getBroker().getMapByQuery(staffQuery, X2BaseBean.COL_OID,
     * 128);
     *
     *
     * SubQuery staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, teacherCriteria);
     * X2Criteria attendanceCriteria = new X2Criteria();
     * attendanceCriteria.addIn(StaffAttendance.COL_STAFF_OID, staffSubQuery);
     * attendanceCriteria.addEqualTo(StaffAttendance.COL_CODE, "A");
     * BeanQuery attendanceQuery = new BeanQuery(StaffAttendance.class, attendanceCriteria);
     *
     * Map<String, Collection<StaffAttendance>> attendanceMap;
     * attendanceMap = getBroker().getGroupedCollectionByQuery(attendanceQuery,
     * StaffAttendance.COL_STAFF_OID, 32);
     *
     * int totalFte = 0;
     * for (Entry<String, Collection<StaffAttendance>> entry : attendanceMap.entrySet())
     * {
     * Collection<StaffAttendance> attendances = entry.getValue();
     * double staffAbsentCount = 0.0;
     * for (StaffAttendance attendance : attendances)
     * {
     * BigDecimal timeAbsent = attendance.getTimeAbsent();
     * if (timeAbsent != null)
     * {
     * // use the time absent to increment
     * staffAbsentCount += timeAbsent.doubleValue();
     * }
     * else
     * {
     * // otherwise assume staff was absent for one day
     * totalFte += 1.0;
     * }
     * }
     *
     * String staffOid = entry.getKey();
     * if (staffAbsentCount > 10)
     * {
     * SisStaff staff = staffMap.get(staffOid);
     * String fteAsString = (String) staff.getFieldValueByAlias(stfFte);
     * if (StringUtils.isNumeric(fteAsString))
     * {
     * totalFte += Double.parseDouble(fteAsString);
     * }
     * }
     * }
     *
     * grid.append();
     * grid.set("Category", "FTE of teachers who were absent more than 10 days of the school year");
     * grid.set("FTE", Double.valueOf(totalFte));
     * }
     *
     * return grid;
     * }
     */

    /**
     * Does the {@code student} have a {@code Transcript} record with a course subject area
     * code in {@code courseNumbers} and received a final grade that earns credit?
     *
     * @param courseNumbers list of 5-digit SCED codes
     */
    private boolean passedCourse(SisStudent student, String[] courseSCEDCodeGroup) {
        boolean passedCourse = false;
        String scedCourseCodeFieldName = m_crdcHelper.getScedCourseCodeFieldName();
        String scedCourseCodeOverrideFieldName = m_crdcHelper.getScedCourseCodeOverrideFieldName();

        if (!StringUtils.isEmpty(scedCourseCodeFieldName)) {
            List<Transcript> transcripts = m_helper.getStudentTranscripts(student);

            if (transcripts != null && !transcripts.isEmpty()) {
                for (Transcript transcript : transcripts) {
                    SchoolCourse schoolCourse = transcript.getSchoolCourse();
                    if (schoolCourse != null) {
                        Course course = schoolCourse.getCourse();

                        if (course != null) {
                            String courseSCEDCode = (String) course.getFieldValueByBeanPath(scedCourseCodeFieldName);

                            // If SCED Code Override is present then use it.
                            String courseSCEDOverrideCode = null;
                            if (scedCourseCodeOverrideFieldName != null) {
                                courseSCEDOverrideCode =
                                        (String) course.getFieldValueByBeanPath(scedCourseCodeOverrideFieldName);

                                if (!StringUtils.isEmpty(courseSCEDOverrideCode)) {
                                    courseSCEDCode = courseSCEDOverrideCode;
                                }
                            }

                            if (!StringUtils.isEmpty(courseSCEDCode)) {
                                courseSCEDCode = StringUtils.padRight(courseSCEDCode, 5);

                                if (CrdcHelper.arrayContains(courseSCEDCodeGroup, courseSCEDCode)) {
                                    // For Mukiteo, the FinalGrade can be pulled form different
                                    // fields in the Transcript, depending on the Term Grade.
                                    String termCode = getTranscriptTermCode(transcript);

                                    String finalGrade = getGrade(transcript, termCode);

                                    if (finalGrade.isEmpty()) {
                                        finalGrade = transcript.getFinalGrade();
                                    }

                                    if (finalGrade != null) {
                                        String grade = finalGrade.trim();

                                        GradeScale gradeScale =
                                                m_gradeScaleMap.get(transcript.getTranscriptDefinitionOid());

                                        // if grade is numeric, get the letter value of it
                                        if (StringUtils.isNumeric(grade)) {
                                            grade = m_gradesManager.getLetterValue(new BigDecimal(grade), gradeScale,
                                                    transcript.getSchool(), transcript.getSchoolCourseOid());
                                        }

                                        GradeScaleGradeDefinition gradeDefinition =
                                                m_gradesManager.getGradeDefinition(grade, gradeScale,
                                                        transcript.getSchoolOid(),
                                                        transcript.getSchoolCourseOid());

                                        if (gradeDefinition != null) {
                                            if (gradeDefinition.getCreditIndicator()) {
                                                passedCourse = true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return passedCourse;
    }

    /**
     * Translates a list of ReferenceCode oids to a list of their actual codes.
     *
     * @param inputField String
     * @return List<String> - reference codes (RCD_CODE)
     */
    private List<String> translateRcdOidsToCodes(String inputField) {
        List<String> rcdOids = StringUtils.convertDelimitedStringToList(inputField, ',');

        return (List<String>) ReferenceManager.getCodes(rcdOids, getBroker());
    }

    /**
     * Civil Rights Data Collection Helper class
     * <p>
     * Helper class for Civil Rights Data Collection exports. You need to use
     * {@link #setPersonRaceMap(Map)}
     * and {@link #setReportDate(PlainDate)} to utilize this class.
     *
     * @author Follett Software Company
     *
     */
    public static class CrdcHelper {

        /**
         * Category codes.
         */
        public enum Category {
            HISPANIC {
                @Override
                public String toString() {
                    return "Hispanic or Latino of any race";
                }
            },
            NATIVE_AMERICAN {
                @Override
                public String toString() {
                    return "American Indian or Alaska Native";
                }
            },
            ASIAN {
                @Override
                public String toString() {
                    return "Asian";
                }
            },
            PACIFIC_ISLANDER {
                @Override
                public String toString() {
                    return "Native Hawaiian or Other Pacific Islander";
                }
            },
            AFRICAN_AMERICAN {
                @Override
                public String toString() {
                    return "Black or African American";
                }
            },
            CAUCASIAN {
                @Override
                public String toString() {
                    return "White";
                }
            },
            UNKNOWN {
                @Override
                public String toString() {
                    return "Unknown race";
                }
            },
            MULTI {
                @Override
                public String toString() {
                    return "Two or more races";
                }
            },
            IDEA {
                @Override
                public String toString() {
                    return "Students with Disabilities (IDEA)";
                }
            },
            LEP {
                @Override
                public String toString() {
                    // Limited English Proficiency
                    return "LEP";
                }
            },
            SECTION_504 {
                @Override
                public String toString() {
                    return "Section 504 Only";
                }
            }
        }

        /**
         * Discipline a student can receive.
         */
        public enum Discipline {
            CORPORAL_PUNISHMENT, IN_SCHOOL_SUSPENSION, OUT_OF_SHCOOL_SUSPENSION, EXPULSION_WITH_EDUCATION_SERVICES, EXPULSION_WITHOUT_EDUCATION_SERVICES, EXPULSION_UNDER_ZERO_TOLERANCE, REFERRAL_TO_LAW_ENFORCEMENT, SCHOOL_RELATED_ARREST
        }

        /**
         * Incident Basis.
         */
        public enum IncidentType {
            BASED_ON_DISABILITY, BASED_ON_RACE, BASED_ON_SEX, OTHER
        }

        /**
         * List of SCED course codes for Advanced Mathematics courses
         */
        public static final String[] ADVANCED_MATH_COURSES = new String[] {
                "02057", "02073", "02075", "02101", "02102", "02103", "02104",
                "02105", "02106", "02107", "02108", "02109", "02110", "02111",
                "02112", "02113", "02141", "02149", "02201", "02202", "02204",
                "02209", "02124", "02125", "02203", "02055", "02074", "10157"
        };

        /**
         * List of SCED course codes for Algebra I courses
         */
        public static final String[] ALGEBRA_1_COURSES = new String[] {
                "02052", "02053", "02054", "52052", "02052CC"
        };

        /**
         * List of SCED course codes for Algebra II courses
         */
        public static final String[] ALGEBRA_2_COURSES = new String[] {
                "02056", "02106"
        };

        /**
         * List of SCED course codes for AP Math courses
         */
        public static final String[] AP_MATH = new String[] {
                "02124", "02125", "02203", "02204"
        };

        /**
         * List of SCED course codes for AP Science courses
         */
        public static final String[] AP_SCIENCE = new String[] {
                "03056", "03106", "03155", "03156", "03207", "03151"
        };

        /**
         * List of SCED course codes for AP Other courses
         * ****Separate check**** leaving null for logic comparison
         */
        // ??? Was commented out
        // public static final String[] AP_OTHER = null;
        public static final String[] AP_OTHER = new String[] {
                "01005", "01006", "04004", "04056", "04057", "04104", "04157",
                "04158", "04159", "04203", "04204", "04205", "04256", "05114",
                "05153", "05171", "05172", "06112", "06113", "06132", "06133",
                "06212", "06313", "10157", "10158",
                "04052", "04101", "05117", "06148",
        };

        /**
         * List of All AP SCED course codes.
         */
        public static final String[] AP_ALL = new String[] {
                "02124", "02125", "02203", "02204",
                "03056", "03106", "03155", "03156", "03207", "03151",
                "01005", "01006", "04004", "04056", "04057", "04104", "04157",
                "04158", "04159", "04203", "04204", "04205", "04256", "05114",
                "05153", "05171", "05172", "06112", "06113", "06132", "06133",
                "06212", "06313", "10157", "10158",
                "04052", "04101", "05117", "06148",
        };

        /**
         * List of SCED course codes for Biology courses
         */
        public static final String[] BIOLOGY_COURSES = new String[] {
                "03051", "03052"
        };

        /**
         * List of SCED course codes for Calculus courses
         */
        public static final String[] CALCULUS_COURSES = new String[] {
                "02121", "02122", "02123", "02125", "02124"
        };

        /**
         * List of SCED course codes for Chemistry courses
         */
        public static final String[] CHEMISTRY_COURSES = new String[] {
                "03101", "03102", "03103", "03104", "03105", "03108"
        };

        /**
         * List of SCED course codes for Geometry courses
         */
        public static final String[] GEOMETRY_COURSES = new String[] {
                "52072", "02072"
        };

        /**
         * List of SCED course codes for International Baccalaureate (IB) Diploma programs
         */
        public static final String[] IB_COURSES = new String[] {
                "01007", "02131", "02132", "02133", "02134", "03057", "03107",
                "03157", "03160", "03206", "03208", "04003", "04054", "04066",
                "04206", "04253", "04257", "04262", "04304", "04309", "05115",
                "05173", "06110", "06111", "06130", "06131", "06150", "06151",
                "06170", "06171", "06190", "06191", "06210", "06211", "06250",
                "06251", "06270", "06271", "06290", "06291", "06311", "06331",
                "06410", "06411", "06430", "06431", "06450", "06451", "06490",
                "06491", "06510", "06511", "06530", "06531", "06590", "06591",
                "06610", "06611", "06650", "06651", "06670", "06671", "06690",
                "06691", "06710", "06711", "06712", "06730", "06731", "06732",
                "06770", "06771", "06790", "06791", "06830", "06831", "06850",
                "06851", "06870", "06871", "10007", "10159", "12059", "51007",
                "52132", "53203", "54171", "55202", "56101", "56121", "56141",
                "56161", "56201", "56281", "56401", "56421", "56441", "56501",
                "56521", "56601", "56701", "56721", "56761", "56801", "56821",
                "58040", "71052", "72260", "73041"
        };

        /**
         * List of SCED course codes for Physics courses
         */
        public static final String[] PHYSICS_COURSES = new String[] {
                "03151", "03152"
        };

        /**
         * Category
         */
        public static final String CATEGORY = "Category";

        /**
         * Male code
         */
        public static final String MALE = "Male";

        /**
         * Female code
         */
        public static final String FEMALE = "Female";

        /**
         * Does the {@code array} contain {@code value}?.
         *
         * @param array String[]
         * @param value String
         * @return true, if successful
         */
        public static boolean arrayContains(String[] array, String value) {
            boolean contains = false;

            if (value != null) {
                value = value.trim();
            }

            for (int i = 0; i < array.length; i++) {
                if (array[i].equals(value)) {
                    contains = true;
                    break;
                }
            }
            return contains;
        }

        /**
         * Translate a String to a boolean. Case insensitive.
         * <p>
         * Example:
         *
         * <pre>
         * +------------+-------------+
         * | s | result |
         * +------------+-------------+
         * | Y | true |
         * | Yes | true |
         * | 1 | true |
         * +------------+-------------+
         * </pre>
         *
         * @param s String
         * @return true, if successful
         */
        public static boolean logical(String s) {
            return (s != null) && s.matches("(?i)y(es)?|0*1|" + TRUE);
        }

        /**
         * Creates a generic data grid used by CRDC:
         *
         * <pre>
         * +-------------------------------------------+------+--------+
         * | Category | Male | Female |
         * +-------------------------------------------+------+--------+
         * | Hispanic or Latino of any race | 0 | 0 |
         * | American Indian or Alaska Native | 0 | 0 |
         * | Asian | 0 | 0 |
         * | Native Hawaiian or Other Pacific Islander | 0 | 0 |
         * | Black or African American | 0 | 0 |
         * | White | 0 | 0 |
         * | Unknown race | 0 | 0 |
         * | Two or more races | 0 | 0 |
         * | Students with Disabilities (IDEA) | 0 | 0 |
         * | LEP | 0 | 0 |
         * | Section 504 Only | 0 | 0 |
         * +-------------------------------------------+------+--------+
         * </pre>
         *
         * .
         *
         * @return DataGrid
         */
        public static DataGrid generateDataGrid() {
            DataGrid grid = new DataGrid();
            for (Category category : Category.values()) {
                grid.append();
                grid.set(CrdcHelper.CATEGORY, category);
                grid.set(CrdcHelper.MALE, Integer.valueOf(0));
                grid.set(CrdcHelper.FEMALE, Integer.valueOf(0));
            }
            return grid;
        }

        /**
         * Map of action codes by discipline
         */
        private EnumMap<Discipline, Collection<String>> m_actionCodes;

        /**
         * StateReportData class from CRDC export
         */
        private StateReportData m_data;

        /**
         * Map of student's conduct actions by discipline
         */
        private EnumMap<Discipline, Map<String, Collection<ConductAction>>> m_discipline;

        /**
         * State code value for female
         */
        private String m_femaleStateCode;

        /**
         * Field on the COURSE table that identifies what kind of class it is
         */
        private String m_fieldCourseScedCode;

        private String m_fieldCourseScedExclude;

        private String m_fieldCourseScedCodeOverride;

        /**
         * Field that would be on the STAFF table if state were to use that
         */
        private String m_fieldStfFte;

        /**
         * Map of LEP program by student oids
         */
        private Map<String, Collection<StudentProgramParticipation>> m_lepProgramMap;

        /**
         * Map of SPED program by student oids
         */
        private Map<String, Collection<StudentProgramParticipation>> m_spedProgramMap;

        /**
         * State code value for male
         */
        private String m_maleStateCode;

        /**
         * Map of person's races
         */
        private Map<String, Collection<Race>> m_personRaceMap;

        /**
         * Set of student OIDs that have a 504 Student Ed. Plan that is "Active" or
         * that the report date is between the effective date and end date
         */
        private Set<String> m_plansSet;

        /**
         * Map of race state codes
         */
        private Map<Category, String> m_raceCodes;

        /**
         * The report date
         */
        private PlainDate m_reportDate;

        /**
         * The start date (used to query program participations)
         */
        private PlainDate m_startDate;

        /**
         * State abbreviation
         */
        String m_state;

        /**
         * Constructor.
         *
         * @param data StateReportData
         */
        public CrdcHelper(StateReportData data) {
            if (data == null) {
                throw new X2RuntimeException();
            }

            m_data = data;
            m_state = PreferenceManager.getPreferenceValue(data.getOrganization(), SystemPreferenceDefinition.STATE);
            m_startDate = m_data.getOrganization().getCurrentContext().getStartDate();

            m_actionCodes = new EnumMap<Discipline, Collection<String>>(Discipline.class);
            m_discipline = new EnumMap<Discipline, Map<String, Collection<ConductAction>>>(Discipline.class);
            for (Discipline discipline : Discipline.values()) {
                m_discipline.put(discipline, new HashMap<String, Collection<ConductAction>>());
                m_actionCodes.put(discipline, new ArrayList<String>());
            }

            loadGenderCodes();
            loadRaceCodes();
            loadLepPrograms();
            loadSpedPrograms();
            loadEdPlans();
        }

        /**
         * Get the student's gender
         * <p>
         * If the student's person's gender code equals the state's male code, that student is
         * {@value #MALE}.
         * <p>
         * If the student's person's gender code equals the state's female code, that student is
         * {@value #FEMALE}.
         *
         * @param student SisStudent
         * @return String
         */
        public String getGender(SisStudent student) {
            String genderCodeAsString = student.getPerson().getGenderCode();
            String genderState = m_data.lookupStateValue(Person.class, Person.COL_GENDER_CODE, genderCodeAsString);
            String result = null;
            if (m_maleStateCode.equals(genderState)) {
                result = MALE;
            } else if (m_femaleStateCode.equals(genderState)) {
                result = FEMALE;
            }

            return result;
        }

        /**
         * Returns the javaName for a 'retained' field on the STUDENT table if it exists for the
         * state
         * <p>
         * Returns an empty string if the state doesn't have one.
         *
         * @return String
         */
        public String getRetainedCode() {
            String retainedCode = "";
            if (m_state.equals(STATE_CODE_GA)) {
                retainedCode = m_data.translateAliasToJavaName(ALIAS_GA_CRDC_RETAINED, true);
            } else if (m_state.equals(STATE_CODE_VA)) {
                retainedCode = m_data.translateAliasToJavaName(ALIAS_VA_CRDC_RETAINED, true);
            } else {
                retainedCode = m_data.translateAliasToJavaName(ALIAS_DEFAULT_CRDC_RETAINED, false);
            }

            return retainedCode;
        }

        /**
         * Get the java name for the SCED course field on the COURSE table.
         *
         * @return String
         */
        public String getScedCourseCodeFieldName() {
            if (m_fieldCourseScedCode == null) {
                if (m_state.equals(STATE_CODE_CT)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_CT_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_IL)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_IL_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_MA)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_MA_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_MD)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_MD_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_NY)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_NY_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_RI)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_RI_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_VA)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_VA_CRDC_SCED_CODE, true);
                } else if (m_state.equals(STATE_CODE_WA)) {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_WA_CRDC_SCED_CODE, true);
                } else {
                    m_fieldCourseScedCode = m_data.translateAliasToJavaName(ALIAS_DEFAULT_CRDC_SCED_CODE, false);
                }
            }

            return m_fieldCourseScedCode;
        }

        /**
         * Get the java name for the SCED course exclude field on the COURSE table.
         *
         * @return String
         */
        public String getScedCourseCodeOverrideFieldName() {
            if (m_fieldCourseScedCodeOverride == null) {
                if (m_state.equals(STATE_CODE_NY)) {
                    m_fieldCourseScedCodeOverride = m_data.translateAliasToJavaName(ALIAS_NY_CRDC_OVERRIDE, false);
                } else {
                    m_fieldCourseScedCodeOverride = m_data.translateAliasToJavaName(ALIAS_DEFAULT_CRDC_OVERRIDE, false);
                }
            }

            return m_fieldCourseScedCodeOverride;
        }

        /**
         * Get the java name for the SCED course exclude field on the COURSE table.
         *
         * @return String
         */
        public String getScedCourseExcludeFieldName() {
            if (m_fieldCourseScedExclude == null) {
                if (m_state.equals(STATE_CODE_NY)) {
                    m_fieldCourseScedExclude = m_data.translateAliasToJavaName(ALIAS_NY_CRDC_EXCLUDE_IND, false);
                } else {
                    m_fieldCourseScedExclude = m_data.translateAliasToJavaName(ALIAS_DEFAULT_CRDC_EXCLUDE_IND, false);
                }
            }

            return m_fieldCourseScedExclude;
        }

        /**
         * Get the java name for the Staff FTE field on the STAFF table.
         *
         * @return String
         */
        public String getStaffFteCode() {
            if (m_fieldStfFte == null) {
                if (m_state.equals(STATE_CODE_MA)) {
                    m_fieldStfFte = m_data.translateAliasToJavaName(ALIAS_MA_CRDC_OVERALL_FTE, true);
                } else if (m_state.equals(STATE_CODE_VA)) {
                    m_fieldStfFte = m_data.translateAliasToJavaName(ALIAS_VA_CRDC_TEACHER_FTE, true);
                } else {
                    m_fieldStfFte = "";
                }
            }

            return m_fieldStfFte;
        }

        /**
         * Get the student's race in {@link Category} form
         * <p>
         * This method will check the {@code student}'s race in this order:
         * <ol>
         * <li>If the student's hispanic/latino indicator is checked, return {@link
         * Category#HISPANIC HISPANIC}</li>
         * <li>If the student has 2 or more {@code Race} records, return {@link Category#MULTI
         * MULTI}</li>
         * <li>If the student has 0 {@code Race} records, return {@link Category#UNKNOWN
         * UNKNOWN}</li>
         * <li>If the student's {@code Race}'s state value matches one of the {@code m_raceState***}
         * fields,
         * return the corresponding race's Category</li>
         * </ol>
         * .
         *
         * @param student SisStudent
         * @return Category
         */
        public Category getStudentRace(SisStudent student) {
            if (m_personRaceMap == null) {
                throw new X2RuntimeException();
            }

            Category race = null;
            if (student.getPerson().getHispanicLatinoIndicator()) {
                race = Category.HISPANIC;
            } else {
                Collection<Race> races = m_personRaceMap.get(student.getPersonOid());
                if (races != null) {
                    int caucasionCount = 0;
                    int africanAmericanCount = 0;
                    int asianCount = 0;
                    int nativeAmericanCount = 0;
                    int pacificIslandCount = 0;

                    for (Race pRaces : races) {
                        race = Category.UNKNOWN;

                        String raceCode = pRaces.getRaceCode();
                        if (raceCode != null) {
                            String raceCodeStateValue =
                                    m_data.lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
                            // Some customer sites have the leading zero and some don't so we will
                            // have to deal with it either way.
                            // If there is a leading zero in the state code then remove it.
                            if (raceCodeStateValue != null) {
                                if (raceCodeStateValue.length() > 1 && raceCodeStateValue.startsWith("0")) {
                                    raceCodeStateValue = raceCodeStateValue.substring(1);
                                }

                                if (StringUtils.isEqual(raceCodeStateValue, m_raceCodes.get(Category.CAUCASIAN))) {
                                    race = Category.CAUCASIAN;
                                    caucasionCount++;
                                } else if (StringUtils.isEqual(raceCodeStateValue,
                                        m_raceCodes.get(Category.AFRICAN_AMERICAN))) {
                                    race = Category.AFRICAN_AMERICAN;
                                    africanAmericanCount++;
                                } else if (StringUtils.isEqual(raceCodeStateValue, m_raceCodes.get(Category.ASIAN))) {
                                    race = Category.ASIAN;
                                    asianCount++;
                                } else if (StringUtils.isEqual(raceCodeStateValue,
                                        m_raceCodes.get(Category.NATIVE_AMERICAN))) {
                                    race = Category.NATIVE_AMERICAN;
                                    nativeAmericanCount++;
                                } else if (StringUtils.isEqual(raceCodeStateValue,
                                        m_raceCodes.get(Category.PACIFIC_ISLANDER))) {
                                    race = Category.PACIFIC_ISLANDER;
                                    pacificIslandCount++;
                                } else {
                                    race = Category.UNKNOWN;
                                }
                            }
                        }
                    }

                    int raceCount = 0;
                    if (caucasionCount > 0) {
                        raceCount++;
                    }
                    if (africanAmericanCount > 0) {
                        raceCount++;
                    }
                    if (asianCount > 0) {
                        raceCount++;
                    }
                    if (nativeAmericanCount > 0) {
                        raceCount++;
                    }
                    if (pacificIslandCount > 0) {
                        raceCount++;
                    }

                    if (raceCount > 1) {
                        race = Category.MULTI;
                    }
                } else {
                    race = Category.UNKNOWN;
                }
            }

            return race;
        }

        /**
         * Does the {@code student} have a conduct action related to a {@code discipline}?
         * <p>
         * This only checks if there exists a conduct action, and it does not count them.
         * <p>
         * Note: This only applies to disciplines NOT in-school suspension or out-of-school
         * suspension
         *
         * @param student SisStudent
         * @param discipline Discipline
         * @return true, if successful
         */
        public boolean hasConductAction(SisStudent student, Discipline discipline) {
            boolean hasConductAction = false;
            Map<String, Collection<ConductAction>> studentActionMap = m_discipline.get(discipline);
            Collection<ConductAction> conductActions = studentActionMap.get(student.getOid());
            if (conductActions != null && conductActions.size() > 0) {
                Collection<String> disciplineCodes = m_actionCodes.get(discipline);

                // override: for georgia or Massachusetts and discipline == expulsion without ed
                // services, go through all the expulsion
                // actions and make sure all of them DON'T have this field true
                if (m_state.matches(STATE_CODE_GA + "|" + STATE_CODE_MA)
                        && discipline == Discipline.EXPULSION_WITHOUT_EDUCATION_SERVICES) {
                    for (ConductAction action : conductActions) {
                        String eduSvcField;
                        if (m_state.equals(STATE_CODE_GA)) {
                            eduSvcField =
                                    (String) action.getFieldValueByAlias(ALIAS_GA_CRDC_EXPULSION_WITHOUT_EDUCATION);
                            hasConductAction = logical(eduSvcField);
                        } else if (m_state.equals(STATE_CODE_MA)) {
                            eduSvcField =
                                    (String) action.getFieldValueByAlias(ALIAS_MA_CRDC_EXPULSION_WITHOUT_EDUCATION);
                            if (!StringUtils.isEmpty(eduSvcField)) {
                                String stateCode =
                                        m_data.lookupReferenceCodeByAlias(ALIAS_MA_CRDC_EXPULSION_WITHOUT_EDUCATION,
                                                eduSvcField,
                                                ReferenceMapTypeCode.STATE.ordinal());
                                hasConductAction = !"Y".equals(stateCode);
                            }
                        }

                        if (!hasConductAction) {
                            break;
                        }
                    }
                } else {
                    for (ConductAction action : conductActions) {

                        // override: for connecticut and new hampshire, there's an 'arrested' field
                        // right on the conduct action/incident
                        if (m_state.matches(STATE_CODE_CT + "|" + STATE_CODE_NH)
                                && discipline == Discipline.SCHOOL_RELATED_ARREST) {
                            String arrestedField = "";
                            if (m_state.equals(STATE_CODE_CT)) {
                                arrestedField = (String) action.getFieldValueByAlias(ALIAS_CT_CRDC_ARRESTED_IND);
                            } else if (m_state.equals(STATE_CODE_NH)) {
                                ConductIncident incident = action.getIncident();
                                arrestedField = (String) incident.getFieldValueByAlias(ALIAS_NH_CRDC_ARRESTED_IND);
                            }
                            hasConductAction = logical(arrestedField);
                            if (hasConductAction) {
                                break;
                            }
                        }

                        // override: for Virginia and New Hampshire, there's a "reported to law" on
                        // the conduct incident
                        else if (m_state.matches(STATE_CODE_VA + "|" + STATE_CODE_NH)
                                && discipline == Discipline.REFERRAL_TO_LAW_ENFORCEMENT) {
                            ConductIncident incident = action.getIncident();
                            if (incident != null) {
                                String reportedField =
                                        (String) incident.getFieldValueByAlias(ALIAS_VA_CRDC_REFERRAL_TO_LAW);
                                hasConductAction = logical(reportedField);
                                if (hasConductAction) {
                                    break;
                                }
                            }
                        }

                        // override: for georgia or Massachusetts, there's a continuing education on
                        // conduct action
                        else if (m_state.matches(STATE_CODE_GA + "|" + STATE_CODE_MA)
                                && discipline == Discipline.EXPULSION_WITH_EDUCATION_SERVICES) {
                            String eduSvcField;
                            if (m_state.equals(STATE_CODE_GA)) {
                                eduSvcField =
                                        (String) action.getFieldValueByAlias(ALIAS_GA_CRDC_EXPULSION_WITH_EDUCATION);
                                hasConductAction = logical(eduSvcField);
                            } else if (m_state.equals(STATE_CODE_MA)) {
                                eduSvcField =
                                        (String) action.getFieldValueByAlias(ALIAS_MA_CRDC_EXPULSION_WITH_EDUCATION);
                                if (!StringUtils.isEmpty(eduSvcField)) {
                                    String stateCode =
                                            m_data.lookupReferenceCodeByAlias(ALIAS_MA_CRDC_EXPULSION_WITH_EDUCATION,
                                                    eduSvcField,
                                                    ReferenceMapTypeCode.STATE.ordinal());
                                    hasConductAction = "Y".equals(stateCode);
                                }
                            }

                            if (hasConductAction) {
                                break;
                            }
                        }

                        // everyone else uses an action code
                        else {
                            String codeFromAction = action.getActionCode();
                            hasConductAction = disciplineCodes.contains(codeFromAction);
                            if (hasConductAction) {
                                break;
                            }
                        }
                    }
                }
            }

            return hasConductAction;
        }

        /**
         * Add a student to a standard grid.
         * <p>
         * Checks the student's race, IDEA status, Section 504 status, and LEP
         *
         * @param grid DataGrid
         * @param student SisStudent
         */
        public void includeStudent(DataGrid grid, SisStudent student) {
            // race
            Category category = getStudentRace(student);
            addStudentToCategory(grid, category, student);

            // Students with Disabilities (IDEA)
            if (isSped(student)) {
                addStudentToCategory(grid, Category.IDEA, student);
            }

            // Section 504
            if (isSection504(student)) {
                addStudentToCategory(grid, Category.SECTION_504, student);
            }

            // LEP
            if (isConsideredLep(student)) {
                addStudentToCategory(grid, Category.LEP, student);
            }
        }

        /**
         * Is the {@code student} considered an Limited English Proficiency student?
         * <p>
         * Note: not to be confused with {@link #isInLepProgram} which checks if
         * the {@code student} is <em>in</em> an LEP program.
         *
         * @param student SisStudent
         * @return true, if is considered lep
         */
        public boolean isConsideredLep(SisStudent student) {
            boolean isConsideredLep = false;

            if (m_state.equals(STATE_CODE_CT)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_CT_CRDC_LEP_IND);
                isConsideredLep = logical(lepAsString);
            } else if (m_state.equals(STATE_CODE_GA)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_GA_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = m_data.lookupReferenceCodeByAlias(ALIAS_GA_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    isConsideredLep = "Y".equals(stateCode);
                }
            } else if (m_state.equals(STATE_CODE_IL)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_IL_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = m_data.lookupReferenceCodeByAlias(ALIAS_IL_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(stateCode)) {
                        isConsideredLep = stateCode.matches("10|11|12|13");
                    }
                }
            } else if (m_state.equals(STATE_CODE_MA)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_MA_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = m_data.lookupReferenceCodeByAlias(ALIAS_MA_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    isConsideredLep = "01".equals(stateCode);
                }
            } else if (m_state.equals(STATE_CODE_MD)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_MD_CRDC_LEP_IND);
                isConsideredLep = logical(lepAsString);
            } else if (m_state.equals(STATE_CODE_NH)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_DEFAULT_CRDC_LEP_IND);
                if (!StringUtils.isEmpty(lepAsString)) {
                    String stateCode = m_data.lookupReferenceCodeByAlias(ALIAS_DEFAULT_CRDC_LEP_IND,
                            lepAsString,
                            ReferenceMapTypeCode.STATE.ordinal());
                    isConsideredLep = "Y".equals(stateCode);
                }
            } else if (m_state.equals(STATE_CODE_NY)) {
                if (m_lepProgramMap != null) {
                    Collection<StudentProgramParticipation> programs = m_lepProgramMap.get(student.getOid());

                    if (programs != null && programs.size() > 0) {
                        for (StudentProgramParticipation program : programs) {
                            String programCode = program.getProgramCode();
                            PlainDate stateDate = program.getStartDate();
                            PlainDate endDate = program.getEndDate();

                            // If the student has any 0231 programs codes he/she is considered to be
                            // LEP Eligible
                            if (("0231".equals(programCode))
                                    && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                    && (endDate == null
                                            || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                                isConsideredLep = true;
                                break;
                            }
                        }
                    }
                }
            } else if (m_state.equals(STATE_CODE_RI)) {
                String lepAsString = (String) student.getFieldValueByAlias(ALIAS_RI_CRDC_LEP_IND);
                isConsideredLep = logical(lepAsString);
            } else if (m_state.equals(STATE_CODE_WA)) {
                if (m_lepProgramMap != null) {
                    Collection<StudentProgramParticipation> programs = m_lepProgramMap.get(student.getOid());

                    if (programs != null && programs.size() > 0) {
                        for (StudentProgramParticipation program : programs) {
                            String programCode = program.getProgramCode();
                            PlainDate stateDate = program.getStartDate();
                            PlainDate endDate = program.getEndDate();

                            // If the student has any 0231 programs codes he/she is considered to be
                            // LEP Eligible
                            if (("ELL".equals(programCode))
                                    && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                    && (endDate == null
                                            || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                                isConsideredLep = true;
                                break;
                            }
                        }
                    }
                }
            }

            return isConsideredLep;
        }

        /**
         * Is the {@code student} gifted/talented?
         * <p>
         * If there is no state implementation, this is always false.
         *
         * @param student SisStudent
         * @return true, if is gifted
         */
        public boolean isGifted(SisStudent student) {
            boolean isGifted = false;

            if (m_state.equals(STATE_CODE_CT)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_CT_CRDC_GIFTED_IND);
                if (!StringUtils.isEmpty(giftedFieldValue)) {
                    String giftedStateValue = m_data.lookupReferenceCodeByAlias(ALIAS_CT_CRDC_GIFTED_IND,
                            giftedFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(giftedStateValue)) {
                        isGifted = giftedStateValue.matches("0[2-9]"); // 02 through 09
                    }
                }
            } else if (m_state.equals(STATE_CODE_FL)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_FL_CRDC_GIFTED_IND);
                if (!StringUtils.isEmpty(giftedFieldValue)) {
                    String giftedStateValue = m_data.lookupReferenceCodeByAlias(ALIAS_FL_CRDC_GIFTED_IND,
                            giftedFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(giftedStateValue)) {
                        isGifted = giftedStateValue.matches("A|B");
                    }
                }
            } else if (m_state.equals(STATE_CODE_GA)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_GA_CRDC_GIFTED_IND);
                if (!StringUtils.isEmpty(giftedFieldValue)) {
                    String giftedStateValue = m_data.lookupReferenceCodeByAlias(ALIAS_GA_CRDC_GIFTED_IND,
                            giftedFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(giftedStateValue)) {
                        isGifted = giftedStateValue.matches("[1-6]"); // 1 through 6
                    }
                }
            } else if (m_state.equals(STATE_CODE_MA)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_MA_CRDC_GIFTED_IND);
                isGifted = logical(giftedFieldValue);
            } else if (m_state.equals(STATE_CODE_VA)) {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_VA_CRDC_GIFTED_IND);
                isGifted = logical(giftedFieldValue);
            } else if (m_state.equals(STATE_CODE_WA)) {
                Collection<StudentProgramParticipation> programs = student.getProgramParticipation();

                if (programs != null && programs.size() > 0) {
                    for (StudentProgramParticipation program : programs) {
                        PlainDate stateDate = program.getStartDate();
                        PlainDate endDate = program.getEndDate();

                        // Is currently active
                        if ((stateDate != null)
                                && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                && (endDate == null
                                        || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                            String programCode = program.getProgramCode();

                            String programStateCode =
                                    m_data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                                            StudentProgramParticipation.COL_PROGRAM_CODE,
                                            programCode, ReferenceMapTypeCode.STATE.ordinal());

                            if (programStateCode != null && programStateCode.matches("32|33|34|35")) {
                                isGifted = true;
                                break;
                            }
                        }
                    }
                }
            } else {
                String giftedFieldValue = (String) student.getFieldValueByAlias(ALIAS_DEFAULT_CRDC_GIFTED_IND);
                isGifted = logical(giftedFieldValue);
            }

            return isGifted;
        }

        /**
         * Is the student in a GED program?
         * <p>
         * If there is no state implementation, this is always false.
         *
         * @param student SisStudent
         * @return true, if is in ged
         */
        public boolean isInGed(SisStudent student) {
            boolean isInGed = false;

            if (m_state.equals(STATE_CODE_VA)) {
                String gedFieldValue = (String) student.getFieldValueByAlias(ALIAS_VA_CRDC_GED_IND);
                String gedStateValue = m_data.lookupReferenceCodeByAlias(ALIAS_VA_CRDC_GED_IND,
                        gedFieldValue,
                        ReferenceMapTypeCode.STATE.ordinal());
                if (!StringUtils.isEmpty(gedStateValue)) {
                    isInGed = gedStateValue.matches("1"); // might be 2 (ISAEP) or 3 (GAD)
                }
            } else if (m_state.equals(STATE_CODE_WA)) {
                Collection<StudentEnrollment> enrollments = student.getEnrollments();
                for (StudentEnrollment enrollment : enrollments) {
                    if ("GED (completed)".equalsIgnoreCase(enrollment.getEnrollmentCode())) {
                        isInGed = true;
                    }
                }
            } else {
                isInGed = logical((String) student.getFieldValueByAlias(ALIAS_DEFAULT_CRDC_GED_IND));
            }

            return isInGed;
        }

        /**
         * Is the {@code student} <em>in an</em> LEP program?
         * <p>
         * If there's no state implementation, this is always false.
         *
         * @param student SisStudent
         * @return true, if is in lep program
         * @throws X2RuntimeException if the program participation map is not initialized (if the
         *         state requires it),
         *         call {@link #setLepPrograms(Map)} to fix this
         */
        public boolean isInLepProgram(SisStudent student) {
            boolean isInLepProgram = false;

            if (m_state.equals(STATE_CODE_CT)) {
                String lepProgramFieldValue = (String) student.getFieldValueByAlias(ALIAS_CT_CRDC_LEP_PROGRAM_IND);
                if (!StringUtils.isEmpty(lepProgramFieldValue)) {
                    String lepProgramStateValue = m_data.lookupReferenceCodeByAlias(ALIAS_CT_CRDC_LEP_PROGRAM_IND,
                            lepProgramFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(lepProgramStateValue)) {
                        isInLepProgram = lepProgramStateValue.matches("01|02|03|05|07");
                    }
                }
            } else if (m_state.equals(STATE_CODE_IL)) {
                if (m_lepProgramMap != null) {
                    Collection<StudentProgramParticipation> programs = m_lepProgramMap.get(student.getOid());

                    if (programs != null && programs.size() > 0) {
                        for (StudentProgramParticipation program : programs) {
                            String lepFieldValue = (String) program.getFieldValueByAlias(ALIAS_IL_CRDC_LEP_PROGRAM_IND);
                            if (!StringUtils.isEmpty(lepFieldValue)) {
                                String lepStateValue = m_data.lookupReferenceCodeByAlias(ALIAS_IL_CRDC_LEP_PROGRAM_IND,
                                        lepFieldValue,
                                        ReferenceMapTypeCode.STATE.ordinal());
                                if (!StringUtils.isEmpty(lepStateValue)) {
                                    // isInLepProgram = lepStateValue.matches("01|02|03");
                                    if (lepStateValue.matches("01|02|03")) {
                                        isInLepProgram = true;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            } else if (m_state.equals(STATE_CODE_MA)) {
                String lepProgramFieldValue = (String) student.getFieldValueByAlias(ALIAS_MA_CRDC_LEP_PROGRAM_IND);
                if (!StringUtils.isEmpty(lepProgramFieldValue)) {
                    String lepProgramStateValue = m_data.lookupReferenceCodeByAlias(ALIAS_MA_CRDC_LEP_PROGRAM_IND,
                            lepProgramFieldValue,
                            ReferenceMapTypeCode.STATE.ordinal());
                    if (!StringUtils.isEmpty(lepProgramStateValue)) {
                        isInLepProgram = lepProgramStateValue.matches("01|02|03");
                    }
                }
            } else if (m_state.equals(STATE_CODE_NY)) {
                if (m_lepProgramMap != null) {
                    Collection<StudentProgramParticipation> programs = m_lepProgramMap.get(student.getOid());

                    if (programs != null && programs.size() > 0) {
                        for (StudentProgramParticipation program : programs) {
                            String programCode = program.getProgramCode();
                            PlainDate stateDate = program.getStartDate();
                            PlainDate endDate = program.getEndDate();

                            // If the student has any of the LEP Programs and currently active
                            if (programCode.matches("5676|5687|5698|5709")
                                    && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                    && (endDate == null
                                            || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                                isInLepProgram = true;
                                break;
                            }
                        }
                    }
                }
            } else if (m_state.equals(STATE_CODE_WA)) {
                if (m_lepProgramMap != null) {
                    Collection<StudentProgramParticipation> programs = m_lepProgramMap.get(student.getOid());

                    if (programs != null && programs.size() > 0) {
                        for (StudentProgramParticipation program : programs) {
                            String programCode = program.getProgramCode();
                            PlainDate stateDate = program.getStartDate();
                            PlainDate endDate = program.getEndDate();

                            // If the student has any of the LEP Programs and currently active
                            if (programCode.matches("ELL")
                                    && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                    && (endDate == null
                                            || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                                isInLepProgram = true;
                                break;
                            }
                        }
                    }
                }
            } else {
                // for every other state, fall back to using "is considered an LEP"
                isInLepProgram = isConsideredLep(student);
            }

            return isInLepProgram;
        }

        /**
         * Check for the A student is counted as Section 504 if the following is true:
         * <p>
         * <ol>
         * <li>If the student's section 504 status is {@link Section504StatusCode#ACTIVE ACTIVE},
         * then the student is.</li>
         * <li>If the student's section 504 status is {@link Section504StatusCode#EXITED EXITED}
         * <em>and</em>
         * the student's section 504 last end date is after the report date.</li>
         * </ol>
         *
         * @param student SisStudent
         * @return true, if is section 504
         */
        public boolean isSection504(SisStudent student) {
            if (m_reportDate == null) {
                throw new X2RuntimeException();
            }

            boolean isSection504 = false;

            if (!isSped(student)) // If student is IDEA, then cannot be 504 Only
            {
                // 1. student has a 504 student ed plan that is 'active' and report date is between
                // effective/end date
                if (m_plansSet.contains(student.getOid())) {
                    isSection504 = true;
                } else {
                    // 2. std_504_status = active OR std_504_status = Exited and end date is after
                    // report date
                    Section504StatusCode section504StatusCode = student.getSection504StatusCodeEnum();
                    if (section504StatusCode == Section504StatusCode.ACTIVE) {
                        isSection504 = true;
                    } else if (section504StatusCode == Section504StatusCode.EXITED) {
                        PlainDate section504LastEndDate = student.getSection504LastEndDate();
                        if (section504LastEndDate != null && section504LastEndDate.after(m_reportDate)) {
                            isSection504 = true;
                        }
                    }
                }
            }

            return isSection504;
        }

        /**
         * Is the {@code student}'s sped status {@link SpedStatusCode#ACTIVE ACTIVE}?.
         *
         * @param student SisStudent
         * @return true, if is sped
         */
        public boolean isSped(SisStudent student) {
            if (m_reportDate == null) {
                throw new X2RuntimeException();
            }

            boolean isSped = false;

            if (m_state.equals(STATE_CODE_WA)) {
                if (m_spedProgramMap != null) {
                    Collection<StudentProgramParticipation> programs = m_spedProgramMap.get(student.getOid());

                    if (programs != null && programs.size() > 0) {
                        for (StudentProgramParticipation program : programs) {
                            String programCode = program.getProgramCode();
                            PlainDate stateDate = program.getStartDate();
                            PlainDate endDate = program.getEndDate();

                            // If the student has any of the SPED Programs and currently active
                            if (programCode.matches("SpEd CEDARS")
                                    && (m_reportDate.after(stateDate) || m_reportDate.equals(stateDate))
                                    && (endDate == null
                                            || (m_reportDate.before(endDate) || m_reportDate.equals(endDate)))) {
                                isSped = true;
                                break;
                            }
                        }
                    }
                }
            } else {
                SpedStatusCode spedStatusCode = student.getSpedStatusCodeEnum();
                if (spedStatusCode == SpedStatusCode.ACTIVE) {
                    isSped = true;
                } else if (spedStatusCode == SpedStatusCode.EXITED) {
                    PlainDate spedExitDate = student.getSpedExitDate();
                    if (spedExitDate != null && spedExitDate.after(m_reportDate)) {
                        isSped = true;
                    }
                }
            }

            return isSped;
        }

        /**
         * Set conduct actions for a discipline.
         *
         * @param discipline the type of discipline
         * @param conductActionMap the map of conduct actions by {@code studentOid}
         * @param actionCodes conduct action codes pertaining to the {@code discipline}
         */
        public void setConductActions(Discipline discipline,
                                      Map<String, Collection<ConductAction>> conductActionMap,
                                      Collection<String> actionCodes) {
            if (conductActionMap != null) {
                m_discipline.get(discipline).putAll(conductActionMap);
            }

            if (actionCodes != null) {
                m_actionCodes.get(discipline).addAll(actionCodes);
            }
        }

        /**
         * Set the person-race map for fast retrieval.
         *
         * @param personRaceMap Map<String,Collection<Race>>
         */
        public void setPersonRaceMap(Map<String, Collection<Race>> personRaceMap) {
            m_personRaceMap = personRaceMap;
        }

        /**
         * Set the report date for use checking student's sped, section 504 status.
         *
         * @param reportDate void
         */
        public void setReportDate(PlainDate reportDate) {
            m_reportDate = reportDate;
        }

        /**
         * Set the start date (Optional)
         * <p>
         * By default, it is the district's beginning year.
         *
         * @param startDate void
         */
        public void setStartDate(PlainDate startDate) {
            m_startDate = startDate;
        }

        /**
         * Increments a cell to a grid
         * <p>
         * It assumes the grid is in the following format (which is generated
         * from {@link #generateDataGrid()}):
         * <p>
         *
         * <pre>
         * +-------------------------------------------+------+--------+
         * | Category | Male | Female |
         * +-------------------------------------------+------+--------+
         * | Hispanic or Latino of any race | 0 | 0 |
         * | American Indian or Alaska Native | 0 | 0 |
         * | Asian | 0 | 0 |
         * | Native Hawaiian or Other Pacific Islander | 0 | 0 |
         * | Black or African American | 0 | 0 |
         * | White | 0 | 0 |
         * | Unknown race | 0 | 0 |
         * | Two or more races | 0 | 0 |
         * | Students with Disabilities (IDEA) | 0 | 0 |
         * | LEP | 0 | 0 |
         * | Section 504 Only | 0 | 0 |
         * +-------------------------------------------+------+--------+
         * </pre>
         *
         * .
         *
         * @param grid DataGrid
         * @param category Category
         * @param student SisStudent
         */
        private void addStudentToCategory(DataGrid grid,
                                          Category category,
                                          SisStudent student) {
            grid.gotoRow(category.ordinal());
            String gender = getGender(student);
            if (!StringUtils.isEmpty(gender)) {
                Integer count = (Integer) grid.get(gender);
                grid.set(gender, Integer.valueOf(count.intValue() + 1));
            }
        }

        /**
         * Load students' ed plans (for section 504 checking).
         */
        private void loadEdPlans() {
            m_plansSet = new HashSet<String>();
            String ddx504 = PreferenceManager.getPreferenceValue(m_data.getOrganization(),
                    SisPreferenceConstants.SECTION_504_DEFAULT_DEFINITION);
            if (!StringUtils.isEmpty(ddx504)) {
                /*
                 * SELECT SEP_STD_OID
                 * FROM STUDENT_ED_PLAN
                 * WHERE SEP_DDX_OID = 'ddxStandard504' AND
                 * (SEP_STATUS = '1' OR
                 * (SEP_EFFECTIVE_DATE <= 'm_reportDate' AND
                 * (SEP_END_DATE >= 'm_reportDate' OR
                 * SEP_END_DATE IS NULL)))
                 */

                X2Criteria planCriteria = new X2Criteria();
                planCriteria.addEqualTo(StudentEdPlan.COL_EXTENDED_DATA_DICTIONARY_OID, ddx504);

                X2Criteria endDate = new X2Criteria();
                endDate.addGreaterOrEqualThan(StudentEdPlan.COL_END_DATE, m_reportDate);
                X2Criteria endDateNullCriteria = new X2Criteria();
                endDateNullCriteria.addIsNull(StudentEdPlan.COL_END_DATE);
                endDate.addOrCriteria(endDateNullCriteria);

                X2Criteria effectiveDate = new X2Criteria();
                effectiveDate.addLessOrEqualThan(StudentEdPlan.COL_EFFECTIVE_DATE, m_reportDate);
                effectiveDate.addAndCriteria(endDate);

                X2Criteria statusCriteria = new X2Criteria();
                statusCriteria.addEqualTo(StudentEdPlan.COL_STATUS_CODE,
                        Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()));
                statusCriteria.addOrCriteria(effectiveDate);
                planCriteria.addAndCriteria(statusCriteria);

                SubQuery subQuery = new SubQuery(StudentEdPlan.class, StudentEdPlan.COL_STUDENT_OID, planCriteria);
                Collection<String> studentOids = m_data.getBroker().getSubQueryCollectionByQuery(subQuery);
                m_plansSet.addAll(studentOids);
            }
        }

        /**
         * Get state-specific gender codes.
         */
        private void loadGenderCodes() {
            /*
             * By default...
             *
             * Male = M
             * Female = F
             */
            m_maleStateCode = REF_CODE_DEFAULT_CRDC_MALE;
            m_femaleStateCode = REF_CODE_DEFAULT_CRDC_FEMALE;

            if (m_state.equals(STATE_CODE_IL)) {
                /*
                 * Male = 01
                 * Female = 02
                 */
                m_maleStateCode = REF_CODE_IL_CRDC_MALE;
                m_femaleStateCode = REF_CODE_IL_CRDC_FEMALE;
            } else if (m_state.equals(STATE_CODE_MD)) {
                m_maleStateCode = REF_CODE_MD_CRDC_MALE;
                m_femaleStateCode = REF_CODE_MD_CRDC_FEMALE;
            }
        }

        /**
         * Load LEP programs (if states have them).
         */
        private void loadLepPrograms() {
            if (m_state.equals(STATE_CODE_IL)) {
                X2Criteria lepCriteria = new X2Criteria();
                lepCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_startDate);
                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
                X2Criteria endDateCriteriaB = new X2Criteria();
                endDateCriteriaB.addNotNull(StudentProgramParticipation.COL_END_DATE);
                endDateCriteria.addOrCriteria(endDateCriteriaB);
                lepCriteria.addAndCriteria(endDateCriteria);

                String ellField = m_data.translateAliasToJavaName(ALIAS_IL_CRDC_LEP_PROGRAM_IND, true);
                lepCriteria.addNotEmpty(ellField, m_data.getBroker().getPersistenceKey());

                BeanQuery lepQuery = new BeanQuery(StudentProgramParticipation.class, lepCriteria);
                m_lepProgramMap = m_data.getBroker().getGroupedCollectionByQuery(lepQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, 100);
            } else if (m_state.equals(STATE_CODE_NY)) {
                String NYLepCodes[] = new String[] {"0231", "5676", "5687", "5698", "5709"};
                List NYLepCodeslist = Arrays.asList(NYLepCodes);

                X2Criteria lepCriteria = new X2Criteria();
                lepCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, NYLepCodeslist);
                lepCriteria.addNotNull(StudentProgramParticipation.COL_START_DATE);

                BeanQuery lepQuery = new BeanQuery(StudentProgramParticipation.class, lepCriteria);
                m_lepProgramMap = m_data.getBroker().getGroupedCollectionByQuery(lepQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, 100);
            }
        }

        /**
         * Load Sped programs (if states have them).
         */
        private void loadSpedPrograms() {
            if (m_state.equals(STATE_CODE_WA)) {
                String WALepCodes[] = new String[] {"SpEd CEDARS"};
                List WALepCodeslist = Arrays.asList(WALepCodes);

                X2Criteria lepCriteria = new X2Criteria();
                lepCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, WALepCodeslist);
                lepCriteria.addNotNull(StudentProgramParticipation.COL_START_DATE);

                BeanQuery lepQuery = new BeanQuery(StudentProgramParticipation.class, lepCriteria);
                m_spedProgramMap = m_data.getBroker().getGroupedCollectionByQuery(lepQuery,
                        StudentProgramParticipation.COL_STUDENT_OID, 100);
            }
        }

        /**
         * Load race codes, depending on the states.
         */
        private void loadRaceCodes() {
            m_raceCodes = new HashMap<Category, String>();

            if (m_state.equals(STATE_CODE_FL)) {
                /*
                 * Caucasian = W
                 * African American = B
                 * Asian = A
                 * Native American = I
                 * Pacific Islander = A
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_FL_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_FL_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_FL_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_FL_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_FL_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_GA)) {
                /*
                 * Caucasian = W
                 * African American = B
                 * Asian = S
                 * Native American = I
                 * Pacific Islander = P
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_GA_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_GA_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_GA_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_GA_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_GA_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_IL)) {
                /*
                 * Caucasian = 16
                 * African American = 14
                 * Asian = 13
                 * Native American = 12
                 * Pacific Islander = 15
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_IL_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_IL_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_IL_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_IL_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_IL_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_MD)) {
                /*
                 * Caucasian = 5
                 * African American = 3
                 * Asian = 2
                 * Native American = 1
                 * Pacific Islander = 4
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_MD_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_MD_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_MD_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_MD_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_MD_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_NH)) {
                /*
                 * Caucasian = 5
                 * African American = 4
                 * Asian = 2
                 * Native American = 1
                 * Pacific Islander = 6
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_NH_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_NH_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_NH_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_NH_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_NH_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_NY)) {
                /*
                 * Caucasian = W
                 * African American = B
                 * Asian = A
                 * Native American = I
                 * Pacific Islander = P
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_NY_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_NY_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_NY_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_NY_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_NY_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_RI)) {
                /*
                 * Caucasian = E
                 * African American = C
                 * Asian = B
                 * Native American = A
                 * Pacific Islander = B (same as Asian)
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_RI_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_RI_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_RI_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_RI_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_RI_CRDC_PACIFIC_ISLANDER);
            } else if (m_state.equals(STATE_CODE_WA)) {
                /*
                 * Caucasian = W
                 * African American = B
                 * Asian = A
                 * Native American = I
                 * Pacific Islander = P
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_WA_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_WA_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_WA_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_WA_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_WA_CRDC_PACIFIC_ISLANDER);
            } else // by default, MA and CA uses these
            {
                /*
                 * Caucasian = 1
                 * African American = 2
                 * Asian = 4
                 * Native American = 8
                 * Pacific Islander = 16
                 */
                m_raceCodes.put(Category.CAUCASIAN, REF_CODE_DEFAULT_CRDC_CAUCASIAN);
                m_raceCodes.put(Category.AFRICAN_AMERICAN, REF_CODE_DEFAULT_CRDC_AFRICAN_AMERICAN);
                m_raceCodes.put(Category.ASIAN, REF_CODE_DEFAULT_CRDC_ASIAN);
                m_raceCodes.put(Category.NATIVE_AMERICAN, REF_CODE_DEFAULT_CRDC_NATIVE_AMERICAN);
                m_raceCodes.put(Category.PACIFIC_ISLANDER, REF_CODE_DEFAULT_CRDC_PACIFIC_ISLANDER);
            }
        }
    }

}
