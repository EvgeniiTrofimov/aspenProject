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
package com.x2dev.reports.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.CRDCReportData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class CRDCSchoolSummaryReport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class CRDCSummaryReport extends ReportJavaSourceNet {

    /**
     * The Class DataRow.
     */
    static class DataRow {
        private String m_heading;
        private Boolean m_highSchoolOnly;
        private Boolean m_middleSchoolOnly;
        private String[] m_prefix;
        private String[] m_suffixes;
        private String m_type;

        /**
         * Instantiates a new data row.
         *
         * @param heading String
         * @param type String
         * @param prefix String
         * @param suffixes String[]
         */
        public DataRow(String heading, String type, String prefix, String[] suffixes) {
            this(heading, type, prefix, suffixes, false, false);
        }

        /**
         * Instantiates a new data row.
         *
         * @param heading String
         * @param type String
         * @param prefix String[]
         * @param suffixes String[]
         */
        public DataRow(String heading, String type, String[] prefix, String[] suffixes) {
            this(heading, type, prefix, suffixes, false, false);
        }

        /**
         * Instantiates a new data row.
         *
         * @param heading String
         * @param type String
         * @param prefix String
         * @param suffixes String[]
         * @param highSchoolOnly boolean
         */
        public DataRow(String heading, String type, String prefix, String[] suffixes, boolean highSchoolOnly) {
            this(heading, type, prefix, suffixes, highSchoolOnly, false);
        }

        /**
         * Instantiates a new data row.
         *
         * @param heading String
         * @param type String
         * @param prefix String
         * @param suffixes String[]
         * @param highSchoolOnly boolean
         * @param middleSchoolOnly boolean
         */
        public DataRow(String heading, String type, String prefix, String[] suffixes, boolean highSchoolOnly,
                boolean middleSchoolOnly) {
            this(heading, type, new String[] {prefix}, suffixes, highSchoolOnly, middleSchoolOnly);
        }

        /**
         * Instantiates a new data row.
         *
         * @param heading String
         * @param type String
         * @param prefix String[]
         * @param suffixes String[]
         * @param highSchoolOnly boolean
         * @param middleSchoolOnly boolean
         */
        public DataRow(String heading, String type, String[] prefix, String[] suffixes, boolean highSchoolOnly,
                boolean middleSchoolOnly) {
            m_heading = heading;
            m_highSchoolOnly = highSchoolOnly;
            m_middleSchoolOnly = middleSchoolOnly;
            m_type = type;
            m_prefix = prefix;
            m_suffixes = suffixes;
        }

        /**
         * Evaluate.
         *
         * @param cache ColumnCache
         * @param column String
         * @return BigDecimal
         */
        public BigDecimal evaluate(ColumnCache cache, String column) {
            BigDecimal value = null;
            if (m_suffixes != null && m_suffixes.length > 0) {
                for (String prefix : m_prefix) {
                    for (String suffix : m_suffixes) {
                        String key = prefix + "_" + column + "_" + suffix;
                        String stringValue = cache.getString(key);
                        if (stringValue != null) {
                            BigDecimal testValue = null;
                            try {
                                testValue = new BigDecimal(stringValue);
                            } catch (Exception e) {
                                // cannot be converted
                            }
                            if (testValue != null) {
                                value = value == null ? testValue : value.add(testValue);
                            }
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Gets the heading.
         *
         * @return Object
         */
        public Object getHeading() {
            return m_heading;
        }

        /**
         * Gets the type.
         *
         * @return Object
         */
        public Object getPrefix() {
            return m_prefix;
        }

        /**
         * Gets the type.
         *
         * @return Object
         */
        public Object getType() {
            return m_type;
        }

        /**
         * Checks if is high school only.
         *
         * @return true, if is high school only
         */
        boolean isHighSchoolOnly() {
            return m_highSchoolOnly;
        }

        /**
         * Checks if is middle school only.
         *
         * @return true, if is middle school only
         */
        boolean isMiddleSchoolOnly() {
            return m_middleSchoolOnly;
        }

        /**
         * Total.
         *
         * @param cache ColumnCache
         * @return Object
         */
        public Object total(ColumnCache cache) {
            BigDecimal value = null;
            for (String column : DATA_COLUMNS) {
                BigDecimal testValue = evaluate(cache, column);
                if (testValue != null) {
                    value = value == null ? testValue : value.add(testValue);
                }
            }
            return value;
        }
    }

    private static final String ALIAS_CRDC_REF_CODE = "all-rcd-CRDCCode";
    private static final String ALIAS_NCES_SCHOOL_ID = "all-skl-NCESSchoolID";

    private static final List<String> DATA_COLUMNS =
            Arrays.asList("HI", "AM", "AS", "HP", "BL", "WH", "TR");
    private static final List<DataRow> DATA_ROWS_LEA = Arrays.asList(
            // Distance Ed
            new DataRow("#top", "M", "LEA_DISTEDENR", new String[] {"M"}),
            new DataRow("Distance Ed", "F", "LEA_DISTEDENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "LEA_DISTEDENR", new String[] {"M", "F"}),
            // GED PREP
            new DataRow("#top", "M", "LEA_HSEPART", new String[] {"M"}),
            new DataRow("GED PREP", "F", "LEA_HSEPART", new String[] {"F"}),
            new DataRow("#bottom", "Total", "LEA_HSEPART", new String[] {"M", "F"}));

    private static final List<DataRow> DATA_ROWS_PART_1 = Arrays.asList(
            // Total Enrollment
            new DataRow("#top", "M", "SCH_ENR", new String[] {"M"}),
            new DataRow("Total Student Enrollment", "F", "SCH_ENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_ENR", new String[] {"M", "F"}),
            // PK Enrollment
            new DataRow("#top", "M", "SCH_PSENR", new String[] {"M"}),
            new DataRow("PK", "F", "SCH_PSENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_PSENR", new String[] {"M", "F"}),
            // LEP Enrollment
            new DataRow("#top", "M", "SCH_LEPENR", new String[] {"M"}),
            new DataRow("LEP", "F", "SCH_LEPENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_LEPENR", new String[] {"M", "F"}),
            // LEP Program Enrollment
            new DataRow("#top", "M", "SCH_LEPPROGENR", new String[] {"M"}),
            new DataRow("LEP Program", "F", "SCH_LEPPROGENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_LEPPROGENR", new String[] {"M", "F"}),
            // IDEA Enrollment
            new DataRow("#top", "M", "SCH_IDEAENR", new String[] {"M"}),
            new DataRow("IDEA", "F", "SCH_IDEAENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_IDEAENR", new String[] {"M", "F"}),
            // PK Section 504
            new DataRow("#top", "M", "SCH_504ENR", new String[] {"M"}),
            new DataRow("Section 504", "F", "SCH_504ENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_504ENR", new String[] {"M", "F"}),
            // PK Gifted / Talented
            new DataRow("#top", "M", "SCH_GTENR", new String[] {"M"}),
            new DataRow("Gifted / Talented", "F", "SCH_GTENR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_GTENR", new String[] {"M", "F"}),
            // PK Enrollment
            new DataRow("#top", "M", "SCH_ALGENR_G08", new String[] {"M"}, false, true),
            new DataRow("Algebra 1 Gr 8", "F", "SCH_ALGENR_G08", new String[] {"F"}, false, true),
            new DataRow("#bottom", "Total", "SCH_ALGENR_G08", new String[] {"M", "F"}, false, true),
            // Algebra 1 Gr 9/10
            new DataRow("#top", "M", "SCH_ALGENR_GS0910", new String[] {"M"}, true),
            new DataRow("Algebra 1 Gr 9/10 ", "F", "SCH_ALGENR_GS0910", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_ALGENR_GS0910", new String[] {"M", "F"}, true),
            // Algebra 1 Gr 11/12
            new DataRow("#top", "M", "SCH_ALGENR_GS1112", new String[] {"M"}, true),
            new DataRow("Algebra 1 Gr 11/12 ", "F", "SCH_ALGENR_GS1112", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_ALGENR_GS1112", new String[] {"M", "F"}, true),
            // Geometry
            new DataRow("#top", "M", "SCH_MATHENR_GEOM", new String[] {"M"}, true),
            new DataRow("Geometry", "F", "SCH_MATHENR_GEOM", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_MATHENR_GEOM", new String[] {"M", "F"}, true),
            // Algebra II
            new DataRow("#top", "M", "SCH_MATHENR_ALG2", new String[] {"M"}, true),
            new DataRow("Algebra II", "F", "SCH_MATHENR_ALG2", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_MATHENR_ALG2", new String[] {"M", "F"}, true),
            // Adv Mathematics
            new DataRow("#top", "M", "SCH_MATHENR_ADVM", new String[] {"M"}, true),
            new DataRow("Adv Mathematics", "F", "SCH_MATHENR_ADVM", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_MATHENR_ADVM", new String[] {"M", "F"}, true),
            // Calculus
            new DataRow("#top", "M", "SCH_MATHENR_CALC", new String[] {"M"}, true),
            new DataRow("Calculus", "F", "SCH_MATHENR_CALC", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_MATHENR_CALC", new String[] {"M", "F"}, true),
            // Biology
            new DataRow("#top", "M", "SCH_SCIENR_BIOL", new String[] {"M"}, true),
            new DataRow("Biology", "F", "SCH_SCIENR_BIOL", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_SCIENR_BIOL", new String[] {"M", "F"}, true),
            // Chemistry
            new DataRow("#top", "M", "SCH_SCIENR_CHEM", new String[] {"M"}, true),
            new DataRow("Chemistry", "F", "SCH_SCIENR_CHEM", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_SCIENR_CHEM", new String[] {"M", "F"}, true),
            // Physics
            new DataRow("#top", "M", "SCH_SCIENR_PHYS", new String[] {"M"}, true),
            new DataRow("Physics", "F", "SCH_SCIENR_PHYS", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_SCIENR_PHYS", new String[] {"M", "F"}, true),
            // Computer Science
            new DataRow("#top", "M", "SCH_COMPENR_CSCI", new String[] {"M"}, true),
            new DataRow("Computer Science", "F", "SCH_COMPENR_CSCI", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_COMPENR_CSCI", new String[] {"M", "F"}, true),
            // IB
            new DataRow("#top", "M", "SCH_IBENR", new String[] {"M"}, true),
            new DataRow("IB", "F", "SCH_IBENR", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_IBENR", new String[] {"M", "F"}, true),
            // AP
            new DataRow("#top", "M", "SCH_APENR", new String[] {"M"}, true),
            new DataRow("AP", "F", "SCH_APENR", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_APENR", new String[] {"M", "F"}, true),
            // AP Mathemetics
            new DataRow("#top", "M", "SCH_APMATHENR", new String[] {"M"}, true),
            new DataRow("AP Mathemetics", "F", "SCH_APMATHENR", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_APMATHENR", new String[] {"M", "F"}, true),
            // AP Science
            new DataRow("#top", "M", "SCH_APSCIENR", new String[] {"M"}, true),
            new DataRow("AP Science", "F", "SCH_APSCIENR", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_APSCIENR", new String[] {"M", "F"}, true),
            // AP Other
            new DataRow("#top", "M", "SCH_APOTHENR", new String[] {"M"}, true),
            new DataRow("AP Other", "F", "SCH_APOTHENR", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_APOTHENR", new String[] {"M", "F"}, true),
            // AP Computer Science
            new DataRow("#top", "M", "SCH_APCOMPENR", new String[] {"M"}, true),
            new DataRow("AP Computer Science", "F", "SCH_APCOMPENR", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_APCOMPENR", new String[] {"M", "F"}, true),
            // Dual
            new DataRow("#top", "M", "SCH_DUALENR", new String[] {"M"}, true),
            new DataRow("Dual", "F", "SCH_DUALENR", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_DUALENR", new String[] {"M", "F"}, true));

    private static final List<DataRow> DATA_ROWS_PART_2 = Arrays.asList(
            // Gr 8 Passed Alg 1
            new DataRow("#top", "M", "SCH_ALGPASS_G08", new String[] {"M"}),
            new DataRow("Gr 8 Passed Alg 1", "F", "SCH_ALGPASS_G08", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_ALGPASS_G08", new String[] {"M", "F"}),
            // Gr 9/10 Passed Alg 1
            new DataRow("#top", "M", "SCH_ALGPASS_GS0910", new String[] {"M"}, true),
            new DataRow("Gr 9/10 Passed Alg 1", "F", "SCH_ALGPASS_GS0910", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_ALGPASS_GS0910", new String[] {"M", "F"}, true),
            // Gr 11/12 Passed Alg 1
            new DataRow("#top", "M", "SCH_ALGPASS_GS1112", new String[] {"M"}, true),
            new DataRow("Gr 11/12 Passed Alg 1", "F", "SCH_ALGPASS_GS1112", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_ALGPASS_GS1112", new String[] {"M", "F"}, true),
            // SAT/ACT Test
            new DataRow("#top", "M", "SCH_SATACT", new String[] {"M"}, true),
            new DataRow("SAT/ACT Test", "F", "SCH_SATACT", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_SATACT", new String[] {"M", "F"}, true),
            // Took AP Test
            new DataRow("#top", "M", "SCH_APEXAM_ONEORMORE", new String[] {"M"}, true),
            new DataRow("Took AP Test", "F", "SCH_APEXAM_ONEORMORE", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_APEXAM_ONEORMORE", new String[] {"M", "F"}, true),
            // Did NOT Take AP Test
            new DataRow("#top", "M", "SCH_APEXAM_NONE", new String[] {"M"}, true),
            new DataRow("Did NOT Take AP Test", "F", "SCH_APEXAM_NONE", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_APEXAM_NONE", new String[] {"M", "F"}, true),
            // Retained Gr K
            new DataRow("#top", "M", "SCH_RET_KG", new String[] {"M"}),
            new DataRow("Retained Gr K", "F", "SCH_RET_KG", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_KG", new String[] {"M", "F"}),
            // Retained Gr 1
            new DataRow("#top", "M", "SCH_RET_G01", new String[] {"M"}),
            new DataRow("Retained Gr 1", "F", "SCH_RET_G01", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G01", new String[] {"M", "F"}),
            // Retained Gr 2
            new DataRow("#top", "M", "SCH_RET_G02", new String[] {"M"}),
            new DataRow("Retained Gr 2", "F", "SCH_RET_G02", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G02", new String[] {"M", "F"}),
            // Retained Gr 3
            new DataRow("#top", "M", "SCH_RET_G03", new String[] {"M"}),
            new DataRow("Retained Gr 3", "F", "SCH_RET_G03", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G03", new String[] {"M", "F"}),
            // Retained Gr 4
            new DataRow("#top", "M", "SCH_RET_G04", new String[] {"M"}),
            new DataRow("Retained Gr 4", "F", "SCH_RET_G04", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G04", new String[] {"M", "F"}),
            // Retained Gr 5
            new DataRow("#top", "M", "SCH_RET_G05", new String[] {"M"}),
            new DataRow("Retained Gr 5", "F", "SCH_RET_G05", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G05", new String[] {"M", "F"}),
            // Retained Gr 6
            new DataRow("#top", "M", "SCH_RET_G06", new String[] {"M"}),
            new DataRow("Retained Gr 6", "F", "SCH_RET_G06", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G06", new String[] {"M", "F"}),
            // Retained Gr 7
            new DataRow("#top", "M", "SCH_RET_G07", new String[] {"M"}),
            new DataRow("Retained Gr 7", "F", "SCH_RET_G07", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G07", new String[] {"M", "F"}),
            // Retained Gr 8
            new DataRow("#top", "M", "SCH_RET_G08", new String[] {"M"}),
            new DataRow("Retained Gr 8", "F", "SCH_RET_G08", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RET_G08", new String[] {"M", "F"}),
            // Retained Gr 9
            new DataRow("#top", "M", "SCH_RET_G09", new String[] {"M"}, true),
            new DataRow("Retained Gr 9", "F", "SCH_RET_G09", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_RET_G09", new String[] {"M", "F"}, true),
            // Retained Gr 10
            new DataRow("#top", "M", "SCH_RET_G10", new String[] {"M"}, true),
            new DataRow("Retained Gr 10", "F", "SCH_RET_G10", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_RET_G10", new String[] {"M", "F"}, true),
            // Retained Gr 11
            new DataRow("#top", "M", "SCH_RET_G11", new String[] {"M"}, true),
            new DataRow("Retained Gr 11", "F", "SCH_RET_G11", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_RET_G11", new String[] {"M", "F"}, true),
            // Retained Gr 12
            new DataRow("#top", "M", "SCH_RET_G12", new String[] {"M"}, true),
            new DataRow("Retained Gr 12", "F", "SCH_RET_G12", new String[] {"F"}, true),
            new DataRow("#bottom", "Total", "SCH_RET_G12", new String[] {"M", "F"}, true),
            // Corp Punish Gr PK
            new DataRow("#top", "M", "SCH_PSDISC_CORP", new String[] {"M"}),
            new DataRow("Corp Punish Gr PK", "F", "SCH_PSDISC_CORP", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_PSDISC_CORP", new String[] {"M", "F"}),
            // Single OOS Suspension Gr PK
            new DataRow("#top", "M", "SCH_PSDISC_SINGOOS", new String[] {"M"}),
            new DataRow("Single OOS Suspension Gr PK", "F", "SCH_PSDISC_SINGOOS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_PSDISC_SINGOOS", new String[] {"M", "F"}),
            // Multiple OOS Suspensions Gr PK
            new DataRow("#top", "M", "SCH_PSDISC_MULTOOS", new String[] {"M"}),
            new DataRow("Multiple OOS Suspensions Gr PK", "F", "SCH_PSDISC_MULTOOS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_PSDISC_MULTOOS", new String[] {"M", "F"}),
            // Expelled Gr PK
            new DataRow("#top", "M", "SCH_PSDISC_EXP", new String[] {"M"}),
            new DataRow("Expelled Gr PK", "F", "SCH_PSDISC_EXP", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_PSDISC_EXP", new String[] {"M", "F"}),
            // Corp Punish K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_CORP", new String[] {"M"}),
            new DataRow("Corp Punish K-12", "F", "SCH_DISCWODIS_CORP", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_CORP", new String[] {"M", "F"}),
            // ISS K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_ISS", new String[] {"M"}),
            new DataRow("ISS K-12", "F", "SCH_DISCWODIS_ISS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_ISS", new String[] {"M", "F"}),
            // Single OOS Suspension K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_SINGOOS", new String[] {"M"}),
            new DataRow("Single OOS Suspension K-12", "F", "SCH_DISCWODIS_SINGOOS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_SINGOOS", new String[] {"M", "F"}),
            // Multiple OOS Suspensions K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_MULTOOS", new String[] {"M"}),
            new DataRow("Multiple OOS Suspensions K-12", "F", "SCH_DISCWODIS_MULTOOS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_MULTOOS", new String[] {"M", "F"}),
            // Expelled with Ed Services K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_EXPWE", new String[] {"M"}),
            new DataRow("Expelled with Ed Services K-12", "F", "SCH_DISCWODIS_EXPWE", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_EXPWE", new String[] {"M", "F"}),
            // Expelled without Ed Services K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_EXPWOE", new String[] {"M"}),
            new DataRow("Expelled without Ed Services K-12", "F", "SCH_DISCWODIS_EXPWOE", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_EXPWOE", new String[] {"M", "F"}),
            // Expelled Zero Tolerance K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_EXPZT", new String[] {"M"}),
            new DataRow("Expelled Zero Tolerance K-12", "F", "SCH_DISCWODIS_EXPZT", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_EXPZT", new String[] {"M", "F"}),
            // Referred to Law Enforcement K1-2
            new DataRow("#top", "M", "SCH_DISCWODIS_REF", new String[] {"M"}),
            new DataRow("Referred to Law Enforcement K-12", "F", "SCH_DISCWODIS_REF", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_REF", new String[] {"M", "F"}),
            // School Related Arrest K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_ARR", new String[] {"M"}),
            new DataRow("School Related Arrest K-12", "F", "SCH_DISCWODIS_ARR", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_ARR", new String[] {"M", "F"}),
            // Transfer to Alt School Discipline K-12
            new DataRow("#top", "M", "SCH_DISCWODIS_TFRALT", new String[] {"M"}),
            new DataRow("Transfer to Alt School Discipline K-12", "F", "SCH_DISCWODIS_TFRALT", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DISCWODIS_TFRALT", new String[] {"M", "F"}),
            // Corp Punish IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_CORP_IDEA", "SCH_DISCWDIS_CORP"}, new String[] {"M"}),
            new DataRow("Corp Punish IDEA K-12", "F", new String[] {"SCH_DISCWDIS_CORP_IDEA", "SCH_DISCWDIS_CORP"},
                    new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_CORP_IDEA", "SCH_DISCWDIS_CORP"},
                    new String[] {"M", "F"}),
            // ISS IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_ISS_IDEA", "SCH_DISCWDIS_ISS"}, new String[] {"M"}),
            new DataRow("ISS IDEA K-12", "F", new String[] {"SCH_DISCWDIS_ISS_IDEA", "SCH_DISCWDIS_ISS"},
                    new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_ISS_IDEA", "SCH_DISCWDIS_ISS"},
                    new String[] {"M", "F"}),
            // Single OOS Suspension IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_SINGOOS_IDEA", "SCH_DISCWDIS_SINGOOS"},
                    new String[] {"M"}),
            new DataRow("Single OOS Suspension IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_SINGOOS_IDEA", "SCH_DISCWDIS_SINGOOS"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_SINGOOS_IDEA", "SCH_DISCWDIS_SINGOOS"},
                    new String[] {"M", "F"}),
            // Multiple OOS Suspensions IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_MULTOOS_IDEA", "SCH_DISCWDIS_MULTOOS"},
                    new String[] {"M"}),
            new DataRow("Multiple OOS Suspensions IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_MULTOOS_IDEA", "SCH_DISCWDIS_MULTOOS"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_MULTOOS_IDEA", "SCH_DISCWDIS_MULTOOS"},
                    new String[] {"M", "F"}),
            // Expelled with Ed Services IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_EXPWE_IDEA", "SCH_DISCWDIS_EXPWE"},
                    new String[] {"M"}),
            new DataRow("Expelled with Ed Services IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_EXPWE_IDEA", "SCH_DISCWDIS_EXPWE"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_EXPWE_IDEA", "SCH_DISCWDIS_EXPWE"},
                    new String[] {"M", "F"}),
            // Expelled without Ed Services IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_EXPWOE_IDEA", "SCH_DISCWDIS_EXPWOE"},
                    new String[] {"M"}),
            new DataRow("Expelled without Ed Services IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_EXPWOE_IDEA", "SCH_DISCWDIS_EXPWOE"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_EXPWOE_IDEA", "SCH_DISCWDIS_EXPWOE"},
                    new String[] {"M", "F"}),
            // Expelled Zero Tolerance IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_EXPZT_IDEA", "SCH_DISCWDIS_EXPZT"},
                    new String[] {"M"}),
            new DataRow("Expelled Zero Tolerance IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_EXPZT_IDEA", "SCH_DISCWDIS_EXPZT"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_EXPZT_IDEA", "SCH_DISCWDIS_EXPZT"},
                    new String[] {"M", "F"}),
            // Referred to Law Enforcement IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_REF_IDEA", "SCH_DISCWDIS_REF"}, new String[] {"M"}),
            new DataRow("Referred to Law Enforcement IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_REF_IDEA", "SCH_DISCWDIS_REF"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_REF_IDEA", "SCH_DISCWDIS_REF"},
                    new String[] {"M", "F"}),
            // School Related Arrest IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_ARR_IDEA", "SCH_DISCWDIS_ARR"}, new String[] {"M"}),
            new DataRow("School Related Arrest IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_ARR_IDEA", "SCH_DISCWDIS_ARR"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_ARR_IDEA", "SCH_DISCWDIS_ARR"},
                    new String[] {"M", "F"}),
            // Transfer to Alt School Discipline IDEA K-12
            new DataRow("#top", "M", new String[] {"SCH_DISCWDIS_TFRALT_IDEA", "SCH_DISCWDIS_TFRALT"},
                    new String[] {"M"}),
            new DataRow("Transfer to Alt School Discipline IDEA K-12", "F",
                    new String[] {"SCH_DISCWDIS_TFRALT_IDEA", "SCH_DISCWDIS_TFRALT"}, new String[] {"F"}),
            new DataRow("#bottom", "Total", new String[] {"SCH_DISCWDIS_TFRALT_IDEA", "SCH_DISCWDIS_TFRALT"},
                    new String[] {"M", "F"}),
            // School Days Missed due to OOS Suspension
            new DataRow("#top", "M", "SCH_DAYSMISSED", new String[] {"M"}),
            new DataRow("School Days Missed due to OOS Suspension", "F", "SCH_DAYSMISSED", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_DAYSMISSED", new String[] {"M", "F"}),
            // Harassed/Bullied on the Basis of Sex
            new DataRow("#top", "M", "SCH_HBREPORTED_SEX", new String[] {"M"}),
            new DataRow("Harassed/Bullied on the Basis of Sex", "F", "SCH_HBREPORTED_SEX", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_HBREPORTED_SEX", new String[] {"M", "F"}),
            // Harassed/Bullied on the Basis of Race
            new DataRow("#top", "M", "SCH_HBREPORTED_RAC", new String[] {"M"}),
            new DataRow("Harassed/Bullied on the Basis of Race", "F", "SCH_HBREPORTED_RAC", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_HBREPORTED_RAC", new String[] {"M", "F"}),
            // Harassed/Bullied on the Basis of Disability
            new DataRow("#top", "M", "SCH_HBREPORTED_DIS", new String[] {"M"}),
            new DataRow("Harassed/Bullied on the Basis of Disability", "F", "SCH_HBREPORTED_DIS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_HBREPORTED_DIS", new String[] {"M", "F"}),
            // Disciplined for Sexual Harassment
            new DataRow("#top", "M", "SCH_HBDISCIPLINED_SEX", new String[] {"M"}),
            new DataRow("Disciplined for Sexual Harassment", "F", "SCH_HBDISCIPLINED_SEX", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_HBDISCIPLINED_SEX", new String[] {"M", "F"}),
            // Disciplined for Racial Harassment
            new DataRow("#top", "M", "SCH_HBDISCIPLINED_RAC", new String[] {"M"}),
            new DataRow("Disciplined for Racial Harassment", "F", "SCH_HBDISCIPLINED_RAC", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_HBDISCIPLINED_RAC", new String[] {"M", "F"}),
            // Disciplined for Disability Harassment
            new DataRow("#top", "M", "SCH_HBDISCIPLINED_DIS", new String[] {"M"}),
            new DataRow("Disciplined for Disability Harassment", "F", "SCH_HBDISCIPLINED_DIS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_HBDISCIPLINED_DIS", new String[] {"M", "F"}),
            // Number of Mechanical Restraints Non IDEA
            new DataRow("#top", "M", "SCH_RS_NONIDEA_MECH", new String[] {"M"}),
            new DataRow("Number of Mechanical Restraints Non IDEA", "F", "SCH_RS_NONIDEA_MECH", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RS_NONIDEA_MECH", new String[] {"M", "F"}),
            // Number of Physical Restraints Non IDEA
            new DataRow("#top", "M", "SCH_RS_NONIDEA_PHYS", new String[] {"M"}),
            new DataRow("Number of Physical Restraints Non IDEA", "F", "SCH_RS_NONIDEA_PHYS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RS_NONIDEA_PHYS", new String[] {"M", "F"}),
            // Number of Seclusions IDEA
            new DataRow("#top", "M", "SCH_RS_IDEA_SECL", new String[] {"M"}),
            new DataRow("Number of Seclusions IDEA", "F", "SCH_RS_IDEA_SECL", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RS_IDEA_SECL", new String[] {"M", "F"}),
            // Number of Mechanical Restraints IDEA
            new DataRow("#top", "M", "SCH_RS_IDEA_MECH", new String[] {"M"}),
            new DataRow("Number of Mechanical Restraints IDEA", "F", "SCH_RS_IDEA_MECH", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RS_IDEA_MECH", new String[] {"M", "F"}),
            // Number of Physical Restraints IDEA
            new DataRow("#top", "M", "SCH_RS_IDEA_PHYS", new String[] {"M"}),
            new DataRow("Number of Physical Restraints IDEA", "F", "SCH_RS_IDEA_PHYS", new String[] {"F"}),
            new DataRow("#bottom", "Total", "SCH_RS_IDEA_PHYS", new String[] {"M", "F"}));

    private static final String FIELD_504 = "section504";
    private static final String FIELD_AMERICAN_INDIAN = "americanIndian";
    private static final String FIELD_ASIAN = "asian";
    private static final String FIELD_BLACK = "black";
    private static final String FIELD_HAWAIIAN = "hawaiian";
    private static final String FIELD_HEADING = "heading";
    private static final String FIELD_HISPANIC = "hispanic";
    private static final String FIELD_IDEA = "idea";
    private static final String FIELD_LEP = "lep";
    private static final String FIELD_MULTI_RACE = "multiRace";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_PK_PREFIX = "pk_";
    private static final String FIELD_PK_TOTAL = "pk_tot";
    private static final String FIELD_SCHOOL_ID = "schoolId";
    private static final String FIELD_TOTAL = "total";
    private static final String FIELD_TYPE = "type";
    private static final String FIELD_WHITE = "white";

    private static final String GRID_FIELD_LEA_ID = "LEA_ID";
    private static final String GRID_FIELD_SCH_ID = "SCH_ID";

    private static final String INPUT_PARAM_HIGH_SCHOOL_ONLY = "highSchoolOnly";
    private static final String INPUT_PARAM_PART_1_PROCEDURE_ID = "procedureIdPart1";
    private static final String INPUT_PARAM_PART_2_PROCEDURE_ID = "procedureIdPart2";
    private static final String INPUT_PARAM_REPORT_TITLE = "reportTitle";
    private static final String INPUT_PARAM_REPORT_TYPE = "reportType";

    private static final String KEY_PK_PREFIX = "LEA_PSENR_A";

    private static final int NCES_ID_LEA_LENGTH = 7;

    private static final String PROCEDURE_ID_LEA_1 = "EXPDATA-CRDC-LEA-P1";
    private static final String PROCEDURE_ID_LEA_2 = "EXPDATA-CRDC-LEA-P2";
    private static final String PROCEDURE_ID_SCH_1 = "EXPDATA-CRDC-SCH-P1";
    private static final String PROCEDURE_ID_SCH_2 = "EXPDATA-CRDC-SCH-P2";

    private static final String REPORT_TYPE_1 = "1";
    private static final String REPORT_TYPE_2 = "2";
    private static final String REPORT_TYPE_BOTH = "3";
    private static final String REPORT_TYPE_LEA = "4";

    private static final String REPORT_TYPE_1_TITLE = "CRDC School Part 1 Summary";
    private static final String REPORT_TYPE_2_TITLE = "CRDC School Part 2 Summary";
    private static final String REPORT_TYPE_BOTH_TITLE = "CRDC School Part 1 & 2 Summary";
    private static final String REPORT_TYPE_LEA_TITLE = "CRDC LEA Part 1 & 2 Summary";

    private static final Set<String> SET_HIGH_SCHOOL_LEVELS = new HashSet(Arrays.asList("High School"));
    private static final Set<String> SET_MIDDLE_SCHOOL_LEVELS = new HashSet(Arrays.asList("Middle School"));

    /**
     * The Class ColumnCache.
     */
    private class ColumnCache {
        private List<DataGrid> m_dataGrids = new LinkedList();
        private Map<String, String> m_fieldsMap = new HashMap();
        private Map<String, String> m_valueCache;

        /**
         * Adds the grid.
         *
         * @param procedureId String
         * @throws X2BaseException exception
         */
        public void addGrid(String procedureId) throws X2BaseException {
            DataGrid grid = loadDataGrid(procedureId);
            m_dataGrids.add(grid);
        }

        /**
         * Gets the decimal value.
         *
         * @param key String
         * @return Big decimal
         */
        public BigDecimal getDecimalValue(String key) {
            String stringValue = getString(key);
            BigDecimal testValue = null;
            try {
                testValue = new BigDecimal(stringValue);
            } catch (Exception e) {
                // cannot be converted
            }
            return testValue;
        }

        /**
         * Gets the string.
         *
         * @param key String
         * @return String
         */
        public String getString(String key) {
            String value = null;
            if (m_valueCache.containsKey(key)) {
                value = m_valueCache.get(key);
            } else {
                String mappedKey = m_fieldsMap.get(key);
                for (DataGrid grid : m_dataGrids) {
                    String test = (String) grid.get(mappedKey);
                    if (test != null) {
                        value = test;
                        break;
                    }
                }
                m_valueCache.put(key, value);
            }
            return value;
        }

        /**
         * Gets the value set.
         *
         * @param key String
         * @return Sets the
         */
        public Set<String> getValueSet(String key) {
            Set<String> values = new HashSet();
            for (DataGrid grid : m_dataGrids) {
                grid.beforeTop();
                while (grid.next()) {
                    String test = (String) grid.get(key);
                    if (test != null) {
                        values.add(test);
                        break;
                    }
                }
            }
            return values;
        }

        /**
         * Sets the school.
         *
         * @param ncesId String
         * @return true, if successful
         */
        public boolean setNcesId(String ncesId) {
            m_valueCache = new HashMap();
            for (DataGrid grid : m_dataGrids) {
                boolean found = false;
                grid.beforeTop();
                while (grid.next()) {
                    if (ncesId.length() == NCES_ID_LEA_LENGTH) {
                        String lea = (String) grid.get(GRID_FIELD_LEA_ID);
                        if (!StringUtils.isEmpty(lea) && lea.equals(ncesId)) {
                            found = true;
                            break;
                        }
                    } else {
                        String school = (String) grid.get(GRID_FIELD_SCH_ID);
                        if (!StringUtils.isEmpty(school) && school.equals(ncesId)) {
                            found = true;
                            break;
                        }
                    }
                }
                if (!found) {
                    return false;
                }
            }
            return true;
        }

        /**
         * Load data grid.
         *
         * @param procedureId String
         * @return DataGrid
         * @throws X2BaseException exception
         */
        private DataGrid loadDataGrid(String procedureId) throws X2BaseException {
            Collection<StateReportValidationError> initErrors = new ArrayList<StateReportValidationError>();
            StateReportData reportData =
                    StateReportData.getReportDataFromProcedure(procedureId, getBroker(), initErrors);
            if (reportData != null && initErrors.size() == 0) {
                // Initialize the report data object.
                reportData.setBroker(getBroker());
                reportData.setCurrentContext(getCurrentContext());
                reportData.setOrganization(getOrganization());
                reportData.setPrivilegeSet(getPrivilegeSet());
                reportData.setSchoolContext(isSchoolContext());
                reportData.setSchool(getSchool());
                reportData.setParameters(getParameters());
                reportData.setUser(getUser());
                reportData.initializeExport();

                initErrors.addAll(reportData.getSetupErrors());
            }

            mapFields(reportData);
            DataGrid dataGrid = new DataGrid();
            if (initErrors.size() == 0) {
                if (reportData.open()) {
                    try {
                        StateReportEntity entity = null;
                        while ((entity = reportData.next()) != null) {
                            StateReportValidationError err = entity.filterEntity();
                            if (err == null) {
                                entity.preProcess();
                                entity.setScriptManager(reportData.getScriptManager());
                                dataGrid.append();

                                /*
                                 * Add all fields
                                 */
                                for (int pos = 0; pos < reportData.getFieldCount(); pos++) {
                                    FieldDefinition field = reportData.getFieldDefinition(pos);
                                    String fieldValue = entity.getFieldValue(pos);

                                    /*
                                     * If the value requires padding, pad it and trim it to field
                                     * max
                                     * length.
                                     */
                                    fieldValue = ExportFormatManager.doPadding(fieldValue,
                                            (field.getResizeMode() == null
                                                    ? ExportFormatField.PaddingDirectionCode.NONE.ordinal()
                                                    : field.getResizeMode().ordinal()),
                                            field.getPaddingChar(),
                                            field.getExportLength());

                                    // Set the final value.
                                    dataGrid.set(field.getFieldId(), fieldValue);
                                }
                                entity.postProcess();
                            } else {
                                initErrors.add(err);
                            }
                        }
                    } finally {
                        reportData.close();
                    }
                }

            } else {
                for (StateReportValidationError error : initErrors) {
                    dataGrid.append();
                    dataGrid.set("Entity name", error.getEntityName());
                    dataGrid.set("Error ID", error.getErrorId());
                    dataGrid.set("Field Name", error.getFieldName());
                    dataGrid.set("Error message", error.getErrorMessage());
                }
            }
            dataGrid.beforeTop();
            return dataGrid;
        }

        /**
         * Map fields.
         *
         * @param reportData StateReportData
         */
        private void mapFields(StateReportData reportData) {
            for (int pos = 0; pos < reportData.getFieldCount(); pos++) {
                FieldDefinition field = reportData.getFieldDefinition(pos);
                String key = field.getSifPath();
                String value = field.getFieldId();
                if (!StringUtils.isEmpty(key) && !GRID_FIELD_SCH_ID.equals(key) && !GRID_FIELD_LEA_ID.equals(key)) {
                    if (m_fieldsMap.containsKey(key)) {
                        throw new IllegalStateException("Duplicate key found: " + key);
                    }
                    m_fieldsMap.put(key, value);
                }
            }
        }
    }

    private DataDictionary m_dictionary;
    private Set<String> m_highSchoolLevelCodes;
    private Set<String> m_middleSchoolLevelCodes;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ColumnCache cache = new ColumnCache();
        String reportType = (String) getParameter(INPUT_PARAM_REPORT_TYPE);
        List<DataRow> rows = null;
        boolean isSchoolReport = true;
        if (!StringUtils.isEmpty(reportType)) {
            if (reportType.equals(REPORT_TYPE_1)) {
                addParameter(INPUT_PARAM_REPORT_TITLE, REPORT_TYPE_1_TITLE);
                rows = DATA_ROWS_PART_1;
                cache.addGrid(PROCEDURE_ID_SCH_1);
            } else if (reportType.equals(REPORT_TYPE_2)) {
                addParameter(INPUT_PARAM_REPORT_TITLE, REPORT_TYPE_2_TITLE);
                rows = DATA_ROWS_PART_2;
                cache.addGrid(PROCEDURE_ID_SCH_2);
            } else if (reportType.equals(REPORT_TYPE_BOTH)) {
                addParameter(INPUT_PARAM_REPORT_TITLE, REPORT_TYPE_BOTH_TITLE);
                rows = new ArrayList(DATA_ROWS_PART_1);
                rows.addAll(DATA_ROWS_PART_2);
                cache.addGrid(PROCEDURE_ID_SCH_1);
                cache.addGrid(PROCEDURE_ID_SCH_2);
            } else if (reportType.equals(REPORT_TYPE_LEA)) {
                isSchoolReport = false;
                addParameter(INPUT_PARAM_REPORT_TITLE, REPORT_TYPE_LEA_TITLE);
                rows = DATA_ROWS_LEA;
                cache.addGrid(PROCEDURE_ID_LEA_1);
                cache.addGrid(PROCEDURE_ID_LEA_2);
            }
        } else {
            throw new IllegalStateException("Input parameter report type must be set");
        }

        ReportDataGrid reportGrid = new ReportDataGrid();
        if (isSchoolReport) {
            X2Criteria criteria = new X2Criteria();
            if (isSchoolContext()) {
                criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
            } else {
                criteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
                criteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
            }
            if (getParameter(INPUT_PARAM_HIGH_SCHOOL_ONLY) != null &&
                    getParameter(INPUT_PARAM_HIGH_SCHOOL_ONLY) instanceof Boolean &&
                    ((Boolean) getParameter(INPUT_PARAM_HIGH_SCHOOL_ONLY)).booleanValue()) {
                criteria.addIn(SisSchool.COL_SCHOOL_LEVEL_CODE,
                        getCodesForCRDCValue(SisSchool.class, SisSchool.COL_SCHOOL_LEVEL_CODE, SET_HIGH_SCHOOL_LEVELS));
            }

            QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    SisSchool skl = (SisSchool) iterator.next();
                    String ncesId = (String) skl.getFieldValueByAlias(ALIAS_NCES_SCHOOL_ID);
                    addSchoolToGrid(cache, reportGrid, skl, ncesId, rows);
                }
            }
        } else {
            for (String ncesId : cache.getValueSet(GRID_FIELD_LEA_ID)) {
                addSchoolToGrid(cache, reportGrid, null, ncesId, rows);
            }
        }

        reportGrid.beforeTop();
        return reportGrid;
    }

    /**
     * Adds the school to grid.
     *
     * @param cache ColumnCache
     * @param reportGrid ReportDataGrid
     * @param skl SisSchool
     * @param ncesId String
     * @param rows List<DataRow>
     */
    private void addSchoolToGrid(ColumnCache cache,
                                 ReportDataGrid reportGrid,
                                 SisSchool skl,
                                 String ncesId,
                                 List<DataRow> rows) {
        if (ncesId != null && ncesId.length() >= NCES_ID_LEA_LENGTH && cache.setNcesId(ncesId)) {
            Iterator<DataRow> rowIterator = rows.iterator();
            boolean firstRow = true;
            while (rowIterator.hasNext()) {
                DataRow row = rowIterator.next();
                if (includeRow(skl, row)) {
                    reportGrid.append();
                    reportGrid.set(FIELD_SCHOOL, skl);
                    reportGrid.set(FIELD_SCHOOL_ID, ncesId);
                    reportGrid.set(FIELD_TYPE, row.getType());
                    reportGrid.set(FIELD_TOTAL, row.total(cache));
                    reportGrid.set(FIELD_HISPANIC, row.evaluate(cache, "HI"));
                    reportGrid.set(FIELD_AMERICAN_INDIAN, row.evaluate(cache, "AM"));
                    reportGrid.set(FIELD_ASIAN, row.evaluate(cache, "AS"));
                    reportGrid.set(FIELD_HAWAIIAN, row.evaluate(cache, "HP"));
                    reportGrid.set(FIELD_BLACK, row.evaluate(cache, "BL"));
                    reportGrid.set(FIELD_WHITE, row.evaluate(cache, "WH"));
                    reportGrid.set(FIELD_MULTI_RACE, row.evaluate(cache, "TR"));
                    reportGrid.set(FIELD_LEP, row.evaluate(cache, "LEP"));
                    reportGrid.set(FIELD_IDEA, row.evaluate(cache, "IDEA"));
                    reportGrid.set(FIELD_504, row.evaluate(cache, "504"));
                    if (skl == null && firstRow) {
                        BigDecimal total = BigDecimal.ZERO;
                        for (int age = 2; age <= 5; ++age) {
                            BigDecimal count = cache.getDecimalValue(KEY_PK_PREFIX + age);
                            reportGrid.set(FIELD_PK_PREFIX + age, count);
                            total = total.add(count);
                        }
                        reportGrid.set(FIELD_PK_TOTAL, total);
                        firstRow = false;
                    }
                }
                if (rowIterator.hasNext()) {
                    row = rowIterator.next();
                    if (includeRow(skl, row)) {
                        reportGrid.set(FIELD_HEADING, row.getHeading());
                        reportGrid.set(FIELD_TYPE + "_2", row.getType());
                        reportGrid.set(FIELD_TOTAL + "_2", row.total(cache));
                        reportGrid.set(FIELD_HISPANIC + "_2", row.evaluate(cache, "HI"));
                        reportGrid.set(FIELD_AMERICAN_INDIAN + "_2", row.evaluate(cache, "AM"));
                        reportGrid.set(FIELD_ASIAN + "_2", row.evaluate(cache, "AS"));
                        reportGrid.set(FIELD_HAWAIIAN + "_2", row.evaluate(cache, "HP"));
                        reportGrid.set(FIELD_BLACK + "_2", row.evaluate(cache, "BL"));
                        reportGrid.set(FIELD_WHITE + "_2", row.evaluate(cache, "WH"));
                        reportGrid.set(FIELD_MULTI_RACE + "_2", row.evaluate(cache, "TR"));
                        reportGrid.set(FIELD_LEP + "_2", row.evaluate(cache, "LEP"));
                        reportGrid.set(FIELD_IDEA + "_2", row.evaluate(cache, "IDEA"));
                        reportGrid.set(FIELD_504 + "_2", row.evaluate(cache, "504"));
                    }
                }
                if (rowIterator.hasNext()) {
                    row = rowIterator.next();
                    if (includeRow(skl, row)) {
                        reportGrid.set(FIELD_TYPE + "_3", row.getType());
                        reportGrid.set(FIELD_TOTAL + "_3", row.total(cache));
                        reportGrid.set(FIELD_HISPANIC + "_3", row.evaluate(cache, "HI"));
                        reportGrid.set(FIELD_AMERICAN_INDIAN + "_3", row.evaluate(cache, "AM"));
                        reportGrid.set(FIELD_ASIAN + "_3", row.evaluate(cache, "AS"));
                        reportGrid.set(FIELD_HAWAIIAN + "_3", row.evaluate(cache, "HP"));
                        reportGrid.set(FIELD_BLACK + "_3", row.evaluate(cache, "BL"));
                        reportGrid.set(FIELD_WHITE + "_3", row.evaluate(cache, "WH"));
                        reportGrid.set(FIELD_MULTI_RACE + "_3", row.evaluate(cache, "TR"));
                        reportGrid.set(FIELD_LEP + "_3", row.evaluate(cache, "LEP"));
                        reportGrid.set(FIELD_IDEA + "_3", row.evaluate(cache, "IDEA"));
                        reportGrid.set(FIELD_504 + "_3", row.evaluate(cache, "504"));
                    }
                }
            }
        }
    }

    /**
     * Gets the codes for CRDC value.
     *
     * @param beanClass Class
     * @param columnName String
     * @param codes Collection<String>
     * @return Sets the
     */
    private Set<String> getCodesForCRDCValue(Class beanClass, String columnName, Collection<String> codes) {
        Set<String> value = new HashSet();
        // make sure at least one value is included.
        value.add("__dummy__");

        ModelProperty prop = new ModelProperty(beanClass, columnName, getBroker().getPersistenceKey());
        DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());
        DataDictionaryField aliasField =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRDC_REF_CODE);
        if (!StringUtils.isEmpty(field.getReferenceTableOid())) {
            Map<String, String> map = CRDCReportData.getCRDCCodeLookup(getBroker(), aliasField.getJavaName(),
                    field.getReferenceTableOid(), codes);
            value.addAll(map.keySet());
        }
        return value;
    }

    /**
     * Returns a local instance of a district data dictionary.
     *
     * @return DataDictionary.
     */
    private DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Include row.
     *
     * @param skl SisSchool
     * @param row DataRow
     * @return true, if successful
     */
    private boolean includeRow(SisSchool skl, DataRow row) {
        boolean value = false;
        if (skl == null) {
            value = true;
        } else if (!row.isHighSchoolOnly() && !row.isMiddleSchoolOnly()) {
            value = true;
        } else if (row.isHighSchoolOnly() && isHighSchool(skl)) {
            value = true;
        } else if (row.isMiddleSchoolOnly() && isMiddleSchool(skl)) {
            value = true;
        }
        return value;
    }

    /**
     * Checks if is high school.
     *
     * @param skl SisSchool
     * @return true, if is high school
     */
    private boolean isHighSchool(SisSchool skl) {
        if (m_highSchoolLevelCodes == null) {
            m_highSchoolLevelCodes =
                    getCodesForCRDCValue(SisSchool.class, SisSchool.COL_SCHOOL_LEVEL_CODE, SET_HIGH_SCHOOL_LEVELS);
        }
        return m_highSchoolLevelCodes.contains(skl.getSchoolLevelCode());
    }

    /**
     * Checks if is middle school.
     *
     * @param skl SisSchool
     * @return true, if is middle school
     */
    private boolean isMiddleSchool(SisSchool skl) {
        if (m_middleSchoolLevelCodes == null) {
            m_middleSchoolLevelCodes =
                    getCodesForCRDCValue(SisSchool.class, SisSchool.COL_SCHOOL_LEVEL_CODE, SET_MIDDLE_SCHOOL_LEVELS);
        }
        return m_middleSchoolLevelCodes.contains(skl.getSchoolLevelCode());
    }

}
