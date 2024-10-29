/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.health.ma;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the MA Annual District BMI Summary Report.
 *
 * @author X2 Development Corporation
 */
public class AnnualHealthBMIData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public static final String INPUT_PARAM_START_DATE = "startDate";
    public static final String INPUT_PARAM_END_DATE = "endDate";

    public static final String REPORT_PARAM_START_DATE = "startDate";
    public static final String REPORT_PARAM_END_DATE = "endDate";
    public static final String REPORT_PARAM_PERSON = "person";
    public static final String REPORT_PARAM_POSITION = "position";

    private static final String HSC_GENERAL_OID = "ddxHscGeneral";
    private static final String ALIAS_GENERAL_BMI_PERCENT = "hsc-general-bmi-percent";

    private static final String GRADE_PREFIX = "G";
    private static final String GRADE_LEVEL_01 = "01";
    private static final String GRADE_LEVEL_04 = "04";
    private static final String GRADE_LEVEL_07 = "07";
    private static final String GRADE_LEVEL_10 = "10";
    private static final String GENDER_MALE = "M";
    private static final String GENDER_FEMALE = "F";
    private static final String BMI_CATEGORY_UNDERWEIGHT = "U";
    private static final String BMI_CATEGORY_NORMAL = "N";
    private static final String BMI_CATEGORY_OVERWEIGHT = "O";
    private static final String BMI_CATEGORY_OBESE = "B";

    private static final String[] m_gradeLevelsArray =
            new String[] {GRADE_LEVEL_01, GRADE_LEVEL_04, GRADE_LEVEL_07, GRADE_LEVEL_10};
    private static final String[] m_gendersArray = new String[] {GENDER_MALE, GENDER_FEMALE};
    private static final String[] m_bMICategoryArray =
            new String[] {BMI_CATEGORY_UNDERWEIGHT, BMI_CATEGORY_NORMAL, BMI_CATEGORY_OVERWEIGHT, BMI_CATEGORY_OBESE};

    private static final int BMI_PRECENTILE_5 = 5;
    private static final int BMI_PRECENTILE_85 = 85;
    private static final int BMI_PRECENTILE_95 = 95;

    private HashMap<String, Integer> m_bMIValues = new HashMap<String, Integer>();

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        // Get Input parameters
        PlainDate startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        PlainDate endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);

        // Set Report parameters
        addParameter(REPORT_PARAM_START_DATE, startDate);
        addParameter(REPORT_PARAM_END_DATE, endDate);

        // pre-load values
        presetBMIValues();

        // Load population
        Collection<HealthScreening> healthScreenings = loadHealthScreening(startDate, endDate);

        // Summarize BMI percentile counts
        updateBMIValues(healthScreenings);

        // Prepare report grid
        ReportDataGrid grid = new ReportDataGrid(100);
        grid.append();
        for (String key : m_bMIValues.keySet()) {
            Integer value = m_bMIValues.get(key);
            grid.set(key, value);
        }
        grid.beforeTop();

        return grid;
    }

    /**
     * Returns a collection of HealthScreening records for the dates specified.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return Collection<HealthScreening>
     */
    private Collection<HealthScreening> loadHealthScreening(PlainDate startDate, PlainDate endDate) {
        Criteria healthScreeningCriteria = getOrganizationCriteria(HealthScreening.class);
        healthScreeningCriteria.addEqualTo(HealthScreening.COL_EXTENDED_DATA_DICTIONARY_OID, HSC_GENERAL_OID);

        healthScreeningCriteria.addGreaterOrEqualThan(HealthScreening.COL_DATE, startDate);
        healthScreeningCriteria.addLessOrEqualThan(HealthScreening.COL_DATE, endDate);

        ArrayList selectedGradeLevels = new ArrayList();
        for (int i = 0; i < m_gradeLevelsArray.length; i++) {
            selectedGradeLevels.add(m_gradeLevelsArray[i]);
        }

        healthScreeningCriteria.addIn(
                HealthScreening.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.COL_GRADE_LEVEL,
                selectedGradeLevels);

        QueryByCriteria healthScreeningQuery = new QueryByCriteria(HealthScreening.class, healthScreeningCriteria);
        healthScreeningQuery.addOrderByDescending(HealthScreening.COL_DATE);

        Collection<HealthScreening> healthScreenings = getBroker().getCollectionByQuery(healthScreeningQuery);

        return healthScreenings;
    }

    /**
     * Pre-loads the BMI Values to 0.
     */
    private void presetBMIValues() {
        for (int i = 0; i < m_gradeLevelsArray.length; i++) {
            String gradeLevel = m_gradeLevelsArray[i];
            for (int j = 0; j < m_gendersArray.length; j++) {
                String gender = m_gendersArray[j];
                for (int k = 0; k < m_bMICategoryArray.length; k++) {
                    String bMICategory = m_bMICategoryArray[k];
                    String key = GRADE_PREFIX + gradeLevel + gender + bMICategory;
                    m_bMIValues.put(key, Integer.valueOf(0));
                }
            }
        }
    }

    /**
     * Updates the BMI Values from the HealthScreenings .
     *
     * @param healthScreenings Collection<HealthScreening>
     */
    private void updateBMIValues(Collection<HealthScreening> healthScreenings) {
        // Get field name for BMI Percent
        ExtendedDictionaryAttributes extendedDictionaryAttributes =
                (ExtendedDictionaryAttributes) getBroker().getBeanByOid(ExtendedDataDictionary.class, HSC_GENERAL_OID);
        DataDictionary extendedDictionary =
                DataDictionary.getDistrictDictionary(extendedDictionaryAttributes, getBroker().getPersistenceKey());
        DataDictionaryField bMIPercentField =
                extendedDictionary.findDataDictionaryFieldByAlias(ALIAS_GENERAL_BMI_PERCENT);
        String bMIPercentFieldName = bMIPercentField.getJavaName();
        HashSet loggedStudentOids = new HashSet();

        for (HealthScreening healthScreening : healthScreenings) {
            String studentOid = healthScreening.getStudentOid();
            // Count each student only once.
            if (!loggedStudentOids.contains(studentOid)) {
                String bMIPercentageStr = (String) healthScreening.getFieldValueByBeanPath(bMIPercentFieldName);
                int bMIPercentage = 0;
                if (bMIPercentageStr != null && StringUtils.isNumeric(bMIPercentageStr)) {
                    bMIPercentage = Integer.valueOf(bMIPercentageStr).intValue();
                }

                String gradeLevel = healthScreening.getStudent().getGradeLevel();
                String gender = healthScreening.getStudent().getPerson().getGenderCode();
                String percentileGroup = null;
                if (bMIPercentage < BMI_PRECENTILE_5) {
                    percentileGroup = BMI_CATEGORY_UNDERWEIGHT;
                } else if (bMIPercentage >= BMI_PRECENTILE_5 && bMIPercentage < BMI_PRECENTILE_85) {
                    percentileGroup = BMI_CATEGORY_NORMAL;
                } else if (bMIPercentage >= BMI_PRECENTILE_85 && bMIPercentage < BMI_PRECENTILE_95) {
                    percentileGroup = BMI_CATEGORY_OVERWEIGHT;
                } else {
                    percentileGroup = BMI_CATEGORY_OBESE;
                }

                String key = GRADE_PREFIX + gradeLevel + gender + percentileGroup;

                if (!m_bMIValues.containsKey(key)) {
                    m_bMIValues.put(key, Integer.valueOf(1));
                } else {
                    Integer count = m_bMIValues.get(key);
                    m_bMIValues.put(key, Integer.valueOf(count.intValue() + 1));
                }

                loggedStudentOids.add(studentOid);
            }
        }
    }

}
