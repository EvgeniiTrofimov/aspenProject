/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.health.ma;

import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.SisStudent;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Certificate of Immunization report. The data for each health
 * immunization dose category is displayed in a subreport.
 *
 * @author X2 Development Corporation
 */
public class CertificateOfImmunizationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report column name which contains the vaccine category.
     */
    public static final String FIELD_CATEGORIES = "category";

    /**
     * Report column name which contains the vaccine category examples.
     */
    public static final String FIELD_CATEGORIES_EXAMPLES = "examples";

    /**
     * Report column name which contains the vaccine type.
     */
    public static final String FIELD_IS_IMMUNE = "immune";

    /**
     * Report column name for the vaccine date. This is for displaying the date.
     */
    public static final String FIELD_VACCINE_DATE = "date";

    /**
     * Report column name for the vaccine date used to sort the grid. This does not format the date.
     */
    public static final String FIELD_VACCINE_DATE_SORT = "dateSort";

    /**
     * Report column name which contains the vaccine type.
     */
    public static final String FIELD_VACCINE_TYPE = "type";

    /*
     * Subreport ID
     */
    private static final String NO_DATE = "No Date";

    /**
     * Report parameter name for the category grid collection.
     */
    public static final String PARAMETER_CATEGORY_GRID_LIST_ITERATOR = "categoryGridList";

    /**
     * Report parameter name for the date.
     */
    public static final String PARAMETER_DATE = "date";

    /**
     * Report parameter name for the immunities grid.
     */
    public static final String PARAMETER_IMMUNITIES_GRID = "immunitiesGrid";

    /**
     * Report parameter name for the school.
     */
    public static final String PARAMETER_SCHOOL = "school";

    /**
     * Report parameter name for the student.
     */
    public static final String PARAMETER_STUDENT = "student";

    /**
     * Report parameter name for the categories subreport.
     */
    public static final String PARAMETER_SUBREPORT_CATEGORIES = "subreportCategories";

    /**
     * Report parameter name for the immunities subreport.
     */
    public static final String PARAMETER_SUBREPORT_IMMUNITIES = "subreportImmunities";

    private static final String SUBREPORT_CATEGORIES_ID = "SYS-HTH-MA-001-SUB-1";
    private static final String SUBREPORT_IMMUNITIES_ID = "SYS-HTH-MA-001-SUB-2";

    private SisStudent m_currentStudent;

    private DateFormat m_dateFormatter = new SimpleDateFormat("MM-dd-yyyy");

    private String[] m_tests = {"Measles", "Mumps", "Rubella", "Varicella*", "Hepatitis B"};

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        /*
         * Set up the grid and parameters.
         */
        addParameter(PARAMETER_STUDENT, m_currentStudent);
        addParameter(PARAMETER_DATE, m_dateFormatter.format(getPlainDate()));
        addParameter(PARAMETER_SUBREPORT_CATEGORIES, getSubreportCategories());
        addParameter(PARAMETER_SUBREPORT_IMMUNITIES, getSubreportImmunities());

        ReportDataGrid grid = new ReportDataGrid();

        prepareCategoryDoses(grid);
        prepareImmunities();

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.follett.fsc.core.k12.tools.reports.ReportJavaSource#initialize(com.follett.fsc.core.k12.
     *      web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Returns the subreport bean for the categories subreport.
     *
     * @return Report
     */
    private Report getSubreportCategories() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, SUBREPORT_CATEGORIES_ID);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        return (Report) getBroker().getBeanByQuery(query);
    }

    /**
     * Returns the subreport bean for the immunities subreport.
     *
     * @return Report
     */
    private Report getSubreportImmunities() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ID, SUBREPORT_IMMUNITIES_ID);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        return (Report) getBroker().getBeanByQuery(query);
    }

    /**
     * Prepares the categories.
     *
     * @param grid ReportDataGrid
     */
    private void prepareCategoryDoses(ReportDataGrid grid) {
        ArrayList<ReportDataGrid> categoryGridArrayList = new ArrayList<ReportDataGrid>();
        Collection<HealthImmunizationDose> studentDoses = m_currentStudent.getImmunizationDoses();

        /*
         * Get the Health Immunization Definition categories
         */
        Criteria hidCatCriteria = new Criteria();
        hidCatCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID,
                BeanManager.getFullOid("rtbHimCat", getBroker().getPersistenceKey()));
        hidCatCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, hidCatCriteria);
        query.addOrderByAscending(ReferenceCode.COL_SEQUENCE_NUMBER);

        QueryIterator himCategoriesIterator = getBroker().getIteratorByQuery(query);

        try {
            /*
             * Iterate over the categories.
             */
            while (himCategoriesIterator.hasNext()) {
                ReferenceCode himCategory = (ReferenceCode) himCategoriesIterator.next();
                String himCategoryCode = himCategory.getCode();
                String himCategoryCodeExamples = himCategory.getLocalizedDescription(1);
                if (himCategoryCodeExamples == null) {
                    himCategoryCodeExamples = " ";
                }
                int himCategoryCount = 2;

                try {
                    himCategoryCount = Integer.parseInt(himCategory.getLocalCode());
                } catch (NumberFormatException e) {
                    // ignore number formatting, default to 2 rows
                }

                grid.append();
                grid.set(FIELD_CATEGORIES, himCategoryCode);
                grid.set(FIELD_CATEGORIES_EXAMPLES, himCategoryCodeExamples);

                ReportDataGrid categoryGrid = new ReportDataGrid();

                // Automatically fill in "himCategoryCount" empty doses
                for (int i = 0; i < himCategoryCount; i++) {
                    categoryGrid.append();
                    categoryGrid.set(FIELD_CATEGORIES, himCategoryCode);
                    categoryGrid.set(FIELD_CATEGORIES_EXAMPLES, himCategoryCodeExamples);
                    categoryGrid.set(FIELD_VACCINE_DATE, null);
                    categoryGrid.set(FIELD_VACCINE_TYPE, null);
                }
                categoryGrid.beforeTop();

                /*
                 * Find all HealthImmunizationDose's that are in category and add them to the
                 * category grid.
                 */
                for (HealthImmunizationDose dose : studentDoses) {
                    HealthImmunizationDefinition def = dose.getImmunizationSeries().getImmunizationDefinition();
                    if (def != null && def.getCategories() != null && def.getCategories().contains(himCategoryCode)) {
                        if (!categoryGrid.next()) {
                            categoryGrid.append();
                        }

                        String date = dose.getDate() != null ? m_dateFormatter.format(dose.getDate()) : NO_DATE;
                        categoryGrid.set(FIELD_CATEGORIES, himCategoryCode);
                        categoryGrid.set(FIELD_VACCINE_DATE, date);
                        categoryGrid.set(FIELD_VACCINE_DATE_SORT, dose.getDate());
                        categoryGrid.set(FIELD_VACCINE_TYPE, def.getSeriesId());
                    }
                }

                List<String> sortColumns = new ArrayList<String>(2);
                sortColumns.add(FIELD_CATEGORIES);
                sortColumns.add(FIELD_VACCINE_DATE_SORT);

                categoryGrid.sort(sortColumns, false);
                categoryGrid.beforeTop();
                categoryGridArrayList.add(categoryGrid);
            }
        } finally {
            himCategoriesIterator.close();
        }

        addParameter(PARAMETER_CATEGORY_GRID_LIST_ITERATOR, categoryGridArrayList.listIterator());
    }

    /**
     * Prepare the immunities ReportDataGrid.
     */
    private void prepareImmunities() {
        ReportDataGrid immunitiesGrid = new ReportDataGrid();

        for (int i = 0; i < m_tests.length; i++) {
            immunitiesGrid.append();
            immunitiesGrid.set(FIELD_VACCINE_TYPE, m_tests[i]);
            immunitiesGrid.set(FIELD_VACCINE_DATE, "");
        }

        immunitiesGrid.beforeTop();
        addParameter(PARAMETER_IMMUNITIES_GRID, immunitiesGrid);
    }
}
