/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.health.ri;

/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STUDENT_ACTIVE_CODE;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.CollectionUtils;
import java.io.ByteArrayInputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Certificate of Immunization report. The data for each health
 * immunization dose category is displayed in a subreport.
 *
 * @author X2 Development Corporation
 */
public class RICertificateOfImmunizationMultiData extends ReportJavaSourceNet {
    /**
     * Name for the "active only" report parameter. The value is an Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

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
     * Report column name for student
     */
    public static final String FIELD_STUDENT = "student";

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

    /**
     * Report parameter name for the date.
     */
    public static final String PARAMETER_DATE = "date";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for sort report parameter.
     */
    private static final String SORT_PARAM = "sort";

    /*
     * Subreport Fields
     */
    private static final String FIELD_CATEGORIES_SUBREPORT_DATA = "categoriesSubreportData";
    private static final String FIELD_CATEGORIES_SUBREPORT_FORMAT = "categoriesSubreportFormat";
    private static final String FIELD_IMMUNITIES_SUBREPORT_DATA = "immunitiesSubreportData";
    private static final String FIELD_IMMUNITIES_SUBREPORT_FORMAT = "immunitiesSubreportFormat";

    private static final String NO_DATE = "No Date";

    // Subreport ID's
    private static final String SUBREPORT_CATEGORIES_ID = "SYS-HTH-MA-001-SUB-1";
    private static final String SUBREPORT_IMMUNITIES_ID = "SYS-HTH-MA-001-SUB-2";

    private Report m_categoriesSubreport;
    private SisStudent m_currentStudent;
    private Map<String, Collection<HealthImmunizationDose>> m_healthImmunizationDoses;
    private Collection<ReferenceCode> m_himCategories;
    private Report m_immunitiesSubreport;
    private Criteria m_studentCriteria;

    private DateFormat m_dateFormatter = new SimpleDateFormat("MM-dd-yyyy");

    private String[] m_tests = {"Measles", "Mumps", "Rubella", "Varicella*", "Hepatitis B"};

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        buildCriteria();
        loadHealthImmunizationDoses();
        loadHimCategories();

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, m_studentCriteria);

        String sortBy = (String) getParameter(SORT_PARAM);
        applyUserSort(query, sortBy);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                prepareCategoryDoses(grid, student);
                prepareImmunities(grid);
            }
        } finally {
            iterator.close();
        }

        addParameter(PARAMETER_DATE, m_dateFormatter.format(getPlainDate()));

        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_categoriesSubreport = ReportUtils.getReport(SUBREPORT_CATEGORIES_ID, getBroker());
        m_immunitiesSubreport = ReportUtils.getReport(SUBREPORT_IMMUNITIES_ID, getBroker());
    }

    /**
     * Builds the student criteria
     */
    private void buildCriteria() {
        m_studentCriteria = new Criteria();

        if (m_currentStudent != null) {
            m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            /*
             * Add the user criteria
             */
            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);

            addUserCriteria(m_studentCriteria, queryBy, queryString, null, null);

            /*
             * If we are in the school context, filter the list by the current school. This is not
             * necessary if current selection is being used, assuming that the user cannot create an
             * out-of-scope selection.
             */
            if (isSchoolContext()) {
                m_studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
            }

            boolean activeOnly =
                    getParameter(ACTIVE_ONLY_PARAM) != null ? ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue()
                            : false;
            if (activeOnly) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(), STUDENT_ACTIVE_CODE);
                m_studentCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
            }
        }
    }

    /**
     * Loads a map of collection of health Immunization doses keyed on student Oid.
     */
    private void loadHealthImmunizationDoses() {
        Criteria criteria = new Criteria();

        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
        criteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, subQuery);

        QueryByCriteria query = new QueryByCriteria(HealthImmunizationDose.class, criteria);
        m_healthImmunizationDoses = getBroker().getGroupedCollectionByQuery(query,
                HealthImmunizationDose.COL_STUDENT_OID,
                (int) (getBroker().getCount(query) * 1.5));
    }

    /**
     * Loads a collection of
     */
    private void loadHimCategories() {
        Criteria hidCatCriteria = new Criteria();
        hidCatCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID,
                BeanManager.getFullOid("rtbHimCat", getBroker().getPersistenceKey()));
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, hidCatCriteria);
        query.addOrderByAscending(ReferenceCode.COL_SEQUENCE_NUMBER);

        m_himCategories = getBroker().getCollectionByQuery(query);
    }

    /**
     * Prepares the categories subreport.
     *
     * @param grid
     */
    private void prepareCategoryDoses(ReportDataGrid grid, SisStudent student) {
        if (!CollectionUtils.isEmpty(m_himCategories)) {
            for (ReferenceCode himCategory : m_himCategories) {
                String himCategoryCode = himCategory.getCode();
                String himCategoryCodeExamples = himCategory.getDescription();
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
                grid.set(FIELD_STUDENT, student);
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
                Collection<HealthImmunizationDose> studentDoses = m_healthImmunizationDoses.get(student.getOid());
                if (!CollectionUtils.isEmpty(studentDoses)) {
                    for (HealthImmunizationDose dose : studentDoses) {
                        HealthImmunizationDefinition def = dose.getImmunizationSeries().getImmunizationDefinition();
                        if (def != null && def.getCategories() != null
                                && def.getCategories().contains(himCategoryCode)) {
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
                }

                categoryGrid.beforeTop();

                grid.set(FIELD_CATEGORIES_SUBREPORT_DATA, categoryGrid);
                grid.set(FIELD_CATEGORIES_SUBREPORT_FORMAT,
                        new ByteArrayInputStream(m_categoriesSubreport.getCompiledFormat()));
            }
        }
    }

    /**
     * Prepares the immunities subreport.
     */
    private void prepareImmunities(ReportDataGrid grid) {
        ReportDataGrid immunitiesGrid = new ReportDataGrid();

        if (!CollectionUtils.isEmpty(m_himCategories)) {
            for (int i = 0; i < m_tests.length; i++) {
                immunitiesGrid.append();
                immunitiesGrid.set(FIELD_VACCINE_TYPE, m_tests[i]);
                immunitiesGrid.set(FIELD_VACCINE_DATE, "");
            }

            immunitiesGrid.beforeTop();
            grid.set(FIELD_IMMUNITIES_SUBREPORT_DATA, immunitiesGrid);
            grid.set(FIELD_IMMUNITIES_SUBREPORT_FORMAT,
                    new ByteArrayInputStream(m_immunitiesSubreport.getCompiledFormat()));
        }
    }
}
