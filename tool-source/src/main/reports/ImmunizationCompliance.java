/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationGroup;
import com.x2dev.sis.model.beans.HealthImmunizationGroupOverride;
import com.x2dev.sis.model.beans.HealthImmunizationRuleAttributes;
import com.x2dev.sis.model.beans.HealthImmunizationRuleInstance;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.model.business.health.HealthDateRange;
import com.x2dev.sis.model.business.health.ImmunizationRuleEngine;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.sis.web.health.ImmunizationRuleException;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.DateRange;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Immunization compliance report.
 *
 * @author X2 Development Corporation
 */
public class ImmunizationCompliance extends ReportJavaSourceNet {
    /**
     * Name for the "active only" report parameter. This value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "group by category" report parameter. This value is a Boolean.
     */
    public static final String GROUP_BY_PARAM = "groupByCategory";

    /**
     * Name for the "immunizations to include" report parameter. This value is a String.
     */
    public static final String HIM_TO_INCLUDE_PARAM = "himOids";

    /**
     * Name for the "immunization groups to include" report parameter. This value is a String.
     */
    public static final String HIG_TO_INCLUDE_PARAM = "higOids";

    /**
     * Name for the "required only" report parameter. This value is a Boolean.
     */
    public static final String REQUIRED_ONLY_PARAM = "requiredOnly";

    /**
     * Name for the "secondary students" report parameter. The value is a Boolean.
     */
    public static final String SECONDARY_STUDENT_PARAM = "secondaryStudent";

    /**
     * Name for the "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Value for the "Query By" input parameter. This value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Value for the "Query String" input parameter. This value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    protected static final String COL_DEFINITION = "definition";
    protected static final String COL_DOSE = "dose_";
    protected static final String COL_SERIES = "series";
    protected static final String COL_STUDENT = "student";

    /**
     * Aliases
     */
    protected static final String ALIAS_CONDITIONAL_EXPIRE = "DOE CONDITIONAL EXPIRE";
    protected static final String ALIAS_CONDITIONAL_STATUS = "DOE CONDITIONAL STATUS";
    protected static final String ALIAS_CONDITIONAL_EXPIRE_GRP = "DOE CONDITIONAL EXPIRE GRP";
    protected static final String ALIAS_CONDITIONAL_STATUS_GRP = "DOE CONDITIONAL STATUS GRP";

    protected String m_conditionalExpire;
    protected String m_conditionalStatus;
    protected String m_conditionalExpireGrp;
    protected String m_conditionalStatusGrp;
    protected SisStudent m_currentStudent;
    protected DateFormat m_dateFormat;
    protected Collection<HealthImmunizationRuleAttributes> m_definitions;
    protected String m_defsToInclude;
    protected DateAsStringConverter m_dConv;
    protected Map<String, Map<String, List<HealthImmunizationDose>>> m_doses;
    protected GradeLevelHistory m_history;
    protected boolean m_requiredOnly;
    protected Map<String, Map<String, HealthImmunizationRuleInstance>> m_series;

    /*
     * Constants for label key lookup.
     */
    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "show compliance errors" report parameter. This value is a Boolean.
     */
    public static final String SHOW_ERRORS_PARAM = "showErrors";

    /**
     * Name for the "show next dose" report parameter. This value is a Boolean.
     */
    public static final String SHOW_NEXT_DOSE_PARAM = "showNextDose";

    private static final String COL_ERROR = "error";
    private static final String COL_NEXT_DOSES = "nextDoses";

    private static final String COMMA = ", ";
    private static final String DASH = " - ";
    private static final String MESSAGE_NO_DATE = "label.health.immunizationNoDate";
    private static final String MESSAGE_SKIP = "label.health.immunizationSkipped";
    private static final String MESSAGE_WAIVED = "label.health.immunizationWaived";

    private static final String PARAM_ERROR = "error";
    private static final String PARAM_ERROR_MSGS = "errorMessages";

    /**
     * A local copy of the data dictionary for use by various lookup utilities.
     */
    private DataDictionary m_dictionary;

    /**
     * A list of errors encountered during initialization. Accessible through getSetupErrors().
     */
    private List<String> m_setupErrors = new ArrayList();

    private Boolean m_showErrors;
    private Boolean m_showNextDose;
    protected PlainDate m_reportDate;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ImmunizationReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_conditionalExpire = translateAliasToJavaName(ALIAS_CONDITIONAL_EXPIRE, false);
        m_conditionalExpireGrp = translateAliasToJavaName(ALIAS_CONDITIONAL_EXPIRE_GRP, false);
        m_conditionalStatus = translateAliasToJavaName(ALIAS_CONDITIONAL_STATUS, false);
        m_conditionalStatusGrp = translateAliasToJavaName(ALIAS_CONDITIONAL_STATUS_GRP, false);

        ReportDataGrid grid = new ReportDataGrid(100, 19);

        if (m_setupErrors.size() > 0) {
            addParameter(PARAM_ERROR, Boolean.valueOf(true));
            grid.append();
            addParameter(PARAM_ERROR_MSGS, m_setupErrors);
        } else {
            m_dConv = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                    getLocale(), true);
            m_showErrors = (Boolean) getParameter(SHOW_ERRORS_PARAM);
            m_showNextDose = (Boolean) getParameter(SHOW_NEXT_DOSE_PARAM);

            m_dateFormat = new java.text.SimpleDateFormat("MM/dd/yy");
            m_defsToInclude = (String) getParameter(HIM_TO_INCLUDE_PARAM);
            if (StringUtils.isEmpty(m_defsToInclude)) {
                m_defsToInclude = "";
            } else {
                m_defsToInclude += ",";
            }
            m_defsToInclude += (String) getParameter(HIG_TO_INCLUDE_PARAM);

            m_requiredOnly = ((Boolean) getParameter(REQUIRED_ONLY_PARAM)).booleanValue();

            QueryByCriteria studentQuery = getStudentQuery();

            m_history = new GradeLevelHistory(studentQuery.getCriteria(),
                    20,
                    OrganizationManager.getRootOrganization(getOrganization()),
                    getBroker());
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
            populateLookupMaps(studentQuery);

            boolean groupByCategory = ((Boolean) getParameter(GROUP_BY_PARAM)).booleanValue();

            if (!groupByCategory) {
                QueryIterator iterator = getBroker().getIteratorByQuery(studentQuery);

                while (iterator.hasNext()) {
                    SisStudent student = (SisStudent) iterator.next();
                    populateGridForStudent(grid, student, null);
                }
            } else {
                // Get student collection lookup map to reduce query calls
                Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

                List<HealthImmunizationRuleAttributes> ruleAttributes = getSortedRuleAttributes();
                Iterator iterator = ruleAttributes.iterator();
                while (iterator.hasNext()) {
                    HealthImmunizationRuleAttributes definition = (HealthImmunizationRuleAttributes) iterator.next();

                    for (SisStudent student : students) {
                        populateGridForStudent(grid, student, definition.getOid());
                    }
                }
            }

            addParameter(PARAM_ERROR, Boolean.valueOf(false));
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Populates the grid with the passed students immunization series/dose information. If
     * himFilter
     * is specified, only series/dose information for that definition are set up.
     *
     * @param grid ReportDataGrid
     * @param student SisStudent
     * @param himFilter (Optional)
     */
    protected void populateGridForStudent(ReportDataGrid grid, SisStudent student, String himFilter) {
        if (student != null) {
            for (HealthImmunizationRuleAttributes ruleAttributes : m_definitions) {
                /*
                 * Filter out the following if requested by report input:
                 *
                 * - non-required definitions
                 * - definitions not in picklist selection
                 * - definitions not matching himFilter oid passed into method optionally
                 */
                if ((!m_requiredOnly || ruleAttributes.getRequiredIndicator()) &&
                        (StringUtils.isEmpty(m_defsToInclude) || m_defsToInclude.contains(ruleAttributes.getOid())) &&
                        (StringUtils.isEmpty(himFilter) || himFilter.contains(ruleAttributes.getOid()))) {
                    HealthImmunizationRuleInstance ruleInstance = getRuleInstance(ruleAttributes, student);

                    // Skip waived series.
                    if (ruleAttributes instanceof HealthImmunizationDefinition) {
                        if (ruleInstance != null && ruleInstance.getWaivedIndicator()) {
                            continue;
                        }
                    }
                    // Skip groups that containing only waived series.
                    else if (ruleAttributes instanceof HealthImmunizationGroup) {
                        boolean containsNotWaived = false;
                        for (HealthImmunizationDefinition definition : ruleAttributes
                                .getImmunizationDefinitions(getBroker())) {
                            HealthImmunizationSeries series =
                                    (HealthImmunizationSeries) getRuleInstance(definition, student);
                            if (series == null || !series.getWaivedIndicator()) {
                                containsNotWaived = true;
                            }
                        }
                        if (!containsNotWaived) {
                            continue;
                        }
                    }

                    List<HealthImmunizationDose> doses = getDoses(ruleAttributes, student);

                    /*
                     * Like the input IU, only show non-required definitions when doses exist
                     */
                    if (ruleAttributes.getRequiredIndicator() || !doses.isEmpty()) {
                        try {
                            if (!StringUtils.isEmpty(ruleAttributes.getRuleDefinition())) {
                                ImmunizationRuleEngine engine =
                                        new ImmunizationRuleEngine(ruleAttributes, m_history, getBroker());

                                boolean compliant = engine.evaluateCompliance(student, doses) ||
                                        (ruleInstance != null && ruleInstance.getComplianceOverrideIndicator());

                                if (!compliant) {
                                    if (ruleAttributes instanceof HealthImmunizationDefinition) {
                                        /*
                                         * Note: we continue to call populateGrid which accepts
                                         * HealthImmunizationDefinition and HealthImmunizationSeries
                                         * objects to support backwards-compatibility for reports
                                         * developed prior to introducing the
                                         * HealthImmunizationRuleAttributes and
                                         * HealthImmunizationRuleInstance interfaces.
                                         *
                                         * The default implementation of this simply passes through
                                         * to populateImmunizationGrid. If a subclass implements
                                         * it, populateImmunizationGrid will not be called.
                                         */

                                        if (!getStudentConditionalStatus(ruleInstance)) {
                                            populateGrid(student, (HealthImmunizationDefinition) ruleAttributes,
                                                    (HealthImmunizationSeries) ruleInstance, doses, grid, engine);
                                        }
                                    } else {
                                        if (!getStudentConditionalStatus(ruleInstance)) {
                                            populateImmunizationGrid(student, ruleAttributes, ruleInstance, doses, grid,
                                                    engine);
                                        }
                                    }

                                }
                            }
                        } catch (ImmunizationRuleException ire) {
                            // Series for definitions with malformed XML rules are ignored
                        }
                    }
                }
            }
        }
    }

    /**
     * Populates the grid for the passed student, definition, series, and list of doses.
     *
     * @param student SisStudent
     * @param definition HealthImmunizationDefinition
     * @param series (optional)
     * @param dosesList List<HealthImmunizationDose>
     * @param grid ReportDataGrid
     * @param engine ImmunizationRuleEngine
     */
    @Deprecated
    protected void populateGrid(SisStudent student,
                                HealthImmunizationDefinition definition,
                                HealthImmunizationSeries series,
                                List<HealthImmunizationDose> dosesList,
                                ReportDataGrid grid,
                                ImmunizationRuleEngine engine) {
        populateImmunizationGrid(student, definition, series, dosesList, grid, engine);
    }

    /**
     * Populate immunization grid.
     *
     * @param student SisStudent
     * @param ruleAttributes HealthImmunizationRuleAttributes
     * @param ruleInstance HealthImmunizationRuleInstance
     * @param dosesList List<HealthImmunizationDose>
     * @param grid ReportDataGrid
     * @param engine ImmunizationRuleEngine
     * @see
     *      com.follett.fsc.core.k12.tools.reports.ImmunizationReportJavaSource#populateImmunizationGrid
     */
    protected void populateImmunizationGrid(SisStudent student,
                                            HealthImmunizationRuleAttributes ruleAttributes,
                                            HealthImmunizationRuleInstance ruleInstance,
                                            List<HealthImmunizationDose> dosesList,
                                            ReportDataGrid grid,
                                            ImmunizationRuleEngine engine) {

        int rows = new BigDecimal(String.valueOf(dosesList.size() / 5.0)).setScale(0, RoundingMode.DOWN).intValue()
                + 1 +
                (m_showErrors.booleanValue() ? 1 : 0) +
                (m_showNextDose.booleanValue() ? 1 : 0);

        for (int row = 0; row < rows; row++) {
            // Add all doses for this series
            for (int index = row * 5; index < dosesList.size() && index < (row + 1) * 5; index++) {
                HealthImmunizationDose dose = dosesList.get(index);

                String doseLabel = dose.getDate() != null ? m_dateFormat.format(dose.getDate()) : "";

                if (StringUtils.isEmpty(doseLabel)) {
                    if (dose.getSkippedIndicator()) {
                        doseLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(MESSAGE_SKIP);
                    } else if (dose.getNoDateIndicator()) {
                        doseLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(MESSAGE_NO_DATE);
                    } else if (dose.getWaivedIndicator()) {
                        doseLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                                .getMessage(MESSAGE_WAIVED);
                    }
                }

                if (ruleAttributes instanceof HealthImmunizationGroup) {
                    doseLabel += " (" + dose.getImmunizationSeries().getImmunizationDefinition().getSeriesId() + ")";
                }

                if (index == row * 5) {
                    grid.append();
                    grid.set(COL_DEFINITION, ruleAttributes);
                    grid.set(COL_SERIES, ruleInstance);
                    grid.set(COL_STUDENT, student);
                }

                grid.set(COL_DOSE + ((index % 5) + 1), doseLabel);
            }

            // If doses list is empty, add a blank doses row
            if (dosesList.size() == 0 && row == 0) {
                grid.append();
                grid.set(COL_DEFINITION, ruleAttributes);
                grid.set(COL_SERIES, ruleInstance);
                grid.set(COL_STUDENT, student);
            }

            // Find the next dose ranges if requested and last row
            if (m_showNextDose.booleanValue() && row == rows - 1) {
                String dateRanges = "";
                Collection<HealthDateRange> ranges = engine.evaluateNextDoses(student, dosesList);
                if (ranges != null) {
                    for (DateRange range : ranges) {
                        if (range != null) {
                            String startDate = m_dateFormat.format(range.getLowerPlainDate());
                            String endDate = m_dateFormat.format(range.getUpperPlainDate());

                            if (!StringUtils.isEmpty(dateRanges)) {
                                dateRanges += COMMA;
                            }

                            dateRanges += startDate + DASH + endDate;
                        }
                    }
                }

                if (!StringUtils.isEmpty(dateRanges)) {
                    grid.append();
                    grid.set(COL_DEFINITION, ruleAttributes);
                    grid.set(COL_SERIES, ruleInstance);
                    grid.set(COL_STUDENT, student);
                    grid.set(COL_NEXT_DOSES, dateRanges);
                }
            }

            // Add error message if requested and 2nd to last row
            if (m_showErrors.booleanValue() &&
                    row == rows - 2 &&
                    !StringUtils.isEmpty(engine.getErrorMessage())) {
                grid.append();
                grid.set(COL_DEFINITION, ruleAttributes);
                grid.set(COL_SERIES, ruleInstance);
                grid.set(COL_STUDENT, student);
                grid.set(COL_ERROR, stripErrorHTML(engine.getErrorMessage()));
            }
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Creates a query to find HealthImmunizationDefinitions, ordered by category name and
     * series name descending.
     *
     * @return QueryByCriteria
     */
    private List<HealthImmunizationRuleAttributes> getSortedRuleAttributes() {
        ArrayList<HealthImmunizationRuleAttributes> ruleAttributes = new ArrayList<>(64);

        addRuleAttributesToList(ruleAttributes, HealthImmunizationDefinition.class,
                HealthImmunizationDefinition.COL_REQUIRED_INDICATOR);
        addRuleAttributesToList(ruleAttributes, HealthImmunizationGroup.class,
                HealthImmunizationGroup.COL_REQUIRED_INDICATOR);

        Comparator comparator = new Comparator<HealthImmunizationRuleAttributes>() {
            @Override
            public int compare(HealthImmunizationRuleAttributes o1, HealthImmunizationRuleAttributes o2) {
                String category1 = StringUtils.isEmpty(o1.getCategories()) ? "zzzzzzz" : o1.getCategories();
                String category2 = StringUtils.isEmpty(o2.getCategories()) ? "zzzzzzz" : o2.getCategories();

                category1 += o1.getName();
                category2 += o2.getName();

                return category1.compareTo(category2);
            }
        };

        Collections.sort(ruleAttributes, comparator);

        return ruleAttributes;
    }

    /**
     * Populates the passed ruleAttribuets list with instances of the passed ruleAttributesClass.
     *
     * @param ruleAttributes ArrayList<HealthImmunizationRuleAttributes>
     * @param ruleAttributesClass Class<? extends HealthImmunizationRuleAttributes>
     * @param requiredColumn String
     */
    private void addRuleAttributesToList(
                                         ArrayList<HealthImmunizationRuleAttributes> ruleAttributes,
                                         Class<? extends HealthImmunizationRuleAttributes> ruleAttributesClass,
                                         String requiredColumn) {
        Criteria attributesCriteria = getOrganizationCriteria(ruleAttributesClass);

        if (!StringUtils.isEmpty(m_defsToInclude)) {
            List<String> oids = StringUtils.convertDelimitedStringToList(m_defsToInclude, ',', false);
            attributesCriteria.addIn(X2BaseBean.COL_OID, oids);
        }

        boolean requiredOnly = ((Boolean) getParameter(REQUIRED_ONLY_PARAM)).booleanValue();
        if (requiredOnly) {
            attributesCriteria.addEqualTo(requiredColumn, Boolean.valueOf(true));
        }

        QueryByCriteria attributesQuery = new QueryByCriteria(ruleAttributesClass, attributesCriteria);
        QueryIterator definitions = getBroker().getIteratorByQuery(attributesQuery);
        try {
            while (definitions.hasNext()) {
                ruleAttributes.add((HealthImmunizationRuleAttributes) definitions.next());
            }
        } finally {
            definitions.close();
        }
    }

    /**
     * Returns a list of doses for the given series and student. Empty list if none exist or passed
     * series was null.
     *
     * @param ruleAttributes HealthImmunizationRuleAttributes
     * @param student SisStudent
     * @return List<HealthImmunizationDose>
     */
    private List<HealthImmunizationDose> getDoses(HealthImmunizationRuleAttributes ruleAttributes, SisStudent student) {
        List<HealthImmunizationDose> dosesList = new LinkedList<>();

        if (ruleAttributes != null) {
            for (HealthImmunizationDefinition definition : ruleAttributes.getImmunizationDefinitions(getBroker())) {
                // Skip doses that have waived series.
                if (ruleAttributes instanceof HealthImmunizationGroup) {
                    HealthImmunizationSeries series = (HealthImmunizationSeries) getRuleInstance(definition, student);
                    if (series != null && series.getWaivedIndicator()) {
                        continue;
                    }
                }
                Map<String, List<HealthImmunizationDose>> doseMap = m_doses.get(definition.getOid());
                if (doseMap != null) {
                    List<HealthImmunizationDose> dosesForStudent = doseMap.get(student.getOid());
                    if (dosesForStudent != null) {
                        dosesList.addAll(dosesForStudent);
                    }
                }
            }
        }

        // The doses list must be sorted by date before passing into the compliance engine
        Comparator comparator = new Comparator<HealthImmunizationDose>() {
            @Override
            public int compare(HealthImmunizationDose o1, HealthImmunizationDose o2) {
                if (o1.getDate() == null) {
                    return -1;
                } else if (o2.getDate() == null) {
                    return 1;
                } else {
                    return o1.getDate().compareTo(o2.getDate());
                }
            }
        };

        Collections.sort(dosesList, comparator);

        return dosesList;
    }

    /**
     * Returns a series for a given definition and student.
     *
     * @param def HealthImmunizationRuleAttributes
     * @param student SisStudent
     * @return HealthImmunizationSeries
     */
    private HealthImmunizationRuleInstance getRuleInstance(HealthImmunizationRuleAttributes def, SisStudent student) {
        HealthImmunizationRuleInstance ruleInstance = null;

        Map<String, HealthImmunizationRuleInstance> ruleInstanceMap = m_series.get(def.getOid());
        if (ruleInstanceMap != null) {
            ruleInstance = ruleInstanceMap.get(student.getOid());
        }

        return ruleInstance;
    }

    /**
     * Determine whether have student a conditional status.
     *
     * @param ruleInstance HealthImmunizationRuleInstance
     * @return boolean conditionalStatusForStd
     */
    private boolean getStudentConditionalStatus(HealthImmunizationRuleInstance ruleInstance) {
        String date = "";
        String status = null;
        PlainDate expireDate = null;
        if (ruleInstance instanceof HealthImmunizationSeries) {
            status = m_conditionalStatus == null ? BooleanAsStringConverter.FALSE
                    : (String) ((HealthImmunizationSeries) ruleInstance).getFieldValueByBeanPath(m_conditionalStatus);
            date = m_conditionalExpire == null ? null
                    : StringUtils.unNullify((String) ((HealthImmunizationSeries) ruleInstance)
                            .getFieldValueByBeanPath(m_conditionalExpire));
        } else if (ruleInstance instanceof HealthImmunizationGroupOverride) {
            status = m_conditionalStatusGrp == null ? BooleanAsStringConverter.FALSE
                    : (String) ((HealthImmunizationGroupOverride) ruleInstance)
                            .getFieldValueByBeanPath(m_conditionalStatusGrp);
            date = m_conditionalExpireGrp == null ? null
                    : StringUtils.unNullify((String) ((HealthImmunizationGroupOverride) ruleInstance)
                            .getFieldValueByBeanPath(m_conditionalExpireGrp));
        }
        // if date == null, there is no expiration date
        expireDate = date == null ? m_reportDate : (PlainDate) m_dConv.parseSystemString(date);
        boolean conditionalStatusForStd = (status != null && status.equals(BooleanAsStringConverter.TRUE) &&
                expireDate != null && !m_reportDate.after(expireDate));
        return conditionalStatusForStd;
    }

    /**
     * Creates a query to find student's matching the reports input attribute specifications.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria getStudentQuery() {
        Criteria studentCriteria = new Criteria();

        if (m_currentStudent != null) {
            studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            StudentContextReportHelper helper =
                    new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

            String queryBy = (String) getParameter(QUERY_BY_PARAM);
            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            addUserCriteria(studentCriteria, queryBy, queryString, SisStudent.class, X2BaseBean.COL_OID);

            /*
             * Active only students (if current selection was not chosen)
             */
            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
            if (activeOnly && !queryBy.contains(CURRENT_KEY)) {
                studentCriteria.addAndCriteria(helper.getActiveStudentCriteria());
            }

            /*
             * Get records for current school only (if current selection was not chosen)
             */
            if (isSchoolContext() && !queryBy.contains(CURRENT_KEY)) {
                studentCriteria.addEqualTo(helper.getSchoolOidField(), getSchool().getOid());

                /*
                 * Include secondary students of the school if needed.
                 */
                if (getParameter(SECONDARY_STUDENT_PARAM) != null
                        && ((Boolean) getParameter(SECONDARY_STUDENT_PARAM)).booleanValue()) {
                    studentCriteria.addOrCriteria(StudentManager.buildSecondaryStudentCriteria(getSchool()));
                }
            } else {
                studentCriteria.addAndCriteria(getOrganizationCriteria(SisStudent.class));
            }
        }

        /*
         * Build and sort the query
         */
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        applyUserSort(studentQuery, (String) getParameter(SORT_PARAM));

        return studentQuery;
    }

    /**
     * Looks up health immunization definition, series, and doses for the students in the passed
     * query.
     *
     * @param studentQuery QueryByCriteria
     */
    private void populateLookupMaps(QueryByCriteria studentQuery) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentQuery.getCriteria());

        // need to populate definitions, series, and doses.

        QueryByCriteria definitionQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_definitions = getBroker().getCollectionByQuery(definitionQuery);

        QueryByCriteria groupQuery = new QueryByCriteria(HealthImmunizationGroup.class, new Criteria());
        m_definitions.addAll(getBroker().getCollectionByQuery(groupQuery));

        // Add the series data
        Criteria seriesCriteria = new Criteria();
        seriesCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria seriesQuery =
                new QueryByCriteria(HealthImmunizationSeries.class, seriesCriteria);

        m_series = getBroker().getNestedMapByQuery(seriesQuery,
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID,
                16,
                128);

        Criteria overridesCriteria = new Criteria();
        overridesCriteria.addIn(HealthImmunizationGroupOverride.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria overridesQuery =
                new QueryByCriteria(HealthImmunizationGroupOverride.class, overridesCriteria);

        m_series.putAll(getBroker().getNestedMapByQuery(overridesQuery,
                HealthImmunizationGroupOverride.COL_IMMUNIZATION_GROUP_OID,
                HealthImmunizationGroupOverride.COL_STUDENT_OID,
                16,
                128));

        // Add the dose data
        Criteria doseCriteria = new Criteria();
        doseCriteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria doseQuery =
                new QueryByCriteria(HealthImmunizationDose.class, doseCriteria);
        doseQuery.addOrderByAscending(HealthImmunizationDose.COL_DATE);

        String[] columns = new String[] {HealthImmunizationDose.REL_IMMUNIZATION_SERIES + "."
                + HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID};
        int[] sizes = new int[] {16, 128};

        m_doses = getBroker().getGroupedCollectionByQuery(doseQuery, columns, sizes);
    }

    /**
     * Replaces certain HTMl characters with characters iReport can handle.
     *
     * TODO: Compliance evaluator should return error objects instead of HTML formatted messages.
     * The displayer should decide the format of the message. This is a short-term hack.
     *
     * @param str String
     * @return String
     */
    private String stripErrorHTML(String str) {
        str = str.replaceAll("<ul>", " ");
        str = str.replaceAll("</ul>", "");
        str = str.replaceAll("<li>", ", ");
        str = str.replaceAll("and,", "and");
        str = str.replaceAll("and ,", "and");
        str = str.replaceAll("or,", "or");
        str = str.replaceAll("or ,", "or");
        str = str.trim();

        if (str.startsWith("or")) {
            str = str.substring(2);
        }

        if (str.startsWith("and")) {
            str = str.substring(3);
        }

        if (str.startsWith(", ")) {
            str = str.substring(2);
        }

        return str;
    }

    /**
     * Adds one validation error with no entity or field.
     *
     *
     * @param errorType String
     * @param errorMessage String
     */
    private void addSetupError(String errorType, String errorMessage) {
        m_setupErrors.add(errorType + "-" + errorMessage);
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
     * Translates an alias into a Java bean path name. An initialization error will be logged
     * if the alias does not exist.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;

        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }

        return javaName;
    }
}
