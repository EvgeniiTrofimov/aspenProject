/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever;
import com.follett.fsc.core.k12.business.ReferenceCodeRetriever.ReferenceCodeCriteria;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.k12.web.presentation.ReferenceFieldFormatter;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Data provider class for the "Education Profile" report.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class EducationProfileData extends OnBaseFormReportJavaSource {

    private static final String ALIAS_IEP_IPRC_NEEDS = "iep-iprc-std-needs";
    private static final String ALIAS_IEP_IPRC_NEEDS_OTHER = "iep-iprc-std-needs-oth";
    private static final String ALIAS_IEP_IPRC_STRENGTHS = "iep-iprc-std-strengths";
    private static final String ALIAS_IEP_IPRC_STRENGTHS_OTHER = "iep-iprc-std-strengths-oth";
    private static final String ALIAS_IPRC_PLACEMENT = "iep-iprc-placement-decision";

    private static final String ALIAS_IEP_AGENCY_PROVIDER = "ipl-agency-provider";
    private static final String ALIAS_IEP_CURRENT_INTERVENTIONS = "iep-epr-current-interventions";
    private static final String ALIAS_IEP_CURRENT_SUPPORTS = "iep-epr-current-supports";
    private static final String ALIAS_IEP_STRATEGIES = "iep-epr-strategies";

    private static final String ALIAS_IPL_DATE = "ipl-date";
    private static final String ALIAS_IPL_ASSESSMENT = "ipl-assessment";
    private static final String ALIAS_IPL_RESULTS_SUMMARY = "ipl-results-summary";

    private static final String DDX_ID_IEP = "ON-SPED-IEP";

    private static final String FIELD_COLUMN_1 = "column1";
    private static final String FIELD_COLUMN_2 = "column2";

    private static final String FIELD_IEP_ATTENDANCE_ABSENCE = "absence";
    private static final String FIELD_IEP_ATTENDANCE_LATE = "late";
    private static final String FIELD_IEP_ATTENDANCE_MONTH = "month";

    private static final String FIELD_IPL_DATE = "date";
    private static final String FIELD_IPL_INFORMATION_SOURCE = "source";
    private static final String FIELD_IPL_RESULTS_SUMMARY = "summary";

    private static final String MEMBER_ROLE_PARENT = "Parent";
    private static final String MEMBER_ROLE_GUARDIAN = "Guardian";

    private static final int MIN_ROWS_ASSESSMENT_DATA = 3;
    private static final int MIN_ROWS_ATTENDANCE_DATA = 3;
    private static final int MIN_ROWS_COMMENT_DATA = 4;
    private static final int MIN_ROWS_STRENGTHS_DATA = 3;
    private static final int MIN_ROWS_INTERVENTIONS_DATA = 4;

    private static final String PARAM_EXCEPTIONALITY = "exceptionality";
    private static final String PARAM_OBSERVATIONS = "observations";
    private static final String PARAM_IPRC_PLACEMENT = "currentPlacement";
    private static final String PARAM_STRATEGIES = "strategies";
    private static final String PARAM_SUB_GRID_ASSESSMENT_DATA = "assessmentDataGrid";
    private static final String PARAM_SUB_GRID_ATTENDANCE_DATA = "attendanceDataGrid";
    private static final String PARAM_SUB_GRID_STRENGTHS_DATA = "strengthsDataGrid";
    private static final String PARAM_SUB_GRID_CURRENT_INTERVENTIONS_DATA = "currentInterventionsDataGrid";
    private static final String PARAM_SUPPORTS = "supports";

    private static final Pattern PATTERN_SPLIT_BY_COMMA = Pattern.compile("\\s*,\\s*");
    private static final Pattern PATTERN_SPLIT_BY_CR = Pattern.compile("\\r\\n");

    protected static final List<KeyValuePair<String, String>> SUB_REPORT_FORMATS = Arrays.asList(
            new KeyValuePair<String, String>("SYS-SPED-ON-EPR-ASM", "assessmentDataFormat"),
            new KeyValuePair<String, String>("SYS-SPED-ON-EPR-2COL", "twoColumnDataFormat2"),
            new KeyValuePair<String, String>("SYS-SPED-ON-EPR-2COL", "twoColumnDataFormatInterventions"),
            new KeyValuePair<String, String>("SYS-SPED-ON-EPR-ATT", "studentAttendanceFormat"));

    private IepData m_currentIep;
    private UserDataContainer m_userData;

    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
    private Locale m_user_locale;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        super.gatherData();

        addParameter(PARAM_EXCEPTIONALITY, getExceptionality());

        gatherAssessmentData();
        gatherStrengths();
        gatherCurrentInterventions();
        gatherAttendanceData();
        loadPlacementReference();

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * @see com.x2dev.reports.sys.sped.on.OnBaseFormReportJavaSource#getTitle()
     */
    @Override
    protected String getTitle() {
        return "Educational Profile";
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));

            addFormParameters();
        }
        initSubReports(SUB_REPORT_FORMATS);

        initializeLocalized();
    }

    /**
     * Initializes for localization.
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<String, MessageResources>();
        m_validLocales = new HashMap<String, String>();

        for (OrganizationLocale loc : locales) {
            if (loc.getEnabledIndicator()) {

                MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        loc.getLocale());
                // save the messages for that language
                resources.put(loc.getLocale(), messages);

                // populate the map of valid locales
                m_validLocales.put(loc.getName(), loc.getLocale());
                if (loc.getPrimaryIndicator()) {
                    m_defaultLocale = loc.getLocale();
                }
            }
        }

        if (m_defaultLocale == null) {
            m_defaultLocale = CONST_AMERICAN_ENGLISH_LOCALE;
        }
        addParameter(PARAM_PREFIX, CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".");
        addParameter(PARAM_REPORT_LOCALE, m_default_message_resource);
        // Additional hooks for enhanced implementations
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
        // Comment line below if your numeric notation, currencies and others don't display as
        // expected
        addParameter(net.sf.jasperreports5.engine.JRParameter.REPORT_LOCALE, m_user_locale); // Only
                                                                                             // tested
                                                                                             // for
                                                                                             // JasperReports
                                                                                             // engine
                                                                                             // 5
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#saveState(com.x2dev.sis.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_userData = userData;

        m_currentIep = userData.getCurrentRecord(IepData.class);

        try {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), userData.getLocale());
            m_user_locale = userData.getLocale();
        } catch (Exception e) {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
            m_user_locale = Locale.US;
        }
    }

    /**
     * Ensure minimum rows.
     *
     * @param dataGrid ReportDataGrid
     * @param minRows int
     */
    private void ensureMinimumRows(ReportDataGrid dataGrid, int minRows) {
        if (dataGrid.rowCount() < minRows) {
            dataGrid.bottom();
            for (int i = dataGrid.rowCount(); i < minRows; ++i) {
                dataGrid.append();
            }
        }
    }

    /**
     * Gather assessment data.
     *
     * @throws X2BaseException exception
     */
    private void gatherAssessmentData() throws X2BaseException {
        ReportDataGrid assessmentDataGrid = new ReportDataGrid();

        ExtendedDataDictionary ddxIep = getExtendedDataDictionaryById(DDX_ID_IEP);
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(ddxIep,
                getBroker().getPersistenceKey());

        for (IepPerformanceLevel ipl : getIepPerformanceLevel()) {
            assessmentDataGrid.append();
            assessmentDataGrid.set(FIELD_IPL_INFORMATION_SOURCE,
                    ipl.getFieldValueByAlias(ALIAS_IPL_ASSESSMENT, dictionary));
            assessmentDataGrid.set(FIELD_IPL_DATE, getAliasAsJavaType(ipl, ALIAS_IPL_DATE, dictionary));
            assessmentDataGrid.set(FIELD_IPL_RESULTS_SUMMARY,
                    ipl.getFieldValueByAlias(ALIAS_IPL_RESULTS_SUMMARY, dictionary));
        }

        if (assessmentDataGrid.rowCount() < MIN_ROWS_ASSESSMENT_DATA) {
            for (int i = assessmentDataGrid.rowCount(); i < MIN_ROWS_ASSESSMENT_DATA; ++i) {
                assessmentDataGrid.append();
            }
        }

        assessmentDataGrid.beforeTop();

        addParameter(PARAM_SUB_GRID_ASSESSMENT_DATA, assessmentDataGrid);
    }

    /**
     * Gather attendance data.
     *
     * @throws X2BaseException exception
     */
    private void gatherAttendanceData() throws X2BaseException {
        ReportDataGrid attendanceDataGrid = new ReportDataGrid();
        IepData iep = getIep();
        if (iep != null) {
            // Query for Student attendance for the current school year.
            DistrictSchoolYearContext context = getCurrentContext();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, iep.getStudentOid());
            criteria.addBetween(StudentAttendance.COL_DATE, context.getStartDate(), context.getEndDate());
            BeanQuery query = new BeanQuery(StudentAttendance.class, criteria);
            query.addOrderBy(StudentAttendance.COL_DATE, true);
            List<StudentAttendance> attendance = (List<StudentAttendance>) getBroker().getCollectionByQuery(query);

            // Sum absence and tardy by month.
            int lastMonth = -1;
            int absent = 0;
            int tardy = 0;
            Calendar cal = Calendar.getInstance();
            for (StudentAttendance att : attendance) {
                if (att.getAbsentIndicator() || att.getTardyIndicator()) {
                    PlainDate attDate = att.getDate();
                    cal.setTime(attDate);
                    int month = cal.get(Calendar.MONTH);
                    if (lastMonth != month) {
                        if (lastMonth > -1) {
                            attendanceDataGrid.append();
                            cal.set(Calendar.MONTH, lastMonth);
                            String monthKey = "label.calendar.month." + Integer.toString(lastMonth);
                            String monthName = LocalizationCache.getMessages(getBroker().getPersistenceKey())
                                    .getMessage(getLocale(), monthKey);
                            attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_MONTH, monthName);
                            attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_ABSENCE,
                                    (absent > 0) ? Integer.toString(absent) : "");
                            attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_LATE,
                                    (tardy > 0) ? Integer.toString(tardy) : "");
                        }
                        lastMonth = month;
                        absent = 0;
                        tardy = 0;
                    }
                    if (att.getAbsentIndicator()) {
                        absent++;
                    }
                    if (att.getTardyIndicator()) {
                        tardy++;
                    }
                }
            }
            if (lastMonth > -1 && (absent > 0 || tardy > 0)) {
                attendanceDataGrid.append();
                cal.set(Calendar.MONTH, lastMonth);
                String monthKey = "label.calendar.month." + Integer.toString(lastMonth);
                String monthName = LocalizationCache.getMessages(getBroker().getPersistenceKey())
                        .getMessage(getLocale(), monthKey);
                attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_MONTH, monthName);
                attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_ABSENCE, (absent > 0) ? Integer.toString(absent) : "");
                attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_LATE, (tardy > 0) ? Integer.toString(tardy) : "");
            }
        }

        // TODO need attendance data here

        if (attendanceDataGrid.rowCount() < MIN_ROWS_ATTENDANCE_DATA) {
            for (int i = attendanceDataGrid.rowCount(); i < MIN_ROWS_ATTENDANCE_DATA; ++i) {
                attendanceDataGrid.append();
                attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_MONTH, "");
                attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_ABSENCE, "");
                attendanceDataGrid.set(FIELD_IEP_ATTENDANCE_LATE, "");
            }
        }

        attendanceDataGrid.beforeTop();

        addParameter(PARAM_SUB_GRID_ATTENDANCE_DATA, attendanceDataGrid);
    }

    /**
     * Gather interventions data.
     */
    private void gatherCurrentInterventions() {
        List<String> interVentions =
        		getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), ALIAS_IEP_CURRENT_INTERVENTIONS,
                        getDictionary(), m_default_message_resource);
        Collections.sort(interVentions);
        populateTwoColumnGrid(PARAM_SUB_GRID_CURRENT_INTERVENTIONS_DATA,
                interVentions.subList(0, interVentions.size() / 2),
                interVentions.subList(interVentions.size() / 2, interVentions.size()), MIN_ROWS_INTERVENTIONS_DATA);
    }


    /**
     * Gather strengths.
     */
    private void gatherStrengths() {
        List<String> strengths =
        		getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), ALIAS_IEP_IPRC_STRENGTHS, getDictionary(), m_default_message_resource);
        strengths.addAll(
        		getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), ALIAS_IEP_IPRC_STRENGTHS_OTHER, getDictionary(), m_default_message_resource));

        Collections.sort(strengths);
        List<String> needs =
        		getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA, getIep(), ALIAS_IEP_IPRC_NEEDS, getDictionary(), m_default_message_resource);
        needs.addAll(
        		getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), ALIAS_IEP_IPRC_NEEDS_OTHER, getDictionary(), m_default_message_resource));
        Collections.sort(needs);
        populateTwoColumnGrid(PARAM_SUB_GRID_STRENGTHS_DATA, strengths, needs, MIN_ROWS_STRENGTHS_DATA);
    }

    /**
     * Gets the alias as list.
     *
     * @param alias String
     * @return List
     */
    private List<String> getAliasAsList(String alias) {
        List<String> data = new ArrayList<>();
        List<String> values = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR, getIep(), alias, getDictionary(), m_default_message_resource);
        for (String value : values) {
            if (!StringUtils.isEmpty(value)) {
                data.add(value);
            }
        }
        return data;
    }

    /**
     * Gets the alias as java type.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param dictionary DataDictionary
     * @return Object
     * @throws X2BaseException exception
     */
    private Object getAliasAsJavaType(X2BaseBean bean, String alias, DataDictionary dictionary) throws X2BaseException {
        Object value = null;
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field == null) {
            throw new X2RuntimeException(
                    new IllegalStateException("The alias [" + alias + "] could not be found."));
        }
        value = WebUtils.getProperty(bean, field.getJavaName());

        if (value instanceof String) {
            // Get a SystemStringConverter for the field and convert the value from a string
            // value to java type.
            SystemStringConverter converter = getStringConverter(field);
            if (converter != null) {
                value = converter.parseSystemString((String) value);
            }

        }
        return value;
    }

    /**
     * Gets the exceptionality.
     *
     * @return String
     */
    private String getExceptionality() {
        Collection<IepDisability> disabilities = Collections.EMPTY_LIST;
        if (getIep() != null) {
            disabilities = getIepDisability();
        }
        if (disabilities == null || disabilities.isEmpty()) {
            return "";
        }
        DataDictionaryField field = getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                IepDisability.COL_DISABILITY_CODE);
        StringBuilder sb = new StringBuilder();
        for (IepDisability disability : disabilities) {
            if (!StringUtils.isEmpty(disability.getDisabilityCode())) {
                if (sb.length() > 0) {
                    sb.append(", ");
                }
                sb.append(translateCode(field, disability.getDisabilityCode()));
            }
        }
        return sb.toString();
    }

    /**
     * Gets the extended data dictionary by id.
     *
     * @param ddxId String
     * @return Extended data dictionary
     */
    private ExtendedDataDictionary getExtendedDataDictionaryById(String ddxId) {
        ExtendedDataDictionary extendedDataDictionary = null;
        X2Criteria ddxCriteria = new X2Criteria();

        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);

        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        extendedDataDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        return extendedDataDictionary;
    }

    /**
     * Gets the string converter.
     *
     * @param field DataDictionaryField
     * @return System string converter
     */
    private SystemStringConverter getStringConverter(DataDictionaryField field) {
        SystemStringConverter stringConverter = null;
        if (field.isString()) {
            Converter converter = ConverterFactory.getConverterForClass(
                    field.getEffectiveJavaType(),
                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                    field.isString());
            if (converter instanceof SystemStringConverter) {
                stringConverter = ((SystemStringConverter) converter);
            }
        }
        return stringConverter;
    }

    /**
     * Gets the view.
     *
     * @param ddf DataDictionaryField
     * @param value String
     * @return String
     */
    private String getView(DataDictionaryField ddf, String value) {
        String view = null;
        if (ddf.hasReferenceTable()) {
            MessageResources resources = getLocale() == null ? null
                    : LocalizationCache.getMessages(m_userData.getPersistenceKey(),
                            getLocale().toString(),
                            true);
            ReferenceCodeRetriever codeRetriever =
                    ReferenceCodeRetriever.getInstance(ddf.getReferenceTable().getPersistenceKey());

            ReferenceCodeCriteria codeCriteria =
                    new ReferenceCodeCriteria(ddf.getReferenceTable(),
                            ReferenceCodeRetriever.EXCLUDE_HIDDEN_CODES);
            codeCriteria.addFieldCriteria(ReferenceCode.COL_CODE, value);

            ModelBroker broker = new ModelBroker(m_userData.getPrivilegeSet());

            ReferenceCode code = codeRetriever.getCode(codeCriteria, m_userData, broker);

            if (code != null) {
                view = ReferenceFieldFormatter.getReferenceTableViewFields(ddf.getReferenceTable(), code, broker,
                        resources);
            }
        }
        return view;
    }


    /**
     * Look up the placement reference description.
     */
    private void loadPlacementReference() {
        if (getIep() != null) {
            String placement = (String) getIep().getFieldValueByAlias(ALIAS_IPRC_PLACEMENT, getDictionary());
            DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPRC_PLACEMENT);
            if (!StringUtils.isEmpty(placement) && field != null && field.hasReferenceTable()) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_CODE, placement);
                ReferenceCode rcd = getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
                if (rcd != null && !StringUtils.isEmpty(rcd.getDescription())) {
                    placement = rcd.getDescription();
                }
            }
            addParameter(PARAM_IPRC_PLACEMENT, placement);
        }
    }

    /**
     * Populate two column grid.
     *
     * @param parameterName String
     * @param itemsOne List<String>
     * @param itemsTwo List<String>
     * @param minRows int
     */
    private void populateTwoColumnGrid(String parameterName,
                                       List<String> itemsOne,
                                       List<String> itemsTwo,
                                       int minRows) {
        ReportDataGrid dataGrid = new ReportDataGrid();
        for (String item : itemsOne) {
            dataGrid.append();
            dataGrid.set(FIELD_COLUMN_1, item);
        }
        dataGrid.beforeTop();

        for (String item : itemsTwo) {
            if (dataGrid.isBottom()) {
                dataGrid.append();
            } else {
                dataGrid.next();
            }
            dataGrid.set(FIELD_COLUMN_2, item);
        }

        ensureMinimumRows(dataGrid, minRows);

        dataGrid.beforeTop();
        addParameter(parameterName, dataGrid);
    }

    /**
     * Translate code.
     *
     * @param field DataDictionaryField
     * @param code String
     * @return Object
     */
    private Object translateCode(DataDictionaryField field, String code) {
        String value = code;
        if (field.hasReferenceTable()) {
            String view = getView(field, code);
            if (!StringUtils.isEmpty(view)) {
                value = view;
            }
        }
        return value;
    }

}
