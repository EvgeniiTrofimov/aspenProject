/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
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
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.struts.util.MessageResources;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class MinistryReportSectionData extends ReportJavaSourceNet {

    /**
     * The Class Record.
     */
    private static class Record implements Comparable<Record> {
        private Map<Field, Object> m_fieldValuePairs = new HashMap<Field, Object>();

        private enum Field {
            //
            annualIPRCDate,
            //
            boardName,
            //
            boardNumber,
            //
            currentDate,
            //
            format,
            //
            fullySelfContained,
            //
            gender,
            //
            grade,
            //
            iep,
            //
            iepDate,
            //
            indirectService,
            //
            mainExceptionality,
            //
            oen,
            //
            partiallyIntegrated,
            //
            programType,
            //
            resourceAssistance,
            //
            schoolName,
            //
            sepReceivingDate,
            //
            studentName,
            //
            withdrawlAssistance
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.boardName.toString(),
                        Field.boardNumber.toString(),
                        Field.schoolName.toString(),
                        Field.studentName.toString(),
                        Field.oen.toString());

        /**
         * Gets the.
         *
         * @param key Field
         * @return Object
         */
        Object get(Field key) {
            return m_fieldValuePairs.get(key);
        }

        /**
         * Sets the.
         *
         * @param field Field
         * @param value Object
         */
        void set(Field field, Object value) {
            m_fieldValuePairs.put(field, value);
        }


        /**
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(Record o) {
            if (o == null) {
                return -1;
            }
            if (o.m_fieldValuePairs.get(Record.Field.studentName) == null) {
                return -1;
            }
            String studentName = (String) o.m_fieldValuePairs.get(Record.Field.studentName);
            return -1 * studentName.compareTo((String) m_fieldValuePairs.get(Record.Field.studentName));
        }
    }

    /**
    *
    */
    private static final long serialVersionUID = 1L;

    private static final String DDX_IEP = "ON-SPED-IEP";
    private static final String PRINT_FOOTER_REPORT_PARAM = "printFooter";

    private DataDictionary m_dictionary;
    private SimpleDateFormat m_formatter;
    private Map<Object, ReferenceCode> m_exceptionalityCodes;
    private SimpleDateFormat m_sysFormatter;
    private Map<String, List<Record>> m_seaClaimsRecords = new TreeMap<>();

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
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        addParameter("schoolYear",
                Integer.toString(getCurrentContext().getSchoolYear() - 1) + "-" +
                        Integer.toString(getCurrentContext().getSchoolYear()));
        m_seaClaimsRecords = getRecords();

        Set<String> schoolNames = m_seaClaimsRecords.keySet();
        for (String schoolName : schoolNames) {
            List<Record> records = m_seaClaimsRecords.get(schoolName);
            Collections.sort(records);
            for (Record record : records) {
                grid.append();
                for (Record.Field field : Record.Field.values()) {
                    grid.set(field.toString(), record.m_fieldValuePairs.get(field));
                }
                grid.set(PRINT_FOOTER_REPORT_PARAM, false);
            }
            grid.set(PRINT_FOOTER_REPORT_PARAM, true);
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_formatter = new SimpleDateFormat("dd MMM yyyy");
        m_sysFormatter = new SimpleDateFormat("yyyy-MM-dd");
        loadExceptionalityCodes();

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
                MessageResources messages =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey(), loc.getLocale());
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
     * Gets the report records of current school
     *
     * @param school SisSchool
     * @return Filterable
     */
    private Map<String, List<Record>> getRecords() {

        String spedActiveCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
        Collection<String> enrActiveCodes = StudentManager.getActiveStudentCodeList(getOrganization());

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_ENROLLMENT_STATUS, enrActiveCodes);
        criteria.addEqualTo(SisStudent.COL_SPED_STATUS_CODE, spedActiveCode);
        if (isSchoolContext()) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }
        BeanQuery query = new BeanQuery(SisStudent.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        Map<String, List<Record>> records = new HashMap<String, List<Record>>();
        Calendar currentDate = Calendar.getInstance();
        String reportFormat = String.valueOf(getParameter(Record.Field.format.toString()));
        String reportDate = m_formatter.format(currentDate.getTime());
        String sepReceivingDate;
        if (m_user_locale.equals(Locale.CANADA_FRENCH) || m_user_locale.equals(Locale.FRANCE)
                || m_user_locale.equals(Locale.FRENCH)) {
            sepReceivingDate = currentDate.get(Calendar.DATE) +
                    " " + currentDate.getDisplayName(Calendar.MONTH, Calendar.LONG, m_user_locale) + ", "
                    + currentDate.get(Calendar.YEAR);
        } else {
            sepReceivingDate = currentDate.getDisplayName(Calendar.MONTH, Calendar.LONG, m_user_locale) +
                    " " + currentDate.get(Calendar.DATE) + ", " + currentDate.get(Calendar.YEAR);
        }
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                IepData iep = student.getActiveIep();
                if (iep != null) {
                    String schoolName = student.getSchool().getName();
                    IepDisability disability = iep.getPrimaryDisability();
                    String placement = (String) iep.getFieldValueByAlias("iep-iprc-placement-decision", m_dictionary);
                    if (placement == null) {
                        placement = "";
                    }
                    String exceptionality = null;
                    String exceptionalityCode = null;
                    ReferenceCode exRefCode = null;
                    if (disability != null) {
                        exceptionality = disability.getDisabilityCode();
                        exRefCode = m_exceptionalityCodes.get(exceptionality);
                        if (exRefCode != null) {
                            exceptionalityCode = exRefCode.getLocalCode();
                        }
                    }
                    if (StringUtils.isEmpty(exceptionalityCode)) {
                        exceptionalityCode = exceptionality;
                    }
                    if (exceptionalityCode == null) {
                        exceptionalityCode = "";
                    }

                    Record record = new Record();
                    record.set(Record.Field.currentDate, reportDate);
                    record.set(Record.Field.schoolName, schoolName);
                    record.set(Record.Field.annualIPRCDate, getDateField(iep, "iep-date-iprc", m_dictionary));
                    record.set(Record.Field.boardName, student.getOrganization1().getName());
                    record.set(Record.Field.boardNumber, student.getOrganization1().getId());
                    record.set(Record.Field.gender, student.getPerson().getGenderCode());
                    record.set(Record.Field.grade, student.getGradeLevel());
                    // TODO: compare to REF Local code instead of hard code.
                    record.set(Record.Field.fullySelfContained, Boolean.valueOf(placement.contains("Full-Time")));
                    record.set(Record.Field.resourceAssistance, Boolean.valueOf(placement.contains("Resource")));
                    record.set(Record.Field.partiallyIntegrated, Boolean.valueOf(placement.contains("Partial")));
                    record.set(Record.Field.indirectService, Boolean.valueOf(placement.contains("Indirect")));
                    record.set(Record.Field.withdrawlAssistance, Boolean.valueOf(placement.contains("Withdraw")));
                    record.set(Record.Field.programType,
                            iep.getFieldValueByAlias("iep-program-placement-type", m_dictionary));
                    record.set(Record.Field.mainExceptionality, exceptionalityCode);
                    record.set(Record.Field.studentName, student.getNameView());
                    record.set(Record.Field.oen, student.getFieldValueByAlias("all-std-Oen"));
                    record.set(Record.Field.sepReceivingDate, sepReceivingDate);
                    record.set(Record.Field.iepDate, getDateField(iep, "startDate", m_dictionary));
                    record.set(Record.Field.iep, iep == null ? "" : "iep");
                    record.set(Record.Field.format, reportFormat);

                    List<Record> schoolList = records.get(schoolName);
                    if (schoolList == null) {
                        schoolList = new ArrayList<Record>();
                        records.put(schoolName, schoolList);
                    }
                    schoolList.add(record);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return records;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

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

    private String getDateField(X2BaseBean bean, String alias, DataDictionary dictionary) {
        Object fieldValue = null;
        String result = null;
        try {
            fieldValue = bean.getFieldValueByAlias(alias, dictionary);
        } catch (RuntimeException re) {
            // bad field name.
        }
        if (fieldValue == null) {
            try {
                fieldValue = bean.getFieldValueByBeanPath(alias);
            } catch (RuntimeException re) {
                // bad field name.
            }
        }
        if (fieldValue instanceof String) {
            try {
                fieldValue = m_sysFormatter.parse((String) fieldValue);
            } catch (ParseException pe) {
                // bad date format.
            }
        }
        if (fieldValue instanceof Date) {
            result = m_formatter.format(fieldValue);
        }
        return result;
    }

    /**
     * Load a map of Reference Codes for the Exceptionality reference table.
     */
    private void loadExceptionalityCodes() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_IEP);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(query);
        m_dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
        DataDictionaryField field =
                m_dictionary.findDataDictionaryField(IepDisability.class.getName(), IepDisability.COL_DISABILITY_CODE);
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                m_exceptionalityCodes = CollectionUtils.getPropertyMap(codes, ReferenceCode.COL_CODE);
            }
        }
    }
}
