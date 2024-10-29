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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;
import net.sf.jasperreports5.engine.JRException;
import net.sf.jasperreports5.engine.data.JRBeanCollectionDataSource;
import net.sf.jasperreports5.engine.util.JRLoader;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class IprcFormData.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class IprcFormData extends OnBaseFormReportJavaSource {
    private static final String ALIAS_IEP_PROGRAM = "iep-program";
    private static final String ALIAS_IEP_PROGRAM_LOCATION = "iep-program-location";
    private static final String ALIAS_IMA_ADDITIONAL_NAME = "ima-additionalName";
    private static final String ALIAS_IMA_ADDITIONAL_ROLE = "ima-additionalRole";
    private static final String ALIAS_IMG_CATEGORY = "img-category";
    private static final String ALIAS_IPRC_DOCUMENTATION = "iep-iprc-documentation";
    private static final String ALIAS_IPRC_MEMBER_INDICATOR = "itm-iprc-member-indicator";
    private static final String ALIAS_IPRC_STD_NEEDS = "iep-iprc-std-needs";
    private static final String ALIAS_IPRC_STD_NEEDS_OTHER = "iep-iprc-std-needs-oth";
    private static final String ALIAS_IPRC_STD_STRENGTHS = "iep-iprc-std-strengths";
    private static final String ALIAS_IPRC_STD_STRENGTHS_OTHER = "iep-iprc-std-strengths-oth";
    private static final String ALIAS_IPRC_PLACEMENT = "iep-iprc-placement-decision";
    private static final String ALIAS_IPRC_PREV_DECISION = "iep-iprc-prv-decision";
    private static final String ALIAS_IPRC_PREV_PLACEMENT = "iep-iprc-prv-placement";
    private static final String ALIAS_IPRC_PREV_IDENTIFICATION = "iep-iprc-prv-identification";
    private static final String ALIAS_IPRC_TYPE = "iep-iprc-type";

    private static final String KEY_EXCEPTIONAL = "rpt.Exceptional";
    private static final String KEY_NON_EXCEPTIONAL = "rpt.Non.Exceptional";
    private static final String KEY_NOT_IDENTIFIED = "rpt.Not.Identified";

    private static final String IMG_CATEGORY_IPRC = "IPRC";

    private static final String PARAM_COMMITTEE_MEMBERS = "committeeMembers";
    private static final String PARAM_COMMITTEE_MEMBERS_TITLES = "committeeMembersTitles";
    private static final String PARAM_EXCEPTIONALITIES = "exceptionalities";
    private static final String PARAM_EXCEPTIONALITY_PRIMARY = "primaryExceptionality";
    private static final String PARAM_MEETING_ATTENDEES = "meetingAttendees";
    private static final String PARAM_MEETING_ATTENDEES_ROLES = "meetingAttendeesRoles";
    private static final String PARAM_PRINT_DEFINITIONS = "printDefinitions";
    private static final String PARAM_PROGRAM_LOCATION = "programLocation";
    private static final String PARAM_PROGRAM_NAME = "programName";
    private static final String PARAM_IPRC_PLACEMENT = "currentPlacement";
    private static final String PARAM_SUB_GRID_STRENGTHS_DATA = "strengthsDataGrid";
    private static final String PARAM_IPRC_PREV_DECISION = "prevDecision";
    private static final String PARAM_IPRC_PREV_PLACEMENT = "prevPlacement";
    private static final String PARAM_IPRC_PREV_IDENTIFICATION = "prevIdentification";

    private static final int MIN_ROWS_STRENGTHS_DATA = 1;
    private static final String FIELD_COLUMN_1 = "column1";
    private static final String FIELD_COLUMN_2 = "column2";

    private static final Pattern PATTERN_SPLIT_BY_COMMA = Pattern.compile("\\s*,\\s*");
    private static final Pattern PATTERN_SPLIT_BY_CR = Pattern.compile("\\r\\n");

    private static final String INPUT_PARAM_PRINT_DEFINITIONS = "printDefinitions";

    protected static final List<KeyValuePair<String, String>> SUB_REPORT_FORMATS =
            Arrays.asList(new KeyValuePair<String, String>("SYS-SPED-ON-IEP-SUB2", "twoColumnDataFormat2"));

    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
    private String m_ownerStaffOid;
    private Locale m_userLocale;
    private String m_userLocaleLanguage;
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

        initSubReports();
        Boolean printDefs = (Boolean) getParameter(INPUT_PARAM_PRINT_DEFINITIONS);
        addParameter(PARAM_PRINT_DEFINITIONS, printDefs == null ? Boolean.TRUE : printDefs);
        addParameter(ALIAS_IPRC_TYPE, getIprcTypeIndicator());
        addParameter(ALIAS_IPRC_DOCUMENTATION, getStringTranslationFromAlias(PATTERN_SPLIT_BY_COMMA,
                getIep(), ALIAS_IPRC_DOCUMENTATION, getDictionary(), m_default_message_resource));

        loadCommitteeMembers();
        loadExceptionalities();
        loadMeetingAttendees();
        loadNeedsAndStrengths();
        loadProgramLocation();
        loadPreviousStatus();
        // adjustMeetingAttendanceAndCommitteeMembersTables();
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_ownerStaffOid = userData.getStaffOid();
        m_userLocale = userData.getLocale();
        m_userLocaleLanguage = userData.getLocale().getDisplayLanguage();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        // Enabling localization
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

        if (m_userLocale != null) {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_userLocale);
        } else {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
        }

        if (StringUtils.isBlank(m_userLocaleLanguage)) {
            m_userLocaleLanguage = LocalizationCache.getCurrentLocale().getDisplayLanguage();
        }


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
    }

    /**
     * Gets the title.
     *
     * @return String
     */
    @Override
    protected String getTitle() {
        return "Identification, Placement and Review Committee\nIPRC Statement of Decision";
    }

    /**
     * Inits the sub reports.
     */
    private void initSubReports() {
        for (KeyValuePair<String, String> item : SUB_REPORT_FORMATS) {
            try {
                byte[] compiledFormat = ReportUtils.getReport(item.getKey(), getBroker()).getCompiledFormat();
                Object loadedJRReport = JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
                addParameter(item.getValue(), loadedJRReport);
            } catch (JRException e) {
                String message = "ERROR: Loading subreport for '" + item.getValue() + "' from report " + item.getKey();
                message += "\n" + e.getMessage();
                this.addCustomErrorMessage(message);
            }
        }
    }

    /**
     * Fill in empty data to dataSource according to count.
     *
     * @param count
     * @param dataSource
     * @return
     */
    private JRBeanCollectionDataSource addEmptyDataToJRBeanCollectionDataSource(int count,
                                                                                JRBeanCollectionDataSource dataSource) {
        Collection<String> data = dataSource.getData() == null || dataSource.getData().isEmpty() ? new ArrayList<>()
                : (Collection<String>) dataSource.getData();
        for (int i = 0; i < count; i++) {
            data.add("");
        }
        return new JRBeanCollectionDataSource(data);
    }

    /**
     * Fill in data to 'meetingAttendees' and 'committeeMembers' datasources
     * in order to have table view
     */
    private void adjustMeetingAttendanceAndCommitteeMembersTables() {
        JRBeanCollectionDataSource meetingAttendeesSource =
                (JRBeanCollectionDataSource) getParameter(PARAM_MEETING_ATTENDEES);
        JRBeanCollectionDataSource committeeMembersSource =
                (JRBeanCollectionDataSource) getParameter(PARAM_COMMITTEE_MEMBERS);
        JRBeanCollectionDataSource meetingAttendesRolesSourse =
                (JRBeanCollectionDataSource) getParameter(PARAM_MEETING_ATTENDEES_ROLES);
        JRBeanCollectionDataSource committeeMembersTitlesSource =
                (JRBeanCollectionDataSource) getParameter(PARAM_COMMITTEE_MEMBERS_TITLES);
        int meetingAttendancesRecordsCount = meetingAttendeesSource.getRecordCount();
        int committeeMembersCount = committeeMembersSource.getRecordCount();

        if (meetingAttendancesRecordsCount > committeeMembersCount) {
            addParameter(PARAM_COMMITTEE_MEMBERS, addEmptyDataToJRBeanCollectionDataSource(
                    meetingAttendancesRecordsCount - committeeMembersCount, committeeMembersSource));
            addParameter(PARAM_COMMITTEE_MEMBERS_TITLES, addEmptyDataToJRBeanCollectionDataSource(
                    meetingAttendancesRecordsCount - committeeMembersCount, committeeMembersTitlesSource));
        } else if (meetingAttendancesRecordsCount < committeeMembersCount) {
            addParameter(PARAM_MEETING_ATTENDEES, addEmptyDataToJRBeanCollectionDataSource(
                    committeeMembersCount - meetingAttendancesRecordsCount, meetingAttendeesSource));
            addParameter(PARAM_MEETING_ATTENDEES_ROLES, addEmptyDataToJRBeanCollectionDataSource(
                    committeeMembersCount - meetingAttendancesRecordsCount, meetingAttendesRolesSourse));
        } else {
            if (meetingAttendancesRecordsCount < 4) {
                int diff = 4 - meetingAttendancesRecordsCount;
                addParameter(PARAM_COMMITTEE_MEMBERS,
                        addEmptyDataToJRBeanCollectionDataSource(diff, committeeMembersSource));
                addParameter(PARAM_COMMITTEE_MEMBERS_TITLES,
                        addEmptyDataToJRBeanCollectionDataSource(diff, committeeMembersTitlesSource));
                addParameter(PARAM_MEETING_ATTENDEES,
                        addEmptyDataToJRBeanCollectionDataSource(diff, meetingAttendeesSource));
                addParameter(PARAM_MEETING_ATTENDEES_ROLES,
                        addEmptyDataToJRBeanCollectionDataSource(diff, meetingAttendesRolesSourse));
            }
        }
    }

    /**
     * Gets the alias as list.
     *
     * @param alias String
     * @return List
     */
    private String getIprcTypeIndicator() {
        String value = null;
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPRC_TYPE);
        IepData iep = getIep();
        if (field != null && iep != null) {
            String fieldValue = (String) iep.getFieldValueByBeanPath(field.getJavaName());
            ReferenceTable refTbl = field.getReferenceTable();
            if (refTbl != null && fieldValue != null) {
                Collection<ReferenceCode> codes = refTbl.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    if (fieldValue.equals(code.getCode())) {
                        value = code.getStateCode();
                        break;
                    }
                }
            }
        }
        return value;
    }

    /**
     * Load committee members.
     */
    private void loadCommitteeMembers() {
        List<KeyValuePair> items = new ArrayList<KeyValuePair>();

        if (getIep() != null) {
            DataDictionaryField ddfMemb = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IPRC_MEMBER_INDICATOR);
            DataDictionaryField ddfCat = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IMG_CATEGORY);
            if (ddfMemb == null) {
                throw new IllegalStateException(
                        "The alias " + ALIAS_IPRC_MEMBER_INDICATOR + " must be defined on IEP_TEAM_MEMBER");
            }
            if (ddfCat == null) {
                throw new IllegalStateException("The alias " + ALIAS_IMG_CATEGORY + " must be defined on IEP_MEETING");
            }
            DataDictionaryField roleField = getDictionary().findDataDictionaryField(IepTeamMember.class.getName(),
                    IepTeamMember.COL_MEMBER_ROLE_CODE);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepMeetingAttendance.COL_IEP_DATA_OID, getIep().getIepDataOid());
            criteria.addEqualTo(IepMeetingAttendance.REL_IEP_MEETING + PATH_DELIMITER +
                    ddfCat.getJavaName(), IMG_CATEGORY_IPRC);
            criteria.addEqualTo(IepMeetingAttendance.REL_TEAM_MEMBER + PATH_DELIMITER +
                    ddfMemb.getJavaName(), BooleanAsStringConverter.TRUE);

            QueryByCriteria query = new QueryByCriteria(IepMeetingAttendance.class, criteria);
            query.addOrderBy(IepMeetingAttendance.REL_TEAM_MEMBER + PATH_DELIMITER + IepTeamMember.COL_NAME_VIEW, true);
            for (IepMeetingAttendance ima : (Collection<IepMeetingAttendance>) getBroker()
                    .getCollectionByQuery(query)) {
                IepTeamMember itm = ima.getTeamMember();
                if (itm != null) {
                    String name = itm.getNameView();
                    if (ima.getHideNameIndicator()) {
                        name = "";
                    }
                    KeyValuePair item = new KeyValuePair(name, translateCode(roleField, itm.getMemberRoleCode()));
                    items.add(item);
                }
            }
        }
        addParameter(PARAM_COMMITTEE_MEMBERS, new JRBeanCollectionDataSource(items));
        // addParameter(PARAM_COMMITTEE_MEMBERS_TITLES, new JRBeanCollectionDataSource(titles));
    }

    /**
     * Gets the exceptionality.
     *
     * @return String
     */
    private void loadExceptionalities() {
        Map<String, String> map = new HashMap();
        StringBuilder builder = new StringBuilder();
        Collection<IepDisability> disabilities = getIepDisability();
        DataDictionaryField field =
                getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                        IepDisability.COL_DISABILITY_CODE);
        Map<String, ReferenceCode> rcdMap = new HashMap();
        if (field != null && field.hasReferenceTable()) {
            for (ReferenceCode code : field.getReferenceTable().getReferenceCodes()) {
                rcdMap.put(code.getCode(), code);
            }
        }
        if (disabilities != null && !disabilities.isEmpty()) {
            // When building a concatinated list of disabilities,
            // show the primary disability first, then the rest.
            for (IepDisability disability : disabilities) {
                if (disability.getPrimaryIndicator()) {
                    String description = translateCode(field, disability.getDisabilityCode());
                    if ("en_US".equals(getLocale().toString())) {
                        // Core bug, translations will not be found in promary locale and will
                        // return the Code
                        // as default rather than the ref table view fields (description).
                        String code = disability.getDisabilityCode();
                        ReferenceCode rcd = rcdMap.get(code);
                        if (rcd != null) {
                            description = rcd.getDescription();
                        }
                    }

                    if (!StringUtils.isEmpty(description)) {
                        if (builder.length() > 0) {
                            builder.append("\n");
                        }
                        builder.append(description).append(" (P)");
                    }
                }
            }
            for (IepDisability disability : disabilities) {
                String code = disability.getDisabilityCode();
                String description = translateCode(field, disability.getDisabilityCode());
                ReferenceCode rcd = rcdMap.get(code);
                if ("en_US".equals(getLocale().toString())) {
                    // Core bug, translations will not be found in promary locale and will return
                    // the Code
                    // as default rather than the ref table view fields (description).
                    if (rcd != null) {
                        description = rcd.getDescription();
                    }
                }
                if (rcd != null) {
                    code = rcd.getStateCode();
                }
                map.put(code, description);

                if (!disability.getPrimaryIndicator()) {
                    if (builder.length() > 0) {
                        builder.append("\n");
                    }
                    builder.append(description);
                }
            }
        }
        addParameter(PARAM_EXCEPTIONALITY_PRIMARY, builder.toString());
        addParameter(PARAM_EXCEPTIONALITIES, map);
    }

    /**
     * Load meeting attendees.
     */
    private void loadMeetingAttendees() {
        List<KeyValuePair> items = new ArrayList<KeyValuePair>();

        if (getIep() != null) {
            DataDictionaryField ddf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IMG_CATEGORY);
            if (ddf == null) {
                throw new IllegalStateException("The alias " + ALIAS_IMG_CATEGORY + " must be defined");
            }
            DataDictionaryField roleField = getDictionary().findDataDictionaryField(IepTeamMember.class.getName(),
                    IepTeamMember.COL_MEMBER_ROLE_CODE);

            ModelProperty nameModel = null;
            DataDictionaryField nameDdf = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IMA_ADDITIONAL_NAME);
            if (nameDdf != null) {
                nameModel = new ModelProperty(nameDdf, getDictionary());
            }
            int anonymousNameCounter = 0;

            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepMeetingAttendance.COL_IEP_DATA_OID, getIep().getIepDataOid());
            criteria.addEqualTo(IepMeetingAttendance.REL_IEP_MEETING + PATH_DELIMITER + ddf.getJavaName(),
                    IMG_CATEGORY_IPRC);
            QueryByCriteria query = new QueryByCriteria(IepMeetingAttendance.class, criteria);
            for (IepMeetingAttendance ima : (Collection<IepMeetingAttendance>) getBroker()
                    .getCollectionByQuery(query)) {
                if (ima.getTeamMember() != null) {
                    if (ima.getPresentIndicator()) {
                        String name = ima.getTeamMember().getNameView();
                        if (ima.getHideNameIndicator()) {
                            name = "";
                        }
                        KeyValuePair item = new KeyValuePair(name,
                                translateCode(roleField, ima.getTeamMember().getMemberRoleCode()));
                        items.add(item);
                    }
                } else {
                    String name = (String) ima.getFieldValueByAlias(ALIAS_IMA_ADDITIONAL_NAME, getDictionary());
                    if (StringUtils.isEmpty(name)) {
                        if (nameModel != null) {
                            name = WebUtils.getLabel(nameModel, true, false, false, getLocale())
                                    + " " + Integer.toString(++anonymousNameCounter);
                        }
                        if (StringUtils.isEmpty(name)) {
                            name = "Attendee " + Integer.toString(++anonymousNameCounter);
                        }
                    }
                    if (ima.getHideNameIndicator()) {
                        name = "";
                    }
                    KeyValuePair item = new KeyValuePair(name,
                            ima.getFieldValueByAlias(ALIAS_IMA_ADDITIONAL_ROLE, getDictionary()));
                    items.add(item);
                }
            }
        }

        addParameter(PARAM_MEETING_ATTENDEES, new JRBeanCollectionDataSource(items));
        // addParameter(PARAM_MEETING_ATTENDEES_ROLES, new
        // JRBeanCollectionDataSource(data.values()));

    }

    private void loadNeedsAndStrengths() {
        List<String> strengths = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA,
                getIep(), ALIAS_IPRC_STD_STRENGTHS, getDictionary(), m_default_message_resource);
        strengths.addAll(getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR,
                getIep(), ALIAS_IPRC_STD_STRENGTHS_OTHER, getDictionary(), m_default_message_resource));
        Collections.sort(strengths);
        List<String> needs = getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_COMMA,
                getIep(), ALIAS_IPRC_STD_NEEDS, getDictionary(), m_default_message_resource);
        needs.addAll(getCollectionTranslationFromAlias(PATTERN_SPLIT_BY_CR,
                getIep(), ALIAS_IPRC_STD_NEEDS_OTHER, getDictionary(), m_default_message_resource));
        Collections.sort(needs);
        populateTwoColumnGrid(PARAM_SUB_GRID_STRENGTHS_DATA, strengths, needs, MIN_ROWS_STRENGTHS_DATA);
    }


    /**
     * Load needs and strengths.
     * private void loadNeedsAndStrengths() {
     * List<String> studentNeeds = new ArrayList(getCollectionFromAlias(PATTERN_SPLIT_BY_COMMA,
     * getIep(),
     * ALIAS_IPRC_STD_NEEDS, getDictionary()));
     * studentNeeds.addAll(
     * getCollectionFromAlias(PATTERN_SPLIT_BY_CR, getIep(), ALIAS_IPRC_STD_NEEDS_OTHER,
     * getDictionary()));
     * Collections.sort(studentNeeds);
     * List<String> studentStrengths = new ArrayList(getCollectionFromAlias(PATTERN_SPLIT_BY_COMMA,
     * getIep(),
     * ALIAS_IPRC_STD_STRENGTHS, getDictionary()));
     * studentStrengths.addAll(
     * getCollectionFromAlias(PATTERN_SPLIT_BY_CR, getIep(), ALIAS_IPRC_STD_STRENGTHS_OTHER,
     * getDictionary()));
     * Collections.sort(studentStrengths);
     * if (studentNeeds.isEmpty() && studentStrengths.isEmpty()) {
     * for (int i = 0; i < 2; i++) {
     * studentNeeds.add("");
     * studentStrengths.add("");
     * }
     * } else {
     * int stdNeedsSize = studentNeeds.size();
     * int stdStrenghtsSize = studentStrengths.size();
     * if (stdNeedsSize > stdStrenghtsSize) {
     * for (int i = 0; i < stdNeedsSize - stdStrenghtsSize; i++) {
     * studentStrengths.add("");
     * }
     * } else if (stdStrenghtsSize > stdNeedsSize) {
     * for (int i = 0; i < stdStrenghtsSize - stdNeedsSize; i++) {
     * studentNeeds.add("");
     * }
     * }
     * }
     * if (studentNeeds.size() % 2 == 1) {
     * studentNeeds.add("");
     * studentStrengths.add("");
     * }
     *
     * addParameter(PARAM_STUDENT_NEEDS_COLUMN1,
     * new JRBeanCollectionDataSource(studentNeeds.subList(0, studentNeeds.size() / 2)));
     * addParameter(PARAM_STUDENT_NEEDS_COLUMN2,
     * new JRBeanCollectionDataSource(studentNeeds.subList(studentNeeds.size() / 2,
     * studentNeeds.size())));
     * addParameter(PARAM_STUDENT_STRENGHTS_COLUMN1,
     * new JRBeanCollectionDataSource(studentStrengths.subList(0, studentStrengths.size() / 2)));
     * addParameter(PARAM_STUDENT_STRENGHTS_COLUMN2,
     * new JRBeanCollectionDataSource(
     * studentStrengths.subList(studentStrengths.size() / 2, studentStrengths.size())));
     * }
     */

    /**
     * Load program location.
     */
    private void loadProgramLocation() {
        if (getIep() != null) {
            String program = (String) getIep().getFieldValueByAlias(ALIAS_IEP_PROGRAM, getDictionary());
            DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(ALIAS_IEP_PROGRAM);
            if (!StringUtils.isEmpty(program) && field != null && field.hasReferenceTable()) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                criteria.addEqualTo(ReferenceCode.COL_CODE, program);
                ReferenceCode rcd = getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
                if (rcd != null && !StringUtils.isEmpty(rcd.getDescription())) {
                    program = rcd.getDescription();
                }
            }

            String programLocation = "";
            String sklOid = (String) getIep().getFieldValueByAlias(ALIAS_IEP_PROGRAM_LOCATION, getDictionary());
            if (!StringUtils.isEmpty(sklOid)) {
                SisSchool skl = getBroker().getBeanByOid(SisSchool.class, sklOid);
                if (skl != null) {
                    programLocation = skl.getName();
                }
            }
            addParameter(PARAM_PROGRAM_NAME, program);
            addParameter(PARAM_PROGRAM_LOCATION, programLocation);

            String placement = (String) getIep().getFieldValueByAlias(ALIAS_IPRC_PLACEMENT, getDictionary());
            loadPlacementReference(placement, PARAM_IPRC_PLACEMENT);
        }
    }

    /**
     * Lookup the current identification and placement from the previous (active) Iep/Iprc.
     *
     * @param detail
     * @param userData
     * @param errorsList
     *
     * @return String[]
     */
    private void loadPreviousStatus() {
        String decision = null;
        String identification = null;
        String placement = null;

        IepData iep = getIep();
        if (iep != null) {
            // Lookup identification from an Active IEP.
            decision = m_default_message_resource.getMessage(m_userLocale,
                    getParameter(PARAM_PREFIX) + KEY_NOT_IDENTIFIED);
            if (decision == null) {
                decision = "Not Identified"; // No previous IEP/IPRC
            }
            SisStudent student = iep.getStudent();
            IepData activeIep = student.getActiveIep();
            if (activeIep != null) {
                decision = m_default_message_resource.getMessage(m_userLocale,
                        getParameter(PARAM_PREFIX) + KEY_NON_EXCEPTIONAL);
                if (decision == null) {
                    decision = "Non-Exceptional"; // Previous IEP/IPRC but no disability.
                }
                // For identification, copy previous primary Disability.
                Collection<IepDisability> disabilities = activeIep.getIepDisability(getBroker());
                for (IepDisability idb : disabilities) {
                    identification = idb.getDisabilityCode();
                    if (idb.getPrimaryIndicator()) {
                        break;
                    }
                }
                if (!StringUtils.isEmpty(identification)) {
                    DataDictionaryField field = getDictionary().findDataDictionaryField(IepDisability.class.getName(),
                            IepDisability.COL_DISABILITY_CODE);
                    if (field.hasReferenceTable()) {
                        X2Criteria criteria = new X2Criteria();
                        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                        criteria.addEqualTo(ReferenceCode.COL_CODE, identification);
                        ReferenceCode rcd =
                                getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
                        if (rcd != null && !StringUtils.isEmpty(rcd.getDescription())) {
                            identification = rcd.getDescription();
                        }
                    }
                }
                if (!StringUtils.isEmpty(identification)) {
                    decision = m_default_message_resource.getMessage(m_userLocale,
                            getParameter(PARAM_PREFIX) + KEY_EXCEPTIONAL);
                    if (decision == null) {
                        decision = "Exceptional"; // Previous IEP/IPRC with disability.
                    }
                } else {
                    identification = m_default_message_resource.getMessage(m_userLocale,
                            getParameter(PARAM_PREFIX) + KEY_NOT_IDENTIFIED);
                    if (identification == null) {
                        identification = "Not Identified";
                    }
                }

                // copy Placement from previous IEP/IPRC.
                if (activeIep != null) {
                    placement = (String) activeIep.getFieldValueByAlias(ALIAS_IPRC_PLACEMENT, getDictionary());
                }
            }
        }

        addParameter(PARAM_IPRC_PREV_DECISION, decision);
        addParameter(PARAM_IPRC_PREV_IDENTIFICATION, identification);
        loadPlacementReference(placement, PARAM_IPRC_PREV_PLACEMENT);
    }

    /**
     * Look up the placement reference description.
     *
     * @param placement
     * @param parameter
     */
    private void loadPlacementReference(String placement, String parameter) {
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
        addParameter(parameter, placement);
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
     * Translate code.
     *
     * @param field DataDictionaryField
     * @param code String
     * @return String
     */
    private String translateCode(DataDictionaryField field, String code) {
        String value = code;
        if (field.hasReferenceTable()) {
            String view = getView(field, code, m_ownerStaffOid, m_default_message_resource);
            if (!StringUtils.isEmpty(view)) {
                value = view;
            }
        }
        return value;
    }
}
