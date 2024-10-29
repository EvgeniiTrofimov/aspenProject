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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class MeetingInvitationData.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class MeetingInvitationData extends OnBaseFormReportJavaSource {
    // alias definitions
    private static final String ALIAS_IEP_TEACHER = "igl-course-staff";// TODO
    private static final String ALIAS_IMG_CATEGORY = "img-category";

    // ReportDataGrid column constants for the main report
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PARAMETER_MAP = "parameters";

    // IMG literals
    private static final String IMG_CATEGORY_IEP = "IEP";
    private static final String IMG_CATEGORY_IPRC = "IPRC";
    private static final List<String> IMG_CATEGORIES = Arrays.asList(IMG_CATEGORY_IEP, IMG_CATEGORY_IPRC);

    public static final String PAGE_FORMAT_ID_IEP_INVITATION = "SYS-SPED-ON-MTG-SUB1";
    public static final String PAGE_FORMAT_ID_IPRC_INVITATION = "SYS-SPED-ON-MTG-SUB2";
    public static final String PAGE_FORMAT_ID_IPRC_RESPONSE = "SYS-SPED-ON-MTG-SUB3";
    private static final List<String> PAGE_FORMAT_IDS =
            Arrays.asList(PAGE_FORMAT_ID_IEP_INVITATION, PAGE_FORMAT_ID_IPRC_INVITATION, PAGE_FORMAT_ID_IPRC_RESPONSE);

    public static final String PARAM_MEMBER = "member";
    public static final String PARAM_PARENTS = "parents";
    public static final String PARAM_YEAR = "year";

    private static final List<String> VALID_RELATION_CODES = new ArrayList<String>();

    static {
        VALID_RELATION_CODES.add("Mother");
        VALID_RELATION_CODES.add("Father");
        VALID_RELATION_CODES.add("Legal Guardian");
    }

    private int m_currentPageNumber = 0;
    private Map m_subReports = null;

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

        ReportDataGrid grid = new ReportDataGrid();
        IepMeeting meeting = (IepMeeting) getFormStorage();
        String meetingCategory = (String) meeting.getFieldValueByAlias(ALIAS_IMG_CATEGORY, getDictionary());
        loadSubReports();

        if (isBlank() || !IMG_CATEGORIES.contains(meetingCategory)) {
            IepMeetingAttendance attendanceBean = new IepMeetingAttendance(getBroker().getPersistenceKey());
            preparePage(grid, attendanceBean, PAGE_FORMAT_ID_IEP_INVITATION);
            preparePage(grid, attendanceBean, PAGE_FORMAT_ID_IPRC_INVITATION);
            preparePage(grid, attendanceBean, PAGE_FORMAT_ID_IPRC_RESPONSE);
        } else {
            addParameter(PARAM_PARENTS, getParentsData(meeting.getStudent()));
            IepData iepData = meeting.getIepData();
            String yearParam = getCurrentContext().getContextId();
            addParameter(PARAM_YEAR, yearParam);
            /*
             * Add invitations
             */
            X2Criteria attendanceCriteria = new X2Criteria();
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_PRINT_INVITATION_INDICATOR, Boolean.TRUE);

            QueryByCriteria attendanceQuery = new QueryByCriteria(IepMeetingAttendance.class, attendanceCriteria);
            attendanceQuery.addOrderByAscending(IepMeetingAttendance.REL_TEAM_MEMBER + "." +
                    IepTeamMember.COL_NAME_VIEW);

            try (QueryIterator attendance = getBroker().getIteratorByQuery(attendanceQuery)) {
                while (attendance.hasNext()) {
                    IepMeetingAttendance attendanceBean = (IepMeetingAttendance) attendance.next();
                    if (IMG_CATEGORY_IEP.equals(meetingCategory)) {
                        preparePage(grid, attendanceBean, PAGE_FORMAT_ID_IEP_INVITATION);
                    } else if (IMG_CATEGORY_IPRC.equals(meetingCategory)) {
                        preparePage(grid, attendanceBean, PAGE_FORMAT_ID_IPRC_INVITATION);
                        preparePage(grid, attendanceBean, PAGE_FORMAT_ID_IPRC_RESPONSE);
                    }
                }
            }
        }

        grid.beforeTop();
        return grid;
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
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
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
     * Retrieves student's parent/guardians names
     *
     * @param student
     * @return
     */
    private String getParentsData(SisStudent student) {
        String parentName = "";
        String and = " and ";
        if (getLocale().getLanguage().equals("fr")) {
            and = " et ";
        }

        Collection<StudentContact> contacts = student.getContacts();
        for (StudentContact contact : contacts) {
            if (contact.getContact() != null && VALID_RELATION_CODES.contains(contact.getRelationshipCode())) {
                SisPerson person = (SisPerson) contact.getContact().getPerson();
                if (StringUtils.isEmpty(parentName)) {
                    parentName = person.getFirstName() + " " + person.getLastName();
                } else {
                    parentName = parentName + and + person.getFirstName() + " " + person.getLastName();
                }

            }
        }

        return parentName;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String pageId) {
        Report report = (Report) m_subReports.get(pageId);
        return report.getCompiledFormat();
    }

    /**
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        X2Criteria criteria = new X2Criteria();
        // criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getRootOrganization().getOid());
        criteria.addIn(Report.COL_ID, PAGE_FORMAT_IDS);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Prepare page.
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     * @param reportFormatId String
     */
    private void preparePage(ReportDataGrid grid, IepMeetingAttendance attendanceBean, String reportFormatId) {
        JRDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        Map<String, Object> parameterMap = new HashMap<String, Object>();
        parameterMap.put(PARAM_MEMBER, attendanceBean.getTeamMember());
        parameterMap.putAll(getParameters());

        grid.append();
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(reportFormatId));
        grid.set(COL_PARAMETER_MAP, parameterMap);
        grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
        grid.set(COL_PAGE_IDENTIFIER, "IEP");
    }

}
