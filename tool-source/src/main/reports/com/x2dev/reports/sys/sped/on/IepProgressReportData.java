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
package com.x2dev.reports.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalProgress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Java source for the IEP progress report. This report is designed to run from 3 different areas,
 * each with differing results:
 * <p>
 * <table border="1">
 * <tr>
 * <th>Area</th>
 * <th>Result</th>
 * </tr>
 * <tr>
 * <td>IEP list</td>
 * <td>Progress records for all IEPs in the selection within the specified date range</td>
 * </tr>
 * <tr>
 * <td>Progress list</td>
 * <td>Progress records for the current IEP within the specified date range</td>
 * </tr>
 * <tr>
 * <td>Progress detail</td>
 * <td>The current progress record</td>
 * </tr>
 * </table>
 * <p>
 * The data source for this report is a <code>ReportDataGrid</code>. For each progress record, a row
 * is added for each text field displayed. This supports a "floating text" effect on the report
 * format.
 *
 * @author X2 Development Corporation
 */
public class IepProgressReportData extends ReportJavaSourceNet implements Publishable {
    private static final long serialVersionUID = 1L;

    /**
     * Optional parameter to filter the report by a specific school year.
     */
    public static final String CONTEXT_OID = "contextOid";

    /**
     * End date in the progress report date window. Only progress reports dated prior to and
     * including this date will be included.
     */
    public static final String END_DATE = "endDate";

    /**
     * boolean in the input window that when checked will include unposted reports in this report
     */
    public static final String INCLUDE_UNPOSTED = "includeUnposted";

    /**
     * Report parameter containing the reporting period to filter on. This parameter is optional.
     */
    public static final String REPORT_PERIOD = "reportPeriod";

    /**
     * Report parameter containing the total number of progress reports included.
     */
    public static final String REPORT_TOTALS = "reportTotals";

    /**
     * Oid of a specific staff member to filter by. Chosen on input screen.
     */
    public static final String STAFF_OID = "staffOid";

    /**
     * Start date in the progress report date window. Only progress reports dated after and
     * including this date will be included.
     */
    public static final String START_DATE = "startDate";

    private static final String COL_PROGRESS_BEAN = "progress";
    private static final String COL_PROGRESS_OID = "progressOid";
    private static final String COL_PROGRESS_CODE = "progressCode";
    private static final String COL_PROGRESS_DATE = "progressDate";
    private static final String COL_COURSE_DESCRIPTION = "courseDescription";
    private static final String COL_COURSE_TEACHER = "courseTeacher";
    private static final String COL_COMMENT = "comment";
    private static final String COL_STUDENT = "student";
    private static final String COL_STUDENT_DOB = "dob";
    private static final String COL_STUDENT_OID = "studentOid";

    private static final String ALIAS_INCLUDE_PROGRESS = "igl-include-progress";
    private static final String ALIAS_DELIVERY = "igl-course-delivery";
    private static final String ALIAS_DESCRIPTION = "igl-course-desc";
    private static final String ALIAS_STAFF_NAME = "igl-course-staff";
    private static final String ALIAS_COMMENT = "igp-comment";

    private static final String LOGO_CODE_ON_BOARD = "OnBoardLogo";
    private static final String PARAM_LOGO = "logoOntario";
    private static final String RTB_OID_ON_SIS_IMAGES = "rtbOnImage    ";// OnSIS Images
    private static final String ALIAS_RCD_IMAGE_BASE64 = "all-rcd-ImageBase64";

    // private static final String FIELD_ID_BASELINE = "baseline";
    // private static final String FIELD_ID_BENCHMARK = "benchmark";
    // private static final String FIELD_ID_GOAL = "goal";
    // private static final String FIELD_ID_PERFORMANCE_LEVEL = "performanceLevel";

    private static final String NO_PUBLISH_EMAIL_PARAM = "noEmail";
    private static final String PUBLISH_TO_PARAM = "publishTo";

    private Boolean m_noPublishEmailParam;
    private Map<String, List<StudentContact>> m_emailRecipients;
    private SisStudent m_currentStudent;

    private IepData m_iep = null;
    private IepGoalProgress m_progress = null;

    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
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
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {

        DataDictionary ddx = null;

        ReportDataGrid grid = new ReportDataGrid();
        HashMap<String, Integer> progressCounts = new HashMap<String, Integer>(m_iep != null ? 16 : 128);

        addParameter(PARAM_LOGO, getBase64ImageString(LOGO_CODE_ON_BOARD, getBroker()));

        Object startDate = getParameter(START_DATE);
        Object endDate = getParameter(END_DATE);

        Criteria criteria = new Criteria();
        if (startDate != null) {
            criteria.addGreaterOrEqualThan(IepGoalProgress.COL_DATE, startDate);
        }

        if (endDate != null) {
            criteria.addLessOrEqualThan(IepGoalProgress.COL_DATE, endDate);
        }

        String staffOid = (String) getParameter(STAFF_OID);
        if (!StringUtils.isEmpty(staffOid)) {
            criteria.addEqualTo(IepGoalProgress.COL_STAFF_OID, staffOid);
        }

        Object includeUnposted = getParameter(INCLUDE_UNPOSTED);
        if (includeUnposted != null && !((Boolean) includeUnposted).booleanValue()) {
            criteria.addEqualTo(IepGoalProgress.COL_POSTED_INDICATOR, Boolean.TRUE);
        }

        String postingPeriod = (String) getParameter(REPORT_PERIOD);
        if (!StringUtils.isEmpty(postingPeriod)) {
            criteria.addEqualTo(IepGoalProgress.COL_REPORT_PERIOD, postingPeriod);
        }

        String contextOid = (String) getParameter(CONTEXT_OID);
        if (contextOid != null) {
            criteria.addEqualTo(IepGoalProgress.COL_DISTRICT_CONTEXT_OID, contextOid);
        }

        if (m_progress != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_progress.getOid());
        } else if (m_iep != null) {
            criteria.addEqualTo(IepGoalProgress.COL_IEP_DATA_OID, m_iep.getOid());
        } else {
            criteria.addIn(IepGoalProgress.COL_IEP_DATA_OID,
                    new SubQuery(IepData.class, X2BaseBean.COL_OID, getCurrentCriteria()));
        }

        QueryByCriteria query = new QueryByCriteria(IepGoalProgress.class, criteria);
        query.addOrderByAscending(IepGoalProgress.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);

        /*
         * if (m_progress == null && m_iep == null) {
         * applyCurrentSort(query);
         * }
         *
         * for (Object orderItem : query.getOrderBy()) {
         * if (!(orderItem instanceof FieldHelper)) {
         * continue;
         * }
         *
         * FieldHelper orderItemFieldHelper = (FieldHelper) orderItem;
         * orderItemFieldHelper.name = IepGoalProgress.REL_IEP_DATA + "." +
         * orderItemFieldHelper.name;
         * }
         */

        DateConverter converter =
                (DateConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());
        query.addOrderByAscending(IepGoalProgress.REL_IEP_GOAL + "." + IepGoal.COL_ID);
        query.addOrderByAscending(IepGoalProgress.COL_DATE);

        List<String> goalTypes = new ArrayList<String>();
        goalTypes.add("Additional");
        goalTypes.add("Section");
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        String studentComment = "";
        String studentOid = null;
        try {
            List<IepGoalProgress> progresses = new ArrayList<IepGoalProgress>();
            while (iterator.hasNext()) {
                IepGoalProgress progress = (IepGoalProgress) iterator.next();
                if (ddx == null) {
                    ddx = DataDictionary.getDistrictDictionary(progress.getIepData().getExtendedDataDictionary(),
                            progress.getPersistenceKey());
                }
                // Exclude progress not for courses, not ALT or marked as include in ALT.
                IepGoal goal = progress.getIepGoal();
                String include = (String) goal.getFieldValueByAlias(ALIAS_INCLUDE_PROGRESS, ddx);
                String alt = (String) goal.getFieldValueByAlias(ALIAS_DELIVERY, ddx);
                String focus = goal.getFocus();
                if (("ALT".equals(alt) || BooleanAsStringConverter.TRUE.equals(include)) &&
                        goalTypes.contains(focus)) {

                    if (studentOid == null || !studentOid.equals(progress.getStudentOid())) {
                        if (progresses.size() > 0) {
                            // Apply the list of progresses for the current student.
                            for (IepGoalProgress igp : progresses) {
                                IepGoal igl = igp.getIepGoal();
                                grid.append();
                                grid.set(COL_STUDENT_OID, igp.getStudentOid());
                                grid.set(COL_STUDENT, igp.getStudent());
                                PlainDate rptDate = igp.getStudent().getPerson().getDob();
                                String dateStr = "";
                                if (rptDate != null) {
                                    dateStr = converter.javaToString(rptDate);
                                }
                                grid.set(COL_STUDENT_DOB, dateStr);
                                grid.set(COL_PROGRESS_BEAN, igp);
                                grid.set(COL_PROGRESS_OID, igp.getOid());
                                grid.set(COL_PROGRESS_CODE, getProgressCode(igp, ddx));
                                rptDate = igp.getDate();
                                dateStr = "";
                                if (rptDate != null) {
                                    dateStr = converter.javaToString(rptDate);
                                }
                                grid.set(COL_PROGRESS_DATE, dateStr);
                                grid.set(COL_COURSE_DESCRIPTION, igl.getFieldValueByAlias(ALIAS_DESCRIPTION, ddx));
                                grid.set(COL_COURSE_TEACHER, igl.getFieldValueByAlias(ALIAS_STAFF_NAME, ddx));
                                grid.set(COL_COMMENT, studentComment);
                            }
                        }
                        progresses.clear();
                        studentComment = "";
                        studentOid = progress.getStudentOid();
                    }
                    progresses.add(progress);
                    String progressComment = (String) progress.getFieldValueByAlias(ALIAS_COMMENT, ddx);
                    if (progressComment != null) {
                        if (studentComment.length() > 0) {
                            studentComment += "\n";
                        }
                        studentComment += progressComment;
                    }

                    // lookup and set the Staff OID on the progress record
                    // from the staff name on the goal.
                    String staffName = (String) goal.getFieldValueByAlias(ALIAS_STAFF_NAME, ddx);
                    String progressStaffOid = progress.getStaffOid();
                    if (StringUtils.isEmpty(staffOid) && !StringUtils.isEmpty(staffName)) {
                        progressStaffOid = getStaffOid(staffName);
                        if (!StringUtils.isEmpty(progressStaffOid)) {
                            progress.setStaffOid(progressStaffOid);
                            getBroker().saveBeanForced(progress);
                        }
                    }
                } else {
                    // Remove the unnecessary progress records.
                    // We only want the ALT and ALT indicated records.
                    getBroker().deleteBean(progress);
                }
            }
            if (progresses.size() > 0) {
                // Apply the list of progresses for the last student.
                for (IepGoalProgress igp : progresses) {
                    IepGoal igl = igp.getIepGoal();
                    grid.append();
                    grid.set(COL_STUDENT_OID, igp.getStudentOid());
                    grid.set(COL_STUDENT, igp.getStudent());
                    PlainDate rptDate = igp.getStudent().getPerson().getDob();
                    String dateStr = "";
                    if (rptDate != null) {
                        dateStr = converter.javaToString(rptDate);
                    }
                    grid.set(COL_STUDENT_DOB, dateStr);
                    grid.set(COL_PROGRESS_BEAN, igp);
                    grid.set(COL_PROGRESS_OID, igp.getOid());
                    grid.set(COL_PROGRESS_CODE, getProgressCode(igp, ddx));
                    rptDate = igp.getDate();
                    dateStr = "";
                    if (rptDate != null) {
                        dateStr = converter.javaToString(rptDate);
                    }
                    grid.set(COL_PROGRESS_DATE, dateStr);
                    grid.set(COL_COURSE_DESCRIPTION, igl.getFieldValueByAlias(ALIAS_DESCRIPTION, ddx));
                    grid.set(COL_COURSE_TEACHER, igl.getFieldValueByAlias(ALIAS_STAFF_NAME, ddx));
                    grid.set(COL_COMMENT, studentComment);
                }
            }
        } finally

        {
            iterator.close();
        }

        grid.beforeTop();

        addParameter(REPORT_TOTALS, progressCounts);

        return grid;
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
        if (!userData.getCurrentList().getDataClass().equals(IepData.class)) {
            m_iep = userData.getCurrentRecord(IepData.class);
        }

        m_progress = userData.getCurrentRecord(IepGoalProgress.class);
        m_noPublishEmailParam = (Boolean) getParameter(NO_PUBLISH_EMAIL_PARAM);
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);

        try {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), userData.getLocale());
        } catch (Exception e) {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
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
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        return IepGoalProgress.REL_STUDENT;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDescription(com.x2dev.sis.model.beans.X2BaseBean)
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        return "IEP Progress Report for " + ((SisStudent) bean).getNameView();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailAddress(SisPerson)
     */
    @Override
    public String getEmailAddress(Person person) {
        if (m_noPublishEmailParam != null && m_noPublishEmailParam.booleanValue()) {
            return "";
        }

        return person.getEmail01();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailRecipients(X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        Collection<Person> recipients = null;

        if (m_emailRecipients == null) {
            loadEmailRecipients();
        }

        recipients = new ArrayList<Person>(3);

        int publishTo = 0;

        if (getParameter(PUBLISH_TO_PARAM) != null) {
            publishTo = ((Integer) getParameter(PUBLISH_TO_PARAM)).intValue();
        }

        if (publishTo == 0 || publishTo == 1) {
            recipients.add(((SisStudent) bean).getPerson());
        }
        if (publishTo == 0 || publishTo == 2) {
            Collection<StudentContact> contacts = m_emailRecipients.get(bean.getOid());
            if (contacts != null) {
                for (StudentContact contact : contacts) {
                    if ((SisPerson) contact.getPerson() != null) {
                        recipients.add(contact.getPerson());
                    }
                }
            }
        }

        return recipients;
    }

    /**
     * Populates the email recipients map, which stores the contacts that will be notified of
     * students published grade reports.
     */
    protected void loadEmailRecipients() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_PORTAL_ACCESS_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentContact.REL_STUDENT + "." +
                    SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        }

        if (m_currentStudent != null) {
            criteria.addEqualTo(StudentContact.COL_STUDENT_OID, m_currentStudent.getOid());
        }

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);

        m_emailRecipients = getBroker().getGroupedCollectionByQuery(query, StudentContact.COL_STUDENT_OID, 2000);

    }

    /**
     * Find a staff that matches the name provided.
     *
     * @param staffName
     *
     * @return String a staff OID
     */
    private String getStaffOid(String staffName) {
        String staffOid = null;
        int pos = staffName.indexOf(",");
        String first = null;
        String last = null;
        if (pos > -1) {
            first = staffName.substring(pos + 1).trim();
            last = staffName.substring(0, pos).trim();
        } else {
            pos = staffName.indexOf(" ");
            if (pos > -1) {
                last = staffName.substring(pos + 1).trim();
                first = staffName.substring(0, pos).trim();
            }
        }
        if (first != null && last != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualToIgnoreCase(SisStaff.REL_PERSON + "." + SisPerson.COL_FIRST_NAME, first);
            criteria.addEqualToIgnoreCase(SisStaff.REL_PERSON + "." + SisPerson.COL_LAST_NAME, last);
            BeanQuery query = new BeanQuery(SisStaff.class, criteria);
            SisStaff staff = getBroker().getBeanByQuery(query);
            if (staff != null) {
                staffOid = staff.getOid();
            }
        }
        return staffOid;
    }

    /**
     * Gets the base 64 image string.
     *
     * @param imageCode String
     * @param broker X2Broker
     * @return String
     */
    public String getBase64ImageString(String imageCode, X2Broker broker) {
        String base64Image = "";

        X2Criteria imageCriteria = new X2Criteria();
        imageCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, RTB_OID_ON_SIS_IMAGES);
        imageCriteria.addEqualTo(ReferenceCode.COL_CODE, imageCode);
        BeanQuery imageQuery = new BeanQuery(ReferenceCode.class, imageCriteria);
        ReferenceCode rcdBean = broker.getBeanByQuery(imageQuery);
        if (rcdBean != null) {
            base64Image = (String) rcdBean.getFieldValueByAlias(ALIAS_RCD_IMAGE_BASE64);
        }
        return base64Image;
    }

    /**
     * Get the progress code description.
     *
     * @param igp
     * @param dictionary
     *
     * @return String
     */
    private String getProgressCode(IepGoalProgress igp, DataDictionary dictionary) {
        DataDictionaryField field =
                dictionary.findDataDictionaryField(IepGoalProgress.class.getName(), IepGoalProgress.COL_PROGRESS_CODE);
        String progressCode = igp.getProgressCode();
        if (!StringUtils.isEmpty(progressCode) && field != null && field.hasReferenceTable()) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addEqualTo(ReferenceCode.COL_CODE, progressCode);
            ReferenceCode rcd = getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria));
            if (rcd != null && !StringUtils.isEmpty(rcd.getDescription())) {
                progressCode = rcd.getDescription();
            }
        }
        return progressCode;
    }
}
