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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Document;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepGoalProgress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.metadata.FieldHelper;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

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

    private static final String COL_FIELD_ID = "fieldId";
    private static final String COL_PROGRESS_BEAN = "progress";
    private static final String COL_PROGRESS_COUNT = "progressCount";
    private static final String COL_PROGRESS_STATE_CODE = "progressCode";
    private static final String COL_STUDENT = "student";
    private static final String COL_TEXT = "text";
    private static final String COL_PERIOD_YEAR = "periodYear";

    private static final String FIELD_ID_BASELINE = "baseline";
    private static final String FIELD_ID_BENCHMARK = "benchmark";
    private static final String FIELD_ID_GOAL = "goal";
    private static final String FIELD_ID_PERFORMANCE_LEVEL = "performanceLevel";

    private static final String NO_PUBLISH_EMAIL_PARAM = "noEmail";
    private static final String PUBLISH_TO_PARAM = "publishTo";

    private Boolean m_noPublishEmailParam;
    private Map<String, List<StudentContact>> m_emailRecipients;
    private SisStudent m_currentStudent;

    private IepData m_iep = null;
    private IepGoalProgress m_progress = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {

        DataDictionary ddx = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField ddxField =
                ddx.findDataDictionaryField(IepGoalProgress.class.getName(), IepGoalProgress.COL_PROGRESS_CODE);
        Map<String, ReferenceCode> refCodeMap = null;
        ReferenceTable refTable = ddxField.getReferenceTable();
        if (refTable != null) {
            refCodeMap = refTable.getCodeMap();
        } else {
            refCodeMap = new HashMap<String, ReferenceCode>();
        }

        ReportDataGrid grid = new ReportDataGrid();
        HashMap<String, Integer> progressCounts = new HashMap<String, Integer>(m_iep != null ? 16 : 128);

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

        if (m_progress == null && m_iep == null) {
            applyCurrentSort(query);
        }

        for (Object orderItem : query.getOrderBy()) {
            if (!(orderItem instanceof FieldHelper)) {
                continue;
            }

            FieldHelper orderItemFieldHelper = (FieldHelper) orderItem;
            orderItemFieldHelper.name = IepGoalProgress.REL_IEP_DATA + "." + orderItemFieldHelper.name;
        }

        query.addOrderByAscending(IepGoalProgress.REL_IEP_GOAL + "." + IepGoal.COL_ID);
        query.addOrderByAscending(IepGoalProgress.COL_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepGoalProgress progress = (IepGoalProgress) iterator.next();

                Integer progressCount = progressCounts.get(progress.getIepGoalOid());
                if (progressCount == null) {
                    progressCount = Integer.valueOf(0);
                }

                progressCount = Integer.valueOf(progressCount.intValue() + 1);
                progressCounts.put(progress.getIepGoalOid(), progressCount);
                String progressCode = progress.getProgressCode();
                String progressStateCode = null;
                if (!StringUtils.isEmpty(progressCode)) {
                    ReferenceCode refCode = refCodeMap.get(progressCode);
                    if (refCode != null) {
                        progressStateCode = refCode.getStateCode();
                    }
                }

                String periodYear = "";
                if (progress.getIepData().getStartDate() != null) {
                    periodYear = "" + DateUtils.getYear(progress.getIepData().getStartDate());
                }

                grid.append();
                grid.set(COL_PROGRESS_BEAN, progress);
                grid.set(COL_STUDENT, progress.getStudent());
                grid.set(COL_FIELD_ID, FIELD_ID_BASELINE);
                grid.set(COL_PERIOD_YEAR, periodYear);
                grid.set(COL_TEXT, progress.getIepGoal().getBaseline());
                grid.set(COL_PROGRESS_COUNT, progressCount);

                grid.append();
                grid.set(COL_PROGRESS_BEAN, progress);
                grid.set(COL_STUDENT, progress.getStudent());
                grid.set(COL_FIELD_ID, FIELD_ID_GOAL);
                grid.set(COL_TEXT, progress.getIepGoal().getGoal());
                grid.set(COL_PROGRESS_COUNT, progressCount);

                grid.append();
                grid.set(COL_PROGRESS_BEAN, progress);
                grid.set(COL_STUDENT, progress.getStudent());
                grid.set(COL_FIELD_ID, FIELD_ID_BENCHMARK);
                grid.set(COL_TEXT, progress.getIepGoal().getObjectivesView(getBroker()));
                grid.set(COL_PROGRESS_COUNT, progressCount);

                grid.append();
                grid.set(COL_PROGRESS_BEAN, progress);
                grid.set(COL_STUDENT, progress.getStudent());
                grid.set(COL_FIELD_ID, FIELD_ID_PERFORMANCE_LEVEL);
                grid.set(COL_TEXT, progress.getPerformanceLevel());
                grid.set(COL_PROGRESS_COUNT, progressCount);
                grid.set(COL_PROGRESS_STATE_CODE, progressStateCode);
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
     * Some customers have custom fields on the Documents table for upload date and school year.
     * If the aliases are present, populate them.
     * upload-date
     * school-year
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#createDocument(boolean,
     *      com.follett.fsc.core.k12.beans.Person, com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.beans.School, java.io.File)
     */
    @Override
    public Document createDocument(boolean overwriteExisting,
                                   Person person,
                                   X2BaseBean bean,
                                   School school,
                                   File resultFile) {
        Document document = super.createDocument(overwriteExisting, person, bean, school, resultFile);
        if (document != null) {
            String contextOid = (String) getParameter(CONTEXT_OID);
            if (!StringUtils.isEmpty(contextOid)) {
                DistrictSchoolYearContext context =
                        getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);
                if (context != null) {
                    document.setFieldValueByAlias("school-year", context.getContextId());
                }
            }

            PlainDate today = new PlainDate();
            SystemStringConverter converter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
            document.setFieldValueByAlias("upload-date", converter.getSystemString(today));
        }
        return document;
    }

    /**
     * Builds the document name.
     *
     * @param reportName String
     * @param context DistrictSchoolYearContext
     * @param dictionary DataDictionary
     * @return String
     */
    protected String buildDocumentName(String reportName,
                                       DistrictSchoolYearContext context,
                                       DataDictionary dictionary) {
        String documentName = super.buildDocumentName(reportName, context, dictionary);

        String postingPeriod = (String) getParameter(REPORT_PERIOD);
        if (!StringUtils.isEmpty(postingPeriod)) {
        	documentName = documentName + "_" + postingPeriod; 

	        if (dictionary != null) {
		        DataDictionaryField documentNameField = dictionary.findDataDictionaryField(Document.class.getName(), Document.COL_NAME);
		        if (documentNameField != null) {
		            if (documentName != null && documentName.length() > documentNameField.getDatabaseLength()) {
		                documentName = documentName.substring(0, documentNameField.getDatabaseLength());
		            }
		        }
	        }
        }

        return documentName;
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
}
