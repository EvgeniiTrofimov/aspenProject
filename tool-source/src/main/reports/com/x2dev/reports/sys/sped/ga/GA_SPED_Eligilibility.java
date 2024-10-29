/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class GA_SPED_Eligilibility.
 */
public class GA_SPED_Eligilibility extends BaseFormReportJavaSource {

    // The subreports RPT_ID's
    private static final String PARAM_PAGE_RESULTS_FORMAT_ID = "SYS-SPED-GA-ELIG-RES";
    private static final String PARAM_PAGE_STUDENT_DATA_FORMAT_ID = "SYS-SPED-GA-ELIG-STD";
    private static final String PARAM_TEAM_MEMBER_FORMAT_ID = "SYS-SPED-GA-ELIG-TMD";
    private static final String PARAM_GENERIC_CHILD_FORMAT_ID = "SYS-SPED-GA-ELIG-GEN";

    // The subreports parameter name
    private static final String PARAM_PAGE_RESULTS_FORMAT = "resultsPage";
    private static final String PARAM_PAGE_STUDENT_DATA_FORMAT = "studentPage";
    private static final String PARAM_TEAM_MEMBER_FORMAT = "teamMemberSection";
    private static final String PARAM_FACTORS_FORMAT = "factorsPage";
    private static final String PARAM_QUESTIONS_FORMAT = "questionsPage";


    // Data source to be passed into the report
    private static final String PARAM_RESULTS = "resultsData";
    private static final String PARAM_STUDENT_DATA = "studentData";
    private static final String PARAM_TEAM_MEMBER_DATA = "teamMemberData";
    private static final String PARAM_FACTORS_DATA = "factorsData";
    private static final String PARAM_QUESTIONS_DATA = "questionsData";

    // Fields
    private static final String ALIAS_ELIGIBILITY_FACTORS = "elig-factors";
    private static final String ALIAS_ELIGIBILITY_QUESTIONS = "elig-question";

    // Map holding reference to subreports for class
    private Map<String, Report> m_subReports;
    private Map<String, String> m_fieldRefTables;


    private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    private SimpleDateFormat stringFormat = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        loadSubReports();
        initializeMaps();

        prepareResultsPage();
        prepareStudentDataPage();
        prepareTeamMemberData();
        prepareGenericDataPage(ALIAS_ELIGIBILITY_FACTORS,
                PARAM_GENERIC_CHILD_FORMAT_ID,
                PARAM_FACTORS_FORMAT,
                PARAM_FACTORS_DATA);
        prepareGenericDataPage(ALIAS_ELIGIBILITY_QUESTIONS,
                PARAM_GENERIC_CHILD_FORMAT_ID,
                PARAM_QUESTIONS_FORMAT,
                PARAM_QUESTIONS_DATA);

        addParameter("iepType", getIepType());
        addParameter("dateFormat", stringFormat);

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * Get the IEP type of the form owner.
     *
     * @return IEP Type
     */
    private String getIepType() {
        String iepType = null;

        IepData iepData = (IepData) getFormOwner();
        Collection<IepMeeting> iepMeetings = iepData.getIepMeeting();
        for (IepMeeting iepMeeting : iepMeetings) {
            if (IepMeeting.TypeCode.INITIAL.equals(iepMeeting.getTypeCodeEnum())) {
                iepType = "Initial";
                break;
            } else if (IepMeeting.TypeCode.REEVAL.equals(iepMeeting.getTypeCodeEnum())) {
                iepType = "Reevaluation";
                break;
            }
        }

        return iepType;
    }

    /**
     * Load the reference codes into a map.
     *
     * @param fieldId String
     * @return Map
     */
    private Map<String, ReferenceCode> getRefCodeMap(String fieldId) {
        Map<String, ReferenceCode> refCodeMap = new TreeMap<String, ReferenceCode>();

        String refTableOid = m_fieldRefTables.get(fieldId);

        if (refTableOid != null) {
            Criteria refCodeCriteria = new Criteria();
            refCodeCriteria.addEqualTo(
                    ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, refTableOid);

            QueryByCriteria refCodesQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);

            refCodeMap = getBroker().getMapByQuery(refCodesQuery, ReferenceCode.COL_CODE, 64);
        }

        return refCodeMap;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private InputStream getSubreportFormat(String pageId) {
        Report report = m_subReports.get(pageId);
        return new ByteArrayInputStream(report.getCompiledFormat());
    }

    /**
     * Load maps for reference tables used in the report.
     */
    private void initializeMaps() {
        m_fieldRefTables = new HashMap<String, String>();

        DataDictionaryField ddField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_ELIGIBILITY_FACTORS);
        if (ddField != null) {
            m_fieldRefTables.put(ALIAS_ELIGIBILITY_FACTORS, ddField.getReferenceTableOid());
        }
        ddField = getDictionary().findDataDictionaryFieldByAlias(ALIAS_ELIGIBILITY_QUESTIONS);
        if (ddField != null) {
            m_fieldRefTables.put(ALIAS_ELIGIBILITY_QUESTIONS, ddField.getReferenceTableOid());
        }
    }

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PARAM_PAGE_RESULTS_FORMAT_ID,
                PARAM_PAGE_STUDENT_DATA_FORMAT_ID,
                PARAM_TEAM_MEMBER_FORMAT_ID,
                PARAM_GENERIC_CHILD_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 4);
    }

    /**
     * Prepares the eligibility data page subreport.
     *
     * @param discriminatorField String
     * @param reportFormatId String
     * @param reportFormatParameter String
     * @param reportDataParameter String
     */
    private void prepareGenericDataPage(String discriminatorField,
                                        String reportFormatId,
                                        String reportFormatParameter,
                                        String reportDataParameter) {
        ReportDataGrid grid = new ReportDataGrid();
        Map<String, Map<String, String>> reportData = new TreeMap<String, Map<String, String>>();

        // get refCodes and use to initialize a map of values.
        Map<String, ReferenceCode> refCodeMap = getRefCodeMap(discriminatorField);
        for (Map.Entry<String, ReferenceCode> entry : refCodeMap.entrySet()) {
            Map<String, String> dataValues = new HashMap<String, String>();
            dataValues.put("domain", entry.getValue().getDescription());
            dataValues.put("domainNeeded", "");
            dataValues.put("detail", "");
            reportData.put(entry.getKey(), dataValues);
        }

        // get collection of child data
        Collection<GenericFormChildData> childDataCollection =
                ((GenericFormData) getFormStorage()).getGenericFormDataChildren();

        // populate map with any user data. overrite any existing data so that only last element is
        // used.
        for (GenericFormChildData childData : childDataCollection) {
            if (!StringUtils.isEmpty((String) childData.getFieldValueByAlias(discriminatorField, getDictionary()))) {
                String key = (String) childData.getFieldValueByAlias(discriminatorField, getDictionary());
                Map<String, String> dataValues = reportData.get(key);
                dataValues.put("domainNeeded", (String) childData.getFieldValueByAlias("elig-yes-no", getDictionary()));
                dataValues.put("detail", (String) childData.getFieldValueByAlias("elig-explain", getDictionary()));
                reportData.put(key, dataValues);
            }
        }

        // populate the grid with the data from the ordered map
        for (Map.Entry<String, Map<String, String>> entry : reportData.entrySet()) {
            Map<String, String> dataValues = entry.getValue();
            grid.append();
            grid.set("domain", dataValues.get("domain"));
            grid.set("domainNeeded", dataValues.get("domainNeeded"));
            grid.set("detail", dataValues.get("detail"));
        }

        grid.beforeTop();

        addParameter(reportFormatParameter, getSubreportFormat(reportFormatId));
        addParameter(reportDataParameter, grid);
    }

    /**
     * Prepares the results page subreport.
     */
    private void prepareResultsPage() {
        ReportDataGrid grid = new ReportDataGrid();

        // populate grid with data
        Collection<GenericFormChildData> childDataCollection =
                ((GenericFormData) getFormStorage()).getGenericFormDataChildren();

        for (GenericFormChildData childData : childDataCollection) {
            if (!StringUtils
                    .isEmpty((String) childData.getFieldValueByAlias("elig-assessment-names", getDictionary()))) {
                grid.append();
                grid.set("assessmentNames", childData.getFieldValueByAlias("elig-assessment-names", getDictionary()));
                grid.set("assessmentResults",
                        childData.getFieldValueByAlias("elig-assessment-results", getDictionary()));
                String rawDate = (String) childData.getFieldValueByAlias("elig-date", getDictionary());
                if (rawDate != null) {
                    Date date;

                    try {
                        date = dateFormat.parse(rawDate);
                        grid.set("date", stringFormat.format(date));
                    } catch (ParseException e) {
                        // do nothing
                    }
                }
            }
        }

        // fill in rest of grid with empty rows
        if (grid.rowCount() < 5) {
            for (int i = grid.rowCount(); i < 5; i++) {
                grid.append();
            }
        }

        grid.beforeTop();

        addParameter(PARAM_PAGE_RESULTS_FORMAT, getSubreportFormat(PARAM_PAGE_RESULTS_FORMAT_ID));
        addParameter(PARAM_RESULTS, grid);
    }

    /**
     * Prepares the student data page subreport.
     */
    private void prepareStudentDataPage() {
        ReportDataGrid grid = new ReportDataGrid();

        DataDictionaryField ddField = getDictionary().findDataDictionaryFieldByAlias("elig-domain");

        // populate grid with data
        Collection<GenericFormChildData> childDataCollection =
                ((GenericFormData) getFormStorage()).getGenericFormDataChildren();
        childDataCollection = CollectionUtils.sortBeans(childDataCollection, ddField.getJavaName(), false);

        for (GenericFormChildData childData : childDataCollection) {
            if (!StringUtils.isEmpty((String) childData.getFieldValueByAlias("elig-domain", getDictionary()))) {
                grid.append();
                grid.set("domain", childData.getFieldValueByAlias("elig-domain", getDictionary()));
                grid.set("scores", childData.getFieldValueByAlias("elig-scores", getDictionary()));
                grid.set("resultsStrengths", childData.getFieldValueByAlias("elig-results-strengths", getDictionary()));
                grid.set("resultsWeaknesses",
                        childData.getFieldValueByAlias("elig-results-weaknesses", getDictionary()));
                String rawDate = (String) childData.getFieldValueByAlias("elig-date", getDictionary());
                if (rawDate != null) {
                    Date date;

                    try {
                        date = dateFormat.parse(rawDate);
                        grid.set("date", stringFormat.format(date));
                    } catch (ParseException e) {
                        // do nothing
                    }
                }
            }
        }

        // fill in rest of grid with empty rows
        if (grid.rowCount() < 5) {
            for (int i = grid.rowCount(); i < 5; i++) {
                grid.append();
            }
        }

        grid.beforeTop();

        addParameter(PARAM_PAGE_STUDENT_DATA_FORMAT, getSubreportFormat(PARAM_PAGE_STUDENT_DATA_FORMAT_ID));
        addParameter(PARAM_STUDENT_DATA, grid);
    }

    /**
     * Prepares the team member data page subreport.
     */
    private void prepareTeamMemberData() {
        IepData iepData = (IepData) getFormOwner();
        Collection<IepTeamMember> iepTeamMembers = iepData.getTeamMembers();

        ReportDataGrid grid = new ReportDataGrid();

        for (IepTeamMember iepTeamMember : iepTeamMembers) {
            grid.append();
            grid.set("teamMemberName", iepTeamMember.getNameView());
            grid.set("teamMemberRole", iepTeamMember.getMemberRoleCode()); // need to get role name
                                                                           // using reference table
        }

        // fill in rest of grid with empty rows
        if (grid.rowCount() < 7) {
            for (int i = grid.rowCount(); i < 7; i++) {
                grid.append();
            }
        }

        grid.beforeTop();

        addParameter(PARAM_TEAM_MEMBER_FORMAT, getSubreportFormat(PARAM_TEAM_MEMBER_FORMAT_ID));
        addParameter(PARAM_TEAM_MEMBER_DATA, grid);

    }

}
