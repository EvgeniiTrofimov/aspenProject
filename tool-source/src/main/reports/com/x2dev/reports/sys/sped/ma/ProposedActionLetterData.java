/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.ma;

import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_DATA_SOURCE;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_FORMAT;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_PARAMETER_MAP;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper.ContactParameters;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.types.PlainDate;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Proposed Action Notice. This class supports multiple pages. Page 1 is the
 * N1 letter; page 2 is the narrative response section.
 *
 * @author X2 Development Corporation
 */
public class ProposedActionLetterData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public static final String FIELD_CONTACT = "contact";

    // Report parameters
    public static final String PARAM_CONTACT_NAME = "contact";
    public static final String PARAM_CONTACT_ADDRESS1 = "address1";
    public static final String PARAM_CONTACT_ADDRESS2 = "address2";
    public static final String PARAM_PRINT_TO_STUDENT = "printStudent";

    private static final String EMPTY_REPORT_CONTACT = "Empty Report";
    // Subreport constants
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-MA-N1-1";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-MA-N1-2";

    private static final String ALIAS_SPED_DECISION_18 = "sped-decision";
    private static final String ALIAS_NOTICE_DATE = "n1-notice-date";
    private static final String PARAM_GRADE = "grade";
    private static final String PARAM_YOG = "yog";

    private static final String DELEGATED = "Delegated";
    private static final String OWNBEHALF = "Own Behalf";
    private static final String SHARED = "Shared";

    private MaSpedAttribHelper m_attribHelper;
    private Map m_subReports = null;


    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.x2dev.sis.tools.reports.SpedFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {PAGE_1_FORMAT_ID, PAGE_2_FORMAT_ID};
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
        int studentAge;
        boolean printToStudent = false;
        String spedDecision18 = null;
        String m_spedDecisionPath = null;
        DataDictionaryField dataDictionary = getDictionary().findDataDictionaryFieldByAlias(ALIAS_SPED_DECISION_18);
        IepData iep = (IepData) getFormOwner();

        studentAge = iep.getStudent().getPerson().getAge();

        if (studentAge > 18) {
            if (dataDictionary != null) {
                m_spedDecisionPath = dataDictionary.getJavaName();
            } else {
                printToStudent = false;
            }
            spedDecision18 = (String) iep.getStudent().getFieldValueByBeanPath(m_spedDecisionPath);
            if (!StringUtils.isEmpty(spedDecision18)) {
                if (spedDecision18.equals(DELEGATED) || spedDecision18.equals(OWNBEHALF)
                        || spedDecision18.equals(SHARED)) {
                    printToStudent = true;
                }
            } else {
                printToStudent = false;
            }
        } else {
            printToStudent = false;
        }

        X2BaseBean noticeForm = getFormStorage();
        String noticeDateStr = (String) noticeForm.getFieldValueByAlias(ALIAS_NOTICE_DATE, getDictionary());
        PlainDate noticeDate = iep.getStartDate();
        if (noticeDateStr != null) {
        	noticeDate = PlainDate.fromString(noticeDateStr);
        }
        String gradeLevelAtStart = iep.getStudent().getGradeLevel();
        int yogAtStart = getYog(iep.getStudent(), noticeDate);
        int schoolYearAtStart = getSchoolYear(noticeDate);

        // get grade level on creation time based on meeting date, if not form creation date,
        // on most recent entry enrollment record
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
        List<String> grades =
                StudentManager.getMatchingGradeLevels(maxGradeLevel, yogAtStart, schoolYearAtStart, gradeLevels);
        if (grades != null && !grades.isEmpty()) {
            gradeLevelAtStart = grades.get(0);
        }

        addParameter(PARAM_YOG, Integer.toString(yogAtStart));
        addParameter(PARAM_GRADE, gradeLevelAtStart);

        ReportDataGrid grid = new ReportDataGrid();
        loadSubReports();
        List<ContactParameters> contacts = MaSpedAttribHelper.getStudentContactParameters(iep, getBroker());
        if (contacts != null && !contacts.isEmpty()) {
            for (ContactParameters contact : contacts) {
                Map parameters = new HashMap(getParameters());
                parameters.put(PARAM_CONTACT_NAME, contact.getName());
                parameters.put(PARAM_CONTACT_ADDRESS1, contact.getAddress1());
                parameters.put(PARAM_CONTACT_ADDRESS2, contact.getAddress2());
                parameters.put(PARAM_PRINT_TO_STUDENT, printToStudent);
                parameters.put(PARAM_YOG, Integer.toString(yogAtStart));
                parameters.put(PARAM_GRADE, gradeLevelAtStart);

                grid.append();
                grid.set(FIELD_DATA_SOURCE, m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(),
                        getDictionary(), getLocale()));
                grid.set(FIELD_FORMAT, ((Report) m_subReports.get(PAGE_1_FORMAT_ID)).getCompiledFormat());
                grid.set(FIELD_PARAMETER_MAP, parameters);
                grid.set(FIELD_CONTACT, contact.getName());

                grid.append();
                grid.set(FIELD_DATA_SOURCE, m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(),
                        getDictionary(), getLocale()));
                grid.set(FIELD_FORMAT, ((Report) m_subReports.get(PAGE_2_FORMAT_ID)).getCompiledFormat());
                grid.set(FIELD_PARAMETER_MAP, parameters);
                grid.set(FIELD_CONTACT, contact.getName());
            }
        } else {
            // empty report
            grid.append();
            grid.set(FIELD_DATA_SOURCE,
                    m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
            grid.set(FIELD_FORMAT, ((Report) m_subReports.get(PAGE_1_FORMAT_ID)).getCompiledFormat());
            grid.set(FIELD_PARAMETER_MAP, getParameters());
            grid.set(FIELD_CONTACT, EMPTY_REPORT_CONTACT);

            grid.append();
            grid.set(FIELD_DATA_SOURCE,
                    m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale()));
            grid.set(FIELD_FORMAT, ((Report) m_subReports.get(PAGE_2_FORMAT_ID)).getCompiledFormat());
            grid.set(FIELD_PARAMETER_MAP, getParameters());
            grid.set(FIELD_CONTACT, EMPTY_REPORT_CONTACT);
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Loads each N1 subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {
                PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Gets the yog for the student on a particular date
     *
     * @param student the student
     * @param startDate the start date
     * @return the yog
     */
    private int getYog(SisStudent student, PlainDate startDate) {
        int yog = student.getYog();
        if (startDate != null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            criteria.addGreaterOrEqualThan(StudentEnrollment.COL_YOG, Integer.valueOf(0));
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    StudentEnrollment enr = (StudentEnrollment) iterator.next();
                    if (enr.getYog() > 0) {
                        yog = enr.getYog();
                    }
                }
            }
        }
        return yog;
    }

    /**
     * Gets the school year for a particular date. If no school year matches, return the most recent
     * school year before the date.
     *
     * @param startDate the start date
     * @return the school year
     */
    private int getSchoolYear(PlainDate startDate) {
        DistrictSchoolYearContext ctx = null;
        if (startDate != null) {
            // get matching CTX
            X2Criteria criteria = new X2Criteria();
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
            criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
            BeanQuery query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                if (iterator.hasNext()) {
                    ctx = (DistrictSchoolYearContext) iterator.next();
                }
            }

            // get latest CTX before date
            if (ctx == null) {
                criteria = new X2Criteria();
                criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                query = new BeanQuery(DistrictSchoolYearContext.class, criteria);
                query.addOrderByDescending(DistrictSchoolYearContext.COL_END_DATE);
                try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                    if (iterator.hasNext()) {
                        ctx = (DistrictSchoolYearContext) iterator.next();
                    }
                }
            }
        } else {
            ctx = getCurrentContext();
        }
        return ctx.getSchoolYear();
    }
}
