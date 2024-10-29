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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepAmendmentDetail;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Massachusetts IEPA form. This class prepares a ReportDataGrid that contains
 * a row for each section of the IEPA.
 *
 * @author X2 Development Corporation
 */
public class IepaFormData extends MaBeanReport {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final long serialVersionUID = 1L;

    private static final String STD_GRADE_LEVEL = "gradeLevel";
    /*
     * -------- Constants for the main report -------
     */
    private static final String COL_IEP = "iep";
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";
    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";

    // Format IDs
    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-MA-IEPA1";
    private static final String PAGE_2_FORMAT_ID = "SYS-SPED-MA-IEPA2";

    private static final String IEP_PARAM = "iep";

    private MaSpedAttribHelper m_attribHelper;

    private Map m_subReports = null;

    /**
     * Returns true if the form being printed is blank.
     *
     * @return boolean
     */
    @Override
    public boolean isBlank() {
        return getFormStorage().getOid() == null || !reportIsNotBlank();
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);

        IepData iep = getIep();

        setGradeLevel(iep);

        loadSubReports();

        preparePage1(grid, iep);

        preparePage2(grid, iep);

        grid.beforeTop();

        addParameter(IEP_PARAM, iep);

        return grid;
    }

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {
                PAGE_1_FORMAT_ID,
                PAGE_2_FORMAT_ID
        }));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned. (IEP created automatic in BaseFormReportJavaSource in initialize method and set
     * like storage)
     *
     * @return iep
     */
    private IepData getIep() {
        return (IepData) getFormOwner();
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
     * Prepares the eighth IEPA page 1 (IEPA 1).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage1(ReportDataGrid grid, IepData iep) {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);

        String amendmentOid = iep == null ? null : iep.getIepAmendmentOid();

        Collection<IepAmendmentDetail> amendmentDetail = null;

        if (isBlank() || iep == null) {
            IepAmendmentDetail blankAmendmentDetail = new IepAmendmentDetail(getBroker().getPersistenceKey());
            blankAmendmentDetail.setIepAmendmentOid(amendmentOid);

            amendmentDetail = new ArrayList<IepAmendmentDetail>(3);
            for (int i = 0; i < 4; i++) {
                amendmentDetail.add(blankAmendmentDetail);
            }
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepAmendmentDetail.COL_IEP_AMENDMENT_OID, amendmentOid);

            QueryByCriteria query = new QueryByCriteria(IepAmendmentDetail.class, criteria);
            query.addOrderByAscending(IepAmendmentDetail.COL_SEQUENCE_NUMBER);

            amendmentDetail = getBroker().getCollectionByQuery(query);
        }


        JRDataSource dataSource =
                m_attribHelper.getMaSpedCollectionDataSource(getFormOwner(), amendmentDetail, getDictionary(),
                        getLocale());

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEPA 1");
    }

    /**
     * Prepares the eighth IEPA page 2 (IEPA 2).
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void preparePage2(ReportDataGrid grid, IepData iep) {
        JRDataSource dataSource =
                m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        grid.append();
        grid.set(COL_IEP, iep);
        grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
        grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_2_FORMAT_ID));
        grid.set(COL_PAGE_IDENTIFIER, "IEPA 2");
    }

    /**
     * Sets the grade level.
     *
     * @param iep void
     */
    private void setGradeLevel(IepData iep) {
        // get age on as of iep start date.
        String gradeLevel = null;
        if (iep != null) {
            PlainDate startDate = new PlainDate();
            if (iep.getStartDate() != null) {
                startDate = iep.getStartDate();
            }

            // get grade level on creation time based on iep start date, if not form creation date,
            // on most recent entry enrollment record
            TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, iep.getStudentOid());
            BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
            query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
            query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
            Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

            for (StudentEnrollment e : enrollments) {
                if (startDate != null && e.getEnrollmentDate().before(startDate)) {

                    // student's YOG at this particular time
                    int yog = e.getYog();

                    // get the school year from basedDate
                    X2Criteria schoolYearCriteria = new X2Criteria();
                    schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                    schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);
                    QueryByCriteria schoolYearQuery =
                            new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                    DistrictSchoolYearContext ctx =
                            (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                    String currentContextOid = getCurrentContext().getContextId();
                    if (!StringUtils.isEmpty(currentContextOid)
                            && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                        gradeLevel = iep.getStudent().getGradeLevel();
                    } else {
                        int schoolYear = ctx.getSchoolYear();
                        List<String> grades =
                                StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                        gradeLevel = grades.get(0);
                    }
                    break;
                }
            }
            if (StringUtils.isEmpty(gradeLevel)) {
                gradeLevel = iep.getStudent().getGradeLevel();
            }
        }
        addParameter(STD_GRADE_LEVEL, gradeLevel);
    }
}
