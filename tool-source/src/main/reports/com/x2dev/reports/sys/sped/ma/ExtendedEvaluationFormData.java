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

import static com.x2dev.sis.model.business.sped.MassachusettsAliases.EXTENDED_EVALUATION_MEETING_DATE;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.EXTENDED_EVALUATION_MEETING_LOCATION;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.EXTENDED_EVALUATION_MEETING_TIME;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Extended Evaluation form. This class supports multiple pages and provides
 * access to the following information on the format:
 * <ul>
 * <li>The form storage and owner objects as provided by <code>SimpleBeanDataSource</code>
 * <li>Extended evaluation meeting records as parameters in the format: <i>alias[n]</i> where
 * <i>alias</i> is one of <code>ee-meeting-date, ee-meeting-time, ee-meeting-location</code> and
 * <i>n</i> is a 0-based meeting number
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class ExtendedEvaluationFormData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    public static final String PAGE_1_FORMAT_ID = "SYS-SPED-MA-EE1";
    public static final String PAGE_2_FORMAT_ID = "SYS-SPED-MA-EE2";
    private static final String STD_GRADE_LEVEL = "gradeLevel";

    private MaSpedAttribHelper m_attribHelper;

    /**
     * Gets the format ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.SpedFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        return new String[] {PAGE_1_FORMAT_ID, PAGE_2_FORMAT_ID};
    }

    /**
     * Prepare page.
     *
     * @param grid ReportDataGrid
     * @see
     *      com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#preparePage(com.follett.
     *      fsc.core.k12.tools.reports.ReportDataGrid)
     */
    @Override
    protected void preparePage(ReportDataGrid grid) {
        JRDataSource dataSource =
                m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        prepareCurrentPage(grid, dataSource);

        if (getCurrentPageNumber() == 1) {
            GenericFormData formData = (GenericFormData) getFormStorage();

            DataDictionaryField meetingDateField =
                    getDictionary().findDataDictionaryFieldByAlias(EXTENDED_EVALUATION_MEETING_DATE);
            DataDictionaryField meetingLocationField =
                    getDictionary().findDataDictionaryFieldByAlias(EXTENDED_EVALUATION_MEETING_LOCATION);
            DataDictionaryField meetingTimeField =
                    getDictionary().findDataDictionaryFieldByAlias(EXTENDED_EVALUATION_MEETING_TIME);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(GenericFormChildData.COL_GENERIC_FORM_DATA_OID, formData.getOid());

            QueryByCriteria query = new QueryByCriteria(GenericFormChildData.class, criteria);
            query.addOrderByAscending(meetingDateField.getJavaName());

            int count = 0;
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    GenericFormChildData childData = (GenericFormChildData) iterator.next();

                    Object meetingDate =
                            childData.getFieldValueByAlias(EXTENDED_EVALUATION_MEETING_DATE, getDictionary());
                    Object meetingLocation =
                            childData.getFieldValueByAlias(EXTENDED_EVALUATION_MEETING_LOCATION, getDictionary());
                    Object meetingTime =
                            childData.getFieldValueByAlias(EXTENDED_EVALUATION_MEETING_TIME, getDictionary());

                    String meetingDateParam = EXTENDED_EVALUATION_MEETING_DATE + "[" + count + "]";
                    String meetingLocationParam = EXTENDED_EVALUATION_MEETING_LOCATION + "[" + count + "]";
                    String meetingTimeParam = EXTENDED_EVALUATION_MEETING_TIME + "[" + count + "]";

                    addParameter(meetingDateParam,
                            ReportUtils.getStringValue(meetingDate, meetingDateField, getLocale()));
                    addParameter(meetingLocationParam,
                            ReportUtils.getStringValue(meetingLocation, meetingLocationField, getLocale()));
                    addParameter(meetingTimeParam,
                            ReportUtils.getStringValue(meetingTime, meetingTimeField, getLocale()));

                    count++;
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);

        IepData iep = (IepData) getFormOwner();
        setGradeLevel(iep);
        return super.gatherData();
    }

    /**
     * Sets the grade level.
     *
     * @param iep void
     */
    private void setGradeLevel(IepData iep) {
        String gradeLevel = null;
        if (iep != null) {
            // get age on as of iep start date.
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
                    if (!StringUtils.isEmpty(currentContextOid) && ctx != null
                            && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                        gradeLevel = iep.getStudent().getGradeLevel();
                    } else if (ctx != null) {
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
