/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2008 X2 Development Corporation.
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
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Specific Learning Disability Eligibility Observation form.
 * This class supports multiple pages.
 *
 * @author X2 Development Corporation
 */
public class SldObservationData extends MultiPageFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /*
     * -------- Input parameter constants -------
     */
    private static final String INPUT_GRADE = "grade";

    /**
     * Some other constants
     */
    private static final String STD_GRADE_LEVEL = "gradeLevel";
    
    /**
     * Additional footer note that should be printed on page
     */
    public static final String FOOTER_NOTE = "footerNote";

    /*
     * -------- Constants for the main report -------
     */
    public static final String OBSERVATION_FORM_PREK = "SYS-SPED-MA-SLDE0";
    public static final String OBSERVATION_FORM_G1_4 = "SYS-SPED-MA-SLDE1";
    public static final String OBSERVATION_FORM_G5_8 = "SYS-SPED-MA-SLDE2";
    public static final String OBSERVATION_FORM_G9_12 = "SYS-SPED-MA-SLDE3";

    /**
     * Only prints one of the 4 possible observation forms as specified by the user
     * in the input.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#getFormatIds()
     */
    @Override
    protected String[] getFormatIds() {
        String[] formatId = new String[1];
        switch (((Integer) getParameter(INPUT_GRADE)).intValue()) {
            case 0:
                formatId[0] = OBSERVATION_FORM_PREK;
                break;
            case 1:
                formatId[0] = OBSERVATION_FORM_G1_4;
                break;
            case 2:
                formatId[0] = OBSERVATION_FORM_G5_8;
                break;
            case 3:
                formatId[0] = OBSERVATION_FORM_G9_12;
                break;
        }
        return formatId;
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
        IepData iep = (IepData) getFormOwner();
        if (iep != null) {
            setGradeLevel(iep);
        }
        return super.gatherData();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.MultiPageFormReportJavaSource#preparePage(com.follett.fsc.core.k12.tools.reports.ReportDataGrid)
     */
    @Override
    protected void preparePage(ReportDataGrid grid) {
        super.preparePage(grid);

        grid.set(FOOTER_NOTE, "");
    }

    /**
     * Sets the grade level.
     *
     * @param iep void
     */
    private void setGradeLevel(IepData iep) {
        // get age on as of iep start date.
        PlainDate startDate = new PlainDate();
        if (iep.getStartDate() != null) {
            startDate = iep.getStartDate();
        }

        // get grade level on creation time based on iep start date, if not form creation date, on
        // most recent entry enrollment record
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        String gradeLevel = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, iep.getStudentOid());
        BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
        Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(query);
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
                DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                String currentContextOid = getCurrentContext().getContextId();
                int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

                if (!StringUtils.isEmpty(currentContextOid) && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
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
        addParameter(STD_GRADE_LEVEL, gradeLevel);
    }
}
