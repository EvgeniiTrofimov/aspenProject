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
package com.x2dev.reports.sys.sped.ri;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.TreeMap;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the RI IEP Review of Referral form.
 *
 * @author X2 Development Corporation
 */
public class LearningDisabilitiesReportData extends BaseFormReportJavaSource {

    private static final String PARAM_FORMDATE = "formDate";
    private static final String PARAM_RETAINED = "retained";
    private static final String PARAM_RETAINED_GRADES = "retainedGrades";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        IepData iep = (IepData) getFormOwner();
        if (iep != null) {
            Student student = iep.getStudent();
            findRetained(student);
        }

        Date formDate = null;
        if (getFormInstance() != null) {
            long formCreateTime = getFormInstance().getCreatedTime();
            formDate = new Date(formCreateTime);
        } else {
            formDate = new Date();
        }
        addParameter(PARAM_FORMDATE, formDate);

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Find all enrollment YOG changes that increase the YOG value.
     * Determine grade level in year and set retained parameters.
     *
     * @param student
     */
    private void findRetained(Student student) {
        Collection<StudentEnrollment> enrollments = getOrderedEnrollment(student);
        List<String> grades = new ArrayList<String>();
        int lastYog = 0;
        for (StudentEnrollment enrollment : enrollments) {
            if (lastYog == 0) {
                lastYog = enrollment.getYog();
            } else if (enrollment.getYog() != 0 && lastYog < enrollment.getYog()) {
                int schoolYear = getSchoolYear(enrollment.getEnrollmentDate());
                if (schoolYear > 0) {
                    TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(getBroker());
                    List<String> gradeLevels = StudentManager.getMatchingGradeLevels(12, enrollment.getYog(),
                            schoolYear, sortedGradeLevels);
                    if (gradeLevels != null && gradeLevels.size() > 0) {
                        String grade = gradeLevels.iterator().next();
                        if (!grades.contains(grade)) {
                            grades.add(grade);
                        }
                    }
                }
            }
        }

        if (grades.size() > 0) {
            addParameter(PARAM_RETAINED, "Yes");
            addParameter(PARAM_RETAINED_GRADES, StringUtils.convertCollectionToDelimitedString(grades, ", "));
        } else {
            addParameter(PARAM_RETAINED, "No");
            addParameter(PARAM_RETAINED_GRADES, "");
        }
    }

    /**
     * Return the school year that is applicable to the enrollment record date.
     *
     * @param enrollmentDate
     *
     * @return int SchoolYear
     */
    private int getSchoolYear(PlainDate enrollmentDate) {
        Criteria criteria = new Criteria();
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, enrollmentDate);
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, enrollmentDate);
        QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        DistrictSchoolYearContext context = getBroker().getBeanByQuery(query);
        if (context != null) {
            return context.getSchoolYear();
        }
        return 0;
    }

    private Collection<StudentEnrollment> getOrderedEnrollment(Student student) {
        ArrayList typeCodes = new ArrayList(2);
        typeCodes.add(StudentEnrollment.ENTRY);
        typeCodes.add(StudentEnrollment.WITHDRAWAL);
        typeCodes.add(StudentEnrollment.YOG_CHANGE);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_TYPE);

        return getBroker().getCollectionByQuery(query);
    }
}
