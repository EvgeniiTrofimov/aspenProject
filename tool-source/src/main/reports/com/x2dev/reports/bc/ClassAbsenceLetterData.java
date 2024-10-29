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

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for Fujitsu's "Class Absence Letter" report.
 *
 * @author X2 Development Corporation
 */
public class ClassAbsenceLetterData extends ReportJavaSourceNet {
    // Input parameters
    private static final String PARAM_ABSENCE_REASON = "absenceReason";
    private static final String PARAM_ABSENCE_TYPE = "absenceType";
    private static final String PARAM_END_DATE = "endDate";
    private static final String PARAM_EXCUSAL_TYPE = "excusalType";
    private static final String PARAM_MIN_ABSENCES = "minAbsences";
    private static final String PARAM_PERIOD_OID = "periodOid";
    private static final String PARAM_START_DATE = "startDate";

    // Report parameters
    private static final String PERIOD_ID_PARAM = "periodId";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        List<StudentPeriodAttendance> reportBeanCollection = new ArrayList<StudentPeriodAttendance>(128);

        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(StudentPeriodAttendance.COL_DATE, getParameter(PARAM_START_DATE));
        criteria.addLessOrEqualThan(StudentPeriodAttendance.COL_DATE, getParameter(PARAM_END_DATE));
        criteria.addEqualTo(StudentPeriodAttendance.COL_SCHOOL_OID, getSchool().getOid());

        // Select Period Attendance only for active students
        X2Criteria enrollmentCriteria = new X2Criteria();
        StudentContextReportHelper helper =
                new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());
        enrollmentCriteria
                .addAndCriteria(helper.getActiveStudentCriteria(StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER));

        if (getSchool() != null) {
            Criteria secondaryCriteria = StudentManager.getSecondaryStudentCriteria(
                    StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS +
                            PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey());

            enrollmentCriteria.addOrCriteria(secondaryCriteria);
        }

        criteria.addAndCriteria(enrollmentCriteria);


        String reason = (String) getParameter(PARAM_ABSENCE_REASON);
        if (!StringUtils.isEmpty(reason)) {
            criteria.addEqualTo(StudentPeriodAttendance.COL_REASON_CODE, reason);
        }

        int excusalType = ((Integer) getParameter(PARAM_EXCUSAL_TYPE)).intValue();
        if (excusalType == 1) {
            criteria.addEqualTo(StudentPeriodAttendance.COL_EXCUSED_INDICATOR, Boolean.TRUE);
        } else if (excusalType == 2) {
            criteria.addNotEqualTo(StudentPeriodAttendance.COL_EXCUSED_INDICATOR, Boolean.TRUE);
        }

        X2Criteria absenceTypeCriteria = new X2Criteria();

        int absenceType = ((Integer) getParameter(PARAM_ABSENCE_TYPE)).intValue();
        if (absenceType == 0 || absenceType == 1) {
            absenceTypeCriteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
        }

        if (absenceType == 0) {
            absenceTypeCriteria.addOrEqualTo(StudentPeriodAttendance.COL_TARDY_INDICATOR, Boolean.TRUE);
        } else if (absenceType == 2) {
            absenceTypeCriteria.addEqualTo(StudentPeriodAttendance.COL_TARDY_INDICATOR, Boolean.TRUE);
        }

        criteria.addAndCriteria(absenceTypeCriteria);

        String periodOid = (String) getParameter(PARAM_PERIOD_OID);
        if (!StringUtils.isEmpty(periodOid)) {
            SchedulePeriod period = (SchedulePeriod) getBroker().getBeanByOid(SchedulePeriod.class, periodOid);

            criteria.addContains(StudentPeriodAttendance.COL_PERIOD_VIEW, period.getId());

            addParameter(PERIOD_ID_PARAM, period.getId());
        }

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);
        query.addOrderByAscending(StudentPeriodAttendance.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
        query.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(StudentPeriodAttendance.COL_DATE);
        query.addOrderByAscending(StudentPeriodAttendance.COL_PERIOD_VIEW);

        int minAbsences = ((Integer) getParameter(PARAM_MIN_ABSENCES)).intValue();
        SisStudent lastStudent = null;
        List<StudentPeriodAttendance> periodAttendanceList = new ArrayList<StudentPeriodAttendance>();

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentPeriodAttendance periodAttendance = (StudentPeriodAttendance) iterator.next();
                SisStudent student = periodAttendance.getStudent();

                if (lastStudent != null && !student.equals(lastStudent)) {
                    if (periodAttendanceList.size() >= minAbsences) {
                        reportBeanCollection.addAll(periodAttendanceList);
                    }

                    periodAttendanceList = new ArrayList<StudentPeriodAttendance>();
                }

                periodAttendanceList.add(periodAttendance);

                lastStudent = student;
            }

            if (periodAttendanceList.size() >= minAbsences) {
                reportBeanCollection.addAll(periodAttendanceList);
            }
        } finally {
            iterator.close();
        }

        return new BeanCollectionDataSource(null,
                reportBeanCollection,
                false,
                false,
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()),
                getLocale());
    }
}
