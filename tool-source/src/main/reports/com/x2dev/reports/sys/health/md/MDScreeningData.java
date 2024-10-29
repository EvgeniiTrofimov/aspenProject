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

package com.x2dev.reports.sys.health.md;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Report Cards" report for Allegany.
 *
 * @author X2 Development Corporation
 */
public class MDScreeningData extends ReportJavaSourceNet {
    private static final String NA = "Not Available";
    private static final String DATE = "date";
    private static final String DIGITS_ONLY_RE = "^[0-9]+$";
    private static final String DOB = "DOB";
    private static final String HEADER = "header";
    private static final String NEWLINE = "\n";
    private static final String PROGRAM = "Program";
    private static final String QUERY_BY = "queryBy";
    private static final String REFERRAL_TYPE_PARAM = "type";
    private static final String SCHOOL = "school";
    private static final long serialVersionUID = 1L;
    private static final String STUDENT = "student";
    private static final String STUDENT_FULL_NAME = "studentFullName";
    private static final String TELEPHONE = "Telephone ";
    private static final String URL = " www.acpsmd.org";

    private SisStudent m_currentStudent;
    private String m_referralType;
    private Criteria m_studentCriteria;

    /**
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = null;

        buildCriteria();
        grid = new ReportDataGrid();
        PlainDate returnBy = (PlainDate) getParameter(DATE);
        QueryByCriteria query = new QueryByCriteria(HealthScreening.class, m_studentCriteria);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                HealthScreening screening = (HealthScreening) students.next();
                SisStudent student = screening.getStudent();
                SisSchool school = student.getSchool();
                grid.append();
                grid.set(DATE, returnBy);
                grid.set(DOB, student.getPerson().getDob());
                grid.set(STUDENT, student);
                grid.set(SCHOOL, school);
                grid.set(STUDENT_FULL_NAME, student.getPerson().getFirstName() + " "
                        + student.getPerson().getLastName());
                grid.set(PROGRAM, m_referralType);

                SisAddress address = school.getAddress();

                String line2 = address.getAddressLine02();
                if (line2 == null) {
                    line2 = address.getAddressLine03();
                }

                String phone = address.getPhone01();
                if (phone != null && phone.matches(DIGITS_ONLY_RE)) {
                    phone = "(" + phone.substring(0, 3) + ") " + phone.substring(3, 6) + " - " + phone.substring(6);
                }
                if (phone == null) {
                    phone = NA;
                }
                String subtext = school.getName() + NEWLINE + address.getAddressLine01() + " " + line2 + NEWLINE
                        + TELEPHONE + phone + URL;
                grid.set(HEADER, subtext);
            }
        } finally {
            students.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_referralType = getParameter(REFERRAL_TYPE_PARAM).toString();
    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#saveState(com.x2dev.sis.web.UserDataContainer)
     */
    @Override
    protected void saveState(final UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }

    /**
     * Determines what to pull based on user input.
     */
    private void buildCriteria() {
        m_studentCriteria = new Criteria();

        if (m_currentStudent != null) {
            /*
             * Running for one student
             */
            m_studentCriteria.addEqualTo(X2BaseBean.COL_OID, m_currentStudent.getOid());
        } else {
            String queryBy = getParameter(QUERY_BY).toString();
            addUserCriteria(m_studentCriteria, queryBy, "", HealthScreening.class, X2BaseBean.COL_OID);
        }
    }
}
