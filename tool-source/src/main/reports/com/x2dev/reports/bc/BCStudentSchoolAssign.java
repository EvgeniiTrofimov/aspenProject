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
package com.x2dev.reports.bc;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class BCStudentSchoolAssign.
 */
public class BCStudentSchoolAssign extends ProcedureJavaSource {
    private final String holdingSchoolKey = SisPreferenceConstants.SYS_STD_HOLDING_SCHOOL;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String dbServerName = (String) getParameter(StudentLocatorDAO.DB_SERVER_KEY);

        Criteria criteria = new Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        criteria.addEqualTo(Student.COL_SCHOOL_OID, getPref(holdingSchoolKey));
        QueryByCriteria query = new QueryByCriteria(Student.class, criteria);
        StudentLocatorDAO dao = new StudentLocatorDAO(getBroker(), dbServerName);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        int processedCount = 0;
        int studentsInHoldingSchoolCount = 0;
        try {
            while (iterator.hasNext()) {
                studentsInHoldingSchoolCount++;
                Student student = (Student) iterator.next();
                String esisSchoolID = dao.getStudentSchoolID(student.getStateId());
                if (esisSchoolID != null) {
                    criteria = new Criteria();
                    criteria.addEqualTo(School.COL_SCHOOL_ID, esisSchoolID);
                    query = new QueryByCriteria(School.class, criteria);
                    School school = (School) getBroker().getBeanByQuery(query);
                    if (school != null) {
                        processedCount++;
                        student.setSchoolOid(school.getOid());
                        getBroker().saveBean(student);
                    }
                }
            }
            logMessage("Number of students in holding school: " + studentsInHoldingSchoolCount);
            logMessage("Number of students processed out of holding school: " + processedCount);
        } finally {
            iterator.close();
        }

    }

    /**
     * Gets the pref.
     *
     * @param key String
     * @return String
     */
    private String getPref(String key) {
        return PreferenceManager.getPreferenceValue(getOrganization(), key);
    }
}
