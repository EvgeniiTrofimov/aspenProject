/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.types.PlainDate;

import java.util.ArrayList;
import java.util.List;

/**
 * Exits the selected students from Special Education.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class ExitStudentsFromSpedProcedure extends ProcedureJavaSource {

    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String queryBy = (String) getParameter(QUERY_BY_PARAM);
        String queryString =
                (String) getParameter(QUERY_STRING_PARAM) == null ? "" : (String) getParameter(QUERY_STRING_PARAM);

        X2Criteria criteria = new X2Criteria();
        addUserCriteria(criteria, queryBy, queryString, SisStudent.class, SisStudent.class, X2BaseBean.COL_OID);
        BeanQuery query = new BeanQuery(SisStudent.class, criteria);

        String spedExitCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_EXITED_CODE);

        boolean logMessageAdded = false;
        List<SisStudent> sisStudents = new ArrayList<>();
        try (QueryIterator<X2BaseBean> iterator = getBroker().getIteratorByQuery(query);) {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                sisStudents.add(student);
            }
        }
        for (SisStudent student : sisStudents) {
            logMessageAdded = setStudentSpedStatusCode(spedExitCode, logMessageAdded, student);
        }
        warnUserIfNoRecordsWereChanged(logMessageAdded);
    }

    /**
     * Warn user if no records were changed.
     *
     * @param logMessageAdded boolean
     */
    protected void warnUserIfNoRecordsWereChanged(boolean logMessageAdded) {
        if (!logMessageAdded) {
            logMessage(
                    "No records were changed. Please verify that you have selected students who are currently enrolled in Special Education and need to be exited and run this procedure again.");
        }
    }

    /**
     * Sets the student sped status code.
     *
     * @param spedExitCode String
     * @param logMessageAdded boolean
     * @param student SisStudent
     * @return true, if successful
     */
    protected boolean setStudentSpedStatusCode(String spedExitCode, boolean logMessageAdded, SisStudent student) {
        EnrollmentManager manager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        if (SpedStatusCode.ACTIVE.equals(student.getSpedStatusCodeEnum())) {
            manager.exitStudentFromSpecialEducation(student, new PlainDate(), null);
        }

        if (student.isDirty()) {
            logMessageAdded = saveStudentChange(student);
        } else {
            logMessage("Info: " + student.getNameView()
                    + " was already exited from Special Education. No changes were made.");
            logMessageAdded = true;
        }
        return logMessageAdded;
    }

    /**
     * Save student change.
     *
     * @param student SisStudent
     * @return true, if successful
     */
    protected boolean saveStudentChange(SisStudent student) {
        boolean logMessageAdded;
        List<ValidationError> errors = getBroker().saveBean(student);
        if (errors.isEmpty()) {
            logMessage(student.getNameView() + " was successfully exited from Special Education.");
            logMessageAdded = true;
        } else {
            logMessage("Error exiting student from special education: " + student.getNameView() + " Error Message: "
                    + errors);
            logMessageAdded = true;
        }
        return logMessageAdded;
    }
}
