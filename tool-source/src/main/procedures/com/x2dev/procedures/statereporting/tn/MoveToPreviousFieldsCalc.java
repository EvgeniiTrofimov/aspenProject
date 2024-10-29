/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.beans.CalculatedField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.CalculatedFieldProcedure;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;

/**
 * The Class MoveToPreviousFieldsCalc.
 */
public class MoveToPreviousFieldsCalc implements CalculatedFieldProcedure {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    // Defines the Alias Names (have to do in Java as no Input Definition for
    // CalculatedFieldProcedure)
    private static final String NIGHTLY_FIRST_NAME = "DOE NIGHTLY FNAME";
    private static final String NIGHTLY_LAST_NAME = "DOE NIGHTLY LNAME";
    private static final String NIGHTLY_MIDDLE_NAME = "DOE NIGHTLY MNAME";
    private static final String NIGHTLY_SSN = "DOE NIGHTLY SSN";
    private static final String NIGHTLY_STUDENT_PIN = "DOE NIGHTLY PIN";
    private static final String PREVIOUS_FIRST_NAME = "DOE PREVIOUS FNAME";
    private static final String PREVIOUS_LAST_NAME = "DOE PREVIOUS LNAME";
    private static final String PREVIOUS_MIDDLE_NAME = "DOE PREVIOUS MNAME";
    private static final String PREVIOUS_SSN = "DOE PREVIOUS SSN";
    private static final String PREVIOUS_STUDENT_PIN = "DOE PREVIOUS PIN";
    private static final String STUDENT_PIN = "DOE PIN";


    /**
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateAllBeans(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateAllBeans(CalculatedField field, X2Broker broker) {
        // Empty block
    }


    /**
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateBean(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.beans.X2BaseBean, com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateBean(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        // Empty block
    }


    /**
     * @see com.follett.fsc.core.k12.business.CalculatedFieldProcedure#updateReferencedBeans(com.follett.fsc.core.k12.beans.CalculatedField,
     *      com.follett.fsc.core.k12.beans.X2BaseBean, com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void updateReferencedBeans(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        // Check if bean is Student and if so, update Student fields
        if (bean.getClass().equals(SisStudent.class)) {
            SisStudent student = (SisStudent) bean;
            String studentPin = (String) student.getFieldValueByAlias(STUDENT_PIN);
            setPreviousField(student, studentPin, NIGHTLY_STUDENT_PIN, PREVIOUS_STUDENT_PIN);
        }
        // Check if bean is Person and if so, update Person fields
        if (bean.getClass().equals(SisPerson.class)) {
            SisPerson person = (SisPerson) bean;
            setPreviousField(person, person.getPersonId(), NIGHTLY_SSN, PREVIOUS_SSN);
            setPreviousField(person, person.getFirstName(), NIGHTLY_FIRST_NAME, PREVIOUS_FIRST_NAME);
            setPreviousField(person, person.getLastName(), NIGHTLY_LAST_NAME, PREVIOUS_LAST_NAME);
            setPreviousField(person, person.getMiddleName(), NIGHTLY_MIDDLE_NAME, PREVIOUS_MIDDLE_NAME);
        }
        broker.saveBeanForced(bean);
    }


    /**
     * Sets the previous field.
     *
     * @param bean X2BaseBean
     * @param currentVal String
     * @param nightlyAliasName String
     * @param prevAliasName String
     */
    private void setPreviousField(X2BaseBean bean, String currentVal, String nightlyAliasName, String prevAliasName) {
        String nightlyVal = (String) bean.getFieldValueByAlias(nightlyAliasName);

        if ((currentVal == null && !StringUtils.isEmpty(nightlyVal)) ||
                (currentVal != null && !currentVal.equals(nightlyVal))) {
            // We don't want valid previous values getting blanked out with nulls or ""
            if (!StringUtils.isEmpty(nightlyVal)) {
                bean.setFieldValueByAlias(prevAliasName, nightlyVal);
            }

            bean.setFieldValueByAlias(nightlyAliasName, currentVal);
        }
    }
}
