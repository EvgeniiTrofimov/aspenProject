/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.CalculatedField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.CalculatedFieldProcedure;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.logging.Level;

/**
 * Calculate Age
 *
 * This procedure was completely rewritten to use the person table and update
 * the table by alias. We now use the built-in methods for constructing the age
 * of a person, rather than manually attempting to do so. Allowing us to stay in
 * step with changes to the application, maintaining consistency.
 *
 * This procedure will work on newly generated records. Existing records will
 * only be triggered by DOB being updated by default. Other trigger fields may
 * be set, but caution should be used.
 *
 * @author Follett School Solutions
 */
public class CalculateAge implements CalculatedFieldProcedure {

    /**
     * Set parameter for Person table alias
     */
    public static final String PERSON_AGE_ALIAS = "person-age";

    private static final String PERSON_AGE_ERROR = "No alias of 'person-age' assigned to Person table";

    /**
     * This method runs when recalculating for everyone. This is performed by
     * going to Dist. View, Admin > Data Dictionary > Calculated Fields >
     * Details > Options > Recalculate All References. Avoid adding logging to
     * this method to prevent system log flooding.
     */
    @Override
    public void updateAllBeans(CalculatedField field, X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotNull(Person.COL_DOB);
        criteria.addLessOrEqualThan(Person.COL_DOB, new PlainDate());
        BeanQuery query = new BeanQuery(Person.class, criteria);


        try (QueryIterator iterator = broker.getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Person person = (Person) iterator.next();
                try {
                    updateAge(person, broker);
                } catch (X2BaseException ex) {
                    AppGlobals.getLog().log(Level.WARNING, PERSON_AGE_ERROR);
                }
                broker.saveBeanForced(person);
            }
        }
    }

    /**
     * This method is used when creating a new record. No other records will be
     * recalculated for their age. Do NOT add a saveBean method or you will
     * introduce an infinite loop.
     */
    @Override
    public void updateBean(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        try {
            updateAge((Person) bean, broker);
        } catch (X2BaseException ex) {
            AppGlobals.getLog().log(Level.WARNING, PERSON_AGE_ERROR);
        }
    }

    /**
     * This method is only run when an existing record is changed, and relies on
     * the aliasing and trigger fields to be correctly setup. We must save the
     * bean for this method to keep the change.
     */
    @Override
    public void updateReferencedBeans(CalculatedField field, X2BaseBean bean, X2Broker broker) {
        try {
            updateAge((Person) bean, broker);
        } catch (X2BaseException ex) {
            AppGlobals.getLog().log(Level.WARNING, PERSON_AGE_ERROR);
        }
        broker.saveBeanForced(bean);
    }

    /**
     * Updates each person record's age based on date of birth for the alias
     * 'person-age'.
     *
     * @param person
     * @param broker
     */
    private void updateAge(Person person, X2Broker broker) throws X2BaseException {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(broker.getPersistenceKey());
        DataDictionaryField ddField = dictionary.findDataDictionaryFieldByAlias(PERSON_AGE_ALIAS);

        PlainDate dateOfBirth = person.getDob();
        PlainDate today = new PlainDate();
        String age = null;

        /*
         * Check to see if we're setup correctly. Don't perform any other checks
         * until this is correct.
         */
        if (ddField != null) {
            /*
             * Sanity check before working.
             */
            if (dateOfBirth == null || dateOfBirth.after(today)) {
                AppGlobals.getLog().log(Level.WARNING, "Person: " + person.getOid() + ", DOB: " + dateOfBirth
                        + ", Can't be born in the future. They have been set to an age " + "of '0'.");
                age = "0";
            } else {
                /*
                 * We do the same function that core does to determine the age
                 * throughout Aspen.
                 */
                age = Integer.toString(person.getAge());
            }
            /*
             * As long as we pass our checks, we want to do something with this
             * field.
             */
            person.setFieldValueByAlias(PERSON_AGE_ALIAS, age);
        }
    }
}
