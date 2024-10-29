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
package com.follett.fsc.core.k12.tools.procedures;

import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.business.AccountCreationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Generates Person security codes.
 */
public class GeneratePersonSecurityCodesProcedure extends ProcedureJavaSource {

    /**
     * Sets up person special codes.
     *
     * @throws Exception exception
     */
    @Override
    protected void execute() throws Exception {
        QueryByCriteria query = new QueryByCriteria(Person.class, getCurrentCriteria());
        Set<Person> people = new HashSet<Person>(getBroker().getCollectionByQuery(query));

        if (generateSpecialCodes(people, getLocale(), getBroker().getPersistenceKey().getDeploymentId(), getBroker())) {
            logMessage("Generated security codes for " + people.size() + " people.");
        } else {
            logMessage("Failed to create security codes!");
        }
    }

    /**
     * Mass enables the user accounts for the passed people.
     *
     * @param people Set<Person>
     * @param locale Locale
     * @param deploymentId String
     * @param broker X2Broker
     * @return boolean
     */
    public static boolean generateSpecialCodes(Set<Person> people,
                                               Locale locale,
                                               String deploymentId,
                                               X2Broker broker) {
        boolean successful = false;

        AccountCreationManager manager = new AccountCreationManager(deploymentId, locale);
        if (manager.findProcedure()) {
            AccountCreationProcedure procedure = manager.getProcedure();
            procedure.createSpecialCodes(people, broker);
            successful = true;
        }

        return successful;
    }
}
