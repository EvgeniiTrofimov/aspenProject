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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisPerson;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetFLEducatorId.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class SetFLEducatorId extends ProcedureJavaSource {
    private static final String ALIAS_FLEID = "all-psn-StateEducationId";
    private static final Pattern PATTERN_FLEID = Pattern.compile("^FL\\d{12}$");

    Random m_random = new Random();
    Set<String> m_setFLEID = new HashSet();

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        loadCurrent();
        process();
    }

    /**
     * Checks if is valid.
     *
     * @param fleid String
     * @return true, if is valid
     */
    private boolean isValid(String fleid) {
        return fleid == null ? false : PATTERN_FLEID.matcher(fleid).find();
    }

    /**
     * Load current.
     */
    private void loadCurrent() {
        QueryIterator iterator = getBroker().getIteratorByQuery(new QueryByCriteria(SisPerson.class));
    
        try {
            while (iterator.hasNext()) {
                SisPerson person = (SisPerson) iterator.next();
                if (person.getStaffIndicator() || person.getStudentIndicator()) {
                    String fleid = (String) person.getFieldValueByAlias(ALIAS_FLEID);
                    if (isValid(fleid)) {
                        m_setFLEID.add(fleid);
                    }
                }
            }
        } finally {
            iterator.close();
        }
    
    }

    /**
     * New id.
     *
     * @return Object
     */
    private Object newId() {
        String fleid = null;
        do {
            fleid = "FL" + String.format("%012d", Integer.valueOf(m_random.nextInt(10000000)));
        } while (m_setFLEID.contains(fleid));
        m_setFLEID.add(fleid);
        return fleid;
    }

    /**
     * Process.
     */
    private void process() {
        QueryIterator iterator = getBroker().getIteratorByQuery(new QueryByCriteria(SisPerson.class));

        try {
            while (iterator.hasNext()) {
                SisPerson person = (SisPerson) iterator.next();
                if (person.getStaffIndicator() || person.getStudentIndicator()) {
                    String fleid = (String) person.getFieldValueByAlias(ALIAS_FLEID);
                    if (!isValid(fleid)) {
                        person.setFieldValueByAlias(ALIAS_FLEID, newId());
                        getBroker().saveBeanForced(person);
                    }
                }
            }
        } finally {
            iterator.close();
        }

    }

}
