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
package com.x2dev.procedures.statereporting.bc;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.StudentJournal;
import com.x2dev.utils.types.PlainDate;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import java.util.List;
import org.apache.ojb.broker.query.Criteria;

/**
 * Procedure to delete expired journal entries.
 *
 * @author X2 Development Corporation
 */
public class ExpireJournalEntriesData extends ProcedureJavaSource {
    private String journalExpireDateAlias = "jnl-expired-date";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        int count = 0;
        int skipped = 0;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(journalExpireDateAlias);

        if (field != null) {
            Criteria journalCriteria = new Criteria();
            journalCriteria.addLessOrEqualThan(field.getJavaName(), new PlainDate());
            BeanQuery journalQuery = new BeanQuery(StudentJournal.class, journalCriteria);
            QueryIterator iterator = getBroker().getIteratorByQuery(journalQuery);

            try {
                while (iterator.hasNext()) {
                    StudentJournal journal = (StudentJournal) iterator.next();
                    List errors = getBroker().deleteBean(journal);
                    if (errors.isEmpty()) {
                        count++;
                    } else {
                        skipped++;
                    }
                }
            } finally {
                iterator.close();
            }
        }

        logMessage(count + " student journal record" + (count != 1 ? "" : "s") + " deleted.");
        logMessage(skipped + " student journal record" + (skipped != 1 ? "" : "s") + " skipped.");
    }
}
