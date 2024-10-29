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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Date;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

/**
 * A custom procedure that will delete all conduct incidents with a deletion date of today or in the
 * past.
 *
 * @author X2 Development Corporation
 */
public class ConductIncidentExpirationProcedure extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;
    private static final String ALIAS_CND_EXPIRE_DATE = "cnd-expired-date";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(ALIAS_CND_EXPIRE_DATE);

        PlainDate today = new PlainDate(new Date());

        // expiration date before or equal today && not null && not ""
        X2Criteria criteria = new X2Criteria();
        criteria.addLessOrEqualThan(dataField.getJavaName(), today);
        criteria.addNotEmpty(dataField.getJavaName(), getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

        int deleteCnt = 0;
        int skipCnt = 0;
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                ConductIncident conduct = (ConductIncident) iterator.next();
                List<ValidationError> errors = getBroker().deleteBean(conduct);

                if (errors.isEmpty()) {
                    deleteCnt++;
                } else {
                    LocalizationMessageResources messageResources =
                            LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());
                    List<String> messages = WebUtils.getValidationErrorMessages(errors, null, messageResources);
                    logMessage(conduct.getOid() + " " + conduct.getIdentifyingValues(getLocale()));
                    logMessage(StringUtils.convertCollectionToDelimitedString(messages, "\n"));
                    logMessage("");
                    skipCnt++;
                }
            }
        } finally {
            iterator.close();
        }

        logMessage("Deleted (" + deleteCnt + ") student conduct incident record(s)");
        logMessage("Skipped (" + skipCnt + ") student conduct incident record(s)");
    }

}
