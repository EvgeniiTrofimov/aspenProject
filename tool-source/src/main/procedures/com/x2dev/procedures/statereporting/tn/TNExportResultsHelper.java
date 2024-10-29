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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import java.util.Collection;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNExportResultsHelper.
 */
public class TNExportResultsHelper {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    private static final String PROCESSED = "Processed:";

    /**
     * This method returns the collection of all the export format fields for the given export
     * format result.
     *
     * @param result ExportFormatResult
     * @param broker X2Broker
     * @return Collection
     */
    public static Collection<ExportFormatField> getExportFormatField(ExportFormatResult result, X2Broker broker) {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(X2BaseBean.COL_OID, result.getOid());
        SubQuery efrSubQuery =
                new SubQuery(ExportFormatResult.class, ExportFormatResult.COL_DEFINITION_OID, efrCriteria);

        X2Criteria ukiCriteria = new X2Criteria();
        ukiCriteria.addIn(ExportFormatResult.COL_DEFINITION_OID, efrSubQuery);

        QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, ukiCriteria);
        query.addOrderByAscending(ExportFormatField.COL_POSITION);

        Collection<ExportFormatField> exportFields = broker.getCollectionByQuery(query);
        return exportFields;
    }

    /**
     * This method returns the export format result for the selected result's oid.
     *
     * @param resultOid String
     * @param broker X2Broker
     * @return Export format result
     */
    public static ExportFormatResult getExportFormatResult(String resultOid, X2Broker broker) {
        ExportFormatResult result = null;

        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(X2BaseBean.COL_OID, resultOid);

        QueryByCriteria query = new QueryByCriteria(ExportFormatResult.class, efrCriteria);

        Collection efrCollection = broker.getCollectionByQuery(query);
        if (!efrCollection.isEmpty()) {
            result = (ExportFormatResult) (efrCollection.iterator().next());
        }
        return result;
    }

    /**
     * Gets the last processed result.
     *
     * @param importExportDefinitionOid String
     * @param broker X2Broker
     * @return Export format result
     */
    public static ExportFormatResult getLastProcessedResult(String importExportDefinitionOid, X2Broker broker) {
        ExportFormatResult result = null;
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(ExportFormatResult.COL_DEFINITION_OID, importExportDefinitionOid);
        efrCriteria.addBeginsWith(ExportFormatResult.COL_COMMENT, PROCESSED);

        QueryByCriteria query = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        query.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

        QueryIterator results = broker.getIteratorByQuery(query);
        try {
            if (results.hasNext()) {
                result = (ExportFormatResult) results.next();
            }
        } finally {
            if (results != null) {
                results.close();
            }
        }

        return result;
    }

    /**
     * Gets the latest result.
     *
     * @param importExportDefinitionOid String
     * @param broker X2Broker
     * @return Export format result
     */
    public static ExportFormatResult getLatestResult(String importExportDefinitionOid, X2Broker broker) {
        ExportFormatResult result = null;
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addEqualTo(ExportFormatResult.COL_DEFINITION_OID, importExportDefinitionOid);

        QueryByCriteria query = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        query.addOrderByDescending(ExportFormatResult.COL_RUN_DATE);

        QueryIterator results = broker.getIteratorByQuery(query);
        try {
            if (results.hasNext()) {
                result = (ExportFormatResult) results.next();
            }
        } finally {
            if (results != null) {
                results.close();
            }
        }

        return result;
    }

    /**
     * Sets the result processed.
     *
     * @param result ExportFormatResult
     * @param broker X2Broker
     */
    public static void setResultProcessed(ExportFormatResult result, X2Broker broker) {
        result.setComment(PROCESSED);
        broker.saveBeanForced(result);
    }
}
