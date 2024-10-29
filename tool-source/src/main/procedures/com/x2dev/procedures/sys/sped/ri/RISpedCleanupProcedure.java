/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.sql.Connection;
import java.sql.Statement;
import java.util.Collection;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class RISpedCleanupProcedure extends ProcedureJavaSource {

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        cleanOldDdxTables("RI-SLD");
        removeFormdef("RI-SLD");        
    }

    /**
     * If the IEP DDX OID is not the standard, first look for orphaned TBX records
     * that refer to the standard and remove them. Otherwise they will reattach to
     * the new DDX and become duplicates.
     */
    private void cleanOldDdxTables(String ddxIdToRemove) {
        StringBuilder builder = new StringBuilder();
        builder.append("DELETE FROM DATA_FIELD_EXTENDED ");
        builder.append("WHERE FDX_TBX_OID IN (");
        builder.append("SELECT TBX_OID FROM DATA_TABLE_EXTENDED ");
        builder.append("INNER JOIN DATA_DICTIONARY_EXTENDED on TBX_DDX_OID = DDX_OID ");
        builder.append("WHERE DDX_ID = '").append(ddxIdToRemove).append("')");
        String sql1 = builder.toString();
        
        builder = new StringBuilder();
        builder.append("DELETE FROM DATA_TABLE_EXTENDED ");
        builder.append("WHERE TBX_DDX_OID IN ( ");
        builder.append("SELECT DDX_OID FROM DATA_DICTIONARY_EXTENDED WHERE DDX_ID = '").append(ddxIdToRemove).append("')");
        String sql2 = builder.toString();

        builder = new StringBuilder();
        builder.append("DELETE FROM DATA_DICTIONARY_EXTENDED WHERE DDX_ID = '").append(ddxIdToRemove).append("'");
        String sql3 = builder.toString();

        Connection conn = null;
        Statement stmt = null;
        try {
            conn = getBroker().borrowConnection();
            stmt = conn.createStatement();
            int updateCount = stmt.executeUpdate(sql1);
            if (updateCount > 0) {
                logMessage(" Removing " + ddxIdToRemove + " FDX entries: " + Integer.toString(updateCount)
                        + " Records.");
            }
        } catch (Exception e) {
            logMessage(" Exc: [" + e.getMessage() + "] on: " + sql1);
        } finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (Exception e) {
                    logMessage(" Exc closing statement for: " + sql1);
                }
                stmt = null;
            }
            getBroker().returnConnection();
        }
        try {
            conn = getBroker().borrowConnection();
            stmt = conn.createStatement();
            int updateCount = stmt.executeUpdate(sql2);
            if (updateCount > 0) {
                logMessage(" Removing " + ddxIdToRemove + " TBX entries: " + Integer.toString(updateCount)
                        + " Records.");
            }
        } catch (Exception e) {
            logMessage(" Exc: [" + e.getMessage() + "] on: " + sql2);
        } finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (Exception e) {
                    logMessage(" Exc closing statement for: " + sql2);
                }
                stmt = null;
            }
            getBroker().returnConnection();
        }
        try {
            conn = getBroker().borrowConnection();
            stmt = conn.createStatement();
            int updateCount = stmt.executeUpdate(sql3);
            if (updateCount > 0) {
                logMessage(" Removing " + ddxIdToRemove + " DDX entries: " + Integer.toString(updateCount)
                        + " Records.");
            }
        } catch (Exception e) {
            logMessage(" Exc: [" + e.getMessage() + "] on: " + sql3);
        } finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (Exception e) {
                    logMessage(" Exc closing statement for: " + sql3);
                }
                stmt = null;
            }
            getBroker().returnConnection();
        }
    }

    /**
     * The DDX import can duplicate table validation rules.
     * this will remove excess duplicate copies.
     *
     * @return boolean if records were removed.
     */
    private void removeFormdef(String formDefId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FormDefinition.COL_ID, formDefId);
        BeanQuery query = new BeanQuery(FormDefinition.class, criteria);
        Collection<FormDefinition> forms = getBroker().getCollectionByQuery(query);
        for (FormDefinition form : forms) {
            getBroker().deleteBean(form);
        }
    }
}
