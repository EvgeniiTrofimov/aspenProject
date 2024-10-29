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
package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.utils.DataGrid;
import java.util.Collections;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Setup for IL Student Course Assignment
 * <p>
 * Find an available field in the STUDENT_TRANSCRIPT table and assign it an alias so that
 * {@link StudentCourseAssignment} may use it.
 *
 * @author Follett Software Company
 *
 */
public class StudentCourseAssignmentSetup extends ProcedureJavaSource {

    /**
     * The alias of the field
     */
    private static final String FIELD_ALIAS = "DOE COURSE OVERRIDE TRN";

    /**
     * The long name of the field
     */
    private static final String FIELD_LONG_NAME = "Course Number Override";


    /**
     * The short name of the field
     */
    private static final String FIELD_SHORT_NAME = "Course Number Override";

    /**
     * The data type of the field
     */
    private static final String FIELD_DATA_TYPE = "Character";

    /**
     * The length of the field
     */
    private static final int FIELD_LENGTH = 10;

    /**
     * The decimal length of the field
     */
    private static final int FIELD_DECIMAL = 0;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(FIELD_ALIAS);

        // If the field does not exist, create it
        if (dataField == null) {
            DataFieldConfig availableFdd = findAvailableField(dataField);
            if (availableFdd != null) {
                saveField(availableFdd);
            } else {
                logMessage("Unable to find a non-conflicting, available FieldA on the STUDENT_TRANSCRIPT table!");
            }
        } else {
            logMessage("There already exists " + FIELD_ALIAS + " field on the STUDENT_TRANSCRIPT table!");
        }
    }

    /**
     * Find an available "A" field (the higher numbered FieldA field, the better) in the
     * STUDENT_TRANSCRIPT table. This is to avoid any conflicts with transcript definitions
     *
     * @param field DataDictionaryField
     * @return DataFieldConfig
     */
    private DataFieldConfig findAvailableField(DataDictionaryField field) {
        // get the list data field configs that all transcript column definitions use
        SubQuery gtcSubQuery = new SubQuery(TranscriptColumnDefinition.class,
                TranscriptColumnDefinition.COL_DATA_FIELD_CONFIG_OID,
                null);

        List<String> gtcOids = (List<String>) getBroker().getSubQueryCollectionByQuery(gtcSubQuery);
        gtcOids.removeAll(Collections.singleton(null)); // remove any nulls from the list

        // find an the available fields that DO NOT use any of the data field configs that
        // transcript column defs use
        X2Criteria fddCriteria = new X2Criteria();
        fddCriteria.addNotIn(X2BaseBean.COL_OID, gtcOids);
        fddCriteria.addBeginsWith(X2BaseBean.COL_OID, "fddX2TRNA"); // preferably fieldA's
        fddCriteria.addEqualTo(DataFieldConfig.COL_ENABLED_INDICATOR, Boolean.FALSE);
        SubQuery fddQuery = new SubQuery(DataFieldConfig.class, "MAX(FDD_OID)", fddCriteria);

        Criteria criteria = new Criteria();
        criteria.addIn(X2BaseBean.COL_OID, fddQuery);
        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);

        DataFieldConfig fdd = (DataFieldConfig) getBroker().getBeanByQuery(query);
        return fdd;

    }

    /**
     * Set and save the field.
     *
     * @param fdd DataFieldConfig
     */
    private void saveField(DataFieldConfig fdd) {
        String alias = fdd.getAlias();
        String longName = fdd.getUserLongName();
        String shortName = fdd.getUserShortName();
        String userType = fdd.getUserType();
        int length = fdd.getUserLength();
        int decimal = fdd.getUserDecimal();

        // update the field
        fdd.setAlias(FIELD_ALIAS);
        fdd.setUserLongName(FIELD_LONG_NAME);
        fdd.setUserShortName(FIELD_SHORT_NAME);
        fdd.setUserType(FIELD_DATA_TYPE);
        fdd.setUserLength(FIELD_LENGTH);
        fdd.setUserDecimal(FIELD_DECIMAL);
        fdd.setEnabledIndicator(true);
        fdd.setListEditIndicator(true);
        fdd.setUpdateIndicator(true);

        // save the bean and print a before/after grid
        if (fdd.isDirty()) {
            DataGrid grid = new DataGrid(6, 2);
            grid.append();
            grid.set("Before", alias);
            grid.set("After", fdd.getAlias());

            grid.append();
            grid.set("Before", longName);
            grid.set("After", fdd.getUserLongName());

            grid.append();
            grid.set("Before", shortName);
            grid.set("After", fdd.getUserShortName());

            grid.append();
            grid.set("Before", userType);
            grid.set("After", fdd.getUserType());

            grid.append();
            grid.set("Before", Integer.valueOf(length));
            grid.set("After", Integer.valueOf(fdd.getUserLength()));

            grid.append();
            grid.set("Before", Integer.valueOf(decimal));
            grid.set("After", Integer.valueOf(fdd.getUserDecimal()));

            // save it, log it, clear the cache
            getBroker().saveBeanForced(fdd);
            logMessage(grid.format(true, false, false));
            DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
        }
    }

}
