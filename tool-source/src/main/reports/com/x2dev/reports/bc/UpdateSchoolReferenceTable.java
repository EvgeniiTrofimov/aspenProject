/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisSchool;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure will create or update a specific reference table to include each active school
 * name as a code. This
 * reference table is used as a location selection for the Course Exam Assessment Definition.
 *
 * @author Follett Software Company
 */
public class UpdateSchoolReferenceTable extends ProcedureJavaSource {
    /**
     * Constant - reference table name.
     */
    public static final String REFERENCE_TABLE_USER_NAME = "Course Exam School Names";

    /**
     *
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Collection<String> schoolNames = loadSchoolNames();

        String schoolNamesTableOid = getOrCreateReferenceTable();

        Collection<String> existingCodes = loadExistingCodeValues(schoolNamesTableOid);
        if (existingCodes == null) {
            existingCodes = new ArrayList<String>();
        }

        boolean addedNewCodes = false;

        /*
         * Iterate over each school name and add as a reference code if it doesn't exist already.
         */
        for (String school : schoolNames) {
            if (!existingCodes.contains(school)) {
                ReferenceCode code = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
                code.setReferenceTableOid(schoolNamesTableOid);
                code.setCode(school);
                code.setDescription(school);
                code.setDisabledIndicator(false);
                code.setSequenceNumber(0);
                code.setOwnerType(Ownable.OWNER_TYPE_ORG1);
                code.setOwnerOid(getOrganization().getOrganization(0).getOid());

                getBroker().saveBeanForced(code);

                logMessage("Added new code: " + school);
                addedNewCodes = true;
            }
        }

        if (!addedNewCodes) {
            logMessage("No new codes added.");
        }
    }

    /**
     * Returns the existing reference table for school names or creates a new one with static user
     * name.
     *
     * @return String
     */
    private String getOrCreateReferenceTable() {
        Criteria refTableCriteria = new Criteria();
        refTableCriteria.addEqualTo(ReferenceTable.COL_USER_NAME, REFERENCE_TABLE_USER_NAME);

        QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, refTableCriteria);
        ReferenceTable table = (ReferenceTable) getBroker().getBeanByQuery(query);

        if (table == null) {
            table = X2BaseBean.newInstance(ReferenceTable.class, getBroker().getPersistenceKey());
            table.setUserName(REFERENCE_TABLE_USER_NAME);
            table.setCategory("Assessment");
            table.setCodeLength(50);
            table.setDataTableOid("tblRefCode");

            // Just in case this is ran at a lower organization level, set the owner to the root
            // organization.
            table.setOwnerOid(getOrganization().getOrganization(0).getOid());
            table.setOwnerType(Ownable.OWNER_TYPE_ORG1);

            getBroker().saveBeanForced(table);

            logMessage("Created Reference Table: " + REFERENCE_TABLE_USER_NAME);
        } else {
            logMessage("Found existing Reference Table: " + REFERENCE_TABLE_USER_NAME);
        }

        return table.getOid();
    }

    /**
     * Builds a Collection<String> of all existing reference code Code values for the specified
     * Reference Table.
     *
     * @param oid String
     * @return Collection<String>
     */
    private Collection<String> loadExistingCodeValues(String oid) {
        Criteria codeCriteria = new Criteria();
        codeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, oid);

        SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, codeCriteria);

        Collection<String> existingCodes = getBroker().getSubQueryCollectionByQuery(query);

        return existingCodes;
    }

    /**
     * Loads a Collection of all active schools (non-archive/non-history).
     *
     * @return Collection
     */
    private Collection<String> loadSchoolNames() {
        Criteria schoolCriteria = new Criteria();
        schoolCriteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
        schoolCriteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);

        SubQuery query = new SubQuery(SisSchool.class, SisSchool.COL_NAME, schoolCriteria);

        return getBroker().getSubQueryCollectionByQuery(query);
    }
}
