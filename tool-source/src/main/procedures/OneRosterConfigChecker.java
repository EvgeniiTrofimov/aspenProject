/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class OneRosterConfigChecker extends ProcedureJavaSource {

    private static final String EXECUTION_MODE_PARAM = "executionMode";

    private final int mode_check_config = 0;
    private final int mode_update_config = 1;

    private List<OneRosterEnumeration> genderAliasValues;
    private List<OneRosterEnumeration> personRelAliasValues;
    private List<OneRosterEnumeration> staffTypeCodeAliasValues;

    private final String REFERENCE_TABLE_OID_GENDER_CODES = "rtbGenderCode";
    private final String REFERENCE_TABLE_OID_PERSON_RELATIONSHIP_CODES = "rtbPsnRelate";
    private final String REFERENCE_TABLE_OID_STAFF_TYPE_CODES = "rtbStaffType";

    private final String DATABASE_COLUMN_GENDER_CODE = "PSN_GENDER_CODE";
    private final String DATABASE_COLUMN_STAFF_TYPE = "STF_STAFF_TYPE";
    private final String DATABASE_COLUMN_STUDENT_CONTACT_RELATIONSHIP = "CTJ_RELATIONSHIP_CODE";

    private final String ONE_ROSTER_ENUMARTION_ALIAS = "rcd_oneroster_enumeration";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        int executionMode = ((Integer) getParameter(EXECUTION_MODE_PARAM)).intValue();

        boolean update = false;
        if (mode_update_config == executionMode) {
            update = true;
        }

        if (validateRequiredConfig()) {
            processConfig(REFERENCE_TABLE_OID_GENDER_CODES, genderAliasValues, ONE_ROSTER_ENUMARTION_ALIAS, update);

            processConfig(REFERENCE_TABLE_OID_PERSON_RELATIONSHIP_CODES, personRelAliasValues,
                    ONE_ROSTER_ENUMARTION_ALIAS,
                    update);

            processConfig(REFERENCE_TABLE_OID_STAFF_TYPE_CODES, staffTypeCodeAliasValues, ONE_ROSTER_ENUMARTION_ALIAS,
                    update);

            logMessage(
                    "!!!!IMPORTANT: Review the codes on each reference table listed above. There may be additional codes we do not check for that require ORE values to set up.\r\n"
                            + "     Examples:\r\n"
                            + "             The 'Staff Type Codes' ref table may have additional codes that represent teachers. They will need the one roster enumeration value specified.\r\n"
                            + "             The 'Person Relationship Codes' ref table may have additional codes that represent parents or guardians. Specify the appropriate one roster enumeration value for each.\r\n"
                            + "             The 'Gender Codes' ref table may have unique gender codes that represent male, female, uknown, etc.\r\n"
                            + "     Ensuring these codes are set up properly may require communication with the district.\r\n"
                            + "     To see a list of valid one roster enumeration values, review the java source for this procedure, in the loadOneRosterData() method.\r\n"
                            + "     The acceptable values are the right most String value for each entry for the appropriate table.\r\n\r\n");

            // Now validate the values stored in the appropriate columns all have valid reference
            // code values on the appropriate reference table
            identifyMissingReferenceCodes(DATABASE_COLUMN_GENDER_CODE);
            identifyMissingReferenceCodes(DATABASE_COLUMN_STUDENT_CONTACT_RELATIONSHIP);
            identifyMissingReferenceCodes(DATABASE_COLUMN_STAFF_TYPE);
        }
    }

    /**
     * Identifies missing reference codes from the passed in database column. Locates the Data Field
     * Config
     * for this column, identifies the reference table, and queries the column to see existing
     * values.
     *
     * @param dbColumn
     *
     * @throws ClassNotFoundException
     */
    private void identifyMissingReferenceCodes(String dbColumn) throws ClassNotFoundException {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DataFieldConfig.REL_DATA_FIELD + ModelProperty.PATH_DELIMITER + DataField.COL_DATABASE_NAME,
                dbColumn);
        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);
        DataFieldConfig field = (DataFieldConfig) getBroker().getBeanByQuery(query);

        if (field != null) {
            Class tableClass = Class.forName(field.getDataField().getDataTable().getClassName());

            Criteria valueCriteria = new Criteria();
            valueCriteria.addNotNull(field.getDataField().getJavaName());

            // Get distinct codes in this field
            ReportQueryByCriteria codesQuery = new ReportQueryByCriteria(tableClass,
                    new String[] {field.getDataField().getJavaName()}, valueCriteria, true);

            Collection<String> missingCodes = findMissingCodes(codesQuery, field.getReferenceTable());
            if (!missingCodes.isEmpty()) {
                StringBuilder string = new StringBuilder();
                string.append(
                        "WARNING: Missing reference codes identified. You may encounter errors with API calls if not corrected.\r\n");
                string.append(
                        "The following missing codes were identified by querying distinct values in the '" + dbColumn
                                + "' column and comparing those values to what exist in the '"
                                + field.getReferenceTable().getUserName() + "' reference table\r\n");
                for (String code : missingCodes) {
                    string.append("code: '" + code + "'\r\n");
                }

                logMessage(string.toString());
            } else {
                logMessage("No missing codes identified on the '" + field.getReferenceTable().getUserName()
                        + "' reference table.\r\n");
            }
        } else {
            logMessage(
                    "ERROR: Unable to find data field config for database column: '" + dbColumn + "'\r\n");
        }
    }

    /**
     * Compares the passed in query to existing reference codes to identify missing reference codes.
     *
     * @param query
     * @param table
     *
     * @return Collection<String>
     */
    private Collection<String> findMissingCodes(ReportQueryByCriteria query, ReferenceTable table) {
        Collection<String> missingCodes = new ArrayList<String>();

        // Get existing codes to avoid duplicates
        X2Criteria existingCodeCriteria = new X2Criteria();
        existingCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, table.getOid());
        SubQuery existingCodeQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, existingCodeCriteria);
        Collection<String> existingCodes = getBroker().getSubQueryCollectionByQuery(existingCodeQuery);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                String value = ((Object[]) iterator.next())[0].toString();

                if (!existingCodes.contains(value)) {
                    missingCodes.add(value);
                }
            }
        } finally {
            iterator.close();
        }

        return missingCodes;
    }

    /**
     * Validates required config exists. Does not care whether mode is to check config or update
     * config, this always happens.
     *
     * return boolean
     */
    private boolean validateRequiredConfig() {
        boolean success = true;
        DataFieldConfig config = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DataFieldConfig.COL_ALIAS, ONE_ROSTER_ENUMARTION_ALIAS);
        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);

        config = getBroker().getBeanByQuery(query);
        if (config != null) {
            logMessage("Success - Found DataFieldConfig for DataTable: "
                    + config.getDataField().getDataTable().getClassName() + " with alias value: "
                    + ONE_ROSTER_ENUMARTION_ALIAS + " - field: " + config.getDataField().getDatabaseName() + "\r\n");
        } else {
            logMessage(
                    "FAIL - missing required setup of the rcd_oneroster_enumeration alias on the Reference Code table.\r\n\r\nPlease follow the setup instructions below.\r\n"
                            + "\r\nCreate a new UDF by doing the following:  Go to District, Admin > Data Dictionary "
                            + "\r\nSelect table:  Reference Code - Common > Fields"
                            + "\r\nSet Filter to All Records"
                            + "\r\nSelect an unused User Defined Field (B Field) and enter the following values:"
                            + "\r\n     Long Name:  One Roster"
                            + "\r\n     Short Name:  One Roster"
                            + "\r\n     Alias:  rcd_oneroster_enumeration"
                            + "\r\n     User Data Type:  Character"
                            + "\r\n     Enabled:  check"
                            + "\r\n     List edit:  check"
                            + "\r\nSAVE > Options > Reload Data Dictionary"
                            + "\r\n\r\nThis application will not proceed past this validation step until the steps above have been completed.");

            success = false;
        }

        return success;
    }

    /**
     * Processes the config for the passed in reference table oid. Will either validate the config
     * exists, or update the config.
     *
     * @param refTableOid
     * @param config
     * @param orAlias
     * @param update
     */
    private void processConfig(String refTableOid, List<OneRosterEnumeration> config, String orAlias, boolean update) {
        logMessage("Processing reference table oid: '" + refTableOid + "'");
        ReferenceTable refTable = getBroker().getBeanByOid(ReferenceTable.class, refTableOid);

        if (refTable != null) {
            logMessage("Reference table name: '" + refTable.getUserName() + "'");
            Map<String, ReferenceCode> genderCodes = refTable.getCodeMap(null, null, null, true);

            for (OneRosterEnumeration ore : config) {
                ReferenceCode rcd = genderCodes.get(ore.getCode());
                if (rcd != null) {
                    Object value = rcd.getFieldValueByAlias(orAlias);
                    if (update) {
                        if (value != null && ore.getOrValue().equals(value.toString())) {
                            logMessage("    SKIPPED UPDATE: Code: '" + rcd.getCode()
                                    + "' already has expected ORE alias value: '" + value.toString() + "'");
                        } else {
                            rcd.setFieldValueByAlias(orAlias, ore.getOrValue());
                            getBroker().saveBeanForced(rcd);
                            logMessage("    UPDATE CODE: Code: '" + rcd.getCode() + "' - updated ORE alias value to: '"
                                    + ore.getOrValue() + "'");
                        }
                    } else {
                        if (value != null && ore.getOrValue().equals(value.toString())) {
                            logMessage("    PASS: Found Match on code: '" + rcd.getCode() + "' - " + orAlias + ": '"
                                    + value.toString() + "'");
                        } else {
                            logMessage(
                                    "    WARNING: Existing code found but one roster alias value does not match. code: '"
                                            + rcd.getCode() + "' - "
                                            + orAlias + ": '" + (value != null ? value.toString() : "'")
                                            + " - expected alias: '"
                                            + ore.getOrValue() + "'");
                        }
                    }
                } else {
                    logMessage("    WARNING: No ref code found matching the expected code value of: '" + ore.getCode()
                            + "' - Nothing exists to update.");
                }
            }
        } else {
            logMessage("Unable to find matching ref table with oid: '" + refTableOid + "'");
        }

        logMessage("\r\n");
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        loadOneRosterData(); // Pre load the expected config.
    }

    /**
     * Loads the one roster config data. This is used to validate existing config or update config
     * based on input options.
     */
    private void loadOneRosterData() {

        /*
         * If customer uses custom codes that do not match the code value expected below, you can
         * update as needed to suit the needs of the district. Do not commit those changes.
         *
         * Example: We expect a gender code with a code value of "M" to exist. A customer may have
         * "Male" instead.
         *
         * A customer may have multiple codes that represent a male, or multiple codes that
         * represent an unknown gender. In that case, you can add additional lines below to
         * accommodate those codes. They should not be committed.
         */

        genderAliasValues = new ArrayList<OneRosterEnumeration>();
        genderAliasValues.add(new OneRosterEnumeration("F", "female"));
        genderAliasValues.add(new OneRosterEnumeration("M", "male"));
        genderAliasValues.add(new OneRosterEnumeration("N", "non-binary"));
        genderAliasValues.add(new OneRosterEnumeration("U", "unknown"));

        personRelAliasValues = new ArrayList<OneRosterEnumeration>();
        personRelAliasValues.add(new OneRosterEnumeration("Father", "parent"));
        personRelAliasValues.add(new OneRosterEnumeration("Foster Parents", "parent"));
        personRelAliasValues.add(new OneRosterEnumeration("Guardian", "guardian"));
        personRelAliasValues.add(new OneRosterEnumeration("Mother", "parent"));
        personRelAliasValues.add(new OneRosterEnumeration("Parents", "parent"));
        personRelAliasValues.add(new OneRosterEnumeration("Step Father", "parent"));
        personRelAliasValues.add(new OneRosterEnumeration("Step Mother", "parent"));
        personRelAliasValues.add(new OneRosterEnumeration("Step Parents", "parent"));

        staffTypeCodeAliasValues = new ArrayList<OneRosterEnumeration>();
        staffTypeCodeAliasValues.add(new OneRosterEnumeration("Teacher", "Teacher"));
    }

    /**
     * The Class OneRosterEnumeration. The code matches the reference code code value. The orValue
     * represents the value to place in the 'rcd_oneroster_enumaration' aliased field on the
     * reference code.
     */
    private class OneRosterEnumeration {
        private String m_code;
        private String m_orValue;

        public OneRosterEnumeration(String code, String orValue) {
            m_code = code;
            m_orValue = orValue;
        }

        /**
         * @return the code
         */
        public String getCode() {
            return m_code;
        }

        /**
         * @param code
         */
        public void setCode(String code) {
            this.m_code = code;
        }

        /**
         * @return the orValue
         */
        public String getOrValue() {
            return m_orValue;
        }

        /**
         * @param orValue
         */
        public void setOrValue(String orValue) {
            this.m_orValue = orValue;
        }
    }
}
