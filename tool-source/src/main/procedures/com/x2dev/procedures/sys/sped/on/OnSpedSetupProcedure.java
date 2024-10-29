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
package com.x2dev.procedures.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.IepAccommodation;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepGoal;
import com.x2dev.sis.model.beans.IepOtherService;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.StringUtils;
import java.sql.Connection;
import java.sql.Statement;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnSpedSetupProcedure extends ProcedureJavaSource {

    // Set DDX OID to standard values.
    private static final String IEP_DDX_OID = "ddxOnSpedIep";
    private static final String IEP_DDX_ID = "ON-SPED-IEP";
    private static final String INV_DDX_OID = "ddxOnInventory";
    private static final String INV_DDX_ID = "ON-SPED-INVENTORY";
    private static final String IEP_MEETING_ID = "ON-SPED-MTG";
    private static final String IEP_FDX_OID_MEETING_LETTER = "fdxOnSpEDIep96";

    private static final String TABLE_DDX = "DATA_DICTIONARY_EXTENDED";
    private static final String TABLE_TBX = "DATA_TABLE_EXTENDED";
    private static final String TABLE_FDX = "DATA_FIELD_EXTENDED";
    private static final String TABLE_FMD = "FORM_DEFINITION";
    private static final String TABLE_VWT = "VIEW_TEMPLATE";
    private static final String TABLE_FSN = "FIELD_SET";
    private static final String TABLE_RPT = "REPORT";
    private static final String TABLE_EFD = "EXPORT_FORMAT_DEFINITION";
    private static final String TABLE_UDA = "USER_DEFINED_TABLE_A";
    private static final String TABLE_UDB = "USER_DEFINED_TABLE_B";
    private static final String TABLE_UDC = "USER_DEFINED_TABLE_C";
    private static final String TABLE_IEP = "IEP_DATA";
    private static final String TABLE_IOS = "IEP_OTHER_SERVICE";
    private static final String TABLE_IAC = "STUDENT_ACCOMMODATION";
    private static final String FIELD_DDX = "DDX_OID";
    private static final String FIELD_TBX = "TBX_DDX_OID";
    private static final String FIELD_FMD = "FMD_DDX_OID";
    private static final String FIELD_VWT = "VWT_DDX_OID";
    private static final String FIELD_FSN = "FSN_DDX_OID";
    private static final String FIELD_RPT = "RPT_DDX_OID";
    private static final String FIELD_EFD = "EFD_DDX_OID";
    private static final String FIELD_UDA = "UDA_DDX_OID";
    private static final String FIELD_UDB = "UDB_DDX_OID";
    private static final String FIELD_UDC = "UDC_DDX_OID";
    private static final String FIELD_IEP = "IEP_DDX_OID";
    private static final String FIELD_IOS = "IOS_DDX_OID";
    private static final String FIELD_IAC = "IAC_DDX_OID";
    private static final String FIELD_FDX = "FDX_OID";

    // Set the dependencies on inventory type, category and description.
    private static final String ALIAS_ORGANIZATION_LANGUAGE = "all-org-BoardLanguage";
    private static final String ALIAS_IEP_COURSE_DESCR = "igl-course-desc";
    private static final String ALIAS_IMG_DATE_LETTER = "img-date-letter";
    private static final String ALIAS_INV_TYPE = "uda-spedinv-type";
    private static final String ALIAS_INV_CATEGORY = "uda-spedinv-category";
    private static final String ALIAS_INV_DESCRIPTION = "uda-spedinv-description";
    private static List<String> INV_ALIASES =
            Arrays.asList(ALIAS_INV_TYPE, ALIAS_INV_CATEGORY, ALIAS_INV_DESCRIPTION);

    private static final String VALIDATION_ID_ASSESSMENT = "ASSESSMENT-OTHER";
    private static final String VALIDATION_ID_SERVICE = "SERVICE-OTHER";
    private static List<String> VALIDATION_IDS =
            Arrays.asList(VALIDATION_ID_ASSESSMENT, VALIDATION_ID_SERVICE);

    // Remove problematic field leftover from previous INV dictionary.
    private static final String ALIAS_INV_MODEL = "uda-spedinv-model";

    // Set the template extended dictionaries.
    private static final String VWT_CONTEXT_INV = "spedinv";
    private static final String VWT_CONTEXT_INV_ITEM = "spedinv.item";
    private static final String VWT_CONTEXT_INV_STUDENT = "spedstdinv";
    private static List<String> INV_VWT_CONTEXTS =
            Arrays.asList(VWT_CONTEXT_INV, VWT_CONTEXT_INV_ITEM, VWT_CONTEXT_INV_STUDENT);

    // Set the behavior on the IEP Services record.
    private static final String CONFIG_SERVICE_END = "fddX2000003878"; // isvEndDate
    private static final String CONFIG_SERVICE_TYPE = "fddX2000003871"; // isvServiceType
    private static final String CONFIG_DISABILITY_CODE = "fddX2000003736"; // idbDisability
    private static final String CONFIG_IEP_GOAL_GOAL = "fddX2000003792"; // iglGoal
    private static final String SERVICE_TYPE_REF_TABLE_NAME = "Service Type Codes";

    private static final String REFTABLE_EXCEPTIONALITY_OID = "rtbOnExcptlity";
    private static final String REFTABLE_EXCEPTIONALITY_NAME = "OnSIS Exceptionality Codes";

    private static final String EFD_SEA_ID = "SYS-SPED-ON-SEA-CL";
    private static final String RPT_INV_ITM_ID = "ON-SPED-INV-ITEM";
    private static final String RPT_INV_STD_ID = "ON-SPED-INV-STD";
    private static List<String> INV_RPT_IDS = Arrays.asList(RPT_INV_ITM_ID, RPT_INV_STD_ID);

    // Set up Inventory field sets.
    private static final String FSN_INV_ITEM_CONTEXT = "global.spedinvitm.list";
    private static final String FSN_INV_STUDENT_CONTEXT = "global.spedinvstd.list";
    private static final String FSN_INV_NAME = "Inventory Fields";

    DataDictionary m_inventoryDictionary;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // Updates to data dictionaries.
        boolean updated = false;
        updated |= setTableProperties();
        updated |= checkIepDdxOid();
        updated |= checkDdxNewFieldsOid();
        updated |= removeIepDdxValidations();
        updated |= checkIepDdxFields();
        updated |= checkInventoryDdxOid();
        updated |= checkInventoryDdxDependencies();

        // reload data dictionary to pick up any changes.
        if (updated) {
            DataDictionaryCache.clearDictionaries(getBroker().getPersistenceKey(), true);
        }
        LocalizationCache.reload(getBroker().getPersistenceKey(), true);
        ExtendedDataDictionary invDdx = getBroker().getBeanByOid(ExtendedDataDictionary.class, INV_DDX_OID);
        m_inventoryDictionary = DataDictionary.getDistrictDictionary(invDdx, getBroker().getPersistenceKey());

        // Set preference for IEP ddx.
        String prefIepDdxOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.SPED_DEFAULT_IEP_DEFINITION);
        if (!IEP_DDX_OID.equals(prefIepDdxOid)) {
            PreferenceManager.setPreferenceValue(getOrganization(), getBroker(),
                    SisPreferenceConstants.SPED_DEFAULT_IEP_DEFINITION, IEP_DDX_OID);
            updated = true;
            logMessage(" Setting SPED Dictionary preference value.");
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SystemPreferenceDefinition.COL_KEY, SisPreferenceConstants.SPED_MEETING_FORM_DEFINITION);
        BeanQuery prfQuery = new BeanQuery(SystemPreferenceDefinition.class, criteria);
        SystemPreferenceDefinition prefDef = getBroker().getBeanByQuery(prfQuery);
        if (prefDef == null) {
            criteria = new X2Criteria();
            criteria.addEqualTo(FormDefinition.COL_ID, IEP_MEETING_ID);
            BeanQuery formQuery = new BeanQuery(FormDefinition.class, criteria);
            FormDefinition form = getBroker().getBeanByQuery(formQuery);
            if (form != null) {
                PreferenceManager.addNewPreferenceDefinition(SisPreferenceConstants.SPED_MEETING_FORM_DEFINITION,
                        "Meeting form definition", "sped", null, 1, getBroker(), true);
                prefDef = getBroker().getBeanByQuery(prfQuery);
                if (prefDef != null) {
                    prefDef.setDefaultValue(form.getOid());
                    getBroker().saveBeanForced(prefDef);
                    PreferenceManager.addPreferenceDefinition(prefDef, true);
                }
                updated = true;
                logMessage(" Setting SPED Meeting Form preference value.");
            } else {
                logMessage("   ** WARNING : SPED Meeting Form Def not found : " + IEP_MEETING_ID);
            }
        }

        // Set french language resources.
        updateIEPStatusPreferences();
        updateWorkflowNames();
        
        // After dictionary updates take affect adjust other dictionary dependent resources.
        updated |= checkInventoryResources();
        updated |= setupFieldSets();
    }

    /**
     * Determine if the board is a french language board.
     *
     * @return boolean
     */
    protected boolean isFrenchBoard() {
        boolean isFrench = false;
        Organization org = getOrganization();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_ORGANIZATION_LANGUAGE);
        if (field != null) {
            String lang = (String) org.getFieldValueByBeanPath(field.getJavaName());
            if (!StringUtils.isEmpty(lang)) {
                ReferenceTable refTable = field.getReferenceTable();
                if (refTable != null) {
                    Map<String, ReferenceCode> codes = refTable.getCodeMap(getBroker());
                    ReferenceCode code = codes.get(lang);
                    if (code != null) {
                        isFrench = ("F".equals(code.getStateCode()));
                    }
                }
            }
        }
        return isFrench;
    }

    /**
     * Check the OID of the IEP extended dictionary, and change it to the pre-defined OID.
     */
    private boolean checkIepDdxOid() {
        boolean updated = false;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, IEP_DDX_ID);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = getBroker().getBeanByQuery(query);
        if (ddx != null) {
            String oldDdxOid = ddx.getOid().trim();
            if (!IEP_DDX_OID.equals(oldDdxOid)) {
                updated = true;
                cleanOldDdxTables(IEP_DDX_OID);
                updateOidField(TABLE_DDX, FIELD_DDX, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_TBX, FIELD_TBX, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_FMD, FIELD_FMD, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_VWT, FIELD_VWT, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_FSN, FIELD_FSN, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_RPT, FIELD_RPT, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_IEP, FIELD_IEP, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_IOS, FIELD_IOS, oldDdxOid, IEP_DDX_OID);
                updateOidField(TABLE_IAC, FIELD_IAC, oldDdxOid, IEP_DDX_OID);
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ExtendedDataDictionary.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ExtendedDataTable.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(FormDefinition.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ViewTemplate.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(FieldSet.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(Report.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(IepData.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(IepOtherService.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(IepAccommodation.class.getName());
            }
        } else {
            logMessage("** Extended Dictionary for IEP (" + IEP_DDX_ID + ") not found!");
        }
        return updated;
    }

    /**
     * Check the OID of some new IEP extended fields, and change it to the pre-defined OID.
     */
    private boolean checkDdxNewFieldsOid() {
        boolean updated = false;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataField.COL_ALIAS, ALIAS_IMG_DATE_LETTER);
        BeanQuery query = new BeanQuery(ExtendedDataField.class, criteria);
        ExtendedDataField fdx = getBroker().getBeanByQuery(query);
        if (fdx != null) {
            String oldFdxOid = fdx.getOid().trim();
            if (!IEP_FDX_OID_MEETING_LETTER.equals(oldFdxOid)) {
                updated = true;
                updateOidField(TABLE_FDX, FIELD_FDX, oldFdxOid, IEP_FDX_OID_MEETING_LETTER);
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ExtendedDataField.class.getName());
                
                X2Criteria rsmCriteria = new X2Criteria();
                rsmCriteria.addEqualTo(MessageResource.COL_OBJECT_OID, oldFdxOid);
                BeanQuery rsmQuery = new BeanQuery(MessageResource.class, rsmCriteria);
                Collection<MessageResource> resources = getBroker().getCollectionByQuery(rsmQuery);
                for (MessageResource resource : resources) {
                    resource.setObjectOid(IEP_FDX_OID_MEETING_LETTER);
                    resource.setKey(resource.getKey().replace(oldFdxOid, IEP_FDX_OID_MEETING_LETTER));
                    getBroker().saveBeanForced(resource);
                }
            }
        } else {
            logMessage("** Extended Dictionary field for Date of Letter (" + ALIAS_IMG_DATE_LETTER + ") not found!");
        }
        return updated;
    }

    /**
     * Correct the dependency fields Type, Category and Description so the reference tables link.
     *
     * @return boolean, true if and change was made.
     */
    private boolean checkInventoryDdxDependencies() {
        boolean updated = false;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataField.REL_EXTENDED_DATA_TABLE + ModelProperty.PATH_DELIMITER +
                ExtendedDataTable.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, INV_DDX_ID);
        criteria.addIn(ExtendedDataField.COL_ALIAS, INV_ALIASES);

        BeanQuery query = new BeanQuery(ExtendedDataField.class, criteria);
        Map<String, ExtendedDataField> fdxs = getBroker().getMapByQuery(query, ExtendedDataField.COL_ALIAS, 3);
        ExtendedDataField typeField = fdxs.get(ALIAS_INV_TYPE);
        ExtendedDataField categoryField = fdxs.get(ALIAS_INV_CATEGORY);
        ExtendedDataField descrField = fdxs.get(ALIAS_INV_DESCRIPTION);

        if (typeField != null && categoryField != null) {
            String typeOid = typeField.getDataFieldConfig().getDataFieldOid().trim();
            if (!typeOid.equals(categoryField.getDependency())) {
                categoryField.setDependency(typeOid);
                getBroker().saveBeanForced(categoryField);
                logMessage(" Fixing up Inventory Category reference dependency.");
                updated = true;
            }
        }
        if (categoryField != null && descrField != null) {
            String typeOid = categoryField.getDataFieldConfig().getDataFieldOid().trim();
            if (!typeOid.equals(descrField.getDependency())) {
                descrField.setDependency(typeOid);
                getBroker().saveBeanForced(descrField);
                logMessage(" Fixing up Inventory Description reference dependency.");
                updated = true;
            }
        }

        // Remove problematic dictionary field that might be left over from previous dictionary.
        criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataField.REL_EXTENDED_DATA_TABLE + ModelProperty.PATH_DELIMITER +
                ExtendedDataTable.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, INV_DDX_ID);
        criteria.addEqualTo(ExtendedDataField.COL_ALIAS, ALIAS_INV_MODEL);
        query = new BeanQuery(ExtendedDataField.class, criteria);
        ExtendedDataField fdx = getBroker().getBeanByQuery(query);
        if (fdx != null) {
            getBroker().deleteBean(fdx);
            logMessage(" Removing inventory model field: " + ALIAS_INV_MODEL);
        }

        return updated;
    }

    /**
     * Move the course description field to a larger field (100) if it is currently in a C field
     * (50).
     *
     * @return boolean, true if any change was made.
     */
    private boolean checkIepDdxFields() {
        boolean updated = false;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataField.REL_EXTENDED_DATA_TABLE + ModelProperty.PATH_DELIMITER +
                ExtendedDataTable.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, IEP_DDX_ID);
        criteria.addEqualTo(ExtendedDataField.COL_ALIAS, ALIAS_IEP_COURSE_DESCR);
        BeanQuery query = new BeanQuery(ExtendedDataField.class, criteria);
        ExtendedDataField fdx = getBroker().getBeanByQuery(query);
        if (fdx != null) {
            DataFieldConfig fieldConfig = fdx.getDataFieldConfig();
            if (fieldConfig != null) {
                DataField field = fieldConfig.getDataField();
                if (!field.isLob()) {
                    // Need to move the field to larger field.
                    DataTable table = fdx.getExtendedDataTable().getDataTableConfig().getDataTable();
                    DataFieldConfig newFieldConfig = DataDictionary.getAvailableField(
                            table.getOid(), 100, fdx.getExtendedDataTableOid(), getBroker());
                    if (newFieldConfig != null) {
                        DataField newField = newFieldConfig.getDataField();
                        fdx.setDataFieldConfigOid(newFieldConfig.getOid());
                        fdx.setUserLength(100);
                        fdx.setUserType("Text");
                        getBroker().saveBean(fdx);
                        updateFieldValue(table.getDatabaseName(), field.getDatabaseName(),
                                newField.getDatabaseName(), true);
                        AppGlobals.getCache(getBroker().getPersistenceKey()).clear(IepGoal.class.getName());
                    }
                }
            }
        }

        return updated;
    }

    /**
     * Check the OID of the Sped Inventory extended dictionary, and change it to the pre-defined
     * OID.
     *
     * @return boolean, true if any change was made.
     */
    private boolean checkInventoryDdxOid() {
        boolean updated = false;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, INV_DDX_ID);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = getBroker().getBeanByQuery(query);
        if (ddx != null) {
            String oldDdxOid = ddx.getOid().trim();
            if (!INV_DDX_OID.equals(oldDdxOid)) {
                updated = true;
                cleanOldDdxTables(INV_DDX_OID);
                updateOidField(TABLE_DDX, FIELD_DDX, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_TBX, FIELD_TBX, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_EFD, FIELD_EFD, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_VWT, FIELD_VWT, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_FSN, FIELD_FSN, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_RPT, FIELD_RPT, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_UDA, FIELD_UDA, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_UDB, FIELD_UDB, oldDdxOid, INV_DDX_OID);
                updateOidField(TABLE_UDC, FIELD_UDC, oldDdxOid, INV_DDX_OID);
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ExtendedDataDictionary.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ExtendedDataTable.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ExportFormatDefinition.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(ViewTemplate.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(FieldSet.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(Report.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(UserDefinedTableA.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(UserDefinedTableB.class.getName());
                AppGlobals.getCache(getBroker().getPersistenceKey()).clear(UserDefinedTableC.class.getName());
            }
        } else {
            logMessage("** Extended Dictionary for SPED Inventory (" + INV_DDX_ID + ") not found!");
        }
        return updated;
    }

    /**
     * If the IEP DDX OID is not the standard, first look for orphaned TBX records
     * that refer to the standard and remove them. Otherwise they will reattach to
     * the new DDX and become duplicates.
     */
    private void cleanOldDdxTables(String ddxOidToRemove) {
        StringBuilder builder = new StringBuilder();
        builder.append("DELETE FROM DATA_FIELD_EXTENDED ");
        builder.append("WHERE FDX_TBX_OID IN (");
        builder.append("SELECT TBX_OID FROM DATA_TABLE_EXTENDED ");
        builder.append("WHERE TBX_DDX_OID = '").append(ddxOidToRemove).append("')");
        String sql1 = builder.toString();
        builder = new StringBuilder();
        builder.append("DELETE FROM DATA_TABLE_EXTENDED ");
        builder.append("WHERE TBX_DDX_OID = '").append(ddxOidToRemove).append("'");
        String sql2 = builder.toString();

        Connection conn = null;
        Statement stmt = null;
        try {
            conn = getBroker().borrowConnection();
            stmt = conn.createStatement();
            int updateCount = stmt.executeUpdate(sql1);
            if (updateCount > 0) {
                logMessage(" Removing orphaned " + ddxOidToRemove + " FDX entries: " + Integer.toString(updateCount)
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
                logMessage(" Removing orphaned " + ddxOidToRemove + " TBX entries: " + Integer.toString(updateCount)
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
    }

    /**
     * Update the inventory resources to reference the
     * extended dictionary if they do not already do so.
     *
     * @return boolean, true if any record has been updated.
     */
    private boolean checkInventoryResources() {
        boolean updated = false;
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(ViewTemplate.COL_CONTEXT, INV_VWT_CONTEXTS);
        BeanQuery query = new BeanQuery(ViewTemplate.class, criteria);
        Collection<ViewTemplate> templates = getBroker().getCollectionByQuery(query);
        for (ViewTemplate template : templates) {
            if (!INV_DDX_OID.equals(template.getExtendedDataDictionaryOid())) {
                template.setExtendedDataDictionaryOid(INV_DDX_OID);
                getBroker().saveBeanForced(template);
                updated = true;
            }
        }

        criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, EFD_SEA_ID);
        query = new BeanQuery(ExportFormatDefinition.class, criteria);
        Collection<ExportFormatDefinition> exports = getBroker().getCollectionByQuery(query);
        for (ExportFormatDefinition export : exports) {
            if (!INV_DDX_OID.equals(export.getExtendedDataDictionaryOid())) {
                export.setExtendedDataDictionaryOid(INV_DDX_OID);
                getBroker().saveBeanForced(export);
                updated = true;
            }
        }

        criteria = new X2Criteria();
        criteria.addIn(Report.COL_ID, INV_RPT_IDS);
        query = new BeanQuery(Report.class, criteria);
        Collection<Report> reports = getBroker().getCollectionByQuery(query);
        for (Report report : reports) {
            if (!INV_DDX_OID.equals(report.getExtendedDataDictionaryOid())) {
                report.setExtendedDataDictionaryOid(INV_DDX_OID);
                getBroker().saveBeanForced(report);
                updated = true;
            }
        }

        return updated;
    }

    /**
     * On the IEP Services table,
     * set the reference table, and turn off the end date required flag.
     * On Student Disability table,
     * turn on disability code required flag.
     *
     * @return boolean, true if any change was made.
     */
    private boolean setTableProperties() {
        boolean updated = false;
        DataFieldConfig configField = getBroker().getBeanByOid(DataFieldConfig.class, CONFIG_IEP_GOAL_GOAL);
        if (configField != null && configField.getRequiredIndicator()) {
            configField.setRequiredIndicator(false);
            getBroker().saveBeanForced(configField);
            logMessage(" Setting Goal field to not-required.");
            updated = true;
        }
        configField = getBroker().getBeanByOid(DataFieldConfig.class, CONFIG_SERVICE_END);
        if (configField != null && configField.getRequiredIndicator()) {
            configField.setRequiredIndicator(false);
            getBroker().saveBeanForced(configField);
            logMessage(" Setting Service End Date field to not-required.");
            updated = true;
        }
        configField = getBroker().getBeanByOid(DataFieldConfig.class, CONFIG_SERVICE_TYPE);
        if (configField != null) {
            String refTableOid = configField.getReferenceTableOid();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceTable.COL_USER_NAME, SERVICE_TYPE_REF_TABLE_NAME);
            BeanQuery query = new BeanQuery(ReferenceTable.class, criteria);
            ReferenceTable newRefTable = getBroker().getBeanByQuery(query);
            if (newRefTable != null && !newRefTable.getOid().trim().equals(refTableOid.trim())) {
                configField.setReferenceTableOid(newRefTable.getOid());
                getBroker().saveBeanForced(configField);
                logMessage(" Setting Service Type field reference table.");
                updated = true;
            }
        }
        configField = getBroker().getBeanByOid(DataFieldConfig.class, CONFIG_DISABILITY_CODE);
        if (configField != null) {
            boolean disabilityUpdated = false;
            ReferenceTable excRefTbl = getBroker().getBeanByOid(ReferenceTable.class, REFTABLE_EXCEPTIONALITY_OID);
            if (excRefTbl == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceTable.COL_USER_NAME, REFTABLE_EXCEPTIONALITY_NAME);
                BeanQuery query = new BeanQuery(ReferenceTable.class, criteria);
                excRefTbl = getBroker().getBeanByQuery(query);
            }
            if (!configField.getRequiredIndicator()) {
                configField.setRequiredIndicator(true);
                disabilityUpdated = true;
            }
            if (excRefTbl != null && !(excRefTbl.getOid().equals(configField.getReferenceTableOid()))) {
                configField.setReferenceTableOid(excRefTbl.getOid());
                disabilityUpdated = true;
            }
            if (disabilityUpdated) {
                getBroker().saveBeanForced(configField);
                logMessage(" Setting Exceptionality Code field.");
                updated = true;
            }
        }
        return updated;
    }

    /**
     * Set up field sets on the Inventory custom navigation lists.
     *
     * @return boolean, true if any change was made.
     */
    private boolean setupFieldSets() {
        boolean updated = false;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(FieldSet.COL_CONTEXT, FSN_INV_ITEM_CONTEXT);
        criteria.addEqualTo(FieldSet.COL_NAME, FSN_INV_NAME);
        BeanQuery query = new BeanQuery(FieldSet.class, criteria);
        Collection<FieldSet> fieldsets = getBroker().getCollectionByQuery(query);
        if (fieldsets.size() == 0) {
            // Add the standard field set.
            FieldSet fieldSet = X2BaseBean.newInstance(FieldSet.class, getBroker().getPersistenceKey());
            fieldSet.setContext(FSN_INV_ITEM_CONTEXT);
            fieldSet.setName(FSN_INV_NAME);
            fieldSet.setExtendedDataDictionaryOid(INV_DDX_OID);
            fieldSet.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
            fieldSet.setOwnerType(1);
            getBroker().saveBeanForced(fieldSet);

            addFieldSetMember(fieldSet.getOid(), null, "uda-spedinv-type", null, 0);
            addFieldSetMember(fieldSet.getOid(), null, "uda-spedinv-category", null, 1);
            addFieldSetMember(fieldSet.getOid(), null, "uda-spedinv-description", null, 2);
            addFieldSetMember(fieldSet.getOid(), null, "uda-spedinv-sea", null, 3);

            logMessage(" Adding field set '" + FSN_INV_NAME + "' to " + FSN_INV_ITEM_CONTEXT);
            updated = true;
        }

        criteria = new X2Criteria();
        criteria.addEqualTo(FieldSet.COL_CONTEXT, FSN_INV_STUDENT_CONTEXT);
        criteria.addEqualTo(FieldSet.COL_NAME, FSN_INV_NAME);
        query = new BeanQuery(FieldSet.class, criteria);
        fieldsets = getBroker().getCollectionByQuery(query);
        if (fieldsets.size() == 0) {
            // Add the standard field set.
            FieldSet fieldSet = X2BaseBean.newInstance(FieldSet.class, getBroker().getPersistenceKey());
            fieldSet.setContext(FSN_INV_STUDENT_CONTEXT);
            fieldSet.setName(FSN_INV_NAME);
            fieldSet.setExtendedDataDictionaryOid(INV_DDX_OID);
            fieldSet.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
            fieldSet.setOwnerType(1);
            getBroker().saveBeanForced(fieldSet);

            addFieldSetMember(fieldSet.getOid(), "stdViewName", null, "relUdbStdOid", 0);
            addFieldSetMember(fieldSet.getOid(), null, "udc-spedinv-serial-no", "relUdbUdcOid", 1);
            addFieldSetMember(fieldSet.getOid(), null, "uda-spedinv-category", "relUdbUdcOid.relUdcUdaOid", 2);
            addFieldSetMember(fieldSet.getOid(), null, "uda-spedinv-description", "relUdbUdcOid.relUdcUdaOid", 3);
            addFieldSetMember(fieldSet.getOid(), null, "uda-spedinv-sea", "relUdbUdcOid.relUdcUdaOid", 4);
            addFieldSetMember(fieldSet.getOid(), null, "udc-spedinv-funding-source", "relUdbUdcOid", 5);

            logMessage(" Adding field set '" + FSN_INV_NAME + "' to " + FSN_INV_STUDENT_CONTEXT);
            updated = true;
        }

        return updated;
    }

    /**
     * Add a Field set member to a field set.
     *
     * @param fieldSetOid
     * @param fieldOid
     * @param relation
     * @param sequence
     */
    private void addFieldSetMember(String fieldSetOid,
                                   String fieldOid,
                                   String fieldAlias,
                                   String relation,
                                   int sequence) {
        if (fieldAlias != null) {
            DataDictionaryField ddField = m_inventoryDictionary.findDataDictionaryFieldByAlias(fieldAlias);
            if (ddField != null) {
                fieldOid = ddField.getId();
            }
        }
        FieldSetMember fieldMember = X2BaseBean.newInstance(FieldSetMember.class, getBroker().getPersistenceKey());
        fieldMember.setFieldSetOid(fieldSetOid);
        fieldMember.setFieldType(0);
        fieldMember.setWidth(0);
        fieldMember.setRelation(relation);
        fieldMember.setObjectOid(fieldOid);
        fieldMember.setSequenceNumber(sequence);
        getBroker().saveBeanForced(fieldMember);
    }

    /**
     * Use direct SQL to alter the OID field on a record.
     * All parameters are database structure names.
     *
     * @param table
     * @param field
     * @param oldOid
     * @param newOid
     */
    private void updateOidField(String table, String field, String oldOid, String newOid) {
        StringBuilder builder = new StringBuilder();
        builder.append("UPDATE ").append(table);
        builder.append(" SET ").append(field);
        builder.append(" = '").append(newOid);
        builder.append("' WHERE ").append(field);
        builder.append(" = '").append(oldOid).append("'");
        String sql = builder.toString();

        Connection conn = null;
        Statement stmt = null;
        try {
            conn = getBroker().borrowConnection();
            stmt = conn.createStatement();
            int updateCount = stmt.executeUpdate(sql);
            if (updateCount > 0) {
                logMessage("  Updating " + table + " to '" + newOid + "' : " + Integer.toString(updateCount)
                        + " Records.");
            }
        } catch (Exception e) {
            logMessage(" Exc: [" + e.getMessage() + "] on: " + sql);
        } finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (Exception e) {
                    logMessage(" Exc closing statement for: " + sql);
                }
                stmt = null;
            }
            getBroker().returnConnection();
        }
    }

    /**
     * Use SQL to copy data from one field to another field, and optionally clear the onld field.
     *
     * @param table Database Table Name
     * @param oldField Database Field Name
     * @param newField Database Field Name
     * @param clear boolean, clear old field after
     */
    private void updateFieldValue(String table, String oldField, String newField, boolean clear) {
        StringBuilder builder = new StringBuilder();
        builder.append("UPDATE ").append(table);
        builder.append(" SET ").append(newField);
        builder.append(" = ").append(oldField);
        String sql = builder.toString();

        builder = new StringBuilder();
        builder.append("UPDATE ").append(table);
        builder.append(" SET ").append(oldField);
        builder.append(" = NULL");
        String sql2 = builder.toString();

        Connection conn = null;
        Statement stmt = null;
        try {
            boolean err = false;
            conn = getBroker().borrowConnection();
            try {
                stmt = conn.createStatement();
                int updateCount = stmt.executeUpdate(sql);
                if (updateCount > 0) {
                    logMessage("  Updating " + table + ", copy " + oldField + " to " + newField + " : "
                            + Integer.toString(updateCount) + " Records.");
                }
            } catch (Exception e) {
                logMessage(" Exc: [" + e.getMessage() + "] on: " + sql);
                err = true;
            } finally {
                if (stmt != null) {
                    try {
                        stmt.close();
                    } catch (Exception e) {
                        logMessage(" Exc closing statement for: " + sql);
                        err = true;
                    }
                    stmt = null;
                }
            }
            if (clear && !err) {
                try {
                    stmt = conn.createStatement();
                    int updateCount = stmt.executeUpdate(sql2);
                    if (updateCount > 0) {
                        logMessage("  Updating " + table + ", clear field " + oldField + " : "
                                + Integer.toString(updateCount) + " Records.");
                    }
                } catch (Exception e) {
                    logMessage(" Exc: [" + e.getMessage() + "] on: " + sql2);
                    err = true;
                } finally {
                    if (stmt != null) {
                        try {
                            stmt.close();
                        } catch (Exception e) {
                            logMessage(" Exc closing statement for: " + sql2);
                            err = true;
                        }
                        stmt = null;
                    }
                }
            }
        } catch (Exception e) {
            logMessage(" Exc: [" + e.getMessage() + "] on: " + sql);
        } finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (Exception e) {
                    logMessage(" Exc closing statement for: " + sql);
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
    private boolean removeIepDdxValidations() {
        int removed = 0;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DataValidationRule.REL_EXTENDED_DATA_TABLE + ModelProperty.PATH_DELIMITER +
                ExtendedDataTable.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER +
                ExtendedDataDictionary.COL_ID, IEP_DDX_ID);
        criteria.addIn(DataValidationRule.COL_ID, VALIDATION_IDS);
        BeanQuery query = new BeanQuery(DataValidationRule.class, criteria);
        Collection<DataValidationRule> rules = getBroker().getCollectionByQuery(query);
        DataValidationRule assessmentOther = null;
        DataValidationRule servicesOther = null;

        for (DataValidationRule rule : rules) {
            if (VALIDATION_ID_ASSESSMENT.equals(rule.getId())) {
                if (assessmentOther == null) {
                    assessmentOther = rule;
                } else {
                    getBroker().deleteBean(rule);
                    removed++;
                }
            } else if (VALIDATION_ID_SERVICE.equals(rule.getId())) {
                if (servicesOther == null) {
                    servicesOther = rule;
                } else {
                    getBroker().deleteBean(rule);
                    removed++;
                }
            }
        }

        if (removed > 0) {
            logMessage(" Removing " + Integer.toString(removed) + " duplicate validation rules.");
        }
        return (removed > 0);
    }

    /**
     * Set the french language preferences for IEP status and IPE Meeting types.
     * Requires SQL since the field is CLOB, not NCLOB and
     * setting through the app corrupts special characters.
     */
    private void updateIEPStatusPreferences() {
        if (isFrenchBoard()) {
            String[][] prefsList = new String[][] {
                    {SisPreferenceConstants.IEP_STATUS_0, "Ébauche"},
                    {SisPreferenceConstants.IEP_STATUS_1, "Actif"},
                    {SisPreferenceConstants.IEP_STATUS_2, "Précédent"},
                    {SisPreferenceConstants.IEP_STATUS_3, "Modifié"},
                    {SisPreferenceConstants.IEP_STATUS_4, "Refusé"},
                    {SisPreferenceConstants.IEP_STATUS_5, "Ébauche des modifications"},
                    {SisPreferenceConstants.IEP_STATUS_6, "Modification rejetée"},
                    {SisPreferenceConstants.IEP_STATUS_7, "Rejeté"},
                    {SisPreferenceConstants.IEP_STATUS_8, "En attente d'approbation"},
                    {SisPreferenceConstants.IEP_MEETING_TYPE_0, "Initial"},
                    {SisPreferenceConstants.IEP_MEETING_TYPE_1, "Révision"},
                    {SisPreferenceConstants.IEP_MEETING_TYPE_2, "Modification"},
                    {SisPreferenceConstants.IEP_MEETING_TYPE_3, ""},
                    {SisPreferenceConstants.IEP_MEETING_TYPE_4, "Autre"}};

            for (String[] pref : prefsList) {
                try {
                    PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), pref[0], pref[1]);
                } catch (InvalidPreferenceException e) {
                    logMessage("ERROR: Setting preference value: " + pref[0] + ": " + e.getMessage());
                    e.printStackTrace();
                }
            }
        }
    }
    
    /**
     * Update the workflow names to french for frnech districts.
     */
    private void updateWorkflowNames() {
        if (isFrenchBoard()) {
            String[] workflows = new String[] {
                    "SYS-SPED-AMEND",
                    "SYS-SPED-REFER",
                    "SYS-SPED-RENEW",
                    "SYS-SPED-REEVAL"};
            for (String wfd: workflows) {
                try {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(WorkflowDefinition.COL_ID, wfd);
                    BeanQuery query = new BeanQuery(WorkflowDefinition.class, criteria);
                    WorkflowDefinition definition = getBroker().getBeanByQuery(query);
                    if (definition != null) {
                        String nameKey = LocalizationUtils.generateKey(definition.getOid(), "wfdName");
                        String descKey = LocalizationUtils.generateKey(definition.getOid(), "wfdDescription");
                        String frName = LocalizationCache.getMessages(getBroker().getPersistenceKey(), "fr_FR", true).getMessage(nameKey);
                        String frDesc = LocalizationCache.getMessages(getBroker().getPersistenceKey(), "fr_FR", true).getMessage(descKey);
                        if (frName != null) {
                            definition.setName(frName);
                        }
                        if (frDesc != null) {
                            definition.setDescription(frDesc);
                        }
                        if (definition.isDirty()) {
                            getBroker().saveBeanForced(definition);
                            logMessage("  Setting Workflow name and description: " + wfd);
                        }
                    }
                } catch (Exception e) {
                    logMessage("ERROR: Setting Workflow namevalue: " + wfd + ": " + e.getMessage());
                    e.printStackTrace();
                }
            }
        }
    }
}
