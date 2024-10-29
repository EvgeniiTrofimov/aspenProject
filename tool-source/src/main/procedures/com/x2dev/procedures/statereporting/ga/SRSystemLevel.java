/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationAttributes;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for Student Record System Level export.
 *
 * @author X2 Development Corporation
 */
public class SRSystemLevel extends StateReportData {
    /**
     * Entity class for Student Record System Level export.
     *
     * @author X2 Development Corporation
     */
    public static class SRSystemLevelEntity extends StateReportEntity {
        /**
         * List of format definitions to use for this entity.
         * The root entity should generate two record types: A01, A02
         */
        private List<String> m_definitionId = new ArrayList<String>();

        /**
         * The organization attribute record associated with this organization
         * that implements the extended dictionary for Student Record System Level values.
         */
        private OrganizationAttributes attribute = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SRSystemLevelEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            // find the OrganizationAttributes record with reporting information.
            Organization org = (Organization) bean;
            String schoolYear = Integer.toString(data.getCurrentContext().getSchoolYear()).trim();
            for (OrganizationAttributes attrs : org.getOrganizationAttributes(data.getBroker())) {
                if (attrs.getExtendedDataDictionary() != null &&
                        ORA_DDX_ID.equals(attrs.getExtendedDataDictionary().getId().trim()) &&
                        schoolYear.equals(attrs.getFieldValueByBeanPath(((SRSystemLevel) data).m_schoolYearField))) {
                    attribute = attrs;
                    break;
                }
            }
            if (attribute == null) {
                // create the organization attribute if it doesn't exist
                Criteria criteria = new Criteria();
                criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DDX_ID);
                QueryByCriteria query = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
                ExtendedDataDictionary ddx = (ExtendedDataDictionary) data.getBroker().getBeanByQuery(query);

                OrganizationAttributes attrs = X2BaseBean.newInstance(OrganizationAttributes.class,
                        data.getBroker().getPersistenceKey());
                attrs.setOrganizationOid(org.getOid());
                attrs.setExtendedDataDictionaryOid(ddx.getOid());
                attrs.setFieldValueByBeanPath(((SRSystemLevel) data).m_schoolYearField, schoolYear);
                data.getBroker().saveBeanForced(attrs);

                attribute = attrs;
            }

            // Set up two reporting rows. Both rows use the same bean.
            m_definitionId.add(null); // definition row 1: A01
            m_definitionId.add("02"); // definition row 2: A02
            setRowCount(m_definitionId.size());
        }

        /**
         * Return the OrganizationAttribute record for this organization and the SR dictionary.
         *
         * @return OrganizationAttributes
         */
        public OrganizationAttributes getAttrib() {
            return attribute;
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Organization org = (Organization) getBean();

            String name = org.getName() +
                    " [ID: " + org.getId() + "]";
            return name;
        }

        /**
         * Return the row definition for the current row.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getCurrentFormatDefinitionId()
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            String definitionId = null;
            if (getCurrentRow() >= 0 && getCurrentRow() < m_definitionId.size()) {
                definitionId = m_definitionId.get(getCurrentRow());
            }
            return definitionId;
        }
    }

    /**
     * Constants for reporting information.
     */
    protected static final String ORA_DDX_ID = "ORA-GA-SR-SYS";
    protected static final String ORA_SCHOOL_YEAR_ALIAS = "srsys-school-year";
    protected static final String ORA_GA_SR_SYS = "srsy-TKES-90%-teaching";


    /**
     * Local variables for reporting information.
     */
    protected DataDictionary m_oraDataDictionary;
    protected String m_schoolYearField;
    protected String m_tkesTeaching;
    protected Map<String, String> m_teachingReferenceMap;

    /**
     * Retriever for finding aliased fields on the
     * OrganizationAttributes record from the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAttributeAliasNumber implements FieldRetriever {
        private Map<String, String> m_aliasToField = new HashMap<String, String>();

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String value = null;
            String alias = (String) field.getParameter();
            String fieldName = null;
            if (m_aliasToField.containsKey(alias)) {
                fieldName = m_aliasToField.get(alias);
            } else {
                DataDictionaryField ddField = m_oraDataDictionary.findDataDictionaryFieldByAlias(alias);
                if (ddField != null) {
                    fieldName = ddField.getJavaName();
                    m_aliasToField.put(alias, fieldName);
                }
            }
            if (!StringUtils.isEmpty(fieldName)) {
                OrganizationAttributes attribs = ((SRSystemLevelEntity) entity).getAttrib();
                if (attribs != null) {
                    value = (String) attribs.getFieldValueByBeanPath(fieldName);
                    // left Trim to max length.
                    if (value != null && value.length() > field.getMaxLength()) {
                        value = value.substring(value.length() - field.getMaxLength());
                    }
                }
            } else {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field, "Field alias not found", alias));
            }
            return value;
        }
    }

    /**
     * Calculation to return on current school year.
     * Return state code from reference table.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeaching implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {

            OrganizationAttributes attribs = ((SRSystemLevelEntity) entity).getAttrib();
            String tkesTeaching = EMPTY_STRING;

            if (m_tkesTeaching != null) {
                tkesTeaching = (String) attribs.getFieldValueByBeanPath(m_tkesTeaching);
            }

            // Find the reference table for teaching
            DataDictionaryField sidField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_GA_SR_SYS);
            m_teachingReferenceMap = new HashMap<String, String>();

            if (sidField != null && sidField.getReferenceTable() != null) {
                for (ReferenceCode code : sidField.getReferenceTable().getReferenceCodes()) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        m_teachingReferenceMap.put(code.getCode(), code.getStateCode());
                    }
                }
            }

            if (m_teachingReferenceMap.containsKey(tkesTeaching)) {
                return m_teachingReferenceMap.get(tkesTeaching).toString();
            }

            return EMPTY_STRING;
        }
    }

    /**
     * Initialize data.
     * Query for root organization.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() {
        // Select one record, for Organization level 1.
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Organization.REL_ORGANIZATION_DEFINITION + ModelProperty.PATH_DELIMITER +
                OrganizationDefinition.COL_LEVEL, Integer.valueOf(0));
        QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);
        setQuery(query);
        setEntityClass(SRSystemLevelEntity.class);

        // Get the extended data dictionary for the OrganizationAttributes "ORA-GA-SR-SYS".
        criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DDX_ID);
        query = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(query);
        if (ddx == null) {
            addSetupError("Extended data dictionary", "No extended data dictionary found for " + ORA_DDX_ID);
        }
        m_oraDataDictionary = DataDictionary.getDistrictDictionary(ddx,
                getBroker().getPersistenceKey());

        // Find the school year field in ORA from its DDX alias.
        DataDictionaryField syField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_SCHOOL_YEAR_ALIAS);
        if (syField != null) {
            m_schoolYearField = syField.getJavaName();
        } else {
            addSetupError("School year alias", "No alias found for " + ORA_SCHOOL_YEAR_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        // Find the school year field in ORA from its DDX alias.
        syField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_GA_SR_SYS);
        if (syField != null) {
            m_tkesTeaching = syField.getJavaName();
        } else {
            addSetupError("TKES teaching", "No alias found for " + ORA_GA_SR_SYS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }

        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("GA-SR-ALIAS", new RetrieveAttributeAliasNumber());
        calcs.put("GA-SR-SYS", new RetrieveTeaching());
        super.addCalcs(calcs);
    }
}
