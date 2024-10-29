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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.OrganizationAttributes;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for Student Record School Level export.
 *
 * @author X2 Development Corporation
 */
public class SRSchoolLevel extends StateReportData {
    /**
     * Entity class for Student Record School Level Export.
     *
     * @author X2 Development Corporation
     */
    public static class SRSchoolLevelEntity extends StateReportEntity {

        /**
         * The organization attribute record associated with this organization
         * that implements the extended dictionary for Student Record System Level values.
         */
        private OrganizationAttributes m_attribute = null;

        private SRSchoolLevel m_srData = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SRSchoolLevelEntity() {
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
            m_srData = (SRSchoolLevel) data;

            // find the OrganizationAttributes record with reporting information.
            School school = (School) bean;
            String schoolYear = Integer.toString(data.getCurrentContext().getSchoolYear()).trim();
            String schoolId = (String) school.getFieldValueByBeanPath(m_srData.m_schoolAlias);
            for (OrganizationAttributes attrs : school.getOrganization1().getOrganizationAttributes(data.getBroker())) {
                if (attrs.getExtendedDataDictionary() != null &&
                        !StringUtils.isEmpty(schoolId) &&
                        ORA_DDX_ID.equals(attrs.getExtendedDataDictionary().getId().trim()) &&
                        schoolYear.equals(attrs.getFieldValueByBeanPath(m_srData.m_oraSchoolYearField)) &&
                        schoolId.equals(attrs.getFieldValueByBeanPath((m_srData.m_oraSchoolIdField)))) {
                    m_attribute = attrs;
                    break;
                }
            }

            // If the ALIAS isn't populated for a school, ie DOE School is blank/null, do not return
            // the record.
            if (m_attribute == null && !StringUtils.isEmpty(schoolId)) {
                // create the organization attribute if it doesn't exist
                m_attribute = createOrganizationAttribute();
            }

            if (m_attribute == null && StringUtils.isEmpty(schoolId)) {
                setRowCount(0);
            }

        }

        /**
         * Return the OrganizationAttribute record for this organization and the SR dictionary.
         *
         * @return OrganizationAttributes
         */
        public OrganizationAttributes getAttribute() {
            return m_attribute;
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            School school = (School) getBean();

            String name = school.getName() + " [ID: " + school.getSchoolId() + "]";
            return name;
        }

        /**
         * Create and save an OrganizationAttribute record for this entity's school.
         *
         * @return the new OrganizationAttribute for this entity's school
         */
        private OrganizationAttributes createOrganizationAttribute() {
            StateReportData data = getData();
            School school = (School) getBean();
            String schoolYear = Integer.toString(data.getCurrentContext().getSchoolYear()).trim();
            String schoolId = (String) school.getFieldValueByBeanPath(m_srData.m_schoolAlias);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ORA_DDX_ID);
            QueryByCriteria query = new QueryByCriteria(ExtendedDataDictionary.class, criteria);

            ExtendedDataDictionary ddx = (ExtendedDataDictionary) data.getBroker().getBeanByQuery(query);
            OrganizationAttributes attrs = X2BaseBean.newInstance(OrganizationAttributes.class,
                    data.getBroker().getPersistenceKey());
            attrs.setOrganizationOid(school.getOrganization1Oid());
            attrs.setExtendedDataDictionaryOid(ddx.getOid());
            attrs.setFieldValueByBeanPath(((SRSchoolLevel) data).m_oraSchoolYearField, schoolYear);
            attrs.setFieldValueByBeanPath(((SRSchoolLevel) data).m_oraSchoolIdField, schoolId);
            data.getBroker().saveBeanForced(attrs);

            return attrs;
        }

    }

    /**
     * Constants for reporting information.
     */
    protected static final String ORA_DDX_ID = "ORA-GA-SR-SCHOOL";
    protected static final String ORA_SCHOOL_YEAR_ALIAS = "srskl-school-year";
    protected static final String ORA_SCHOOL_ID_ALIAS = "srskl-school-id";
    protected static final String ORA_TITLE1_SCHOOL_ALIAS = "srskl-title1-school";

    protected static final String ORA_INCIDENT_ALIAS_PREFIX = "srskl-cnd";
    protected static final String ORA_INCIDENT_ARSON_ALIAS = "srskl-cnd-arson";
    protected static final String ORA_INCIDENT_BATTERY_ALIAS = "srskl-cnd-battery";
    protected static final String ORA_INCIDENT_DRUGS_ALIAS = "srskl-cnd-drugs";
    protected static final String ORA_INCIDENT_FELONY_DRUGS_ALIAS = "srskl-cnd-felony-drugs";
    protected static final String ORA_INCIDENT_KIDNAP_ALIAS = "srskl-cnd-kidnap";
    protected static final String ORA_INCIDENT_MANSLAUGHTER_ALIAS = "srskl-cnd-manslaughter";
    protected static final String ORA_INCIDENT_MOLESTATION_ALIAS = "srskl-cnd-molestation";
    protected static final String ORA_INCIDENT_MURDER_ALIAS = "srskl-cnd-murder";
    protected static final String ORA_INCIDENT_RAPE_ALIAS = "srskl-cnd-rape";
    protected static final String ORA_INCIDENT_ROBBERY_ALIAS = "srskl-cnd-robbery";
    protected static final String ORA_INCIDENT_SEX_BATTERY_ALIAS = "srskl-cnd-sex-battery";
    protected static final String ORA_INCIDENT_SODOMY_ALIAS = "srskl-cnd-sodomy";
    protected static final String ORA_INCIDENT_THREATS_ALIAS = "srskl-cnd-threats";
    protected static final String ORA_INCIDENT_WEAPONS_ALIAS = "srskl-cnd-weapons";
    protected static final String ORA_DUAL_ENROLL_8 = "srskl-sw-dual-enroll-8";
    protected static final String ORA_DUAL_ENROLL_9 = "srskl-sw-dual-enroll-9";
    protected static final String ORA_DUAL_ENROLL_10 = "srskl-sw-dual-enroll-10";
    protected static final String ORA_DUAL_ENROLL_11 = "srskl-sw-dual-enroll-11";
    protected static final String ALIAS_DOE_ONLINE_SCHOOL = "DOE ONLINE SCHOOL";
    protected static final String ALIAS_DOE_CPR_AED = "DOE CPR AED";
    protected static final String ALIAS_DOE_SCHOOL = "DOE School";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    protected static final String EXTENDED_DICTIONARY_ID_GRADE_NUMERIC = "REF-GRADE-LEVELS";
    protected static final String ALIAS_NUMERIC_GRADE = "NumericGradeLevel";

    protected static final String PARAM_RETRIEVE = "0";
    protected static final String PARAM_CALCULATE = "1";
    protected static final String PARAM_UPDATE = "2";
    protected static final String NO_N = "N";
    protected static final String YES_Y = "Y";

    /**
     * Local variables for reporting information.
     */
    protected String m_excludeSchool;
    protected DataDictionary m_oraDataDictionary;
    protected String m_oraSchoolYearField;
    protected String m_oraSchoolIdField;
    protected String m_schoolAlias;
    protected String m_dualEnroll8Alias;
    protected String m_dualEnroll9Alias;
    protected String m_dualEnroll10Alias;
    protected String m_dualEnroll11Alias;
    protected Map<String, String> m_title1ReferenceMap;
    protected String m_oraArsonCountField;
    protected String m_oraBatteryField;
    protected String m_oraDrugsField;
    protected String m_oraFelonyDrugsField;
    protected String m_oraKidnapField;
    protected String m_oraManslaughterField;
    protected String m_oraMolestationField;
    protected String m_oraMurderField;
    protected String m_oraRapeField;
    protected String m_oraRobberyField;
    protected String m_oraSexBatteryField;
    protected String m_oraSodomyField;
    protected String m_oraThreatsField;
    protected String m_oraWeaponsField;
    protected String m_conductRetrieval = "0"; // default: "Retrieve" all
    protected String m_doeOnlineSchool;
    protected String m_doeCPRAED;
    protected Map<String, ReferenceCode> m_conductMap;
    protected Map<String, Collection<ConductIncident>> m_conductsMap;
    protected Map<String, String> m_numericStateGradeMap;
    protected Map<String, String> m_yesNoBlankMap;


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
            String reference = null;
            String alias = (String) field.getParameter();
            if (!StringUtils.isEmpty(alias) && alias.contains(",")) {
                String[] split = alias.split(",");
                alias = split[0];
                reference = split[1];
            }
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
                OrganizationAttributes attributes = ((SRSchoolLevelEntity) entity).getAttribute();
                if (attributes != null) {
                    value = (String) attributes.getFieldValueByBeanPath(fieldName);

                    if (!StringUtils.isEmpty(value) && !StringUtils.isEmpty(reference)
                            && StringUtils.isNumeric(reference)) {
                        DataDictionaryField ddField = m_oraDataDictionary.findDataDictionaryFieldByAlias(alias);
                        if (ddField != null && ddField.getExtendedDataField() != null
                                && ddField.getExtendedDataField().getReferenceTableOid() != null) {
                            String refTableOid = ddField.getExtendedDataField().getReferenceTableOid();
                            value = lookupReferenceCodeByRefTbl(refTableOid, value,
                                    Integer.valueOf(reference).intValue());
                        }
                    }

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
     * Retrieve Online School Field.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveOnlineSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            School school = (School) entity.getBean();
            String value = null;
            String onlineSchool = (String) school.getFieldValueByBeanPath(m_doeOnlineSchool);
            if (onlineSchool == null || onlineSchool.equals(PARAM_RETRIEVE)) {
                value = NO_N;
            } else {
                value = YES_Y;
            }
            return value;
        }
    }

    /**
     * Retrieve CPR AED Field.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCprAed implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            School school = (School) entity.getBean();
            String value = null;
            String cprAed = (String) school.getFieldValueByBeanPath(m_doeCPRAED);

            if (!StringUtils.isEmpty(cprAed)) {
                if (cprAed.equals(PARAM_RETRIEVE)) {
                    value = "No";
                } else {
                    value = "Yes";
                }
            }

            if (m_yesNoBlankMap.containsKey(value)) {
                value = m_yesNoBlankMap.get(value);
            }

            return value;
        }
    }

    /**
     * Calculation to return on current school year. Field shall only return if school contains
     * Grade level 08,09,10,11 otherwise, must be blank.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchoolDualEnroll implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String calcParam = (String) field.getParameter();
            School school = (School) entity.getBean();
            int schoolGrade = 0;
            int lastGrade = 0;

            schoolGrade = school.getStartGrade();
            lastGrade = school.getStartGrade() + school.getNumberOfGrades();

            while (schoolGrade <= lastGrade) {
                if (m_numericStateGradeMap.containsKey(Integer.toString(schoolGrade))) {
                    if (m_numericStateGradeMap.get(Integer.toString(schoolGrade)).equals(calcParam)) {
                        return YES_Y;
                    }
                }
                schoolGrade++;
            }

            return EMPTY_STRING;
        }
    }


    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        initGrades();
        initYesNoBlank();
        m_doeOnlineSchool = translateAliasToJavaName(ALIAS_DOE_ONLINE_SCHOOL, true);
        m_doeCPRAED = translateAliasToJavaName(ALIAS_DOE_CPR_AED, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_schoolAlias = translateAliasToJavaName(ALIAS_DOE_SCHOOL, true);

        Criteria criteria = getSchoolCriteria();
        QueryByCriteria query = new QueryByCriteria(School.class, criteria);
        setQuery(query);
        setEntityClass(SRSchoolLevelEntity.class);

        // Get the extended data dictionary for the OrganizationAttributes "ORA-GA-SR-SCHOOL".
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
            m_oraSchoolYearField = syField.getJavaName();
        } else {
            addSetupError("School year alias", "No alias found for " + ORA_SCHOOL_YEAR_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }

        // Find the school Id field in ORA from its DDX alias
        DataDictionaryField sidField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_SCHOOL_ID_ALIAS);
        if (sidField != null) {
            m_oraSchoolIdField = sidField.getJavaName();
        } else {
            addSetupError("School ID alias", "No alias found for " + ORA_SCHOOL_ID_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }

        // Find the conduct incident fields in ORA from its DDX alias
        m_oraArsonCountField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_ARSON_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraArsonCountField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_ARSON_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraBatteryField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_BATTERY_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraBatteryField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_BATTERY_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraDrugsField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_DRUGS_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraDrugsField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_DRUGS_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraFelonyDrugsField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_FELONY_DRUGS_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraFelonyDrugsField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_FELONY_DRUGS_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraKidnapField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_KIDNAP_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraKidnapField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_KIDNAP_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraManslaughterField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_MANSLAUGHTER_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraManslaughterField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_MANSLAUGHTER_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraMolestationField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_MOLESTATION_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraMolestationField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_MOLESTATION_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraMurderField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_MURDER_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraMurderField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_MURDER_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraRapeField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_RAPE_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraRapeField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_RAPE_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraRobberyField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_ROBBERY_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraRobberyField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_ROBBERY_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraSexBatteryField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_SEX_BATTERY_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraSexBatteryField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_SEX_BATTERY_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraSodomyField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_SODOMY_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraSodomyField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_SODOMY_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraThreatsField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_THREATS_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraThreatsField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_THREATS_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_oraWeaponsField =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_INCIDENT_WEAPONS_ALIAS).getJavaName();
        if (StringUtils.isEmpty(m_oraWeaponsField)) {
            addSetupError("Conduct Incident alias", "No alias found for " + ORA_INCIDENT_WEAPONS_ALIAS +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_dualEnroll8Alias =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_DUAL_ENROLL_8).getJavaName();
        if (StringUtils.isEmpty(m_dualEnroll8Alias)) {
            addSetupError("Dual enroll 8 alias", "No alias found for " + ORA_DUAL_ENROLL_8 +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_dualEnroll9Alias =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_DUAL_ENROLL_9).getJavaName();
        if (StringUtils.isEmpty(m_dualEnroll9Alias)) {
            addSetupError("Dual enroll 9 alias", "No alias found for " + ORA_DUAL_ENROLL_9 +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_dualEnroll10Alias =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_DUAL_ENROLL_10).getJavaName();
        if (StringUtils.isEmpty(m_dualEnroll10Alias)) {
            addSetupError("Dual enroll 10 alias", "No alias found for " + ORA_DUAL_ENROLL_10 +
                    " in extended data dictionary " + ORA_DDX_ID);
        }
        m_dualEnroll11Alias =
                m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_DUAL_ENROLL_11).getJavaName();
        if (StringUtils.isEmpty(m_dualEnroll11Alias)) {
            addSetupError("ual enroll 11 alias", "No alias found for " + ORA_DUAL_ENROLL_11 +
                    " in extended data dictionary " + ORA_DDX_ID);
        }

        // Find the reference table for Title1 school.
        sidField = m_oraDataDictionary.findDataDictionaryFieldByAlias(ORA_TITLE1_SCHOOL_ALIAS);
        m_title1ReferenceMap = new HashMap<String, String>();
        if (sidField != null && sidField.getReferenceTable() != null) {
            for (ReferenceCode code : sidField.getReferenceTable().getReferenceCodes()) {
                if (!StringUtils.isEmpty(code.getStateCode())) {
                    m_title1ReferenceMap.put(code.getCode(), code.getStateCode());
                }
            }
        }


        // Grab all the conduct types
        DataDictionaryField conductTypeField =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                        ConductIncident.class.getName(),
                        ConductIncident.COL_INCIDENT_CODE);
        String conductRefTableOid = conductTypeField.getReferenceTableOid();
        if (StringUtils.isEmpty(conductRefTableOid)) {
            conductRefTableOid = "rtbCndIncident";
        }
        Criteria conductIncidentTypesCriteria = new Criteria();
        conductIncidentTypesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, conductRefTableOid);
        QueryByCriteria citQuery = new QueryByCriteria(ReferenceCode.class, conductIncidentTypesCriteria);
        m_conductMap = getBroker().getMapByQuery(citQuery, ReferenceCode.COL_CODE, 64);

        // Grab all the conducts within the school year
        PlainDate startDate = getCurrentContext().getStartDate();
        PlainDate endDate = getCurrentContext().getEndDate();
        Criteria conductCriteria = new Criteria();
        conductCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, startDate);
        conductCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, endDate);
        QueryByCriteria conductQuery = new QueryByCriteria(ConductIncident.class, conductCriteria);
        m_conductsMap = getBroker().getGroupedCollectionByQuery(conductQuery,
                ConductIncident.COL_SCHOOL_OID,
                16);

        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("GA-SR-ALIAS", new RetrieveAttributeAliasNumber());
        calcs.put("GA-SR-ONLINE-SKL", new RetrieveOnlineSchool());
        calcs.put("GA-SR-CPR-AED", new RetrieveCprAed());
        calcs.put("GA-SR-SCHOOL", new RetrieveSchoolDualEnroll());
        super.addCalcs(calcs);
    }

    /**
     * Load map with grade state codes keyed by numeric grade value.
     */
    private void initGrades() {
        m_numericStateGradeMap = new HashMap<String, String>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_GRADE_NUMERIC);
        ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
        DataDictionaryField gradeLevelField = dictionary.findDataDictionaryField(Student.class.getName(),
                Student.COL_GRADE_LEVEL);

        DataDictionaryField numericGradeLevelField = dictionary.findDataDictionaryFieldByAlias(ALIAS_NUMERIC_GRADE);
        if (gradeLevelField.hasReferenceTable()) {
            for (ReferenceCode code : gradeLevelField.getReferenceTable().getReferenceCodes(getBroker())) {
                String numericLevel = (String) code.getFieldValueByBeanPath(numericGradeLevelField.getJavaName());

                if (!StringUtils.isEmpty(numericLevel)) {
                    m_numericStateGradeMap.put(numericLevel, code.getStateCode());
                }
            }
        }
    }

    /**
     * Load map with yes no codes
     */
    private void initYesNoBlank() {
        m_yesNoBlankMap = new HashMap<String, String>();

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField yesNoBlankField = dictionary.findDataDictionaryFieldByAlias(ALIAS_DOE_CPR_AED);

        if (yesNoBlankField.hasReferenceTable()) {
            for (ReferenceCode code : yesNoBlankField.getReferenceTable().getReferenceCodes(getBroker())) {
                m_yesNoBlankMap.put(code.getCode(), code.getStateCode());
            }
        }
    }

    /**
     * Create criteria to select schools to report.
     *
     * @return Criteria
     */
    private Criteria getSchoolCriteria() {
        X2Criteria schoolCriteria = new X2Criteria();
        if (isSchoolContext()) {
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            schoolCriteria.addNotEqualTo(School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        }
        return schoolCriteria;
    }
}
