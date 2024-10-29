/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Follett Software Company
 * @copyright 2019
 */
public class EyeTemplateProcedure implements DynamicFormProcedure {
    static final String ALIAS_GFC_AGE_OF_CHILD = "eye-gfc-age-of-child";
    static final String ALIAS_GFC_ADDRESS = "eye-gfc-intersection-street";
    static final String ALIAS_GFC_CITY = "eye-gfc-city-community";
    static final String ALIAS_GFC_LICENSE = "eye-gfc-centre-agency-license";
    static final String ALIAS_GFC_NAME = "eye-gfc-centre-agency-name";
    static final String ALIAS_GFC_NOT_APPLICABLE = "eye-gfc-not-applicable";
    static final String ALIAS_GFD_LIC_CARE_CENTRE = "eye-gfd-lic-care-centre-";
    static final String ALIAS_GFD_LIC_HOME_BASED = "eye-gfd-lic-home-based-";
    static final String ALIAS_GFD_NO_CARE_ANSWER = "eye-gfd-no-answer-";
    static final String ALIAS_GFD_PAID_CARE = "eye-gfd-lic-home-based-";
    static final String ALIAS_GFD_PARENT_CARE = "eye-gfd-parent-care-";
    static final String ALIAS_GFD_UNPAID_CARE = "eye-gfd-unpaid-care-";

    static final List<String> CARE_TYPES = Arrays.asList(ALIAS_GFD_LIC_CARE_CENTRE, ALIAS_GFD_LIC_HOME_BASED,
            ALIAS_GFD_PAID_CARE, ALIAS_GFD_PARENT_CARE, ALIAS_GFD_UNPAID_CARE);

    static final String EYE_DDX_ID = "STD-GFD-EYE-SURVEY";

    static final String PROPERTY_VALUE_TRUE = "true";

    /**
     * The Enum AgeOfChild contains a record for each age group in OnSis reporting
     */
    public enum AgeOfChild {
        ONSIS_010("010", 1), ONSIS_020("020", 2), ONSIS_030("030", 3), ONSIS_040("040", 4), ONSIS_050("050", 5);

        private int index;
        private String code;

        /**
         * Instantiates a new age of child.
         *
         * @param code String
         * @param index int
         */
        private AgeOfChild(String code, int index) {
            this.index = index;
            this.code = code;
        }

        /**
         * Gets the code.
         *
         * @return String
         */
        public String getCode() {
            return code;
        }

        /**
         * Gets the index.
         *
         * @return int
         */
        public int getIndex() {
            return index;
        }
    }

    /**
     * The Class ChildCareCentre stores the information from the embedded list for processing
     */
    public class ChildCareCentre {
        private String address;
        private String ageCode;
        private String city;
        private String licenseNumber;
        private String name;

        /**
         * Instantiates a new child care centre.
         *
         * @param address String
         * @param ageCode String
         * @param city String
         * @param licenseNumber String
         * @param name String
         */
        public ChildCareCentre(String address, String ageCode, String city, String licenseNumber, String name) {
            super();
            this.address = address;
            this.ageCode = ageCode;
            this.city = city;
            this.licenseNumber = licenseNumber;
            this.name = name;
        }

        /**
         * Checks if is valid.
         *
         * @return true, if is valid
         */
        public boolean isValid() {
            return !StringUtils.isEmpty(licenseNumber) || !StringUtils.isEmpty(name);
        }
    }

    private X2Broker m_broker = null;
    private Map<String, Map<String, String>> m_codeToStateMap = new HashMap();
    private Map<String, Map<String, String>> m_stateToCodeMap = new HashMap();

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(com.follett.fsc.core.k12.web.template.Template,
     *      com.follett.fsc.core.k12.web.ApplicationContext,
     *      com.follett.fsc.core.k12.business.dictionary.DataDictionary,
     *      com.follett.fsc.core.k12.business.PrivilegeSet, java.util.Locale)
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {
        m_broker = new ModelBroker(privilegeSet);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(com.follett.fsc.core.k12.web.GenericDetail,
     *      java.lang.String, java.lang.String, com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.web.template.Template, java.util.List)
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errors)
            throws X2BaseException {
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(com.follett.fsc.core.k12.web.GenericDetailForm,
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        List<ValidationError> errors = new LinkedList<ValidationError>();
        DataDictionary dictionary = detail.getDataDictionary();
        ExtendedDictionaryAttributes extendedDictionary = dictionary.getExtendedDictionary();
        if (extendedDictionary != null && EYE_DDX_ID.equals(extendedDictionary.getId())) {
            Map<String, List<ChildCareCentre>> centres = loadCentres(detail.getChildDetailSets());
            errors.addAll(validateCentres(detail, centres));
            errors.addAll(validateChildCare(detail));
        }
        return errors;
    }

    /**
     * Adds the reference table to the code translation maps
     *
     * @param referenceTableOid String
     */
    private void addReferenceTable(String referenceTableOid) {
        Map<String, String> codeToStateMap = new HashMap();
        m_codeToStateMap.put(referenceTableOid, codeToStateMap);
        Map<String, String> stateToCodeMap = new HashMap();
        m_stateToCodeMap.put(referenceTableOid, stateToCodeMap);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);

        String[] columns = new String[] {ReferenceCode.COL_CODE, ReferenceCode.COL_STATE_CODE};
        ColumnQuery query = new ColumnQuery(ReferenceCode.class, columns, criteria);
        try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String code = (String) row[0];
                String state = (String) row[1];
                codeToStateMap.put(code, state);
                stateToCodeMap.put(state, code);
            }
        }
    }

    /**
     * Gets the broker.
     *
     * @return X2Broker
     */
    private X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Gets the code value for a state code
     *
     * @param referenceTableOid String
     * @param value String
     * @return String
     */
    private String getCodeFromState(String referenceTableOid, String value) {
        Map<String, String> codes = m_stateToCodeMap.get(referenceTableOid);
        if (codes == null) {
            addReferenceTable(referenceTableOid);
            codes = m_stateToCodeMap.get(referenceTableOid);
        }
        return codes.get(value);
    }

    /**
     * Gets the field value from a detail
     *
     * @param detail GenericDetail
     * @param alias String
     * @return String
     */
    private String getFieldValue(GenericDetail detail, String alias) {
        String value = null;
        DataDictionaryField field = detail.getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            value = (String) detail.getValueByBeanPath(field.getJavaName());
            if (field.hasReferenceTable()) {
                value = getStateFromCode(field.getReferenceTableOid(), value);
            }
        }
        return value;
    }

    /**
     * Gets the state code from a code value.
     *
     * @param referenceTableOid String
     * @param value String
     * @return String
     */
    private String getStateFromCode(String referenceTableOid, String value) {
        Map<String, String> codes = m_codeToStateMap.get(referenceTableOid);
        if (codes == null) {
            addReferenceTable(referenceTableOid);
            codes = m_codeToStateMap.get(referenceTableOid);
        }
        return codes.get(value);
    }

    /**
     * Load centres for the parent detail
     *
     * @param childDetailSets Collection<ChildDetailSet>
     * @return Map
     */
    private Map<String, List<ChildCareCentre>> loadCentres(Collection<ChildDetailSet> childDetailSets) {
        Map<String, List<ChildCareCentre>> map = new HashMap();
        if (childDetailSets != null) {
            for (ChildDetailSet childDetailSet : childDetailSets) {
                if (GenericFormChildData.class.equals(childDetailSet.getChildClass())) {
                    Collection<GenericDetail> childrenDetails = childDetailSet.getChildDetails();
                    if (childrenDetails != null) {
                        for (GenericDetail childDetail : childrenDetails) {
                            String address = getFieldValue(childDetail, ALIAS_GFC_ADDRESS);
                            String ageCode = getFieldValue(childDetail, ALIAS_GFC_AGE_OF_CHILD);
                            String city = getFieldValue(childDetail, ALIAS_GFC_CITY);
                            String licenseNumber = getFieldValue(childDetail, ALIAS_GFC_LICENSE);
                            String name = getFieldValue(childDetail, ALIAS_GFC_NAME);
                            List<ChildCareCentre> centres = map.get(ageCode);
                            if (centres == null) {
                                centres = new LinkedList();
                                map.put(ageCode, centres);
                            }
                            centres.add(new ChildCareCentre(address, ageCode, city, licenseNumber, name));
                        }
                    }
                }
            }
        }
        return map;
    }

    /**
     * Lookup code from state code value.
     *
     * @param detail GenericDetail
     * @param alias String
     * @param value String
     * @return String
     */
    private String lookupCodeFromStateCodeValue(GenericDetail detail, String alias, String value) {
        DataDictionaryField field = detail.getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            if (field.hasReferenceTable()) {
                value = getCodeFromState(field.getReferenceTableOid(), value);
            }
        }
        return value;
    }

    /**
     * Validate centres.
     *
     * @param detail GenericDetail
     * @param centres Map<String,List<ChildCareCentre>>
     * @return the collection<? extends validation error>
     */
    private Collection<? extends ValidationError> validateCentres(GenericDetail detail,
                                                                  Map<String, List<ChildCareCentre>> centres) {
        List<ValidationError> errors = new LinkedList<ValidationError>();
        for (Entry<String, List<ChildCareCentre>> entry : centres.entrySet()) {
            String ageCode = entry.getKey();
            if (entry.getValue().size() > 1) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "Only one child care centre can be defined for each age range. There are "
                                + entry.getValue().size() + " entries for age "
                                + lookupCodeFromStateCodeValue(detail, ALIAS_GFC_AGE_OF_CHILD, ageCode) + "."));
            } else {
                for (ChildCareCentre centre : entry.getValue()) {
                    if (!centre.isValid()) {
                        errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                                Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                                "The child care centre for age range "
                                        + lookupCodeFromStateCodeValue(detail, ALIAS_GFC_AGE_OF_CHILD, ageCode)
                                        + " must contain a values for name."));
                    }
                }
            }
        }
        for (AgeOfChild age : AgeOfChild.values()) {
            String homeBased = getFieldValue(detail, ALIAS_GFD_LIC_HOME_BASED + age.getIndex());
            String careCentre = getFieldValue(detail, ALIAS_GFD_LIC_CARE_CENTRE + age.getIndex());
            List<ChildCareCentre> centreList = centres.get(age.getCode());
            if (StringUtils.isEmpty(homeBased) && StringUtils.isEmpty(careCentre) && centreList != null
                    && !centreList.isEmpty()) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "A child care centre is defined for an age range "
                                + lookupCodeFromStateCodeValue(detail, ALIAS_GFC_AGE_OF_CHILD, age.getCode())
                                + " but full-time or part-time is not selected."));
            }
            if (!StringUtils.isEmpty(homeBased) && !StringUtils.isEmpty(careCentre)) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "Only one of licensed home based care and licensed care center can be selected for age range "
                                + lookupCodeFromStateCodeValue(detail, ALIAS_GFC_AGE_OF_CHILD, age.getCode())
                                + "."));
            }
            if ((!StringUtils.isEmpty(homeBased) || !StringUtils.isEmpty(careCentre))
                    && (centreList == null || centreList.isEmpty())) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "Licensed home based care or licensed care center is selected for age range "
                                + lookupCodeFromStateCodeValue(detail, ALIAS_GFC_AGE_OF_CHILD, age.getCode())
                                + " but no child care center is provided."));
            }
        }
        return errors;
    }

    /**
     * Validate child care centers
     *
     * @param detail GenericDetail
     * @return the collection<? extends validation error>
     */
    private Collection<? extends ValidationError> validateChildCare(GenericDetail detail) {
        List<ValidationError> errors = new LinkedList<ValidationError>();
        for (AgeOfChild age : AgeOfChild.values()) {
            boolean careTypeEntered = false;
            for (String alias : CARE_TYPES) {
                String value = getFieldValue(detail, alias + age.getIndex());
                if (!StringUtils.isEmpty(value)) {
                    careTypeEntered = true;
                    break;
                }
            }
            String value = getFieldValue(detail, ALIAS_GFD_NO_CARE_ANSWER + age.getIndex());
            boolean noAnswerSelected = PROPERTY_VALUE_TRUE.equals(value);
            if (careTypeEntered && noAnswerSelected) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "If care type is entered for age range "
                                + lookupCodeFromStateCodeValue(detail, ALIAS_GFC_AGE_OF_CHILD, age.getCode())
                                + " prefer not to answer cannot be checked."));
            } else if (!careTypeEntered && !noAnswerSelected) {
                errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                        Integer.valueOf(BusinessRules.REQUIRE_FIELD),
                        "If no care type is entered for age range "
                                + lookupCodeFromStateCodeValue(detail, ALIAS_GFC_AGE_OF_CHILD, age.getCode())
                                + " prefer not to answer must be checked."));
            }

        }
        return errors;
    }


}
