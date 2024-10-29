/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolAttribute;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Dataset for School data to be used in final CRDC export.
 *
 * @author X2 Development Corporation
 */
public class CRDCSchoolData extends CRDCReportData {
    /**
     * Implementation of StateReportEntity to be used for School CRDC Data export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SchoolCRDCEntity extends StateReportEntity {

        /**
         * Instantiates a new school CRDC entity.
         */
        public SchoolCRDCEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        SisSchool m_school;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_school.getName();

            return name;
        }

        /**
         * Initialize the entity for the student bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_school = (SisSchool) bean;
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

    }

    /**
     * Retrieves count of bullying related incidents based on field parameter.
     */
    public class FLRetrBullCndCnt implements FieldRetriever {
        private static final String ALIAS_BULLYING = "all-cnd-BullyingRelated";

        private static final String ALIAS_HARRASS_DISB = "all-cnd-HarassmentBasedOnDisability";
        private static final String ALIAS_HARRASS_RACE = "all-cnd-HarassmentBasedOnRace";
        private static final String ALIAS_HARRASS_RELIGION = "all-cnd-HarassmentBasedOnReligion";
        private static final String ALIAS_HARRASS_SEX = "all-cnd-HarassmentBasedOnSex";
        private static final String ALIAS_HARRASS_SEX_ORIENTATION = "all-cnd-HarassmentBasedOnSexualOrientation";

        private Map<String, Collection<ConductIncident>> m_schoolIncidents = null;

        private String m_disbJavaName = null;
        private String m_raceJavaName = null;
        private String m_religJavaName = null;
        private String m_sexJavaName = null;
        private String m_sexOrientJavaName = null;

        /**
         * Instantiates a new FLRetrBullCndCnt.
         */
        public FLRetrBullCndCnt() {
            m_disbJavaName = translateAliasToJavaName(ALIAS_HARRASS_DISB, true);
            m_raceJavaName = translateAliasToJavaName(ALIAS_HARRASS_RACE, true);
            m_religJavaName = translateAliasToJavaName(ALIAS_HARRASS_RELIGION, true);
            m_sexJavaName = translateAliasToJavaName(ALIAS_HARRASS_SEX, true);
            m_sexOrientJavaName = translateAliasToJavaName(ALIAS_HARRASS_SEX_ORIENTATION, true);

            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
            String bullyingJavaName = translateAliasToJavaName(ALIAS_BULLYING, true);
            criteria.addEqualTo(bullyingJavaName, BooleanAsStringConverter.TRUE);

            X2Criteria andCriteria = new X2Criteria();
            andCriteria.addEqualTo(m_disbJavaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrEqualTo(m_raceJavaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrEqualTo(m_religJavaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrEqualTo(m_sexJavaName, BooleanAsStringConverter.TRUE);
            andCriteria.addOrEqualTo(m_sexOrientJavaName, BooleanAsStringConverter.TRUE);

            criteria.addAndCriteria(andCriteria);

            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

            m_schoolIncidents = getBroker().getGroupedCollectionByQuery(query, ConductIncident.COL_SCHOOL_OID, 1000);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            Collection<ConductIncident> schoolIncidents = m_schoolIncidents.get(entity.getBean().getOid());

            String parameter = (String) field.getParameter();

            String filterJavaName = null;

            switch (parameter) {
                case ALIAS_HARRASS_DISB:
                    filterJavaName = m_disbJavaName;
                    break;
                case ALIAS_HARRASS_RACE:
                    filterJavaName = m_raceJavaName;
                    break;
                case ALIAS_HARRASS_RELIGION:
                    filterJavaName = m_religJavaName;
                    break;
                case ALIAS_HARRASS_SEX:
                    filterJavaName = m_sexJavaName;
                    break;
                case ALIAS_HARRASS_SEX_ORIENTATION:
                    filterJavaName = m_sexOrientJavaName;
                    break;
                default:
                    break;
            }

            int count = 0;
            if (schoolIncidents != null) {
                for (ConductIncident inc : schoolIncidents) {
                    count += BooleanAsStringConverter.TRUE.equals(inc.getFieldValueByBeanPath(filterJavaName)) ? 1 : 0;
                }
            }

            return Integer.valueOf(count);
        }
    }

    /**
     * FL Retriever to determine if there were firearms incidents.
     */
    public class FLRetrFirearmsYN implements FieldRetriever {
        private static final String ALIAS_WEAPON_RELATED = "all-cnd-WeaponRelated";

        private Collection<String> m_firearmsStateCodes = Arrays.asList("3", "4");
        private Collection<String> m_firearmsCodes = null;

        private Map<String, Collection<ConductIncident>> m_schoolIncidents = null;

        /**
         * Instantiates a new FLRetrFirearmsYN.
         */
        public FLRetrFirearmsYN() {

            DataDictionaryField fieldWeaponRelated =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_WEAPON_RELATED);

            X2Criteria codesCriteria = new X2Criteria();
            codesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, fieldWeaponRelated.getReferenceTableOid());
            codesCriteria.addIn(ReferenceCode.COL_STATE_CODE, m_firearmsStateCodes);

            QueryByCriteria codesQuery = new QueryByCriteria(ReferenceCode.class, codesCriteria);

            m_firearmsCodes = new ArrayList<>();

            QueryIterator codesIterator = CRDCSchoolData.this.getBroker().getIteratorByQuery(codesQuery);
            try {
                while (codesIterator.hasNext()) {
                    ReferenceCode record = (ReferenceCode) codesIterator.next();
                    String code = record.getCode();
                    m_firearmsCodes.add(code);
                }
            } finally {
                codesIterator.close();
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
            String weaponRelatedJavaName = translateAliasToJavaName(ALIAS_WEAPON_RELATED, true);
            criteria.addIn(weaponRelatedJavaName, m_firearmsCodes);

            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

            m_schoolIncidents = getBroker().getGroupedCollectionByQuery(query, ConductIncident.COL_SCHOOL_OID, 1000);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            Collection<ConductIncident> schoolIncidents = m_schoolIncidents.get(entity.getBean().getOid());

            boolean value = false;
            if (schoolIncidents != null && schoolIncidents.size() > 0) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * FL Retriever to determine if there were homicide incidents.
     */
    public class FLRetrHomicideYN implements FieldRetriever {
        private static final String ALIAS_HOMICIDE_VICTIMS = "all-cnd-HomicideVictims";

        private Collection<String> m_homicideStateCodes = Arrays.asList("E", "F", "O", "S");
        private Collection<String> m_homicideCodes = null;

        private Map<String, Collection<ConductIncident>> m_schoolIncidents = null;

        /**
         * Instantiates a new FLRetrHomicideYN.
         */
        public FLRetrHomicideYN() {

            DataDictionaryField fieldHomicideVictims =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_HOMICIDE_VICTIMS);

            X2Criteria codesCriteria = new X2Criteria();
            codesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID,
                    fieldHomicideVictims.getReferenceTableOid());
            codesCriteria.addIn(ReferenceCode.COL_STATE_CODE, m_homicideStateCodes);

            QueryByCriteria codesQuery = new QueryByCriteria(ReferenceCode.class, codesCriteria);

            m_homicideCodes = new ArrayList<>();

            QueryIterator codesIterator = CRDCSchoolData.this.getBroker().getIteratorByQuery(codesQuery);
            try {
                while (codesIterator.hasNext()) {
                    ReferenceCode record = (ReferenceCode) codesIterator.next();
                    String code = record.getCode();
                    m_homicideCodes.add(code);
                }
            } finally {
                codesIterator.close();
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
            String weaponRelatedJavaName = translateAliasToJavaName(ALIAS_HOMICIDE_VICTIMS, true);

            X2Criteria andContainsCriteria = new X2Criteria();
            andContainsCriteria.addContains(weaponRelatedJavaName, "*__dummy__*");
            for (String homicideCode : m_homicideCodes) {
                X2Criteria orContainsCodeCriteria = new X2Criteria();
                orContainsCodeCriteria.addContains(weaponRelatedJavaName, homicideCode);
                andContainsCriteria.addOrCriteria(orContainsCodeCriteria);
            }
            criteria.addAndCriteria(andContainsCriteria);

            QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);

            m_schoolIncidents = getBroker().getGroupedCollectionByQuery(query, ConductIncident.COL_SCHOOL_OID, 1000);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            Collection<ConductIncident> schoolIncidents = m_schoolIncidents.get(entity.getBean().getOid());

            boolean value = false;
            if (schoolIncidents != null && schoolIncidents.size() > 0) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * Returns number of incidents based on CRDC code from calculation parameter.
     *
     * @author Follett Software Company
     */
    public class RetrieverIncidentCnt implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCSchoolData crdcSklData = (CRDCSchoolData) data;

            String sklOid = entity.getBean().getOid();
            String parameterCrdcCode = (String) field.getParameter();

            int value = 0;

            if (!StringUtils.isEmpty(parameterCrdcCode)) {
                Set<ConductIncident> incidents =
                        crdcSklData.m_conductHelper.getIncidentsForSchool(sklOid, parameterCrdcCode);
                if (incidents != null) {
                    Set<String> ids = new HashSet();
                    int count = 0;
                    for (ConductIncident incident : incidents) {
                        String id = incident.getIncidentId();
                        if (StringUtils.isEmpty(id)) {
                            count++;
                        } else if (!ids.contains(id)) {
                            ids.add(id);
                            ++count;
                        }
                    }
                    value = count;
                }
            }

            return Integer.valueOf(value);
        }
    }

    /**
     * Returns true if there were incidents with CRDC codes from calculation parameters, otherwise
     * false.
     *
     * @author Follett Software Company
     */
    public class RetrieverIncidentYN implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCSchoolData crdcSklData = (CRDCSchoolData) data;
            CRDCConductHelper conductHelper = crdcSklData.m_conductHelper;
            String sklOid = entity.getBean().getOid();
            boolean value = false;

            String calcParameter = (String) field.getParameter();

            String[] crdcCodes = calcParameter.split(",");
            for (String crdcCode : crdcCodes) {
                Set<ConductIncident> incidents = null;
                if (crdcCode.startsWith("%") && crdcCode.endsWith("%")) {
                    crdcCode = crdcCode.substring(1, crdcCode.length() - 2);
                    incidents = conductHelper.getIncidentsForSchoolCodeContains(sklOid, crdcCode);
                } else {
                    incidents = conductHelper.getIncidentsForSchool(sklOid, crdcCode);
                }
                if (incidents != null && !incidents.isEmpty()) {
                    value = true;
                    break;
                }
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * Returns NCES School Id (7 or 12 digit codes).
     *
     * @author Follett Software Company
     */
    public class RetrieverNcesSchId implements FieldRetriever {
        private static final String PARAM_7_DIGITS_CODE = "PARAM_7_DIGITS_CODE";
        private static final String PARAM_12_DIGITS_CODE = "PARAM_12_DIGITS_CODE";


        /**
         * Instantiates a new retriever nces sch id.
         */
        public RetrieverNcesSchId() {
            super();
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            SisSchool school = (SisSchool) entity.getBean();

            String ncesSchoolId = (String) school.getFieldValueByBeanPath(m_fieldNCESSchoolId);

            if (!StringUtils.isEmpty(ncesSchoolId)) {
                if (PARAM_7_DIGITS_CODE.equals(field.getParameter())) {
                    if (ncesSchoolId.length() >= 7) {
                        value = ncesSchoolId.substring(0, 7);
                    }
                } else if (PARAM_12_DIGITS_CODE.equals(field.getParameter())) {
                    value = ncesSchoolId;
                }
            }

            return value;
        }
    }

    /**
     * Returns school name.
     *
     * @author Follett Software Company
     */
    public class RetrieverSchoolName implements FieldRetriever {
        private Map<String, SchoolAttribute> m_schoolAttributesMap;

        /**
         * Instantiates a new retriever school name.
         */
        public RetrieverSchoolName() {
            super();
            initializeSchoolAttributes();
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            SisSchool school = (SisSchool) entity.getBean();

            SchoolAttribute attribute = m_schoolAttributesMap.get(school.getOid());
            if (attribute != null) {
                value = attribute.getName();
            }

            if (StringUtils.isEmpty(value)) {
                value = school.getName();
            }

            return value;
        }

        /**
         * Initialize school attributes.
         */
        private void initializeSchoolAttributes() {
            Criteria schoolCriteria = CRDCSchoolData.this.getQuery().getCriteria();
            X2Criteria attrCriteria = new X2Criteria();
            SubQuery subQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, schoolCriteria);
            attrCriteria.addIn(SchoolAttribute.COL_SCHOOL_OID, subQuery);
            attrCriteria.addEqualTo(SchoolAttribute.COL_CONTEXT_OID, m_context.getOid());

            QueryByCriteria attrQuery = new QueryByCriteria(SchoolAttribute.class, attrCriteria);

            m_schoolAttributesMap = getBroker().getMapByQuery(attrQuery, SchoolAttribute.COL_SCHOOL_OID, 1);
        }
    }

    /**
     * VA Retriever to determine count of incidents based on one of the categories (SEX, RACE,
     * DISABILITY).
     *
     * @author Follett Software Company
     */
    public class VARetrIncidentCnt implements FieldRetriever {
        public static final String ALIAS_CND_ON_DISABILITY = "BASED ON DISABILITY";
        public static final String ALIAS_CND_ON_RACE = "BASED ON RACE";
        public static final String ALIAS_CND_ON_SEX = "BASED ON SEX";

        public static final String CRDC_CODE_HARRAS = "Harrassment";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCSchoolData crdcSklData = (CRDCSchoolData) data;
            String sklOid = entity.getBean().getOid();

            int count = 0;
            String category = (String) field.getParameter();

            Set<ConductIncident> incidents =
                    crdcSklData.m_conductHelper.getIncidentsForSchool(sklOid, CRDC_CODE_HARRAS);
            if (incidents != null) {
                if (ALIAS_CND_ON_SEX.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_SEX))) {
                            count += 1;
                        }
                    }
                } else if (ALIAS_CND_ON_RACE.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_RACE))) {
                            count += 1;
                        }
                    }
                } else if (ALIAS_CND_ON_DISABILITY.equals(category)) {
                    for (ConductIncident cnd : incidents) {
                        if (BooleanAsStringConverter.TRUE.equals(cnd.getFieldValueByAlias(ALIAS_CND_ON_DISABILITY))) {
                            count += 1;
                        }
                    }
                }
            }
            return Integer.valueOf(count);
        }
    }

    private static final String ALIAS_NCES_SCHOOL_ID = "all-skl-NCESSchoolID";

    protected CRDCConductHelper m_conductHelper;
    protected DistrictSchoolYearContext m_context;
    protected String m_fieldNCESSchoolId;

    /**
     * @see com.x2dev.procedures.statereporting.CRDCReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        PlainDate reportDate = (PlainDate) getParameter(PARAM_PART_1_DATE);
        m_context = getDistrictSchoolYearContextByDate(reportDate);
        m_fieldNCESSchoolId = translateAliasToJavaName(ALIAS_NCES_SCHOOL_ID, true);

        if (m_context == null) {
            addSetupError("Cannot use " + reportDate + " as report date.",
                    "Connot determine District School Year Context by date " + reportDate);
        }

        if (m_fieldNCESSchoolId == null) {
            addSetupError("NPE for the field look up", "Cannot find field with alias = " + ALIAS_NCES_SCHOOL_ID);
        }

        if (getSetupErrors().size() == 0) {
            X2Criteria schoolCriteria = new X2Criteria();
            if (isSchoolContext()) {
                schoolCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
            }
            schoolCriteria.addNotEmpty(m_fieldNCESSchoolId, getBroker().getPersistenceKey());
            schoolCriteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);

            setQuery(schoolQuery);
            setEntityClass(SchoolCRDCEntity.class);

            CRDCDataHelper crdcHelper = new CRDCDataHelper(this);
            addCalcs(crdcHelper.getUsedRetrievers());
            addValidators(crdcHelper.getUsedValidators());

            m_conductHelper = new CRDCConductHelper(getBroker(), m_context);
        }
    }

    /**
     * Gets the district school year context by date.
     *
     * @param date PlainDate
     * @return District school year context
     */
    private DistrictSchoolYearContext getDistrictSchoolYearContextByDate(PlainDate date) {
        X2Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, date);
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, date);
        QueryByCriteria byCriteria = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
        return (DistrictSchoolYearContext) getBroker().getBeanByQuery(byCriteria);
    }

}
