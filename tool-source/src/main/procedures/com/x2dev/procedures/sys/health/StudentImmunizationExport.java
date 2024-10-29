/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.health;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.model.business.health.ImmunizationRuleEngine;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This export is used to report the compliance, non-compliance, conditional, waived and other
 * exempt reasons for all
 * health immunization definitions per student.
 *
 * @author Follett Software Company
 */

public class StudentImmunizationExport extends StateReportData {
    /**
     * Entity class for Student Health Immunization.
     *
     * @author Follett Software Company
     */

    public static class ImmunizationEntity extends StateReportEntity {
        private Map<String, String> m_immunizationStatus;
        private Map<String, HealthImmunizationDefinition> m_immunizationCondCompVal;
        private StudentImmunizationExport m_reportData;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ImmunizationEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisStudent student = (SisStudent) bean;
            m_reportData = (StudentImmunizationExport) data;
            m_immunizationStatus = new HashMap<String, String>(m_reportData.m_definitions.size());
            m_immunizationCondCompVal =
                    new HashMap<String, HealthImmunizationDefinition>(m_reportData.m_definitions.size());

            for (HealthImmunizationDefinition definition : m_reportData.m_definitions) {
                List<HealthImmunizationDose> doses = null;
                String exemptReason = null;
                boolean isWaived = false;
                boolean isCompliant = false;
                boolean isConditional = false;
                String status = null;

                HealthImmunizationSeries series = m_reportData.getSeries(definition, student);
                if (series != null) {
                    // Check for isWaived indicator on the series and then override it with the
                    // dose' isWaived indicator
                    isWaived = series.getWaivedIndicator();
                    if (m_reportData.m_fieldImmunizationSeriesExempt != null) {
                        exemptReason =
                                (String) series.getFieldValueByBeanPath(m_reportData.m_fieldImmunizationSeriesExempt);
                    }
                    doses = m_reportData.m_seriesImmunizationDoseMap.get(series.getOid());

                    if (doses != null) {
                        for (HealthImmunizationDose dose : doses) {
                            isWaived = isWaived || dose.getWaivedIndicator();
                            if (dose.getWaivedIndicator() && StringUtils.isEmpty(exemptReason)
                                    && m_reportData.m_fieldImmunizationDoseExempt != null) {
                                exemptReason = (String) dose
                                        .getFieldValueByBeanPath(m_reportData.m_fieldImmunizationDoseExempt);
                            }
                        }
                        if (!StringUtils.isEmpty(definition.getRuleDefinition())) {
                            ImmunizationRuleEngine engine = new ImmunizationRuleEngine(definition,
                                    m_reportData.m_gradeLevelHistory, m_reportData.getBroker());
                            isCompliant = engine.evaluateCompliance(student, doses);
                        }
                    }
                    isCompliant = isCompliant || (series != null && series.getComplianceOverrideIndicator());
                    isConditional = hasConditionalApproval(series);
                }
                status = getImmunizationStatus(isCompliant, isWaived, isConditional, exemptReason);
                m_immunizationStatus.put(definition.getOid(), status);
                if (isCompliant && isConditional) {
                    m_immunizationCondCompVal.put(definition.getOid(), definition);
                }
            }

        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }

        /**
         * This method gives the immunization status.
         *
         * @param isCompliant boolean
         * @param isWaived boolean
         * @param isConditional boolean
         * @param exemptReason String
         * @return String
         */
        private String getImmunizationStatus(boolean isCompliant,
                                             boolean isWaived,
                                             boolean isConditional,
                                             String exemptReason) {

            String status = null;
            if (isWaived) {
                status = STATUS_WAIVED + WAIVED_REASON_SEPARATOR + exemptReason;
                if (StringUtils.isEmpty(exemptReason)) {
                    status = STATUS_WAIVED;
                }
            } else if (isConditional) {
                status = STATUS_CONDITIONAL;
                if (isCompliant) {
                    status = STATUS_COMPLIANT;
                }
            } else if (isCompliant) {
                status = STATUS_COMPLIANT;
            } else {
                status = STATUS_NON_COMPLIANT;
            }
            return status;

        }

        /**
         * This method checks for the hasApprovedFlag for a Health Immunization Series.
         *
         * @param series HealthImmunizationSeries
         * @return true, if successful
         */
        private boolean hasConditionalApproval(HealthImmunizationSeries series) {
            boolean hasConditionalApproval = false;
            if (m_reportData.m_fieldConditionalApproval != null) {
                String value = (String) series.getFieldValueByBeanPath(m_reportData.m_fieldConditionalApproval);
                if (value != null) {
                    hasConditionalApproval = BooleanAsStringConverter.TRUE.equals(value);
                }
            }
            return hasConditionalApproval;
        }

        /**
         * This method returns the Immunization status.
         *
         * @param definitionCode String
         * @return String
         */
        public String getImmunizationStatus(String definitionCode) {
            return m_immunizationStatus.get(definitionCode);
        }

        /**
         * Gets the immunization cond comp val.
         *
         * @return Map
         */
        public Map<String, HealthImmunizationDefinition> getImmunizationCondCompVal() {
            return m_immunizationCondCompVal;
        }
    }

    /**
     * A field retriever used to lookup and return the immunization status for this entity.
     *
     * @author Administrator
     */
    public class RetrieveImmunizationStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String definitionOid = (String) field.getParameter();
            ImmunizationEntity immunizationEntity = (ImmunizationEntity) entity;
            return immunizationEntity.getImmunizationStatus(definitionOid);
        }
    }

    /**
     * The Class ImmunizationValidator.
     */
    public class ImmunizationValidator implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         *      This method validates the instructional days and instructional minutes.
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errorList = new ArrayList<StateReportValidationError>();
            ImmunizationEntity immunizationEntity = (ImmunizationEntity) entity;
            String definitionOid = (String) field.getParameter();

            Map<String, HealthImmunizationDefinition> immunizationCondCompVal =
                    immunizationEntity.getImmunizationCondCompVal();
            if (immunizationCondCompVal.containsKey(definitionOid)) {
                HealthImmunizationDefinition definition = immunizationCondCompVal.get(definitionOid);
                errorList.add(new StateReportValidationError(entity, field, "Series ID: " + definition.getSeriesId(),
                        "This student is compliant and also has conditional compliance."));
            }
            return errorList;
        }
    }

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return true;
    }

    /**
     * This method returns the HealthImmunizationSeries for a student's Health Immunization
     * Definition.
     *
     * @param definition HealthImmunizationDefinition
     * @param student SisStudent
     * @return Health immunization series
     */
    protected HealthImmunizationSeries getSeries(HealthImmunizationDefinition definition, SisStudent student) {
        Map<String, HealthImmunizationSeries> studentSeries = m_studentImmunizationSeriesMap.get(definition.getOid());
        HealthImmunizationSeries series = null;
        if (studentSeries != null) {
            series = studentSeries.get(student.getOid());
        }
        return series;
    }

    /**
     * Aliases
     */
    private static final String ALIAS_CONDITIONAL_APPROVAL = "CONDITIONAL APPROVAL";
    private static final String ALIAS_IMMUNIZATION_SERIES_EXEMPT = "SERIES EXEMPT REASON";
    private static final String ALIAS_IMMUNIZATION_DOSE_EXEMPT = "DOSE EXEMPT REASON";


    /**
     * Parameters
     */
    private static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Other Constants
     */
    private static final String RETRIEVER_IMMUNIZATION_STATUS = "HIS-CALC";
    private static final String STATUS_COMPLIANT = "COMPLIANT";
    private static final String STATUS_CONDITIONAL = "CONDITIONAL";
    private static final String STATUS_NON_COMPLIANT = "NOT-COMPLIANT";
    private static final String STATUS_WAIVED = "WAIVED";
    private static final String VALIDATOR_IMMUNIZATION = "HIS-CALC-VAL";
    private static final String WAIVED_REASON_SEPARATOR = "-";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    private Map<String, FieldRetriever> m_calcs = new HashMap<String, FieldRetriever>();
    protected Collection<HealthImmunizationDefinition> m_definitions;
    protected String m_fieldConditionalApproval;
    protected String m_fieldImmunizationDoseExempt;
    protected String m_fieldImmunizationSeriesExempt;
    protected GradeLevelHistory m_gradeLevelHistory;
    private StudentHistoryHelper m_helper;
    private PlainDate m_reportDate;
    protected Map<String, List<HealthImmunizationDose>> m_seriesImmunizationDoseMap;
    private Map<String, Map<String, HealthImmunizationSeries>> m_studentImmunizationSeriesMap;
    private Map<String, FieldValidator> m_validators = new HashMap<String, FieldValidator>();

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_fieldImmunizationSeriesExempt = translateAliasToJavaName(ALIAS_IMMUNIZATION_SERIES_EXEMPT, false);
        m_fieldImmunizationDoseExempt = translateAliasToJavaName(ALIAS_IMMUNIZATION_DOSE_EXEMPT, false);
        m_fieldConditionalApproval = translateAliasToJavaName(ALIAS_CONDITIONAL_APPROVAL, false);
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(ImmunizationEntity.class);

            m_gradeLevelHistory = new GradeLevelHistory(m_helper.getStudentCriteria(),
                    20,
                    OrganizationManager.getRootOrganization(getOrganization()),
                    getBroker());
            buildHealthImmunizationCollections(m_helper.getStudentCriteria());

            // Build a map of calculations/retrievers
            m_calcs.put(RETRIEVER_IMMUNIZATION_STATUS, new RetrieveImmunizationStatus());
            addCalcs(m_calcs);
            m_validators.put(VALIDATOR_IMMUNIZATION, new ImmunizationValidator());
            addValidators(m_validators);

            // Set the field definition array.
            List<FieldDefinition> fieldDefinitions = getFieldDefinitions();
            for (HealthImmunizationDefinition definition : m_definitions) {
                fieldDefinitions.add(getImmunizationStatus(definition));
            }
            setFieldDefinitions(fieldDefinitions);
        }
    }

    /**
     * This method builds the Immunization Criteria.
     *
     * @param studentCriteria X2Criteria
     */
    private void buildHealthImmunizationCollections(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria himDefinitions = new X2Criteria();
        himDefinitions.addEqualTo(HealthImmunizationDefinition.COL_REQUIRED_INDICATOR, Boolean.TRUE);
        QueryByCriteria definitionQuery = new QueryByCriteria(HealthImmunizationDefinition.class, himDefinitions);
        definitionQuery.addOrderByAscending(HealthImmunizationDefinition.COL_SERIES_ID);
        m_definitions = getBroker().getCollectionByQuery(definitionQuery);

        // series
        X2Criteria seriesMapCriteria = new X2Criteria();
        seriesMapCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria seriesQuery = new QueryByCriteria(HealthImmunizationSeries.class, seriesMapCriteria);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, true);
        seriesQuery.addOrderBy(HealthImmunizationSeries.COL_STUDENT_OID, true);
        m_studentImmunizationSeriesMap = getBroker().getNestedMapByQuery(seriesQuery,
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID,
                16,
                128);
        // doses
        SubQuery seriesSubQuery = new SubQuery(HealthImmunizationSeries.class, X2BaseBean.COL_OID, seriesMapCriteria);
        X2Criteria doseMapCriteria = new X2Criteria();
        doseMapCriteria.addIn(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, seriesSubQuery);
        QueryByCriteria doseQuery = new QueryByCriteria(HealthImmunizationDose.class, seriesMapCriteria);
        doseQuery.addOrderBy(HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, true);
        doseQuery.addOrderBy(HealthImmunizationDose.COL_DATE, true);
        m_seriesImmunizationDoseMap = getBroker().getGroupedCollectionByQuery(doseQuery,
                HealthImmunizationDose.COL_IMMUNIZATION_SERIES_OID, 500);

    }


    /**
     * Build Field definition for each Health Immunization Definition status.
     *
     * @param definition HealthImmunizationDefinition
     * @return a FieldDefinition
     */
    protected FieldDefinition getImmunizationStatus(HealthImmunizationDefinition definition) {
        String seriesId = definition.getSeriesId();
        FieldDefinition field = new FieldDefinition(seriesId,
                LABEL_PREFIX_CHAR + seriesId,
                null, false, 1, 32, null,
                null, m_calcs.get(RETRIEVER_IMMUNIZATION_STATUS), m_validators.get(VALIDATOR_IMMUNIZATION),
                definition.getOid());
        return field;
    }
}
