/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2015 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.health;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.HealthImmunizationGroup;
import com.x2dev.sis.model.beans.HealthImmunizationGroupOverride;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

/**
 * Procedure that copies data from individual immunization series objects (HealthImmunizationSeries)
 * to grouped series (HealthImmunizationGroupOverride). This utility is necessary when migrating
 * from
 * individual to grouped series.
 *
 * Standard data fields are copied in addition to up to 3 aliases that can be mapped from the
 * individual
 * series to the grouped series record. The standard fields copied are:
 *
 * - ComplianceOverrideIndicator
 * - WaivedIndicator
 * - HistoryOfDiseaseIndicator
 * - FollowupIndicator
 * - Comment
 * - ImmunizationGroupOid
 * - StudentOid
 *
 * Unlike HealthImmunizationSeries objects, HealthImmunizationGroupOverride objects are only
 * required if
 * override information must be attached to the group. Therefore, series objects without at least
 * one of the
 * fields above populated are skipped.
 *
 * An option to update existing objects is provided. If this is not selected, groups with an
 * existing override
 * object for a student are skipped and that override record is untouched. If update existing is
 * selected, the
 * override record is updated with values from the series.
 *
 * @author mmastrangelo
 */
public class ImmunizationOverrideUpdateProcedure extends ProcedureJavaSource {
    private static final String PARAM_UPDATE_EXISTING = "updateExisting";
    private static final String PARAM_SOURCE_HIM = "himOid";
    private static final String PARAM_TARGET_HIG = "higOid";

    private static final String PARAM_HIM_ALIAS_1 = "himAlias1";
    private static final String PARAM_HIM_ALIAS_2 = "himAlias2";
    private static final String PARAM_HIM_ALIAS_3 = "himAlias3";

    private static final String PARAM_HIG_ALIAS_1 = "higAlias1";
    private static final String PARAM_HIG_ALIAS_2 = "higAlias2";
    private static final String PARAM_HIG_ALIAS_3 = "higAlias3";

    private DataDictionary m_dictionary = null;
    private boolean m_updateExisting = false;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_updateExisting = Boolean.TRUE.equals(getParameter(PARAM_UPDATE_EXISTING));

        String sourceHimOid = (String) getParameter(PARAM_SOURCE_HIM);
        String targetHigOid = (String) getParameter(PARAM_TARGET_HIG);

        String himAlias1 = (String) getParameter(PARAM_HIM_ALIAS_1);
        String himAlias2 = (String) getParameter(PARAM_HIM_ALIAS_2);
        String himAlias3 = (String) getParameter(PARAM_HIM_ALIAS_3);

        String higAlias1 = (String) getParameter(PARAM_HIG_ALIAS_1);
        String higAlias2 = (String) getParameter(PARAM_HIG_ALIAS_2);
        String higAlias3 = (String) getParameter(PARAM_HIG_ALIAS_3);

        if (validateAliasPair(himAlias1, higAlias1) && validateAliasPair(himAlias2, higAlias2)
                && validateAliasPair(himAlias3, higAlias3)) {
            copySeriesToOverrides(sourceHimOid, targetHigOid, himAlias1, himAlias2, himAlias3, higAlias1, higAlias2,
                    higAlias3);
        }
    }

    /**
     * Copies series data to override records.
     *
     * @param sourceHimOid String
     * @param targetHigOid String
     * @param himAlias1 String
     * @param himAlias2 String
     * @param himAlias3 String
     * @param higAlias1 String
     * @param higAlias2 String
     * @param higAlias3 String
     */
    private void copySeriesToOverrides(String sourceHimOid,
                                       String targetHigOid,
                                       String himAlias1,
                                       String himAlias2,
                                       String himAlias3,
                                       String higAlias1,
                                       String higAlias2,
                                       String higAlias3) {
        HealthImmunizationGroup group =
                (HealthImmunizationGroup) getBroker().getBeanByOid(HealthImmunizationGroup.class, targetHigOid);

        if (group == null) {
            logMessage("Unable to find specified group");
        } else {
            X2Criteria existingOverrideCriteria = new X2Criteria();
            existingOverrideCriteria.addEqualTo(HealthImmunizationGroupOverride.COL_IMMUNIZATION_GROUP_OID,
                    group.getOid());

            BeanQuery existingOverrideQuery =
                    new BeanQuery(HealthImmunizationGroupOverride.class, existingOverrideCriteria);

            Map<String, HealthImmunizationGroupOverride> existingOverrides = getBroker()
                    .getMapByQuery(existingOverrideQuery, HealthImmunizationGroupOverride.COL_STUDENT_OID, 128);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, sourceHimOid);

            BeanQuery beanQuery = new BeanQuery(HealthImmunizationSeries.class, criteria);

            int updated = 0;
            int created = 0;

            QueryIterator iterator = getBroker().getIteratorByQuery(beanQuery);
            try {
                while (iterator.hasNext()) {
                    HealthImmunizationSeries series = (HealthImmunizationSeries) iterator.next();

                    if (series.getComplianceOverrideIndicator() || series.getWaivedIndicator() ||
                            series.getHistoryOfDiseaseIndicator() || series.getFollowupIndicator() ||
                            !StringUtils.isEmpty(series.getComment())) {
                        HealthImmunizationGroupOverride override = existingOverrides.get(series.getStudentOid());

                        if (override != null) {
                            if (m_updateExisting) {
                                updated++;
                            } else {
                                override = null;
                            }
                        } else {
                            override = new HealthImmunizationGroupOverride(getBroker().getPersistenceKey());
                            created++;
                        }

                        if (override != null) {
                            override.setComplianceOverrideIndicator(series.getComplianceOverrideIndicator());
                            override.setWaivedIndicator(series.getWaivedIndicator());
                            override.setHistoryOfDiseaseIndicator(series.getHistoryOfDiseaseIndicator());
                            override.setFollowupIndicator(series.getFollowupIndicator());
                            override.setComment(series.getComment());
                            override.setImmunizationGroupOid(group.getOid());
                            override.setStudentOid(series.getStudentOid());

                            if (himAlias1 != null) {
                                override.setFieldValueByAlias(higAlias1, series.getFieldValueByAlias(himAlias1));
                            }
                            if (himAlias2 != null) {
                                override.setFieldValueByAlias(higAlias2, series.getFieldValueByAlias(himAlias2));
                            }
                            if (himAlias3 != null) {
                                override.setFieldValueByAlias(higAlias3, series.getFieldValueByAlias(himAlias3));
                            }

                            getBroker().saveBeanForced(override);
                        }

                    }
                }
            } finally {
                iterator.close();
            }

            logMessage("Created " + created + " grouped override records");
            logMessage("Updated " + updated + " grouped override records");
        }
    }

    /**
     * Validates the passed series - group alias pair.
     *
     * @param himAlias String
     * @param higAlias String
     * @return boolean
     */
    private boolean validateAliasPair(String himAlias, String higAlias) {
        boolean valid = StringUtils.isEmpty(himAlias) == StringUtils.isEmpty(higAlias);

        if (valid && !StringUtils.isEmpty(himAlias)) {
            DataDictionaryField himField = m_dictionary.findDataDictionaryFieldByAlias(himAlias);
            DataDictionaryField higField = m_dictionary.findDataDictionaryFieldByAlias(higAlias);

            valid = himField != null && higField != null;
            valid = valid && HealthImmunizationSeries.DICTIONARY_ID.equals(himField.getDataTableOid().trim());
            valid = valid && HealthImmunizationGroupOverride.DICTIONARY_ID.equals(higField.getDataTableOid().trim());
        }

        if (!valid) {
            logMessage("Invalid alias input; no records updated.");
        }

        return valid;
    }
}
