/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Provides the data source for the Washington behavior report.
 *
 * @author Administrator
 *
 */
public class WABehaviorSummaryData extends ReportJavaSourceNet {
    protected static final String ALIAS_INTERVENTION = "DOE INTERVENTION";
    protected static final String ALIAS_REPORTABLE_BEHAVIOR = "DOE REPORTABLE BEHAVIOR";

    protected static final String CODE_ALCOHOL = "3";
    protected static final String CODE_BULLYING = "1";
    protected static final String CODE_EXPEL = "EX";
    protected static final String CODE_FIGHTING = "5";
    protected static final String CODE_ILLICIT_DRUG = "4";
    protected static final String CODE_TOBACCO = "2";
    protected static final String CODE_VIOLENCE = "6";
    protected static final String CODE_VIOLENCE_MAJOR = "7";

    protected static final String FIELD_ALCOHOL = "alcohol";
    protected static final String FIELD_ALCOHOL_EXPEL = "alcoholExpel";
    protected static final String FIELD_BULLYING = "bullying";
    protected static final String FIELD_BULLYING_EXPEL = "bullyingExpel";
    protected static final String FIELD_FIGHTING = "fighting";
    protected static final String FIELD_FIGHTING_EXPEL = "fightingExpel";
    protected static final String FIELD_ILLICIT_DRUG = "illicitDrug";
    protected static final String FIELD_ILLICIT_DRUG_EXPEL = "illicitDrugExpel";
    protected static final String FIELD_TOBACCO = "tobacco";
    protected static final String FIELD_TOBACCO_EXPEL = "tobaccoExpel";
    protected static final String FIELD_SCHOOL_NAME = "schoolName";
    protected static final String FIELD_VIOLENCE = "violence";
    protected static final String FIELD_VIOLENCE_EXPEL = "violenceExpel";
    protected static final String FIELD_VIOLENCE_MAJOR = "violenceMajor";
    protected static final String FIELD_VIOLENCE_MAJOR_EXPEL = "violenceMajorExpel";

    protected static final String PARAM_DATE = "date";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected int m_cAlcohol;
    protected int m_cAlcoholExpel;
    protected int m_cBullying;
    protected int m_cBullyingExpel;
    protected int m_cFighting;
    protected int m_cFightingExpel;
    protected int m_cIllicitDrug;
    protected int m_cIllicitDrugExpel;
    protected int m_cTobacco;
    protected int m_cTobaccoExpel;
    protected int m_cViolence;
    protected int m_cViolenceExpel;
    protected int m_cViolenceMajor;
    protected int m_cViolenceMajorExpel;
    protected DataDictionaryField m_fieldIntervention;
    protected DataDictionaryField m_fieldReportableBehavior;

    protected Map<String, ReferenceCode> m_interventionCodeMap = null;
    protected SisSchool m_school;
    protected Map<String, ReferenceCode> m_behaviorCodeMap = null;

    /**
     * Gathers the behavior data for the report including all ConductIncident records
     * for the year that are state reportable.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        PlainDate reportDate = (PlainDate) this.getParameter(PARAM_REPORT_DATE);
        addParameter(PARAM_DATE, reportDate);

        ReportDataGrid grid = new ReportDataGrid(10000, 20);

        // get alias fields
        m_fieldIntervention = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(ALIAS_INTERVENTION);
        m_fieldReportableBehavior = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(ALIAS_REPORTABLE_BEHAVIOR);
        if (m_fieldIntervention == null || m_fieldReportableBehavior == null) {
            return grid;
        }

        X2Criteria criteria = new X2Criteria();
        // add school selection
        if (isSchoolContext()) {
            criteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            criteria.addEqualTo(ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }
        // add date selection
        criteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getStartDate());
        criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, getCurrentContext().getEndDate());
        // limit to reportable incidents
        criteria.addNotEmpty(m_fieldReportableBehavior.getJavaName(), getBroker().getPersistenceKey());
        // limit to rows with incidentId
        criteria.addNotEmpty(ConductIncident.COL_INCIDENT_ID, getBroker().getPersistenceKey());
        QueryByCriteria query = new QueryByCriteria(ConductIncident.class, criteria);
        query.addOrderByAscending(ConductIncident.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        query.addOrderByAscending(ConductIncident.COL_INCIDENT_ID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        initCounters();
        m_school = null;
        String lastIncidentId = null;
        try {
            while (iterator.hasNext()) {
                ConductIncident incident = (ConductIncident) iterator.next();
                if (m_school != null && !m_school.getOid().equals(incident.getSchoolOid())) {
                    appendRow(grid);
                    initCounters();
                }
                m_school = incident.getSchool();
                String behaviorType =
                        (String) incident.getFieldValueByBeanPath(m_fieldReportableBehavior.getJavaName());
                String code = getBehaviorCode(behaviorType);
                // report unique incident count
                if (!incident.getIncidentId().equals(lastIncidentId)) {
                    String intervention = (String) incident.getFieldValueByBeanPath(m_fieldIntervention.getJavaName());
                    String interventionCode = getInterventionCode(intervention);
                    boolean fExpulsion = CODE_EXPEL.equals(interventionCode);
                    if (CODE_ALCOHOL.equals(code)) {
                        if (fExpulsion) {
                            m_cAlcoholExpel++;
                        } else {
                            m_cAlcohol++;
                        }
                    } else if (CODE_BULLYING.equals(code)) {
                        if (fExpulsion) {
                            m_cBullyingExpel++;
                        } else {
                            m_cBullying++;
                        }
                    } else if (CODE_FIGHTING.equals(code)) {
                        if (fExpulsion) {
                            m_cFightingExpel++;
                        } else {
                            m_cFighting++;
                        }
                    } else if (CODE_ILLICIT_DRUG.equals(code)) {
                        if (fExpulsion) {
                            m_cIllicitDrugExpel++;
                        } else {
                            m_cIllicitDrug++;
                        }
                    } else if (CODE_TOBACCO.equals(code)) {
                        if (fExpulsion) {
                            m_cTobaccoExpel++;
                        } else {
                            m_cTobacco++;
                        }
                    } else if (CODE_VIOLENCE.equals(code)) {
                        if (fExpulsion) {
                            m_cViolenceExpel++;
                        } else {
                            m_cViolence++;
                        }
                    } else if (CODE_VIOLENCE_MAJOR.equals(code)) {
                        if (fExpulsion) {
                            m_cViolenceMajorExpel++;
                        } else {
                            m_cViolenceMajor++;
                        }
                    }
                }
                lastIncidentId = incident.getIncidentId();
            }
            if (m_school != null) {
                appendRow(grid);
            }
        } finally {
            iterator.close();
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Adds an output row to the grid.
     *
     * @param grid ReportDataGrid
     */
    private void appendRow(ReportDataGrid grid) {
        grid.append();
        grid.set(FIELD_SCHOOL_NAME, m_school.getName());
        grid.set(FIELD_ALCOHOL, Integer.valueOf(m_cAlcohol));
        grid.set(FIELD_BULLYING, Integer.valueOf(m_cBullying));
        grid.set(FIELD_FIGHTING, Integer.valueOf(m_cFighting));
        grid.set(FIELD_ILLICIT_DRUG, Integer.valueOf(m_cIllicitDrug));
        grid.set(FIELD_TOBACCO, Integer.valueOf(m_cTobacco));
        grid.set(FIELD_VIOLENCE, Integer.valueOf(m_cViolence));
        grid.set(FIELD_VIOLENCE_MAJOR, Integer.valueOf(m_cViolenceMajor));
        grid.set(FIELD_ALCOHOL_EXPEL, Integer.valueOf(m_cAlcoholExpel));
        grid.set(FIELD_BULLYING_EXPEL, Integer.valueOf(m_cBullyingExpel));
        grid.set(FIELD_FIGHTING_EXPEL, Integer.valueOf(m_cFightingExpel));
        grid.set(FIELD_ILLICIT_DRUG_EXPEL, Integer.valueOf(m_cIllicitDrugExpel));
        grid.set(FIELD_TOBACCO_EXPEL, Integer.valueOf(m_cTobaccoExpel));
        grid.set(FIELD_VIOLENCE_EXPEL, Integer.valueOf(m_cViolenceExpel));
        grid.set(FIELD_VIOLENCE_MAJOR_EXPEL, Integer.valueOf(m_cViolenceMajorExpel));
    }

    /**
     * Returns the state behavior code for a behavior code.
     * Reference table is cached at first execution
     *
     * @param weaponCode String
     * @return String
     */
    private String getBehaviorCode(String weaponCode) {
        if (m_behaviorCodeMap == null) {
            // load weapon reference codes
            m_behaviorCodeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    m_fieldReportableBehavior.getReferenceTableOid());
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    m_behaviorCodeMap.put(code.getCode(), code);
                }
            }
        }
        return m_behaviorCodeMap.get(weaponCode).getStateCode();
    }

    /**
     * Returns the state intervention code
     * The table is cached on first execution.
     *
     * @param interventionCode String
     * @return String
     */
    private String getInterventionCode(String interventionCode) {
        if (m_interventionCodeMap == null) {
            // load weapon reference codes
            m_interventionCodeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    m_fieldIntervention.getReferenceTableOid());
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    m_interventionCodeMap.put(code.getCode(), code);
                }
            }
        }
        return m_interventionCodeMap.get(interventionCode).getStateCode();
    }

    /**
     * Reset accumulators to zero.
     */
    private void initCounters() {
        m_cAlcohol = 0;
        m_cBullying = 0;
        m_cFighting = 0;
        m_cIllicitDrug = 0;
        m_cTobacco = 0;
        m_cViolence = 0;
        m_cViolenceMajor = 0;
        m_cAlcoholExpel = 0;
        m_cBullyingExpel = 0;
        m_cFightingExpel = 0;
        m_cIllicitDrugExpel = 0;
        m_cTobaccoExpel = 0;
        m_cViolenceExpel = 0;
        m_cViolenceMajorExpel = 0;
    }
}
