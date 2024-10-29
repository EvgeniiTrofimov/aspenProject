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
 * Provides the data source for the Washington Weapon report.
 *
 * @author Administrator
 */
public class WAWeaponSummaryData extends ReportJavaSourceNet {
    protected static final String ALIAS_INTERVENTION = "DOE INTERVENTION";
    protected static final String ALIAS_REPORTABLE_BEHAVIOR = "DOE REPORTABLE BEHAVIOR";
    protected static final String ALIAS_WEAPON_TYPE = "DOE WEAPON TYPE";

    protected static final String CODE_EXPEL = "EX";
    protected static final String CODE_HANDGUN = "HG";
    protected static final String CODE_KNIFE = "RS";
    protected static final String CODE_MULTIPLE_FIREARMS = "MF";
    protected static final String CODE_OTHER_FIREARMS = "OF";
    protected static final String CODE_OTHER_WEAPONS = "KD";
    protected static final String CODE_RIFLE = "OW";

    protected static final String FIELD_HANDGUN = "handgun";
    protected static final String FIELD_HANDGUN_EXPEL = "handgunExpel";
    protected static final String FIELD_KNIFE = "knife";
    protected static final String FIELD_KNIFE_EXPEL = "knifeExpel";
    protected static final String FIELD_MULTIPLE_FIREARMS = "multipleFirearms";
    protected static final String FIELD_MULTIPLE_FIREARMS_EXPEL = "multipleFirearmsExpel";
    protected static final String FIELD_OTHER_FIREARMS = "otherFirearms";
    protected static final String FIELD_OTHER_FIREARMS_EXPEL = "otherFirearmsExpel";
    protected static final String FIELD_OTHER_WEAPONS = "otherWeapons";
    protected static final String FIELD_OTHER_WEAPONS_EXPEL = "otherWeaponsExpel";
    protected static final String FIELD_RIFLE = "rifle";
    protected static final String FIELD_RIFLE_EXPEL = "rifleExpel";
    protected static final String FIELD_SCHOOL_NAME = "schoolName";

    protected static final String PARAM_DATE = "date";
    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected int m_cHandgun;
    protected int m_cHandgunExpel;
    protected int m_cKnife;
    protected int m_cKnifeExpel;
    protected int m_cMultipleFirearms;
    protected int m_cMultipleFirearmsExpel;
    protected int m_cRifle;
    protected int m_cRifleExpel;
    protected int m_cOtherFirearms;
    protected int m_cOtherFirearmsExpel;
    protected int m_cOtherWeapons;
    protected int m_cOtherWeaponsExpel;
    protected DataDictionaryField m_fieldIntervention;
    protected DataDictionaryField m_fieldReportableBehavior;
    protected DataDictionaryField m_fieldWeaponType;

    protected Map<String, ReferenceCode> m_interventionCodeMap = null;
    protected SisSchool m_school;
    protected Map<String, ReferenceCode> m_weaponCodeMap = null;

    /**
     * Fills a grid containing the counts for the Weapon summary report.
     * The report is limited to ConductIncidents that are reportable
     * and have a weapon associated with the incident.
     *
     * @return JRDataSource
     * @throws Exception exception
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        PlainDate reportDate = (PlainDate) this.getParameter(PARAM_REPORT_DATE);
        addParameter(PARAM_DATE, reportDate);

        ReportDataGrid grid = new ReportDataGrid(100, 13);

        // get alias fields
        m_fieldIntervention = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(ALIAS_INTERVENTION);
        m_fieldReportableBehavior = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(ALIAS_REPORTABLE_BEHAVIOR);
        m_fieldWeaponType = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(ALIAS_WEAPON_TYPE);
        if (m_fieldIntervention == null || m_fieldReportableBehavior == null || m_fieldWeaponType == null) {
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
        // limit to weapons incidents
        criteria.addNotEmpty(m_fieldWeaponType.getJavaName(), getBroker().getPersistenceKey());
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
                String weaponType = (String) incident.getFieldValueByBeanPath(m_fieldWeaponType.getJavaName());
                String code = getWeaponCode(weaponType);
                // report unique incident count
                if (!incident.getIncidentId().equals(lastIncidentId)) {
                    String intervention = (String) incident.getFieldValueByBeanPath(m_fieldIntervention.getJavaName());
                    String interventionCode = getInterventionCode(intervention);
                    boolean fExpulsion = CODE_EXPEL.equals(interventionCode);
                    if (CODE_HANDGUN.equals(code)) {
                        if (fExpulsion) {
                            m_cHandgunExpel++;
                        } else {
                            m_cHandgun++;
                        }
                    } else if (CODE_KNIFE.equals(code)) {
                        if (fExpulsion) {
                            m_cKnifeExpel++;
                        } else {
                            m_cKnife++;
                        }
                    } else if (CODE_MULTIPLE_FIREARMS.equals(code)) {
                        if (fExpulsion) {
                            m_cMultipleFirearmsExpel++;
                        } else {
                            m_cMultipleFirearms++;
                        }
                    } else if (CODE_OTHER_FIREARMS.equals(code)) {
                        if (fExpulsion) {
                            m_cOtherFirearmsExpel++;
                        } else {
                            m_cOtherFirearms++;
                        }
                    } else if (CODE_OTHER_WEAPONS.equals(code)) {
                        if (fExpulsion) {
                            m_cOtherWeaponsExpel++;
                        } else {
                            m_cOtherWeapons++;
                        }
                    } else if (CODE_RIFLE.equals(code)) {
                        if (fExpulsion) {
                            m_cRifleExpel++;
                        } else {
                            m_cRifle++;
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
        grid.set(FIELD_HANDGUN, Integer.valueOf(m_cHandgun));
        grid.set(FIELD_KNIFE, Integer.valueOf(m_cKnife));
        grid.set(FIELD_MULTIPLE_FIREARMS, Integer.valueOf(m_cMultipleFirearms));
        grid.set(FIELD_OTHER_FIREARMS, Integer.valueOf(m_cRifle));
        grid.set(FIELD_OTHER_WEAPONS, Integer.valueOf(m_cOtherFirearms));
        grid.set(FIELD_RIFLE, Integer.valueOf(m_cOtherWeapons));
        grid.set(FIELD_HANDGUN_EXPEL, Integer.valueOf(m_cHandgunExpel));
        grid.set(FIELD_KNIFE_EXPEL, Integer.valueOf(m_cKnifeExpel));
        grid.set(FIELD_MULTIPLE_FIREARMS_EXPEL, Integer.valueOf(m_cMultipleFirearmsExpel));
        grid.set(FIELD_OTHER_FIREARMS_EXPEL, Integer.valueOf(m_cRifleExpel));
        grid.set(FIELD_OTHER_WEAPONS_EXPEL, Integer.valueOf(m_cOtherFirearmsExpel));
        grid.set(FIELD_RIFLE_EXPEL, Integer.valueOf(m_cOtherWeaponsExpel));
    }

    /**
     * Returns the state weapon code for a weapon code.
     * Reference table is cached at first execution
     *
     * @param weaponCode String
     * @return String
     */
    private String getWeaponCode(String weaponCode) {
        if (m_weaponCodeMap == null) {
            // load weapon reference codes
            m_weaponCodeMap = new HashMap<String, ReferenceCode>();
            ReferenceTable refTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    m_fieldWeaponType.getReferenceTableOid());
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(getBroker());
                for (ReferenceCode code : codes) {
                    m_weaponCodeMap.put(code.getCode(), code);
                }
            }
        }
        return m_weaponCodeMap.get(weaponCode).getStateCode();
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
        m_cHandgun = 0;
        m_cKnife = 0;
        m_cMultipleFirearms = 0;
        m_cRifle = 0;
        m_cOtherFirearms = 0;
        m_cOtherWeapons = 0;
        m_cHandgunExpel = 0;
        m_cKnifeExpel = 0;
        m_cMultipleFirearmsExpel = 0;
        m_cRifleExpel = 0;
        m_cOtherFirearmsExpel = 0;
        m_cOtherWeaponsExpel = 0;
    }

}
