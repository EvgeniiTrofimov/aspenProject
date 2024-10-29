/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.wa;

/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationGroup;
import com.x2dev.sis.model.beans.HealthImmunizationGroupOverride;
import com.x2dev.sis.model.beans.HealthImmunizationRuleAttributes;
import com.x2dev.sis.model.beans.HealthImmunizationRuleInstance;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentReportJavaSource;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the Bus List report. This report prints the list of students sorted/grouped
 * by AM or PM bus.
 *
 * @author X2 Development Corporation
 */
public class ImmunizationReportData extends StudentReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "immunization groups to include" report parameter. This value is a String.
     */
    public static final String HIG_TO_INCLUDE_PARAM = "higOids";

    /**
     * Immunization Oids
     */
    public static final String IMMUNIZATION_OIDS_PARAM = "ImmunizationOids";

    /**
     * Starting report date
     */
    public static final String START_DATE_PARAM = "date";

    protected Collection<HealthImmunizationRuleAttributes> m_definitions;
    protected String m_defsToInclude;
    protected Map<String, Map<String, HealthImmunizationRuleInstance>> m_series;

    /**
     * @see com.x2dev.sis.tools.reports.StudentReportJavaSource#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        /*
         * String with selected "Immunizations to include" and "Immunization groups to include"
         * separated by a comma
         */
        m_defsToInclude = (String) getParameter(IMMUNIZATION_OIDS_PARAM);
        if (StringUtils.isEmpty(m_defsToInclude)) {
            m_defsToInclude = "";
        } else {
            m_defsToInclude += ",";
        }
        m_defsToInclude += (String) getParameter(HIG_TO_INCLUDE_PARAM);
        /*
         * Build the criteria based on user input
         */
        Criteria criteria = buildCriteria();
        ReportDataGrid grid = new ReportDataGrid();
        ArrayList<String> sort = new ArrayList<String>();
        QueryByCriteria query = createQueryByCriteria(SisStudent.class, criteria);

        /*
         * Build the sort based on user input
         *
         * If we are not in the context of a school, sort by the school first to support school
         * grouping on the format.
         */
        if (!isSchoolContext()) {
            query.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
            query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        }

        populateLookupMaps(query);
        // AppGlobals.getLog().severe("Query : " + query.toString());

        QueryIterator students = null;
        try {
            students = getBroker().getIteratorByQuery(query);
        } catch (org.apache.ojb.broker.PersistenceBrokerSQLException e) {
            if ("##snapshot".equals(getParameter(QUERY_BY_PARAM))) {
                return grid;
            }
            throw e;
        }


        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                for (HealthImmunizationRuleAttributes ruleAttributes : m_definitions) {
                    if (StringUtils.isEmpty(m_defsToInclude) || m_defsToInclude.contains(ruleAttributes.getOid())) {
                        HealthImmunizationRuleInstance ruleInstance = getRuleInstance(ruleAttributes, student);

                        // case for individual immunizations
                        if (ruleInstance instanceof HealthImmunizationSeries) {
                            HealthImmunizationSeries immunization = (HealthImmunizationSeries) ruleInstance;
                            if (immunization.getWaivedIndicator()) {
                                grid.append();
                                grid.set("student", student);
                                grid.set("nameView", student.getNameView());
                                grid.set("yog", Integer.valueOf(student.getYog()));
                                grid.set("school", student.getSchool());
                                grid.set("schoolName", student.getSchool().getName());
                                grid.set("immunization", immunization.getFirstIdentifyingValue());
                            }
                        }

                        // case for group immunizations
                        else if (ruleInstance instanceof HealthImmunizationGroupOverride) {
                            HealthImmunizationGroupOverride immunization =
                                    (HealthImmunizationGroupOverride) ruleInstance;
                            if (immunization.getWaivedIndicator()) {
                                grid.append();
                                grid.set("student", student);
                                grid.set("nameView", student.getNameView());
                                grid.set("yog", Integer.valueOf(student.getYog()));
                                grid.set("school", student.getSchool());
                                grid.set("schoolName", student.getSchool().getName());
                                grid.set("immunization", immunization.getFirstIdentifyingValue());
                            }
                        }
                    }
                }
            }
        } finally {
            students.close();
        }


        // Add the user sort param last.
        sort.add("immunization");
        sort.add("schoolName");
        sort.add((String) getParameter(SORT_PARAM));

        // Sort by immunizations, then the user param
        grid.sort(sort, true);

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns a series for a given definition and student.
     *
     * @param def HealthImmunizationRuleAttributes
     * @param student SisStudent
     * @return HealthImmunizationSeries
     */
    private HealthImmunizationRuleInstance getRuleInstance(HealthImmunizationRuleAttributes def, SisStudent student) {
        HealthImmunizationRuleInstance ruleInstance = null;

        Map<String, HealthImmunizationRuleInstance> ruleInstanceMap = m_series.get(def.getOid());
        if (ruleInstanceMap != null) {
            ruleInstance = ruleInstanceMap.get(student.getOid());
        }

        return ruleInstance;
    }

    /**
     * Looks up health immunization definition and series for the students in the passed
     * query.
     *
     * @param studentQuery QueryByCriteria
     */
    private void populateLookupMaps(QueryByCriteria studentQuery) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentQuery.getCriteria());

        // need to populate definitions, series, and doses.

        QueryByCriteria definitionQuery = new QueryByCriteria(HealthImmunizationDefinition.class, new Criteria());
        m_definitions = getBroker().getCollectionByQuery(definitionQuery);

        QueryByCriteria groupQuery = new QueryByCriteria(HealthImmunizationGroup.class, new Criteria());
        m_definitions.addAll(getBroker().getCollectionByQuery(groupQuery));

        // Add the series data
        Criteria seriesCriteria = new Criteria();
        seriesCriteria.addIn(HealthImmunizationSeries.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria seriesQuery =
                new QueryByCriteria(HealthImmunizationSeries.class, seriesCriteria);

        m_series = getBroker().getNestedMapByQuery(seriesQuery,
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID,
                HealthImmunizationSeries.COL_STUDENT_OID,
                16,
                128);

        Criteria overridesCriteria = new Criteria();
        overridesCriteria.addIn(HealthImmunizationGroupOverride.COL_STUDENT_OID, studentSubQuery);

        QueryByCriteria overridesQuery =
                new QueryByCriteria(HealthImmunizationGroupOverride.class, overridesCriteria);

        m_series.putAll(getBroker().getNestedMapByQuery(overridesQuery,
                HealthImmunizationGroupOverride.COL_IMMUNIZATION_GROUP_OID,
                HealthImmunizationGroupOverride.COL_STUDENT_OID,
                16,
                128));
    }
}
