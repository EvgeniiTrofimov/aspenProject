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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepServiceFte;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.MassachusettsAliases;
import com.x2dev.sis.model.business.sped.MassachusettsFteCalculator;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Report that displays FTE values broken down by month.
 *
 * @author X2 Development Corporation
 */
public class FteByMonthData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    public static final String PARAM_CUMULATIVE = "cumulative";
    public static final String PARAM_DISTRICT_CONTEXT = "context";
    public static final String PARAM_DISTRICT_CONTEXT_OID = "contextOid";
    public static final String PARAM_RECALCULATE = "recalculate";

    public static final String PARAM_COLUMN_LABEL_MAP = "columnLabels";

    public static final String COL_ESY_SUFFIX = "_esy";
    public static final String COL_FTE_PREFIX = "fte";

    public static final String COL_PLACEMENT_CODE = "placementCode";
    public static final String COL_PLACEMENT_CODE_STATE = "placementCodeState";

    public static final String COL_SERVICE_HOURS_PREFIX = "serviceHours";
    public static final String COL_STUDENT = "student";

    private PrivilegeSet m_privilegeSet = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        String contextOid = (String) getParameter(PARAM_DISTRICT_CONTEXT_OID);
        boolean recalculate = ((Boolean) getParameter(PARAM_RECALCULATE)).booleanValue();
        boolean cumulative = ((Boolean) getParameter(PARAM_CUMULATIVE)).booleanValue();

        DistrictSchoolYearContext context =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);

        HashMap<Integer, Integer> columnNumberMap = getColumnNumberMap(context);
        Map<String, ReferenceCode> placementCodeLookup = getPlacementCodeLookup();

        if (recalculate) {
            recalculateFtes(context);
        }

        QueryIterator fteEntries = getBroker().getIteratorByQuery(getFteQuery(contextOid));
        try {
            String lastStudentOid = null;
            while (fteEntries.hasNext()) {
                IepServiceFte fteEntry = (IepServiceFte) fteEntries.next();

                if (!fteEntry.getStudentOid().equals(lastStudentOid)) {
                    grid.append();
                    grid.set(COL_STUDENT, fteEntry.getStudent());

                    for (int i = 0; i < 12; i++) {
                        grid.set(COL_SERVICE_HOURS_PREFIX + i, new BigDecimal(0));
                        grid.set(COL_FTE_PREFIX + i, new BigDecimal(0));
                    }
                }

                int columnNumber = columnNumberMap.get(Integer.valueOf(fteEntry.getMonth())).intValue();
                String column = Integer.toString(columnNumber);

                if (MassachusettsFteCalculator.CALC_INFO_EXTENDED_YEAR.equals(fteEntry.getCalculationInfo())) {
                    column += COL_ESY_SUFFIX;
                }

                BigDecimal serviceHours = null;
                BigDecimal fte = null;

                if (cumulative) {
                    serviceHours = fteEntry.getCumulativeServiceHours();
                    fte = fteEntry.getCumulativeFte();
                } else {
                    serviceHours = fteEntry.getServiceHours();
                    fte = fteEntry.getCumulativeFte();
                }

                if (serviceHours == null) {
                    serviceHours = new BigDecimal(0);
                }
                if (fte == null) {
                    fte = new BigDecimal(0);
                }

                grid.set(column, fteEntry);
                grid.set(COL_SERVICE_HOURS_PREFIX + column, serviceHours);
                grid.set(COL_FTE_PREFIX + column, fte);

                if (fteEntry.getPlacementType() != null) {
                    grid.set(COL_PLACEMENT_CODE, fteEntry.getPlacementType());

                    ReferenceCode code = placementCodeLookup.get(fteEntry.getPlacementType());
                    if (code != null) {
                        grid.set(COL_PLACEMENT_CODE_STATE, code.getStateCode());
                    }
                }

                lastStudentOid = fteEntry.getStudentOid();
            }
        } finally {
            fteEntries.close();
        }

        addParameter(PARAM_COLUMN_LABEL_MAP, getColumnLabelMap(columnNumberMap));
        addParameter(PARAM_DISTRICT_CONTEXT, context);

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_privilegeSet = userData.getPrivilegeSet();
    }

    /**
     * Returns a query that finds the FTE entries to include on the report.
     *
     * @param contextOid String
     * @return Query by criteria
     */
    private QueryByCriteria getFteQuery(String contextOid) {
        Criteria fteCriteria = new Criteria();
        fteCriteria.addEqualTo(IepServiceFte.COL_DISTRICT_CONTEXT_OID, contextOid);

        QueryByCriteria fteQuery = new QueryByCriteria(IepServiceFte.class, fteCriteria);
        fteQuery.addOrderByAscending(
                IepServiceFte.REL_STUDENT + "." +
                        SisStudent.COL_NAME_VIEW);
        fteQuery.addOrderByAscending(
                IepServiceFte.REL_STUDENT + "." +
                        SisStudent.COL_YOG);

        return fteQuery;
    }

    /**
     * Returns a map containing the labels to use for columns 0 - 11 on the report.
     *
     * @param numberMap HashMap<Integer,Integer>
     * @return HashMap
     */
    private HashMap<Integer, String> getColumnLabelMap(HashMap<Integer, Integer> numberMap) {
        HashMap<Integer, String> labelMap = new HashMap<Integer, String>();

        for (int i = 0; i < 12; i++) {
            Integer monthNumber = numberMap.get(Integer.valueOf(i));

            if (monthNumber != null) {
                String resourceKey = "label.calendar.monthAbbr." + monthNumber;
                String monthLabel = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(getLocale(), resourceKey);

                labelMap.put(Integer.valueOf(i), monthLabel);
            }
        }

        return labelMap;
    }

    /**
     * Returns a map that associates a column on the report (0 - 11) to a calendar year.
     *
     * @param context DistrictSchoolYearContext
     * @return HashMap
     */
    private HashMap<Integer, Integer> getColumnNumberMap(DistrictSchoolYearContext context) {
        HashMap<Integer, Integer> headerMap = new HashMap<Integer, Integer>();

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(context.getStartDate());

        int monthNumber = 0;

        while (!calendar.getTime().after(context.getEndDate())) {
            headerMap.put(Integer.valueOf(calendar.get(Calendar.MONTH)), Integer.valueOf(monthNumber));
            calendar.add(Calendar.MONTH, 1);

            monthNumber++;
        }

        return headerMap;
    }


    /**
     * Returns a map of placement ReferenceCode beans keyed on code value.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> getPlacementCodeLookup() {
        Map<String, ReferenceCode> stateCodeLookup = new HashMap<String, ReferenceCode>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                SpedUtils.getIepDictionary(getOrganization(), getBroker()),
                getBroker().getPersistenceKey());

        DataDictionaryField placementField = dictionary.findDataDictionaryFieldByAlias(
                MassachusettsAliases.IEP_EDUCATIONAL_ENVIRONMENT);

        if (placementField.getReferenceTableOid() != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, placementField.getReferenceTableOid());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

            stateCodeLookup = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 16);
        }

        return stateCodeLookup;
    }

    /**
     * Recalculates FTEs for the passed school year.
     *
     * @param context DistrictSchoolYearContext
     */
    private void recalculateFtes(DistrictSchoolYearContext context) {
        MassachusettsFteCalculator calculator =
                new MassachusettsFteCalculator(getBroker(), m_privilegeSet, getOrganization());

        Criteria studentCriteria = new Criteria();
        studentCriteria.addNotNull(SisStudent.COL_SPED_STATUS_CODE);
        studentCriteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());

        calculator.calculate(studentCriteria, context);
    }
}
