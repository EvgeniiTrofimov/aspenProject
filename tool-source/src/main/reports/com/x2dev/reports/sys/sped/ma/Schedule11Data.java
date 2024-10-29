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
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepServiceFte;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.sped.MassachusettsAliases;
import com.x2dev.sis.model.business.sped.MassachusettsFteCalculator;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the Schedule 11 Pupil Membership Summary report (special education stats only).
 *
 * @author X2 Development Corporation
 */
public class Schedule11Data extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;
    private static final String[] COLUMN_2_STATE_CODES = {"01"};
    private static final String[] COLUMN_3_STATE_CODES = {"02", "04", "05", "08", "09", "10"};
    private static final String[] COLUMN_5_STATE_CODES = {"06", "07"};

    private static final String[] PLACEMENT_STATE_CODES = {"08", "09", "10", "20", "40", "41", "50", "60", "70", "90"};

    public static final String PARAM_DISTRICT_CONTEXT = "context";
    public static final String PARAM_DISTRICT_CONTEXT_OID = "contextOid";
    public static final String PARAM_RECALCULATE = "recalculate";

    public static final String COL_PLACEMENT_STATE_CODE = "placementCodeState";
    public static final String COL_1 = "col1";
    public static final String COL_2 = "col2";
    public static final String COL_3 = "col3";
    public static final String COL_5 = "col5";
    public static final String COL_6 = "col6";
    public static final String COL_LINE_NUMBER = "lineNumber";
    public static final String COL_DESCRIPTION = "description";

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

        DistrictSchoolYearContext context =
                (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class, contextOid);

        if (recalculate) {
            recalculateFtes(context);
        }

        Map<String, ReferenceCode> placementCodeLookup = getPlacementCodeLookup();
        Map<String, ReferenceCode> enrollmentTypeLookup = getEnrollmentTypeLookup();
        Map<String, String> descriptions = getDescriptions();
        Map<String, String> lineNumbers = getLineNumbers();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepServiceFte.COL_FINAL_ENTRY_INDICATOR, Boolean.valueOf(true));
        criteria.addEqualTo(IepServiceFte.COL_DISTRICT_CONTEXT_OID, contextOid);

        QueryByCriteria query = new QueryByCriteria(IepServiceFte.class, criteria);

        Map<String, Collection<IepServiceFte>> fteMap =
                getBroker().getGroupedCollectionByQuery(query, IepServiceFte.COL_PLACEMENT_TYPE, 16);

        for (String placementStateCode : PLACEMENT_STATE_CODES) {
            grid.append();
            grid.set(COL_PLACEMENT_STATE_CODE, placementStateCode);
            grid.set(COL_LINE_NUMBER, lineNumbers.get(placementStateCode));
            grid.set(COL_DESCRIPTION, descriptions.get(placementStateCode));

            int col1_headcount = 0;
            double col2_residentFte = 0;
            double col3_tuitionedInFte = 0;
            int col5_tuitionedOutHeadcount = 0;
            double col6_tuitionedOutFte = 0;

            for (String placementUserCode : placementCodeLookup.keySet()) {
                ReferenceCode currentRefCode = placementCodeLookup.get(placementUserCode);
                if (currentRefCode != null && placementStateCode.equals(currentRefCode.getStateCode())) {
                    Collection<IepServiceFte> fteEntries = fteMap.get(placementUserCode);
                    if (fteEntries != null) {
                        for (IepServiceFte fteEntry : fteEntries) {
                            boolean outplaced = "41".equals(placementStateCode) ||
                                    "50".equals(placementStateCode) ||
                                    "60".equals(placementStateCode);

                            if (!outplaced) {
                                col1_headcount++;
                            }

                            ReferenceCode enrollmentTypeCode =
                                    enrollmentTypeLookup.get(fteEntry.getStudent().getEnrollmentTypeCode());
                            if (enrollmentTypeCode != null) {
                                double fte = fteEntry.getCumulativeFte() != null
                                        ? fteEntry.getCumulativeFte().doubleValue() : 0;

                                if (containsCode(COLUMN_2_STATE_CODES, enrollmentTypeCode.getStateCode())
                                        && !outplaced) {
                                    col2_residentFte += fte;
                                } else if (containsCode(COLUMN_3_STATE_CODES, enrollmentTypeCode.getStateCode())
                                        && !outplaced) {
                                    col3_tuitionedInFte += fte;
                                } else if (containsCode(COLUMN_5_STATE_CODES, enrollmentTypeCode.getStateCode())) {
                                    col5_tuitionedOutHeadcount++;
                                    col6_tuitionedOutFte += fte;
                                }
                            }
                        }
                    }
                }
            }

            grid.set(COL_1, Integer.valueOf(col1_headcount));
            grid.set(COL_2, Double.valueOf(col2_residentFte));
            grid.set(COL_3, Double.valueOf(col3_tuitionedInFte));
            grid.set(COL_5, Integer.valueOf(col5_tuitionedOutHeadcount));
            grid.set(COL_6, Double.valueOf(col6_tuitionedOutFte));
        }

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
     * Returns true if the passed array contains the passed code value.
     *
     * @param codeArray String[]
     * @param code String
     * @return boolean
     */
    private boolean containsCode(String[] codeArray, String code) {
        boolean contains = false;

        for (String currentCode : codeArray) {
            if (currentCode.equals(code)) {
                contains = true;
                break;
            }
        }

        return contains;
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

    /**
     * Returns a Map containing the descriptions to use for each line number on the report.
     * Descriptions are keyed by state placement code.
     *
     * @return HashMap
     */
    private HashMap<String, String> getDescriptions() {
        HashMap<String, String> descriptions = new HashMap<String, String>();

        descriptions.put("08", "3-5 yr olds, 100% in classroom (DOE034-08)");
        descriptions.put("09", "3-5 yr olds, 100% separate classroom (DOE034-09)");
        descriptions.put("10", "all ages, full inclusion < 21% (DOE034-10)");
        descriptions.put("20", "all ages, partial inclusion 21-60% (DOE034-20)");
        descriptions.put("40", "all ages, substantially separate > 60% (DOE034-40)");
        descriptions.put("41", "all ages, public separate day school (DOE034-41)");
        descriptions.put("50", "all ages, private separate day school (DOE034-50)");
        descriptions.put("60", "all ages, private residential school (DOE034-60)");
        descriptions.put("70", "all ages, homebound/hospital (DOE034-70)");
        descriptions.put("90", "all ages, public residential institutions (DOE034-90)");

        return descriptions;
    }

    /**
     * Returns a map of enrollment type ReferenceCode beans keyed on code value.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> getEnrollmentTypeLookup() {
        Map<String, ReferenceCode> codeLookup = new HashMap<String, ReferenceCode>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        DataDictionaryField enrollmenTypeField =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_ENROLLMENT_TYPE_CODE);

        if (enrollmenTypeField.getReferenceTableOid() != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, enrollmenTypeField.getReferenceTableOid());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

            codeLookup = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 16);
        }

        return codeLookup;
    }

    /**
     * Returns a map of Schedule 11 line numbers corresponding to each state placement code.
     *
     * @return HashMap
     */
    private HashMap<String, String> getLineNumbers() {
        HashMap<String, String> lineNumbers = new HashMap<String, String>();

        lineNumbers.put("08", "5028");
        lineNumbers.put("09", "5029");
        lineNumbers.put("10", "5031");
        lineNumbers.put("20", "5032");
        lineNumbers.put("40", "5033");
        lineNumbers.put("41", "5034");
        lineNumbers.put("50", "5035");
        lineNumbers.put("60", "5036");
        lineNumbers.put("70", "5037");
        lineNumbers.put("90", "5039");

        return lineNumbers;
    }

    /**
     * Returns a map of placement ReferenceCode beans keyed on code value.
     *
     * @return Map
     */
    private Map<String, ReferenceCode> getPlacementCodeLookup() {
        Map<String, ReferenceCode> codeLookup = new HashMap<String, ReferenceCode>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(
                SpedUtils.getIepDictionary(getOrganization(), getBroker()),
                getBroker().getPersistenceKey());

        DataDictionaryField placementField = dictionary.findDataDictionaryFieldByAlias(
                MassachusettsAliases.IEP_EDUCATIONAL_ENVIRONMENT);

        if (placementField.getReferenceTableOid() != null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, placementField.getReferenceTableOid());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

            codeLookup = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 16);
        }

        return codeLookup;
    }
}
