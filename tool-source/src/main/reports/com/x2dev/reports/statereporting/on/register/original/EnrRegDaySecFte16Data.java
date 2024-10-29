/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.on.register.original;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.procedures.statereporting.on.register.original.OntarioAlias;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.sis.model.beans.UserDefinedTableD;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Enrollment Register Secondary Day (Full and Part time) for Ontario Add
 * Enrolment Register Helper as external data source
 *
 * @author Follett Software Company
 */
public class EnrRegDaySecFte16Data extends EnrRegRptHelperFte16 {
    private static final long serialVersionUID = 1L;

    // Constant values - Student fte table register type
    protected static final Map<String, String> CONST_FTE_REG_TYPE_TO_RPT_TYPE_MAP = new HashMap<String, String>();
    static {
        CONST_FTE_REG_TYPE_TO_RPT_TYPE_MAP.put(CONST_RPT_TYPE_FT, OntarioAlias.CONST_STD_FTE_REG_FT);
        CONST_FTE_REG_TYPE_TO_RPT_TYPE_MAP.put(CONST_RPT_TYPE_PT, OntarioAlias.CONST_STD_FTE_REG_PT);
    }

    /**
     * @see com.x2dev.reports.on.EnrRegRptHelperFte16#setReportType()
     *
     *      Override to set report type variables for this report
     */
    @Override
    protected void setReportType() {
        String reportType = (String) getParameter(PARAM_REPORT_TYPE);
        m_sklLvl = CONST_EMPTY;
        m_rptType = CONST_EMPTY;
        if (reportType != null) {
            if (reportType.equals(CONST_RPT_IN_SEC_FT)) {
                m_sklLvl = CONST_SKL_LVL_SEC;
                m_rptType = CONST_RPT_TYPE_FT;
            } else if (reportType.equals(CONST_RPT_IN_SEC_PT)) {
                m_sklLvl = CONST_SKL_LVL_SEC;
                m_rptType = CONST_RPT_TYPE_PT;
            }
        }
    }

    /**
     * @see com.x2dev.reports.on.EnrRegRptHelperFte16#setGradeLevels()
     *
     *      Override to set grade levels for this report
     */
    @Override
    protected void setGradeLevels() {
        String gradeRange = (String) getParameter(PARAM_GRADE_RANGE);
        m_gradeLvls = new ArrayList<String>();
        if (gradeRange != null) {
            if (gradeRange.equals(CONST_GRADE_RANGE_9_TO_12)) {
                m_gradeLvls.addAll(CONST_GRADE_LVLS_09_TO_12);
            }
        }
    }

    /**
     * @see com.x2dev.reports.on.EnrRegRptHelperFte16#createRptTypeStdOidsSubQuery(X2Criteria)
     *
     *      Override to get from user defined table for this report
     */
    @Override
    protected SubQuery createRptTypeStdOidsSubQuery(X2Criteria enrCriteria) {
        // create sub query of student oids based on enrolment table (with at least one
        // enrolment of
        // report type in school year) based on enrolment records as defined in the
        // super class
        SubQuery enrSubQueryRptTypeEnr = super.createRptTypeStdOidsSubQuery(enrCriteria);

        // create criteria for enrolment subquery
        X2Criteria enrCriteriaEnrOrUdt = new X2Criteria();
        enrCriteriaEnrOrUdt.addIn(X2BaseBean.COL_OID, enrSubQueryRptTypeEnr);

        // create criteria for udt subquery
        X2Criteria enrCriteriaUdt = new X2Criteria();
        enrCriteriaUdt.addIn(X2BaseBean.COL_OID, m_udtSubQueryRptTypeSec);

        // add OR criteria
        enrCriteriaEnrOrUdt.addOrCriteria(enrCriteriaUdt);

        // create sub query of student oids from OR criteria
        SubQuery enrSubQueryRptTypeSec = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, enrCriteriaEnrOrUdt);

        return enrSubQueryRptTypeSec;
    }

    /**
     * @see com.x2dev.reports.on.EnrRegRptHelperFte16#loadPreEnrolmentData()
     *
     *      Override to load student fte user defined table
     */
    @Override
    protected void loadPreEnrolmentData() {
        super.loadPreEnrolmentData();

        // call load UDC table to get subquery even if student summary report not
        // requested as
        // change of dates is still needed
        loadStudentFteUdcByDateTable();

        // load student fte records by student
        if (m_writeGridSummaryFteStd) {
            loadStudentFteUddByMonthTable();
        }
    }

    /**
     * Load student fte user defined table udc with mins/fte values by date Also
     * loads sub query of at least one record for register type
     */
    protected void loadStudentFteUdcByDateTable() {
        ExtendedDataDictionary ddxStudentFte = getBroker().getBeanByOid(ExtendedDataDictionary.class,
                OntarioAlias.EXT_OID_STD_FTE);
        DataDictionary dictionaryStudentFte = DataDictionary.getDistrictDictionary(ddxStudentFte,
                getBroker().getPersistenceKey());

        String fieldStdFteUdcSchoolYear = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDC_SCHOOL_YEAR, false,
                dictionaryStudentFte);
        String fieldStdFteUdcDate = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDC_DATE, false,
                dictionaryStudentFte);
        String fieldStdFteUdcRegister = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDC_REG, false,
                dictionaryStudentFte);
        String fieldStdFteUdcMins = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDC_MINS, false,
                dictionaryStudentFte);
        String fieldStdFteUdcMinsHc = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDC_MINS_HC, false,
                dictionaryStudentFte);
        String fieldStdFteUdcFte = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDC_FTE, false,
                dictionaryStudentFte);
        String fieldStdFteUdcFteHc = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDC_FTE_HC, false,
                dictionaryStudentFte);

        String rptRegType = CONST_FTE_REG_TYPE_TO_RPT_TYPE_MAP.get(m_rptType);

        // create criteria, add condition for school, school year
        X2Criteria udcCriteria = new X2Criteria();
        udcCriteria.addEqualTo(UserDefinedTableC.COL_SCHOOL_OID, m_school.getOid());
        udcCriteria.addEqualTo(fieldStdFteUdcSchoolYear, getCurrentContext().getContextId());

        // add condition for context dates
        udcCriteria.addGreaterOrEqualThan(fieldStdFteUdcDate, m_startDate);
        // use end date, 1 has been subtracted if last day of school as that should not
        // be included
        udcCriteria.addLessOrEqualThan(fieldStdFteUdcDate, m_endDate);

        // create register criteria based on input report type
        X2Criteria udcCriteriaRptType = new X2Criteria();
        if (m_rptType.equals(CONST_RPT_TYPE_FT)) {
            udcCriteriaRptType.addEqualTo(fieldStdFteUdcRegister, OntarioAlias.CONST_STD_FTE_REG_FT);
        } else if (m_rptType.equals(CONST_RPT_TYPE_PT)) {
            udcCriteriaRptType.addEqualTo(fieldStdFteUdcRegister, OntarioAlias.CONST_STD_FTE_REG_PT);
        }

        // create sub query of student oids with at least one enrolment of report type
        // in school year
        m_udtSubQueryRptTypeSec = new SubQuery(UserDefinedTableC.class, UserDefinedTableC.COL_STUDENT_OID,
                udcCriteriaRptType);

        // add condition based on report type/register
        if (m_udtSubQueryRptTypeSec != null) {
            udcCriteria.addIn(UserDefinedTableC.COL_STUDENT_OID, m_udtSubQueryRptTypeSec);
        }

        // create query and sort by student oid, month
        QueryByCriteria udcQuery = new QueryByCriteria(UserDefinedTableC.class, udcCriteria);
        udcQuery.addOrderByAscending(UserDefinedTableC.COL_STUDENT_OID);
        udcQuery.addOrderByAscending(fieldStdFteUdcDate);

        // query iterator for loop through records
        QueryIterator<X2BaseBean> udcs = getBroker().getIteratorByQuery(udcQuery);

        // create and initialize student level variables
        String stdOidPrev = null;
        String regPrevForStd = null;

        // loop through records
        // declare variable used in catch exception
        UserDefinedTableC udc = null;
        m_minsAndFteFromDbInfoByStdOid = new HashMap<String, Map<String, Object>>();
        m_regChgInfoOnePerRegTypeChgByStdOid = new HashMap<String, List<Map<String, Object>>>();
        m_regChgInfoAllRegTypeChgByStdOid = new HashMap<String, List<Map<String, Object>>>();
        List<Map<String, Object>> regChgInfoOnePerRegTypeChgForStdOid = new ArrayList<Map<String, Object>>();
        List<Map<String, Object>> regChgInfoAllRegTypeChgForStdOid = new ArrayList<Map<String, Object>>();
        try {
            while (udcs.hasNext()) {
                // Get UserDefinedTableD
                udc = (UserDefinedTableC) udcs.next();

                // get variables
                String stdOid = udc.getStudentOid();
                String date = (String) udc.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDC_DATE,
                        dictionaryStudentFte);
                String reg = (String) udc.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDC_REG,
                        dictionaryStudentFte);

                // get or create output map for student
                if ((!StringUtils.isEmpty(stdOidPrev)) && (!stdOid.equals(stdOidPrev))) {
                    m_regChgInfoOnePerRegTypeChgByStdOid.put(stdOidPrev, regChgInfoOnePerRegTypeChgForStdOid);
                    m_regChgInfoAllRegTypeChgByStdOid.put(stdOidPrev, regChgInfoAllRegTypeChgForStdOid);
                    regChgInfoOnePerRegTypeChgForStdOid = new ArrayList<Map<String, Object>>();
                    regChgInfoAllRegTypeChgForStdOid = new ArrayList<Map<String, Object>>();

                    regPrevForStd = null;
                }
                stdOidPrev = stdOid;

                // process udc
                if (!StringUtils.isEmpty(date)) {
                    // set date based variables
                    PlainDate regChgDate = PlainDate.fromString(date);
                    m_cal.setTime(regChgDate);
                    Integer regChgCalMonth = Integer.valueOf(m_cal.get(Calendar.MONTH) + 1);
                    Integer regChgSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(regChgCalMonth);

                    // save reg change if different reg
                    if ((regPrevForStd == null) || ((!StringUtils.isEmpty(reg)) && (!reg.equals(regPrevForStd)))) {

                        // save reg change info
                        Map<String, Object> regChgInfoOnePerRegTypeChg = new HashMap<String, Object>();
                        regChgInfoOnePerRegTypeChg.put(CONST_STD_REG_REG_TYPE, reg);
                        regChgInfoOnePerRegTypeChg.put(CONST_STD_REG_CHG_DT, regChgDate);
                        regChgInfoOnePerRegTypeChg.put(CONST_STD_REG_CHG_SKL_MONTH, regChgSklMonth);
                        regChgInfoOnePerRegTypeChgForStdOid.add(regChgInfoOnePerRegTypeChg);

                        regPrevForStd = reg;
                    }

                    // save reg change - saved for all dates found as student may have left and
                    // returned
                    Map<String, Object> regChgInfoAllRegTypeChg = new HashMap<String, Object>();
                    regChgInfoAllRegTypeChg.put(CONST_STD_REG_REG_TYPE, reg);
                    regChgInfoAllRegTypeChg.put(CONST_STD_REG_CHG_DT, regChgDate);
                    regChgInfoAllRegTypeChg.put(CONST_STD_REG_CHG_SKL_MONTH, regChgSklMonth);
                    regChgInfoAllRegTypeChgForStdOid.add(regChgInfoAllRegTypeChg);

                    // get mins and fte from input
                    // initialize to null
                    Integer regMins = null;
                    Integer regMinsHc = null;
                    Double regFte = null;
                    Double regFteHc = null;

                    // get values from record if is for report reg type otherwise leave as null
                    if ((!StringUtils.isEmpty(reg)) && (reg.equals(rptRegType))) {
                        String regMinsStr = (String) udc.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDC_MINS,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(regMinsStr)) {
                            regMins = Integer.valueOf(Double.valueOf(regMinsStr).intValue());
                        }
                        String regMinsHcStr = (String) udc.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDC_MINS_HC,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(regMinsHcStr)) {
                            regMinsHc = Integer.valueOf(Double.valueOf(regMinsHcStr).intValue());
                        }
                        String regFteStr = (String) udc.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDC_FTE,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(regFteStr)) {
                            regFte = Double.valueOf(regFteStr);
                        }
                        String regFteHcStr = (String) udc.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDC_FTE_HC,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(regFteHcStr)) {
                            regFteHc = Double.valueOf(regFteHcStr);
                        }
                    }

                    // save in output map for student
                    Integer stdMinsOct = null;
                    Integer stdMinsMar = null;
                    Integer stdMinsHcOct = null;
                    Integer stdMinsHcMar = null;
                    Double stdFteOct = null;
                    Double stdFteMar = null;
                    Double stdFteHcOct = null;
                    Double stdFteHcMar = null;

                    // get mins and fte if saved already
                    Map<String, Object> minsAndFteInfoForStdOid = null;
                    if (m_minsAndFteFromDbInfoByStdOid.get(stdOid) != null) {
                        minsAndFteInfoForStdOid = m_minsAndFteFromDbInfoByStdOid.get(stdOid);
                        stdMinsOct = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_OCT);
                        stdMinsMar = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_MAR);
                        stdMinsHcOct = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_HC_OCT);
                        stdMinsHcMar = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_HC_MAR);
                        stdFteOct = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_OCT);
                        stdFteMar = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_MAR);
                        stdFteHcOct = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_HC_OCT);
                        stdFteHcMar = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_HC_MAR);
                    }

                    // set values from reg chg record
                    if (regChgSklMonth.intValue() <= OntarioAlias.CONST_SKL_MONTH_MAR) {
                        if (regChgSklMonth.intValue() <= OntarioAlias.CONST_SKL_MONTH_OCT) {
                            stdMinsOct = regMins;
                            stdFteOct = regFte;
                            stdMinsMar = regMins;
                            stdFteMar = regFte;
                            // reset hc to null as maybe set from prior record
                            stdMinsHcOct = null;
                            stdFteHcOct = null;
                            stdMinsHcMar = null;
                            stdFteHcMar = null;
                            if ((regMinsHc != null) && (regMinsHc > CONST_ZERO_DBL)) {
                                stdMinsHcOct = regMinsHc;
                                stdFteHcOct = regFteHc;
                                stdMinsHcMar = regMinsHc;
                                stdFteHcMar = regFteHc;
                            }
                        } else {
                            stdMinsMar = regMins;
                            stdFteMar = regFte;
                            // reset hc to null as maybe set from prior record
                            stdMinsHcMar = null;
                            stdFteHcMar = null;
                            if ((regMinsHc != null) && (regMinsHc > CONST_ZERO_DBL)) {
                                stdMinsHcMar = regMinsHc;
                                stdFteHcMar = regFteHc;
                            }
                        }
                    }

                    // save to map
                    if (minsAndFteInfoForStdOid == null) {
                        minsAndFteInfoForStdOid = new HashMap<String, Object>();
                    }
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_OCT, stdMinsOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_MAR, stdMinsMar);
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_HC_OCT, stdMinsHcOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_HC_MAR, stdMinsHcMar);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_OCT, stdFteOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_MAR, stdFteMar);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_HC_OCT, stdFteHcOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_HC_MAR, stdFteHcMar);
                    m_minsAndFteFromDbInfoByStdOid.put(stdOid, minsAndFteInfoForStdOid);
                }
            }
        } catch (Exception e) {
            logToolMessage(Level.INFO,
                    "ERROR: " + udc.getStudent().getNameView() + "-date-"
                            + udc.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDC_DATE, dictionaryStudentFte)
                            + ";ExeptionMessage:" + e.toString(),
                            false);
        }

        finally {
            udcs.close();
        }

        // process last student
        if (!StringUtils.isEmpty(stdOidPrev)) {
            m_regChgInfoOnePerRegTypeChgByStdOid.put(stdOidPrev, regChgInfoOnePerRegTypeChgForStdOid);
            m_regChgInfoAllRegTypeChgByStdOid.put(stdOidPrev, regChgInfoAllRegTypeChgForStdOid);
        }
    }

    /**
     * Load student fte user defined table udd with mins/fte averages by month Also
     * loads sub query of at least one record for register type
     */
    protected void loadStudentFteUddByMonthTable() {
        ExtendedDataDictionary ddxStudentFte = getBroker().getBeanByOid(ExtendedDataDictionary.class,
                OntarioAlias.EXT_OID_STD_FTE);
        DataDictionary dictionaryStudentFte = DataDictionary.getDistrictDictionary(ddxStudentFte,
                getBroker().getPersistenceKey());

        String fieldStdFteUddSchoolYear = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_SCHOOL_YEAR, false,
                dictionaryStudentFte);
        String fieldStdFteUddDate = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_DATE, false,
                dictionaryStudentFte);
        String fieldStdFteUddMonth = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_MONTH, false,
                dictionaryStudentFte);
        String fieldStdFteUddRegister = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_REG, false,
                dictionaryStudentFte);
        String fieldStdFteUddMins = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_MINS, false,
                dictionaryStudentFte);
        String fieldStdFteUddFte = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_FTE, false,
                dictionaryStudentFte);
        String fieldStdFteUddFteHc = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_FTE_HC, false,
                dictionaryStudentFte);
        String fieldStdFteUddResOpCode = getBeanPathFromAlias(OntarioAlias.ALIAS_STD_FTE_UDD_RESIDENCY_OP_CODE, false,
                dictionaryStudentFte);

        String rptRegType = CONST_FTE_REG_TYPE_TO_RPT_TYPE_MAP.get(m_rptType);

        // create criteria, add condition for school, school year
        X2Criteria uddCriteria = new X2Criteria();
        uddCriteria.addEqualTo(UserDefinedTableD.COL_SCHOOL_OID, m_school.getOid());
        uddCriteria.addEqualTo(fieldStdFteUddSchoolYear, getCurrentContext().getContextId());

        // add condition for context dates for summary fte report
        uddCriteria.addGreaterOrEqualThan(fieldStdFteUddDate, m_startDateSummaryFte);
        // use end date, 1 has been subtracted if last day of school as that should not
        // be included
        uddCriteria.addLessOrEqualThan(fieldStdFteUddDate, m_endDateSummaryFte);

        // create sub query of student oids for this query
        m_udtSubQueryRptTypeSec = new SubQuery(UserDefinedTableD.class, UserDefinedTableD.COL_STUDENT_OID, uddCriteria);

        // there is no condition based on enrolment or fte changes from UDC table, only
        // on this udd table
        // a student could be FT on this monthly user table and PT on the regular table
        // and vice-versa

        // add sort by student name, state id (oen) if duplicates
        QueryByCriteria uddQuery = new QueryByCriteria(UserDefinedTableD.class, uddCriteria);
        uddQuery.addOrderByAscending(UserDefinedTableD.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PERSON
                + PATH_DELIMITER + m_fieldPsnLegalLastName);
        uddQuery.addOrderByAscending(UserDefinedTableD.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_PERSON
                + PATH_DELIMITER + m_fieldPsnLegalFirstName);
        uddQuery.addOrderByAscending(UserDefinedTableD.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);

        // add sort ascending by month
        uddQuery.addOrderByAscending(fieldStdFteUddMonth);

        // query iterator for loop through records
        QueryIterator<X2BaseBean> udds = getBroker().getIteratorByQuery(uddQuery);

        // loop through records
        // declare variable used in catch exception
        UserDefinedTableD udd = null;
        m_minsAndFteAvgInfoByStdOid = new HashMap<String, Map<String, Object>>();
        try {
            while (udds.hasNext()) {
                // Get UserDefinedTableD
                udd = (UserDefinedTableD) udds.next();

                // get variables
                String stdOid = udd.getStudentOid();
                String date = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_DATE,
                        dictionaryStudentFte);
                String reg = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_REG,
                        dictionaryStudentFte);
                String opCode = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_RESIDENCY_OP_CODE,
                        dictionaryStudentFte);

                // add to stdOids
                if (!m_stdOidsInOrder.contains(stdOid)) {
                    m_stdOidsInOrder.add(stdOid);
                    m_stdByStdOid.put(stdOid, udd.getStudent());
                }

                // process udd
                if (!StringUtils.isEmpty(date)) {
                    // set date based variables
                    PlainDate uddDate = PlainDate.fromString(date);
                    m_cal.setTime(uddDate);
                    Integer uddCalMonth = Integer.valueOf(m_cal.get(Calendar.MONTH) + 1);
                    Integer uddSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP.get(uddCalMonth);

                    // get mins and fte from input
                    // initialize to null
                    Integer avgMins = null;
                    Integer avgMinsHc = null;
                    Double avgFte = null;
                    Double avgFteHc = null;

                    // get values from record if is for report reg type otherwise leave as null
                    if ((!StringUtils.isEmpty(reg)) && (reg.equals(rptRegType))) {
                        // get mins/fte
                        String avgMinsStr = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_MINS,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(avgMinsStr)) {
                            avgMins = Integer.valueOf(Double.valueOf(avgMinsStr).intValue());
                        }
                        String avgMinsHcStr = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_MINS_HC,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(avgMinsHcStr)) {
                            avgMinsHc = Integer.valueOf(Double.valueOf(avgMinsHcStr).intValue());
                        }
                        String avgFteStr = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_FTE,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(avgFteStr)) {
                            avgFte = Double.valueOf(avgFteStr);
                        }
                        String avgFteHcStr = (String) udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_FTE_HC,
                                dictionaryStudentFte);
                        if (!StringUtils.isEmpty(avgFteHcStr)) {
                            avgFteHc = Double.valueOf(avgFteHcStr);
                        }
                    }

                    // get or initialize op ind values for student
                    Boolean stdPbIndOct = null;
                    Boolean stdOpIndOct = null;
                    Boolean stdPbIndMar = null;
                    Boolean stdOpIndMar = null;
                    Map<Integer, Boolean> stdPbIndByMthForStdOid = m_stdPbIndByMthByStdOid.get(stdOid);
                    if (stdPbIndByMthForStdOid == null) {
                        stdPbIndByMthForStdOid = new HashMap<Integer, Boolean>();
                    } else {
                        stdPbIndOct = stdPbIndByMthForStdOid.get(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_OCT));
                        stdPbIndMar = stdPbIndByMthForStdOid.get(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_MAR));
                    }
                    Map<Integer, Boolean> stdOpIndByMthForStdOid = m_stdOpIndByMthByStdOid.get(stdOid);
                    if (stdOpIndByMthForStdOid == null) {
                        stdOpIndByMthForStdOid = new HashMap<Integer, Boolean>();
                    } else {
                        stdOpIndOct = stdOpIndByMthForStdOid.get(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_OCT));
                        stdOpIndMar = stdOpIndByMthForStdOid.get(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_MAR));
                    }

                    // save mins/fte in output map for student
                    Integer stdMinsOct = null;
                    Integer stdMinsMar = null;
                    Integer stdMinsHcOct = null;
                    Integer stdMinsHcMar = null;
                    Double stdFteOct = null;
                    Double stdFteMar = null;
                    Double stdFteHcOct = null;
                    Double stdFteHcMar = null;

                    // get mins and fte if saved already
                    Map<String, Object> minsAndFteInfoForStdOid = null;
                    if (m_minsAndFteAvgInfoByStdOid.get(stdOid) != null) {
                        minsAndFteInfoForStdOid = m_minsAndFteAvgInfoByStdOid.get(stdOid);
                        stdMinsOct = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_OCT);
                        stdMinsMar = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_MAR);
                        stdMinsHcOct = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_HC_OCT);
                        stdMinsHcMar = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_HC_MAR);
                        stdFteOct = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_OCT);
                        stdFteMar = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_MAR);
                        stdFteHcOct = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_HC_OCT);
                        stdFteHcMar = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_HC_MAR);
                    }

                    // set values from the record
                    if ((uddSklMonth.intValue() >= OntarioAlias.CONST_SKL_MONTH_OCT)
                            && (uddSklMonth.intValue() < OntarioAlias.CONST_SKL_MONTH_MAR)) {
                        stdMinsOct = avgMins;
                        stdFteOct = avgFte;
                        // reset hc to null as maybe set from prior record
                        stdMinsHcOct = null;
                        stdFteHcOct = null;
                        if ((avgMinsHc != null) && (avgMinsHc > CONST_ZERO_DBL)) {
                            stdMinsHcOct = avgMinsHc;
                            stdFteHcOct = avgFteHc;
                        }

                        // set op/pb indicator
                        if ((!StringUtils.isEmpty(opCode)) && (OntarioAlias.ENR_OTHER_PUPIL_CODES.contains(opCode))) {
                            stdOpIndOct = Boolean.TRUE;
                        } else {
                            stdPbIndOct = Boolean.TRUE;
                        }
                    } else if (uddSklMonth.intValue() >= OntarioAlias.CONST_SKL_MONTH_MAR) {
                        stdMinsMar = avgMins;
                        stdFteMar = avgFte;
                        // reset hc to null as maybe set from prior record
                        stdMinsHcMar = null;
                        stdFteHcMar = null;
                        if ((avgMinsHc != null) && (avgMinsHc > CONST_ZERO_DBL)) {
                            stdMinsHcMar = avgMinsHc;
                            stdFteHcMar = avgFteHc;
                        }

                        // set op/pb indicator
                        if ((!StringUtils.isEmpty(opCode)) && (OntarioAlias.ENR_OTHER_PUPIL_CODES.contains(opCode))) {
                            stdOpIndMar = Boolean.TRUE;
                        } else {
                            stdPbIndMar = Boolean.TRUE;
                        }
                    }

                    // save mins/fte to map
                    if (minsAndFteInfoForStdOid == null) {
                        minsAndFteInfoForStdOid = new HashMap<String, Object>();
                    }
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_OCT, stdMinsOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_MAR, stdMinsMar);
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_HC_OCT, stdMinsHcOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_MINS_HC_MAR, stdMinsHcMar);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_OCT, stdFteOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_MAR, stdFteMar);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_HC_OCT, stdFteHcOct);
                    minsAndFteInfoForStdOid.put(CONST_STD_FTE_HC_MAR, stdFteHcMar);
                    m_minsAndFteAvgInfoByStdOid.put(stdOid, minsAndFteInfoForStdOid);

                    // save op ind values for student
                    stdPbIndByMthForStdOid.put(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_OCT), stdPbIndOct);
                    stdPbIndByMthForStdOid.put(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_MAR), stdPbIndMar);
                    m_stdPbIndByMthByStdOid.put(stdOid, stdPbIndByMthForStdOid);
                    stdOpIndByMthForStdOid.put(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_OCT), stdOpIndOct);
                    stdOpIndByMthForStdOid.put(Integer.valueOf(OntarioAlias.CONST_SKL_MONTH_MAR), stdOpIndMar);
                    m_stdOpIndByMthByStdOid.put(stdOid, stdOpIndByMthForStdOid);
                }
            }
        } catch (Exception e) {
            logToolMessage(Level.INFO,
                    "ERROR: " + udd.getStudent().getNameView() + "-month-"
                            + udd.getFieldValueByAlias(OntarioAlias.ALIAS_STD_FTE_UDD_MONTH, dictionaryStudentFte)
                            + ";ExeptionMessage:" + e.toString(),
                            false);
        }

        finally {
            udds.close();
        }
    }

    /**
     * @see com.x2dev.reports.on.EnrRegRptHelperFte16#accumEnrMinsFte(StudentEnrollment,
     *      String, Integer)
     *
     *      Override to reset to null for withdrawal records
     */
    @Override
    protected void accumEnrMinsFte(String stdOid,
                                   Map<String, Object> enrInfo,
                                   Map<String, Object> enrRec,
                                   String enrRegType,
                                   Integer enrSklMonth,
                                   boolean saveEntry,
                                   boolean saveWithdraw,
                                   Map<String, Map<String, Object>> minsAndFteInfoByStdOid,
                                   Map<String, Map<String, Object>> minsAndFteFromDbInfoByStdOid) {
        if (saveWithdraw) {
            // initialize variables
            SisStudent std = m_stdByStdOid.get(stdOid);
            String enrGradeLvl = (String) enrInfo.get(CONST_ENR_STD_GRADE_LVL);
            Integer stdMinsOct = null;
            Integer stdMinsMar = null;
            Integer stdMinsHcOct = null;
            Integer stdMinsHcMar = null;
            Double stdFteOct = null;
            Double stdFteMar = null;
            Double stdFteHcOct = null;
            Double stdFteHcMar = null;

            // get mins and fte if saved already
            Map<String, Object> minsAndFteInfoForStdOid = null;
            if (minsAndFteInfoByStdOid.get(stdOid) != null) {
                minsAndFteInfoForStdOid = minsAndFteInfoByStdOid.get(stdOid);
                stdMinsOct = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_OCT);
                stdMinsMar = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_MAR);
                stdMinsHcOct = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_HC_OCT);
                stdMinsHcMar = (Integer) minsAndFteInfoForStdOid.get(CONST_STD_MINS_HC_MAR);
                stdFteOct = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_OCT);
                stdFteMar = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_MAR);
                stdFteHcOct = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_HC_OCT);
                stdFteHcMar = (Double) minsAndFteInfoForStdOid.get(CONST_STD_FTE_HC_MAR);
            }

            // Only to set to null (depending on school month) for withdrawal
            Integer enrMins = null;
            Integer enrMinsHc = null;
            Double enrFte = null;
            Double enrFteHc = null;

            // save minutes and fte
            if (enrSklMonth.intValue() <= OntarioAlias.CONST_SKL_MONTH_MAR) {
                if (enrSklMonth.intValue() <= OntarioAlias.CONST_SKL_MONTH_OCT) {
                    stdMinsOct = enrMins;
                    stdFteOct = enrFte;
                    stdMinsMar = enrMins;
                    stdFteMar = enrFte;

                    // if additional high credit
                    if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryStdHcAdult)) {
                        stdMinsHcOct = enrMinsHc;
                        stdFteHcOct = enrFteHc;
                        stdMinsHcMar = enrMinsHc;
                        stdFteHcMar = enrFteHc;
                    }
                } else {
                    stdMinsMar = enrMins;
                    stdFteMar = enrFte;

                    // if additional high credit
                    if ((m_writeGridSummaryStdHc) || (m_writeGridSummaryStdHcAdult)) {
                        stdMinsHcMar = enrMinsHc;
                        stdFteHcMar = enrFteHc;
                    }
                }
            }

            // save to map
            if (minsAndFteInfoForStdOid == null) {
                minsAndFteInfoForStdOid = new HashMap<String, Object>();
            }
            minsAndFteInfoForStdOid.put(CONST_STD_MINS_OCT, stdMinsOct);
            minsAndFteInfoForStdOid.put(CONST_STD_MINS_MAR, stdMinsMar);
            minsAndFteInfoForStdOid.put(CONST_STD_MINS_HC_OCT, stdMinsHcOct);
            minsAndFteInfoForStdOid.put(CONST_STD_MINS_HC_MAR, stdMinsHcMar);
            minsAndFteInfoForStdOid.put(CONST_STD_FTE_OCT, stdFteOct);
            minsAndFteInfoForStdOid.put(CONST_STD_FTE_MAR, stdFteMar);
            minsAndFteInfoForStdOid.put(CONST_STD_FTE_HC_OCT, stdFteHcOct);
            minsAndFteInfoForStdOid.put(CONST_STD_FTE_HC_MAR, stdFteHcMar);
            minsAndFteInfoByStdOid.put(stdOid, minsAndFteInfoForStdOid);

            if (m_addLogs) {
                Double sumFteOctLogs = (stdFteOct == null) ? CONST_ZERO_DBL : stdFteOct;
                Double sumFteHcOctLogs = (stdFteHcOct == null) ? CONST_ZERO_DBL : stdFteHcOct;
                Double sumFteMarLogs = (stdFteMar == null) ? CONST_ZERO_DBL : stdFteMar;
                Double sumFteHcMarLogs = (stdFteHcMar == null) ? CONST_ZERO_DBL : stdFteHcMar;
                if (!m_log1StdOidPrev.equals(stdOid)) {
                    m_log1StdOidPrev = stdOid;
                    m_log1StdCt++;
                }
                m_logMessageMapToLogType.get(CONST_LOG_1)
                .append(CONST_LOG_1 + CONST_COMMA + m_log1StdCt + CONST_COMMA + CONST_QUOTE + std.getNameView()
                + CONST_QUOTE + CONST_COMMA
                + (((enrInfo != null) && (enrInfo.get(CONST_ENR_DATE) != null))
                        ? ((PlainDate) enrInfo.get(CONST_ENR_DATE)).toString()
                                : "null")
                + CONST_COMMA + CONST_COMMA + CONST_COMMA
                + (sumFteOctLogs != null ? sumFteOctLogs.toString() : "null") + CONST_COMMA
                + (sumFteHcOctLogs != null ? sumFteHcOctLogs.toString() : "null") + CONST_COMMA
                + (sumFteMarLogs != null ? sumFteMarLogs.toString() : "null") + CONST_COMMA
                + (sumFteHcMarLogs != null ? sumFteHcMarLogs.toString() : "null") + CONST_COMMA
                + m_pBFteSumOct + CONST_COMMA + m_oPFteSumOct + CONST_COMMA + m_pBFteSumMar
                + CONST_COMMA + m_oPFteSumMar + CONST_COMMA + m_pBFteSumAdultOct + CONST_COMMA
                + m_oPFteSumAdultOct + CONST_COMMA + m_pBFteSumAdultMar + CONST_COMMA
                + m_oPFteSumAdultMar + CONST_COMMA + m_pBFteSumHcOct + CONST_COMMA + m_oPFteSumHcOct
                + CONST_COMMA + m_pBFteSumHcMar + CONST_COMMA + m_oPFteSumHcMar + CONST_COMMA
                + m_pBFteSumHcAdultOct + CONST_COMMA + m_oPFteSumHcAdultOct + CONST_COMMA
                + m_pBFteSumHcAdultMar + CONST_COMMA + m_oPFteSumHcAdultMar + CONST_NEWLINE);
            }
        }
        // fetch from backup if entry
        else {
            // get mins and fte from backup if exists
            if (minsAndFteFromDbInfoByStdOid.get(stdOid) != null) {
                Map<String, Object> minsAndFteFromDbInfoForStdOid = minsAndFteFromDbInfoByStdOid.get(stdOid);
                Map<String, Object> minsAndFteInfoForStdOid = new HashMap<String, Object>();
                if (minsAndFteFromDbInfoForStdOid != null) {
                    Set<String> minsAndFteMapKeys = minsAndFteFromDbInfoForStdOid.keySet();
                    for (String key : minsAndFteMapKeys) {
                        Object value = minsAndFteFromDbInfoForStdOid.get(key);
                        minsAndFteInfoForStdOid.put(key, value);
                    }
                    minsAndFteInfoByStdOid.put(stdOid, minsAndFteInfoForStdOid);
                }
            }
        }
    }
}
