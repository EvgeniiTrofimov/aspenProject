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

import com.x2dev.procedures.statereporting.on.register.original.OntarioAlias;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Enrollment Register Elementary Day (Full and Part time) for Ontario Add
 * Enrolment Register Helper as external data source
 *
 * @author Follett Software Company
 */
public class EnrRegDayElemFte16Data extends EnrRegRptHelperFte16 {
    private static final long serialVersionUID = 1L;

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
            if (reportType.equals(CONST_RPT_IN_ELEM_FT)) {
                m_sklLvl = CONST_SKL_LVL_ELEM;
                m_rptType = CONST_RPT_TYPE_FT;
            } else if (reportType.equals(CONST_RPT_IN_ELEM_PT)) {
                m_sklLvl = CONST_SKL_LVL_ELEM;
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
            if (gradeRange.equals(CONST_GRADE_RANGE_JK)) {
                m_gradeLvls.addAll(CONST_GRADE_LVLS_JK);
            } else if (gradeRange.equals(CONST_GRADE_RANGE_K)) {
                m_gradeLvls.addAll(CONST_GRADE_LVLS_K);
            } else if (gradeRange.equals(CONST_GRADE_RANGE_1_TO_3)) {
                m_gradeLvls.addAll(CONST_GRADE_LVLS_01_TO_03);
            } else if (gradeRange.equals(CONST_GRADE_RANGE_4_TO_8)) {
                m_gradeLvls.addAll(CONST_GRADE_LVLS_04_TO_08);
            } else if (gradeRange.equals(CONST_GRADE_RANGE_ALL)) {
                m_gradeLvls.addAll(CONST_GRADE_LVLS_JK);
                m_gradeLvls.addAll(CONST_GRADE_LVLS_K);
                m_gradeLvls.addAll(CONST_GRADE_LVLS_01_TO_03);
                m_gradeLvls.addAll(CONST_GRADE_LVLS_04_TO_08);
            }
        }
    }

    /**
     * @see com.x2dev.reports.on.EnrRegRptHelperFte16#accumEnrMinsFte(StudentEnrollment,
     *      String, Integer)
     *
     *      Override to get from enrollment records for this report Last parameter
     *      not used as only used for secondary registers.
     *
     *      - Resets to null for withdrawal records
     */
    @Override
    protected void accumEnrMinsFte(String stdOid, Map<String, Object> enrInfo, Map<String, Object> enrRec,
                                   String enrRegType, Integer enrSklMonth, boolean saveEntry, boolean saveWithdraw,
                                   Map<String, Map<String, Object>> minsAndFteInfoByStdOid,
                                   Map<String, Map<String, Object>> minsAndFteFromDbInfoByStdOid) {
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

        // Only change values for entry records, set to null (depending on school month)
        // for
        // withdrawal
        Integer enrMins = null;
        Integer enrMinsHc = null;
        Double enrFte = null;
        Double enrFteHc = null;
        if (saveEntry) {
            if (
                    // rpt type check
                    (((m_rptType.equals(CONST_RPT_TYPE_FT)) && (enrRegType.equals(OntarioAlias.ENR_REG_TYPE_FT)))
                            || ((m_rptType.equals(CONST_RPT_TYPE_PT)) && (enrRegType.equals(OntarioAlias.ENR_REG_TYPE_PT))))
                    && (m_gradeLvls.contains(enrGradeLvl))) {
                // initialize enr variables
                Integer enrMinsIn = null;

                // save minutes (will overlay with newer values)
                String enrMinsDb = (String) enrRec.get(CONST_ENR_MINS);
                enrMinsIn = CONST_ZERO;
                if ((!StringUtils.isEmpty(enrMinsDb)) && (enrMinsDb.matches(CONST_PATTERN_INT))) {
                    enrMinsIn = Integer.valueOf(enrMinsDb);
                }

                // save fte (regular) from enr if populated, otherwise get fte from minutes
                String enrFteDb = (String) enrRec.get(CONST_ENR_FTE);
                if ((!StringUtils.isEmpty(enrFteDb)) && (enrFteDb.matches(CONST_PATTERN_DOUBLE))) {
                    enrFte = Double.valueOf(enrFteDb);
                } else {
                    // calculate fte from minutes if not populated
                    // (should be populated, otherwise takes default constant for elem/sec)
                    enrFte = Double
                            .valueOf(enrMinsIn.doubleValue() / OntarioAlias.ENR_ELEM_CYCLE_MINS_TOT.doubleValue());
                }

                // save fte (hc) from enr if populated
                String enrFteHcDb = (String) enrRec.get(CONST_ENR_FTE_HC);
                if ((!StringUtils.isEmpty(enrFteHcDb)) && (enrFteHcDb.matches(CONST_PATTERN_DOUBLE))) {
                    enrFteHc = Double.valueOf(enrFteHcDb);
                }

                // overlay minutes by multiplying total student mins with fte if high credit fte
                // populated
                if ((enrFteHc != null) && (enrFteHc.doubleValue() > CONST_ZERO_DBL.doubleValue())) {
                    enrMins = Integer.valueOf(Double
                            .valueOf(Math
                                    .round(OntarioAlias.ENR_ELEM_CYCLE_MINS_TOT.doubleValue() * (enrFte.doubleValue())))
                            .intValue());
                    enrMinsHc = Integer.valueOf(enrMinsIn.intValue() - enrMins.intValue());
                } else {
                    enrMins = enrMinsIn;
                }
            }
        }

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
            + CONST_COMMA + CONST_COMMA + CONST_COMMA + sumFteOctLogs.toString() + CONST_COMMA
            + sumFteHcOctLogs.toString() + CONST_COMMA + sumFteMarLogs.toString() + CONST_COMMA
            + sumFteHcMarLogs.toString() + CONST_COMMA + m_pBFteSumOct + CONST_COMMA + m_oPFteSumOct
            + CONST_COMMA + m_pBFteSumMar + CONST_COMMA + m_oPFteSumMar + CONST_COMMA
            + m_pBFteSumAdultOct + CONST_COMMA + m_oPFteSumAdultOct + CONST_COMMA + m_pBFteSumAdultMar
            + CONST_COMMA + m_oPFteSumAdultMar + CONST_COMMA + m_pBFteSumHcOct + CONST_COMMA
            + m_oPFteSumHcOct + CONST_COMMA + m_pBFteSumHcMar + CONST_COMMA + m_oPFteSumHcMar
            + CONST_COMMA + m_pBFteSumHcAdultOct + CONST_COMMA + m_oPFteSumHcAdultOct + CONST_COMMA
            + m_pBFteSumHcAdultMar + CONST_COMMA + m_oPFteSumHcAdultMar + CONST_NEWLINE);
        }
    }
}
