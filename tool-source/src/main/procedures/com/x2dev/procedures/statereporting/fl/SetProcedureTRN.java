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

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentTransportation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureTRN.
 */
public class SetProcedureTRN extends SetProcedure {

    private static final String ALIAS_STD_STATE_ID = "all-std-StateId";
    private static final String ALIAS_STR_BUS_NUM = "all-str-BusNumber";
    private static final String ALIAS_STR_BUS_ROUTE = "all-str-BusRoute";
    private static final String ALIAS_STR_MEMB_CATEGORY = "all-str-MembershipCategory";
    private static final String ALIAS_STR_VEHICLE_CATEGORY = "all-str-VehicleCategory";

    protected Map<String, List<StudentTransportation>> m_strMap;
    private FLStudentHelper m_studentHelper = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        super.execute();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.TRN, SisStudent.class);

        m_studentHelper = getFLReportData().getStudentHelper();
        QueryByCriteria transporationQuery = new QueryByCriteria(StudentTransportation.class, getSTRCriteria());
        m_strMap =
                getBroker().getGroupedCollectionByQuery(transporationQuery, StudentTransportation.COL_STUDENT_OID, 100);

        initializeRule3Fix();
        initializeRule9Fix();
        initializeRule10Fix();
        initializeRule11Fix();
        initializeRule12Fix();
    }

    /**
     * Function for building custom Student Transportation criteria.
     *
     * @return criteria for query for list of active student transportations
     *         limited by report date range
     * @throws X2BaseException exception
     */
    private X2Criteria getSTRCriteria() throws X2BaseException {
        X2Criteria strCriteria = null;

        if (m_studentHelper != null) {
            strCriteria = new X2Criteria();
            strCriteria.addNotNull(StudentTransportation.COL_START_DATE);
            strCriteria.addLessOrEqualThan(StudentTransportation.COL_START_DATE,
                    getFLReportData().getSurveyPeriod().getEndDate());
            SubQuery stdSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentHelper.getStudentCriteria());
            strCriteria.addIn(StudentTransportation.COL_STUDENT_OID, stdSubQuery);
            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addIsNull(StudentTransportation.COL_END_DATE);
            X2Criteria orEndDateCriteria = new X2Criteria();
            orEndDateCriteria.addGreaterOrEqualThan(StudentTransportation.COL_END_DATE,
                    getFLReportData().getSurveyPeriod()
                            .getStartDate());
            endDateCriteria.addOrCriteria(orEndDateCriteria);
            strCriteria.addAndCriteria(endDateCriteria);
        }

        return strCriteria;
    }

    /**
     * Gets the student transportation.
     *
     * @param studentOid String
     * @return Student transportation
     */
    private StudentTransportation getStudentTransportation(String studentOid) {
        StudentTransportation transport = null;
        if (m_strMap.containsKey(studentOid) && !m_strMap.get(studentOid).isEmpty()) {
            transport = m_strMap.get(studentOid).get(0);
        }
        return transport;
    }

    /**
     * Initialize rule 10 fix.
     */
    private void initializeRule10Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Bus Route Number", "^\\S{1,15}$",
                        "Bus Route Number must not contain all spaces (blanks)."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        StudentTransportation transport = getStudentTransportation(student.getOid());
                        if (transport != null) {
                            transport.setFieldValueByAlias(ALIAS_STR_BUS_ROUTE, "1");
                            getBroker().saveBeanForced(transport);
                        }
                    }
                }));
        addFixesByRuleNumber("10", ruleWithFixes);
    }

    /**
     * Initialize rule 11 fix.
     */
    private void initializeRule11Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Vehicle Category", "^[BEPG]$",
                        "Vehicle Category must be B, E, P, or G."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        StudentTransportation transport = getStudentTransportation(student.getOid());
                        if (transport != null) {
                            transport.setFieldValueByAlias(ALIAS_STR_VEHICLE_CATEGORY, "B");
                            getBroker().saveBeanForced(transport);
                        }
                    }
                }));
        addFixesByRuleNumber("11", ruleWithFixes);
    }

    /**
     * Initialize rule 12 fix.
     */
    private void initializeRule12Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Tran Memb Category", "^[FGLMN]$",
                        "Transportation Membership Category must be F, G, L, M, or N."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        StudentTransportation transport = getStudentTransportation(student.getOid());
                        if (transport != null) {
                            transport.setFieldValueByAlias(ALIAS_STR_MEMB_CATEGORY, "N");
                            getBroker().saveBeanForced(transport);
                        }
                    }
                }));
        addFixesByRuleNumber("12", ruleWithFixes);
    }

    /**
     * Initialize rule 3 fix.
     */
    private void initializeRule3Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Student Number",
                        "^(?!(00|7[67]|[89]\\d{8})|0{3}\\d{6}X|\\d{9}[^\\dX])\\d{9}\\w$",
                        "Student Number, must be numeric, tenth position must either be an X or numeric."
                                + "If the tenth position numeric, the first two digits must be a valid "
                                + "district number in the range 01-75 or 78-79. "
                                + "If the tenth position is an X, the first three positions may not all be zeroes."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        String currentId = (String) bean.getFieldValueByAlias(ALIAS_STD_STATE_ID);
                        if (currentId == null) {
                            currentId = String.valueOf(ThreadLocalRandom.current().nextLong(1000000000l, 9999999999l));
                        }
                        currentId = StringUtils.padLeft(currentId, 10, '0');
                        String randomDistrict = "";
                        if (currentId.substring(9).matches("\\d")
                                && !currentId.substring(0, 2).matches("(?!(00|69|7[067]|[89]))\\d{2}")) {
                            while (StringUtils.isEmpty(randomDistrict)
                                    || !randomDistrict.matches("(?!(00|69|7[067]|[89]))\\d{2}")) {
                                randomDistrict = String.valueOf(ThreadLocalRandom.current().nextInt(79));
                            }
                        } else if (currentId.substring(9).matches("X")
                                && currentId.substring(0, 3).matches(("(?!(00|69|7[067]|[89])|0{3})\\d{3}"))) {
                            while (randomDistrict == null
                                    || !randomDistrict.matches("(?!(00|69|7[067]|[89])|0{3})\\d{3}")) {
                                randomDistrict = String.valueOf(ThreadLocalRandom.current().nextInt(999));
                            }
                        }
                        if (!StringUtils.isEmpty(randomDistrict)) {
                            currentId = randomDistrict + currentId.substring(2);
                        }
                        bean.setFieldValueByAlias(ALIAS_STD_STATE_ID, currentId);
                        getModelBroker().saveBean(bean);
                    }
                }));
        addFixesByRuleNumber("3", ruleWithFixes);
    }

    /**
     * Initialize rule 9 fix.
     */
    private void initializeRule9Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Bus Number", "^\\S{1,12}$",
                        "Bus Number must not contain all spaces (blanks)."),
                new Fix() {
                    @Override
                    protected void fixError(X2BaseBean bean) {
                        SisStudent student = (SisStudent) bean;
                        StudentTransportation transport = getStudentTransportation(student.getOid());
                        if (transport != null) {
                            transport.setFieldValueByAlias(ALIAS_STR_BUS_NUM, "1");
                            getBroker().saveBeanForced(transport);
                        }
                    }
                }));
        addFixesByRuleNumber("9", ruleWithFixes);
    }
}
