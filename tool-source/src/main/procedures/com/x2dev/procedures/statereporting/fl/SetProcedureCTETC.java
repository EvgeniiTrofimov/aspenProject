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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.ValidateRegularExpression;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SchoolRoom;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SetProcedureCTETC.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class SetProcedureCTETC extends SetProcedure {

    private static final String ALIAS_CRS_STATE_ID = "all-crs-StateId";
    private static final String ALIAS_RMS_FACILITY_TYPE = "all-rms-FacilityType";

    private DataDictionaryField m_fieldStateId;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.procedures.statereporting.fl.SetProcedure#execute()
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
        super.initialize(FL_EXPORT.CTETC, SisStaff.class);

        m_fieldStateId = getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CRS_STATE_ID);

        adjustCertificationNumbers();

        initializeRule9Fix();
        initializeRule19Fix();
    }

    /**
     * Adjust certification numbers.
     */
    private void adjustCertificationNumbers() {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(StaffCertification.COL_CERTIFICATION_NUMBER, getBroker().getPersistenceKey());
        QueryByCriteria query = new QueryByCriteria(StaffCertification.class, criteria);
        Collection<StaffCertification> certifications = getBroker().getCollectionByQuery(query);
        for (StaffCertification cert : certifications) {
            String certNumber = cert.getCertificationNumber();
            if (!StringUtils.isEmpty(certNumber) && certNumber.length() < 10) {
                cert.setCertificationNumber(String.format("%010d", Integer.valueOf(certNumber)));
                getModelBroker().saveBean(cert);
            }
        }
    }

    /**
     * Initialize rule 9 fix.
     */
    private void initializeRule9Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Facility Type", "^([01][0-9]|20)$",
                        "Facility Type code must be in the range 00 to 20."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<ScheduleTeacher> schedules = getFLReportData().getStaffHelper().getTeacherSchedules(bean.getOid());
                            if (schedules != null) {
                                for (ScheduleTeacher info : schedules) {
                                    SchoolRoom room = info.getSection().getPrimaryRoom();
                                    room.setFieldValueByAlias(ALIAS_RMS_FACILITY_TYPE,
                                            String.valueOf(ThreadLocalRandom.current().nextInt(0, 21)));
                                    getModelBroker().saveBean(room);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }

                }));

        addFixesByRuleNumber("9", ruleWithFixes);
    }

    /**
     * Initialize rule 19 fix.
     */
    private void initializeRule19Fix() {
        ArrayList<RuleWithFix> ruleWithFixes = new ArrayList<>();
        ruleWithFixes.add(new RuleWithFix(
                new ValidateRegularExpression("Course Number", "^(?!M810015|M810016)\\S{7}$",
                        "Course Number cannot be = M810015 (Insurance Claims Adjustor) or M810016 (Insurance Customer Service Representative)."),
                new Fix() {

                    @Override
                    protected void fixError(X2BaseBean bean) {
                        try {
                            List<ScheduleTeacher> schedules = getFLReportData().getStaffHelper().getTeacherSchedules(bean.getOid());
                            if (schedules != null) {
                                for (ScheduleTeacher info : schedules) {
                                    Course course = info.getSection().getSchoolCourse().getCourse();
                                    course.setFieldValueByBeanPath(m_fieldStateId.getJavaName(), getRandomCodeForField(m_fieldStateId));

                                    getModelBroker().saveBean(course);
                                }
                            }
                        } catch (X2BaseException e) {
                            e.printStackTrace();
                        }
                    }

                }));

        addFixesByRuleNumber("19", ruleWithFixes);
    }
}
