/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepPerformanceLevel;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class SummaryOfPerformance extends BaseFormReportJavaSource {


    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private static final String PERF_LVL_TRANSITION_PLANNING = "Transition Planning";
    private static final String KEY_DICTIONARY = "dictionary";
    private static final String ALIAS_TRANS_ASSESS_CATEGOTY = "transAssess-categoty";
    private static final String KEY_TR_ASSESSMENT_MAP = "trAssessmentMap";
    private Map<String, IepPerformanceLevel> m_trAssessment;
    private static final long serialVersionUID = 1L;


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        IepData iepData = (IepData) getFormOwner();
        if (iepData != null) {
            m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
            fillStudentInformation(iepData);
            fillPerformanceLevel(iepData);
            addParameter(KEY_DICTIONARY, getDictionary());
        }
        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }


    /**
     * Fill performance level.
     *
     * @param iepData IepData
     */
    private void fillPerformanceLevel(IepData iepData) {
        m_trAssessment = new HashMap<String, IepPerformanceLevel>();
        for (IepPerformanceLevel performanceLevel : iepData.getIepPerformanceLevel()) {
            if (performanceLevel.getType() != null) {
                if (performanceLevel.getType().equals(PERF_LVL_TRANSITION_PLANNING)) {
                    String transitionCategory =
                            (String) performanceLevel.getFieldValueByAlias(ALIAS_TRANS_ASSESS_CATEGOTY,
                                    getDictionary());
                    if (transitionCategory != null) {
                        m_trAssessment.put(transitionCategory, performanceLevel);
                    }
                } else if (performanceLevel.getType().equals("Recommendation")) {
                    String recomendationCategory =
                            (String) performanceLevel.getFieldValueByAlias("transAssess-recom-area",
                                    getDictionary());
                    if (recomendationCategory != null) {
                        recomendationCategory += " RArea";
                        m_trAssessment.put(recomendationCategory, performanceLevel);
                    }
                } else {
                    m_trAssessment.put(performanceLevel.getType() + " Area", performanceLevel);
                }

            }


        }

        addParameter(KEY_TR_ASSESSMENT_MAP, m_trAssessment);
    }

    /**
     * Fill student information.
     *
     * @param iepData IepData
     */
    private void fillStudentInformation(IepData iepData) {

        Student student = iepData.getStudent();
        Person person = student.getPerson();
        Map<String, Object> studentInformation = new HashMap<String, Object>();
        m_ilSpedHelper.fillPhones(person, studentInformation);
        String eMail = person.getEmail01();
        eMail = (eMail == null || eMail.isEmpty()) ? person.getEmail02() : eMail;
        studentInformation.put("eMail", eMail);
        Map<String, String> disabilitiesMap = m_ilSpedHelper.getDisabilities(iepData, true);
        studentInformation.putAll(disabilitiesMap);
        addParameter("studentInfo", studentInformation);
        String recentMeetingData = getMostRecentMeeting(iepData);
        addParameter("recentMeetingData", recentMeetingData);
        addParameter("districtName", getOrganization().getName());
        Address addressBean = getOrganization().getAddress();
        String address = ((addressBean.getAddressLine01() == null ? "" : addressBean.getAddressLine01() + " ") +
                (addressBean.getAddressLine02() == null ? "" : addressBean.getAddressLine02() + " ") +
                (addressBean.getAddressLine03() == null ? "" : addressBean.getAddressLine03() + " "));
        addParameter("districtAddress", address);

    }

    /**
     * Gets the most recent meeting.
     *
     * @param iepData IepData
     * @return String
     */
    /*
     * Get the collection of meetings from the IepData.
     * With the collection, iterate through each meeting, determining which is
     * the latest meeting
     */
    private String getMostRecentMeeting(IepData iepData) {
        PlainDate latestMeetingDate = null;
        for (IepMeeting meeting : iepData.getIepMeeting()) {
            if (latestMeetingDate == null || meeting.getDate().after(latestMeetingDate)) {
                latestMeetingDate = meeting.getDate();
            }
        }

        /*
         * Correctly format the latestMeeting
         */
        String mostRecentMeeting = null;
        if (latestMeetingDate != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat("M/dd/yyyy");
            mostRecentMeeting = dateFormat.format(latestMeetingDate);
        }

        return mostRecentMeeting;
    }

}
