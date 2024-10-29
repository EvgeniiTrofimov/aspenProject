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
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;

/**
 * The Class NJSpedEvalPlanningData.
 */
public class NJSpedEvalPlanningData extends BaseFormReportJavaSource {
    private static final String ALIAS_ASSESSMENT_TYPE = "eval-assessment-type";

    private static final String CODE_PARENT_GUARDIAN = "Parent/Guardian";

    // Report fields
    private static final String FIELD_BLANK_TYPE = "blankType";
    private static final String FIELD_ASSESS_FUNC_DATA = "functionalData";
    private static final String FIELD_ASSESS_OTHER_DATA = "otherData";
    private static final String FIELD_ASSESS_REL_DATA = "relatedData";
    private static final String FIELD_ASSESS_STD_DATA = "standardData";
    private static final String FIELD_ASSESSMENT = "assessment";
    private static final String FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String FIELD_RECIPIENT_NAME = "recipientName";
    private static final String FIELD_RECIPIENT_ADDRESS_01 = "recipientAddress01";
    private static final String FIELD_RECIPIENT_CITY = "recipientCity";
    private static final String FIELD_RECIPIENT_STATE = "recipientState";
    private static final String FIELD_RECIPIENT_ZIP = "recipientZip";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_TEAM_MEMBER = "teamMember";

    // Report parameters
    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_PROC_EVAL_SUB_REPORT = "procEvalReport";
    private static final String PARAM_PROC_EVAL_SUBREPORT_ID = "procEvalSubreportId";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";

    private static final String TYPE_ASSESMENT_STANDARD = "Standardized Test";
    private static final String TYPE_ASSESMENT_FUNCTIONAL = "Functional Assessment";
    private static final String TYPE_ASSESMENT_RELATED = "Related Service";
    private static final String TYPE_ASSESMENT_OTHER = "Other";

    // Member variables
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_MeetingDictionary;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        // this allows blank form to be produced if requested
        Collection dummy = new ArrayList<X2BaseBean>();
        dummy.add(getOrganization());
        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_MeetingDictionary);

            if (m_currentIep.getStaff() != null &&
                    m_currentIep.getStaff().getPerson() != null) {
                String caseManager = getFullName(m_currentIep.getStaff().getPerson());
                addParameter(PARAM_CASE_MANAGER, caseManager);
            }

            Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();

            // print a notification for each parent
            // the report is completed in the detail band
            boolean hasGuardian = false;
            for (IepTeamMember teamMember : teamMembers) {
                if (CODE_PARENT_GUARDIAN.equals(teamMember.getMemberRoleCode()) && !hasGuardian) {
                    grid.append();
                    grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
                    addMeetingData(grid);
                    addNotificationRecipient(grid, teamMember);
                    addAssessmentData(grid);
                    hasGuardian = true;
                }
            }

            if (!hasGuardian) {
                grid.append();
                grid.set(FIELD_RECIPIENT_NAME, "*** No Guardian set for this Student's IEP ***");
                /**
                 * Even if the Student's IEP doesn't have a team member that has the Parent/Guardian
                 * role,
                 * procedural safeguard section has to get displayed.
                 */
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
                addMeetingData(grid);
            }
        }
        Report evalSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_EVAL_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_EVAL_SUB_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));
        Report safeSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(safeSubreport.getCompiledFormat()));

        addParameter(PARAM_IS_REDETERMINE_ELIG,
                getFormDefinition().getId().equals("SPED-NJ-REDETERMINE") ? Boolean.TRUE : Boolean.FALSE);

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
            addAssessmentData(grid);

            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));
            addAssessmentData(grid);
        }
        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize report, m_currentIep (IepData), m_meeting (GenericFormData), and
     * m_meetingDictionary.
     */
    @Override
    protected void initialize() {
        super.initialize();
        m_currentIep = (IepData) getFormOwner();
        m_meeting = (GenericFormData) getFormStorage();
        ExtendedDictionaryAttributes extendDictionary = m_meeting.getExtendedDataDictionary();
        m_MeetingDictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
        addParameter("dateAsStringConverter",
                ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true));

    }

    /**
     * Adds the assessment data.
     *
     * @param grid ReportDataGrid
     */
    private void addAssessmentData(ReportDataGrid grid) {
        ReportDataGrid standardGrid = new ReportDataGrid();
        ReportDataGrid functionalGrid = new ReportDataGrid();
        ReportDataGrid relatedGrid = new ReportDataGrid();
        ReportDataGrid otherGrid = new ReportDataGrid();

        if (m_meeting.getOid() == null) {
            standardGrid.append();
            functionalGrid.append();
            relatedGrid.append();
            otherGrid.append();
        } else {
            for (GenericFormChildData child : m_meeting.getGenericFormDataChildren()) {
                String type = (String) child.getFieldValueByAlias(ALIAS_ASSESSMENT_TYPE, m_MeetingDictionary);

                if (TYPE_ASSESMENT_STANDARD.equals(type)) {
                    standardGrid.append();
                    standardGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESMENT_FUNCTIONAL.equals(type)) {
                    functionalGrid.append();
                    functionalGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESMENT_RELATED.equals(type)) {
                    relatedGrid.append();
                    relatedGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESMENT_OTHER.equals(type)) {
                    otherGrid.append();
                    otherGrid.set(FIELD_ASSESSMENT, child);
                }

            }
        }
        standardGrid.beforeTop();
        functionalGrid.beforeTop();
        relatedGrid.beforeTop();
        otherGrid.beforeTop();

        grid.set(FIELD_ASSESS_STD_DATA, standardGrid);
        grid.set(FIELD_ASSESS_FUNC_DATA, functionalGrid);
        grid.set(FIELD_ASSESS_REL_DATA, relatedGrid);
        grid.set(FIELD_ASSESS_OTHER_DATA, otherGrid);
    }

    /**
     * Add meeting invitation recipient data to grid for display on report.
     *
     * @param grid ReportDataGrid
     * @param teamMember IepTeamMember
     */
    private void addNotificationRecipient(ReportDataGrid grid, IepTeamMember teamMember) {
        grid.set(FIELD_TEAM_MEMBER, teamMember);
        if (teamMember.getPerson() != null) {
            SisPerson person = teamMember.getPerson();
            SisAddress address = person.getPhysicalAddress();

            String recipientName = getFullName(person);
            grid.set(FIELD_RECIPIENT_NAME, recipientName);

            if (address == null) {
                address = person.getMailingAddress();
            }

            if (address != null) {
                grid.set(FIELD_RECIPIENT_ADDRESS_01, address.getAddressLine01());
                grid.set(FIELD_RECIPIENT_CITY, address.getCity());
                grid.set(FIELD_RECIPIENT_STATE, address.getState());
                grid.set(FIELD_RECIPIENT_ZIP, address.getPostalCode());
            }
        }
    }

    /**
     * Add meeting data to grid for display on report.
     *
     * @param grid ReportDataGrid
     */
    private void addMeetingData(ReportDataGrid grid) {
        if (m_currentIep.getStudent() != null &&
                m_currentIep.getStudent().getPerson() != null) {
            String studentName = getFullName(m_currentIep.getStudent().getPerson());
            grid.set(FIELD_STUDENT_NAME, studentName);
        }
    }

    /**
     * Get full name from a person bean.
     *
     * @param person SisPerson
     * @return fullName
     */
    private String getFullName(SisPerson person) {
        String fullName = "";

        fullName = fullName + person.getFirstName();
        if (!StringUtils.isEmpty(fullName)) {
            fullName = fullName + " ";
        }
        fullName = fullName + person.getLastName();

        return fullName;
    }
}
