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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.GenericFormChildData;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NJSpedEvaluationPlanningReevalData.
 */
public class NJSpedEvaluationPlanningReevalData extends BaseFormReportJavaSource {
    private static final String ALIAS_ASSESSMENT_TYPE = "eval-assessment-type";
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "'DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";

    private static final String EMPTY_STRING = "";

    private static final String EXTN_WORK_PHONE = "(908) 284-";

    private static final String FAX = "FAX";

    private static final String FIELD_ASSESS_EDUCATIONAL_DATA = "educationalData";
    private static final String FIELD_ASSESS_FUNC_DATA = "functionalData";
    private static final String FIELD_ASSESS_OTHER_DATA = "otherData";
    private static final String FIELD_ASSESS_PSYCOLOGICAL_DATA = "psycologicalData";
    private static final String FIELD_ASSESS_REL_DATA = "relatedData";
    private static final String FIELD_ASSESS_SOCIAL_DATA = "socialData";
    private static final String FIELD_ASSESS_SPEECH_DATA = "speechData";
    private static final String FIELD_ASSESS_STD_DATA = "standardData";
    private static final String FIELD_ASSESSMENT = "assessment";
    private static final String FIELD_BLANK_TYPE = "blankType";
    private static final String FIELD_PROC_SAFE_DATA = "procSafeData";
    private static final String FIELD_RECIPIENT_ADDRESS_01 = "recipientAddress01";
    private static final String FIELD_RECIPIENT_CITY = "recipientCity";
    private static final String FIELD_RECIPIENT_NAME = "recipientName";
    private static final String FIELD_RECIPIENT_STATE = "recipientState";
    private static final String FIELD_RECIPIENT_ZIP = "recipientZip";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_TEAM_MEMBER = "teamMember";

    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_PHONE = "CHAIR_PERSON_PHONE";
    private static final String PARAM_CHAIR_PERSON_TITLE = "CHAIR_PERSON_TITLE";
    private static final String PARAM_CONSENT_LINE1 = "CONSENT_LINE1";
    private static final String PARAM_CONSENT_LINE2 = "CONSENT_LINE2";
    private static final String PARAM_DATASET_SUB = "DATASET_SUB";
    private static final String PARAM_DATASET_SUB2 = "DATASET_SUB2";
    private static final String PARAM_DIS_CONTACT_NUM = "schoolDisabilityContactNumber";
    private static final String PARAM_EVAL_REQ = "evalreq";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_MEETING_DICTIONARY = "meetingDictionary";
    private static final String PARAM_NOTICE_DETAILS = "NOTICE_DETAILS";
    private static final String PARAM_PARENT_COUNT = "parentCount";
    private static final String PARAM_PARENTADDY1 = "PARENTADDY1";
    private static final String PARAM_PARENTADDY2 = "PARENTADDY2";
    private static final String PARAM_PARENTNAME = "PARENTNAME";
    private static final String PARAM_PROC_EVAL_SUB_REPORT = "procEvalReport";
    private static final String PARAM_PROC_EVAL_SUBREPORT_ID = "procEvalSubreportId";
    private static final String PARAM_PROC_SAFE_REPORT = "procSafeReport";
    private static final String PARAM_PROC_SAFE_SUBREPORT_ID = "procSafeSubreportId";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_CONTACT_NAME = "schoolDisabilityContactName";
    private static final String PARAM_SCHOOL_CSTEAM = "schoolChildStudyTeam";
    private static final String PARAM_SCHOOL_CSTNUM = "schoolCSTNumber";
    private static final String PARAM_SCHOOL_DISRES = "schoolDisabilityResource";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE = "schoolPhoneNumber";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SCHOOL_SD = "schoolSpecialServicesDepartment";
    private static final String PARAM_SCHOOL_STREET = "schoolStreet";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SNAME = "SNAME";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_SUPERVISOR = "countySupervisor";
    private static final String PARAM_SUPERVISOR_NUM = "countySupervisorPhoneNumber";

    private static final String SUB_REPORT_PSS = "subID";
    private static final String SUB_REPORT_PSS2 = "subID2";

    private static final String TYPE_ASSESMENT_EDUCATIONAL = "Educational";
    private static final String TYPE_ASSESMENT_FUNCTIONAL = "Functional Assessment";
    private static final String TYPE_ASSESMENT_OTHER = "Other";
    private static final String TYPE_ASSESMENT_PSYCOLOGICAL = "Psychological";
    private static final String TYPE_ASSESMENT_RELATED = "Related Service";
    private static final String TYPE_ASSESMENT_SOCIAL = "Social";
    private static final String TYPE_ASSESMENT_SPEECH = "Speech/Language";
    private static final String TYPE_ASSESMENT_STANDARD = "Standardized Test";

    // Member variables
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_meetingDictionary;
    private String m_sssStaffName;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        String letterRecp = "Parent";
        if ((String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("parent-version",
                getDictionary())) != null) {
            letterRecp = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("parent-version",
                    getDictionary()));
        }

        addParameter("letterRecp", letterRecp);

        int parentCount = 0;
        String evalReq = (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("eval-evaluation",
                getDictionary()));
        addParameter(PARAM_EVAL_REQ, evalReq);

        ReportDataGrid grid = new ReportDataGrid();
        Report rubricSubreport = ReportUtils.getReport((String) getParameter(SUB_REPORT_PSS), getBroker());
        addParameter("SUB_REPORT_PSS", new ByteArrayInputStream(rubricSubreport.getCompiledFormat()));

        Report rubricSubreport2 = ReportUtils.getReport((String) getParameter(SUB_REPORT_PSS2), getBroker());
        addParameter("SUB_REPORT_PSS2", new ByteArrayInputStream(rubricSubreport2.getCompiledFormat()));

        ReportDataGrid subReportGrid = new ReportDataGrid();
        subReportGrid.append();

        subReportGrid.set(PARAM_SCHOOL_NAME, getParameter(PARAM_SCHOOL_NAME));
        subReportGrid.set(PARAM_SCHOOL_SD, getParameter(PARAM_SCHOOL_SD));
        subReportGrid.set(PARAM_SCHOOL_STREET, getParameter(PARAM_SCHOOL_STREET));
        subReportGrid.set(PARAM_SCHOOL_PHONE, getParameter(PARAM_SCHOOL_PHONE));
        subReportGrid.set(PARAM_SCHOOL_CSTEAM, getParameter(PARAM_SCHOOL_CSTEAM));
        subReportGrid.set(PARAM_SCHOOL_CSTNUM, getParameter(PARAM_SCHOOL_CSTNUM));
        subReportGrid.set(PARAM_SCHOOL_DISRES, getParameter(PARAM_SCHOOL_DISRES));
        subReportGrid.set(PARAM_SCHOOL_CONTACT_NAME, getParameter(PARAM_SCHOOL_CONTACT_NAME));
        subReportGrid.set(PARAM_DIS_CONTACT_NUM, getParameter(PARAM_DIS_CONTACT_NUM));
        subReportGrid.set(PARAM_SUPERVISOR, getParameter(PARAM_SUPERVISOR));
        subReportGrid.set(PARAM_SUPERVISOR_NUM, getParameter(PARAM_SUPERVISOR_NUM));

        subReportGrid.beforeTop();

        addParameter(PARAM_DATASET_SUB, subReportGrid);
        addParameter(SUB_REPORT_PSS, getParameter(SUB_REPORT_PSS));
        addParameter(PARAM_CHAIR_PERSON, "");
        addParameter(PARAM_CHAIR_PERSON_TITLE, "");
        addParameter(PARAM_CHAIR_PERSON_PHONE, "");
        addParameter(PARAM_PARENTNAME, "");
        addParameter(PARAM_PARENTADDY1, "");
        addParameter(PARAM_PARENTADDY2, "");
        addParameter(PARAM_SNAME, "");
        addParameter(PARAM_CONSENT_LINE1, "");
        addParameter(PARAM_CONSENT_LINE2, "");
        addParameter(PARAM_FORM_DATE, "");
        addParameter(PARAM_NOTICE_DETAILS, "");

        String prevParentAddress = "";
        String ParentAddress = "";

        addParameter("studentFirstName", m_currentIep.getStudent().getPerson().getFirstName());

        getChairPersonDetails();
        loadReportHeader();

        // this allows blank form to be produced if requested
        Collection dummy = new ArrayList<X2BaseBean>();
        dummy.add(getOrganization());

        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_meetingDictionary);

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
                if (teamMember.getMemberRoleCode().equals("Parent/Guardian")) {
                    teamMember.getPerson().getContact().getStudentContacts();

                    Collection<StudentContact> studentContacts =
                            teamMember.getPerson().getContact().getStudentContacts();

                    for (StudentContact studentContact : studentContacts) {
                        if (letterRecp.equals("Student")) {
                            if (studentContact.getLivesWithIndicator() == true) {
                                grid.append();
                                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));

                                addMeetingData(grid);
                                addNotificationRecipient(grid, teamMember);
                                addAssessmentData(grid);
                                hasGuardian = true;
                                prevParentAddress = studentContact.getContact().getAddressView();
                            }
                        } else {
                            if (studentContact.getStudentOid().equals(m_currentIep.getStudentOid())) {
                                if (studentContact.getGradeMailingIndicator() == true) {
                                    prevParentAddress = prevParentAddress.trim();
                                    ParentAddress = studentContact.getContact().getAddressView();
                                    ParentAddress = ParentAddress.trim();

                                    if (ParentAddress.equals(prevParentAddress)) {
                                        // skip
                                    } else {
                                        if (letterRecp.equals("Student") && prevParentAddress.length() > 0) {
                                            // skip
                                        } else {
                                            grid.append();
                                            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(dummy));

                                            addMeetingData(grid);
                                            addNotificationRecipient(grid, teamMember);
                                            addAssessmentData(grid);
                                            hasGuardian = true;
                                            prevParentAddress = studentContact.getContact().getAddressView();
                                            parentCount++;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                addParameter(PARAM_PARENT_COUNT, String.valueOf(parentCount));
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

        if (parentCount == 2) {
            ReportDataGrid subReportGrid2 = new ReportDataGrid();

            subReportGrid2.append();

            subReportGrid2.set(PARAM_SCHOOL_NAME, getParameter(PARAM_SCHOOL_NAME));
            subReportGrid2.set(PARAM_SCHOOL_SD, getParameter(PARAM_SCHOOL_SD));
            subReportGrid2.set(PARAM_SCHOOL_STREET, getParameter(PARAM_SCHOOL_STREET));
            subReportGrid2.set(PARAM_SCHOOL_PHONE, getParameter(PARAM_SCHOOL_PHONE));
            subReportGrid2.set(PARAM_SCHOOL_CSTEAM, getParameter(PARAM_SCHOOL_CSTEAM));
            subReportGrid2.set(PARAM_SCHOOL_CSTNUM, getParameter(PARAM_SCHOOL_CSTNUM));
            subReportGrid2.set(PARAM_SCHOOL_DISRES, getParameter(PARAM_SCHOOL_DISRES));
            subReportGrid2.set(PARAM_SCHOOL_CONTACT_NAME, getParameter(PARAM_SCHOOL_CONTACT_NAME));
            subReportGrid2.set(PARAM_DIS_CONTACT_NUM, getParameter(PARAM_DIS_CONTACT_NUM));
            subReportGrid2.set(PARAM_SUPERVISOR, getParameter(PARAM_SUPERVISOR));
            subReportGrid2.set(PARAM_SUPERVISOR_NUM, getParameter(PARAM_SUPERVISOR_NUM));

            subReportGrid2.beforeTop();
            addParameter(PARAM_DATASET_SUB2, subReportGrid2);
            addParameter(SUB_REPORT_PSS2, getParameter(SUB_REPORT_PSS));
        }

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

        m_meetingDictionary = DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());

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

        // added
        ReportDataGrid educationalGrid = new ReportDataGrid();
        ReportDataGrid socialGrid = new ReportDataGrid();
        ReportDataGrid psycologicalGrid = new ReportDataGrid();
        ReportDataGrid speechGrid = new ReportDataGrid();

        if (m_meeting.getOid() == null) {
            standardGrid.append();
            functionalGrid.append();
            relatedGrid.append();
            otherGrid.append();

            // added
            educationalGrid.append();
            socialGrid.append();
            psycologicalGrid.append();
            speechGrid.append();
        } else {
            for (GenericFormChildData child : m_meeting.getGenericFormDataChildren()) {
                String type = (String) child.getFieldValueByAlias(ALIAS_ASSESSMENT_TYPE, m_meetingDictionary);

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
                } else if (TYPE_ASSESMENT_SOCIAL.equals(type)) {
                    socialGrid.append();
                    socialGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESMENT_EDUCATIONAL.equals(type)) {
                    educationalGrid.append();
                    educationalGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESMENT_PSYCOLOGICAL.equals(type)) {
                    psycologicalGrid.append();
                    psycologicalGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESMENT_SPEECH.equals(type)) {
                    speechGrid.append();
                    speechGrid.set(FIELD_ASSESSMENT, child);
                }
            }
        }
        standardGrid.beforeTop();
        functionalGrid.beforeTop();
        relatedGrid.beforeTop();
        otherGrid.beforeTop();
        socialGrid.beforeTop();
        educationalGrid.beforeTop();
        psycologicalGrid.beforeTop();
        speechGrid.beforeTop();

        grid.set(FIELD_ASSESS_STD_DATA, standardGrid);
        grid.set(FIELD_ASSESS_FUNC_DATA, functionalGrid);
        grid.set(FIELD_ASSESS_REL_DATA, relatedGrid);
        grid.set(FIELD_ASSESS_OTHER_DATA, otherGrid);

        grid.set(FIELD_ASSESS_SOCIAL_DATA, socialGrid);
        grid.set(FIELD_ASSESS_EDUCATIONAL_DATA, educationalGrid);
        grid.set(FIELD_ASSESS_PSYCOLOGICAL_DATA, psycologicalGrid);
        grid.set(FIELD_ASSESS_SPEECH_DATA, speechGrid);
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
                grid.set(FIELD_RECIPIENT_ADDRESS_01, "");
                grid.set(FIELD_RECIPIENT_CITY, "");
                grid.set(FIELD_RECIPIENT_STATE, "");
                grid.set(FIELD_RECIPIENT_ZIP, "");
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
     * This method sets the chair person's details for the signature line of the SPED form. If the
     * team members does not
     * include a chair person, then the case manager signs the SPED form.
     *
     * @return void
     */
    private void getChairPersonDetails() {
        addParameter(PARAM_CHAIR_PERSON, "");
        addParameter(PARAM_CHAIR_PERSON_TITLE, "Case Manager");
        addParameter(PARAM_CHAIR_PERSON_PHONE, "");

        SisPerson chairPerson = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                break;
            }
        }

        String workPhone = "";
        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            chairPerson = caseManager.getPerson();
            workPhone = (String) caseManager.getFieldValueByAlias(ALIAS_WORK_PHONE);
        }

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + " " + chairPerson.getLastName());
            workPhone = EXTN_WORK_PHONE + workPhone;
            addParameter(PARAM_CHAIR_PERSON_PHONE, workPhone);
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

    /**
     * Initialize Supervisor Name.
     */
    private void initSssName() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STAFF_ROLE);
        if (field != null) {
            String beanPath = field.getJavaName();
            ReferenceTable refTable = field.getReferenceTable();
            Collection<ReferenceCode> codes = refTable.getReferenceCodes();

            String sssCode = null;

            for (ReferenceCode code : codes) {
                String stateCode = code.getStateCode();
                if (!StringUtils.isEmpty(stateCode) &&
                        stateCode.equals("SSS")) {
                    sssCode = code.getCode();
                }
            }

            if (!StringUtils.isEmpty(sssCode)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(beanPath, sssCode);
                QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
                SisStaff sssStaff = (SisStaff) getBroker().getBeanByQuery(query);
                m_sssStaffName = sssStaff.getPerson().getFirstName() + " " + sssStaff.getPerson().getLastName();
            }
        }
    }

    /**
     * Load Report Header.
     */
    private void loadReportHeader() {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = "";

        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, EMPTY_STRING);
        addParameter(PARAM_SCHOOL_PHONE_NO, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN1, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN2, EMPTY_STRING);
        addParameter(PARAM_SKL_ADMIN3, EMPTY_STRING);

        SisSchool school = student.getSchool();
        String schoolName = school.getName();
        addParameter(PARAM_SCHOOL_NAME, schoolName);

        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : EMPTY_STRING);

        SisAddress schoolAddress = school.getAddress();
        if (schoolAddress != null) {
            if (!StringUtils.isEmpty(schoolAddress.getPhone01()) ||
                    !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                addParameter(PARAM_SCHOOL_PHONE_NO, (StringUtils.isEmpty(schoolAddress.getPhone01())
                        ? schoolAddress.getPhone02() : schoolAddress.getPhone01()));
            }

            addParameter(PARAM_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
            addParameter(PARAM_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
        }

        if (school != null) {
            if (school.getAdministrator1() != null) {
                SisPerson adminPerson1 = school.getAdministrator1().getPerson();
                admin1 = adminPerson1.getFirstName() + " " + adminPerson1.getLastName();
                addParameter(PARAM_SKL_ADMIN1, admin1);

                String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
                if (superintendent != null) {
                    String[] admin2 = superintendent.split(",");
                    addParameter(PARAM_SKL_ADMIN2, admin2[1] + " " + admin2[0]);
                }
            }
        }
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }
}
