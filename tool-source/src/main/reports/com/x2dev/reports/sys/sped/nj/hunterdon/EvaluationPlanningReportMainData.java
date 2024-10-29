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
package com.x2dev.reports.sys.sped.nj.hunterdon;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
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
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import net.sf.jasperreports3.engine.data.JRBeanCollectionDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class EvaluationPlanningReportMainData.
 */
public class EvaluationPlanningReportMainData extends BaseFormReportJavaSource {
    /**
     * Aliases
     */
    private static final String ALIAS_ASSESSMENT_TYPE = "eval-assessment-type";
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_EVAL_EVALUATION = "eval-evaluation";
    private static final String ALIAS_PARENT_VERSION = "parent-version";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";
    /**
     * Report Constants
     */
    private static final String EXTN_WORK_PHONE = "(908) 284-";
    /**
     * Report fields
     */
    private static final String FIELD_ASSESS_EDUCATIONAL_DATA = "educationalData";
    private static final String FIELD_ASSESS_FUNC_DATA = "functionalData";
    private static final String FIELD_ASSESS_OTHER_DATA = "otherData";
    private static final String FIELD_ASSESS_PSYCHOLOGICAL_DATA = "psycologicalData";
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
    private static final String FORM_DEF_ID_REDETERMINE = "SPED-NJ-REDETERMINE";
    /**
     * Codes
     */
    private static final String LETTER_RECIPIENT = "Parent";
    private static final String MEMBER_ROLE_STUDENT = "Student";
    /**
     * Report Parameters
     */
    private static final String PARAM_CASE_MANAGER = "caseManager";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_PHONE = "CHAIR_PERSON_PHONE";
    private static final String PARAM_CHAIR_PERSON_TITLE = "CHAIR_PERSON_TITLE";
    private static final String PARAM_CONSENT_LINE1 = "CONSENT_LINE1";
    private static final String PARAM_CONSENT_LINE2 = "CONSENT_LINE2";
    private static final String PARAM_DATASET_SUB = "DATASET_SUB";
    private static final String PARAM_DATASET_SUB2 = "DATASET_SUB2";
    private static final String PARAM_DATE_CONVERTER = "dateAsStringConverter";
    private static final String PARAM_DIS_CONTACT_NUM = "schoolDisabilityContactNumber";
    private static final String PARAM_EVAL_REQ = "evalreq";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_IEP_DATA = "iepData";
    private static final String PARAM_IS_REDETERMINE_ELIG = "isRedetermineEligibility";
    private static final String PARAM_LETTER_RECIPIENT = "letterRecp";
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
    private static final String PARAM_STUDENT_FIRST_NAME = "studentFirstName";
    private static final String PARAM_SUPERVISOR = "countySupervisor";
    private static final String PARAM_SUPERVISOR_NUM = "countySupervisorPhoneNumber";
    private static final String PARAM_TITLE_CASE_MANAGER = "Case Manager";
    /**
     * Constants
     */
    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_SPACE = " ";

    private static final String SUB_REPORT_ID_PSS = "SUB_REPORT_PSS";
    private static final String SUB_REPORT_ID_PSS2 = "SUB_REPORT_PSS2";
    private static final String SUB_REPORT_PSS = "subID";
    private static final String SUB_REPORT_PSS2 = "subID2";

    private static final String TYPE_ASSESSMENT_EDUCATIONAL = "Educational";
    private static final String TYPE_ASSESSMENT_FUNCTIONAL = "Functional Assessment";
    private static final String TYPE_ASSESSMENT_OTHER = "Other";
    private static final String TYPE_ASSESSMENT_PSYCHOLOGICAL = "Psychological";
    private static final String TYPE_ASSESSMENT_RELATED = "Related Service";
    private static final String TYPE_ASSESSMENT_SOCIAL = "Social";
    private static final String TYPE_ASSESSMENT_SPEECH = "Speech/Language";
    private static final String TYPE_ASSESSMENT_STANDARD = "Standardized Test";

    /**
     * Variables
     */
    private IepData m_currentIep;
    private GenericFormData m_meeting;
    private DataDictionary m_meetingDictionary;
    private String m_sssStaffName;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        DataDictionary dictionary = getDictionary();
        GenericFormData genericFormData = (GenericFormData) getFormStorage();
        String letterRecp = LETTER_RECIPIENT;
        if ((String) (genericFormData.getFieldValueByAlias(ALIAS_PARENT_VERSION, dictionary)) != null) {
            letterRecp = (String) (genericFormData.getFieldValueByAlias(ALIAS_PARENT_VERSION, dictionary));
        }

        addParameter(PARAM_LETTER_RECIPIENT, letterRecp);

        int parentCount = 0;
        String evalReq = (String) (genericFormData.getFieldValueByAlias(ALIAS_EVAL_EVALUATION, dictionary));
        addParameter(PARAM_EVAL_REQ, evalReq);

        ReportDataGrid grid = new ReportDataGrid();
        Report rubricSubreport = ReportUtils.getReport((String) getParameter(SUB_REPORT_PSS), getBroker());
        addParameter(SUB_REPORT_ID_PSS, new ByteArrayInputStream(rubricSubreport.getCompiledFormat()));

        Report rubricSubreport2 = ReportUtils.getReport((String) getParameter(SUB_REPORT_PSS2), getBroker());
        addParameter(SUB_REPORT_ID_PSS2, new ByteArrayInputStream(rubricSubreport2.getCompiledFormat()));

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

        addParameter(PARAM_CHAIR_PERSON, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_TITLE, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_PHONE, STRING_EMPTY);
        addParameter(PARAM_PARENTNAME, STRING_EMPTY);
        addParameter(PARAM_PARENTADDY1, STRING_EMPTY);
        addParameter(PARAM_PARENTADDY2, STRING_EMPTY);
        addParameter(PARAM_SNAME, STRING_EMPTY);
        addParameter(PARAM_CONSENT_LINE1, STRING_EMPTY);
        addParameter(PARAM_CONSENT_LINE2, STRING_EMPTY);
        addParameter(PARAM_FORM_DATE, STRING_EMPTY);
        addParameter(PARAM_NOTICE_DETAILS, STRING_EMPTY);

        SisStudent student = m_currentIep.getStudent();
        Person studentPerson = student.getPerson();
        addParameter(PARAM_STUDENT_FIRST_NAME, studentPerson.getFirstName());

        // This allows blank form to be produced if requested
        Collection emptyReportGrid = new ArrayList<X2BaseBean>();
        emptyReportGrid.add(getOrganization());

        getChairPersonDetails();

        loadReportHeader();


        if (getParameter(PARAM_BLANK).equals(Boolean.FALSE)) {
            addParameter(PARAM_IEP_DATA, m_currentIep);
            addParameter(PARAM_MEETING, m_meeting);
            addParameter(PARAM_MEETING_DICTIONARY, m_meetingDictionary);

            if (m_currentIep.getStaff() != null &&
                    m_currentIep.getStaff().getPerson() != null) {
                String caseManager = getFullName(m_currentIep.getStaff().getPerson());

                addParameter(PARAM_CASE_MANAGER, caseManager);
            }


            Address studentMailingAddress = student.getPerson().getMailingAddress();
            if (studentMailingAddress == null) {
                studentMailingAddress = student.getPerson().getPhysicalAddress();
            }
            String studentMailingAddressView = STRING_EMPTY;
            if (studentMailingAddress.getAddressLine01() != null) {
                studentMailingAddressView = studentMailingAddress.getAddressLine01().trim();
            }

            Collection<StudentContact> studentContacts = student.getContacts();

            Set<String> prevMailingAddresses = new HashSet<String>();
            String contactMailingAddressView = STRING_EMPTY;

            if (letterRecp.equals(MEMBER_ROLE_STUDENT)) {
                grid.append();
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

                addMeetingData(grid);

                addNotificationRecipient(grid, studentPerson, studentMailingAddress);

                addAssessmentData(grid);
            } else // MEMBER_ROLE_PARENT
            {
                // One single copy to the Student's mailing address
                grid.append();
                grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

                addMeetingData(grid);

                // For this report the recipients name will not be used.
                addNotificationRecipient(grid, studentPerson, studentMailingAddress);

                addAssessmentData(grid);

                prevMailingAddresses.add(studentMailingAddressView);

                parentCount++;

                // Additional single copies to any other contacts that don't LiveWith the student
                for (StudentContact studentContact : studentContacts) {
                    if (studentContact.getGradeMailingIndicator()) {
                        Contact contact = studentContact.getContact();
                        Person contactPerson = contact.getPerson();

                        Address contactMailingAddress = contactPerson.getMailingAddress();
                        contactMailingAddressView = STRING_EMPTY;

                        // Don't print a letter if the contact doesn't have an address.
                        if (contactMailingAddress != null) {
                            if (contactMailingAddress.getAddressLine01() != null) {
                                contactMailingAddressView = contactMailingAddress.getAddressLine01().trim();
                            }

                            if (!StringUtils.isEmpty(contactMailingAddressView)) {
                                // Print only one copy of the report for each address of all
                                // contacts having the GradeMailing flag set.
                                // Only send a copy of the report to contact that don't Live With
                                // the student.
                                if (!prevMailingAddresses.contains(contactMailingAddressView)
                                        && !(studentContact.getLivesWithIndicator())) {
                                    grid.append();
                                    grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

                                    addMeetingData(grid);

                                    addNotificationRecipient(grid, contactPerson, contactMailingAddress);

                                    addAssessmentData(grid);

                                    prevMailingAddresses.add(contactMailingAddressView);

                                    parentCount++;
                                }
                            }
                        }
                    }

                    addParameter(PARAM_PARENT_COUNT, String.valueOf(parentCount));
                }
            }
        }


        Report evalSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_EVAL_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_EVAL_SUB_REPORT, new ByteArrayInputStream(evalSubreport.getCompiledFormat()));
        Report safeSubreport = ReportUtils.getReport((String) getParameter(PARAM_PROC_SAFE_SUBREPORT_ID), getBroker());
        addParameter(PARAM_PROC_SAFE_REPORT, new ByteArrayInputStream(safeSubreport.getCompiledFormat()));

        addParameter(PARAM_IS_REDETERMINE_ELIG,
                getFormDefinition().getId().equals(FORM_DEF_ID_REDETERMINE) ? Boolean.TRUE : Boolean.FALSE);

        // if there are no recipients, use an empty grid to print a blank report
        if (grid.rowCount() == 0) {
            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.FALSE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

            addAssessmentData(grid);

            grid.append();
            grid.set(FIELD_BLANK_TYPE, Boolean.TRUE);
            grid.set(FIELD_PROC_SAFE_DATA, new JRBeanCollectionDataSource(emptyReportGrid));

            addAssessmentData(grid);
        }
        grid.beforeTop();

        if (parentCount >= 2) {
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

        addParameter(PARAM_DATE_CONVERTER,
                ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true));
    }

    /**
     * Assessment Date.
     *
     * @param grid ReportDataGrid
     */
    private void addAssessmentData(ReportDataGrid grid) {
        ReportDataGrid standardGrid = new ReportDataGrid();
        ReportDataGrid functionalGrid = new ReportDataGrid();
        ReportDataGrid relatedGrid = new ReportDataGrid();
        ReportDataGrid otherGrid = new ReportDataGrid();
        ReportDataGrid educationalGrid = new ReportDataGrid();
        ReportDataGrid socialGrid = new ReportDataGrid();
        ReportDataGrid psycologicalGrid = new ReportDataGrid();
        ReportDataGrid speechGrid = new ReportDataGrid();

        if (m_meeting.getOid() == null) {
            standardGrid.append();
            functionalGrid.append();
            relatedGrid.append();
            otherGrid.append();

            educationalGrid.append();
            socialGrid.append();
            psycologicalGrid.append();
            speechGrid.append();
        } else {
            for (GenericFormChildData child : m_meeting.getGenericFormDataChildren()) {
                String type = (String) child.getFieldValueByAlias(ALIAS_ASSESSMENT_TYPE, m_meetingDictionary);

                if (TYPE_ASSESSMENT_STANDARD.equals(type)) {
                    standardGrid.append();
                    standardGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESSMENT_FUNCTIONAL.equals(type)) {
                    functionalGrid.append();
                    functionalGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESSMENT_RELATED.equals(type)) {
                    relatedGrid.append();
                    relatedGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESSMENT_OTHER.equals(type)) {
                    otherGrid.append();
                    otherGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESSMENT_SOCIAL.equals(type)) {
                    socialGrid.append();
                    socialGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESSMENT_EDUCATIONAL.equals(type)) {
                    educationalGrid.append();
                    educationalGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESSMENT_PSYCHOLOGICAL.equals(type)) {
                    psycologicalGrid.append();
                    psycologicalGrid.set(FIELD_ASSESSMENT, child);
                } else if (TYPE_ASSESSMENT_SPEECH.equals(type)) {
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
        grid.set(FIELD_ASSESS_PSYCHOLOGICAL_DATA, psycologicalGrid);
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
     * @param person Person
     * @param address Address
     */
    private void addNotificationRecipient(ReportDataGrid grid, Person person, Address address) {
        if (person != null) {
            String recipientName = getFullName(person);
            grid.set(FIELD_RECIPIENT_NAME, recipientName);

            grid.set(FIELD_RECIPIENT_ADDRESS_01, null);
            grid.set(FIELD_RECIPIENT_CITY, null);
            grid.set(FIELD_RECIPIENT_STATE, null);
            grid.set(FIELD_RECIPIENT_ZIP, null);

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
        addParameter(PARAM_CHAIR_PERSON, STRING_EMPTY);
        addParameter(PARAM_CHAIR_PERSON_TITLE, PARAM_TITLE_CASE_MANAGER);
        addParameter(PARAM_CHAIR_PERSON_PHONE, STRING_EMPTY);

        SisPerson chairPerson = null;
        Collection<IepTeamMember> teamMbrs = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMbr : teamMbrs) {
            if (teamMbr.getChairpersonIndicator()) {
                chairPerson = teamMbr.getPerson();
                break;
            }
        }

        String workPhone = STRING_EMPTY;
        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            chairPerson = caseManager.getPerson();
            workPhone = (String) caseManager.getFieldValueByAlias(ALIAS_WORK_PHONE);
        }

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + STRING_SPACE + chairPerson.getLastName());
            workPhone = EXTN_WORK_PHONE + workPhone;
            addParameter(PARAM_CHAIR_PERSON_PHONE, workPhone);
        }
    }

    /**
     * Get full name from a person bean.
     *
     * @param person Person
     * @return fullName
     */
    private String getFullName(Person person) {
        String fullName = STRING_EMPTY;

        fullName = person.getFirstName();

        if (!StringUtils.isEmpty(fullName)) {
            fullName = fullName + STRING_SPACE;
        }

        fullName = fullName + person.getLastName();

        return fullName;
    }

    /**
     * Initialize Supervisor name.
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
        String admin1 = STRING_EMPTY;

        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        addParameter(PARAM_SCHOOL_PHONE_NO, STRING_EMPTY);
        addParameter(PARAM_SKL_ADMIN1, STRING_EMPTY);
        addParameter(PARAM_SKL_ADMIN2, STRING_EMPTY);
        addParameter(PARAM_SKL_ADMIN3, STRING_EMPTY);

        SisSchool school = student.getSchool();
        String schoolName = school.getName();
        addParameter(PARAM_SCHOOL_NAME, schoolName);
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

        if (school.getAdministrator1() != null) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + STRING_SPACE + adminPerson1.getLastName();
            addParameter(PARAM_SKL_ADMIN1, admin1);

            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
            if (superintendent != null) {
                String[] admin2 = superintendent.split(STRING_COMMA);
                addParameter(PARAM_SKL_ADMIN2, admin2[1] + STRING_SPACE + admin2[0]);
            }
        }
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }
}
