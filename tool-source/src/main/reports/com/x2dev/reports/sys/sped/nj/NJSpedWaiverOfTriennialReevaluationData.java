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

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.Collection;

/**
 * The Class NJSpedWaiverOfTriennialReevaluationData.
 */
public class NJSpedWaiverOfTriennialReevaluationData extends BaseFormReportJavaSource {
    /**
     * Instances
     */
    private ReportDataGrid m_consentForm = null;
    private IepData m_currentIep = null;

    /**
     * Aliases
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_WORK_PHONE = "DOE STAFF WORK PHONE";
    private static final String EXTN_WORK_PHONE = "(908) 284-";

    /**
     * Params
     */
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_TITLE = "CHAIR_PERSON_TITLE";
    private static final String PARAM_CHAIR_PERSON_PHONE = "CHAIR_PERSON_PHONE";
    private static final String PARAM_PARENTNAME = "PARENTNAME";
    private static final String PARAM_PARENTADDY1 = "PARENTADDY1";
    private static final String PARAM_PARENTADDY2 = "PARENTADDY2";
    private static final String SUB_REPORT_PSS = "subID";
    private static final String PARAM_SNAME = "SNAME";
    private static final String PARAM_CONSENT_LINE1 = "CONSENT_LINE1";
    private static final String PARAM_CONSENT_LINE2 = "CONSENT_LINE2";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_NOTICE_DETAILS = "NOTICE_DETAILS";
    private static final String PARAM_DATASET_SUB = "DATASET_SUB";

    // Report Header Parameters
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SCHOOL_SD = "schoolSpecialServicesDepartment";
    private static final String PARAM_SCHOOL_STREET = "schoolStreet";
    private static final String PARAM_SCHOOL_PHONE = "schoolPhoneNumber";
    private static final String PARAM_SCHOOL_CSTEAM = "schoolChildStudyTeam";
    private static final String PARAM_SCHOOL_CSTNUM = "schoolCSTNumber";
    private static final String PARAM_SCHOOL_DISRES = "schoolDisabilityResource";
    private static final String PARAM_SCHOOL_CONTACT_NAME = "schoolDisabilityContactName";
    private static final String PARAM_DIS_CONTACT_NUM = "schoolDisabilityContactNumber";
    private static final String PARAM_SUPERVISOR = "countySupervisor";
    private static final String PARAM_SUPERVISOR_NUM = "countySupervisorPhoneNumber";

    /**
     * Other Constants
     */
    private static final String EMPTY_STRING = "";
    private static final String FAX = "FAX";

    /**
     * This method builds the entire report depending on whether it is the parent version or the
     * child version.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        m_currentIep = (IepData) getFormOwner();
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        String prevParentAddress = "";
        String ParentAddress = "";

        // print a notification for each parent
        // the report is completed in the detail band

        for (IepTeamMember teamMember : teamMembers) {
            if (teamMember.getMemberRoleCode().equals("Parent/Guardian")) {
                teamMember.getPerson().getContact().getStudentContacts();

                Collection<StudentContact> studentContacts = teamMember.getPerson().getContact().getStudentContacts();
                for (StudentContact sContact : studentContacts) {
                    if (sContact.getStudentOid().equals(m_currentIep.getStudentOid())) {
                        if (sContact.getGradeMailingIndicator() == true) {
                            prevParentAddress = prevParentAddress.trim();
                            ParentAddress = sContact.getContact().getAddressView();
                            ParentAddress = ParentAddress.trim();

                            if (ParentAddress.equals(prevParentAddress)) {
                                // skip
                            } else {
                                Report rubricSubreport =
                                        ReportUtils.getReport((String) getParameter(SUB_REPORT_PSS), getBroker());
                                addParameter("SUB_REPORT_PSS",
                                        new ByteArrayInputStream(rubricSubreport.getCompiledFormat()));

                                IepData iep = (IepData) getFormOwner();
                                SisStudent sisStudent = iep.getStudent();

                                String studentsName = sisStudent.getNameView();
                                addParameter("footersname", studentsName);
                                addParameter(PARAM_SNAME, " of " + studentsName);
                                String studentFirstNameFormatted = "";
                                String firstName = "";

                                if (studentsName != null) {
                                    String delims = ",";
                                    String[] tokens = studentsName.split(delims);

                                    firstName = tokens[1];
                                    String lastName = tokens[0];

                                    studentsName = firstName + " " + lastName;
                                    if (firstName.endsWith("s")) {
                                        studentFirstNameFormatted = firstName + "'";
                                    } else {
                                        studentFirstNameFormatted = firstName + "'s";
                                    }
                                }

                                String selection = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("triennial-again", getDictionary()));

                                if (selection.equals("1")) {
                                    addParameter("selection", "If you consent to waive this triennial reevaluation,"
                                            + firstName + " continues to be eligible "
                                            + "for special education and related services and"
                                            + studentFirstNameFormatted
                                            + " next triennial will be due three years from "
                                            + "the date you provide consent to the district.");
                                } else {
                                    addParameter("selection", "If you consent to waive this triennial reevaluation,"
                                            + firstName + " continues to be eligible "
                                            + "for special education and related services but no other triennial reevaluation is anticipated, as"
                                            + firstName + " "
                                            + "will be graduating/exiting the school system before another three years have passed.");
                                }

                                String riop1 = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("ri-op1", getDictionary()));
                                String riop2 = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("ri-op2", getDictionary()));
                                String riop3 = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("ri-op3", getDictionary()));

                                if (riop1.equals("1")) {
                                    addParameter("riop1", "X");
                                } else {
                                    addParameter("riop1", "");
                                }

                                if (riop2.equals("1")) {
                                    addParameter("riop2", "X");
                                } else {
                                    addParameter("riop2", "");
                                }

                                if (riop3.equals("1")) {
                                    addParameter("riop3", "X");
                                } else {
                                    addParameter("riop3", "");
                                }

                                String reasons = "";
                                reasons = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("triennial-reasons-waiving", getDictionary()));
                                addParameter("reasons", reasons);

                                String other = "";
                                other = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("triennial-other-options", getDictionary()));
                                addParameter("other", other);

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

                                Person studentPerson = null;
                                ReportType reportType = null;
                                m_consentForm = new ReportDataGrid();

                                String formDate = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("triennial-letter-date", getDictionary()));

                                String sFormatDate = "";

                                if (formDate != null) {
                                    String delims = "-";
                                    String[] tokens = formDate.split(delims);

                                    String pYear = tokens[0];
                                    String pMonth = tokens[1];
                                    String pDay = tokens[2];

                                    sFormatDate = pMonth + "/" + pDay + "/" + pYear;
                                }

                                formDate = sFormatDate;

                                SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                                SimpleDateFormat formatted = new SimpleDateFormat("MMMMM dd, yyyy");

                                String dFormDate = formatted.format(formatter.parse(formDate));

                                String formReturnDate = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("triennial-letter-returndate", getDictionary()));

                                String sFormatReturnDate = "";

                                if (formReturnDate != null) {
                                    String delims = "-";
                                    String[] tokens = formReturnDate.split(delims);

                                    String pYear = tokens[0];
                                    String pMonth = tokens[1];
                                    String pDay = tokens[2];

                                    sFormatReturnDate = pMonth + "/" + pDay + "/" + pYear;
                                }

                                formReturnDate = sFormatReturnDate;

                                addParameter(PARAM_FORM_DATE, dFormDate);

                                String formDateDue = (String) (((GenericFormData) getFormStorage())
                                        .getFieldValueByAlias("triennial-due-date", getDictionary()));

                                String sFormatDate2 = null;

                                if (formDateDue != null) {
                                    String delims = "-";
                                    String[] tokens = formDateDue.split(delims);

                                    String pYear = tokens[0];
                                    String pMonth = tokens[1];
                                    String pDay = tokens[2];

                                    sFormatDate2 = pMonth + "/" + pDay + "/" + pYear;
                                }

                                getChairPersonDetails();
                                loadReportHeader();

                                SisStudent student = m_currentIep.getStudent();
                                if (student != null) {
                                    studentPerson = student.getPerson();
                                }

                                if (studentPerson != null) {
                                    buildStudentForm(studentPerson);
                                }

                                iep = (IepData) getFormOwner();
                                sisStudent = iep.getStudent();
                                studentsName = sisStudent.getNameView();
                                studentFirstNameFormatted = "";
                                firstName = "";

                                if (studentsName != null) {
                                    String delims = ",";
                                    String[] tokens = studentsName.split(delims);

                                    firstName = tokens[1];
                                    String lastName = tokens[0];

                                    studentsName = firstName + " " + lastName;
                                    if (firstName.endsWith("s")) {
                                        studentFirstNameFormatted = firstName + "'";
                                    } else {
                                        studentFirstNameFormatted = firstName + "'s";
                                    }

                                }

                                String Statement = "";
                                Statement = "A triennial (three year) reevaluation of" + firstName +
                                        " which includes a reevaluation and re-determination of eligibility would need to be completed by "
                                        + sFormatDate2 + ". " +
                                        "However, in accordance with N.J.A.C. 6A:14-3.8(e), the district is proposing to waive the triennial reevaluation"
                                        +
                                        " of your child at this time and is requesting your consent to do so.  The district reviewed the following relevant information:";

                                addParameter(PARAM_NOTICE_DETAILS, Statement);

                                if (sFormatDate2 != null) {
                                    addParameter(PARAM_CONSENT_LINE1,
                                            bldConsLnBasedOnRptTypeAndCons(reportType, true) + sFormatDate2);
                                    addParameter(PARAM_CONSENT_LINE2,
                                            bldConsLnBasedOnRptTypeAndCons(reportType, false) + sFormatDate2);
                                    addParameter("returnline",
                                            "Please return this signed form by " + formReturnDate + " to:");
                                } else {
                                    addParameter(PARAM_CONSENT_LINE1, bldConsLnBasedOnRptTypeAndCons(reportType, true));
                                    addParameter(PARAM_CONSENT_LINE2,
                                            bldConsLnBasedOnRptTypeAndCons(reportType, false));
                                    addParameter("returnline", "Please return this signed form to:");
                                }
                                prevParentAddress = sContact.getContact().getAddressView();
                            }
                        }
                    }

                }
            }
        }

        if (m_consentForm.rowCount() == 0) {
            m_consentForm.append();
        }
        m_consentForm.beforeTop();

        return m_consentForm;
    }

    /**
     * This method builds the student version of the form.
     *
     * @param studentPerson Person
     */
    public void buildStudentForm(Person studentPerson) {
        setAllReportFieldsExceptAgency(studentPerson);
    }

    /**
     * This method sets most of the report fields.
     *
     * @param person void
     */
    public void setAllReportFieldsExceptAgency(Person person) {
        String addy1 = "";
        String addy2 = "";

        Address parentMailingAddress = person.getResolvedMailingAddress();
        m_consentForm.append();
        if (parentMailingAddress != null) {
            addy1 = parentMailingAddress.getAddressLine01();
            addy2 = parentMailingAddress.getAddressLine03();
        }

        IepData iep = (IepData) getFormOwner();
        SisStudent sisStudent = iep.getStudent();
        String studentsName = "";
        studentsName = sisStudent.getNameView();

        if (studentsName != null) {
            String delims = ",";
            String[] tokens = studentsName.split(delims);
            String firstName = tokens[1];
            String lastName = tokens[0];
            studentsName = firstName + " " + lastName;
        }

        addParameter(PARAM_PARENTNAME, "Parent/Guardian of" + studentsName);
        addParameter(PARAM_PARENTADDY1, addy1);
        addParameter(PARAM_PARENTADDY2, addy2);
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
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        for (IepTeamMember teamMember : teamMembers) {
            if (teamMember.getChairpersonIndicator()) {
                chairPerson = teamMember.getPerson();
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
     * This method builds the consent part of the form based on the report type and consent type.
     *
     * @param reportType ReportType
     * @param consentGiven boolean
     * @return String
     */
    private String bldConsLnBasedOnRptTypeAndCons(ReportType reportType, boolean consentGiven) {
        IepData iep = (IepData) getFormOwner();
        SisStudent sisStudent = iep.getStudent();

        String studentsName = sisStudent.getNameView();
        addParameter(PARAM_SNAME, " of " + studentsName);

        String consent = "";
        String firstName = "";

        if (studentsName != null) {
            String delims = ",";
            String[] tokens = studentsName.split(delims);

            firstName = tokens[1];
            String lastName = tokens[0];

            studentsName = firstName + " " + lastName;
        }

        if (consentGiven) {
            consent = "I consent to waive the current triennial reevaluation of" + firstName + " that is due on ";
        } else {
            consent =
                    "I do not consent to waive the current triennial reevaluation of" + firstName + " that is due on ";
        }

        return consent;
    }

    /**
     * This sets the report type.
     * 
     * @author Follett Software Company
     *
     */
    private enum ReportType {
        PARENT, STUDENT;
    }

    /**
     * The following methods are called, in order, during the life-cycle of a ReportJavaSource
     * instance:
     * <ol>
     * <li><code>saveState(UserDataContainer)</code>
     * <li><code>initialize()</code>
     * <li><code>gatherData()</code>
     * <li><code>releaseResources()</code>
     * </ol>
     *
     * This method is provided as a convenient way for subclasses to initialize member variables
     * before the <code>run()</code> method is called. The default implementation does nothing.
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        }
    }

    /**
     * This method is provided as a way for subclasses to save session state information. The
     * default implementation does nothing.
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);
        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Load the title data of the report.
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
                SisPerson adminPerson = school.getAdministrator1().getPerson();
                admin1 = adminPerson.getFirstName() + " " + adminPerson.getLastName();
                addParameter(PARAM_SKL_ADMIN1, admin1);

                String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
                if (superintendent != null) {
                    String[] admin2 = superintendent.split(",");
                    addParameter(PARAM_SKL_ADMIN2, admin2[1] + " " + admin2[0]);
                }
            }
        }
    }
}
