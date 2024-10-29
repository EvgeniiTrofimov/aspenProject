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
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepOtherService;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SummaryOfGradPerformanceData.
 */
public class SummaryOfGradPerformanceData extends BaseFormReportJavaSource {
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_INTERAGENCY_NAME = "iep-agency";
    private static final String ALIAS_SCHOOL_FAX = "'DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";

    private static final String EMPTY_STRING = "";
    private static final String EXTN_WORK_PHONE = "(908) 284-";

    private static final String FAX = "FAX";

    /**
     * Fields
     */
    private static final String FIELD_ADDRESS_LINE1 = "addressLine1";
    private static final String FIELD_ADDRESS_LINE2 = "addressLine2";
    private static final String FIELD_AGENCY_DESCRIPTION = "agencyDescription";
    private static final String FIELD_AGENCY_NAME = "agencyName";
    private static final String FIELD_AGENCY_PHONE = "agencyphone";
    private static final String FIELD_AGENCY_WEBSITE = "agencywebsite";
    private static final String FIELD_CONSENTEE_NAME = "consenteeName";

    private static final String PARAM_CB1 = "CB1";
    private static final String PARAM_CB2 = "CB2";
    private static final String PARAM_CB3 = "CB3";
    private static final String PARAM_CB4 = "CB4";
    private static final String PARAM_CB5 = "CB5";
    private static final String PARAM_CB6 = "CB6";
    private static final String PARAM_CB7 = "CB7";
    private static final String PARAM_CBDDD1 = "CBDDD1";
    private static final String PARAM_CBDDD2 = "CBDDD2";
    private static final String PARAM_CBDDD3 = "CBDDD3";
    private static final String PARAM_CHAIR_PERSON = "CHAIR_PERSON";
    private static final String PARAM_CHAIR_PERSON_PHONE = "CHAIR_PERSON_PHONE";
    private static final String PARAM_CHAIR_PERSON_TITLE = "CHAIR_PERSON_TITLE";
    private static final String PARAM_CONSENT_TYPE = "CONSENT_TYPE";
    private static final String PARAM_DOB = "DOB";
    private static final String PARAM_ELIGIBILITY = "ELIGIBILITY";
    private static final String PARAM_FOOTER_NAMEVIEW = "FOOTER_NAMEVIEW";
    private static final String PARAM_FORM_DATE = "FORM_DATE";
    private static final String PARAM_GRAD_DATE = "GRAD_DATE";
    private static final String PARAM_OTCB1 = "OTCB1";
    private static final String PARAM_OTCB2 = "OTCB2";
    private static final String PARAM_OTCB3 = "OTCB3";
    private static final String PARAM_OTTB = "OTTB";
    private static final String PARAM_POSTSECONDARY = "POSTSECONDARY";
    private static final String PARAM_PRESENT_LEVELS = "PRESENT_LEVELS";
    private static final String PARAM_RECCOMENDATIONS = "RECCOMENDATIONS";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SERVICE_NAME_INTERAGENCY = "InterAgency";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SNAME = "SNAME";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_TBDDD = "TBDDD";

    private static final String STRING_STUDENT = "Student";
    private static final String STRING_TABS = "\t\t";

    private Map<String, String> m_agencyInfo = new HashMap<String, String>();
    private ReportDataGrid m_consentForm = null;
    private IepData m_currentIep = null;
    private String m_sssStaffName;

    /**
     * This method builds the entire report depending on whether it is the parent version or the
     * child version.
     *
     * @return Object
     * @throws Exception exception
     */
    @Override
    protected Object gatherData() throws Exception {
        addParameter(PARAM_POSTSECONDARY, (((GenericFormData) getFormStorage())
                .getFieldValueByAlias("sogp-postsecondary-goals", getDictionary())));
        addParameter(PARAM_PRESENT_LEVELS,
                (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-present-levels", getDictionary())));
        addParameter(PARAM_TBDDD,
                (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-wl-othertb", getDictionary())));
        addParameter(PARAM_OTTB,
                (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ot-othertb", getDictionary())));
        addParameter(PARAM_RECCOMENDATIONS,
                (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-recommendations", getDictionary())));

        String cb1 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ag-njdvrs", getDictionary()));
        String cb2 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ag-njcbvi", getDictionary()));
        String cb3 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ag-njtal", getDictionary()));
        String cb4 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ag-cps", getDictionary()));
        String cb5 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ag-njdyfs", getDictionary()));
        String cb6 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ag-ss", getDictionary()));
        String cb7 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ag-njddd", getDictionary()));

        if (cb1.equals("1")) {
            addParameter(PARAM_CB1, "X");
        } else {
            addParameter(PARAM_CB1, "");
        }

        if (cb2.equals("1")) {
            addParameter(PARAM_CB2, "X");
        } else {
            addParameter(PARAM_CB2, "");
        }

        if (cb3.equals("1")) {
            addParameter(PARAM_CB3, "X");
        } else {
            addParameter(PARAM_CB3, "");
        }

        if (cb4.equals("1")) {
            addParameter(PARAM_CB4, "X");
        } else {
            addParameter(PARAM_CB4, "");
        }

        if (cb5.equals("1")) {
            addParameter(PARAM_CB5, "X");
        } else {
            addParameter(PARAM_CB5, "");
        }

        if (cb6.equals("1")) {
            addParameter(PARAM_CB6, "X");
        } else {
            addParameter(PARAM_CB6, "");
        }

        if (cb7.equals("1")) {
            addParameter(PARAM_CB7, "X");
        } else {
            addParameter(PARAM_CB7, "");
        }

        String cbddd1 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-wl-res", getDictionary()));

        if (cbddd1.equals("1")) {
            addParameter(PARAM_CBDDD1, "X");
        } else {
            addParameter(PARAM_CBDDD1, "");
        }


        String cbddd2 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-wl-dso", getDictionary()));

        if (cbddd2.equals("1")) {
            addParameter(PARAM_CBDDD2, "X");
        } else {
            addParameter(PARAM_CBDDD2, "");
        }

        String cbddd3 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-wl-other", getDictionary()));

        if (cbddd3.equals("1")) {
            addParameter(PARAM_CBDDD3, "X");
        } else {
            addParameter(PARAM_CBDDD3, "");
        }

        String cbot1 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ot-cmha", getDictionary()));

        if (cbot1.equals("1")) {
            addParameter(PARAM_OTCB1, "X");
        } else {
            addParameter(PARAM_OTCB1, "");
        }

        String cbot2 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ot-cil", getDictionary()));

        if (cbot2.equals("1")) {
            addParameter(PARAM_OTCB2, "X");
        } else {
            addParameter(PARAM_OTCB2, "");
        }

        String cbot3 =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-ot-other", getDictionary()));

        if (cbot3.equals("1")) {
            addParameter(PARAM_OTCB3, "X");
        } else {
            addParameter(PARAM_OTCB3, "");
        }

        Person studentPerson = null;
        m_consentForm = new ReportDataGrid();

        String formDate =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-form-date", getDictionary()));

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
        new SimpleDateFormat("MMMMM dd, yyyy");

        String dFormDate = formatter.format(formatter.parse(formDate));
        // String dFormDate = formatted.format(formatter.parse(formDate));

        String formReturnDate =
                (String) (((GenericFormData) getFormStorage()).getFieldValueByAlias("sogp-grad-date", getDictionary()));

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
        addParameter(PARAM_GRAD_DATE, formReturnDate);

        SisStudent student = m_currentIep.getStudent();
        if (student != null) {
            studentPerson = student.getPerson();
        }

        String dob = String.valueOf(student.getPerson().getDob());

        SimpleDateFormat dateformatter = new SimpleDateFormat("yyyy-MM-dd");

        String dDOB = formatter.format(dateformatter.parse(dob));


        addParameter(PARAM_DOB, dDOB);

        getChairPersonDetails();
        loadReportHeader();

        addParameter(PARAM_CONSENT_TYPE, STRING_STUDENT);

        if (studentPerson != null) {
            buildParentForm(studentPerson);
        }

        if (m_consentForm.rowCount() == 0) {
            m_consentForm.append();
        }
        m_consentForm.beforeTop();
        return m_consentForm;
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
     *
     * @param studentPerson Person
     */

    /**
     * This method builds the parent version of the form.
     * 
     * @param studentPerson
     * @return
     */
    public void buildParentForm(Person studentPerson) {
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        if (teamMembers == null || teamMembers.isEmpty()) {
            initReportFields();
        }

        loadInterAgencyDetails(studentPerson);

        // for(IepTeamMember teamMember : teamMembers)
        // {
        // String title = teamMember.getMemberRoleCode();
        // if(title != null && REFCODE_PARENT.equalsIgnoreCase(title))
        // {
        // loadInterAgencyDetails(teamMember.getPerson());
        // }
        // }
    }


    /**
     * This method builds the student version of the form.
     *
     * @param studentPerson Person
     */
    public void buildStudentForm(Person studentPerson) {
        loadInterAgencyDetails(studentPerson);
    }

    /**
     * Load the title data of the report.
     *
     * @param person void
     */

    /**
     * This method sets most of the report fields.
     * 
     * @param person
     */
    public void setAllReportFieldsExceptAgency(Person person) {
        Address parentMailingAddress = person.getResolvedMailingAddress();
        m_consentForm.append();
        m_consentForm.set(FIELD_ADDRESS_LINE1, STRING_TABS);
        m_consentForm.set(FIELD_ADDRESS_LINE2, STRING_TABS);
        if (parentMailingAddress != null) {
            m_consentForm.set(FIELD_ADDRESS_LINE1, parentMailingAddress.getAddressLine01());
            m_consentForm.set(FIELD_ADDRESS_LINE2, parentMailingAddress.getAddressLine03());
        }

        IepData iep = (IepData) getFormOwner();
        SisStudent sisStudent = iep.getStudent();

        Collection<IepDisability> dis = iep.getIepDisability();
        String disCode = "";
        String prevCode = "";

        for (IepDisability indDis : dis) {
            disCode = indDis.getDisabilityCode();
            if (prevCode != "") {
                disCode = prevCode + ", " + disCode;
            }
            prevCode = disCode;
            // String pInd = String.valueOf(indDis.getPrimaryIndicator());
        }

        addParameter(PARAM_ELIGIBILITY, disCode);


        // String sDis = String.valueOf(dis);

        // addParameter(PARAM_ELIGIBILITY, sDis);



        String studentsName = sisStudent.getNameView();
        addParameter(PARAM_FOOTER_NAMEVIEW, studentsName);

        if (studentsName != null) {
            String delims = ",";
            String[] tokens = studentsName.split(delims);

            String fName = tokens[1];
            String lName = tokens[0];

            studentsName = fName + " " + lName;
        }
        addParameter(PARAM_SNAME, studentsName);
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
            workPhone = (String) caseManager.getFieldValueByAlias("DOE STAFF WORK PHONE");
        }

        if (chairPerson != null) {
            addParameter(PARAM_CHAIR_PERSON, chairPerson.getFirstName() + " " + chairPerson.getLastName());
            workPhone = EXTN_WORK_PHONE + workPhone;
            addParameter(PARAM_CHAIR_PERSON_PHONE, workPhone);
        }
    }


    /**
     * This method initializes the report fields.
     */
    private void initReportFields() {
        m_consentForm.append();
        m_consentForm.set(FIELD_ADDRESS_LINE1, STRING_TABS);
        m_consentForm.set(FIELD_ADDRESS_LINE2, STRING_TABS);
        m_consentForm.set(FIELD_CONSENTEE_NAME, STRING_TABS);
        m_consentForm.set(FIELD_AGENCY_NAME, STRING_TABS);
        m_consentForm.set(FIELD_AGENCY_DESCRIPTION, STRING_TABS);
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
     * This method loads the interagency details of the student.
     *
     * @param studentPerson Person
     */
    private void loadInterAgencyDetails(Person studentPerson) {
        Criteria agencyCriteria = new Criteria();
        agencyCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE, "RTB0000002h15m");

        QueryByCriteria cGetAgencyQuery = new QueryByCriteria(ReferenceCode.class, agencyCriteria);

        QueryIterator attniterator = getBroker().getIteratorByQuery(
                cGetAgencyQuery);

        try {
            while (attniterator.hasNext()) {
                ReferenceCode refCode = (ReferenceCode) attniterator.next();
                m_agencyInfo.put(refCode.getCode(), refCode.getDescription());
                m_agencyInfo.put(refCode.getCode() + "Phone", refCode.getLocalizedDescription(1));
                m_agencyInfo.put(refCode.getCode() + "Web", refCode.getLocalizedDescription(2));
            }
        }

        finally {
            attniterator.close();
        }

        ExtendedDataDictionary extDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByOid(ExtendedDataDictionary.class, "ddxNjIep");
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extDictionary, getBroker().getPersistenceKey());

        Collection<IepOtherService> otherInterAgencySvcs = m_currentIep.getIepOtherServices(getBroker());
        String agencyName = null;
        String agencyNameDis = null;
        setAllReportFieldsExceptAgency(studentPerson);

        if (otherInterAgencySvcs == null || otherInterAgencySvcs.isEmpty()) {
            setAllReportFieldsExceptAgency(studentPerson);
        }

        for (IepOtherService otherInterAgencySvc : otherInterAgencySvcs) {
            String serviceType = otherInterAgencySvc.getServiceType();
            if (PARAM_SERVICE_NAME_INTERAGENCY.equals(serviceType)) {
                agencyName =
                        (String) otherInterAgencySvc.getFieldValueByAliasExtended(ALIAS_INTERAGENCY_NAME, dictionary);

                m_agencyInfo.get(agencyName + "Phone");
                m_agencyInfo.get(agencyName + "Web");
                agencyNameDis = m_agencyInfo.get(agencyName);

                m_consentForm.set(FIELD_AGENCY_NAME, agencyName != null ? agencyNameDis : "");
                m_consentForm.set(FIELD_AGENCY_PHONE, agencyName != null ? m_agencyInfo.get(agencyName + "Phone") : "");
                m_consentForm.set(FIELD_AGENCY_WEBSITE, agencyName != null ? m_agencyInfo.get(agencyName + "Web") : "");
            }
            setAllReportFieldsExceptAgency(studentPerson);
        }
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

        if (null != school.getAdministrator1()) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + " " + adminPerson1.getLastName();
            addParameter(PARAM_SKL_ADMIN1, admin1);
            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
            if (null != superintendent) {
                String[] admin2 = superintendent.split(",");
                addParameter(PARAM_SKL_ADMIN2, admin2[1] + " " + admin2[0]);
            }
        }
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);
    }
}
