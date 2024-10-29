/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company
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
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.GenericFormData;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.ByteArrayInputStream;
import java.sql.Date;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class InitialIdEvaluationDeterminationPlan.
 */
public class InitialIdEvaluationDeterminationPlan extends BaseFormReportJavaSource {
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_LIAISON = "iep-itm-liaison";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";

    private static final String DATASOURCE_COVERPAGE_NAMES = "DATASOURCE_COVERPAGE_NAMES";

    private static final String FAX = "FAX";

    private static final String FIELD_NAME = "name";
    private static final String FIELD_TITLE = "title";

    private static final String PARAM_1_TEXT = "1Text";
    private static final String PARAM_2_TEXT = "2Text";
    private static final String PARAM_3_TEXT = "3Text";
    private static final String PARAM_4_TEXT = "4Text";
    private static final String PARAM_5_TEXT = "5Text";

    private static final String PARAM_ADDRESS1 = "address1";
    private static final String PARAM_ADDRESS2 = "address2";

    private static final String PARAM_CASE_MANAGER_NAME = "CASE_MANAGER_NAME";
    private static final String PARAM_CASE_MANAGER_NUMBER = "CASE_MANAGER_NUMBER";

    private static final String PARAM_CONTACT_NAME = "CONTACT_NAME";

    private static final String PARAM_EDUCATIONAL_ASSESSMENT = "educational-assess";
    private static final String PARAM_EVAL_DETERMINATION = "eval-determination";
    private static final String PARAM_III_B1 = "IIIB1";
    private static final String PARAM_III_B2 = "IIIB2";
    private static final String PARAM_III_B3 = "IIIB3";
    private static final String PARAM_III_B4 = "IIIB4";
    private static final String PARAM_III_B5 = "IIIB5";
    private static final String PARAM_III_B6 = "IIIB6";
    private static final String PARAM_III_B7 = "IIIB7";
    private static final String PARAM_III_B8 = "IIIB8";
    private static final String PARAM_III_B9 = "IIIB9";
    private static final String PARAM_INFO_AVAILABLE = "info-available";
    private static final String PARAM_IV_1 = "IV1";
    private static final String PARAM_IV_2 = "IV2";
    private static final String PARAM_IV_2_TEXT = "IV2Text";
    private static final String PARAM_MEDICAL_ASSESSMENT = "medical-assess";
    private static final String PARAM_MEETING_DATE = "meeting-date";
    private static final String PARAM_OTHER_ASSESSMENT = "other-assess";
    private static final String PARAM_OTHER_ASSESSMENT_TEXT = "other-assess-txt";
    private static final String PARAM_PRIMARY_DISABILITY = "PRIMARY_DISABILITY";
    private static final String PARAM_PSYCHO_ASSESSMENT = "psycho-assess";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_NAME = "SKL_NAME";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SECONDARY_CONTACT_NAME = "SECONDARY_CONTACT_NAME";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SOCIAL_ASSESSMENT = "social-assess";
    private static final String PARAM_SPEECH_LANG_ASSESSMENT = "speech-lang";
    private static final String PARAM_SSS_STAFF = "sssStaff";
    private static final String PARAM_STUDENT = "student";
    private static final String PARAM_STUDENT_NAME = "STUDENT_NAME";
    private static final String PARAM_SUBREPORT_COVERPAGE_NAMES = "SUB_REPORT_COVERPAGE_NAMES";

    private static final String REFCODE_PARENT = "Parent/Guardian";

    private static final String REPORT_ID_COVERPAGE_NAMES = "SPED-NJ-INIT-ID-TEAM";

    private static final String TEXT_LIAISON = "Liaison";

    private ReportDataGrid m_coverPageNamesGrid = null;
    private IepData m_currentIep = null;
    private Report m_reportCoverPageNames = null;
    private String m_sssStaffName;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected ReportDataGrid gatherData() throws Exception {
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getFormDefinition().getExtendedDataDictionary(),
                        getBroker().getPersistenceKey());

        GenericFormData formData = (GenericFormData) getFormStorage();
        m_currentIep = (IepData) getFormOwner();
        SisStudent student = m_currentIep.getStudent();
        SisSchool school = student.getSchool();
        ReportDataGrid grid = new ReportDataGrid();

        Address studentAddress = student.getPerson().getResolvedMailingAddress();
        String addressOne = (studentAddress != null ? studentAddress.getAddressLine01() : "");
        String addressTwo = (studentAddress != null ? studentAddress.getAddressLine03() : "");

        addParameters(school);
        loadCoverPageNames();

        grid.append();
        grid.set(PARAM_1_TEXT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_1_TEXT, dictionary), ""));
        grid.set(PARAM_2_TEXT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_2_TEXT, dictionary), ""));
        grid.set(PARAM_3_TEXT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_3_TEXT, dictionary), ""));
        grid.set(PARAM_4_TEXT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_4_TEXT, dictionary), ""));
        grid.set(PARAM_5_TEXT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_5_TEXT, dictionary), ""));
        grid.set(PARAM_ADDRESS1, addressOne);
        grid.set(PARAM_ADDRESS2, addressTwo);
        grid.set(PARAM_EDUCATIONAL_ASSESSMENT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_EDUCATIONAL_ASSESSMENT, dictionary), ""));
        grid.set(PARAM_EVAL_DETERMINATION, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_EVAL_DETERMINATION, dictionary), ""));
        grid.set(PARAM_INFO_AVAILABLE, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_INFO_AVAILABLE, dictionary), ""));
        grid.set(PARAM_III_B1, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B1, dictionary), ""));
        grid.set(PARAM_III_B2, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B2, dictionary), ""));
        grid.set(PARAM_III_B3, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B3, dictionary), ""));
        grid.set(PARAM_III_B4, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B4, dictionary), ""));
        grid.set(PARAM_III_B5, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B5, dictionary), ""));
        grid.set(PARAM_III_B6, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B6, dictionary), ""));
        grid.set(PARAM_III_B7, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B7, dictionary), ""));
        grid.set(PARAM_III_B8, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B8, dictionary), ""));
        grid.set(PARAM_III_B9, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_III_B9, dictionary), ""));
        grid.set(PARAM_IV_1, StringUtils.coalesce((String) formData.getFieldValueByAlias(PARAM_IV_1, dictionary), ""));
        grid.set(PARAM_IV_2, StringUtils.coalesce((String) formData.getFieldValueByAlias(PARAM_IV_2, dictionary), ""));
        grid.set(PARAM_IV_2_TEXT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_IV_2_TEXT, dictionary), ""));
        grid.set(PARAM_MEDICAL_ASSESSMENT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_MEDICAL_ASSESSMENT, dictionary), ""));
        String meetingDateAsString = (String) formData.getFieldValueByAlias(PARAM_MEETING_DATE, dictionary);
        if (!StringUtils.isEmpty(meetingDateAsString)) {
            grid.set(PARAM_MEETING_DATE, Date.valueOf(meetingDateAsString));
        } else {
            grid.set(PARAM_MEETING_DATE, null);
        }
        grid.set(PARAM_PSYCHO_ASSESSMENT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_PSYCHO_ASSESSMENT, dictionary), ""));
        grid.set(PARAM_OTHER_ASSESSMENT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_OTHER_ASSESSMENT, dictionary), ""));
        grid.set(PARAM_OTHER_ASSESSMENT_TEXT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_OTHER_ASSESSMENT_TEXT, dictionary), ""));
        grid.set(PARAM_SOCIAL_ASSESSMENT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_SOCIAL_ASSESSMENT, dictionary), ""));
        grid.set(PARAM_SPEECH_LANG_ASSESSMENT, StringUtils.coalesce((String) formData
                .getFieldValueByAlias(PARAM_SPEECH_LANG_ASSESSMENT, dictionary), ""));
        grid.set(PARAM_STUDENT, student);
        grid.beforeTop();
        return grid;
    }

    /**
     * Adds all the parameters to the report.
     *
     * @param school SisSchool
     */
    private void addParameters(SisSchool school) {
        addParameter(PARAM_SCHOOL_PHONE_NO, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, "");
        addParameter(PARAM_SCHOOL_NAME, school.getName());
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : "");

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

        String admin1 = "";
        addParameter(PARAM_SKL_ADMIN1, "");
        if (null != school.getAdministrator1()) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + " " + adminPerson1.getLastName();
            addParameter(PARAM_SKL_ADMIN1, admin1);
        }
        initSssName();
        addParameter(PARAM_SSS_STAFF, m_sssStaffName);

    }

    /**
     * This method sets parameters for the names of all people on the cover page
     * of the IEP report form.
     *
     * @return void
     */
    private void getCoverPageNamesCriteria() {
        String studentName = "";
        String caseManagerName = "";
        String schoolName = "";
        String primaryContactName = "";
        String secondaryContactName = "";
        String primaryDisability = "";
        String admin1 = "";
        IepTeamMember primaryContactTeamMember = null;
        IepTeamMember secondaryContactTeamMember = null;
        SisStudent student = m_currentIep.getStudent();
        SisSchool school = student.getSchool();
        if (student != null) {
            studentName = student.getPerson().getFirstName() + " " + student.getPerson().getLastName();
            schoolName = student.getSchool().getName();
            Collection<IepDisability> disabilities = m_currentIep.getIepDisability();
            for (IepDisability disability : disabilities) {
                if (disability.getPrimaryIndicator()) {
                    String disabilityRefCode = disability.getDisabilityCode();
                    if (!StringUtils.isEmpty(primaryDisability)) {
                        primaryDisability = primaryDisability + "; ";
                    }
                    primaryDisability = primaryDisability + disabilityRefCode;
                }
            }
        }

        SisStaff caseManager = m_currentIep.getStaff();
        if (caseManager != null) {
            caseManagerName = caseManager.getPerson().getFirstName() + " " + caseManager.getPerson().getLastName();
        }
        String caseManagerNumber =
                StringUtils.coalesce(caseManager.getPerson().getPhone01(), caseManager.getPerson().getPhone02());
        if (StringUtils.isEmpty(caseManagerNumber)) {
            caseManagerNumber = "";
        }
        Collection<IepTeamMember> teamMembers = m_currentIep.getTeamMembers();
        m_coverPageNamesGrid = new ReportDataGrid();
        for (IepTeamMember member : teamMembers) {
            if (null != member) {

                String title = member.getMemberRoleCode();
                Person person = member.getPerson();
                String name = "";

                if (person != null) {
                    name = person.getFirstName() + " " + person.getLastName();
                }
                if (title != null && REFCODE_PARENT.equalsIgnoreCase(title)) {
                    if (primaryContactTeamMember == null) {
                        primaryContactTeamMember = member;
                        primaryContactName = name;
                    } else {
                        if ((primaryContactTeamMember.getFormPriority() == null && member.getFormPriority() != null) ||
                                (primaryContactTeamMember.getFormPriority() != null && member.getFormPriority() != null
                                        &&
                                        Integer.parseInt(primaryContactTeamMember.getFormPriority()) > Integer
                                                .parseInt(member.getFormPriority()))) {
                            secondaryContactTeamMember = primaryContactTeamMember;
                            secondaryContactName = primaryContactName;
                            primaryContactTeamMember = member;
                            primaryContactName = name;
                        } else if (secondaryContactTeamMember == null ||
                                (secondaryContactTeamMember.getFormPriority() != null
                                        && member.getFormPriority() != null &&
                                        Integer.parseInt(secondaryContactTeamMember.getFormPriority()) > Integer
                                                .parseInt(member.getFormPriority()))) {
                            secondaryContactTeamMember = member;
                            secondaryContactName = name;
                        }
                    }
                }
                String isLiaison = (String) member.getFieldValueByAlias(ALIAS_LIAISON, getDictionary());
                if (BooleanAsStringConverter.TRUE.equals(isLiaison)) {
                    title = title + ";" + " " + TEXT_LIAISON;
                }
                m_coverPageNamesGrid.append();
                m_coverPageNamesGrid.set(FIELD_TITLE, title);
                m_coverPageNamesGrid.set(FIELD_NAME, name);
            }
        }

        addParameter(PARAM_SCHOOL_PHONE_NO, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE1, "");
        addParameter(PARAM_SCHOOL_ADDRESS_LINE2, "");
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        addParameter(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : "");

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

        addParameter(PARAM_SKL_ADMIN1, "");
        addParameter(PARAM_SKL_ADMIN2, "");
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

        m_coverPageNamesGrid.beforeTop();
        addParameter(DATASOURCE_COVERPAGE_NAMES, m_coverPageNamesGrid);

        addParameter(PARAM_CASE_MANAGER_NAME, caseManagerName);
        addParameter(PARAM_CASE_MANAGER_NUMBER, caseManagerNumber);
        addParameter(PARAM_CONTACT_NAME, primaryContactName);
        addParameter(PARAM_PRIMARY_DISABILITY, primaryDisability);
        addParameter(PARAM_SCHOOL_NAME, schoolName);
        addParameter(PARAM_SECONDARY_CONTACT_NAME, secondaryContactName);
        addParameter(PARAM_STUDENT_NAME, studentName);
    }

    /**
     * This method sets the input stream for the cover page names sub report.
     *
     * @return void
     */
    private void getCoverPageNamesSubRptCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, REPORT_ID_COVERPAGE_NAMES);
        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
        m_reportCoverPageNames = (Report) getBroker().getBeanByQuery(query);
        addParameter(PARAM_SUBREPORT_COVERPAGE_NAMES,
                new ByteArrayInputStream(m_reportCoverPageNames.getCompiledFormat()));
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
     * Load cover page names.
     */
    private void loadCoverPageNames() {
        getCoverPageNamesSubRptCriteria();
        getCoverPageNamesCriteria();
    }
}
