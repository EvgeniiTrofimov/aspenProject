/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.nj;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the NJ IEP meeting invitation form. This class prepares a ReportDataGrid that
 * contains a row for each page of the report. One page is printed per team member with the
 * "print invitation" field set to true.l
 * <p>
 * Special handling exists for printing blank invitations. In this case, a single copy of the letter
 * is included. The report is designed to work directly off a current "IepMeetingInvitation" object
 * on a detail page in addition to from a form-enabled area (forms manager, workflow, etc.).
 *
 * @author X2 Development Corporation
 */
public class MeetingInvitationData extends BaseFormReportJavaSource {
    // Constants for the waiver (N3W) portion of the report
    public static final String TEAM_MEMBER_PARAMETER = "teamMember";
    public static final String WAIVER_TYPE_PARAMETER = "waiverType";
    public static final String WAIVER_TYPE_EXCUSED = "excused";
    public static final String WAIVER_TYPE_NOT_INVITED = "notInvited";

    /**
     * Aliases
     */
    private static final String ALIAS_DEPT_SUPERVISOR = "DOE DEPT SUPERVISOR";
    private static final String ALIAS_SCHOOL_FAX = "DOE SCHOOL FAX";
    private static final String ALIAS_STAFF_ROLE = "DOE STAFF ROLE";
    private static final String ALIAS_STAFF_WORK_PHONE = "DOE STAFF WORK PHONE";

    private static final String COL_PAGE_IDENTIFIER = "pageIdentifier";
    private static final String COL_PAGE_NUMBER = "pageNumber";
    private static final String COL_PARAMETER_MAP = "parameters";
    private static final String COL_SUBREPORT_DATA_SOURCE = "datasource";
    private static final String COL_SUBREPORT_FORMAT = "format";

    private static final String EXTN_WORK_PHONE = "(908)284-";

    private static final String FAX = "FAX";

    /**
     * Other Constants
     */
    private static final String INVITEE_DELIMITER = " - ";

    private static final int MAX_REPORT_COL_LIMIT = 6;

    private static final String MEETING_FORM_DEF_ID = "MTG";

    private static final String MEMBER_ROLE_PARENT = "Parent/Guardian";
    private static final String MEMBER_ROLE_STUDENT = "Student";

    private static final String PAGE_1_FORMAT_ID = "SYS-SPED-NJ-MTG-P1";
    private static final String PAGE_1_PAGE_IDENTIFIER = "P1";

    private static final String PARAM_ADDRESS_LINE1 = "ADDRESS_LINE1";
    private static final String PARAM_ADDRESS_LINE2 = "ADDRESS_LINE2";
    private static final String PARAM_BUSINESS_PHONE_NO = "BUSINESS_PHONE_NO";
    private static final String PARAM_INVITEES_COL1 = "invitees1";
    private static final String PARAM_INVITEES_COL2 = "invitees2";
    private static final String PARAM_SCHOOL_ADDRESS_LINE1 = "SKL_ADDRESS_LINE1";
    private static final String PARAM_SCHOOL_ADDRESS_LINE2 = "SKL_ADDRESS_LINE2";
    private static final String PARAM_SCHOOL_FAX_NO = "SKL_FAX_NO";
    private static final String PARAM_SCHOOL_PHONE_NO = "SKL_PHONE_NO";
    private static final String PARAM_SKL_ADMIN1 = "SKL_ADMIN1";
    private static final String PARAM_SKL_ADMIN2 = "SKL_ADMIN2";
    private static final String PARAM_SKL_ADMIN3 = "SKL_ADMIN3";
    private static final String PARAM_SSS_STAFF = "sssStaff";

    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_NEW_LINE = "\n";
    private static final String STRING_SPACE = " ";

    private static final String TO_PARAMETER = "to";

    private IepData m_currentIep = null;
    private IepMeeting m_currentMeeting = null;
    /**
     * Variables
     */
    private int m_currentPageNumber = 0;
    private String m_sssStaffName;
    private Map<?, ?> m_subReports = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        loadSubReports();

        if (m_currentMeeting != null) {
            setFormStorage(m_currentMeeting);
            setFormOwner(m_currentMeeting.getIepData());
        }

        IepMeeting meeting = (IepMeeting) getFormStorage();

        if (isBlank()) {
            preparePage1(grid, new IepMeetingAttendance(getBroker().getPersistenceKey()), STRING_EMPTY, STRING_EMPTY);
        } else {
            StringBuilder inviteeStringColumn1 = new StringBuilder();
            StringBuilder inviteeStringColumn2 = new StringBuilder();
            int inviteeCount = 0;

            /*
             * Build list of invitees and collection of attendance objects for each page of the
             * invitation
             */
            List<IepMeetingAttendance> attendanceList = new LinkedList<IepMeetingAttendance>();

            Criteria attendanceCriteria = new Criteria();
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_IEP_MEETING_OID, meeting.getOid());
            attendanceCriteria.addEqualTo(IepMeetingAttendance.COL_EXCUSED_INDICATOR, Boolean.valueOf(false));

            QueryByCriteria attendanceQuery = new QueryByCriteria(IepMeetingAttendance.class, attendanceCriteria);

            attendanceQuery.addOrderByAscending(
                    IepMeetingAttendance.REL_TEAM_MEMBER + ModelProperty.PATH_DELIMITER + IepTeamMember.COL_NAME_VIEW);

            Collection<IepMeetingAttendance> attendanceMembers = getBroker().getCollectionByQuery(attendanceQuery);

            for (IepMeetingAttendance attendanceBean : attendanceMembers) {
                StringBuilder inviteeString = inviteeStringColumn1;
                if (inviteeCount++ > MAX_REPORT_COL_LIMIT) {
                    inviteeString = inviteeStringColumn2;
                }

                if (!attendanceBean.getHideNameIndicator()) {
                    inviteeString.append(attendanceBean.getTeamMember().getNameView());
                    inviteeString.append(INVITEE_DELIMITER);
                }

                inviteeString.append(attendanceBean.getTeamMember().getMemberRoleCode());
                inviteeString.append(STRING_NEW_LINE);

                attendanceList.add(attendanceBean);
            }

            for (IepMeetingAttendance attendanceBean : attendanceList) {
                preparePage1(grid, attendanceBean, inviteeStringColumn1.toString(), inviteeStringColumn2.toString());
            }
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);
        /*
         * If the form def is null, then the invitation was run from the reports menu on the meeting
         * list.
         * In this case the superclass can't determine the form definition so we hard-set it here.
         */

        if (getFormDefinition() == null) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(FormDefinition.COL_ID, MEETING_FORM_DEF_ID);

            BeanQuery beanQuery = new BeanQuery(FormDefinition.class, criteria);

            setFormDefinition((FormDefinition) getBroker().getBeanByQuery(beanQuery));

            m_currentMeeting = userData.getCurrentRecord(IepMeeting.class);
        }
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private byte[] getSubreportFormat(String pageId) {
        Report report = (Report) m_subReports.get(pageId);

        return report.getCompiledFormat();
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
     * Load Invitee Address Details.
     *
     * @param attendanceBean IepMeetingAttendance
     * @param parameterMap HashMap<String,Object>
     */
    private void loadInviteeAddressDetails(IepMeetingAttendance attendanceBean, HashMap<String, Object> parameterMap) {
        SisStudent student = m_currentIep.getStudent();
        parameterMap.put(PARAM_ADDRESS_LINE1, STRING_EMPTY);
        parameterMap.put(PARAM_ADDRESS_LINE2, STRING_EMPTY);
        String title = attendanceBean.getTeamMember().getMemberRoleCode();

        if (title != null && MEMBER_ROLE_STUDENT.equalsIgnoreCase(title)
                || MEMBER_ROLE_PARENT.equalsIgnoreCase(title)) {
            Address mailingAddress = student.getPerson().getMailingAddress();
            if (null == mailingAddress) {
                mailingAddress = student.getPerson().getPhysicalAddress();
            }

            if (mailingAddress != null) {
                parameterMap.put(PARAM_ADDRESS_LINE1, mailingAddress.getAddressLine01());
                parameterMap.put(PARAM_ADDRESS_LINE2, mailingAddress.getAddressLine03());
            }
        } else {
            SisSchool school = student.getSchool();
            SisAddress schoolAddress = school.getAddress();
            if (schoolAddress != null) {
                parameterMap.put(PARAM_ADDRESS_LINE1, schoolAddress.getAddressLine01());
                parameterMap.put(PARAM_ADDRESS_LINE2, schoolAddress.getAddressLine03());
            }
        }
    }

    /**
     * Load Report Header.
     *
     * @param parameterMap HashMap<String,Object>
     */
    private void loadReportHeader(HashMap<String, Object> parameterMap) {
        SisStudent student = m_currentIep.getStudent();
        String admin1 = STRING_EMPTY;

        parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE1, STRING_EMPTY);
        parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE2, STRING_EMPTY);
        parameterMap.put(PARAM_SCHOOL_PHONE_NO, STRING_EMPTY);
        parameterMap.put(PARAM_SKL_ADMIN1, STRING_EMPTY);
        parameterMap.put(PARAM_SKL_ADMIN2, STRING_EMPTY);
        parameterMap.put(PARAM_SKL_ADMIN3, STRING_EMPTY);

        SisSchool school = student.getSchool();
        String fax = (String) school.getFieldValueByAlias(ALIAS_SCHOOL_FAX);
        parameterMap.put(PARAM_SCHOOL_FAX_NO, !StringUtils.isEmpty(fax) ? FAX + fax : STRING_EMPTY);

        SisAddress schoolAddress = school.getAddress();
        if (schoolAddress != null) {
            if (!StringUtils.isEmpty(schoolAddress.getPhone01()) ||
                    !StringUtils.isEmpty(schoolAddress.getPhone02())) {
                parameterMap.put(PARAM_SCHOOL_PHONE_NO, (StringUtils.isEmpty(schoolAddress.getPhone01())
                        ? schoolAddress.getPhone02() : schoolAddress.getPhone01()));
            }

            parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE1, schoolAddress.getAddressLine01());
            parameterMap.put(PARAM_SCHOOL_ADDRESS_LINE2, schoolAddress.getAddressLine03());
        }

        if (null != school.getAdministrator1()) {
            SisPerson adminPerson1 = school.getAdministrator1().getPerson();
            admin1 = adminPerson1.getFirstName() + STRING_SPACE + adminPerson1.getLastName();
            parameterMap.put(PARAM_SKL_ADMIN1, admin1);

            String superintendent = (String) school.getAdministrator1().getFieldValueByAlias(ALIAS_DEPT_SUPERVISOR);
            if (null != superintendent) {
                String[] admin2 = superintendent.split(STRING_COMMA);
                parameterMap.put(PARAM_SKL_ADMIN2, admin2[1] + STRING_SPACE + admin2[0]);
            }
        }
        initSssName();
        parameterMap.put(PARAM_SSS_STAFF, m_sssStaffName);
    }

    /**
     * Loads each subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(Report.COL_ID, PAGE_1_FORMAT_ID);

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Prepares the first page.
     *
     * @param grid ReportDataGrid
     * @param attendanceBean IepMeetingAttendance
     * @param inviteesList1 String
     * @param inviteesList2 String
     */
    private void preparePage1(ReportDataGrid grid,
                              IepMeetingAttendance attendanceBean,
                              String inviteesList1,
                              String inviteesList2) {
        if (attendanceBean.getPrintInvitationIndicator()) {
            JRDataSource dataSource =
                    new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
            m_currentIep = attendanceBean.getIepData();

            ExtendedDictionaryAttributes extendDictionary = m_currentIep.getExtendedDataDictionary();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
            SisStaff staff = m_currentIep.getStaff();
            String extnWorkPhone = (String) staff.getFieldValueByAlias(ALIAS_STAFF_WORK_PHONE, dictionary);
            String businessPhoneNo =
                    (!StringUtils.isEmpty(extnWorkPhone)) ? EXTN_WORK_PHONE + extnWorkPhone : STRING_EMPTY;
            HashMap<String, Object> parameterMap = new HashMap<String, Object>();
            parameterMap.put(TO_PARAMETER, attendanceBean.getTeamMember());
            parameterMap.putAll(getParameters());
            parameterMap.put(PARAM_INVITEES_COL1, inviteesList1);
            parameterMap.put(PARAM_INVITEES_COL2, inviteesList2);
            parameterMap.put(PARAM_BUSINESS_PHONE_NO, businessPhoneNo);

            loadReportHeader(parameterMap);

            loadInviteeAddressDetails(attendanceBean, parameterMap);

            grid.append();
            grid.set(COL_SUBREPORT_DATA_SOURCE, dataSource);
            grid.set(COL_SUBREPORT_FORMAT, getSubreportFormat(PAGE_1_FORMAT_ID));
            grid.set(COL_PARAMETER_MAP, parameterMap);
            grid.set(COL_PAGE_NUMBER, Integer.valueOf(++m_currentPageNumber));
            grid.set(COL_PAGE_IDENTIFIER, PAGE_1_PAGE_IDENTIFIER);
        }
    }

}
