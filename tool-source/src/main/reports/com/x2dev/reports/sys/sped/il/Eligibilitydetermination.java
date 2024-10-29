/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.il;

import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.x2dev.procedures.sys.sped.il.SpedIlWorkflowCommonProcedure;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.utils.StringUtils;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
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
public class Eligibilitydetermination extends BeanReport {
    private static final String DEFAULT_SIGNATURE_ROWS_4 = "4";
    private static final String EMPTY = "";
    private static final String KEY_DATASOURCE = "datasource";
    private static final String KEY_FORMATS_MAP = "formatsMap";
    private static final String KEY_IS_BLANK = "isBlank";
    private static final String KEY_NAME_AND_TITLE1 = "nameAndTitle";
    private static final String KEY_NAME_AND_TITLE2 = "nameAndTitle2";
    private static final String KEY_SIZE_MEMBER = "sizeMember";
    private static final String PARAM_SIGNATUREROWS = "signaturerows";
    private static final String SUB_REPORT1_KEY = "DE-TEAMS1";

    private static final String SUB_REPORT_ID_DE1 = "SYS-SPED-IL-DE-SUB1";


    private Map<String, JRDataSource> m_datasourceMap = null;
    private Map<String, InputStream> m_formatMap = null;

    private static final long serialVersionUID = 1L;


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        IepData iepData = (IepData) getFormStorage();
        Boolean isBlank = Boolean.valueOf(iepData.getOid() == null);
        addParameter(KEY_IS_BLANK, isBlank);
        prepareSubReport();

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * return meeting where logic will get team member attendance<br>
     * current implementation get any meeting with type listed in getTargetMeetingTypes.
     *
     * @return Iep meeting
     * @ see {@link Eligibilitydetermination#getTargetMeetingTypes()}
     */
    private IepMeeting getAppropriateMeeting() {
        IepMeeting returnMeeting = null;
        IepData iepData = (IepData) getFormOwner();
        if (iepData != null && !isBlank()) {
            for (IepMeeting iepMeeting : iepData.getIepMeeting()) {
                iepMeeting.getTypeCodeEnum().equals(IepMeeting.TypeCode.INITIAL);
                String meetingType = (String) iepMeeting.getFieldValueByAlias("mtg-type", getDictionary());
                meetingType = meetingType == null ? EMPTY : meetingType;
                if (getTargetMeetingTypes().contains(meetingType)) {
                    returnMeeting = iepMeeting;
                    break;
                }

            }

        }
        return returnMeeting;
    }

    /**
     * return number of team for current row<br>
     * maximum size for one row - 2 team member<br>
     * can return 1 or 2 value;.
     *
     * @param currentRow current row
     * @param teamMemberSize total team member size
     * @return int
     */
    private int getCountOfMemberForCurrentRow(int currentRow, int teamMemberSize) {
        int rows = ((int) Math.ceil(teamMemberSize / 2.0));
        int countMember = 2;
        if (currentRow == rows - 1 && teamMemberSize % 2 > 0) {
            countMember = 1;
        }
        return countMember;
    }

    /**
     * return name and title for each input IepTeamMember<br>
     * .
     *
     * @param invitedTeams Set<IepTeamMember>
     * @return List
     */
    private List<String> getListOfNameAndTitle(Set<IepTeamMember> invitedTeams) {
        List<String> nameTitleList = new ArrayList<String>();
        for (IepTeamMember teamMember : invitedTeams) {
            nameTitleList.add(getNameAndTitle(teamMember));
        }
        return nameTitleList;
    }

    /**
     * return name and title for input IepTeamMember.
     *
     * @param teamMember IepTeamMember
     * @return String
     */
    private String getNameAndTitle(IepTeamMember teamMember) {
        String nameTitle = null;
        StringBuilder memberNameBuilder = new StringBuilder();
        if (!StringUtils.isEmpty(teamMember.getNameView())) {
            memberNameBuilder.append(teamMember.getNameView());
            memberNameBuilder.append(" , ");
        }

        if (!StringUtils.isEmpty(teamMember.getMemberRoleCode())) {
            memberNameBuilder.append(teamMember.getMemberRoleCode());
        }
        nameTitle = memberNameBuilder.toString();
        return nameTitle == null ? EMPTY : nameTitle;


    }

    /**
     * return name and title for current row<br>
     * one row it is two values from list<br>
     * return String[] with 2 length, if name and title doesn't exist - return empty values.
     *
     * @param nameTitleList List<String>
     * @param currentRow int
     * @return String[]
     */
    private String[] getNameAndTitleForCurrentRow(List<String> nameTitleList, int currentRow) {
        String[] nameAndTitle = {EMPTY, EMPTY};
        int incrementer = currentRow * 2;
        for (int j = 0; j < 2; j++) {
            if (incrementer < nameTitleList.size()) {
                nameAndTitle[j] = nameTitleList.get(incrementer);
                incrementer++;
            }
        }
        return nameAndTitle;
    }

    /**
     * return invited team member and sort it by name.
     *
     * @return Sets the
     */
    private Set<IepTeamMember> getSortedInvitedTeamMember() {
        IepMeeting targetMeeting = getAppropriateMeeting();
        Set<IepTeamMember> invitedTeams = new TreeSet<IepTeamMember>(getTeamComparator());
        if (targetMeeting != null) {
            IepData iepData = (IepData) getFormOwner();
            MeetingAttendanceManager attendanceManager =
                    new MeetingAttendanceManager(targetMeeting.getOid(), iepData, getBroker());
            for (IepTeamMember teamMember : iepData.getTeamMembers()) {
                if (attendanceManager.getInvited(teamMember.getOid())) {
                    invitedTeams.add(teamMember);
                }
            }

        }
        return invitedTeams;
    }

    /**
     * List contain for each workflow only one meeting type which will use for determine attendance
     * .
     *
     * @return List
     */
    private List<String> getTargetMeetingTypes() {
        List<String> returnList = new ArrayList<String>();
        returnList.add(SpedIlWorkflowCommonProcedure.MeetingTypes.INITIAl_ELIGIBILITY.toString());
        returnList.add(SpedIlWorkflowCommonProcedure.MeetingTypes.RE_EVALUATION_ELIGIBILITY.toString());
        returnList.add(SpedIlWorkflowCommonProcedure.MeetingTypes.AMENDMENT.toString());
        returnList.add(SpedIlWorkflowCommonProcedure.MeetingTypes.ANNUAL_REVIEW.toString());
        returnList.add(SpedIlWorkflowCommonProcedure.MeetingTypes.TRANSFER_IN.toString());

        return returnList;
    }

    /**
     * comparator for sort IepTeamMember.
     *
     * @return Comparator
     */
    private Comparator<IepTeamMember> getTeamComparator() {
        Comparator<IepTeamMember> comparator = new Comparator<IepTeamMember>() {

            @Override
            public int compare(IepTeamMember o1, IepTeamMember o2) {
                String nameView0 = o1 == null ? EMPTY : o1.getNameView();
                nameView0 = nameView0 == null ? EMPTY : nameView0;
                String nameView1 = o2 == null ? EMPTY : o2.getNameView();
                nameView1 = nameView1 == null ? EMPTY : nameView1;
                return nameView0.compareTo(nameView1);
            }
        };
        return comparator;
    }

    /**
     * return count team member which will print in report<br>
     * team member count for blank report get form input definition<br>
     * team member count for "report with data" get form "team member attendance" record for target
     * meeting<br>
     * Workflow can contain few meeting and only one meeting type used like target .
     *
     * @param nameTitleList List<String>
     * @return int
     */
    private int getTeamMemberCount(List<String> nameTitleList) {
        int size;
        Integer value = (Integer) getParameter(PARAM_SIGNATUREROWS);
        value = value == null ? Integer.valueOf(DEFAULT_SIGNATURE_ROWS_4) : value;

        if (isBlank()) {
            size = value.intValue() * 2;
        } else {
            size = nameTitleList.size();

            int sizeFromINput = value.intValue() * 2;
            size = size < sizeFromINput ? sizeFromINput : size;
        }
        return size;
    }

    /**
     * prepare Grid for subreport which contain team member signature.
     *
     * @param key String
     * @param teamMemberCount int
     * @param nameTitleList List<String>
     */
    private void prepareGrid(String key, int teamMemberCount, List<String> nameTitleList) {
        ReportDataGrid grid = new ReportDataGrid();
        int rows = ((int) Math.ceil(teamMemberCount / 2.0));
        for (int i = 0; i < rows; i++) {

            String[] nameAndTitle = getNameAndTitleForCurrentRow(nameTitleList, i);
            int countMemberOnRow = getCountOfMemberForCurrentRow(i, teamMemberCount);
            grid.append();
            grid.set(KEY_SIZE_MEMBER, Integer.valueOf(countMemberOnRow));
            grid.set(KEY_NAME_AND_TITLE1, nameAndTitle[0]);
            grid.set(KEY_NAME_AND_TITLE2, nameAndTitle[1]);
        }
        grid.beforeTop();
        m_datasourceMap.put(key, grid);
    }

    /**
     * prepare all sub reports for this report.
     */
    private void prepareSubReport() {
        Report subreport = ReportUtils.getReport(SUB_REPORT_ID_DE1, getBroker());
        ByteArrayInputStream format = new ByteArrayInputStream(subreport.getCompiledFormat());
        Set<IepTeamMember> invitedTeams = getSortedInvitedTeamMember();
        List<String> listOfNameAndTitle = getListOfNameAndTitle(invitedTeams);

        m_datasourceMap = new HashMap<String, JRDataSource>();
        m_formatMap = new HashMap<String, InputStream>();

        int teamMemberCount = getTeamMemberCount(listOfNameAndTitle);
        m_formatMap.put(SUB_REPORT1_KEY, format);
        prepareGrid(SUB_REPORT1_KEY, teamMemberCount, listOfNameAndTitle);


        addParameter(KEY_DATASOURCE, m_datasourceMap);
        addParameter(KEY_FORMATS_MAP, m_formatMap);
    }

}
