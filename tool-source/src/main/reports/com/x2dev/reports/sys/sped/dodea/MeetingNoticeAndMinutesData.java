/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2022 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.sped.dodea;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisReferenceTable;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports3.engine.JRException;
import net.sf.jasperreports3.engine.util.JRLoader;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

public class MeetingNoticeAndMinutesData extends BaseFormReportJavaSource {
    /**
     * Name for the "evaluations data source" report parameter. The value is a BeanDataSource
     * object.
     */
    public static final String EVALUATIONS_DATA_SOURCE_PARAM = "evaluationsData";
    public static final String EVALS_DISPLAY_PARAM = "evalsDisplay";
    public static final String EVALS_DISPLAY_PARAM2 = "evalsDisplay2";
    public static final String EVALS_DISPLAY_PARAM3 = "evalsDisplay3";
    public static final String ACTION_DISPLAY_PARAM = "actionsDisplay";

    /**
     * Name for the "evaluations format" report parameter. The value is a java.io.InputStream
     * object.
     */
    public static final String EVALUATIONS_FORMAT_PARAM = "evaluationsFormat";

    /**
     * Name for the "reference description lookup" parameter. The value is a Map of a reference
     * code's
     * long description (field D001) keyed to the code. This map in in a Map keyed to the reference
     * table OID.
     */
    public static final String REFERENCE_LOOKUP_MAP_PARAM = "referenceLookupMap";

    /**
     * Name for the "report" input parameter. The value is an Integer.
     */
    public static final String REPORT_PARAM = "report";

    /**
     * Name for the "team member list" report parameter. This is the collection of team members that
     * attended the meeting.
     */
    public static final String TEAM_MEMBER_LIST_PARAM = "teamMembers";

    /**
     * Name for the "waivers" report parameter. This is the collection of iep meeting attendance
     * records for which a waiver should print.
     */
    public static final String WAIVERS_LIST_PARAM = "waivers";
    public static final String WAIVERS_COUNT_PARAM = "waiversCount";

    /**
     * Data for the PWN page, identical to the main report data.
     */
    public static final String PWN_DATA_PARAM = "pwnData";

    /**
     * Name for the "team member string" report parameter. The value is a String representation of
     * the list of IEP Team Members that are attending the meeting (excluding the parents).
     */
    public static final String TEAM_MEMBER_STRING_PARAM = "teamMembersAsString";

    /*
     * Sub-report constants - Minutes
     */
    private static final String MEETING_MINUTES_FORMAT_ID = "FSS-DOD-SPED-003-MINUTES";

    /*
     * Sub-report constants
     */
    private static final String MEETING_NOTIFICATION_FORMAT_ID = "FSS-DOD-SPED-003";
    private static final String MEETING_TEAM_FORMAT_ID = "FSS-DOD-SPED-003-TEAM";
    private static final String MEETING_PWN_FORMAT_ID = "FSS-DOD-SPED-003-PWN";
    private static final String MEETING_WAIVER_FORMAT_ID = "FSS-DOD-SPED-003-WAIVER";
    /*
     * Team member role constants
     */

    private static final String PARENT_GUARDIAN_ROLE = "Parent/Guardian";

    private static final String DESCRIBEEVALS_ALIAS = "csc-describe-eval";
    private static final String DESCRIBEEVALS_REFERENCE_TABLE_OID = "RTB000000pQIQd";

    private static final String PARAM_WAIVER_SUBREPORT_FORMAT = "waiverFormat";
    private static final String PARAM_TEAM_SUBREPORT_FORMAT = "teamFormat";
    private static final String PARAM_PWN_SUBREPORT_FORMAT = "pwnFormat";
    private static final String COL_SCHOOL = "school";
    private static final String COL_TEAM_MEMBER = "teamMember";
    private static final String COL_TEAM_MEMBER_ATTENDANCE = "teamMemberAttendance";
    private static final String COL_MEETING_DATE = "meetingDate";

    private Hashtable<String, String> m_evalDescCategory = new Hashtable<String, String>();
    private Hashtable<String, String> m_evalDescription = new Hashtable<String, String>();

    /**
     * @see com.x2dev.sis.tools.reports.MultiPageFormReportJavaSource#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        loadReferenceLookup();
        createactionMap();

        int report = (Integer) getParameter(REPORT_PARAM);
        if (report == 0) {
            // PWN subreport
            // addParameter(PWN_DATA_PARAM, new SimpleFormDataSource(getFormStorage(),
            //         getFormOwner(), getDictionary(), getLocale()));
            // initSubReport(MEETING_PWN_FORMAT_ID, PARAM_PWN_SUBREPORT_FORMAT);

            // Waivers subreport
            setMeetingDate();
            loadTeamMemberList();
            loadTeamMemberString();
            loadEvalsDisplay();
            initSubReport(MEETING_WAIVER_FORMAT_ID, PARAM_WAIVER_SUBREPORT_FORMAT);

            setFormatId(MEETING_NOTIFICATION_FORMAT_ID);
        } else if (report == 1) {
            // PWN subreport
            setMeetingDate();
            addParameter(PWN_DATA_PARAM, new SimpleFormDataSource(getFormStorage(),
                    getFormOwner(), getDictionary(), getLocale()));
            initSubReport(MEETING_PWN_FORMAT_ID, PARAM_PWN_SUBREPORT_FORMAT);
            
            // Team members subreport
            loadTeamMemberList();
            loadTeamMemberString();
            initSubReport(MEETING_TEAM_FORMAT_ID, PARAM_TEAM_SUBREPORT_FORMAT);

            loadEvalsDisplay();
            setFormatId(MEETING_MINUTES_FORMAT_ID);
        }

        return new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Get the meeting date, or use Today. 
     */
    private void setMeetingDate() {
        IepMeeting meeting = (IepMeeting) getFormStorage();
        Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());
        addParameter(COL_MEETING_DATE, converter.javaToString(meeting.getDate()));
    }

    /**
     * Loads a general reference lookup of codes to their "long description" (field D001). This is
     * stored in a Map keyed to the reference table OID.
     */
    private void loadReferenceLookup() {
        Map<String, Map<String, String>> referenceMap = new HashMap<String, Map<String, String>>();

        Criteria referenceCriteria = new Criteria();
        referenceCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + SisReferenceTable.COL_CATEGORY, "Special Ed.");
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, referenceCriteria);
        query.addOrderByAscending(ReferenceCode.COL_REFERENCE_TABLE_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisReferenceTable lastTable = null;
            Map<String, String> codeMap = new HashMap<String, String>();

            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();
                SisReferenceTable table = (SisReferenceTable) (code.getReferenceTable());
                String longDescription = code.getDescription();

                if (!StringUtils.isEmpty(longDescription)) {
                    if (!ObjectUtils.match(table, lastTable)) {
                        codeMap = new HashMap<String, String>();
                        referenceMap.put(table.getOid(), codeMap);
                    }

                    codeMap.put(code.getCode(), longDescription);

                    lastTable = table;
                }
            }
        } finally {
            iterator.close();
        }

        addParameter(REFERENCE_LOOKUP_MAP_PARAM, referenceMap);
    }

    /**
     * Loads the team members that attended the meeting into the proper report parameters.
     */
    private void loadTeamMemberList() {
        ReportDataGrid teamMemberGrid = new ReportDataGrid();
        ReportDataGrid waiversGrid = new ReportDataGrid();

        IepMeeting meeting = (IepMeeting) getFormStorage();
        String formattedMeetingDate = "";
        if (meeting.getDate() != null) {
            formattedMeetingDate = new SimpleDateFormat("M/dd/yyyy").format(meeting.getDate());
        }
        Collection<String> attendingTeamMemberOids = new HashSet<String>();

        Collection<IepMeetingAttendance> attendances = meeting.getMeetingAttendance(getBroker());
        for (IepMeetingAttendance attendance : attendances) {
            if (attendance.getTeamMember() != null) {
                /*
                 * There is no column in the database to indicate which team members were invited,
                 * rather the existence of an IepMeetingAttendance record itself indicates invited,
                 * so we track these here to add waivers for non-invited team members below
                 */
                attendingTeamMemberOids.add(attendance.getTeamMember().getOid());

                /*
                 * Add excused team members to the waiver grid
                 */
                if (attendance.getExcusedIndicator()) {
                    SisSchool school = attendance.getTeamMember().getIepData().getStudent().getNextSchool();
                    waiversGrid.append();
                    waiversGrid.set(COL_SCHOOL, school);
                    waiversGrid.set(COL_TEAM_MEMBER, attendance.getTeamMember());
                    waiversGrid.set(COL_TEAM_MEMBER_ATTENDANCE, attendance);
                    waiversGrid.set(COL_MEETING_DATE, formattedMeetingDate);
                }

                /*
                 * Add ALL invited team members to the team member grid.
                 */
                teamMemberGrid.append();
                teamMemberGrid.set(COL_TEAM_MEMBER, attendance.getTeamMember());
                teamMemberGrid.set(COL_TEAM_MEMBER_ATTENDANCE, attendance);

            }
        }

        if (meeting.getIepData() != null) {
            Collection<IepTeamMember> teamMembers = meeting.getIepData().getTeamMembers();
            for (IepTeamMember member : teamMembers) {
                String teamMemberOid = member.getOid();
                if (!attendingTeamMemberOids.contains(teamMemberOid)) {
                    /*
                     * Add non-invited team members to the waiver grid.
                     */
                    SisSchool school = member.getIepData().getStudent().getNextSchool();
                    waiversGrid.append();
                    waiversGrid.set(COL_SCHOOL, school);
                    waiversGrid.set(COL_TEAM_MEMBER, member);
                    waiversGrid.set(COL_MEETING_DATE, formattedMeetingDate);
                }
            }
        }

        int waiversCount = waiversGrid.rowCount();
        teamMemberGrid.beforeTop();
        waiversGrid.beforeTop();
        addParameter(TEAM_MEMBER_LIST_PARAM, teamMemberGrid);
        addParameter(WAIVERS_LIST_PARAM, waiversGrid);
        addParameter(WAIVERS_COUNT_PARAM, Integer.valueOf(waiversCount));

    }


    // Returns the SQL query building the Hash Map for Evaluation and Action
    private void createactionMap()

    {
        DataDictionaryField field = getDictionary().findDataDictionaryFieldByAlias(DESCRIBEEVALS_ALIAS);
        if (field != null && field.getReferenceTableOid() != null) {
            
            // AppGlobals.getLog().severe(" Start SQL" );
            String query = new String(
                    "SELECT  " +
                            "RCD_CODE, RCD_CATEGORY, RCD_DESCRIPTION  " +
                            "FROM dbo.REF_CODE as RCD " +
                            "WHERE RCD.RCD_RTB_OID = '" + field.getReferenceTableOid() + "'");
    
            try {
                Connection connection = getBroker().borrowConnection();
                Statement statement = connection.createStatement();
                String evalCode;
                String evalDescription;
                String evalCategory;
    
                try {
                    // AppGlobals.getLog().log(Level.INFO, " Before result set");
                    ResultSet resultSet = statement.executeQuery(query);
    
                    while (resultSet.next()) {
                        evalCode = resultSet.getString("RCD_CODE");
                        evalDescription= resultSet.getString("RCD_DESCRIPTION");
                        evalCategory = resultSet.getString("RCD_CATEGORY");
                        AppGlobals.getLog().log(Level.INFO, " Code:" + evalCode + " Category:" + evalCategory + " Desccription:" + evalDescription);
                        if (evalCategory == null) {
                            evalCategory = "";
                        }
                        if (evalDescription == null) {
                            evalDescription = evalCode;
                        }
                        m_evalDescCategory.put(evalCode, evalCategory);
                        // AppGlobals.getLog().severe(" Code:" + evalCode + " Category:" + evalCategory);
                        m_evalDescription.put(evalCode, evalDescription);
                    }
    
                    resultSet.close();
                } catch (Exception e) {
                    // Do nothing
                }
    
                statement.close();
            } catch (SQLException sqle) {
                AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
            } finally {
                getBroker().returnConnection();
            }
        }

    }


    /**
     * Loads the Evaluations considered into the proper report parameters.
     */

    private void loadEvalsDisplay() {
        StringBuilder evalDisplay = new StringBuilder(500);
        StringBuilder evalDisplay2 = new StringBuilder(500);
        int displayCounter = 0;
        StringBuilder actionDisplay = new StringBuilder(500);
        StringBuilder descrDisplay = new StringBuilder(500);
        String checkCategory = "";
        String actions = "";
        IepMeeting meetingDescribe = (IepMeeting) getFormStorage();

        /*
         * Retrieve the reference lookup
         */
        ReferenceDescriptionLookup lookup =
                (ReferenceDescriptionLookup) getParameter(ToolJavaSource.REFERENCE_LOOKUP_KEY);

        /*
         * Iterate over the selected values building a string of the descriptions
         */
        String fieldValue = (String) meetingDescribe.getFieldValueByAlias(DESCRIBEEVALS_ALIAS, getDictionary());
        Collection<String> evals = StringUtils.convertDelimitedStringToList(fieldValue, ',', true);

        for (String eval : evals) {
            actions = m_evalDescCategory.get(eval);
            // AppGlobals.getLog().severe("Start of evals Display Code:" + eval + " Action:" + actions);
            if (!(actions == "")) {
                if (displayCounter <= 8) {
                    evalDisplay.append(lookup.getDescription(DESCRIBEEVALS_REFERENCE_TABLE_OID, eval));
                    evalDisplay.append("\r");
                    displayCounter = displayCounter + 1;
                } else {
                    evalDisplay2.append(lookup.getDescription(DESCRIBEEVALS_REFERENCE_TABLE_OID, eval));
                    evalDisplay2.append("\r");
                    displayCounter = displayCounter + 1;
                }

            }
            checkCategory = actionDisplay.toString();
            if (checkCategory != null && actions != null) {
                if (!(checkCategory.contains(actions))) {
                    actionDisplay.append(lookup.getDescription(DESCRIBEEVALS_REFERENCE_TABLE_OID, actions));
                    actionDisplay.append("\r");
                }
            }

            String evalDescr = m_evalDescription.get(eval);
            descrDisplay.append(evalDescr).append("\r");
        }

        /*
         * Remove last delimiter if necessary
         */
        if (evalDisplay.length() > 0) {
            evalDisplay.replace(evalDisplay.length() - 1, evalDisplay.length(), "");
        }

        if (actionDisplay.length() > 0) {
            actionDisplay.replace(actionDisplay.length() - 1, actionDisplay.length(), "");
        }

        addParameter(EVALS_DISPLAY_PARAM, evalDisplay.toString());
        addParameter(EVALS_DISPLAY_PARAM2, evalDisplay2.toString());
        addParameter(EVALS_DISPLAY_PARAM3, descrDisplay.toString());
        addParameter(ACTION_DISPLAY_PARAM, actionDisplay.toString());
    }

    /**
     * Loads the team members that are invited to the meeting into the proper report parameters as
     * a single string.
     */
    private void loadTeamMemberString() {
        StringBuilder members = new StringBuilder(200);

        IepMeeting meeting = (IepMeeting) getFormStorage();

        Collection<IepMeetingAttendance> attendances = meeting.getMeetingAttendance(getBroker());
        for (IepMeetingAttendance attendance : attendances) {
            IepTeamMember teamMember = attendance.getTeamMember();

            if (teamMember != null && !PARENT_GUARDIAN_ROLE.equals(teamMember.getMemberRoleCode())) {
                if (teamMember.getPerson() != null) {
                    members.append(
                            teamMember.getPerson().getFirstName() + " " + (teamMember.getPerson().getLastName()));
                }

                if (!StringUtils.isEmpty(teamMember.getMemberRoleCode())) {
                    members.append(" (");
                    members.append(teamMember.getMemberRoleCode());
                    members.append(")");
                }

                members.append("; ");
            }
        }

        /*
         * Remove last delimiter, if necessary
         */
        if (members.length() > 0) {
            members.replace(members.length() - 2, members.length(), "");
        }

        addParameter(TEAM_MEMBER_STRING_PARAM, members.toString());
    }

    /**
     * load and initialize a subreport from a report ID into a parameter.
     *
     * @param subreportId
     * @param parameterName
     */
    private void initSubReport(String subreportId, String parameterName) {
        try {
            byte[] compiledFormat = ReportUtils.getReport(subreportId, getBroker()).getCompiledFormat();
            Object loadedJRReport = JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
            addParameter(parameterName, loadedJRReport);
        } catch (JRException e) {
            String message = "ERROR: Loading subreport for '" + parameterName + "' from report " + subreportId;
            message += "\n" + e.getMessage();
            this.addCustomErrorMessage(message);
        }
    }
}
