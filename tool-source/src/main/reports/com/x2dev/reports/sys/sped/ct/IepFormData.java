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
package com.x2dev.reports.sys.sped.ct;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SimpleBeanDataSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the Connecticut IEP form.
 *
 * @author X2 Development Corporation
 */
public class IepFormData extends BaseFormReportJavaSource {
    // IEP aliases used in calculating service hours.
    private static final String ALIAS_LENGTH_DAY = "iep-school-day";
    private static final String ALIAS_DAYS_WEEK = "iep-days-per-week";
    private static final String ALIAS_TOTAL_HOURS = "iep-total-hours-per-week";
    private static final String ALIAS_SPED_HOURS = "iep-sped-hours-per-week";
    private static final String ALIAS_PEER_HOURS = "iep-hours-non-disabled-peers";
    private static final String ALIAS_PEER_PERCENT = "iep-percent-non-disabled-peers";

    // The subreports RPT_ID's
    private static final String PARAM_PAGE_3_ACTIONS_PROPOSED_FORMAT_ID = "SYS-SPED-CT-IEP-AP";
    private static final String PARAM_PAGE_3_ACTIONS_REFUSED_FORMAT_ID = "SYS-SPED-CT-IEP-AR";
    private static final String PARAM_PAGE_7_FORMAT_ID = "SYS-SPED-CT-IEP-PG7";
    private static final String PARAM_PAGE_11_FORMAT_ID = "SYS-SPED-CT-IEP-PG11";

    // The subreports parameter name
    private static final String PARAM_PAGE3_ACTIONS_PROPOSED_FORMAT = "page3_ActionsProposed";
    private static final String PARAM_PAGE3_ACTIONS_REFUSED_FORMAT = "page3_ActionsRefused";
    private static final String PARAM_PAGE7_FORMAT = "page7";
    private static final String PARAM_PAGE11_FORMAT = "page11";

    // Parameters to be passed into the report
    private static final String CREDITS_FORMAT = "##0.0";
    private static final String PARAM_ACTIONS_PROPOSED = "actionsProposed";
    private static final String PARAM_ACTIONS_REFUSED = "actionsRefused";
    private static final String PARAM_ATTENDEE_NAME = "attendeeName";
    private static final String PARAM_ATTENDEE_TITLE = "attendeeTitle";
    private static final String PARAM_DATE_AS_STRING_CONVERTER = "dateAsString";
    private static final String PARAM_IEP = "iep";
    private static final String PARAM_CONTACT_0 = "contact0";
    private static final String PARAM_CONTACT_1 = "contact1";
    private static final String PARAM_MEETING = "meeting";
    private static final String PARAM_RACE = "race";
    private static final String PARAM_CREDITS = "credits";
    private static final String PARAM_AMENDMENT_DETAILS = "amendmentDetails";
    private static final String PARAM_PRIMARY_DISABILITY = "primaryDisability";
    private static final String PARAM_GOALS = "goals";
    private static final String PARAM_SERVICES = "services";
    private static final String PARAM_DISTRICT_WEBSITE = "districtWebsite";
    private static final String PARAM_PRINT_AS_DRAFT = "printAsDraft";
    private static final String PARAM_CALC_SERVICE = "calcServiceHours";
    private static final String PARAM_PPT_RECOMMENDATIONS = "pptRecommendations";
    private static final String PARAM_PROJECTED_MEETING_DATE = "nextProjectedMeetingDate";
    private static final String PARAM_SAFEGUARDS_DATE = "safeguardsDate";
    // Aliases used by report
    private static final String ALIAS_PPT_RECOMMENDATIONS = "img-ppt-recommendation";

    // Reference Table Oids used by the report
    private static final String SERVICE_SITE_REF_TABLE_OID = "rtbCTSetting";

    private final SimpleDateFormat PARAM_STRING_FORMAT = new SimpleDateFormat("MM/dd/yyyy");
    private final SimpleDateFormat PARAM_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");

    private IepData m_currentIep = null;
    private Map<String, Report> m_subReports = null;
    private Boolean m_calcService;

    private Map<String, ReferenceCode> m_serviceSiteRefCodes = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        loadSubReports();

        loadTransitionSvcsRefCodes();

        m_calcService = (Boolean) getParameter(PARAM_CALC_SERVICE);

        Boolean printAsDraft = (Boolean) getParameter(PARAM_PRINT_AS_DRAFT);
        addParameter(PARAM_PRINT_AS_DRAFT, printAsDraft);

        Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
        addParameter(PARAM_DATE_AS_STRING_CONVERTER, converter);

        IepData iep = getIep();
        addParameter(PARAM_IEP, iep);

        preparePage1();
        preparePage3();
        preparePage4_5();
        preparePage7();
        preparePage8();
        preparPage9();
        preparePage11();

        if (m_calcService != null ? m_calcService.booleanValue() : false) {
            calculateServiceHours(iep);
        }

        SimpleFormDataSource dataSource =
                new SimpleFormDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
        return dataSource;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));
            addFormParameters();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#saveState(com.x2dev.sis.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);
    }

    /**
     * Calculate the service hours for the IEP. These are calculate from IEP hours and Service
     * record hours.
     * The results are stored on the IEP.
     * <ul>
     * <li>Total = (hours per day) * (days per week)</li>
     * <br>
     * sped =
     *
     * @param iep IepData
     * @return boolean
     */
    private boolean calculateServiceHours(IepData iep) {
        boolean changes = false;
        double totalHours = 0.0;
        double spedHours = 0.0;
        double peerHours = 0.0;

        // Get total service hours from hours/day * days/week.
        String lengthDayStr = (String) iep.getFieldValueByAlias(ALIAS_LENGTH_DAY, getDictionary());
        String daysWeekStr = (String) iep.getFieldValueByAlias(ALIAS_DAYS_WEEK, getDictionary());
        if (!StringUtils.isEmpty(lengthDayStr) && !StringUtils.isEmpty(daysWeekStr)) {
            try {
                double lengthDay = Double.parseDouble(lengthDayStr);
                double daysWeek = Double.parseDouble(daysWeekStr);
                totalHours = lengthDay * daysWeek;
            } catch (NumberFormatException nfe) {
                // Do nothing. The values are not set.
            }
        }

        // SPED hours is sum of service hours from SPED services.
        Collection<IepService> services = iep.getIepServices();
        for (IepService service : services) {
            if (service.getServiceMode().equals("SpecialEd")) {
                BigDecimal frequency = service.getFrequency();
                int duration = service.getDuration();
                double cycleFactor = 1.0;
                if (service.getCycle() != null && service.getCycle().contains("10")) {
                    cycleFactor = 0.5;
                } else if (service.getCycle() != null && service.getCycle().equals("Other")) {
                    if (service.getDaysPerCycle() > 7) {
                        cycleFactor = 0.5;
                    }
                }
                if (frequency != null) {
                    spedHours += frequency.doubleValue() * duration * cycleFactor / 60;
                }
            }
        }

        // Peer hours is total hours minus sped hours.
        peerHours = totalHours - spedHours;
        DecimalFormat format = new DecimalFormat("##0.00");

        // Update the IEP.
        changes = false;
        String origVal = (String) iep.getFieldValueByAlias(ALIAS_TOTAL_HOURS, getDictionary());
        String tempVal = format.format(totalHours);
        if (origVal == null || !origVal.equals(tempVal)) {
            iep.setFieldValueByAlias(ALIAS_TOTAL_HOURS, tempVal, getDictionary());
            changes = true;
        }

        origVal = (String) iep.getFieldValueByAlias(ALIAS_SPED_HOURS, getDictionary());
        tempVal = format.format(spedHours);
        if (origVal == null || !origVal.equals(tempVal)) {
            iep.setFieldValueByAlias(ALIAS_SPED_HOURS, tempVal, getDictionary());
            changes = true;
        }

        origVal = (String) iep.getFieldValueByAlias(ALIAS_PEER_HOURS, getDictionary());
        tempVal = format.format(peerHours);
        if (origVal == null || !origVal.equals(tempVal)) {
            iep.setFieldValueByAlias(ALIAS_PEER_HOURS, tempVal, getDictionary());
            changes = true;
        }

        origVal = (String) iep.getFieldValueByAlias(ALIAS_PEER_PERCENT, getDictionary());
        tempVal = totalHours > 0 ? format.format(peerHours / totalHours * 100) : "0";
        if (origVal == null || !origVal.equals(tempVal)) {
            iep.setFieldValueByAlias(ALIAS_PEER_PERCENT, tempVal, getDictionary());
            changes = true;
        }

        if (changes) {
            getBroker().saveBeanForced(iep);
        }

        return changes;
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned.
     *
     * @return IepData
     */
    private IepData getIep() {
        IepData iep = null;

        if (isBlank()) {
            IepData ownerIep = (IepData) getFormOwner();

            iep = new IepData(getBroker().getPersistenceKey());
            iep.setStudentOid(ownerIep.getStudentOid());
            iep.setStaffOid(ownerIep.getStaffOid());
        } else {
            iep = (IepData) getFormStorage();
        }

        return iep;
    }

    /**
     * Returns the format of subreport for the given page constant.
     *
     * @param pageId String
     * @return byte[]
     */
    private InputStream getSubreportFormat(String pageId) {
        Report report = m_subReports.get(pageId);
        return new ByteArrayInputStream(report.getCompiledFormat());
    }

    /**
     * Loads each IEP subreport into a map for fast retrieval.
     */
    private void loadSubReports() {
        Criteria criteria = new Criteria();
        // criteria.addEqualTo(Report.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addIn(Report.COL_ID, Arrays.asList(new String[] {PARAM_PAGE_3_ACTIONS_REFUSED_FORMAT_ID,
                PARAM_PAGE_3_ACTIONS_PROPOSED_FORMAT_ID,
                PARAM_PAGE_7_FORMAT_ID,
                PARAM_PAGE_11_FORMAT_ID}));

        QueryByCriteria query = new QueryByCriteria(Report.class, criteria);

        m_subReports = getBroker().getMapByQuery(query, Report.COL_ID, 8);
    }

    /**
     * Prepares page 1 of the CT IEP Form (ED620)
     *
     * Grabs the IEP's latest meeting and sets the meeting attendances into parameters:
     *
     * "contact0" = The student's primary contact (StudentContact)
     * "contact1" = The student's secondary contact (StudentContact)
     * "race" = The student's race (String)
     * "credits" = The student's HS credit (String)
     * "amendmentDetails" = The latest meeting's amendment details.
     */
    private void preparePage1() {

        IepData iep = getIep();

        iep.getStudent().getPerson().getGenderCode();

        // Set the most recent meeting as the meeting of record for printing the report.
        IepMeeting meeting = getLatestMeeting();
        addParameter(PARAM_MEETING, meeting);
        if (meeting != null) {
            addAttendanceParameters(meeting);
        }

        // Set the contacts.
        List<StudentContact> contacts = SpedUtils.getStudentContacts(iep, getBroker(), 2);
        if (!contacts.isEmpty()) {
            addParameter(PARAM_CONTACT_0, contacts.get(0));
        }
        if (contacts.size() >= 2) {
            addParameter(PARAM_CONTACT_1, contacts.get(1));
        }

        // Set the student race codes indicators.
        addParameter(PARAM_RACE, getRaces(iep));

        // Set the student HS credits count as a string.
        addParameter(PARAM_CREDITS, getCredits(iep.getStudent()));

        // Set the student Primary Disability as a string. PARAM_PRIMARY_DISABILITY
        if (iep.getPrimaryDisability() != null) {
            addParameter(PARAM_PRIMARY_DISABILITY, iep.getPrimaryDisability().getDisabilityCode());
        }

        addParameter(PARAM_AMENDMENT_DETAILS, getAmendmentDetails(iep));

        // Retrieve and set the ppt recommendations
        ExtendedDictionaryAttributes extendDictionary = iep.getExtendedDataDictionary();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
        if (meeting != null) {
            addParameter(PARAM_PPT_RECOMMENDATIONS,
                    meeting.getFieldValueByAlias(ALIAS_PPT_RECOMMENDATIONS, dictionary));
        }

        // Determine and set the next projected meeting date
        PlainDate nextProjectedMeetingDate = iep.getNextReviewDate();
        if (iep.getNextEvaluationDate() != null &&
                (nextProjectedMeetingDate == null ||
                        iep.getNextEvaluationDate().before(nextProjectedMeetingDate))) {
            nextProjectedMeetingDate = iep.getNextEvaluationDate();
        }
        addParameter(PARAM_PROJECTED_MEETING_DATE, nextProjectedMeetingDate);
    }

    /**
     * Retrieves the IEP's latest meeting.
     *
     * @return IepMeeting
     */
    private IepMeeting getLatestMeeting() {
        IepData iep = (IepData) getFormOwner();
        PlainDate today = new PlainDate(Calendar.getInstance().getTime());

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, iep.getOid());
        criteria.addLessOrEqualThan(IepMeeting.COL_DATE, today);

        QueryByCriteria query = new QueryByCriteria(IepMeeting.class, criteria);
        query.addOrderByDescending(IepMeeting.COL_DATE);
        query.addOrderByDescending(IepMeeting.COL_TIME);

        return (IepMeeting) getBroker().getBeanByQuery(query);
    }

    /**
     * Calculate and return the students HS total credits.
     *
     * @param student Student
     * @return String
     */
    private Object getCredits(Student student) {
        double credits = 0;
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Transcript.COL_STUDENT_OID, student.getOid());
        criteria.addGreaterThan(Transcript.COL_TOTAL_CREDIT, Integer.valueOf(0));
        QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Transcript transcript = (Transcript) iterator.next();
                if (transcript.getTotalCredit() != null) {
                    credits += transcript.getTotalCredit().doubleValue();
                }
            }
        } finally {
            iterator.close();
        }

        DecimalFormat formatter = new DecimalFormat(CREDITS_FORMAT);
        String formattedCredits = "";
        if (credits > 0) {
            formattedCredits = formatter.format(credits);
        }
        return formattedCredits;
    }

    /**
     * Identify attending team members (by type) and assign them to an array (IepTeamMember).
     *
     * Members with the following roles will be placed in the following position:
     *
     * - meetingAtt0 = The Administrator (IepTeamMember)
     * - meetingAtt1 = The Parent (IepTeamMember)
     * - meetingAtt2 = The Parent (IepTeamMember)
     * - meetingAtt3 = The Surrogate (IepTeamMember)
     * - meetingAtt4 = The Student (IepTeamMember)
     * - meetingAtt5 = The Regular Ed. Teacher (IepTeamMember)
     * - meetingAtt6 = The Special Ed. Teacher (IepTeamMember)
     * - meetingAtt7 = The School Psychologist (IepTeamMember)
     * - meetingAtt8 = The Social Worker (IepTeamMember)
     * - meetingAtt9 = The Speech Therapist (IepTeamMember)
     * - meetingAtt10 = The Guidance Counselor (IepTeamMember)
     * - meetingAtt11 = The Nurse (IepTeamMember)
     * - meetingAtt12 = The Occupational Therapist (IepTeamMember)
     * - meetingAtt13 = The Physical Therapist (IepTeamMember)
     * - meetingAtt14 = The Agency (IepTeamMember)
     * 
     * Any roles that do not fit these will be placed in meetingAtt15, meetingAtt15, meetingAtt16,
     * and meetingAtt17
     *
     * @param meeting IEPMeeting
     */
    private void addAttendanceParameters(IepMeeting meeting) {
        int attendeeCount = 0;

        for (IepMeetingAttendance attendent : meeting.getMeetingAttendance()) {
            IepTeamMember member = attendent.getTeamMember();

            addParameter(PARAM_ATTENDEE_TITLE + attendeeCount, member.getMemberRoleCode() + ":");
            addParameter(PARAM_ATTENDEE_NAME + attendeeCount, member.getNameView());

            attendeeCount++;
        }

    }

    /**
     * Returns a concatenated string of the descriptions in the IEP Amendment details.
     *
     * @param iep IepData
     * @return String
     */
    private Object getAmendmentDetails(IepData iep) {
        String details = "";
        for (IepAmendment amendment : iep.getIepAmendments()) {
            for (IepAmendmentDetail detail : amendment.getIepAmendmentDetails()) {
                if (!StringUtils.isEmpty(details)) {
                    details += " ";
                }
                details += detail.getDescription();
            }
        }
        return details;
    }

    /**
     * Return a string representation of the students race and ethnicity codes.
     * The string can include one or more of:
     * H - Hispanic indicator set.
     * A, B, C, D, E - if student race codes include the various race codes.
     *
     * @param iep IepData
     * @return String of race codes.
     */
    private String getRaces(IepData iep) {
        String races = "";
        Student student = iep.getStudent();
        if (student != null) {
            Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();
            DataDictionary d = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField ddField = d.findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
            if (ddField != null) {
                ReferenceTable refTable = ddField.getReferenceTable();
                if (refTable != null) {
                    for (ReferenceCode refCode : refTable.getReferenceCodes()) {
                        refCodes.put(refCode.getCode(), refCode);
                    }
                }
            }

            if (student.getPerson() != null && student.getPerson().getHispanicLatinoIndicator()) {
                races += "H";
            }

            for (Race race : student.getPerson().getRaces()) {
                ReferenceCode code = refCodes.get(race.getRaceCode());
                if (code != null) {
                    if ("1".equals(code.getStateCode())) {
                        races += "A";
                    } else if ("2".equals(code.getStateCode())) {
                        races += "B";
                    } else if ("4".equals(code.getStateCode())) {
                        races += "C";
                    } else if ("8".equals(code.getStateCode())) {
                        races += "D";
                    } else if ("16".equals(code.getStateCode())) {
                        races += "E";
                    }
                }
            }
        }
        return races;
    }

    /**
     * This method loads the transition services reference codes.
     */
    private void loadTransitionSvcsRefCodes() {
        Criteria refCodeCriteria = new Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID,
                SERVICE_SITE_REF_TABLE_OID);
        QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
        refCodeQuery.addOrderByAscending(ReferenceCode.COL_SEQUENCE_NUMBER);
        m_serviceSiteRefCodes = getBroker().getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 8);
    }

    /**
     * Prepares page 3 (Prior Written Notice) of the CT IEP Form (ED620)
     *
     * Gets the IEP's IepPlacements into a Collection and puts it into the "placements" parameter
     * (BeanCollectionDataSource).
     */
    private void preparePage3() {
        IepData iep = getIep();

        IepPlacement actionsProposed = new IepPlacement(getBroker().getPersistenceKey());
        IepPlacement actionsRefused = new IepPlacement(getBroker().getPersistenceKey());

        addParameter(PARAM_PAGE3_ACTIONS_PROPOSED_FORMAT, getSubreportFormat(PARAM_PAGE_3_ACTIONS_PROPOSED_FORMAT_ID));
        addParameter(PARAM_PAGE3_ACTIONS_REFUSED_FORMAT, getSubreportFormat(PARAM_PAGE_3_ACTIONS_REFUSED_FORMAT_ID));

        Collection<IepPlacement> placements = iep.getPlacements();

        for (IepPlacement placement : placements) {
            String actionType = (String) placement.getFieldValueByAlias("ipa-action-type", getDictionary());
            if (actionType.equals("Proposed")) {
                actionsProposed = placement;
            } else if (actionType.equals("Refused")) {
                actionsRefused = placement;
            }
        }

        addParameter(PARAM_ACTIONS_PROPOSED, new SimpleBeanDataSource(actionsProposed, getDictionary(), getLocale()));
        addParameter(PARAM_ACTIONS_REFUSED, new SimpleBeanDataSource(actionsRefused, getDictionary(), getLocale()));
        addParameter(PARAM_DISTRICT_WEBSITE, getParameter(PARAM_DISTRICT_WEBSITE));

        // Determine and set safeguards date.
        ExtendedDictionaryAttributes extendDictionary = iep.getExtendedDataDictionary();
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendDictionary, getBroker().getPersistenceKey());
        String safeguardsDateString = (String) iep.getFieldValueByAlias("iep-safeguards-date", dictionary);

        if (safeguardsDateString == null) {
            safeguardsDateString = "N/A";
        }

        else {
            try {
                Date safeguardsDate = PARAM_DATE_FORMAT.parse(safeguardsDateString);
                safeguardsDateString = PARAM_STRING_FORMAT.format(safeguardsDate);
            } catch (ParseException e) {
                // do nothing, not a date
            }
        }
        addParameter(PARAM_SAFEGUARDS_DATE, safeguardsDateString);
    }

    /**
     * Prepares page 4 and 5 (PRESENT LEVELS OF ACADEMIC ACHIEVEMENT AND FUNCTIONAL PERFORMANCE) of
     * the CT IEP Form (ED620)
     *
     * Iterate through the IEP's IepPerformanceLevel's and sets the following parameters:
     *
     * - "Academic Language" = (IepPerformanceLevel)
     * - "Academic Math" = (IepPerformanceLevel)
     * - "Behavioral" = (IepPerformanceLevel)
     * - "Communication" = (IepPerformanceLevel)
     * - "Daily Living" = (IepPerformanceLevel)
     * - "Fine/Gross Motor" = (IepPerformanceLevel)
     * - "Vision and Hearing" = (IepPerformanceLevel)
     * - "Other" = (IepPerformanceLevel)
     * - "Other Non/Academic" = (IepPerformanceLevel)
     * - "Vocation/Transition" = (IepPerformanceLevel)
     *
     * The report pulls out the field values.
     */
    private void preparePage4_5() {
        IepData iep = getIep();

        // Set the student's academic achievement and functional performances
        Collection<IepPerformanceLevel> performanceLevels = iep.getIepPerformanceLevel();
        for (IepPerformanceLevel performanceLevel : performanceLevels) {
            addParameter(performanceLevel.getType(), performanceLevel);
        }
    }

    /**
     * Prepares page 7 of the CT IEP Form (ED620)
     *
     * Queries and iterates through the IEP's goals and objectives and adds them into a
     * ReportDataGrid (THE ORDERING IS IMPORTANT).
     * Objectives needs to be grouped in their respective goals.
     *
     * A page contains at most 3 objectives. If more objectives exist for one goal, additional pages
     * will be created for that goal.
     * Each row in the ReportDataGrid, at max, has 3 objectives (represented by
     * <code>igo-[1|2|3]-&lt;alias&gt;</code>).
     *
     */
    private void preparePage7() {
        IepData iep = getIep();

        ReportDataGrid grid = new ReportDataGrid();
        Collection<IepGoal> goals = iep.getIepGoals();

        for (IepGoal goal : goals) {
            grid.append();
            addGoalToGrid(grid, goal);

            Collection<IepGoalObjective> objectives = goal.getIepGoalObjectives();
            Collection<IepGoalProgress> progressCollection = goal.getIepGoalProgress();
            int count = 0;

            for (IepGoalObjective objective : objectives) {
                if (count > 0 && count % 3 == 0) {
                    grid.append();
                    addGoalToGrid(grid, goal);
                }
                addObjectiveToGoal(grid, count % 3, objective);
                addProgressToObjective(grid, count, progressCollection);
                count++;
            }

        }

        addParameter(PARAM_PAGE7_FORMAT, getSubreportFormat(PARAM_PAGE_7_FORMAT_ID));
        if (grid.rowCount() == 0) {
            grid.append();
        }
        grid.beforeTop();
        addParameter(PARAM_GOALS, grid);
    }

    /**
     * Adds a goal in a ReportDataGrid.
     *
     * @param grid ReportDataGrid
     * @param goal IepGoal
     */
    private void addGoalToGrid(ReportDataGrid grid, IepGoal goal) {
        Collection<IepGoalProgress> progressCollection = goal.getIepGoalProgress(); // todo get iep
                                                                                    // progress,
                                                                                    // correctly.

        for (IepGoalProgress progress : progressCollection) {
            grid.set("igl-progress-" + progress.getReportPeriod(), progress.getProgressCode());
        }

        DataDictionary dictionary = getDictionary();
        grid.set("focus", goal.getFocus());
        grid.set("goal", goal.getGoal());
        grid.set("igl-focus-other", goal.getFieldValueByAlias("igl-focus-other", dictionary));
        grid.set("igl-evaluation-procedure-11", goal.getFieldValueByAlias("igl-evaluation-procedure-11", dictionary));
        grid.set("igl-evaluation-procedure-12", goal.getFieldValueByAlias("igl-evaluation-procedure-12", dictionary));
        grid.set("igl-performance-criteria-i", goal.getFieldValueByAlias("igl-performance-criteria-i", dictionary));
        grid.set("igl-performance-criteria-j", goal.getFieldValueByAlias("igl-performance-criteria-j", dictionary));
        grid.set("igl-reporting-progress-other", goal.getFieldValueByAlias("igl-reporting-progress-other", dictionary));
        grid.set("igl-evaluation-procedure", goal.getFieldValueByAlias("igl-evaluation-procedure", dictionary));
        grid.set("igl-performance-criteria", goal.getFieldValueByAlias("igl-performance-criteria", dictionary));
        grid.set("igl-percent-trials", goal.getFieldValueByAlias("igl-percent-trials", dictionary));
        /*
         * grid.set("igl-progress-1", goal.getFieldValueByAlias("igl-progress-1", dictionary));
         * grid.set("igl-progress-2", goal.getFieldValueByAlias("igl-progress-2", dictionary));
         * grid.set("igl-progress-3", goal.getFieldValueByAlias("igl-progress-3", dictionary));
         * grid.set("igl-progress-4", goal.getFieldValueByAlias("igl-progress-4", dictionary));
         * grid.set("igl-progress-5", goal.getFieldValueByAlias("igl-progress-5", dictionary));
         * grid.set("igl-progress-6", goal.getFieldValueByAlias("igl-progress-6", dictionary));
         * grid.set("igl-progress-7", goal.getFieldValueByAlias("igl-progress-7", dictionary));
         * grid.set("igl-progress-8", goal.getFieldValueByAlias("igl-progress-8", dictionary));
         * 
         * TODO remove from data model.
         * 
         */
        grid.set("igl-evaluation-procedure-13", goal.getFieldValueByAlias("igl-evaluation-procedure-13", dictionary));
        grid.set("igl-evaluation-procedure-14", goal.getFieldValueByAlias("igl-evaluation-procedure-14", dictionary));
        grid.set("igl-evaluation-procedure-15", goal.getFieldValueByAlias("igl-evaluation-procedure-15", dictionary));
        grid.set("igl-evaluation-procedure-16", goal.getFieldValueByAlias("igl-evaluation-procedure-16", dictionary));
        grid.set("igl-evaluation-procedure-17", goal.getFieldValueByAlias("igl-evaluation-procedure-17", dictionary));
        grid.set("igl-evaluation-procedure-18", goal.getFieldValueByAlias("igl-evaluation-procedure-18", dictionary));
        grid.set("igl-performance-criteria-k", goal.getFieldValueByAlias("igl-performance-criteria-k", dictionary));
        grid.set("igl-performance-criteria-l", goal.getFieldValueByAlias("igl-performance-criteria-l", dictionary));
        grid.set("igl-performance-criteria-m", goal.getFieldValueByAlias("igl-performance-criteria-m", dictionary));
        grid.set("igl-performance-criteria-n", goal.getFieldValueByAlias("igl-performance-criteria-n", dictionary));
        grid.set("igl-performance-criteria-o", goal.getFieldValueByAlias("igl-performance-criteria-o", dictionary));
        grid.set("igl-performance-criteria-p", goal.getFieldValueByAlias("igl-performance-criteria-p", dictionary));
    }

    /**
     * Adds an objective to a goal in a ReportDataGrid.
     *
     * The caller is responsible (preparePage7) for keeping track of which objective #
     * (<code>objectiveNumber</code>)
     * an IepGoalObjective goes into. Objective number should begin with "0."
     *
     * @param grid ReportDataGrid
     * @param objectiveNumber the objective # within a goal
     * @param objective IepGoalObjective
     */
    private void addObjectiveToGoal(ReportDataGrid grid, int objectiveNumber, IepGoalObjective objective) {
        DataDictionary dictionary = getDictionary();
        grid.set("igo-" + objectiveNumber + "-sequence-number", Integer.valueOf(objective.getSequenceNumber()).toString());
        grid.set("igo-" + objectiveNumber + "-objective", objective.getObjective());
        grid.set("igo-" + objectiveNumber + "-evaluation-procedure",
                objective.getFieldValueByAlias("igo-evaluation-procedure", dictionary));
        grid.set("igo-" + objectiveNumber + "-performance-criteria",
                objective.getFieldValueByAlias("igo-performance-criteria", dictionary));
        grid.set("igo-" + objectiveNumber + "-percent-trials",
                objective.getFieldValueByAlias("igo-percent-trials", dictionary));
    }

    /**
     * Go through the progress collection to find all matching progress belonging to the objective,
     * determined by objectiveProgressCount. Adds appropriate .
     *
     * The caller is responsible (preparePage7) for keeping track of which objective #
     * (<code>objectiveNumber</code>)
     * an IepGoalProgress goes into. Objective number should begin with "0."
     *
     * @param grid ReportDataGrid
     * @param objectiveNumber the objective # within a goal
     * @param progressCollection Collection<IepGoalProgress>
     */
    private void addProgressToObjective(ReportDataGrid grid,
                                        int objectiveNumber,
                                        Collection<IepGoalProgress> progressCollection) {
        for (IepGoalProgress progress : progressCollection) {
            DataDictionary dictionary = getDictionary();
            String progressField =
                    (String) progress.getFieldValueByAlias("igp-progress-" + objectiveNumber, dictionary);
            grid.set("igp-" + objectiveNumber % 3 + "-progress-" + progress.getReportPeriod(), progressField);
        }
    }

    /**
     * Prepares page 8 of the CT IEP Form (ED620)
     *
     * Queries and iterates through the IEP's accommodations and sets each accommodation into a
     * parameter:
     *
     * - "[Category] Accommodation" = The accommodation
     * - "[Category] Sites/Activities" = The accommodation's sites/activities.
     */
    private void preparePage8() {
        IepData iep = getIep();
        Collection<IepAccommodation> accommodations = iep.getAccommodations();
        Map<String, Map> accommodationsMap = new HashMap();

        // Organize + put accommodations data into a map.
        for (IepAccommodation accommodation : accommodations) {
            Map<String, String> accommodationsValueMap = new HashMap();

            String accommodationAccommodation = accommodation.getDescription();
            if (accommodationAccommodation == null) {
                accommodationAccommodation = "";
            }

            String accommodationSites =
                    (String) accommodation.getFieldValueByAlias("iac-sites-activities", getDictionary());
            if (accommodationSites == null) {
                accommodationSites = "";
            }

            if (accommodationsMap.containsKey(accommodation.getCategory())) {
                accommodationsValueMap = accommodationsMap.get(accommodation.getCategory());
                accommodationAccommodation =
                        accommodationsValueMap.get("accommodation") + ", " + accommodationAccommodation;
                accommodationSites = accommodationsValueMap.get("sites") + ", " + accommodationSites;
            }

            accommodationsValueMap.put("accommodation", accommodationAccommodation);
            accommodationsValueMap.put("sites", accommodationSites);
            accommodationsMap.put(accommodation.getCategory(), accommodationsValueMap);
        }

        // Add accommodations data from map onto report.
        for (String category : accommodationsMap.keySet()) {
            Map<String, String> accommodationsValueMap = accommodationsMap.get(category);
            addParameter(category + " Accommodation", accommodationsValueMap.get("accommodation"));
            addParameter(category + " Sites/Activities", accommodationsValueMap.get("sites"));
        }
    }

    /**
     * Prepar page 9.
     */
    private void preparPage9() {
        IepData iep = getIep();
        String assessmentGradesArray = (String) iep.getFieldValueByAlias("iep-dist-assess-grade", getDictionary());

        if (assessmentGradesArray != null) {
            String[] assessmentGrades = assessmentGradesArray.split(",");
            for (String assessmentGrade : assessmentGrades) {
                addParameter(assessmentGrade.trim(), Boolean.TRUE);
            }
        }

    }

    /**
     * Prepares page 11 of the CT IEP Form (ED620)
     *
     * Queries and iterates through the IEP's services. Page 11 of the IEP Form uses two subreports
     * to
     * accommodate services that are "SpecialEd" and are "Related"
     *
     * Create blank services if there are less than 4 services in a collection.
     *
     */
    private void preparePage11() {
        IepData iep = getIep();
        Collection<IepService> services = iep.getIepServices();
        ReportDataGrid grid = new ReportDataGrid();

        addPage11Data(grid, services, iep);

        grid.beforeTop();

        addParameter(PARAM_PAGE11_FORMAT, getSubreportFormat(PARAM_PAGE_11_FORMAT_ID));
        addParameter(PARAM_SERVICES, grid);
    }

    /**
     * Adds the page 11 data.
     *
     * @param grid ReportDataGrid
     * @param services Collection<IepService>
     * @param iep IepData
     */
    private void addPage11Data(ReportDataGrid grid, Collection<IepService> services, IepData iep) {
        Collection<IepService> additionalServices = new ArrayList<IepService>();

        grid.append();
        addIepData(grid, iep);

        int specialEdRowCount = 0;
        int relatedRowCount = 0;

        int specialEdRowLimit = 4;
        int relatedRowLimit = 4;

        boolean specialEdRowsFull = false;
        boolean relatedRowsFull = false;

        for (IepService service : services) {
            if (service.getServiceMode().equals("SpecialEd")) {
                if (specialEdRowCount < specialEdRowLimit) {
                    addServiceData(grid, service, service.getServiceMode(), specialEdRowCount);
                }

                else {
                    specialEdRowsFull = true;
                    additionalServices.add(service);
                }
                specialEdRowCount++;
            } else if (service.getServiceMode().equals("Related")) {
                if (relatedRowCount < relatedRowLimit) {
                    addServiceData(grid, service, service.getServiceMode(), relatedRowCount);
                }

                else {
                    relatedRowsFull = true;
                    additionalServices.add(service);
                }
                relatedRowCount++;
            }
        }

        if (specialEdRowsFull || relatedRowsFull) {
            addPage11Data(grid, additionalServices, iep);
        }
    }

    /**
     * Adds the iep data.
     *
     * @param grid ReportDataGrid
     * @param iep IepData
     */
    private void addIepData(ReportDataGrid grid, IepData iep) {

        DataDictionary dictionary = getDictionary();

        String daysPerWeek = (String) iep.getFieldValueByAlias("iep-days-per-week", dictionary);
        String schoolYear = (String) iep.getFieldValueByAlias("iep-school-year", dictionary);

        grid.set("iep-activity-peers", iep.getFieldValueByAlias("iep-activity-peers", dictionary));
        grid.set("iep-applied-voc-ed", iep.getFieldValueByAlias("iep-applied-voc-ed", dictionary));
        grid.set("iep-applied-voc-ed-specify", iep.getFieldValueByAlias("iep-applied-voc-ed-specify", dictionary));
        grid.set("iep-assistive-technology", iep.getFieldValueByAlias("iep-assistive-technology", dictionary));
        if (!StringUtils.isEmpty(daysPerWeek)) {
            grid.set("iep-days-per-week", Integer.toString(Integer.parseInt(daysPerWeek)));
        }
        grid.set("iep-extended-service", iep.getFieldValueByAlias("iep-extended-service", dictionary));
        grid.set("iep-participate-gen-ed", iep.getFieldValueByAlias("iep-participate-gen-ed", dictionary));
        grid.set("iep-hearing-factors-na", iep.getFieldValueByAlias("iep-hearing-factors-na", dictionary));
        grid.set("iep-hours-non-disabled-peers", iep.getFieldValueByAlias("iep-hours-non-disabled-peers", dictionary));
        grid.set("iep-non-activity-peers", iep.getFieldValueByAlias("iep-non-activity-peers", dictionary));
        grid.set("iep-non-activity-peers-na", iep.getFieldValueByAlias("iep-non-activity-peers-na", dictionary));
        grid.set("iep-non-activity-peers-reason",
                iep.getFieldValueByAlias("iep-non-activity-peers-reason", dictionary));
        grid.set("iep-non-activity-peers-rsn-na",
                iep.getFieldValueByAlias("iep-non-activity-peers-rsn-na", dictionary));
        grid.set("iep-non-activity-peers-rsn-rmv",
                iep.getFieldValueByAlias("iep-non-activity-peers-rsn-rmv", dictionary));
        grid.set("iep-physical-education", iep.getFieldValueByAlias("iep-physical-education", dictionary));
        grid.set("iep-physical-education-specify",
                iep.getFieldValueByAlias("iep-physical-education-specify", dictionary));
        grid.set("iep-school-day", iep.getFieldValueByAlias("iep-school-day", dictionary));
        if (!StringUtils.isEmpty(schoolYear)) {
            grid.set("iep-school-year", Integer.toString(Integer.parseInt(schoolYear)));
        }
        grid.set("iep-sped-hours-per-week", iep.getFieldValueByAlias("iep-sped-hours-per-week", dictionary));
        grid.set("iep-total-hours-per-week", iep.getFieldValueByAlias("iep-total-hours-per-week", dictionary));
        grid.set("iep-transportation", iep.getFieldValueByAlias("iep-transportation", dictionary));
        grid.set("iep-transportation-specify", iep.getFieldValueByAlias("iep-transportation-specify", dictionary));
    }

    /**
     * Add a service and its information into a ReportDataGrid.
     *
     * @param grid ReportDataGrid
     * @param service IepService
     * @param serviceMode String
     * @param row int
     */
    private void addServiceData(ReportDataGrid grid, IepService service, String serviceMode, int row) {
        SimpleDateFormat shortDateFormat = new SimpleDateFormat("MM/dd/yyyy");
        grid.set(serviceMode + "-" + row + "-service", service.getServiceType());
        grid.set(serviceMode + "-" + row + "-goalNum", service.getGoalView());
        grid.set(serviceMode + "-" + row + "-frequency", calculateFrequency(service));
        grid.set(serviceMode + "-" + row + "-responsible-staff",
                service.getFieldValueByAlias("isv-responsible-staff", getDictionary()));
        grid.set(serviceMode + "-" + row + "-service-implementer", service.getProviderCode());
        grid.set(serviceMode + "-" + row + "-start-date", shortDateFormat.format(service.getStartDate()));
        grid.set(serviceMode + "-" + row + "-end-date", shortDateFormat.format(service.getEndDate()));
        grid.set(serviceMode + "-" + row + "-site", getServiceSiteCode(service));
        grid.set(serviceMode + "-" + row + "-delivery", service.getOtherInformation());
    }

    /**
     * Gets the service site code.
     *
     * @param service IepService
     * @return String
     */
    private String getServiceSiteCode(IepService service) {
        String siteCode = service.getSettingCode();
        ReferenceCode refCode = null;
        if (siteCode != null) {
            refCode = m_serviceSiteRefCodes.get(siteCode);
            siteCode = refCode.getStateCode();
        }
        /*
         * if (siteCode != null && siteCode.equals("Other"))
         * {
         * siteCode = (String) service.getFieldValueByAlias("isv-setting-code-other",
         * getDictionary());
         * }
         */

        return siteCode;
    }

    /**
     * Calculate frequency.
     *
     * @param service IepService
     * @return String
     */
    private String calculateFrequency(IepService service) {
        String frequency = null;

        if (service.getFrequency() != null) {
            frequency = "" + new BigDecimal(service.getDuration()).multiply(service.getFrequency()).intValue();
        }
        frequency = frequency + "m/";

        if (service.getCycle() != null) {
            if (service.getCycle().equals("Other")) {
                frequency = frequency + service.getDaysPerCycle() + "d";
            } else {
                frequency = frequency + service.getCycle();
            }
        } else {
            frequency = frequency + "d";
        }

        return frequency;
    }
}
