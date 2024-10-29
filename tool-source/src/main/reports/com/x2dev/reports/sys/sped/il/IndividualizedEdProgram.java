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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.COLON;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.DSBL_KEY_PRIMARY_CODE;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.DSBL_KEY_PRIMARY_NAME;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.DSBL_KEY_SECONDARY_CODES;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.DSBL_KEY_SECONDARY_NAMES;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.EMPTY;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.SEMICOLON;
import static com.x2dev.reports.sys.sped.il.IlSpedHelper.SPACE;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.sped.MeetingAttendanceManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field or
 * alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class IndividualizedEdProgram extends BeanReport {

    /**
     * list of meeting types.
     *
     * @author Follett Software Company
     */
    public enum MeetingTypes {
        INITIAL_DOMAIN("initial domain", 1, "SYS-SPED-REFER"),
        //
        INITIAl_ELIGIBILITY("initial eligibility", 2, "SYS-SPED-REFER"),
        //
        INITIAL_IEP("initial iep", 3, "SYS-SPED-REFER"),
        //
        ANNUAL_REVIEW("annual review", 1, "SYS-SPED-RENEW"),
        //
        RE_EVALUATION_DOMAIN("re-evaluation domain", 1, "SYS-SPED-REEVAL"),
        //
        RE_EVALUATION_ELIGIBILITY("re-evaluation eligibility", 2, "SYS-SPED-REEVAL"),
        //
        RE_ANNUAL_REVIEW("re-eval annual review", 3, "SYS-SPED-REEVAL"),
        //
        AMENDMENT("amendment", 1, "SYS-SPED-AMEND"),
        //
        TRANSFER_IN("transfer-in", 1, "SYS-SPED-TRANS");

        private String m_value = null;
        private int m_intValue;
        private String m_workflowId = null;

        /**
         * Gets the workflow id.
         *
         * @return String
         */
        public String getWorkflowId() {
            return m_workflowId;
        }

        /**
         * Instantiates a new meeting types.
         *
         * @param value String
         * @param intValue int
         * @param workflowId String
         */
        MeetingTypes(String value, int intValue, String workflowId) {
            m_intValue = intValue;
            m_value = value;
            m_workflowId = workflowId;
        }

        /**
         * Gets the int original by sting value.
         *
         * @param value String
         * @return int
         */
        public static int getIntOriginalByStingValue(String value) {
            int original = -1;
            for (MeetingTypes type : MeetingTypes.values()) {
                if (type.toString().equals(value)) {
                    original = type.ordinal();
                }
            }
            return original;
        }

        /**
         * Gets the multiple number.
         *
         * @return int
         */
        public int getMultipleNumber() {
            return m_intValue;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return m_value;
        }
    }

    private static final String ALIAS_57D_NOT_CONF_FBA_BIP = "not-conf-fba-bip";
    private static final String ALIAS_57D_NOT_CONF_GRADUATION = "not-conf-graduation";
    private static final String ALIAS_57D_NOT_CONF_IEP_REVIEW = "not-conf-iep-review";
    private static final String ALIAS_57D_NOT_CONF_INITIAL_EVAL = "not-conf-initial-eval";
    private static final String ALIAS_57D_NOT_CONF_INITIAL_IEP = "not-conf-initial-iep";
    private static final String ALIAS_57D_NOT_CONF_MANIF_DETEM_REVIEW = "not-conf-manif-detem-review";
    private static final String ALIAS_57D_NOT_CONF_OTHER = "not-conf-other";
    private static final String ALIAS_57D_NOT_CONF_REEVAL = "not-conf-reeval";
    private static final String ALIAS_57D_NOT_CONF_REVIEW_DATA = "not-conf-revie-data";
    private static final String ALIAS_57D_NOT_CONF_SEC_TRANSITION = "not-conf-sec-transition";
    private static final String ALIAS_MTG_CHANNEL = "mtg-channel";
    private static final String ALIAS_MTG_TYPE_NOTIFICATION = "mtg-type-notification";
    private static final String ALIAS_MTG_DATE_NOTIFICATION = "mtg-date-notification";
    private static final String ALIAS_MTG_TYPE = "mtg-type";
    private static final String ALIAS_IMG_BEH_INTERV_POLICIES = "img-beh-interv-policies";
    private static final String ALIAS_IMG_BEH_INTERV_POLICIES_IEP = "img-beh-interv-policies-iep";
    private static final String ALIAS_IMG_EVALUATION_REPORT = "img-evaluation-report";
    private static final String ALIAS_IMG_IEP = "img-iep";
    private static final String ALIAS_IMG_NOTES = "nts-mtg-notes";
    private static final String ALIAS_SAFEGUARD_WAS_PROVIDE = "iep-sum-safeguard-was-provide";
    private static final String ALIAS_STD_WAS_INF = "iep-sum-std-was-inf";

    private static final String COLUMN_IMG_DATA = "mtg-date";
    private static final String COLUMN_SAFEGUARDS_PROVIDED = "safeguardsProvided";
    private static final String COLUMN_MEETING_TYPE = "meetingType";
    private static final String COLUMN_PARENTNOTIFIED = "parentnotified";

    private static final String DDXID_SPED_IL_3457D = "SPED-IL-3457D";

    private static final String FORM_DEF_ID_MTG = "MTG";
    private static final String FORM_DEF_ID_SPED_IL_3457D = "SPED-IL-3457D";

    private static final String JR_PARAM_BOUND_REPORTS = "boundReports";

    private static final String KEY_ATTENDANCE = "attendance";
    private static final String KEY_MTG_OID = "mtgOid";
    private static final String KEY_NAME = "name";
    private static final String KEY_ROLE = "role";
    private static final String KEY_RESIDENT_DISTRICT = "residentDistrict";
    private static final String KEY_RESIDENT_SCHOOL = "residentSchool";
    private static final String KEY_SERVING_DISTRICT = "servingDistrict";
    private static final String KEY_SERVING_SCHOOL = "servingSchool";

    private static final String MEMBER_ROLE_PARENT = "Parent";
    private static final String MEMBER_ROLE_GUARDIAN = "Guardian";

    private static final String PARAM_DISABILITY = "disability";
    private static final String PARAM_DOB = "DOB";
    private static final String PARAM_ETHNIC = "ethnic";
    private static final String PARAM_MEMBER_MAP = "memberMap";
    private static final String PARAM_NEXT_EVAL_DATE = "next-evaluation-date";
    private static final String PARAM_NEXT_REVIEW_DATE = "next-review-date";
    private static final String PARAM_OID = "oid";
    private static final String PARAM_OWNER = "owner";
    private static final String PARAM_PARENT_LIST = "parentList";
    private static final String PARAM_REVIEW_DATE = "review-date";
    private static final String PARAM_RESENT_EVALUATION_DATE = "resent-evaluation-date";

    private static final String SCHEDULE_MEETING_ALIGMENT_PREFIX = "Meet";
    private static final String SPACE_FOR_FILLING = "________________";

    private static final String TEXT_MESSAGE_NOTIFIED_ABOUT = " notified parent about participation on ";
    private static final String TEXT_MESSAGE_BY = " by ";

    private ReportDataGrid m_grid;
    private IlSpedHelper m_ilSpedHelper = new IlSpedHelper();
    private Map<String, MeetingAttendanceManager> m_attendanceManagetMap =
            new HashMap<String, MeetingAttendanceManager>();

    private static final long serialVersionUID = 1L;
    private List<IepMeeting> m_meetings = null;
    private DataDictionary m_ddx3457D = null;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.reports.sys.sped.il.BeanReport#fillGrid(Map, Locale)
     */
    @Override
    protected JRDataSource gatherData() {
        m_ilSpedHelper.initializeHelper(getBroker(), getDictionary());
        m_grid = new ReportDataGrid();
        IepData iepData = (IepData) getFormStorage();
        addParameter(JR_PARAM_BOUND_REPORTS, Boolean.TRUE);

        if (iepData != null && iepData.getOid() != null) {

            fillDisabilities(iepData);
            fillParentGuardianInfo(iepData);

            fillOtherDataToMap(iepData);
            for (IepMeeting iepMeeting : getIepMeeting(iepData)) {
                WorkflowProgressForm progressForm = findWorkflowProgressFormForMeeting(iepMeeting);
                if (progressForm != null) {
                    m_grid.append();
                    fillDataFromAliases(iepMeeting,
                            new ArrayList<String>(Arrays.asList(ALIAS_IMG_NOTES, ALIAS_IMG_BEH_INTERV_POLICIES,
                                    ALIAS_IMG_BEH_INTERV_POLICIES, ALIAS_IMG_BEH_INTERV_POLICIES_IEP,
                                    ALIAS_IMG_EVALUATION_REPORT, ALIAS_MTG_TYPE,
                                    ALIAS_IMG_IEP, ALIAS_STD_WAS_INF)));
                    GenericFormData formData = findAppropriate57DStorage(progressForm);
                    if (formData != null) {
                        fillDataFromAliases(
                                formData,
                                new ArrayList<String>(Arrays.asList(ALIAS_57D_NOT_CONF_INITIAL_EVAL,
                                        ALIAS_57D_NOT_CONF_REVIEW_DATA,
                                        ALIAS_57D_NOT_CONF_REEVAL,
                                        ALIAS_57D_NOT_CONF_INITIAL_IEP, ALIAS_57D_NOT_CONF_IEP_REVIEW,
                                        ALIAS_57D_NOT_CONF_SEC_TRANSITION,
                                        ALIAS_57D_NOT_CONF_FBA_BIP, ALIAS_57D_NOT_CONF_MANIF_DETEM_REVIEW,
                                        ALIAS_57D_NOT_CONF_GRADUATION,
                                        ALIAS_57D_NOT_CONF_OTHER)),
                                getDdx3457D());
                    }
                    fillParentInformedIfnotAttend(iepData, iepMeeting);
                    fillOtherDataToGrid(iepData, iepMeeting);
                    String meetingType = (String) iepMeeting.getFieldValueByAlias(ALIAS_MTG_TYPE, getDictionary());
                    m_grid.set(COLUMN_MEETING_TYPE, meetingType);
                    fillParticipantInfo(iepData, iepMeeting);
                }
            }
        } else {
            m_grid.append();
        }
        m_grid.beforeTop();
        return m_grid;
    }

    /**
     * Filter multiple form instance.
     *
     * @param formInstances List<FormInstance>
     * @param multipleNumber String
     * @param workFlow Workflow
     * @param broker X2Broker
     * @return List
     * @see com.x2dev.reports.sys.sped.il.BeanReport#filterMultipleFormInstance(java.util.List,
     *      java.lang.String, com.follett.fsc.core.k12.beans.Workflow,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public List<FormInstance> filterMultipleFormInstance(List<FormInstance> formInstances,
                                                         String multipleNumber,
                                                         Workflow workFlow,
                                                         X2Broker broker) {
        return formInstances;
    }

    /**
     * Process multiple.
     *
     * @param multipleNumber String
     * @param workflow Workflow
     * @see com.x2dev.reports.sys.sped.il.BeanReport#processMultiple(java.lang.String,
     *      com.follett.fsc.core.k12.beans.Workflow)
     */
    @Override
    public void processMultiple(String multipleNumber, Workflow workflow) {
        if (!StringUtils.isEmpty(multipleNumber) && StringUtils.isNumeric(multipleNumber)) {
            m_meetings = new ArrayList<IepMeeting>();
            int multNumber = Integer.parseInt(multipleNumber);
            String meetingType = getMeetingTypeForMultipleNumber(workflow, multNumber);
            IepMeeting iepMeeting = getIepMeetingByWpoAndType(workflow, meetingType);
            if (iepMeeting != null) {
                m_meetings.add(iepMeeting);
            }
            // initialIl
            // wpoIlRfCnfSum1 1
            // wpoIlRfCnfSum2 2
            // wpoIlRfCnfSum3 3
            // annual-review
            // wpoIlArSummRep 1
            // re-evaluation
            // wpoIlEvCnfSum1 1
            // wpoIlEvCnfSum2 2
            // wpoIlEvCnfSum3 3
            // amenment
            // wpoIlAmConfSum 1
            // il transfer
            // wpoIlTrConfSum 2
        }
    }

    /**
     * Gets the attendance manager.
     *
     * @param iepMeetingOid String
     * @param iepData IepData
     * @return Meeting attendance manager
     */
    private MeetingAttendanceManager getAttendanceManager(String iepMeetingOid, IepData iepData) {
        String key = iepMeetingOid + iepData.getOid();
        MeetingAttendanceManager attandanceManager = m_attendanceManagetMap.get(key);
        if (attandanceManager == null) {
            attandanceManager = new MeetingAttendanceManager(iepMeetingOid, iepData, getBroker());
            m_attendanceManagetMap.put(key, attandanceManager);
        }
        return attandanceManager;
    }

    /**
     * Gets the ddx 3457 D.
     *
     * @return Data dictionary
     */
    private DataDictionary getDdx3457D() {
        if (m_ddx3457D == null) {
            m_ddx3457D = m_ilSpedHelper.getDictionaryByExtendedDictionaryId(DDXID_SPED_IL_3457D);
        }
        return m_ddx3457D;
    }


    /**
     * Gets the iep meeting by wpo and type.
     *
     * @param workflow Workflow
     * @param meetingType String
     * @return Iep meeting
     */
    private IepMeeting getIepMeetingByWpoAndType(Workflow workflow, String meetingType) {
        IepMeeting iepMeeting = null;
        if (meetingType != null) {
            m_ilSpedHelper.initializeHelper(getBroker(), IlSpedHelper.EXTENDED_DICTIOANRY_ID_SPED_IL_IEP);
            String meetingField = m_ilSpedHelper.translateAliasToJavaName(ALIAS_MTG_TYPE);
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(IepMeeting.COL_IEP_DATA_OID, workflow.getOwnerOid());
            criteria.addEqualTo(meetingField, meetingType);
            iepMeeting = (IepMeeting) getBroker().getBeanByQuery(new QueryByCriteria(IepMeeting.class, criteria));
        }
        return iepMeeting;
    }

    /**
     * Gets the meeting comparator.
     *
     * @return Comparator
     */
    private Comparator<IepMeeting> getMeetingComparator() {
        Comparator<IepMeeting> meetingComparator = new Comparator<IepMeeting>() {

            @Override
            public int compare(IepMeeting o1, IepMeeting o2) {
                int compare;
                int original1 = getOrinalEnum(o1);
                int original2 = getOrinalEnum(o2);

                if (original1 == -1 && original2 == -1) {
                    compare = o1.getOid().compareTo(o2.getOid());
                } else if (original1 == -1) {
                    compare = 1;
                } else if (original2 == -1) {
                    compare = original2;
                } else {
                    compare = original1 - original2;
                }

                return compare;
            }

            private int getOrinalEnum(IepMeeting meeting) {
                int original = -1;
                String meetingType = (String) meeting.getFieldValueByAlias(ALIAS_MTG_TYPE, getDictionary());
                if (!StringUtils.isEmpty(meetingType)) {
                    original = MeetingTypes.getIntOriginalByStingValue(meetingType);
                }
                return original;
            }
        };
        return meetingComparator;
    }

    /**
     * Gets the meeting type for multiple number.
     *
     * @param workflow Workflow
     * @param multNumber int
     * @return String
     */
    private String getMeetingTypeForMultipleNumber(Workflow workflow, int multNumber) {
        String workflowId = workflow.getWorkflowDefinition().getId();
        String returnValue = null;
        for (MeetingTypes currentType : MeetingTypes.values()) {
            if (currentType.getWorkflowId().equals(workflowId) && multNumber == currentType.getMultipleNumber()) {
                returnValue = currentType.toString();
                break;
            }
        }
        return returnValue;
    }

    // private get

    /**
     * Gets the iep meeting.
     *
     * @param iepData IepData
     * @return Collection
     */
    private Collection<IepMeeting> getIepMeeting(IepData iepData) {
        if (m_meetings == null) {
            Collection<IepMeeting> meetings = iepData.getIepMeeting();

            if (meetings instanceof List) {
                m_meetings = (List) meetings;
            } else {
                m_meetings = new ArrayList<IepMeeting>(meetings);
            }
            Collections.sort(m_meetings, getMeetingComparator());
        }
        return m_meetings;
    }

    /**
     * Fill data from alises.
     *
     * @param x2BaseBean X2BaseBean
     * @param aliases List<String>
     */
    private void fillDataFromAliases(X2BaseBean x2BaseBean, List<String> aliases) {
        fillDataFromAliases(x2BaseBean, aliases, getDictionary());
    }

    /**
     * Fill data from alises.
     *
     * @param x2BaseBean X2BaseBean
     * @param aliases List<String>
     * @param ddx DataDictionary
     */
    private void fillDataFromAliases(X2BaseBean x2BaseBean, List<String> aliases, DataDictionary ddx) {
        for (String alias : aliases) {
            m_grid.set(alias, x2BaseBean.getFieldValueByAlias(alias, ddx));
        }

    }

    /**
     * Fill disabilities.
     *
     * @param iepData IepData
     */
    private void fillDisabilities(IepData iepData) {
        Map<String, String> disabilities = m_ilSpedHelper.getDisabilities(iepData, true);

        String primary = disabilities.get(DSBL_KEY_PRIMARY_NAME);
        String primaryCode = disabilities.get(DSBL_KEY_PRIMARY_CODE);
        String secondary = disabilities.get(DSBL_KEY_SECONDARY_NAMES);
        String secondaryCode = disabilities.get(DSBL_KEY_SECONDARY_CODES);
        StringBuilder allDisabilities = new StringBuilder();
        if (!primary.isEmpty()) {
            allDisabilities.append(primaryCode == null ? EMPTY : primaryCode + COLON + SPACE);
            allDisabilities.append(primary);
            allDisabilities.append(SEMICOLON + SPACE);
        }
        if (!secondary.isEmpty()) {
            allDisabilities.append(secondaryCode == null ? EMPTY : secondaryCode + COLON + SPACE);
            allDisabilities.append(secondary);
        }

        addParameter(PARAM_DISABILITY, allDisabilities.toString());
    }

    /**
     * Fill other data to grid.
     *
     * @param iepData IepData
     * @param iepMeeting IepMeeting
     */
    private void fillOtherDataToGrid(IepData iepData, IepMeeting iepMeeting) {
        PlainDate date = new PlainDate(getFormInstance().getCreatedTime());
        StudentSchool lastOutplacement = m_ilSpedHelper.getLastOutplacement(iepData.getStudent(), null, date);
        StudentEnrollment stdEnrollment = m_ilSpedHelper.getLastStudentEnrollment(iepData.getStudent());
        m_grid.set(KEY_SERVING_DISTRICT, m_ilSpedHelper.getServingDistrict(lastOutplacement, stdEnrollment));
        m_grid.set(KEY_SERVING_SCHOOL, m_ilSpedHelper.getServingSchool(lastOutplacement, stdEnrollment));
        m_grid.set(KEY_RESIDENT_SCHOOL, m_ilSpedHelper.getResidentSchool(stdEnrollment));
        m_grid.set(KEY_RESIDENT_DISTRICT, m_ilSpedHelper.getResidentDistrict(stdEnrollment));
        m_grid.set(COLUMN_IMG_DATA, m_ilSpedHelper.formatDate(iepMeeting.getDate()));
        m_grid.set(KEY_MTG_OID, iepMeeting.getOid());
        m_grid.set(COLUMN_SAFEGUARDS_PROVIDED, m_ilSpedHelper.formatDate(
                iepMeeting.getFieldValueByAlias(ALIAS_SAFEGUARD_WAS_PROVIDE, getDictionary())));
    }

    /**
     * Fill other data to map.
     *
     * @param iepData IepData
     */
    private void fillOtherDataToMap(IepData iepData) {
        addParameter(PARAM_ETHNIC, m_ilSpedHelper.calculateEthnic(iepData.getStudent().getPerson()));
        addParameter(PARAM_OWNER, iepData);
        addParameter(PARAM_OID, iepData.getOid());
        addParameter(PARAM_DOB, m_ilSpedHelper.formatDate(iepData.getStudent().getPerson().getDob()));
        addParameter(PARAM_RESENT_EVALUATION_DATE,
                m_ilSpedHelper.formatDate(iepData.getLastEvaluationDate()));
        addParameter(PARAM_NEXT_EVAL_DATE, m_ilSpedHelper.formatDate(iepData.getNextEvaluationDate()));
        addParameter(PARAM_REVIEW_DATE,
                m_ilSpedHelper.formatDate(iepData.getLastReviewDate()));
        addParameter(PARAM_NEXT_REVIEW_DATE,
                m_ilSpedHelper.formatDate(iepData.getNextReviewDate()));
    }

    /**
     * Fill parent informed ifnot attend.
     *
     * @param iepData IepData
     * @param iepMeeting IepMeeting
     */
    private void fillParentInformedIfnotAttend(IepData iepData, IepMeeting iepMeeting) {

        MeetingAttendanceManager attendanceManager = getAttendanceManager(iepMeeting.getOid(), iepData);
        Collection<IepTeamMember> teamMembers = iepData.getTeamMembers();
        if (isParentInvited(teamMembers, attendanceManager) && !isParentAttend(teamMembers, attendanceManager)) {
            for (int i = 0; i < 3; i++) {
                String postfix = i == 0 ? EMPTY : EMPTY + (i + 1);
                String dateNotification = (String) iepMeeting
                        .getFieldValueByAlias(ALIAS_MTG_DATE_NOTIFICATION + postfix, getDictionary());
                dateNotification = m_ilSpedHelper.formatDate(dateNotification);
                dateNotification = StringUtils.isEmpty(dateNotification) ? SPACE_FOR_FILLING : dateNotification;

                String personNotified = (String) iepMeeting.getFieldValueByAlias(ALIAS_MTG_TYPE_NOTIFICATION + postfix,
                        getDictionary());
                personNotified = StringUtils.isEmpty(personNotified) ? SPACE_FOR_FILLING : personNotified;
                String wayNotified =
                        (String) iepMeeting.getFieldValueByAlias(ALIAS_MTG_CHANNEL + postfix, getDictionary());
                wayNotified = StringUtils.isEmpty(wayNotified) ? SPACE_FOR_FILLING : wayNotified;

                StringBuilder textMessage = new StringBuilder();
                textMessage.append(personNotified);
                textMessage.append(TEXT_MESSAGE_NOTIFIED_ABOUT);
                textMessage.append(dateNotification);
                textMessage.append(TEXT_MESSAGE_BY);
                textMessage.append(wayNotified);
                m_grid.set(COLUMN_PARENTNOTIFIED + i, textMessage.toString());

            }
        }
    }

    /**
     * Fill parent guardian info.
     *
     * @param iepData IepData
     */
    private void fillParentGuardianInfo(IepData iepData) {
        List<Map<String, Object>> parents = new ArrayList<Map<String, Object>>();
        for (IepTeamMember teamMember : iepData.getTeamMembers()) {
            if (teamMember.getMemberRoleCode().equals(MEMBER_ROLE_PARENT)
                    || teamMember.getMemberRoleCode().equals(MEMBER_ROLE_GUARDIAN)) {
                Map<String, Object> parent = new HashMap<String, Object>();
                m_ilSpedHelper.fillParentInformation(teamMember, parent);
                parents.add(parent);
            }
        }
        addParameter(PARAM_PARENT_LIST, parents);

    }

    /**
     * Fill participant info.
     *
     * @param iepData IepData
     * @param iepMeeting IepMeeting
     */
    private void fillParticipantInfo(IepData iepData, IepMeeting iepMeeting) {
        Map<String, List<Map<String, Object>>> meetingMembersMap =
                (Map<String, List<Map<String, Object>>>) getParameter(PARAM_MEMBER_MAP);
        MeetingAttendanceManager attendanceManager = getAttendanceManager(iepMeeting.getOid(), iepData);

        if (meetingMembersMap == null) {
            meetingMembersMap = new HashMap<String, List<Map<String, Object>>>();
            addParameter(PARAM_MEMBER_MAP, meetingMembersMap);
        }

        List<Map<String, Object>> teamMemberList = new ArrayList<Map<String, Object>>();
        for (IepTeamMember teamMember : iepData.getTeamMembers()) {
            boolean invited = attendanceManager.getInvited(teamMember.getOid());
            if (invited) {
                Map<String, Object> memberInfo = new HashMap<String, Object>();
                memberInfo.put(KEY_NAME, teamMember.getNameView());
                memberInfo.put(KEY_ROLE, teamMember.getMemberRoleCode());
                // now we need show team if he was invited
                memberInfo.put(KEY_ATTENDANCE, Boolean.valueOf(invited));
                teamMemberList.add(memberInfo);
            }
        }
        meetingMembersMap.put(iepMeeting.getOid(), teamMemberList);


    }

    /**
     * Find appropriate 57 D storage.
     *
     * @param progressForm WorkflowProgressForm
     * @return GenericFormData
     */
    private GenericFormData findAppropriate57DStorage(WorkflowProgressForm progressForm) {

        GenericFormData genericFormData = null;
        FormInstance formInstance =
                progressForm.getWorkflowProgress().getFormInstanceById(FORM_DEF_ID_SPED_IL_3457D);
        if (formInstance != null) {
            genericFormData = (GenericFormData) formInstance.getStorageObject();
        }

        return genericFormData;
    }

    /**
     * Find workflow progress form for meeting.
     *
     * @param iepMeeting IepMeeting
     * @return WorkflowProgressForm
     */
    WorkflowProgressForm findWorkflowProgressFormForMeeting(IepMeeting iepMeeting) {
        X2Criteria progressFormCriteria = new X2Criteria();

        // Must be associated with workflow for this IEP
        progressFormCriteria.addEqualTo(WorkflowProgressForm.REL_WORKFLOW_PROGRESS + PATH_DELIMITER
                + WorkflowProgress.REL_WORKFLOW + PATH_DELIMITER + Workflow.COL_OWNER_OID,
                iepMeeting.getIepDataOid());
        progressFormCriteria.addEqualTo(
                WorkflowProgressForm.REL_FORM_INSTANCE + PATH_DELIMITER + FormInstance.COL_STORAGE_OBJECT_OID,
                iepMeeting.getOid());
        progressFormCriteria.addEqualTo(WorkflowProgressForm.REL_FORM_INSTANCE + PATH_DELIMITER
                + FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER + FormDefinition.COL_ID,
                FORM_DEF_ID_MTG);
        progressFormCriteria.addBeginsWith(WorkflowProgressForm.REL_WORKFLOW_PROGRESS + PATH_DELIMITER +
                WorkflowProgress.REL_WORKFLOW_PHASE_OUTCOME + PATH_DELIMITER +
                WorkflowPhaseOutcome.REL_WORKFLOW_PHASE_OUTCOME_FORMS + PATH_DELIMITER +
                WorkflowPhaseOutcomeForm.REL_WORKFLOW_PHASE_OUTCOME + PATH_DELIMITER +
                WorkflowPhaseOutcome.COL_ALIGNMENT_ID, SCHEDULE_MEETING_ALIGMENT_PREFIX);
        progressFormCriteria.addEqualTo(WorkflowProgressForm.REL_WORKFLOW_PROGRESS + PATH_DELIMITER
                + WorkflowProgress.REL_WORKFLOW_PHASE_OUTCOME + PATH_DELIMITER +
                WorkflowPhaseOutcome.REL_WORKFLOW_PHASE_OUTCOME_FORMS + PATH_DELIMITER +
                WorkflowPhaseOutcomeForm.REL_FORM_DEFINITION + PATH_DELIMITER +
                FormDefinition.COL_ID, FORM_DEF_ID_MTG);

        WorkflowProgressForm value = null;
        QueryByCriteria byCriteria = new QueryByCriteria(WorkflowProgressForm.class, progressFormCriteria);
        Collection<WorkflowProgressForm> progressForms = getBroker().getCollectionByQuery(byCriteria);
        if (progressForms.size() == 1) {
            value = progressForms.iterator().next();
        }
        return value;
    }

    /**
     * Checks if is parent invited.
     *
     * @param teamMembers Collection<IepTeamMember>
     * @param attendanceManager MeetingAttendanceManager
     * @return true, if is parent invited
     */
    private boolean isParentInvited(Collection<IepTeamMember> teamMembers, MeetingAttendanceManager attendanceManager) {

        boolean isInvited = false;
        for (IepTeamMember teamMember : teamMembers) {
            if (teamMember.getMemberRoleCode().equals(MEMBER_ROLE_PARENT)
                    || teamMember.getMemberRoleCode().equals(MEMBER_ROLE_GUARDIAN)) {

                isInvited = attendanceManager.getInvited(teamMember.getOid());
                if (isInvited) {
                    break;
                }

            }
        }
        return isInvited;
    }

    /**
     * Checks if is parent attend.
     *
     * @param teamMembers Collection<IepTeamMember>
     * @param attendanceManagerd MeetingAttendanceManager
     * @return true, if is parent attend
     */
    private boolean isParentAttend(Collection<IepTeamMember> teamMembers, MeetingAttendanceManager attendanceManagerd) {
        boolean isAttend = false;
        for (IepTeamMember teamMember : teamMembers) {
            if (teamMember.getMemberRoleCode().equals(MEMBER_ROLE_PARENT)
                    || teamMember.getMemberRoleCode().equals(MEMBER_ROLE_GUARDIAN)) {
                isAttend = attendanceManagerd.getPresent(teamMember.getOid());
                if (isAttend) {
                    break;
                }
            }
        }
        return isAttend;
    }


}
