/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Map;

/**
 * Export data module for the RI Special Education Attendance census. The base class for this state
 * report is the IEP_MEETING table. One meeting for each IEP with the "In Last State Report" flag
 * set is included. Therefore, it is required that the Student Census be run prior to running this
 * report.The meeting record with the date matching the IEP's "meeting date" field is included. If
 * the IEP does not have a meeting with a matching meeting date, no record is reported.
 * <p>
 * The attendance role fields are calculated based on child IEP Meeting Attendance records. If an
 * attendance record with the "present" flag set exists for a team member whose state code matches
 * the role field, "Y" is reported.
 *
 * @author mmastrangelo
 */
public class RISpecialEducationAttendance extends StateReportData {

    /**
     * Entity class for WA SSID Export.
     *
     */
    public static class RISPEDAttEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        /**
         * Entity instance variables.
         */
        RISpecialEducationAttendance m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RISPEDAttEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            IepMeeting img = (IepMeeting) getBean();
            Student student = img.getStudent();
            String localId = student == null ? "" : student.getLocalId();
            String stateId = student == null ? "" : student.getStateId();
            String name = img.getOid() +
                    " [LASID: " + localId +
                    ", SASID: " + stateId +
                    "] ";

            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            IepMeeting img = (IepMeeting) bean;

            m_data = (RISpecialEducationAttendance) data;
            String imgMostAttended = m_data.m_imgMostAttended.get(img.getIepDataOid());
            if (!img.getOid().equals(imgMostAttended)) {
                setRowCount(0);
            }
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    // Field Aliases ------------------------------------------------------------------------------
    private static final String IN_LAST_STATE_REPORT_ALIAS = "sped-in-last-state-report";

    /**
     * Input parameters
     */
    public static final String SENIOR_GRAD_DATE = "seniorGraduationDate";

    // State Code Constants -----------------------------------------------------------------------
    private static final String TEAM_MEMBER_DISTRICT_REP = "District_Rep";
    private static final String TEAM_MEMBER_REGULAR_ED_TEACHER = "Regular_Teacher";
    private static final String TEAM_MEMBER_SPECIAL_ED_TEACHER = "Sped_Teacher";
    private static final String TEAM_MEMBER_STUDENT = "Student";
    private static final String TEAM_MEMBER_TRANSITION_SVC = "Transition_Service";
    private static final String TEAM_MEMBER_EDUCATIONAL_ADVOCATE = "Educational_Advocate";
    private static final String TEAM_MEMBER_PARENT = "Parents";
    private static final String TEAM_MEMBER_RELATED_SVC = "Related_Service";
    private static final String TEAM_MEMBER_OTHER = "Other";
    private static final String ENR_TYPE_GRADUATE = "Graduated";

    private static final String GRADUATED_CODE_15 = "15";
    // BitSet position constants ------------------------------------------------------------------
    private static final int POS_DISTRICT_REP = 0;
    private static final int POS_REGULAR_ED_TEACHER = 1;
    private static final int POS_SPECIAL_ED_TEACHER = 2;
    private static final int POS_STUDENT = 3;
    private static final int POS_TRANSITION_SVC = 4;
    private static final int POS_EDUCATIONAL_ADVOCATE = 5;
    private static final int POS_PARENT = 6;
    private static final int POS_RELATED_SVC = 7;
    private static final int POS_OTHER = 8;

    protected Map<String, BitSet> m_meetingAttendance;

    // Map<iepOid, imgOid> for most attended meeting from selection
    protected Map<String, String> m_imgMostAttended;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        PlainDate seniorGradeDate = (PlainDate) getParameter(SENIOR_GRAD_DATE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepMeeting.REL_IEP_DATA + "." + translateAliasToJavaName(IN_LAST_STATE_REPORT_ALIAS, true),
                BooleanAsStringConverter.TRUE);
        criteria.addEqualToField(IepMeeting.REL_IEP_DATA + "." + IepData.COL_MEETING_DATE, IepMeeting.COL_DATE);
        if (seniorGradeDate != null) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);

            X2Criteria orCriteria = new X2Criteria();
            X2Criteria orCriteriaCase1 = new X2Criteria();
            X2Criteria orCriteriaCase2 = new X2Criteria();
            orCriteriaCase1.addEqualTo(StudentEnrollment.COL_REASON_CODE, ENR_TYPE_GRADUATE);
            orCriteriaCase2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, GRADUATED_CODE_15);
            orCriteria.addOrCriteria(orCriteriaCase1);
            orCriteria.addOrCriteria(orCriteriaCase2);
            enrollmentCriteria.addAndCriteria(orCriteria);

            enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_DATE, seniorGradeDate);
            criteria.addIn(IepMeeting.COL_STUDENT_OID,
                    new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria));
        }

        setQuery(new BeanQuery(IepMeeting.class, criteria));

        setEntityClass(RISPEDAttEntity.class);

        loadAttendance(criteria);

        addCustomCalcs();
    }

    /**
     * Add custom field retrievers.
     */
    private void addCustomCalcs() {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();

        calcRetrievers.put("risped-RoleIndicator", new MemberRoleRetriever());

        addCalcs(calcRetrievers);
    }

    /**
     * Gets the meeting attendance count.
     *
     * @param imgOid String
     * @return int
     */
    /*
     * Determine the number of attendance bits set for a meeting
     */
    private int getMeetingAttendanceCount(String imgOid) {
        int count = 0;
        BitSet bs = m_meetingAttendance.get(imgOid);
        if (bs != null) {
            for (int i = bs.nextSetBit(0); i >= 0; i = bs.nextSetBit(i + 1)) {
                ++count;
            }
        }
        return count;
    }

    /**
     * Load a map of meeting attendance information. The value to the map is a BitSet, each position
     * corresponding to a role field.
     *
     * @param meetingCriteria X2Criteria
     */
    private void loadAttendance(X2Criteria meetingCriteria) {
        m_meetingAttendance = new HashMap<String, BitSet>(1000);

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(IepMeetingAttendance.COL_IEP_MEETING_OID,
                new SubQuery(IepMeeting.class, X2BaseBean.COL_OID, meetingCriteria));

        BeanQuery query = new BeanQuery(IepMeetingAttendance.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                IepMeetingAttendance attendance = (IepMeetingAttendance) iterator.next();
                if (attendance.getPresentIndicator()) {
                    IepTeamMember teamMember = attendance.getTeamMember();

                    if (teamMember != null) {
                        String roleStateCode =
                                lookupStateValue(IepTeamMember.class, IepTeamMember.COL_MEMBER_ROLE_CODE,
                                        teamMember.getMemberRoleCode());

                        BitSet memberBits = m_meetingAttendance.get(attendance.getIepMeetingOid());
                        if (memberBits == null) {
                            memberBits = new BitSet(9);
                            m_meetingAttendance.put(attendance.getIepMeetingOid(), memberBits);
                        }

                        if (TEAM_MEMBER_DISTRICT_REP.equals(roleStateCode)) {
                            memberBits.set(POS_DISTRICT_REP);
                        } else if (TEAM_MEMBER_REGULAR_ED_TEACHER.equals(roleStateCode)) {
                            memberBits.set(POS_REGULAR_ED_TEACHER);
                        } else if (TEAM_MEMBER_SPECIAL_ED_TEACHER.equals(roleStateCode)) {
                            memberBits.set(POS_SPECIAL_ED_TEACHER);
                        } else if (TEAM_MEMBER_STUDENT.equals(roleStateCode)) {
                            memberBits.set(POS_STUDENT);
                        } else if (TEAM_MEMBER_TRANSITION_SVC.equals(roleStateCode)) {
                            memberBits.set(POS_TRANSITION_SVC);
                        } else if (TEAM_MEMBER_EDUCATIONAL_ADVOCATE.equals(roleStateCode)) {
                            memberBits.set(POS_EDUCATIONAL_ADVOCATE);
                        } else if (TEAM_MEMBER_PARENT.equals(roleStateCode)) {
                            memberBits.set(POS_PARENT);
                        } else if (TEAM_MEMBER_RELATED_SVC.equals(roleStateCode)) {
                            memberBits.set(POS_RELATED_SVC);
                        } else // Other
                        {
                            memberBits.set(POS_OTHER);
                        }
                    }
                }
            }
        } finally {
            iterator.close();
        }

        /* now determine which meeting for each IEP has the most attendance */
        m_imgMostAttended = new HashMap();
        query = new BeanQuery(IepMeeting.class, meetingCriteria);
        query.addOrderByAscending(IepMeeting.COL_IEP_DATA_OID);
        iterator = getBroker().getIteratorByQuery(query);
        String iepDataOid = "";
        int attendanceCount = 0;
        try {
            while (iterator.hasNext()) {
                IepMeeting meeting = (IepMeeting) iterator.next();
                if (iepDataOid.equals(meeting.getIepDataOid())) {
                    int count = getMeetingAttendanceCount(meeting.getOid());
                    if (count > attendanceCount) {
                        attendanceCount = count;
                        m_imgMostAttended.put(iepDataOid, meeting.getOid());
                    }
                } else {
                    iepDataOid = meeting.getIepDataOid();
                    attendanceCount = getMeetingAttendanceCount(meeting.getOid());
                    m_imgMostAttended.put(iepDataOid, meeting.getOid());
                }
            }
        } finally {
            iterator.close();
        }

    }

    /**
     * Retrieves "Y" or "N" for a member role field. The specific field to set is based on the calc
     * param.
     *
     * @author mmastrangelo
     */
    protected class MemberRoleRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            boolean fieldValue = false;

            IepMeeting meeting = (IepMeeting) entity.getBean();

            BitSet memberBits = m_meetingAttendance.get(meeting.getOid());
            if (memberBits != null) {
                if (TEAM_MEMBER_DISTRICT_REP.equals(parameter)) {
                    fieldValue = memberBits.get(POS_DISTRICT_REP);
                } else if (TEAM_MEMBER_REGULAR_ED_TEACHER.equals(parameter)) {
                    fieldValue = memberBits.get(POS_REGULAR_ED_TEACHER);
                } else if (TEAM_MEMBER_SPECIAL_ED_TEACHER.equals(parameter)) {
                    fieldValue = memberBits.get(POS_SPECIAL_ED_TEACHER);
                } else if (TEAM_MEMBER_STUDENT.equals(parameter)) {
                    fieldValue = memberBits.get(POS_STUDENT);
                } else if (TEAM_MEMBER_TRANSITION_SVC.equals(parameter)) {
                    fieldValue = memberBits.get(POS_TRANSITION_SVC);
                } else if (TEAM_MEMBER_EDUCATIONAL_ADVOCATE.equals(parameter)) {
                    fieldValue = memberBits.get(POS_EDUCATIONAL_ADVOCATE);
                } else if (TEAM_MEMBER_PARENT.equals(parameter)) {
                    fieldValue = memberBits.get(POS_PARENT);
                } else if (TEAM_MEMBER_RELATED_SVC.equals(parameter)) {
                    fieldValue = memberBits.get(POS_RELATED_SVC);
                } else if (TEAM_MEMBER_OTHER.equals(parameter)) {
                    fieldValue = memberBits.get(POS_OTHER);
                }
            }

            return fieldValue ? "Y" : "N";
        }
    }
}
