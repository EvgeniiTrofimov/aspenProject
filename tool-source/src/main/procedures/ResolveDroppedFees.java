/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement from X2
 * Development Corporation.
 *
 * ====================================================================
 */
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.Group;
import com.follett.fsc.core.k12.beans.GroupMember;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.groups.GroupMemberType;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.groups.GroupMemberRetriever;
import com.x2dev.sis.model.beans.CashiersJournalDebitItem;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.community.GroupMemberTypeErm;
import com.x2dev.sis.model.business.community.GroupMembershipManagerSis;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This procedure uses user input to clean up outstanding fees for groups or/and
 * school courses that have been created through the CreateBatchFees procedure. This
 * procedure requires that fees that are being cleaned up have an origin oid associated
 * with a group or school course (These are created by the CreateBatchFees procedure but
 * could also be created manually or through some other process in the future and this
 * procedure would still clean them up). If a fee has been partially or completely paid
 * and the course/group is dropped then a flag is set on a user defined field to denote
 * that manually intervention will be needed to handle/refund this payment and the
 * fee will not be changed.
 *
 * *** IMPORTANT ***
 * For partially or completely paid fees to be handled correctly a user defined field on
 * CashiersJournalEntry needs to be set with an alias of CASH-REFUND-REQUIRED
 *
 * @author X2 Development Corporation
 */
public class ResolveDroppedFees extends ProcedureJavaSource {
    private final String INPUT_PARAM_GROUPS = "groups";
    private final String INPUT_PARAM_COURSES = "courses";
    private final String INPUT_PARAM_RESOLUTION_TYPE = "resolveFeeBy";
    private final String INPUT_PARAM_RESOLUTION_TYPE_VOID = "void";
    private final String INPUT_PARAM_RESOLUTION_TYPE_DELETE = "delete";

    private final String USER_DFINED_ALIAS = "CASH-REFUND-REQUIRED";

    private List<String> m_activeScheduleOids;

    // Used for logging totals
    private int voidedFees = 0;
    private int deletedFees = 0;
    private int refundsRequired = 0;

    private UserDataContainer m_userData;

    // Unsupported Group Member Types
    private static final Integer SCHOOL_LEVEL_INDEX = GroupMemberType.SCHOOL_LEVEL.getTypeIndex();
    private static final Integer GRADE_LEVEL_INDEX = GroupMemberType.GRADE_LEVEL.getTypeIndex();
    private static final Integer DEPARTMENT_INDEX = GroupMemberType.DEPARTMENT.getTypeIndex();
    private static final Integer EXTRACURRICULAR_INDEX = GroupMemberTypeErm.EXTRACURRICULAR_PROGRAME.getTypeIndex();
    private static final Integer SECTION_ROSTER_INDEX = GroupMemberTypeErm.SECTION_ROSTER.getTypeIndex();

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        ScheduleManager sceduleManager = new ScheduleManager(getBroker());
        // Begin by loading active schedules
        m_activeScheduleOids = sceduleManager.getActiveScheduleOids();

        String inputResolutionType = ((String) getParameter(INPUT_PARAM_RESOLUTION_TYPE));

        // Default to void if no resolution type exists
        if (inputResolutionType == null || inputResolutionType.equalsIgnoreCase(INPUT_PARAM_RESOLUTION_TYPE_VOID) ||
                inputResolutionType.isEmpty()) {
            resolveFees(false);
            logMessage("\n");
            logMessage("---------------------------------");
            logMessage("Total Fees voided: " + voidedFees);
        } else if (inputResolutionType.equalsIgnoreCase(INPUT_PARAM_RESOLUTION_TYPE_DELETE)) {
            resolveFees(true);
            logMessage("\n");
            logMessage("---------------------------------");
            logMessage("Total Fees deleted: " + deletedFees);
        }

        logMessage("Manual intervention required on " + refundsRequired + " fees.");
    }

    /**
     * Creates the Fees criteria.
     *
     * @param Oid String
     * @param notInOids List<String>
     * @param getStudentOid boolean
     * @return groupMemberCriteria
     */
    private Criteria buildFeesCriteria(String Oid, List<String> notInOids, boolean getStudentOid) {
        Criteria memberCriteria = new Criteria();

        memberCriteria.addEqualTo(CashiersJournalEntry.COL_ORIGIN_OID, Oid);
        memberCriteria.addNotEqualTo(CashiersJournalEntry.COL_VOIDED_INDICATOR, "1");
        if (getStudentOid) {
            memberCriteria.addNotIn(CashiersJournalEntry.COL_STUDENT_OID, notInOids);
        } else {
            memberCriteria.addNotIn(CashiersJournalEntry.COL_PERSON_OID, notInOids);
        }

        return memberCriteria;
    }

    /**
     * Creates the GroupMember criteria.
     *
     * @return groupMemberCriteria
     */
    private Criteria buildGroupMemberCriteria() {
        Criteria groupMemberCriteria = new Criteria();

        groupMemberCriteria.addNotNull(GroupMember.REL_GROUP + PATH_DELIMITER + Group.COL_FEE_TYPE);

        return groupMemberCriteria;
    }

    /**
     * Creates the StudentSchedule criteria.
     *
     * @return studentScheduleCriteria
     */
    private Criteria buildStudentScheduleCriteria() {
        Criteria studentScheduleCriteria = new Criteria();

        studentScheduleCriteria
                .addNotNull(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHOOL_COURSE +
                        PATH_DELIMITER + SchoolCourse.COL_FEE_TYPE);

        // filter for active schedules
        studentScheduleCriteria.addIn(StudentSchedule.REL_SCHEDULE, m_activeScheduleOids);

        return studentScheduleCriteria;
    }

    /**
     * Create off-setting itemizations for a CashiersJournalEntry.
     *
     * @param cjo CashiersJournalEntry
     */
    private void createOffsettingItemization(CashiersJournalEntry cjo) {
        CashiersJournalDebitItem offsettingCjd = new CashiersJournalDebitItem(getBroker().getPersistenceKey());

        offsettingCjd.setType(cjo.getFeeType());
        offsettingCjd.setCashiersJournalEntryOid(cjo.getOid());
        offsettingCjd.setAmount(BigDecimal.ZERO.subtract(cjo.getAmount()));
        offsettingCjd.setPrice(cjo.getPrice());
        offsettingCjd.setQuantity(cjo.getQuantity());

        getBroker().saveBeanForced(offsettingCjd);
    }

    /**
     * Log and delete CashiersJournalEntry bean.
     *
     * @param cjo CashiersJournalEntry
     */
    private void deleteFee(CashiersJournalEntry cjo) {
        logMessage("Fee deleted: " + getFormattedCashierJournalEntry(cjo));
        List<ValidationError> errors = getBroker().deleteBean(cjo);
        if (!errors.isEmpty()) {
            logMessage("Errors during procedure: " + errors.toString());
        }
    }

    /**
     * Returns a map of person, CashiersJournalEntry for CashiersJournalEntries that have been
     * created from a course
     * but the student is no longer enrolled in that course.
     *
     * For each course that has a fee we make a list of students enrolled in the course and subtract
     * from it psnOids
     * associated with fees that came from that course.
     *
     * @return List
     */
    private List<String> getDroppedCourseFees() {
        List<String> droppedCjoOids = new ArrayList<String>();

        List<String> coursePsnCjoOidMap = null;

        QueryByCriteria studentScheduleQuery = new QueryByCriteria(
                StudentSchedule.class, buildStudentScheduleCriteria());

        Map<String, List<StudentSchedule>> studentScheduleMap =
                getBroker().getGroupedCollectionByQuery(studentScheduleQuery,
                        StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_SCHOOL_COURSE_OID, 10);

        for (String cskOid : studentScheduleMap.keySet()) {
            // list of person Oids for a single group
            List<String> stdOids = new ArrayList<String>();

            for (StudentSchedule ssc : studentScheduleMap.get(cskOid)) {
                stdOids.add(ssc.getStudentOid());
            }

            // create a map of all psns/cjooids for fees with the originoid from this csk
            coursePsnCjoOidMap = getPsnFeeMap(cskOid, stdOids, true);

            // remove psn oids from a map of psn oid/cjo oid
            // coursePsnCjoOidMap.keySet().removeAll(stdOids);

            // if there are no groups this map will be null and we just want to return an empty map
            if (coursePsnCjoOidMap != null) {
                droppedCjoOids.addAll(coursePsnCjoOidMap);
            }
        }

        return droppedCjoOids;
    }

    /**
     * Returns a map of person Oid, CashiersJournalEntry Oid for CashiersJournalEntries that have
     * been created from a group
     * but the student is no longer in that group
     *
     * For each group that has a fee we make a list of students in the group and subtract from it
     * psnOids
     * associated with fees that came from that group (have an origin oid that equals the group
     * oid).
     *
     * @return List
     */
    private List<String> getDroppedGroupFees() {
        List<String> droppedCjoOids = new ArrayList<String>();

        List<String> groupPsnCjoOidMap = null;

        QueryByCriteria groupMemberQuery = new QueryByCriteria(GroupMember.class, buildGroupMemberCriteria());

        // groupOid, group Members
        Map<String, Collection<GroupMember>> groupMemberMap =
                getBroker().getGroupedCollectionByQuery(groupMemberQuery, GroupMember.COL_GROUP_OID, 10);

        for (String groupOid : groupMemberMap.keySet()) {
            // List of person Oids for a single group
            List<String> psnOids = new ArrayList<String>();

            psnOids.addAll(getPsnOidsForGroup(groupMemberMap.get(groupOid)));

            // D-28427 - getPsnOidsForGroup can return an empty collection. If that happened, it
            // means that we don't need to process any dropped fees for this group.
            if (psnOids.isEmpty()) {
                continue;
            }

            // create a map of all psns/cjooids for fees with the originoid from this grp
            groupPsnCjoOidMap = getPsnFeeMap(groupOid, psnOids, false);

            // remove psn oids from a map of psn oid/cjo oid
            // groupPsnCjoOidMap.keySet().removeAll(psnOids);

            // if there are no groups this map will be null and we just want to return an empty map
            if (groupPsnCjoOidMap != null) {
                droppedCjoOids.addAll(groupPsnCjoOidMap);
            }
        }

        return droppedCjoOids;
    }


    /**
     * Return a formatted string for logging CashiersJournalEntry's during the procedure.
     *
     * @param cjo CashiersJournalEntry
     * @return String
     */
    private String getFormattedCashierJournalEntry(CashiersJournalEntry cjo) {
        return "Fee Reference Number: " + cjo.getReferenceNumber() + " Name: " + cjo.getNameView() + " Date: "
                + cjo.getDate();
    }

    /**
     * Returns a map of either studentOid or personOid to cjoOid for any cjos with an origin oid
     * that matches the passed oid
     *
     * If the Oids being passed for the not in criteria are psnOIDs than the getStudentOid flag
     * should be false and if the
     * Oids are stdOIDs than the getStudentOid flag should be true.
     *
     * @param Oid String
     * @param notInOids List<String>
     * @param getStudentOid boolean
     * @return groupPsnCjoOidMap
     */
    private List<String> getPsnFeeMap(String Oid, List<String> notInOids, boolean getStudentOid) {
        List<String> groupPsnCjoOidMap = new ArrayList<String>();

        QueryByCriteria query =
                new QueryByCriteria(CashiersJournalEntry.class, buildFeesCriteria(Oid, notInOids, getStudentOid));
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                CashiersJournalEntry cjo = (CashiersJournalEntry) iterator.next();
                if (getStudentOid) {
                    groupPsnCjoOidMap.add(cjo.getOid());
                } else {
                    groupPsnCjoOidMap.add(cjo.getOid());
                }
            }
        } finally {
            iterator.close();
        }

        return groupPsnCjoOidMap;
    }

    /**
     * Returns a list of all psn's for a group via a collection of group members for a single group.
     *
     * @param groupMembers Collection<GroupMember>
     * @return psnOids
     */
    private List<String> getPsnOidsForGroup(Collection<GroupMember> groupMembers) {
        List<String> psnOids = new ArrayList<String>();

        for (GroupMember groupMember : groupMembers) {
            GroupMemberRetriever memberRetriever = null;
            Integer typeIndex = groupMember.getMemberTypeEnum().getTypeIndex();

            // D-28427 - It was decided to label these member types as unsupported rather
            // than to add the functionality for them because this procedure and the Resolve
            // Dropped Fees procedure are very old and Cashier's Office already has a way to
            // create fees for multiple people.
            if (DEPARTMENT_INDEX.equals(typeIndex)
                    || SCHOOL_LEVEL_INDEX.equals(typeIndex)
                    || GRADE_LEVEL_INDEX.equals(typeIndex)
                    || EXTRACURRICULAR_INDEX.equals(typeIndex)
                    || SECTION_ROSTER_INDEX.equals(typeIndex)) {
                logMessage("The group member " + groupMember.getMemberView()
                        + " is of a type (" + groupMember.getMemberTypeEnum().getId()
                        + ") that is not supported in this procedure.");
            } else {
                memberRetriever = groupMember.getMemberTypeEnum().getMemberRetriever();
            }

            if (memberRetriever != null) {
                String[] columns = new String[] {X2BaseBean.COL_OID};

                X2BaseBean memberObject = getBroker().getBeanByOid(groupMember.getMemberTypeEnum().getMemberClass(),
                        groupMember.getObjectOid());

                // D-28427 - If any districts still use this procedure, this fixes an issue where
                // certain member types could cause fees to be applied to students that are not
                // members of the group
                boolean restrictMemberInSchool = m_userData.getApplicationContext() == ApplicationContext.SCHOOL &&
                        groupMember.getGroup().getOwnerType() == Ownable.OWNER_TYPE_SCHOOL;
                // build a list of all the people in the group
                Criteria sisPersonCriteria = GroupMembershipManagerSis.getPeopleCriteria(groupMember, memberObject,
                        getBroker(), restrictMemberInSchool);

                ReportQueryByCriteria sisPersonQuery =
                        new ReportQueryByCriteria(SisPerson.class, columns, sisPersonCriteria);
                ReportQueryIterator personIterator = getBroker().getReportQueryIteratorByQuery(sisPersonQuery);
                try {
                    while (personIterator.hasNext()) {
                        Object[] row = (Object[]) personIterator.next();
                        String personOid = row[0].toString();
                        psnOids.add(personOid);
                    }
                } finally {
                    personIterator.close();
                }
            }
        }
        return psnOids;
    }

    /**
     * Based on user input for groups and courses handle the resolution outstanding fees (delete,
     * void or mark for
     * manual intervention).
     *
     * @param deleteFees boolean
     */
    private void resolveFees(boolean deleteFees) {
        List<String> cjoOidList = new ArrayList<String>();

        boolean inputGroups = ((Boolean) getParameter(INPUT_PARAM_GROUPS)).booleanValue();
        boolean inputCourses = ((Boolean) getParameter(INPUT_PARAM_COURSES)).booleanValue();

        if (inputGroups == true) {
            cjoOidList.addAll(getDroppedGroupFees());
        }

        if (inputCourses == true) {
            cjoOidList.addAll(getDroppedCourseFees());
        }

        for (String cjoOid : cjoOidList) {
            CashiersJournalEntry cjo =
                    (CashiersJournalEntry) getBroker().getBeanByOid(CashiersJournalEntry.class, cjoOid);
            if (cjo.getAmountPaid().compareTo(BigDecimal.ZERO) != 0) {
                setManualInterventionRequired(cjo);
                // Only for logging purposes
                refundsRequired++;
            } else {
                if (deleteFees) {
                    deleteFee(cjo);
                    // Only for logging purposes
                    deletedFees++;
                } else {
                    voidFee(cjo);
                    // Only for logging purposes
                    voidedFees++;
                }
            }
        }
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
        m_userData = userData;
    }

    /**
     * Set a field with alias 'CASH-REFUND-REQUIRED' on CashiersJournalEntry to true.
     *
     * @param cjo void
     */
    private void setManualInterventionRequired(CashiersJournalEntry cjo) {
        logMessage("Manual Intervention Required for: " + getFormattedCashierJournalEntry(cjo));
        cjo.setFieldValueByAlias(USER_DFINED_ALIAS, "1");
        getBroker().saveBeanForced(cjo);
    }

    /**
     * Log and void CashiersJournalEntry bean. This is done by adding 2 debit itemizations that
     * offset each other,
     * applying the items to the CashiersJournalEntry and flagging the initial CashiersJournalEntry
     * as void.
     *
     * @param cjo CashiersJournalEntry
     */
    private void voidFee(CashiersJournalEntry cjo) {
        // set the voided indicator to true and save
        cjo.setVoidedIndicator(true);
        cjo.setAmount(BigDecimal.ZERO);
        cjo.setAmountDue(BigDecimal.ZERO);

        getBroker().saveBeanForced(cjo);

        // we need to save the cjo first before creating the item so that the cjo has an OID to
        // apply the item to
        createOffsettingItemization(cjo);

        logMessage("Fee voided: " + getFormattedCashierJournalEntry(cjo));
    }
}
