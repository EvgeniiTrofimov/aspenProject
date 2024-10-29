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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Group;
import com.follett.fsc.core.k12.beans.GroupMember;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.groups.GroupMemberType;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.groups.GroupMemberRetriever;
import com.x2dev.sis.model.beans.CashiersJournalDebitItem;
import com.x2dev.sis.model.beans.CashiersJournalEntry;
import com.x2dev.sis.model.beans.CashiersJournalEntry.EntryType;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.community.GroupMemberTypeErm;
import com.x2dev.sis.model.business.community.GroupMembershipManagerSis;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.web.cashier.CashiersOfficeConstants;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This procedure adds fees to all groups and school courses that have
 * fee types assigned to them. This procedure will only assess a fee to
 * persons who have not already had the fee assessed (unless there is a
 * frequency associated with the fee type and this procedure will check
 * to see if the fee should be reassessed).
 *
 * When using a frequency the reference table rtbFeeFreq must have codes
 * added to it with the frequency set on the local code in milliseconds
 *
 * For example: One of the default frequency reference codes is 'Annual'
 * and has a local code of 31556900000 which represents a year in
 * milliseconds, if the user wanted to define their own frequency code
 * of 'Monthly' they would enter 2629741666 (31556900000/12) as the local
 * code
 *
 * @author X2 Development Corporation
 */
public class CreateBatchFees extends ProcedureJavaSource {
    private final String INPUT_PARAM_GROUPS = "groups";
    private final String INPUT_PARAM_COURSES = "courses";
    private final String RESULT_SET_PSN_PARAM = "PSN_OID";
    private final String RESULT_SET_FIRST_NAME_PARAM = "PSN_NAME_FIRST";
    private final String RESULT_SET_LAST_NAME_PARAM = "PSN_NAME_LAST";
    private final String RESULT_SET_NAME_VIEW_PARAM = "nameView";

    private UserDataContainer m_userData;
    private Collection<String> m_courseOids;
    private Collection<String> m_groupOids;
    private Collection<String> m_activeScheduleOids;
    private Map<String, String> m_sscPsnOids;
    private Map<String, String> m_psnStdOids;

    private Map<String, String> m_psnOidNameView = new HashMap<String, String>();
    private Map<String, String> m_GroupRefcodeMap = new HashMap<String, String>();
    private Map<String, String> m_StudentScheduleRefcodeMap = new HashMap<String, String>();
    private Map<String, ReferenceCode> m_lazyFreqRefCodes = new HashMap<String, ReferenceCode>();

    // Used for logging totals
    private int groupFees = 0;
    private int courseFees = 0;

    // Unsupported Group Member Types
    private static final Integer SCHOOL_LEVEL_INDEX = GroupMemberType.SCHOOL_LEVEL.getTypeIndex();
    private static final Integer GRADE_LEVEL_INDEX = GroupMemberType.GRADE_LEVEL.getTypeIndex();
    private static final Integer DEPARTMENT_INDEX = GroupMemberType.DEPARTMENT.getTypeIndex();
    private static final Integer EXTRACURRICULAR_INDEX = GroupMemberTypeErm.EXTRACURRICULAR_PROGRAME.getTypeIndex();

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // Begin by loading several collections/maps that will be used so that we don't have to
        // re-query later
        loadGroupsWithFees();
        loadCoursesWithFees();
        loadSscPsnAndpsnStdMaps();

        ScheduleManager scheduleManager = new ScheduleManager(getBroker());
        m_activeScheduleOids = scheduleManager.getActiveScheduleOids();

        if (((Boolean) getParameter(INPUT_PARAM_GROUPS)).booleanValue() == true) {
            addGroupFees();
            logMessage("\n");
            logMessage("---------------------------------");
            logMessage("Total group fees created: " + groupFees);
            logMessage("\n");
        }

        if (((Boolean) getParameter(INPUT_PARAM_COURSES)).booleanValue() == true) {
            addCourseFees();
            logMessage("\n");
            logMessage("---------------------------------");
            logMessage("Total course fees created: " + courseFees);
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
     * Adds all Course fees.
     *
     * @throws SQLException exception
     */
    private void addCourseFees() throws SQLException {
        Map<String, Collection<String>> psnCourseOIDs = getCoursePsnOids();
        createCourseFeeRecords(psnCourseOIDs);
    }

    /**
     * Adds all Group fees.
     *
     * @throws SQLException exception
     */
    private void addGroupFees() throws SQLException {
        Map<String, Collection<String>> psnGroupOIDs = getGroupPsnOids();
        createGroupFeeRecords(psnGroupOIDs);
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
     * Creates a new JournalEntryDebit record from a list of psn OIDs.
     *
     * @param psnCourseOIDs Map<String,Collection<String>>
     */
    private void createCourseFeeRecords(Map<String, Collection<String>> psnCourseOIDs) {
        for (String psnOid : psnCourseOIDs.keySet()) {
            for (String cskOid : psnCourseOIDs.get(psnOid)) {
                String refCode = m_StudentScheduleRefcodeMap.get(cskOid);

                CashiersJournalEntry cjo = createFeeFromRefCode(refCode, psnOid);

                if (cjo != null) {
                    cjo.setOriginOid(cskOid);

                    getBroker().saveBeanForced(cjo);

                    // create the associated item for this fee and save it
                    createItemForEntry(cjo);

                    // used for logging only
                    courseFees++;

                    logMessage("Created fee for " + m_psnOidNameView.get(psnOid) + " for the amount of $"
                            + cjo.getAmountDue() + ".");
                }
            }
        }
    }

    /**
     * Returns a fee based on the passed reference code and person id.
     *
     * @param refCode String
     * @param psnOid String
     * @return CashiersJournalEntry
     */
    private CashiersJournalEntry createFeeFromRefCode(String refCode, String psnOid) {
        CashiersJournalEntry cjo = null;
        // Reference code bean
        ReferenceCode referenceCodeBean =
                ReferenceManager.getCode(ReferenceTable.REF_TABLE_OID_FEES_AND_FINES, refCode, m_userData, getBroker());

        if (referenceCodeBean != null) {
            cjo = new CashiersJournalEntry(getBroker().getPersistenceKey());

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_userData.getPersistenceKey());
            ModelProperty quantityProperty =
                    new ModelProperty(CashiersJournalEntry.class, CashiersJournalEntry.COL_QUANTITY, dictionary);
            ModelProperty priceProperty =
                    new ModelProperty(CashiersJournalEntry.class, CashiersJournalEntry.COL_PRICE, dictionary);

            ModelProperty stateTaxProperty =
                    new ModelProperty(CashiersJournalEntry.class, CashiersJournalEntry.COL_STATE_TAX, dictionary);
            ModelProperty fedTaxProperty =
                    new ModelProperty(CashiersJournalEntry.class, CashiersJournalEntry.COL_FEDERAL_TAX, dictionary);
            ModelProperty localTaxProperty =
                    new ModelProperty(CashiersJournalEntry.class, CashiersJournalEntry.COL_LOCAL_TAX, dictionary);

            BigDecimal quantity = new BigDecimal(
                    (String) referenceCodeBean.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_DEFAULT_QUANTITY))
                            .setScale(quantityProperty.getField().getDatabaseDecimal(), RoundingMode.HALF_UP);
            BigDecimal price = new BigDecimal(
                    (String) referenceCodeBean.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_DEFAULT_AMOUNT))
                            .setScale(priceProperty.getField().getDatabaseDecimal(), RoundingMode.HALF_UP);

            BigDecimal stateTax = new BigDecimal(
                    (String) referenceCodeBean.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_STATE_TAX))
                            .setScale(stateTaxProperty.getField().getDatabaseDecimal(), RoundingMode.HALF_UP);
            BigDecimal fedTax = new BigDecimal(
                    (String) referenceCodeBean.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_FEDERAL_TAX))
                            .setScale(fedTaxProperty.getField().getDatabaseDecimal(), RoundingMode.HALF_UP);
            BigDecimal localTax = new BigDecimal(
                    (String) referenceCodeBean.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_LOCAL_TAX))
                            .setScale(localTaxProperty.getField().getDatabaseDecimal(), RoundingMode.HALF_UP);

            cjo.setEntryTypeEnum(EntryType.DEBIT);
            cjo.setPrice(price);
            cjo.setQuantity(quantity);
            cjo.setAmount(price.multiply(quantity).setScale(2, RoundingMode.HALF_UP));
            cjo.setAmountDue(price.multiply(quantity).setScale(2, RoundingMode.HALF_UP));
            cjo.setAmountPaid(BigDecimal.ZERO);
            cjo.setDate(new PlainDate());
            cjo.setPersonOid(psnOid);
            cjo.setFeeType(refCode);

            cjo.setStateTax(stateTax.multiply(quantity).setScale(2, RoundingMode.HALF_UP));
            cjo.setFederalTax(fedTax.multiply(quantity).setScale(2, RoundingMode.HALF_UP));
            cjo.setLocalTax(localTax.multiply(quantity).setScale(2, RoundingMode.HALF_UP));

            cjo.setAmountCredit(BigDecimal.ZERO);
            cjo.setAmountApplied(BigDecimal.ZERO);
            cjo.setComment("");
            cjo.setDescription(referenceCodeBean.getDescription());

            if (m_userData.getApplicationContext() == ApplicationContext.SCHOOL) {
                cjo.setSchoolOid(m_userData.getSchoolOid());
                cjo.setSchoolAppliedOid(m_userData.getSchoolOid());
            }

            if (m_psnStdOids.containsKey(psnOid)) {
                cjo.setStudentOid(m_psnStdOids.get(psnOid));
            }

            if (m_psnOidNameView.containsKey(psnOid)) {
                cjo.setNameView(m_psnOidNameView.get(psnOid));
            }
        }

        return cjo;
    }

    /**
     * Creates a new JournalEntryDebit record from a list of psn OIDs.
     *
     * @param psnGroupOIDs Map<String,Collection<String>>
     */
    private void createGroupFeeRecords(Map<String, Collection<String>> psnGroupOIDs) {
        for (String psnOid : psnGroupOIDs.keySet()) {
            for (String grpOid : psnGroupOIDs.get(psnOid)) {
                String refCode = m_GroupRefcodeMap.get(grpOid);

                CashiersJournalEntry cjo = createFeeFromRefCode(refCode, psnOid);

                if (cjo != null) {
                    cjo.setOriginOid(grpOid);

                    getBroker().saveBeanForced(cjo);

                    // create the associated item for this fee and save it
                    createItemForEntry(cjo);

                    // used for logging only
                    groupFees++;

                    logMessage("Created fee for " + m_psnOidNameView.get(psnOid) + " for the amount of $"
                            + cjo.getAmountDue() + ".");
                }
            }
        }
    }

    /**
     * Creates and saves a single debit item based on and associated to a CashierJournalEntry.
     *
     * @param cjo CashiersJournalEntry
     */
    private void createItemForEntry(CashiersJournalEntry cjo) {
        CashiersJournalDebitItem feeCjd = new CashiersJournalDebitItem(getBroker().getPersistenceKey());

        feeCjd.setCashiersJournalEntryOid(cjo.getOid());
        feeCjd.setType(cjo.getFeeType());
        feeCjd.setDescription(cjo.getDescription());
        feeCjd.setComment(cjo.getComment());
        feeCjd.setAmount(cjo.getAmount());
        feeCjd.setPrice(cjo.getPrice());
        feeCjd.setQuantity(cjo.getQuantity());
        feeCjd.setStateTax(cjo.getStateTax());
        feeCjd.setFederalTax(cjo.getFederalTax());
        feeCjd.setLocalTax(cjo.getLocalTax());
        feeCjd.setVoidedIndicator(false);

        getBroker().saveBeanForced(feeCjd);
    }

    /**
     * Gets the date string for the passed course.
     *
     * @param course SchoolCourse
     * @return String
     */
    private String getCourseDate(SchoolCourse course) {
        String refCode = course.getFeeType();
        return getOneFeeTypePeriod(refCode);
    }

    /**
     * Return all psn Ids for all school courses (with fees) that should have the fee applied. This
     * criteria should be all people in the school course
     * who do not have a fee assessed for the school course already unless the fee was assessed more
     * than one period (based on frequency for the fee type) before
     * the date that this method is being called.
     *
     * @return Map
     * @throws SQLException exception
     */
    private Map<String, Collection<String>> getCoursePsnOids() throws SQLException {
        Map<String, Collection<String>> psnOidSchoolCourseMap = new HashMap<String, Collection<String>>();

        QueryByCriteria studentScheduleQuery = new QueryByCriteria(
                StudentSchedule.class, buildStudentScheduleCriteria());

        // school source, student schedule
        Map<String, List<StudentSchedule>> studentScheduleMap =
                getBroker().getGroupedCollectionByQuery(studentScheduleQuery,
                        StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_SCHOOL_COURSE_OID, 10);

        for (String cskOid : studentScheduleMap.keySet()) {
            SchoolCourse schoolCourse = (SchoolCourse) getBroker().getBeanByOid(SchoolCourse.class, cskOid);

            String preparedSqlStatement = getPreparedFeesSqlFromSchedules(studentScheduleMap.get(cskOid));

            PreparedStatement statement = null;

            ResultSet resultSet = null;

            Connection connection = getBroker().borrowConnection();
            try {
                statement = connection.prepareStatement(preparedSqlStatement);

                statement.setString(1, getCourseDate(schoolCourse));
                statement.setString(2, schoolCourse.getOid());
                List<StudentSchedule> ssc = studentScheduleMap.get(cskOid);
                for (int i = 0; i < studentScheduleMap.get(cskOid).size(); i++) {
                    String sscOid = ssc.get(i).getOid();
                    statement.setString(i + 3, m_sscPsnOids.get(sscOid));
                }

                resultSet = statement.executeQuery();

                while (resultSet.next()) {
                    String psnOid = resultSet.getString(RESULT_SET_PSN_PARAM);
                    if (psnOidSchoolCourseMap.get(psnOid) == null) {
                        psnOidSchoolCourseMap.put(psnOid, new HashSet<String>());
                    }
                    psnOidSchoolCourseMap.get(psnOid).add(schoolCourse.getOid());

                    String viewName = resultSet.getString(RESULT_SET_NAME_VIEW_PARAM);

                    if (StringUtils.isEmpty(viewName)) {
                        String firstName = resultSet.getString(RESULT_SET_FIRST_NAME_PARAM);
                        String lastName = resultSet.getString(RESULT_SET_LAST_NAME_PARAM);
                        viewName = lastName + ", " + firstName;
                    }

                    m_psnOidNameView.put(resultSet.getString(RESULT_SET_PSN_PARAM), viewName);
                }

                m_StudentScheduleRefcodeMap.put(schoolCourse.getOid(), schoolCourse.getFeeType());

            } catch (SQLException e) {
                String message = (e.getMessage() == null) ? "" : e.getMessage();
                AppGlobals.getLog().log(Level.WARNING, message, e);
            } finally {
                if (resultSet != null) {
                    resultSet.close();
                }
                if (statement != null) {
                    statement.close();
                }
                getBroker().returnConnection();
            }
        }
        return psnOidSchoolCourseMap;
    }



    /**
     * Gets the date string for the passed group.
     *
     * @param group Group
     * @return String
     */
    private String getGroupDate(Group group) {
        String refCode = group.getFeeType();
        return getOneFeeTypePeriod(refCode);
    }

    /**
     * Return all psn Ids for all groups (with fees) that should have the fee applied. This criteria
     * should be all people in the group
     * who do not have a fee assessed for the group already unless the fee was assessed more than
     * one period (based on frequency
     * for the fee type) before the date that this method is being called.
     *
     * @return Map
     * @throws SQLException exception
     */
    private Map<String, Collection<String>> getGroupPsnOids() throws SQLException {
        Map<String, Collection<String>> psnOidGroupMap = new HashMap<String, Collection<String>>();

        QueryByCriteria groupMemberQuery = new QueryByCriteria(GroupMember.class, buildGroupMemberCriteria());
        // groupOid, group Members
        Map<String, Collection<GroupMember>> groupMemberMap =
                getBroker().getGroupedCollectionByQuery(groupMemberQuery, GroupMember.COL_GROUP_OID, 10);

        for (String groupOid : groupMemberMap.keySet()) {
            if (m_groupOids.contains(groupOid)) {
                Group group = (Group) getBroker().getBeanByOid(Group.class, groupOid);
                for (GroupMember groupMember : groupMemberMap.get(groupOid)) {
                    GroupMemberRetriever memberRetriever = null;
                    Integer typeIndex = groupMember.getMemberTypeEnum().getTypeIndex();

                    // D-28427 - It was decided to label these member types as unsupported rather
                    // than to add the functionality for them because this procedure and the Resolve
                    // Dropped Fees procedure are very old and Cashier's Office already has a way to
                    // create fees for multiple people.
                    if (DEPARTMENT_INDEX.equals(typeIndex)
                            || SCHOOL_LEVEL_INDEX.equals(typeIndex)
                            || GRADE_LEVEL_INDEX.equals(typeIndex)
                            || EXTRACURRICULAR_INDEX.equals(typeIndex)) {
                        logMessage("The group member " + groupMember.getMemberView()
                                + " is of a type (" + groupMember.getMemberTypeEnum().getId()
                                + ") that is not supported in this procedure.");
                    } else {
                        memberRetriever = groupMember.getMemberTypeEnum().getMemberRetriever();
                    }

                    if (memberRetriever != null) {
                        List<String> psnOids = new ArrayList<String>();

                        String[] columns = new String[] {X2BaseBean.COL_OID};

                        X2BaseBean memberObject =
                                getBroker().getBeanByOid(groupMember.getMemberTypeEnum().getMemberClass(),
                                        groupMember.getObjectOid());

                        // D-28427 - If any districts still use this procedure, this fixes an issue
                        // where
                        // certain
                        // member types could cause fees to be applied to students that are not
                        // members of
                        // the group
                        boolean restrictMemberInSchool =
                                m_userData.getApplicationContext() == ApplicationContext.SCHOOL &&
                                        groupMember.getGroup().getOwnerType() == Ownable.OWNER_TYPE_SCHOOL;
                        // build a list of all the people in the group
                        Criteria sisPersonCriteria =
                                GroupMembershipManagerSis.getPeopleCriteria(groupMember, memberObject,
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

                        populatePsnOidGroupMap(psnOidGroupMap, group, psnOids);
                    }
                }
            }
        }
        return psnOidGroupMap;
    }


    /**
     * Populate psn oid group map.
     *
     * @param psnOidGroupMap Map<String,Collection<String>>
     * @param group Group
     * @param psnOids List<String>
     */
    private void populatePsnOidGroupMap(Map<String, Collection<String>> psnOidGroupMap,
                                        Group group,
                                        List<String> psnOids) {

        String preparedSqlStatement;

        preparedSqlStatement = getPreparedFeesSql(psnOids);

        ResultSet resultSet = null;

        try (Connection connection = getBroker().borrowConnection();
                PreparedStatement statement = connection.prepareStatement(preparedSqlStatement)) {

            statement.setString(1, getGroupDate(group));
            statement.setString(2, group.getOid());
            for (int i = 0; i < psnOids.size(); i++) {
                statement.setString(i + 3, psnOids.get(i));
            }

            resultSet = statement.executeQuery();

            while (resultSet.next()) {
                String psnOid = resultSet.getString(RESULT_SET_PSN_PARAM);
                if (psnOidGroupMap.get(psnOid) == null) {
                    psnOidGroupMap.put(psnOid, new HashSet<String>());
                }
                psnOidGroupMap.get(psnOid).add(group.getOid());

                String firstName = resultSet.getString(RESULT_SET_FIRST_NAME_PARAM);
                String lastName = resultSet.getString(RESULT_SET_LAST_NAME_PARAM);
                String viewName = resultSet.getString(RESULT_SET_NAME_VIEW_PARAM);

                if (StringUtils.isEmpty(viewName)) {
                    viewName = lastName + ", " + firstName;
                }

                m_psnOidNameView.put(resultSet.getString(RESULT_SET_PSN_PARAM), viewName);
            }

            m_GroupRefcodeMap.put(group.getOid(), group.getFeeType());

        } catch (SQLException e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            getBroker().returnConnection();
        }
    }


    /**
     * Returns a string representation for the date that is one period before the current date for
     * the given
     * reference code, example: annual reference code would return "2013-01-01" if the method was
     * called on 2014-01-01.
     *
     * @param refCode String
     * @return String
     */
    private String getOneFeeTypePeriod(String refCode) {
        // if there is no localCode value or no frequency assume that we only want to apply the fee
        // once so look for anything within the last 10 years
        PlainDate today = new PlainDate();
        PlainDate onePeriodAgo = DateUtils.add(today, Calendar.YEAR, -10);

        ReferenceCode referenceCodeBean =
                ReferenceManager.getCode(ReferenceTable.REF_TABLE_OID_FEES_AND_FINES, refCode, m_userData, getBroker());

        if (referenceCodeBean != null) {
            String freqRefCode =
                    ((String) referenceCodeBean.getFieldValueByAlias(CashiersOfficeConstants.ALIAS_FEE_FREQ));

            // Lazy load our map with frequency code strings -> frequency Code beans so we don't
            // need to query each time when we likely arent looking
            // at many different frequency codes and are also likely not storing very many

            ReferenceCode freqReferenceCodeBean = null;

            if (m_lazyFreqRefCodes.containsKey(freqRefCode)) {
                freqReferenceCodeBean = m_lazyFreqRefCodes.get(freqRefCode);
            } else {
                freqReferenceCodeBean = ReferenceManager.getCode(ReferenceTable.REF_TABLE_OID_FEE_FREQUENCY,
                        freqRefCode, m_userData, getBroker());
                m_lazyFreqRefCodes.put(freqRefCode, freqReferenceCodeBean);
            }

            if (freqReferenceCodeBean != null && freqReferenceCodeBean.getLocalCode() != null) {
                long onePeriodAgoMilli = today.getTime() - Long.parseLong(freqReferenceCodeBean.getLocalCode());
                onePeriodAgo = new PlainDate(onePeriodAgoMilli);
            }
        }

        return onePeriodAgo.toString();
    }

    /**
     * Writes the raw sql criteria from a list of person ids, this list should be the all students
     * who should have the fee applied
     * including a check for frequency of the last fee applied.
     *
     * @param psnOids Collection<String>
     * @return String
     */
    private String getPreparedFeesSql(Collection<String> psnOids) {
        StringBuilder sql = new StringBuilder();
        sql.append("SELECT CJO_OID, PSN_OID, PSN_NAME_FIRST, PSN_NAME_LAST, ");
        sql.append("CASE WHEN STD_NAME_VIEW IS NOT NULL THEN STD_NAME_VIEW ELSE STF_NAME_VIEW END AS "
                + RESULT_SET_NAME_VIEW_PARAM + " ");
        sql.append("FROM PERSON ");
        sql.append("LEFT OUTER JOIN STUDENT ON STUDENT.STD_PSN_OID = PERSON.PSN_OID ");
        sql.append("LEFT OUTER JOIN STAFF ON STAFF.STF_PSN_OID = PERSON.PSN_OID ");
        sql.append("LEFT OUTER JOIN CASHIERS_JOURNAL ");
        sql.append("ON (PERSON.PSN_OID = CASHIERS_JOURNAL.CJO_PSN_OID ");
        sql.append("AND CASHIERS_JOURNAL.CJO_DATE > ? ");
        sql.append("AND CASHIERS_JOURNAL.CJO_ORIGIN_OID = ?) ");
        sql.append("WHERE PSN_OID IN (");
        for (int i = 0; i < psnOids.size(); i++) {
            if (i == psnOids.size() - 1) {
                sql.append("?)");
            } else {
                sql.append("?,");
            }
        }
        sql.append("AND CJO_OID IS NULL");

        return sql.toString();
    }

    /**
     * Get a list of person ids from the collection of schedules and call getPreparedFeesSql with
     * that list.
     *
     * @param schedules Collection<StudentSchedule>
     * @return String
     */
    private String getPreparedFeesSqlFromSchedules(Collection<StudentSchedule> schedules) {
        List<String> psnOids = new ArrayList<String>();

        for (StudentSchedule ssc : schedules) {
            psnOids.add(m_sscPsnOids.get(ssc.getOid()));
        }

        return getPreparedFeesSql(psnOids);
    }

    /**
     * Loads a Collection of Course OID's that contain a fee.
     */
    private void loadCoursesWithFees() {
        m_courseOids = new ArrayList<String>();

        Criteria courseCriteria = new Criteria();
        courseCriteria.addNotNull(SchoolCourse.COL_FEE_TYPE);

        QueryByCriteria query = new QueryByCriteria(SchoolCourse.class, courseCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SchoolCourse course = (SchoolCourse) iterator.next();
                m_courseOids.add(course.getOid());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a Collection of Course OID's that contain a fee. If this procedure is being run from
     * the school view, scope the groups
     * to that school.
     */
    private void loadGroupsWithFees() {
        m_groupOids = new ArrayList<String>();

        Criteria groupMember = new Criteria();
        groupMember.addNotNull(Group.COL_FEE_TYPE);

        School school = m_userData.getSchool();

        if (school != null && m_userData.getApplicationContext().isSchoolScoped()) {
            groupMember.addEqualTo(Group.COL_CONTEXT_OBJECT_OID, school.getOid());
        }

        QueryByCriteria query = new QueryByCriteria(Group.class, groupMember);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Group member = (Group) iterator.next();
                m_groupOids.add(member.getOid());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a Collection of active Schedule oids for all the active, non-archive schools.
     */
    private void loadSscPsnAndpsnStdMaps() {
        m_sscPsnOids = new HashMap<String, String>();
        m_psnStdOids = new HashMap<String, String>();

        X2Criteria studentScheduleCriteria = new X2Criteria();
        studentScheduleCriteria.addNotEmpty(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.REL_PERSON,
                getBroker().getPersistenceKey());

        String[] columns = new String[] {X2BaseBean.COL_OID, StudentSchedule.COL_STUDENT_OID, Student.COL_PERSON_OID};

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(StudentSchedule.class, columns, studentScheduleCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentScheduleOid = row[0].toString();
                String studentOid = row[1].toString();
                String personOid = row[2].toString();

                m_psnStdOids.put(personOid, studentOid);
                m_sscPsnOids.put(studentScheduleOid, personOid);
            }
        } finally {
            iterator.close();
        }
    }
}
