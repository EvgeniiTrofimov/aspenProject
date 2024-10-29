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
package com.x2dev.procedures.statereporting.fl;

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STAFF_ACTIVE_CODE;
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.procedures.statereporting.fl.FLStateReportData.ALIAS_WDIS_AGE_INDICTOR;
import static com.x2dev.procedures.statereporting.fl.FLStateReportData.WDIS_MODE_ADULT_GENERAL_EDUCATION;
import static com.x2dev.procedures.statereporting.fl.FLStateReportData.WDIS_MODE_POST_SECONDARY_CTE;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.DistinctAdjuster;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.x2dev.procedures.statereporting.fl.FLScheduleHelper.MasterScheduleInfo;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLStaffHelper.
 */
public class FLStaffHelper {

    /**
     * The Class ScheduleTeacherInfo.
     */
    public class ScheduleTeacherInfo {
        private String m_certificationStatus;
        private String m_highlyQualifiedStatus;
        private MasterScheduleInfo m_masterScheduleInfo;
        private ScheduleTeacher m_schedule;
        private String m_schedulingMethod;
        private StaffInfo m_staffInfo;
        private String m_teamTeacherTraining;
        private Boolean m_titleIIIFundedIndicator;

        /**
         * Instantiates a new schedule teacher info.
         *
         * @param schedule the schedule
         */
        public ScheduleTeacherInfo(ScheduleTeacher schedule) {
            m_schedule = schedule;
        }

        /**
         * Gets the certification status.
         *
         * @return the certification status
         * @throws X2BaseException the x 2 base exception
         */
        public String getCertificationStatus() throws X2BaseException {
            if (m_certificationStatus == null) {
                m_certificationStatus = "";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_CERTIFICATION_STATUS_MTC, false);
                if (field != null) {
                    m_certificationStatus = ((String) getFLData().getFieldValue(m_schedule, field));
                }
            }
            return m_certificationStatus;
        }

        /**
         * Gets the highly qualified status.
         *
         * @return the highly qualified status
         * @throws X2BaseException the x 2 base exception
         */
        public String getHighlyQualifiedStatus() throws X2BaseException {
            if (m_highlyQualifiedStatus == null) {
                m_highlyQualifiedStatus = "";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_HIGHLY_QUALIFIED_STATUS_MTC, false);
                if (field != null) {
                    m_highlyQualifiedStatus = ((String) getFLData().getFieldValue(m_schedule, field));
                }
            }
            return m_highlyQualifiedStatus;
        }

        /**
         * Gets the master schedule info.
         *
         * @return the master schedule info
         */
        public MasterScheduleInfo getMasterScheduleInfo() {
            if (m_masterScheduleInfo == null) {
                m_masterScheduleInfo = getScheduleHelper().getMasterScheduleInfo(m_schedule.getSectionOid());
            }
            return m_masterScheduleInfo;
        }

        /**
         * Gets the primary teacher indicator.
         *
         * @return the primary teacher indicator
         */
        public Boolean getPrimaryTeacherIndicator() {
            return m_schedule.getPrimaryTeacherIndicator() ? Boolean.TRUE : Boolean.FALSE;
        }

        /**
         * Gets the NCLB title III funded indicator.
         *
         * @return the NCLB title III funded indicator
         * @throws X2BaseException the x 2 base exception
         */
        public Boolean getNCLBTitleIIIFundedIndicator() throws X2BaseException {
            if (m_titleIIIFundedIndicator == null) {

                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_TITLE_III_FUNDED_MTC, false);
                if (field != null) {
                    m_titleIIIFundedIndicator = ((Boolean) getFLData().getFieldValue(m_schedule, field));
                }
                if (m_titleIIIFundedIndicator == null) {
                    m_titleIIIFundedIndicator = Boolean.FALSE;
                }
            }
            return m_titleIIIFundedIndicator;
        }


        /**
         * Gets the scheduling method.
         *
         * @return the scheduling method
         * @throws X2BaseException the x 2 base exception
         */
        public String getSchedulingMethod() throws X2BaseException {
            if (m_schedulingMethod == null) {
                m_schedulingMethod = "";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_SCHEDULING_METHO_MTC, false);
                if (field != null) {
                    m_schedulingMethod = ((String) getFLData().getFieldValue(m_schedule, field));
                }
            }
            return m_schedulingMethod;
        }

        /**
         * Gets the staff info.
         *
         * @return the staff info
         */
        public StaffInfo getStaffInfo() {
            if (m_staffInfo == null) {
                m_staffInfo = FLStaffHelper.this.getStaffInfo(m_schedule.getStaff());
            }
            return m_staffInfo;
        }

        /**
         * Gets the team teacher training.
         *
         * @return the team teacher training
         * @throws X2BaseException the x 2 base exception
         */
        public String getTeamTeacherTraining() throws X2BaseException {
            if (m_teamTeacherTraining == null) {
                m_teamTeacherTraining = "Z";
                DataDictionaryField field =
                        getFLData().translateAliasToDictionaryField(ALIAS_TEAM_TEACHER_TRAINING_MTC, false);
                if (field != null) {
                    m_teamTeacherTraining = ((String) getFLData().getFieldValue(m_schedule, field));
                }
            }
            return m_teamTeacherTraining;
        }

    }

    /**
     * The Class StaffInfo.
     */
    public class StaffInfo {
        private static final int MAX_LEGAL_NAME_LEN = 42;

        private String m_certificationNumber;
        private SisStaff m_staff;

        /**
         * Instantiates a new staff info.
         *
         * @param staff the staff
         */
        StaffInfo(SisStaff staff) {
            m_staff = staff;
        }

        /**
         * Format staff name according to FL standard
         * http://www.fldoe.org/core/fileparse.php/12025/urlt/1516-203905.pdf
         *
         * @return String
         */
        public String formatStaffLegalName() {
            StringBuilder sb = new StringBuilder(m_staff.getPerson().getLastName());
            String appendage = m_staff.getPerson().getNameSuffixCode();
            if (!StringUtils.isEmpty(appendage)) {
                sb.append(" ");
                sb.append(appendage);
            }
            String fname = m_staff.getPerson().getFirstName();
            if (!StringUtils.isEmpty(fname)) {
                sb.append(" ");
                sb.append(fname);
            }
            String mname = m_staff.getPerson().getMiddleName();
            if (!StringUtils.isEmpty(mname)) {
                sb.append(" ");
                sb.append(mname);
            }
            return sb.length() <= MAX_LEGAL_NAME_LEN ? sb.toString() : sb.substring(0, MAX_LEGAL_NAME_LEN);
        }

        /**
         * Gets the certification number.
         *
         * @return the certification number
         */
        public String getCertificationNumber() {
            if (m_certificationNumber == null) {
                m_certificationNumber = "";
                Collection<StaffCertification> certifications = getStaffCertifications(m_staff);
                if (certifications != null && !certifications.isEmpty()) {
                    PlainDate date = getFLData().getSurveyPeriod().getEndDate();
                    for (StaffCertification item : certifications) {
                        if (!date.before(item.getIssueDate()) &&
                                (item.getExpirationDate() == null || !date.after(item.getExpirationDate()))) {
                            m_certificationNumber = item.getCertificationNumber();
                            if (!StringUtils.isEmpty(m_certificationNumber)) {
                                break;
                            }
                        }
                    }
                }
            }
            return m_certificationNumber;
        }

        /**
         * Gets the ssn.
         *
         * @return the ssn
         */
        public String getSSN() {
            String ssn = m_staff.getPerson().getPersonId();
            if (!StringUtils.isEmpty(ssn)) {
                ssn = ssn.replaceAll("-", "");
            }
            return ssn;
        }
    }

    /**
     * When operating in this mode, the selection will include staff who are scheduled to teach a
     */
    public static final String MODE_SCHEDULED = "MODE_SCHEDULED";
    /**
     * Parameter for use with setStudentSelectionProperties() to indicate if the standard
     * exclude indicators should be applied to any selections (DOE EXCLUDE xxx). This can include
     * STD, CRS, MST.
     * <p>
     * The related property should be a Boolean.
     * <br>
     * If not set, the default will be TRUE.
     */
    public static final String PROPERTY_APPLY_EXCLUDE = "PROPERTY_APPLY_EXCLUDE";

    /**
     * Parameter for use with setStudentSelectionProperties() to indicate if the standard
     * tool input selection criteria and sort should be applied to the student selection.
     * <p>
     * The related property should be a Boolean.
     * <br>
     * If not set, the default will be TRUE.
     */
    public static final String PROPERTY_APPLY_INPUT = "PROPERTY_APPLY_INPUT";

    /**
     * Parameter for use with setStudentSelectionProperties() to indicate if the standard
     * tool input school selection should be applied to the student selection.
     * <p>
     * The related property should be a Boolean.
     * <br>
     * If not set, the default will be TRUE.
     */
    public static final String PROPERTY_APPLY_SCHOOL = "PROPERTY_APPLY_SCHOOL";

    /**
     * Parameter for use with setStudentSelectionProperties() to set the beginning date of the
     * date range for identifying an active student.
     * <p>
     * The related property should be a PlainDate.
     * <br>
     * If not set, the default will be the beginning of the district school year.
     */
    public static final String PROPERTY_BEGIN_DATE = "PROPERTY_BEGIN_DATE";

    /**
     * Parameter for use with setStudentSelectionProperties() to set the ending date of the
     * date range for identifying an active student.
     * <p>
     * The related property should be a PlainDate.
     * <br>
     * If not set, the default will be the current date.
     */
    public static final String PROPERTY_END_DATE = "PROPERTY_END_DATE";

    /**
     * Parameter for use with setStudentSelectionProperties() to indicate that schedule
     * related queries and spans should exclude future terms (terms that begin after end date).
     * <p>
     * The related property should be a Boolean.
     * <br>
     * If not set, the default will be FALSE.
     */
    public static final String PROPERTY_EXCLUDE_FUTURE_SCHEDULES = "PROPERTY_EXCLUDE_FUTURE_SCHEDULES";

    protected static final String ALIAS_CERTIFICATION_STATUS_MTC = "all-mtc-CertificationStatus";
    protected static final String ALIAS_HIGHLY_QUALIFIED_STATUS_MTC = "all-mtc-HighlyQualifiedStatus";
    protected static final String ALIAS_PRIMARY_POSITION_SFP = "all-sfp-PrimaryPosition";
    protected static final String ALIAS_SCHEDULING_METHO_MTC = "all-mtc-SchedulingMethod";
    protected static final String ALIAS_TEAM_TEACHER_TRAINING_MTC = "all-mtc-TeamTeacherTraining";
    protected static final String ALIAS_TITLE_III_FUNDED_MTC = "all-mtc-NCLBTitleIIIFunded";

    /*
     * Standard aliases
     */
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    private static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    protected static final List<String> CERTIFICATION_TYPES = Arrays.asList("Certification");


    private Map<String, Collection<StaffCertification>> m_certificationMap;
    private PlainDate m_currentDate;
    private FLStateReportData m_data;
    private DataDictionaryField m_fieldExcludeCrs;
    private DataDictionaryField m_fieldExcludeMst;
    private DataDictionaryField m_fieldExcludeStf;
    private DataDictionaryField m_fieldPrimaryPositionSfp;
    private DataDictionaryField m_fieldWdisIndicator;

    private Map<String, Collection<StaffPdPlan>> m_pdPlanMap;
    private Map<String, Collection<StaffPosition>> m_positionMap;
    private FLScheduleHelper m_scheduleHelper;
    private X2Criteria m_staffCriteria;
    private Map<String, StaffInfo> m_staffInfoMap = new HashMap();
    private String m_staffSelectionMode;
    private Map<String, Object> m_staffSelectionProperties = new HashMap();
    private QueryByCriteria m_staffQuery;
    private X2Criteria m_teacherScheduleCriteria;
    private Map<String, List<ScheduleTeacher>> m_teacherScheduleMap;
    private boolean m_wdisMode;

    /**
     * Instantiates a new FL staff helper.
     *
     * @param data FLStateReportData
     * @param wdisMode boolean
     */
    public FLStaffHelper(FLStateReportData data, boolean wdisMode) {
        this(data);
        m_wdisMode = wdisMode;
    }

    /**
     * Instantiates a new FL staff helper.
     *
     * @param data the data
     */
    public FLStaffHelper(FLStateReportData data) {
        m_data = data;
        m_fieldExcludeCrs = data.translateAliasToDictionaryField(ALIAS_EXCLUDE_CRS, false);
        m_fieldExcludeMst = data.translateAliasToDictionaryField(ALIAS_EXCLUDE_MST, false);
        m_fieldExcludeStf = data.translateAliasToDictionaryField(ALIAS_EXCLUDE_STF, false);
        m_fieldWdisIndicator = data.translateAliasToDictionaryField(ALIAS_WDIS_AGE_INDICTOR, false);

        m_fieldPrimaryPositionSfp = data.translateAliasToDictionaryField(ALIAS_PRIMARY_POSITION_SFP, true);

        m_currentDate = new PlainDate(OrganizationManager.getTimeZone(getData().getOrganization()));
    }

    /**
     * Gets the primary staff position.
     *
     * @param staff SisStaff
     * @return Staff position
     * @throws X2BaseException exception
     */
    public StaffPosition getPrimaryStaffPosition(SisStaff staff) throws X2BaseException {
        StaffPosition primary = null;
        Collection<StaffPosition> positions = getStaffPositions(staff);
        if (positions != null && !positions.isEmpty()) {
            for (StaffPosition position : positions) {
                Boolean isPrimary = positions.size() == 1 ? Boolean.TRUE
                        : (Boolean) getFLData().getFieldValue(position, m_fieldPrimaryPositionSfp);
                if (isPrimary != null && isPrimary.booleanValue()) {
                    primary = position;
                    break;
                }
            }
        }
        return primary;
    }

    /**
     * Gets the schedule helper.
     *
     * @return the schedule helper
     */
    public FLScheduleHelper getScheduleHelper() {
        if (m_scheduleHelper == null) {
            m_scheduleHelper =
                    new FLScheduleHelper(m_data, m_data.getCurrentContext().getStartDate(),
                            m_data.getSurveyPeriod().getEndDate());
        }
        return m_scheduleHelper;
    }

    /**
     * Gets the staff certifications.
     *
     * @param staff the staff
     * @return the staff certifications
     */
    public Collection<StaffCertification> getStaffCertifications(SisStaff staff) {
        if (m_certificationMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StaffCertification.COL_TYPE, CERTIFICATION_TYPES);
            criteria.addIn(StaffCertification.COL_STAFF_OID,
                    new SubQuery(SisStaff.class, X2BaseBean.COL_OID, getStaffCriteria()));
            m_certificationMap = getFLData().getBroker().getGroupedCollectionByQuery(
                    new QueryByCriteria(StaffCertification.class, criteria),
                    StaffCertification.COL_STAFF_OID, 500);
        }
        return m_certificationMap.get(staff.getOid());
    }

    /**
     * Gets the staff criteria.
     *
     * @return the staff criteria
     */
    public X2Criteria getStaffCriteria() {
        if (m_staffCriteria == null) {
            m_staffCriteria = buildStaffCriteria();
        }
        return m_staffCriteria;
    }

    /**
     * Gets the staff info.
     *
     * @param staff the staff
     * @return the staff info
     */
    public StaffInfo getStaffInfo(SisStaff staff) {
        StaffInfo staffInfo = m_staffInfoMap.get(staff.getOid());
        if (staffInfo == null) {
            staffInfo = new StaffInfo(staff);
            m_staffInfoMap.put(staff.getOid(), staffInfo);
        }
        return staffInfo;
    }

    /**
     * Find and cache staff professional development plan records.
     *
     * @param staff SisStaff
     * @return collection of staff professional development plan records
     */
    public Collection<StaffPdPlan> getStaffPdPlans(SisStaff staff) {
        if (m_pdPlanMap == null) {

            PlainDate start = getData().getCurrentContext().getStartDate();

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StaffPdPlan.COL_STAFF_OID,
                    new SubQuery(SisStaff.class, X2BaseBean.COL_OID, getStaffCriteria()));
            criteria.addGreaterOrEqualThan(StaffPdPlan.COL_RENEWAL_DATE, start);

            QueryByCriteria query = new QueryByCriteria(StaffPdPlan.class, criteria);
            query.addOrderByAscending(StaffPdPlan.COL_STAFF_OID);
            query.addOrderByAscending(StaffPdPlan.COL_RENEWAL_DATE);

            m_pdPlanMap = getFLData().getBroker().getGroupedCollectionByQuery(
                    query, StaffPosition.COL_STAFF_OID, 500);
        }
        return m_pdPlanMap.get(staff.getOid());
    }

    /**
     * Find and cache staff position records.
     *
     * @param staff SisStaff
     * @return collection of staff position records
     */
    public Collection<StaffPosition> getStaffPositions(SisStaff staff) {
        if (m_positionMap == null) {
            X2Criteria criteria = new X2Criteria();

            X2Criteria orCriteria = new X2Criteria();
            X2Criteria startCriteria = new X2Criteria();
            X2Criteria startCriteriaNull = new X2Criteria();
            X2Criteria endCriteria = new X2Criteria();
            X2Criteria endCriteriaNull = new X2Criteria();
            startCriteria.addLessOrEqualThan(StaffPosition.COL_START_DATE,
                    getFLData().getSurveyPeriod().getStartDate());
            startCriteria.addGreaterOrEqualThan(StaffPosition.COL_END_DATE,
                    getFLData().getSurveyPeriod().getStartDate());

            startCriteriaNull.addLessOrEqualThan(StaffPosition.COL_START_DATE,
                    getFLData().getSurveyPeriod().getStartDate());
            startCriteriaNull.addIsNull(StaffPosition.COL_END_DATE);

            endCriteria.addLessOrEqualThan(StaffPosition.COL_START_DATE, getFLData().getSurveyPeriod().getEndDate());
            endCriteria.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, getFLData().getSurveyPeriod().getEndDate());

            endCriteriaNull.addLessOrEqualThan(StaffPosition.COL_START_DATE,
                    getFLData().getSurveyPeriod().getEndDate());
            endCriteriaNull.addIsNull(StaffPosition.COL_END_DATE);

            orCriteria.addOrCriteria(startCriteria);
            orCriteria.addOrCriteria(startCriteriaNull);
            orCriteria.addOrCriteria(endCriteria);
            orCriteria.addOrCriteria(endCriteriaNull);

            criteria.addAndCriteria(orCriteria);
            criteria.addIn(StaffPosition.COL_STAFF_OID,
                    new SubQuery(SisStaff.class, X2BaseBean.COL_OID, getStaffCriteria()));

            QueryByCriteria query = new QueryByCriteria(StaffPosition.class, criteria);
            query.addOrderByAscending(StaffPosition.COL_STAFF_OID);
            query.addOrderByAscending(StaffPosition.COL_START_DATE);
            // ensure repeatable sort order
            query.addOrderByAscending(X2BaseBean.COL_OID);

            m_positionMap = getFLData().getBroker().getGroupedCollectionByQuery(
                    query, StaffPosition.COL_STAFF_OID, 500);
        }
        return m_positionMap.get(staff.getOid());
    }

    /**
     * Returns the current selection mode.
     *
     * @return String
     */
    public String getStaffSelectionMode() {
        return m_staffSelectionMode;
    }

    /**
     * Return the current staff query. If it has not been defined, create it.
     * <br>
     * The distinct flag indicates the query should apply the distinct option.
     * This should only be used if external criteria has been added that will
     * cause duplicate records from the query.
     * <br>
     * The query returned is ready to use in the export.
     *
     * @param distinct - Apply distinct to the query.
     *
     * @return QueryByCriteria
     */
    public QueryByCriteria getStaffQuery(boolean distinct) {
        if (m_staffQuery == null) {
            m_staffQuery = buildStaffQuery(distinct);
        }
        return m_staffQuery;
    }

    /**
     * Gets the teacher schedule criteria.
     *
     * @return the teacher schedule criteria
     */
    public X2Criteria getTeacherScheduleCriteria() {
        if (m_teacherScheduleCriteria == null) {
            m_teacherScheduleCriteria = buildTeacherScheduleCriteria();
        }
        return m_teacherScheduleCriteria;
    }

    /**
     * Return a list of teacher schedules for the requested teacher.
     *
     * @param teacherOid the teacher oid
     * @return List<StudentSchedule>
     */
    public List<ScheduleTeacher> getTeacherSchedules(String teacherOid) {
        if (m_teacherScheduleMap == null) {
            QueryByCriteria scheduleQuery;
            X2Criteria scheduleCriteria = getTeacherScheduleCriteria();
            scheduleQuery = new QueryByCriteria(ScheduleTeacher.class, scheduleCriteria);
            scheduleQuery.addOrderBy(ScheduleTeacher.COL_STAFF_OID, true);
            scheduleQuery.addOrderBy(ScheduleTeacher.COL_SECTION_OID, true);
            m_teacherScheduleMap = getData().getBroker().getGroupedCollectionByQuery(scheduleQuery,
                    ScheduleTeacher.COL_STAFF_OID, 500);
        }

        return m_teacherScheduleMap.get(teacherOid);
    }

    /**
     * <p>
     * Set a property value that is used to control the behavior of the helper.
     * The key should be one of the PROPERTY_* constants in this class. The property
     * should be of the appropriate type specified for the key.
     *
     * @param key the key
     * @param property the property
     */
    public void setSelectionProperty(String key, Object property) {
        m_staffSelectionProperties.put(key, property);
    }

    /**
     * Sets the student selection mode. This mode should be one of the MODE_* constants in this
     * class.
     * This value should not be changed once the selection process has begun.
     *
     * @param mode the new staff selection mode
     */
    public void setStaffSelectionMode(String mode) {
        m_staffSelectionMode = mode;
    }

    /**
     * Gets the FL data.
     *
     * @return the FL data
     */
    protected FLStateReportData getFLData() {
        return m_data;
    }

    /**
     * Builds the staff criteria.
     *
     * @return the x 2 criteria
     */
    private X2Criteria buildStaffCriteria() {
        Boolean applyExclude = (Boolean) getStaffSelectionProperty(PROPERTY_APPLY_EXCLUDE, Boolean.class, Boolean.TRUE);
        Boolean applyInput = (Boolean) getStaffSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);

        X2Criteria criteria = new X2Criteria();
        if (applyInput.booleanValue()) {
            getData().applyInputCriteria(criteria, false, null);
        }
        if (applyExclude.booleanValue() && m_fieldExcludeStf != null) {
            criteria.addNotEqualTo(m_fieldExcludeStf.getJavaName(), BooleanAsStringConverter.TRUE);
        }
        if (MODE_SCHEDULED.equals(getStaffSelectionMode())) {
            X2Criteria scheduleCriteria = getTeacherScheduleCriteria();
            SubQuery scheduleSubquery =
                    new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_STAFF_OID, scheduleCriteria);
            criteria.addIn(X2BaseBean.COL_OID, scheduleSubquery);
        } else {
            String activeCode = PreferenceManager.getPreferenceValue(getFLData().getOrganization(), STAFF_ACTIVE_CODE);
            criteria.addEqualTo(Staff.COL_STATUS, activeCode);
        }
        return criteria;
    }

    /**
     * Builds the staff query.
     *
     * @param distinct the distinct
     * @return the query by criteria
     */
    private QueryByCriteria buildStaffQuery(boolean distinct) {
        BeanQuery query = new BeanQuery(SisStaff.class, getStaffCriteria());

        getData().applyInputSort(query, null);

        // If the calling process requests the query to return a distinct value (due to modified
        // criteria),
        // apply distinct and a distinct adjuster to the query.
        if (distinct) {
            query.setDistinct(distinct);
            DataDictionaryTable table =
                    getData().getDataDictionary().findDataDictionaryTableByClass(getData().getBeanClass().getName());
            DistinctAdjuster adjuster =
                    new DistinctAdjuster(table.getPrimaryKeyColumn(), getData().getBroker().getPersistenceKey());
            query.addQueryAdjuster(adjuster);
        }
        return query;
    }

    /**
     * Builds the teacher schedule criteria.
     *
     * @return the x 2 criteria
     */
    private X2Criteria buildTeacherScheduleCriteria() {
        PlainDate endDate = (PlainDate) getStaffSelectionProperty(PROPERTY_END_DATE, PlainDate.class,
                getData().getCurrentContext().getEndDate().before(m_currentDate)
                        ? getData().getCurrentContext().getEndDate()
                        : m_currentDate);
        Boolean applyExclude = (Boolean) getStaffSelectionProperty(PROPERTY_APPLY_EXCLUDE, Boolean.class, Boolean.TRUE);
        Boolean excludeFutureSched =
                (Boolean) getStaffSelectionProperty(PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.class, Boolean.FALSE);
        Boolean applyInput = (Boolean) getStaffSelectionProperty(PROPERTY_APPLY_INPUT, Boolean.class, Boolean.TRUE);
        Boolean applySchool = (Boolean) getStaffSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.class, Boolean.TRUE);
        X2Criteria teacherScheduleCriteria = new X2Criteria();

        // Master type Class
        teacherScheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        // From active Schedule for the selected year.
        teacherScheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                getData().getCurrentContext().getOid());

        // Require section term to start before end/report date.
        if (excludeFutureSched.booleanValue()) {
            teacherScheduleCriteria.addLessOrEqualThan(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE_TERM_DATES + PATH_DELIMITER +
                    ScheduleTermDate.COL_START_DATE, endDate);
        }

        // check school or organization selection.
        if (applySchool.booleanValue() && getData().isSchoolContext()) {
            teacherScheduleCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getData().getSchool().getOid());
        } else {
            teacherScheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            teacherScheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check exclusion flags for student, section and student schedule.
        if (applyExclude.booleanValue() && m_fieldExcludeStf != null) {
            teacherScheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER +
                    m_fieldExcludeStf.getJavaName(),
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the section exclusion custom field is present.
        if (applyExclude.booleanValue() && m_fieldExcludeMst != null) {
            teacherScheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    m_fieldExcludeMst.getJavaName(),
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (applyExclude.booleanValue() && m_fieldExcludeCrs != null) {
            teacherScheduleCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExcludeCrs.getJavaName(),
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the user criteria needs to be applied.
        if (applyInput.booleanValue()) {
            getData().applyInputCriteria(teacherScheduleCriteria, false, ScheduleTeacher.REL_STAFF);
        }

        // Apply WDIS mode criteria.
        if (m_fieldWdisIndicator != null) {
            if (m_wdisMode) {
                teacherScheduleCriteria.addIn(
                        ScheduleTeacher.REL_SECTION + PATH_DELIMITER + m_fieldWdisIndicator.getJavaName(),
                        Arrays.asList(WDIS_MODE_ADULT_GENERAL_EDUCATION, WDIS_MODE_POST_SECONDARY_CTE));
            }
        }

        return teacherScheduleCriteria;
    }

    /**
     * Gets the data.
     *
     * @return the data
     */
    private FLStateReportData getData() {
        // TODO Auto-generated method stub
        return m_data;
    }

    /**
     * Return the user value entered for a parameter. Verify that the value matches a specified
     * type.
     *
     * @param selectKey The key of the property to retrieve.
     * @param expectedClass The class type expected for the value.
     * @param defaultValue The value to return if the property is not present or is null.
     *
     * @return Object
     */
    private Object getStaffSelectionProperty(String selectKey, Class expectedClass, Object defaultValue) {
        Object value = m_staffSelectionProperties.get(selectKey);
        if (value != null) {
            if (!expectedClass.isInstance(value)) {
                throw new ClassCastException("getStudentSeletionProperty(" + selectKey + "): Expected "
                        + expectedClass.getName() + ", found " + value.getClass().getName());
            }
        } else {
            value = defaultValue;
        }
        return value;
    }

}
