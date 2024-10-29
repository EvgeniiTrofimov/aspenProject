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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.StateReportModel;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.jdom.Element;

/**
 * State report for UK's School Workforce.
 *
 * @author Follett Software Company
 */
public class SchoolWorkforce extends XMLStateReportData {
    public static final String ALIAS_DFE_CONTRACT_TYPE = "DFE CONTRACT TYPE";
    public static final String ALIAS_DFE_CONTRACT_START_DATE = "DFE CONTRACT START";
    public static final String ALIAS_DFE_CONTRACT_END_DATE = "DFE CONTRACT END";
    public static final String ALIAS_DFE_CONTRACT_POST = "DFE POST";
    public static final String ALIAS_DFE_CONTRACT_DAILY_RATE = "DFE DAILY RATE";
    public static final String ALIAS_DFE_STAFF_VACANCY_INDICATOR = "DFE VACANCY INDICATOR";
    public static final String ALIAS_DFE_COURSE_GENERAL_SUBJECT = "DFE GENERAL SUBJECT";
    public static final String ALIAS_DFE_SECTION_CLASS_YEAR_GROUP = "DFE CLASS YEAR GROUP";
    public static final String ALIAS_DFE_STAFF_CENSUS_HIDE = "DFE STAFF CENSUS HIDE";
    public static final String PARAM_CURRENT_BEAN = "currentBean";

    private static final String MODEL_PARAM_CENSUS_DATE = "censusDate";
    private static final String MODEL_PARAM_ABSENCE_END_DATE = "absenceEndDate";
    private static final String MODEL_PARAM_ABSENCE_START_DATE = "absenceStartDate";
    private static final String MODEL_PARAM_CONTRACT_END_DATE = "contractEndDate";
    private static final String MODEL_PARAM_CONTRACT_START_DATE = "contractStartDate";
    private static final String MODEL_PARAM_TERM_END_DATE = "termEndDate";
    private static final String MODEL_PARAM_TERM_START_DATE = "termStartDate";
    private static final String MODEL_REPORT = "report";
    private static final String MODEL_REPORT_SCHOOL_WORKFORCE = "SchoolWorkforce";
    private static final String MODEL_RETRIEVER_STAFF_ABSENCES = "STAFFABSENCES";
    private static final String MODEL_RETRIEVER_STAFF_CONTRACT = "STAFFCONTRACT";
    private static final String MODEL_RETRIEVER_STAFF_STATUS = "STATUS";
    private static final String MODEL_RETRIEVER_SYSTEM = "SYSTEM";

    private static final String ELEMENT_REFERENCE_DATE = "ReferenceDate";
    private static final String ELEMENT_YEAR = "Year";

    protected final SimpleDateFormat m_dateFormatter = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * The UK model
     */
    private PlainDate m_absenceSelectionEndDate;
    private PlainDate m_absenceSelectionStartDate;
    private Map<String, Collection<StaffPosition>> m_contractsMap = new HashMap<String, Collection<StaffPosition>>();
    private PlainDate m_contractSelectionEndDate;
    private PlainDate m_contractSelectionStartDate;
    private StateReportModel m_model;
    private PlainDate m_termSelectionEndDate;
    private PlainDate m_termSelectionStartDate;

    /**
     * Post process.
     *
     * @return Collection
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData.postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        removeEmptyElements();

        return new ArrayList<StateReportValidationError>();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData.initialize() throws
     *      X2BaseException
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_model = loadModel("CBDS");
        m_model.set(MODEL_REPORT, MODEL_REPORT_SCHOOL_WORKFORCE);

        // Loads and sets up census date, contract, term, absence Dates
        int currentSchoolYear = getCurrentContext().getSchoolYear();
        m_model.set("schoolYear", Integer.valueOf(currentSchoolYear));
        m_model.execute("loadDates");

        // UK school years are one year ahead
        Element yearElement = new Element(ELEMENT_YEAR).setText(Integer.toString(currentSchoolYear - 1));
        setElement(ELEMENT_YEAR, yearElement);
        PlainDate censusDate = (PlainDate) m_model.get(MODEL_PARAM_CENSUS_DATE);
        Element censusDateElement = new Element(ELEMENT_REFERENCE_DATE).setText(censusDate.toString());
        setElement(ELEMENT_REFERENCE_DATE, censusDateElement);

        m_contractSelectionStartDate = (PlainDate) m_model.get(MODEL_PARAM_CONTRACT_START_DATE);
        m_contractSelectionEndDate = (PlainDate) m_model.get(MODEL_PARAM_CONTRACT_END_DATE);
        m_absenceSelectionStartDate = (PlainDate) m_model.get(MODEL_PARAM_ABSENCE_START_DATE);
        m_absenceSelectionEndDate = (PlainDate) m_model.get(MODEL_PARAM_ABSENCE_END_DATE);
        m_termSelectionStartDate = (PlainDate) m_model.get(MODEL_PARAM_TERM_START_DATE);
        m_termSelectionEndDate = (PlainDate) m_model.get(MODEL_PARAM_TERM_END_DATE);

        // School
        loadSchool(getSchool());

        // Staff & Staff Roles
        X2Criteria rolesCriteria = getRolesCriteria();
        if (!rolesCriteria.isEmpty()) {
            SubQuery sfpStfSubQuery = new SubQuery(StaffPosition.class, StaffPosition.COL_STAFF_OID, rolesCriteria);

            X2Criteria staffCriteria = getStaffCriteria(sfpStfSubQuery);
            SubQuery staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);

            boolean hasStaff = loadWorkforceMembers(staffCriteria);

            if (hasStaff) {
                loadWorkforceMemberContractsAndRoles(rolesCriteria);
                loadWorkforceMemberAdditionalPayments(staffSubQuery);

                loadWorkforceMemberAbsences(staffSubQuery);
                loadWorkforceMemberCurriculums(staffSubQuery);
                loadWorkforceMemberQualifications(staffSubQuery);

                loadSchoolVacancies(getSchool());

                if (getSetupErrors().isEmpty()) {
                    // Organization level calculated fields
                    m_model.initializeRetriever(MODEL_RETRIEVER_SYSTEM);

                    // Staff level calculated fields
                    m_model.initializeRetriever(MODEL_RETRIEVER_STAFF_STATUS);
                    m_model.initializeRetriever(MODEL_RETRIEVER_STAFF_CONTRACT, m_contractsMap);
                    m_model.initializeRetriever(MODEL_RETRIEVER_STAFF_ABSENCES, staffSubQuery);

                    super.addCalcs(m_model.getRetrievers());
                }
            }
        }
    }

    /**
     * Check if two dates are consecutive.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return boolean
     */
    private boolean areConsecutiveDays(PlainDate startDate, PlainDate endDate) {
        boolean areConsecutive = false;
        if (startDate != null && endDate != null) {
            PlainDate newDate = DateUtils.add(startDate, +1);
            if (newDate.equals(endDate)) {
                areConsecutive = true;
            }
        }
        return areConsecutive;
    }

    /**
     * Convert a Object date to a String Date.
     *
     * @param dateObj Object
     * @return String
     */
    private String convertObjectToStringDate(Object dateObj) {
        String dateStr = null;

        if (dateObj instanceof String) {
            dateStr = (String) dateObj;
        } else if (dateObj instanceof PlainDate) {
            dateStr = m_dateFormatter.format((PlainDate) dateObj);
        }

        return dateStr;
    }

    /**
     * Get Staff Attendance Key by staffOid and reasonCode.
     *
     * @param staffAttendance StaffAttendance
     * @return String
     */
    private String getAttendanceKey(StaffAttendance staffAttendance) {
        String staffOid = staffAttendance.getStaffOid();
        String reasonCode = staffAttendance.getReason();

        StringBuffer attendanceKey = new StringBuffer();
        attendanceKey.append(staffOid);
        if (reasonCode != null) {
            attendanceKey.append(reasonCode);
        }

        return attendanceKey.toString();
    }

    /**
     * Get a StaffPostion's 'ContractKey'. this is a composite key consisting of
     * the staffOid, startDate, startEnd, dailyRate & post fields.
     * This is used to identify unique Contract records to roll-up Roles to.
     *
     * @param staffPosition StaffPosition
     * @return String
     */
    private String getContractKey(StaffPosition staffPosition) {
        String staffOid = staffPosition.getStaffOid();
        String contractType = (String) staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_TYPE);
        String startDate = convertObjectToStringDate(staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_START_DATE));
        String startEnd = convertObjectToStringDate(staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_END_DATE));
        String dailyRate = (String) staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_DAILY_RATE);
        String post = (String) staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_POST);

        StringBuilder contractKey = new StringBuilder();
        contractKey.append(staffOid);
        if (contractType != null) {
            contractKey.append(contractType);
        }
        if (startDate != null) {
            contractKey.append(startDate);
        }
        if (startEnd != null) {
            contractKey.append(startEnd);
        }
        if (dailyRate != null) {
            contractKey.append(dailyRate);
        }
        if (post != null) {
            contractKey.append(post);
        }

        return contractKey.toString();
    }

    /**
     * Get a ScheduleTeacher's 'CurriculumKey' a composite key consisting of
     * the StaffOid, the Course's generalSubject field and the Section's classYearGroup field
     * This is used to identify unique Curriculum records.
     *
     * @param scheduleTeacher ScheduleTeacher
     * @return String
     */
    private String getCurriculumKey(ScheduleTeacher scheduleTeacher) {
        String staffOid = scheduleTeacher.getStaffOid();
        Course course = scheduleTeacher.getSection().getSchoolCourse().getCourse();
        String generalSubject = (String) course.getFieldValueByAlias(ALIAS_DFE_COURSE_GENERAL_SUBJECT);
        String classYearGroup =
                (String) scheduleTeacher.getSection().getFieldValueByAlias(ALIAS_DFE_SECTION_CLASS_YEAR_GROUP);

        StringBuffer curriculumKey = new StringBuffer();
        curriculumKey.append(staffOid);
        if (generalSubject != null) {
            curriculumKey.append(generalSubject);
        }
        if (classYearGroup != null) {
            curriculumKey.append(classYearGroup);
        }

        return curriculumKey.toString();
    }

    /**
     * Sets up the criteria for Workforce Member roles and then return the criteria.
     *
     * @return X2Criteria - roles criteria
     */
    private X2Criteria getRolesCriteria() {
        X2Criteria staffPositionCriteria = new X2Criteria();
        staffPositionCriteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, getSchool().getOid());

        String contractEndDateJavaName = translateAliasToJavaName(ALIAS_DFE_CONTRACT_END_DATE, true);

        /*
         * Lookup the field, determine if it is a date datetype or a string datatype.
         * If it is a string datatype, we get a converter object.
         */
        DataDictionaryField contractEndDateField = getDataDictionaryField(StaffPosition.class, contractEndDateJavaName);
        SystemStringConverter stringConverter = null;
        if (contractEndDateField.isString()) {
            Converter baseConverter = ConverterFactory.getConverterForClass(contractEndDateField.getEffectiveJavaType(),
                    Locale.getDefault(), contractEndDateField.isString());
            if (baseConverter instanceof SystemStringConverter) {
                stringConverter = ((SystemStringConverter) baseConverter);
            }
        }

        /*
         * Prepare the date to be compared in the query criteria.
         * Set up an Object type for the value. Initialize with PlainDate.
         */
        Object contractStartDateCompareObject = m_contractSelectionStartDate;
        if (stringConverter != null) {
            contractStartDateCompareObject = stringConverter.getSystemString(m_contractSelectionStartDate);
        }

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(contractEndDateJavaName, contractStartDateCompareObject);

        X2Criteria endDateNullCriteria = new X2Criteria();
        endDateNullCriteria.addIsNull(contractEndDateJavaName);
        endDateCriteria.addOrCriteria(endDateNullCriteria);
        staffPositionCriteria.addAndCriteria(endDateCriteria);

        BeanQuery staffPositionQuery = new BeanQuery(StaffPosition.class, staffPositionCriteria);
        Collection<StaffPosition> staffPositionCol = getBroker().getCollectionByQuery(staffPositionQuery);

        // Get a list of Staff Oid with contracts open as of report Date
        ArrayList hasOpenContractStaffOids = new ArrayList();
        for (StaffPosition staffPosition : staffPositionCol) {
            String staffPositionEndDateStr =
                    convertObjectToStringDate(staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_END_DATE));

            if (staffPositionEndDateStr == null) {
                // Keep on-going contracts
                hasOpenContractStaffOids.add(staffPosition.getStaffOid());
            } else {
                PlainDate staffPositionEndDate = DateUtils.getDate(staffPositionEndDateStr);
                if (staffPositionEndDate.after(m_contractSelectionEndDate)) {
                    // Keep contracts that close in the future
                    hasOpenContractStaffOids.add(staffPosition.getStaffOid());
                }
            }
        }

        ArrayList includeStaffPositionOids = new ArrayList();
        for (StaffPosition staffPosition : staffPositionCol) {
            String staffPositionStartDateStr =
                    convertObjectToStringDate(staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_START_DATE));
            String staffPositionEndDateStr =
                    convertObjectToStringDate(staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_END_DATE));

            // Include All Open Contracts
            if (staffPositionStartDateStr == null || staffPositionEndDateStr == null) {
                // Keep Open ended Contracts
                includeStaffPositionOids.add(staffPosition.getOid());
            } else {
                PlainDate staffPositionStartDate = DateUtils.getDate(staffPositionStartDateStr);
                PlainDate staffPositionEndDate = DateUtils.getDate(staffPositionEndDateStr);
                PlainDate staffPositionStartPlus28DaysDate = DateUtils.add(staffPositionStartDate, 28);

                // Only include Contracts that are longer than or equal to 28 days
                if (staffPositionEndDate.equals(staffPositionStartPlus28DaysDate)
                        || staffPositionEndDate.after(staffPositionStartPlus28DaysDate)) {
                    if (staffPositionEndDate.after(m_contractSelectionEndDate)) {
                        // Keep Contracts that close in the future
                        includeStaffPositionOids.add(staffPosition.getOid());
                    } else if (!hasOpenContractStaffOids.contains(staffPosition.getStaffOid())) {
                        // If the Staff doesn't have an open contract then include any closed
                        // Contracts from the past school year
                        includeStaffPositionOids.add(staffPosition.getOid());
                    }
                }
            }
        }

        X2Criteria rolesCriteria = new X2Criteria();
        rolesCriteria.addIn(X2BaseBean.COL_OID, includeStaffPositionOids);

        return rolesCriteria;
    }

    /**
     * Sets up the criteria for Workforce Members and then return the criteria.
     *
     * @param sfpStfSubQuery SubQuery
     * @return X2Criteria
     */
    private X2Criteria getStaffCriteria(SubQuery sfpStfSubQuery) {
        X2Criteria staffCriteria = new X2Criteria();

        String staffCensusHideAlias = translateAliasToJavaName(ALIAS_DFE_STAFF_CENSUS_HIDE, true);
        staffCriteria.addNotEqualTo(staffCensusHideAlias, "1");

        X2BaseBean bean = (X2BaseBean) getParameter(PARAM_CURRENT_BEAN);
        if (bean != null && bean instanceof SisStaff) {
            staffCriteria.addEqualTo(X2BaseBean.COL_OID, bean.getOid());
        } else {
            staffCriteria.addIn(X2BaseBean.COL_OID, sfpStfSubQuery);
            staffCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, ((SisSchool) getSchool()).getOid());
        }

        return staffCriteria;
    }

    /**
     * Loads the school.
     *
     * @param school School
     */
    private void loadSchool(School school) {
        setBeanToQuery("school", school);
    }

    /**
     * Load the School's Workforce Member Vacancies.
     *
     * @param school School
     */
    private void loadSchoolVacancies(School school) {
        String vacancyIndAlias = translateAliasToJavaName(ALIAS_DFE_STAFF_VACANCY_INDICATOR, true);

        X2Criteria staffCriteria = new X2Criteria();
        staffCriteria.addEqualTo(vacancyIndAlias, "1");
        staffCriteria.addEqualTo(Staff.COL_SCHOOL_OID, school.getOid());

        BeanQuery staffQuery = new BeanQuery(Staff.class, staffCriteria);
        Collection vacantStaffCol = getBroker().getCollectionByQuery(staffQuery);

        setCollectionToQuery("vacancies", vacantStaffCol);
    }

    /**
     * Load up the Workforce Member's absences (StaffAttendances).
     *
     * @param staffSubQuery SubQuery
     */
    private void loadWorkforceMemberAbsences(SubQuery staffSubQuery) {
        X2Criteria attendanceCriteria = new X2Criteria();
        attendanceCriteria.addIn(StaffAttendance.COL_STAFF_OID, staffSubQuery);
        attendanceCriteria.addGreaterOrEqualThan(StaffAttendance.COL_DATE, m_absenceSelectionStartDate);
        attendanceCriteria.addLessOrEqualThan(StaffAttendance.COL_DATE, m_absenceSelectionEndDate);
        attendanceCriteria.addEqualTo(StaffAttendance.COL_CODE, "A");

        BeanQuery attendanceQuery = new BeanQuery(StaffAttendance.class, attendanceCriteria);
        attendanceQuery.addOrderByAscending(StaffAttendance.COL_STAFF_OID);
        attendanceQuery.addOrderByAscending(StaffAttendance.COL_REASON);
        attendanceQuery.addOrderByAscending(StaffAttendance.COL_DATE);

        Collection<StaffAttendance> attendanceList = getBroker().getCollectionByQuery(attendanceQuery);

        // Get Staff Attendance
        PlainDate currDate = null;
        PlainDate prevDate = null;
        String currAttendanceKey = null;
        String prevAttendanceKey = null;
        StaffAttendance prevStaffAttendance = null;
        ArrayList<StaffAttendance> allStaffAttendances = new ArrayList();

        for (StaffAttendance staffAttendance : attendanceList) {
            currDate = staffAttendance.getDate();
            currAttendanceKey = getAttendanceKey(staffAttendance);

            if (staffAttendance.getReason() == null) {
                staffAttendance.setReason("OTH");
            }

            if (prevDate != null) {
                boolean areConsecutiveDays = areConsecutiveDays(prevDate, currDate);

                if (!(currAttendanceKey.equals(prevAttendanceKey) && areConsecutiveDays)) {
                    allStaffAttendances.add(prevStaffAttendance);
                }
            }

            prevDate = currDate;
            prevAttendanceKey = currAttendanceKey;
            prevStaffAttendance = staffAttendance;
        }

        if (prevStaffAttendance != null) {
            allStaffAttendances.add(prevStaffAttendance);
        }

        // Load map of StaffAttendance records by StaffOid
        Map absenceMap = new HashMap();
        ArrayList<StaffAttendance> individualStaffAttendances = new ArrayList();
        String prevStaffOid = null;
        String currStaffOid = null;

        for (StaffAttendance staffAttendance : allStaffAttendances) {
            currStaffOid = staffAttendance.getStaffOid();

            if (prevStaffOid != null) {
                individualStaffAttendances.add(prevStaffAttendance);

                if (!currStaffOid.equals(prevStaffOid)) {
                    absenceMap.put(prevStaffOid, individualStaffAttendances);

                    individualStaffAttendances = new ArrayList();
                }
            }

            prevStaffOid = currStaffOid;
            prevStaffAttendance = staffAttendance;
        }

        if (prevStaffOid != null) {
            individualStaffAttendances.add(prevStaffAttendance);

            absenceMap.put(prevStaffOid, individualStaffAttendances);
        }

        setGroupedCollectionToQuery("absences", absenceMap);


        // Get Staff Leaves
        X2Criteria leaveCriteria = new X2Criteria();
        leaveCriteria.addIn(StaffLeave.COL_STAFF_OID, staffSubQuery);
        leaveCriteria.addGreaterOrEqualThan(StaffLeave.COL_START_DATE, m_absenceSelectionStartDate);
        leaveCriteria.addLessOrEqualThan(StaffLeave.COL_END_DATE, m_absenceSelectionEndDate);

        BeanQuery leaveQuery = new BeanQuery(StaffLeave.class, leaveCriteria);
        leaveQuery.addOrderByAscending(StaffLeave.COL_STAFF_OID);
        leaveQuery.addOrderByAscending(StaffLeave.COL_REASON_CODE);
        leaveQuery.addOrderByAscending(StaffLeave.COL_START_DATE);

        Map leaveMap = getBroker().getGroupedCollectionByQuery(leaveQuery, StaffLeave.COL_STAFF_OID, 64);

        setGroupedCollectionToQuery("leaves", leaveMap);
    }

    /**
     * Load the Workforce Member's Additional Payments (StaffSupport).
     *
     * @param staffSubQuery SubQuery
     */
    private void loadWorkforceMemberAdditionalPayments(SubQuery staffSubQuery) {
        X2Criteria staffSupportCriteria = new X2Criteria();
        staffSupportCriteria.addIn(StaffSupport.REL_STAFF_POSITION + "." + StaffPosition.COL_STAFF_OID, staffSubQuery);

        BeanQuery staffSupportQuery = new BeanQuery(StaffSupport.class, staffSupportCriteria);
        Map staffSupportMap =
                getBroker().getGroupedCollectionByQuery(staffSupportQuery, StaffSupport.COL_STAFF_POSITION_OID, 64);

        setGroupedCollectionToQuery("additionalPayments", staffSupportMap);
    }

    /**
     * Load the Workforce Member's Contracts and Contract Rolls .
     *
     * @param rolesCriteria X2Criteria
     */
    private void loadWorkforceMemberContractsAndRoles(X2Criteria rolesCriteria) {
        String contractTypeJavaName = translateAliasToJavaName(ALIAS_DFE_CONTRACT_TYPE, true);
        String contractStartDateJavaName = translateAliasToJavaName(ALIAS_DFE_CONTRACT_START_DATE, true);
        String contractEndDateJavaName = translateAliasToJavaName(ALIAS_DFE_CONTRACT_END_DATE, true);
        String contractDailyRateJavaName = translateAliasToJavaName(ALIAS_DFE_CONTRACT_DAILY_RATE, true);
        String contractPostJavaName = translateAliasToJavaName(ALIAS_DFE_CONTRACT_POST, true);

        BeanQuery rolesQuery = new BeanQuery(StaffPosition.class, rolesCriteria);
        rolesQuery.addOrderByAscending(StaffPosition.COL_STAFF_OID);
        rolesQuery.addOrderByAscending(contractTypeJavaName);
        rolesQuery.addOrderByAscending(contractStartDateJavaName);
        rolesQuery.addOrderByAscending(contractEndDateJavaName);
        rolesQuery.addOrderByAscending(contractDailyRateJavaName);
        rolesQuery.addOrderByAscending(contractPostJavaName);

        Collection<StaffPosition> rolesCol = getBroker().getCollectionByQuery(rolesQuery);

        /*
         * - Get a unique single instant of the ContractKey by StaffOid
         * - Create an array list of unique Contract records form StaffPositions.
         * -Create an map of collections of StaffPosition's by the contractKey field
         */
        ArrayList<StaffPosition> contractCol = new ArrayList();
        Map<String, String> contractKeyMap = new HashMap<String, String>();

        String prevStaffOid = "";
        String currStaffOid = "";
        String prevContractKey = "";
        String currContractKey = "";

        StaffPosition prevStaffPosition = null;
        for (StaffPosition staffPosition : rolesCol) {
            currStaffOid = staffPosition.getStaffOid();
            if (currStaffOid != null) {
                currContractKey = getContractKey(staffPosition);

                if (prevStaffPosition != null) {
                    if (!currContractKey.equals(prevContractKey)) {
                        // Don't include if neither of the CurriculumKey fields are populated.
                        if (!prevContractKey.equals(prevStaffOid)) {
                            contractCol.add(prevStaffPosition);
                            contractKeyMap.put(prevContractKey, prevStaffPosition.getOid());
                        }
                    }
                }

                prevStaffOid = currStaffOid;
                prevContractKey = currContractKey;
                prevStaffPosition = staffPosition;
            }
        }

        if (prevStaffPosition != null) {
            contractCol.add(prevStaffPosition);
            contractKeyMap.put(prevContractKey, prevStaffPosition.getOid());
        }

        // Create Map of Collections of Staff Contracts by StaffOid
        m_contractsMap = new HashMap<String, Collection<StaffPosition>>();
        ArrayList<StaffPosition> staffContracts = new ArrayList();
        prevStaffPosition = null;
        prevStaffOid = null;
        currStaffOid = null;

        for (StaffPosition staffPosition : contractCol) {
            currStaffOid = staffPosition.getStaffOid();

            if (prevStaffPosition != null) {
                staffContracts.add(prevStaffPosition);

                if (!currStaffOid.equals(prevStaffOid)) {
                    m_contractsMap.put(prevStaffOid, staffContracts);

                    staffContracts = new ArrayList();
                }
            }

            prevStaffOid = currStaffOid;
            prevStaffPosition = staffPosition;
        }

        if (prevStaffPosition != null) {
            staffContracts.add(prevStaffPosition);

            m_contractsMap.put(prevStaffOid, staffContracts);
        }

        setGroupedCollectionToQuery("contracts", m_contractsMap);


        /*
         * Create a Map of a Collection of Staff Roles by StaffPositionOids
         * Roles are rolled up under Contracts created above.
         */
        Map<String, Collection<StaffPosition>> rolesMap = new HashMap<String, Collection<StaffPosition>>();
        ArrayList<StaffPosition> contractRoles = new ArrayList();
        prevStaffPosition = null;
        prevStaffOid = null;
        currStaffOid = null;
        String prevContractStaffPositionOid = null;

        for (StaffPosition staffPosition : rolesCol) {
            currStaffOid = staffPosition.getStaffOid();
            if (currStaffOid != null) {
                currContractKey = getContractKey(staffPosition);

                if (prevStaffPosition != null) {
                    contractRoles.add(prevStaffPosition);

                    if (!currContractKey.equals(prevContractKey)) {
                        prevContractStaffPositionOid = contractKeyMap.get(prevContractKey);

                        rolesMap.put(prevContractStaffPositionOid, contractRoles);

                        contractRoles = new ArrayList();
                    }
                }

                prevStaffOid = currStaffOid;
                prevContractKey = currContractKey;
                prevStaffPosition = staffPosition;
            }
        }

        if (prevStaffPosition != null) {
            prevContractStaffPositionOid = contractKeyMap.get(prevContractKey);

            contractRoles.add(prevStaffPosition);

            rolesMap.put(prevContractStaffPositionOid, contractRoles);
        }

        setGroupedCollectionToQuery("roles", rolesMap);
    }

    /**
     * Load up the Workforce Member's Curriculums (MasterSchedules).
     *
     * @param staffSubQuery SubQuery
     */
    private void loadWorkforceMemberCurriculums(SubQuery staffSubQuery) {
        X2Criteria sectionCriteria = new X2Criteria();

        // Select Staff
        sectionCriteria.addIn(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.COL_PRIMARY_STAFF_OID, staffSubQuery);

        // Select active Schedule
        sectionCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, MasterSchedule.COL_SCHEDULE_OID);

        // Select Staff's sections that are classes.
        sectionCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

        /*
         * //Use census date to select curriculum records instead of setting term dates.
         * sectionCriteria.addLessOrEqualThan(ScheduleTeacher.REL_SECTION +
         * ModelProperty.PATH_DELIMITER
         * + MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER
         * + ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER
         * + ScheduleTermDate.COL_START_DATE,
         * m_censusDate);
         * sectionCriteria.addGreaterOrEqualThan(ScheduleTeacher.REL_SECTION +
         * ModelProperty.PATH_DELIMITER
         * + MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER
         * + ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER
         * + ScheduleTermDate.COL_END_DATE,
         * m_censusDate);
         */

        // Select sections that are between start term dates
        sectionCriteria.addGreaterOrEqualThan(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER +
                ScheduleTermDate.COL_START_DATE, m_termSelectionStartDate);

        // Select sections that are between end term dates
        sectionCriteria.addLessOrEqualThan(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE_TERM_DATES + ModelProperty.PATH_DELIMITER +
                ScheduleTermDate.COL_END_DATE, m_termSelectionEndDate);

        // Save the SubQuery version of the above selection code in case duplicates are produced.
        /*
         * X2Criteria activeScheduleTermCriteria = new X2Criteria();
         * activeScheduleTermCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_START_DATE,
         * m_termSelectionStartDate);
         * activeScheduleTermCriteria.addLessOrEqualThan(ScheduleTermDate.COL_END_DATE,
         * m_termSelectionEndDate);
         * SubQuery activeScheduleTermSubquery = new SubQuery(ScheduleTermDate.class,
         * ScheduleTermDate.COL_SCHEDULE_TERM_OID, activeScheduleTermCriteria);
         * sectionCriteria.addIn(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
         * MasterSchedule.COL_SCHEDULE_TERM_OID, activeScheduleTermSubquery);
         */

        /*
         * // All Inclusive with Duplicates
         * BeanQuery sectionQuery = new BeanQuery(ScheduleTeacher.class, sectionCriteria);
         * Map sectionMap = getBroker().getGroupedCollectionByQuery(sectionQuery,
         * ScheduleTeacher.COL_STAFF_OID, 64);
         */

        /*
         * Eliminate duplicate Curriculum records. Duplicates are determined by the combination
         * of the fields StaffOid, the Course's generalSubject field and the Section's
         * classYearGroup field.
         */
        ArrayList<ScheduleTeacher> allStaffSections = new ArrayList();

        String courseGeneralSubjectJavaName = translateAliasToJavaName(ALIAS_DFE_COURSE_GENERAL_SUBJECT, true);
        String sectionClassYearGroupJavaName = translateAliasToJavaName(ALIAS_DFE_SECTION_CLASS_YEAR_GROUP, true);

        BeanQuery sectionQuery = new BeanQuery(ScheduleTeacher.class, sectionCriteria);
        sectionQuery.addOrderByAscending(ScheduleTeacher.COL_STAFF_OID);
        sectionQuery.addOrderByAscending(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + courseGeneralSubjectJavaName);
        sectionQuery.addOrderByAscending(
                ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER + sectionClassYearGroupJavaName);
        Collection<ScheduleTeacher> scheduleTeacherList = getBroker().getCollectionByQuery(sectionQuery);

        String currCurriculumKey = "";
        String prevCurriculumKey = "";
        String currStaffOid = "";
        String prevStaffOid = "";
        ScheduleTeacher prevScheduleTeacher = null;

        for (ScheduleTeacher scheduleTeacher : scheduleTeacherList) {
            currStaffOid = scheduleTeacher.getStaffOid();
            currCurriculumKey = getCurriculumKey(scheduleTeacher);

            if (prevScheduleTeacher != null) {
                if (!currCurriculumKey.equals(prevCurriculumKey)) {
                    // Don't include if neither of the CurriculumKey fields are populated.
                    if (!prevCurriculumKey.equals(prevStaffOid)) {
                        allStaffSections.add(prevScheduleTeacher);
                    }
                }
            }

            prevStaffOid = currStaffOid;
            prevCurriculumKey = currCurriculumKey;
            prevScheduleTeacher = scheduleTeacher;
        }

        if (prevScheduleTeacher != null) {
            allStaffSections.add(prevScheduleTeacher);
        }

        // Consolidate the remaining ScheduleTeacher's into a map. Keyed by staffOid
        Map sectionMap = new HashMap();
        ArrayList<ScheduleTeacher> individualStaffSections = new ArrayList();
        prevStaffOid = null;
        currStaffOid = null;

        for (ScheduleTeacher scheduleTeacher : allStaffSections) {
            currStaffOid = scheduleTeacher.getStaffOid();

            if (prevScheduleTeacher != null) {
                individualStaffSections.add(prevScheduleTeacher);

                if (!currStaffOid.equals(prevStaffOid)) {
                    sectionMap.put(prevStaffOid, individualStaffSections);

                    individualStaffSections = new ArrayList();
                }
            }

            prevStaffOid = currStaffOid;
            prevScheduleTeacher = scheduleTeacher;
        }

        if (prevScheduleTeacher != null) {
            individualStaffSections.add(prevScheduleTeacher);

            sectionMap.put(prevStaffOid, individualStaffSections);
        }

        setGroupedCollectionToQuery("curriculums", sectionMap);
    }

    /**
     * Load up the Workforce Member's Qualifications (StaffDegree).
     *
     * @param staffSubQuery SubQuery
     */
    private void loadWorkforceMemberQualifications(SubQuery staffSubQuery) {
        X2Criteria qualificationCriteria = new X2Criteria();
        qualificationCriteria.addIn(StaffDegree.COL_STAFF_OID, staffSubQuery);

        BeanQuery qualificationQuery = new BeanQuery(StaffDegree.class, qualificationCriteria);
        Map qualificationMap =
                getBroker().getGroupedCollectionByQuery(qualificationQuery, StaffDegree.COL_STAFF_OID, 64);

        setGroupedCollectionToQuery("qualifications", qualificationMap);
    }

    /**
     * Load the Workforce Members.
     *
     * @param staffCriteria X2Criteria
     * @return boolean
     */
    private boolean loadWorkforceMembers(X2Criteria staffCriteria) {
        boolean hasStaff = false;
        BeanQuery staffQuery = new BeanQuery(SisStaff.class, staffCriteria);
        Collection<SisStaff> staffs = getBroker().getCollectionByQuery(staffQuery);

        if (staffs.size() > 0) {
            setCollectionToQuery("schoolWorkforceMembers", staffs);
            hasStaff = true;
        }

        return hasStaff;
    }

    /**
     * Remove Blank Elements.
     */
    private void removeEmptyElements() {
        // Remove blank elements
        Element rootElement = getRootElement();
        Element schoolWorkforceMembersElement = rootElement.getChild("SchoolWorkforceMembers");
        if (schoolWorkforceMembersElement == null) {
            return;
        }

        List<Element> schoolWorkforceMemberElementList =
                schoolWorkforceMembersElement.getChildren("SchoolWorkforceMember");

        for (int i = 0; i < schoolWorkforceMemberElementList.size(); i++) {
            Element schoolWorkforceMemberElement = schoolWorkforceMemberElementList.get(i);

            // Staff Details
            Element staffDetailsElement = schoolWorkforceMemberElement.getChild("StaffDetails");
            if (staffDetailsElement != null) {
                // Remove FormerFamilyNames section if PersonFamilyName (only one) is null.
                Element formerFamilyNamesElement = staffDetailsElement.getChild("FormerFamilyNames");
                if (formerFamilyNamesElement != null) {
                    List<Element> personFamilyNamesList = formerFamilyNamesElement.getChildren("PersonFamilyName");
                    for (int j = 0; j < personFamilyNamesList.size(); j++) {
                        Element personFamilyNameElement = personFamilyNamesList.get(j);
                        if (StringUtils.isEmpty(personFamilyNameElement.getText())
                                && personFamilyNamesList.size() == 1) {
                            staffDetailsElement.removeChild("FormerFamilyNames");
                            break;
                        }
                    }
                }

                Element teacherNumberElement = staffDetailsElement.getChild("TeacherNumber");
                if (teacherNumberElement != null && StringUtils.isEmpty(teacherNumberElement.getText())) {
                    staffDetailsElement.removeChild("TeacherNumber");
                }
                Element nINumberElement = staffDetailsElement.getChild("NINumber");
                if (nINumberElement != null && StringUtils.isEmpty(nINumberElement.getText())) {
                    staffDetailsElement.removeChild("NINumber");
                }
                Element genderCurrentElement = staffDetailsElement.getChild("GenderCurrent");
                if (genderCurrentElement != null && StringUtils.isEmpty(genderCurrentElement.getText())) {
                    staffDetailsElement.removeChild("GenderCurrent");
                }
                Element personBirthDateElement = staffDetailsElement.getChild("PersonBirthDate");
                if (personBirthDateElement != null && StringUtils.isEmpty(personBirthDateElement.getText())) {
                    staffDetailsElement.removeChild("PersonBirthDate");
                }
                Element ethnicityElement = staffDetailsElement.getChild("Ethnicity");
                if (ethnicityElement != null && StringUtils.isEmpty(ethnicityElement.getText())) {
                    staffDetailsElement.removeChild("Ethnicity");
                }
                Element disablityElement = staffDetailsElement.getChild("Disability");
                if (disablityElement != null && StringUtils.isEmpty(disablityElement.getText())) {
                    staffDetailsElement.removeChild("Disability");
                }
                Element qTStatusElement = staffDetailsElement.getChild("QTStatus");
                if (qTStatusElement != null && StringUtils.isEmpty(qTStatusElement.getText())) {
                    staffDetailsElement.removeChild("QTStatus");
                }
                Element hLTAStatusElement = staffDetailsElement.getChild("HLTAStatus");
                if (hLTAStatusElement != null && StringUtils.isEmpty(hLTAStatusElement.getText())) {
                    staffDetailsElement.removeChild("HLTAStatus");
                }
                Element qTRouteElement = staffDetailsElement.getChild("QTSRoute");
                if (qTRouteElement != null && StringUtils.isEmpty(qTRouteElement.getText())) {
                    staffDetailsElement.removeChild("QTSRoute");
                }
            }

            // Contract Of Service Group
            Element contractOrServiceGroupElement = schoolWorkforceMemberElement.getChild("ContractOrServiceGroup");
            if (contractOrServiceGroupElement != null) {
                List<Element> contractOrServiceElementList =
                        contractOrServiceGroupElement.getChildren("ContractOrService");
                for (int p = 0; p < contractOrServiceElementList.size(); p++) {
                    Element contractOrServiceElement = contractOrServiceElementList.get(p);
                    if (contractOrServiceElement != null) {
                        Element contractTypeElement = contractOrServiceElement.getChild("ContractType");
                        if (contractTypeElement != null && StringUtils.isEmpty(contractTypeElement.getText())) {
                            contractOrServiceElement.removeChild("ContractType");
                        }
                        Element contractStartElement = contractOrServiceElement.getChild("ContractStart");
                        if (contractStartElement != null && StringUtils.isEmpty(contractStartElement.getText())) {
                            contractOrServiceElement.removeChild("ContractStart");
                        }
                        Element contractEndElement = contractOrServiceElement.getChild("ContractEnd");
                        if (contractEndElement != null && StringUtils.isEmpty(contractEndElement.getText())) {
                            contractOrServiceElement.removeChild("ContractEnd");
                        }
                        Element postElement = contractOrServiceElement.getChild("Post");
                        if (postElement != null && StringUtils.isEmpty(postElement.getText())) {
                            contractOrServiceElement.removeChild("Post");
                        }
                        Element schoolArrivalDateElement = contractOrServiceElement.getChild("SchoolArrivalDate");
                        if (schoolArrivalDateElement != null
                                && StringUtils.isEmpty(schoolArrivalDateElement.getText())) {
                            contractOrServiceElement.removeChild("SchoolArrivalDate");
                        }
                        Element dailyRateElement = contractOrServiceElement.getChild("DailyRate");
                        if (dailyRateElement != null && StringUtils.isEmpty(dailyRateElement.getText())) {
                            contractOrServiceElement.removeChild("DailyRate");
                        }
                        Element destinationCodeElement = contractOrServiceElement.getChild("DestinationCode");
                        if (destinationCodeElement != null && StringUtils.isEmpty(destinationCodeElement.getText())) {
                            contractOrServiceElement.removeChild("DestinationCode");
                        }
                        Element originElement = contractOrServiceElement.getChild("Origin");
                        if (originElement != null && StringUtils.isEmpty(originElement.getText())) {
                            contractOrServiceElement.removeChild("Origin");
                        }

                        // Roles
                        Element rolesElement = contractOrServiceElement.getChild("Roles");
                        if (rolesElement != null) {
                            List<Element> roleElementList = rolesElement.getChildren("Role");
                            for (int k = 0; k < roleElementList.size(); k++) {
                                Element roleElement = roleElementList.get(k);

                                if (roleElement != null) {
                                    Element roleIdentifierElement = roleElement.getChild("RoleIdentifier");
                                    if (roleIdentifierElement != null
                                            && StringUtils.isEmpty(roleIdentifierElement.getText())) {
                                        roleElement.removeChild("RoleIdentifier");
                                    }

                                    Element roleLevelDetailsElement = roleElement.getChild("RoleLevelDetails");
                                    if (roleLevelDetailsElement != null) {
                                        // Payments
                                        Element paymentsElement = roleLevelDetailsElement.getChild("Payments");
                                        if (paymentsElement != null) {
                                            Element scaleElement = paymentsElement.getChild("Scale");
                                            if (scaleElement != null && StringUtils.isEmpty(scaleElement.getText())) {
                                                paymentsElement.removeChild("Scale");
                                            }
                                            Element regionSpineElement = paymentsElement.getChild("RegionSpine");
                                            if (regionSpineElement != null
                                                    && StringUtils.isEmpty(regionSpineElement.getText())) {
                                                paymentsElement.removeChild("RegionSpine");
                                            }
                                            Element spinePointElement = paymentsElement.getChild("SpinePoint");
                                            if (spinePointElement != null
                                                    && StringUtils.isEmpty(spinePointElement.getText())) {
                                                paymentsElement.removeChild("SpinePoint");
                                            }
                                            Element basePayElement = paymentsElement.getChild("BasePay");
                                            if (basePayElement != null
                                                    && StringUtils.isEmpty(basePayElement.getText())) {
                                                paymentsElement.removeChild("BasePay");
                                            }
                                            Element safeguardedSalaryElement =
                                                    paymentsElement.getChild("SafeguardedSalary");
                                            if (safeguardedSalaryElement != null
                                                    && StringUtils.isEmpty(safeguardedSalaryElement.getText())) {
                                                paymentsElement.removeChild("SafeguardedSalary");
                                            }
                                        }

                                        // Hours
                                        Element hoursElement = roleLevelDetailsElement.getChild("Hours");
                                        if (hoursElement != null) {
                                            Element hoursPerWeekElement = hoursElement.getChild("HoursPerWeek");
                                            if (hoursPerWeekElement != null
                                                    && StringUtils.isEmpty(hoursPerWeekElement.getText())) {
                                                hoursElement.removeChild("HoursPerWeek");
                                            }
                                            Element fTEHoursElement = hoursElement.getChild("FTEHours");
                                            if (fTEHoursElement != null
                                                    && StringUtils.isEmpty(fTEHoursElement.getText())) {
                                                hoursElement.removeChild("FTEHours");
                                            }
                                            Element weeksPerYearElement = hoursElement.getChild("WeeksPerYear");
                                            if (weeksPerYearElement != null
                                                    && StringUtils.isEmpty(weeksPerYearElement.getText())) {
                                                hoursElement.removeChild("WeeksPerYear");
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Qualifications
            Element qualificationsElement = schoolWorkforceMemberElement.getChild("Qualifications");
            if (qualificationsElement != null) {
                List<Element> qualificationElementList = qualificationsElement.getChildren("Qualification");

                boolean invalidQualificationCode = false;
                boolean invalidQualificationSubject1 = false;
                boolean invalidQualificationSubject2 = false;
                int numInvalidQualificationElements = 0;
                for (int l = 0; l < qualificationElementList.size(); l++) {
                    Element qualificationElement = qualificationElementList.get(l);
                    if (qualificationElement != null) {
                        Element qualificationCodeElement = qualificationElement.getChild("QualificationCode");
                        if (qualificationCodeElement != null
                                && StringUtils.isEmpty(qualificationCodeElement.getText())) {
                            invalidQualificationCode = true;
                        }

                        // Subjects
                        List<Element> subjectsElementList = qualificationElement.getChildren("Subjects");

                        for (int m = 0; m < subjectsElementList.size(); m++) {
                            Element subjectElement = subjectsElementList.get(m);

                            if (subjectElement != null) {
                                Element qualificationSubjectElement = subjectElement.getChild("QualificationSubject");
                                if (qualificationSubjectElement != null
                                        && StringUtils.isEmpty(qualificationSubjectElement.getText())) {
                                    if (m == 0) {
                                        invalidQualificationSubject1 = true;
                                    } else if (m == 1) {
                                        invalidQualificationSubject2 = true;

                                        // Not required, If empty remove
                                        qualificationSubjectElement.detach();
                                    }
                                }
                            }
                        }
                    }

                    // If most values are empty then remove group element
                    if (invalidQualificationCode && invalidQualificationSubject1 && invalidQualificationSubject2) {
                        numInvalidQualificationElements++;

                        invalidQualificationCode = false;
                        invalidQualificationSubject1 = false;
                        invalidQualificationSubject2 = false;
                    }
                }

                // If all elements are invalid them remove the parent element.
                if (numInvalidQualificationElements == qualificationElementList.size()) {
                    schoolWorkforceMemberElement.removeChild("Qualifications");
                }
            }
        }

        Element schoolElement = rootElement.getChild("School");
        if (schoolElement != null) {
            Element staffInformationElement = schoolElement.getChild("StaffInformation");
            if (staffInformationElement != null) {
                Element occasionalsElement = staffInformationElement.getChild("Occasionals");
                if (occasionalsElement != null) {
                    Element occasionalQTSElement = occasionalsElement.getChild("OccasionalsQTS");
                    if (occasionalQTSElement != null && StringUtils.isEmpty(occasionalQTSElement.getText())) {
                        occasionalQTSElement.detach();
                    }
                    Element occasionalNOTQTSElement = occasionalsElement.getChild("OccasionalsNOTQTS");
                    if (occasionalNOTQTSElement != null && StringUtils.isEmpty(occasionalNOTQTSElement.getText())) {
                        occasionalNOTQTSElement.detach();
                    }
                    Element occasionalNOTKNWNElement = occasionalsElement.getChild("OccasionalsNOTKNWN");
                    if (occasionalNOTKNWNElement != null && StringUtils.isEmpty(occasionalNOTKNWNElement.getText())) {
                        occasionalNOTKNWNElement.detach();
                    }

                    // If everything has been deleted then remove the section.
                    occasionalsElement = staffInformationElement.getChild("Occasionals");
                    if (occasionalsElement != null && StringUtils.isEmpty(occasionalsElement.getText())
                            && occasionalsElement.getContent().size() == 1) {
                        occasionalsElement.detach();
                    }
                }

                Element agencyTPsupportElement = staffInformationElement.getChild("AgencyTPsupport");
                if (agencyTPsupportElement != null) {
                    List<Element> agencyTPSupportCountList = agencyTPsupportElement.getChildren("AgencyTPsupportCount");
                    int removeElementCount = 0;
                    for (int n = 0; n < agencyTPSupportCountList.size(); n++) {
                        Element agencyTPSupportCountElement = agencyTPSupportCountList.get(n);
                        if (agencyTPSupportCountElement != null) {
                            Element agencyTPsupportCategoryElement =
                                    agencyTPSupportCountElement.getChild("AgencyTPsupportCategory");
                            if (agencyTPsupportCategoryElement != null
                                    && StringUtils.isEmpty(agencyTPsupportCategoryElement.getText())) {
                                agencyTPsupportCategoryElement.detach();
                                removeElementCount++;
                            }
                            Element supHeadCountElement = agencyTPSupportCountElement.getChild("SupHeadCount");
                            if (supHeadCountElement != null && StringUtils.isEmpty(supHeadCountElement.getText())) {
                                supHeadCountElement.detach();
                                removeElementCount++;
                            }
                        }

                        agencyTPSupportCountElement = agencyTPSupportCountList.get(n);
                        if (agencyTPSupportCountElement != null
                                && StringUtils.isEmpty(agencyTPSupportCountElement.getText())
                                && agencyTPSupportCountElement.getContent().size() == 1) {
                            agencyTPSupportCountElement.detach();
                        }
                    }

                    agencyTPsupportElement = staffInformationElement.getChild("AgencyTPsupport");
                    if (removeElementCount == 6) {
                        agencyTPsupportElement.detach();
                    }
                }

                if (staffInformationElement != null && StringUtils.isEmpty(staffInformationElement.getText())
                        && staffInformationElement.getContent().size() == 1) {
                    staffInformationElement.detach();
                }
            }
        }

        Element lAElement = rootElement.getChild("LA");
        if (lAElement != null) {
            Element educationalPsychologistsElement = lAElement.getChild("EducationalPsychologists");
            if (educationalPsychologistsElement != null) {
                Element edPsychsFTElement = educationalPsychologistsElement.getChild("EdPsychsFT");
                if (edPsychsFTElement != null && StringUtils.isEmpty(edPsychsFTElement.getText())) {
                    edPsychsFTElement.detach();
                }
                Element edPsychsPTElement = educationalPsychologistsElement.getChild("EdPsychsPT");
                if (edPsychsPTElement != null && StringUtils.isEmpty(edPsychsPTElement.getText())) {
                    edPsychsPTElement.detach();
                }
                Element edPsychsFTEElement = educationalPsychologistsElement.getChild("EdPsychsFTE");
                if (edPsychsFTEElement != null && StringUtils.isEmpty(edPsychsFTEElement.getText())) {
                    edPsychsFTEElement.detach();
                }

                educationalPsychologistsElement = lAElement.getChild("EducationalPsychologists");
                if (educationalPsychologistsElement != null
                        && StringUtils.isEmpty(educationalPsychologistsElement.getText())
                        && educationalPsychologistsElement.getContent().size() == 1) {
                    educationalPsychologistsElement.detach();
                }
            }

            lAElement = rootElement.getChild("LA");
            if (lAElement != null && StringUtils.isEmpty(lAElement.getText()) && lAElement.getContent().size() == 1) {
                lAElement.detach();
            }
        }
    }
}
