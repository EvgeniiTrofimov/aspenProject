/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.on.register.original;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Selection;
import com.follett.fsc.core.k12.beans.SelectionObject;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.on.register.original.OntarioAlias;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.model.beans.TeacherSection;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Enrolment Register - Independent Study and Online Learning Register for Day School Pupils for
 * Ontario is used to print by course.
 *
 * This extends the cont ed register java as is similar to those reports.
 *
 * Combines a portrait summary page and landscape list of students page per
 * course.
 *
 * @author Follett Software Company
 */
public class EnrRegDayIndepElearn extends EnrRegByCourseDayIndep implements Publishable {
    private static final long serialVersionUID = 1L;

    // Input parameters
    protected static final String PARAM_CSK_CON_ED_DELIVERY_TYPES = "cskConEdDeliveryTypes";

    // Report parameters
    // for localization
    protected static final String REPORT_SHORT_DATE_FMT_OUTPUT = "shortDateFormat";
    // for assignment counts
    protected static final String REPORT_ASSIGN_REQUIRED_CT_TO_MST_MAP = "classWorkUnitsRequiredToMstMap";
    protected static final String REPORT_ASSIGN_CT_PB_UNDER21_REG_TO_MST_MAP = "classWorkUnitsUnder21PbRegTotToMstMap";
    protected static final String REPORT_ASSIGN_CT_PB_UNDER21_HC_TO_MST_MAP = "classWorkUnitsUnder21PbHcTotToMstMap";
    protected static final String REPORT_ASSIGN_CT_OP_UNDER21_TO_MST_MAP = "classWorkUnitsUnder21OpTotToMstMap";
    protected static final String REPORT_ASSIGN_CT_PB_OVER21_TO_MST_MAP = "classWorkUnitsOver21PbTotToMstMap";
    protected static final String REPORT_ASSIGN_CT_OP_OVER21_TO_MST_MAP = "classWorkUnitsOver21OpTotToMstMap";

    // for class attendance dates
    protected static final String REPORT_ATT_REQUIRED_CT_TO_MST_MAP =
            "classSessionsRequiredToMstMap";
    protected static final String REPORT_ATT_SCHEDULED_CT_PB_UNDER21_TO_MST_MAP =
            "classSessionsScheduledUnder21PbTotToMstMap";
    protected static final String REPORT_ATT_SCHEDULED_CT_OP_UNDER21_TO_MST_MAP =
            "classSessionsScheduledUnder21OpTotToMstMap";
    protected static final String REPORT_ATT_SCHEDULED_CT_PB_OVER21_TO_MST_MAP =
            "classSessionsScheduledOver21PbTotToMstMap";
    protected static final String REPORT_ATT_SCHEDULED_CT_OP_OVER21_TO_MST_MAP =
            "classSessionsScheduledOver21OpTotToMstMap";
    protected static final String REPORT_ATT_CONTACT_CT_PB_UNDER21_TO_MST_MAP =
            "classSessionsAttendedUnder21PbTotToMstMap";
    protected static final String REPORT_ATT_CONTACT_CT_OP_UNDER21_TO_MST_MAP =
            "classSessionsAttendedUnder21OpTotToMstMap";
    protected static final String REPORT_ATT_CONTACT_CT_PB_OVER21_TO_MST_MAP =
            "classSessionsAttendedOver21PbTotToMstMap";
    protected static final String REPORT_ATT_CONTACT_CT_OP_OVER21_TO_MST_MAP =
            "classSessionsAttendedOver21OpTotToMstMap";

    // Grid fields
    protected static final String FIELD_ADULT_IND = "adultInd";
    protected static final String FIELD_OP_PAYER = "opPayer";
    protected static final String FIELD_HC_FACTOR = "hcFactor";
    protected static final String FIELD_STD_MST_START_DATE = "startDate";
    protected static final String FIELD_STD_MST_END_DATE = "endDate";

    // for assignment counts
    protected static final String FIELD_ASSIGN_CT_PB_UNDER21_REG = "assignCtPbUnder21Reg";
    protected static final String FIELD_ASSIGN_CT_PB_UNDER21_HC = "assignCtPbUnder21Hc";
    protected static final String FIELD_ASSIGN_CT_OP_UNDER21 = "assignCtOpUnder21";
    protected static final String FIELD_ASSIGN_CT_PB_OVER21 = "assignCtPbOver21";
    protected static final String FIELD_ASSIGN_CT_OP_OVER21 = "assignCtOpOver21";

    // for class attendance dates
    protected static final String FIELD_ATT_CLASS_CONTACT_LIST = "attClassContactList";
    protected static final String FIELD_ATT_SCHEDULED_CT_PB_UNDER21 = "attScheduledCtPbUnder21";
    protected static final String FIELD_ATT_SCHEDULED_CT_OP_UNDER21 = "attScheduledCtOpUnder21";
    protected static final String FIELD_ATT_SCHEDULED_CT_PB_OVER21 = "attScheduledCtPbOver21";
    protected static final String FIELD_ATT_SCHEDULED_CT_OP_OVER21 = "attScheduledCtOpOver21";
    protected static final String FIELD_ATT_CONTACT_CT_PB_UNDER21 = "attContactCtPbUnder21";
    protected static final String FIELD_ATT_CONTACT_CT_OP_UNDER21 = "attContactCtOpUnder21";
    protected static final String FIELD_ATT_CONTACT_CT_PB_OVER21 = "attContactCtPbOver21";
    protected static final String FIELD_ATT_CONTACT_CT_OP_OVER21 = "attContactCtOpOver21";

    // Constant values - Localization
    protected static final String CONST_DATE_FMT_STR_ENG = "MM/dd/yyyy";
    protected static final String CONST_DATE_FMT_STR_FR = "dd/MM/yyyy";

    // Constant values - Sessions
    protected static final String ALIAS_CSK_ISR_SESSIONS = "all-csk-ISRSessions";

    // Constant values - attendance
    public static final int CONST_ATT_CONTACT_LIST_MAX_CT = 30;
    public static final String CONST_ATT_CODE_ISP_PRESENT_ENGLISH = "ISP";
    public static final String CONST_ATT_CODE_ISP_PRESENT_FRENCH = "ISP";
    public static final List<String> CONST_ATT_CODE_ISP_PRESENT_LIST =
            new ArrayList<String>(Arrays.asList(CONST_ATT_CODE_ISP_PRESENT_ENGLISH,
                    CONST_ATT_CODE_ISP_PRESENT_FRENCH));
    public static final String CONST_ATT_CODE_ISP_PRESENT_OUT = "x";
    public static final String CONST_ATT_CODE_ISP_ABSENT_OUT = "A";

    // Variables - input
    protected PlainDate m_contextDateDec31;

    // Variables - selection, mst, std
    protected Map<String, Boolean> m_stdAdultIndByStdOid = null;

    // Variables - maps
    // for assignment counts - parameter
    protected Map<String, Integer> m_assignCtPbUnder21RegToMstOidMap;
    protected Map<String, Integer> m_assignCtPbUnder21HcToMstOidMap;
    protected Map<String, Integer> m_assignCtOpUnder21ToMstOidMap;
    protected Map<String, Integer> m_assignCtPbOver21ToMstOidMap;
    protected Map<String, Integer> m_assignCtOpOver21ToMstOidMap;

    // for class attendance dates - parameter
    protected Map<String, Integer> m_attRequiredCtToMstOidMap;
    protected Map<String, Integer> m_attScheduledCtPbUnder21ToMstOidMap;
    protected Map<String, Integer> m_attScheduledCtOpUnder21ToMstOidMap;
    protected Map<String, Integer> m_attScheduledCtPbOver21ToMstOidMap;
    protected Map<String, Integer> m_attScheduledCtOpOver21ToMstOidMap;
    protected Map<String, Integer> m_attContactCtPbUnder21ToMstOidMap;
    protected Map<String, Integer> m_attContactCtOpUnder21ToMstOidMap;
    protected Map<String, Integer> m_attContactCtPbOver21ToMstOidMap;
    protected Map<String, Integer> m_attContactCtOpOver21ToMstOidMap;

    // for reference tables
    protected Map<String, ReferenceCode> m_refCodeOpPayerMap;

    /**
     * Gets report type based on input parameters
     * - override to set both byAssign and byDate to true
     *
     * @see com.x2dev.reports.on.EnrRegByCourseData#getReportType()
     */
    @Override
    protected void getReportType() {
        m_byAssign = true;
        m_byDate = true;
    }

    /**
     * @see com.x2dev.reports.on.EnrRegByCourseData#loadMstSelection()
     *      - override to get sections based on delivery type instead of program type
     */
    @Override
    protected void loadMstSelection() {
        /*
         * General set up of input parameters, master schedule selection objects,
         * initialization
         */
        // Get master schedule oids based on input selection
        Collection<String> mstOidsInput = new ArrayList<String>();
        if (getParameter(PARAM_MST_OIDS) != null) {
            mstOidsInput.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS), ","));
        }
        if (getParameter(PARAM_MST_OIDS_STAFF_VIEW) != null) {
            mstOidsInput.addAll(
                    StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS_STAFF_VIEW), ","));
        }

        // initialize selection
        m_selectionMst = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
        m_selectionMst.setTimestamp(System.currentTimeMillis());

        // initialize other variables
        m_mstToMstOid = new HashMap<String, MasterSchedule>();
        m_classStartDateToMstOid = new HashMap<String, PlainDate>();
        m_classEndDateToMstOid = new HashMap<String, PlainDate>();
        m_classSummerCourseIndToMstOid = new HashMap<String, Boolean>();

        /*
         * Add to selection if sections have been selected
         */
        // loop through, save selection objects if sections have been selected
        try {
            for (String mstOid : mstOidsInput) {
                // create SelectionObject for each mst oid
                SelectionObject selectionObj = X2BaseBean.newInstance(SelectionObject.class,
                        getBroker().getPersistenceKey());
                selectionObj.setObjectOid(mstOid);
                m_selectionMst.addToSelectionObjects(selectionObj);

                // get master schedule for mstOid
                MasterSchedule mst = getBroker().getBeanByOid(MasterSchedule.class, mstOid);
                m_mstToMstOid.put(mstOid, mst);

                // save master schedule start and end dates
                saveMstStartEndDates(mst, mstOid);
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        /*
         * Add to selection if all sections that meet other criteria
         */
        // select all sections based on school/staff if particular sections not selected
        if (mstOidsInput.isEmpty()) {
            Collection<String> cskConEdDeliveryTypesInput = new ArrayList<String>();
            if (getParameter(PARAM_CSK_CON_ED_DELIVERY_TYPES) != null) {
                cskConEdDeliveryTypesInput.addAll(StringUtils
                        .convertDelimitedStringToList((String) getParameter(PARAM_CSK_CON_ED_DELIVERY_TYPES), ";"));
            }

            // create master schedule criteria and add sections to selection object
            X2Criteria mstCriteria = new X2Criteria();

            // add school condition
            mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    getSchool().getOid());

            // add school year condition
            mstCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                    + PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

            // add staff condition (would be from staff view)
            if (m_staffCurrent != null) {
                mstCriteria.addEqualTo(
                        MasterSchedule.REL_TEACHER_SECTIONS + PATH_DELIMITER + TeacherSection.COL_STAFF_OID,
                        m_staffCurrent.getOid());
            }

            // add course con ed program condition
            String fieldSchoolCourseDeliveryType = getBeanPathFromAlias(OntarioAlias.ALIAS_CSK_COURSE_DELIVERY_TYPE,
                    false);
            mstCriteria.addIn(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + fieldSchoolCourseDeliveryType,
                    cskConEdDeliveryTypesInput);

            // create query for master schedule
            QueryByCriteria mstQuery = new QueryByCriteria(MasterSchedule.class, mstCriteria);

            // load master schedules
            try (QueryIterator mstIterator = getBroker().getIteratorByQuery(mstQuery)) {
                // iterates through and saves
                while (mstIterator.hasNext()) {
                    MasterSchedule mst = (MasterSchedule) mstIterator.next();
                    String mstOid = mst.getOid();

                    // create SelectionObject for each mst oid
                    SelectionObject selectionObj = X2BaseBean.newInstance(SelectionObject.class,
                            getBroker().getPersistenceKey());
                    selectionObj.setObjectOid(mstOid);
                    m_selectionMst.addToSelectionObjects(selectionObj);

                    // save master schedule for mstOid
                    m_mstToMstOid.put(mstOid, mst);

                    // save master schedule start and end dates
                    saveMstStartEndDates(mst, mstOid);
                }
            }
        }

        // Save the selection as criteria for use on other queries or show errors
        Collection<ValidationError> errors = getBroker().saveBean(m_selectionMst);
        if (errors.size() > 0) {
            StringBuilder errorCause = new StringBuilder();
            for (ValidationError err : errors) {
                errorCause.append(WebUtils.getMessage(err, getBroker().getPersistenceKey()));
            }
            throw new RuntimeException(errorCause.toString());
        }

        // Save course start/end dates
        addParameter(REPORT_COURSE_START_DATE_TO_MST_MAP, m_classStartDateToMstOid);
        addParameter(REPORT_COURSE_END_DATE_TO_MST_MAP, m_classEndDateToMstOid);
        // map will be empty for this register
        addParameter(REPORT_COURSE_SUMMER_COURSE_iND_TO_MST_MAP, m_classSummerCourseIndToMstOid);
    }

    /**
     * @see com.x2dev.reports.on.EnrRegByCourseData#loadClassAttListToStdOidToMstOid()
     *      - override to load period attendance based on whether att code is for contact made
     *      (equivalent to present)
     */
    @Override
    protected void loadClassAttListToStdOidToMstOid() {
        // create criteria/query to get class attendance for selected sections
        X2Criteria patCriteria = new X2Criteria();

        // based on selection of master schedule oids
        SubQuery subQuery = getSubQueryFromMstSelection(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        patCriteria.addExists(subQuery);

        String[] patColumns = new String[] {StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID,
                StudentPeriodAttendance.COL_STUDENT_OID, StudentPeriodAttendance.COL_DATE,
                StudentPeriodAttendance.COL_CODE_VIEW, StudentPeriodAttendance.COL_OTHER_CODE,
                StudentPeriodAttendance.COL_OTHER_CODE02};

        ColumnQuery patQuery = new ColumnQuery(StudentPeriodAttendance.class, patColumns, patCriteria);

        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID);
        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);
        patQuery.addOrderByAscending(StudentPeriodAttendance.COL_DATE);

        // load map of class attendance to master schedule/student
        m_attListToMstOidStdOidMap = new HashMap<String, List<String>>();
        // keeping unneeded empty map in case looked up
        m_attAbsentUnfundedCtToMstOidStdOid = new HashMap<String, Integer>();
        String mstOidPrev = CONST_EMPTY;
        String mstOidStdOidPrev = CONST_EMPTY;
        List<String> attList = null;
        // these will be initialized based on class dates for section
        List<PlainDate> classDtListForMstOid = null;
        int classDtListSize = CONST_ZERO.intValue();
        // loop through class attendance
        try (ReportQueryIterator patIterator = getBroker().getReportQueryIteratorByQuery(patQuery)) {
            while (patIterator.hasNext()) {
                // get new values
                Object[] data = (Object[]) patIterator.next();
                String mstOid = data[0].toString();
                String stdOid = data[1].toString();
                String mstOidStdOid = mstOid + CONST_HYPHEN + stdOid;
                PlainDate patDate = new PlainDate((Timestamp) data[2]);
                String patCodeIn = data[3].toString();
                String patOtherCode = OntarioAlias.CONST_EMPTY;
                if (data[4] != null) {
                    patOtherCode = data[4].toString();
                }

                // continue to next iteration if patDate is not between start and end date for
                // std for mst
                EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst =
                        m_stdStartEndDatesToMstOidMap.get(mstOid);
                if ((stdEnrStartEndDatesForMst == null)
                        || (!stdEnrStartEndDatesForMst.isDateInStdMstEnrDates(stdOid, patDate))) {
                    continue;
                }

                // if new section/student, write attendance count for prev
                if ((!StringUtils.isEmpty(mstOidStdOidPrev) && (!mstOidStdOidPrev.equals(mstOidStdOid)))) {
                    if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                        attList = setAttCodeListToFrench(attList);
                    }
                    m_attListToMstOidStdOidMap.put(mstOidStdOidPrev, attList);
                    attList = null;
                }
                mstOidStdOidPrev = mstOidStdOid;

                // if new section get class dates and size of class dates list
                if (!mstOid.equals(mstOidPrev)) {
                    classDtListForMstOid = m_classDtListToMstOidMap.get(mstOid);
                    if (classDtListForMstOid != null) {
                        classDtListSize = classDtListForMstOid.size();
                    } else {
                        classDtListSize = CONST_ZERO.intValue();
                    }
                }
                mstOidPrev = mstOid;

                // get index of att date in class date list
                int indexAttDt = CONST_LIST_NOT_FOUND;
                if ((classDtListForMstOid != null) && (classDtListForMstOid.contains(patDate))) {
                    indexAttDt = classDtListForMstOid.indexOf(patDate);
                }

                // save class attendance if att date is found in class date list
                if (!(indexAttDt == CONST_LIST_NOT_FOUND)) {
                    // initialize class attendance list for student/section if first attendance for
                    // them
                    if (attList == null) {
                        attList = new ArrayList<String>(classDtListSize);
                        for (int i = 0; i < classDtListSize; i++) {
                            attList.add(CONST_EMPTY);
                        }
                    }

                    // save class attendance
                    String patCodeEnglish = patCodeIn;
                    if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                        if (OntarioAlias.ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.get(patCodeIn) != null) {
                            patCodeEnglish = OntarioAlias.ATT_CODES_LOCALE_ENGLISH_TO_FRENCH_MAP.get(patCodeIn);
                        }
                    }

                    if ((!StringUtils.isEmpty(patOtherCode))
                            && (CONST_ATT_CODE_ISP_PRESENT_LIST.contains(patOtherCode))) {
                        patCodeEnglish = CONST_ATT_CODE_ISP_PRESENT_OUT;
                    } else if (OntarioAlias.ATT_CODE_ABSENT.equals(patCodeIn)) {
                        patCodeEnglish = CONST_ATT_CODE_ISP_ABSENT_OUT;
                    } else {
                        patCodeEnglish = OntarioAlias.CONST_EMPTY;
                    }

                    // set pat code in output list
                    attList.set(indexAttDt, patCodeEnglish.trim());
                }
            }
        } catch (Exception e) {
            // Catch the exception into the report output message.
            logToolMessage(Level.INFO, e.getMessage(), false);
            throw e;
        }

        // write class attendance for students for last section/student
        if (!StringUtils.isEmpty(mstOidStdOidPrev)) {
            if (!m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
                attList = setAttCodeListToFrench(attList);
            }
            m_attListToMstOidStdOidMap.put(mstOidStdOidPrev, attList);
        }
    }

    /**
     * Build output grid
     *
     * @see com.x2dev.reports.on.EnrRegByCourseData#buildOutputGrid()
     */
    @Override
    protected void buildOutputGrid() {
        // loop through master schedule/student and put output grid records
        Set<String> mstOidsSorted = m_stdOidListToMstOid.keySet();

        // initialize variables
        m_assignCtPbUnder21RegToMstOidMap = new HashMap<String, Integer>();
        m_assignCtPbUnder21HcToMstOidMap = new HashMap<String, Integer>();
        m_assignCtOpUnder21ToMstOidMap = new HashMap<String, Integer>();
        m_assignCtPbOver21ToMstOidMap = new HashMap<String, Integer>();
        m_assignCtOpOver21ToMstOidMap = new HashMap<String, Integer>();
        m_attRequiredCtToMstOidMap = new HashMap<String, Integer>();
        m_attScheduledCtPbUnder21ToMstOidMap = new HashMap<String, Integer>();
        m_attScheduledCtOpUnder21ToMstOidMap = new HashMap<String, Integer>();
        m_attScheduledCtPbOver21ToMstOidMap = new HashMap<String, Integer>();
        m_attScheduledCtOpOver21ToMstOidMap = new HashMap<String, Integer>();
        m_attContactCtPbUnder21ToMstOidMap = new HashMap<String, Integer>();
        m_attContactCtOpUnder21ToMstOidMap = new HashMap<String, Integer>();
        m_attContactCtPbOver21ToMstOidMap = new HashMap<String, Integer>();
        m_attContactCtOpOver21ToMstOidMap = new HashMap<String, Integer>();

        // loop through sections
        for (String mstOid : mstOidsSorted) {
            MasterSchedule mst = m_mstToMstOid.get(mstOid);
            List<String> stdOids = m_stdOidListToMstOid.get(mstOid);
            EnrRegStudentStartEndDatesForSection stdEnrStartEndDatesForMst = m_stdStartEndDatesToMstOidMap.get(mstOid);
            int stdCt = 0;

            // initialize work units variables
            int assignCtPbUnder21RegMst = CONST_ZERO.intValue();
            int assignCtPbUnder21HcMst = CONST_ZERO.intValue();
            int assignCtOpUnder21Mst = CONST_ZERO.intValue();
            int assignCtPbOver21Mst = CONST_ZERO.intValue();
            int assignCtOpOver21Mst = CONST_ZERO.intValue();

            // get att/sessions required and scheduled counts
            int attRequiredCtMst = CONST_ZERO.intValue();
            int attScheduledCtMst = CONST_ZERO.intValue();
            String isrSessionsDb = (String) mst.getSchoolCourse().getFieldValueByAlias(ALIAS_CSK_ISR_SESSIONS);
            if (isrSessionsDb != null) {
                attRequiredCtMst = Integer.valueOf(isrSessionsDb).intValue();
                attScheduledCtMst = attRequiredCtMst;
            }

            // initialize sessions variables
            int attScheduledCtPbUnder21Mst = CONST_ZERO.intValue();
            int attScheduledCtOpUnder21Mst = CONST_ZERO.intValue();
            int attScheduledCtPbOver21Mst = CONST_ZERO.intValue();
            int attScheduledCtOpOver21Mst = CONST_ZERO.intValue();
            int attContactCtPbUnder21Mst = CONST_ZERO.intValue();
            int attContactCtOpUnder21Mst = CONST_ZERO.intValue();
            int attContactCtPbOver21Mst = CONST_ZERO.intValue();
            int attContactCtOpOver21Mst = CONST_ZERO.intValue();

            // get class dates, number of class sessions, student drop (saved in effective
            // end date) dates if report is by
            // date
            List<PlainDate> classDtListForMstOid = new ArrayList<PlainDate>();
            int classSessionsCtForMstOid = 0;
            if ((m_byDate) && (m_classDtListToMstOidMap.get(mstOid) != null)) {
                classDtListForMstOid = m_classDtListToMstOidMap.get(mstOid);
                classSessionsCtForMstOid = classDtListForMstOid.size();
            } else {
                continue;
            }

            // loop through students
            for (String stdOid : stdOids) {
                SisStudent std = getBroker().getBeanByOid(SisStudent.class, stdOid);
                String mstOidStdOid = mstOid + CONST_HYPHEN + stdOid;
                stdCt++;

                // get op indicator from enr info
                String opInd = CONST_EMPTY;
                String opPayerCode = CONST_EMPTY;
                if (m_enrInfoToStdOidTypeMap.containsKey(stdOid + OntarioAlias.CONST_ENR_OP_IND_SUFFIX)) {
                    opInd = m_enrInfoToStdOidTypeMap.get(stdOid + OntarioAlias.CONST_ENR_OP_IND_SUFFIX);
                    opPayerCode = m_enrInfoToStdOidTypeMap.get(stdOid + OntarioAlias.CONST_ENR_OP_PAYER_SUFFIX);
                }

                // get adult indicator
                Boolean stdAdultInd = m_stdAdultIndByStdOid.get(stdOid);
                if (stdAdultInd == null) {
                    // get student adult indicator if not calculated already for student
                    stdAdultInd = isStdAdult(std);
                    m_stdAdultIndByStdOid.put(stdOid, stdAdultInd);
                }

                // set student hc factor
                String stdHcFactor = CONST_EMPTY;

                // get school month for student (first) start date to see if op
                PlainDate effStartDateForStd = stdEnrStartEndDatesForMst.getEffStartDateForStdForMst(stdOid);
                m_calendar.setTime(effStartDateForStd);
                int classStartDateForStdSklMonth = OntarioAlias.CONST_CAL_MONTH_TO_SKL_MONTH_MAP
                        .get(m_calendar.get(Calendar.MONTH) + 1).intValue();

                // update op indicator based on student start date and school association for
                // student/section school
                String opIndSsk = null;
                String mstSklOid = mst.getSchoolCourse().getSchoolOid();
                if ((m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid) != null) &&
                        (m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid).get(mstSklOid) != null)) {
                    Map<Integer, String> sskOpIndToSklMthForSklOid =
                            m_sskOpIndToSklMthToSklOidToStdOidMap.get(stdOid).get(mstSklOid);
                    opIndSsk =
                            sskOpIndToSklMthForSklOid.get(Integer.valueOf(classStartDateForStdSklMonth));
                }
                if (opIndSsk != null) {
                    opInd = opIndSsk;
                }

                // get end date for student for mst
                PlainDate effEndDateForStd = getEffEndDateForStdForMst(stdEnrStartEndDatesForMst, stdOid);

                // start set grid fields
                m_grid.append();
                m_grid.set(FIELD_MASTER_SCHEDULE, mst);
                m_grid.set(FIELD_STUDENT, std);
                m_grid.set(FIELD_OP_IND, opInd);
                m_grid.set(FIELD_ADULT_IND, stdAdultInd);
                String opPayer = opPayerCode;
                if (m_refCodeOpPayerMap.containsKey(opPayerCode)) {
                    String opPayerDesc = m_refCodeOpPayerMap.get(opPayerCode).getDescription();
                    if (!StringUtils.isEmpty(opPayerDesc)) {
                        opPayer = opPayerDesc;
                    }
                }
                m_grid.set(FIELD_OP_PAYER, !StringUtils.isEmpty(opInd) ? opPayer : CONST_EMPTY);
                m_grid.set(FIELD_HC_FACTOR, stdHcFactor);
                m_grid.set(FIELD_STD_MST_START_DATE, effStartDateForStd);
                m_grid.set(FIELD_STD_MST_END_DATE, effEndDateForStd);

                // if report is by assignment - process assignments at section/student level
                if (m_byAssign) {
                    int assignCtPbUnder21RegStd = CONST_ZERO.intValue();
                    int assignCtPbUnder21HcStd = CONST_ZERO.intValue();
                    int assignCtOpUnder21Std = CONST_ZERO.intValue();
                    int assignCtPbOver21Std = CONST_ZERO.intValue();
                    int assignCtOpOver21Std = CONST_ZERO.intValue();

                    // get assignment count
                    int assignCtFt = CONST_ZERO.intValue();
                    int assignCtPt = CONST_ZERO.intValue();
                    if (m_assignCtToRegTypeToMstOidStdOid.get(mstOidStdOid) != null) {
                        Map<String, Integer> assignCtByRegType = m_assignCtToRegTypeToMstOidStdOid.get(mstOidStdOid);
                        assignCtFt = assignCtByRegType.get(OntarioAlias.ENR_REG_TYPE_FT);
                        assignCtPt = assignCtByRegType.get(OntarioAlias.ENR_REG_TYPE_PT);
                    }
                    int assignCt = assignCtFt + assignCtPt;

                    // accumulate assignments based on student type
                    if ((StringUtils.isEmpty(opInd)) && (!stdAdultInd)) {
                        assignCtPbUnder21RegMst += assignCt;
                        assignCtPbUnder21RegStd = assignCt;
                    } else if ((!StringUtils.isEmpty(opInd)) && (!stdAdultInd)) {
                        assignCtOpUnder21Mst += assignCt;
                        assignCtOpUnder21Std = assignCt;
                    } else if ((StringUtils.isEmpty(opInd)) && (stdAdultInd)) {
                        assignCtPbOver21Mst += assignCt;
                        assignCtPbOver21Std = assignCt;
                    } else if ((!StringUtils.isEmpty(opInd)) && (stdAdultInd)) {
                        assignCtOpOver21Mst += assignCt;
                        assignCtOpOver21Std = assignCt;
                    }

                    // set assignment grid fields
                    m_grid.set(FIELD_ASSIGN_CT_PB_UNDER21_REG,
                            Integer.valueOf(assignCtPbUnder21RegStd));
                    m_grid.set(FIELD_ASSIGN_CT_PB_UNDER21_HC,
                            Integer.valueOf(assignCtPbUnder21HcStd));
                    m_grid.set(FIELD_ASSIGN_CT_OP_UNDER21, Integer.valueOf(assignCtOpUnder21Std));
                    m_grid.set(FIELD_ASSIGN_CT_PB_OVER21, Integer.valueOf(assignCtPbOver21Std));
                    m_grid.set(FIELD_ASSIGN_CT_OP_OVER21, Integer.valueOf(assignCtOpOver21Std));
                }

                // if report is by date - process attendance at section/student level
                if (m_byDate) {
                    int attScheduledCtPbUnder21Std = CONST_ZERO.intValue();
                    int attScheduledCtOpUnder21Std = CONST_ZERO.intValue();
                    int attScheduledCtPbOver21Std = CONST_ZERO.intValue();
                    int attScheduledCtOpOver21Std = CONST_ZERO.intValue();
                    int attContactCtPbUnder21Std = CONST_ZERO.intValue();
                    int attContactCtOpUnder21Std = CONST_ZERO.intValue();
                    int attContactCtPbOver21Std = CONST_ZERO.intValue();
                    int attContactCtOpOver21Std = CONST_ZERO.intValue();
                    List<String> attListLocalizedOut = new ArrayList<String>();
                    for (int i = 0; i < CONST_ATT_CONTACT_LIST_MAX_CT; i++) {
                        attListLocalizedOut.add(CONST_EMPTY);
                    }

                    // get class att list to set class scheduled count
                    List<String> attListLocalized = new ArrayList<String>();
                    // get saved list if class att was found
                    if (m_attListToMstOidStdOidMap.get(mstOidStdOid) != null) {
                        attListLocalized = m_attListToMstOidStdOidMap.get(mstOidStdOid);
                    } else {
                        // else initialize class att list with spaces
                        for (int i = 0; i < classSessionsCtForMstOid; i++) {
                            attListLocalized.add(CONST_EMPTY);
                        }
                    }

                    // check if not enrolled on class date for student
                    // get scheduled and contact count while looping through
                    int attContactCt = CONST_ZERO.intValue();
                    int attOutputCt = CONST_ZERO.intValue();
                    for (PlainDate classDt : classDtListForMstOid) {
                        int indexClassDt = classDtListForMstOid.indexOf(classDt);
                        // check if class date is in student effective dates for class
                        if (stdEnrStartEndDatesForMst.isDateInStdMstEnrDates(stdOid, classDt)) {
                            // accumulate contact count
                            if (attListLocalized.get(indexClassDt).equals(CONST_ATT_CODE_ISP_PRESENT_OUT)) {
                                attContactCt++;
                            }
                            // set output attendance value
                            if ((!StringUtils.isEmpty(attListLocalized.get(indexClassDt)))
                                    && (attOutputCt < CONST_ATT_CONTACT_LIST_MAX_CT)) {
                                attListLocalizedOut.set(attOutputCt, attListLocalized.get(indexClassDt));
                                attOutputCt++;
                            }
                        }
                    }

                    // accumulate attendance based on student type
                    if ((StringUtils.isEmpty(opInd)) && (!stdAdultInd)) {
                        attScheduledCtPbUnder21Mst += attScheduledCtMst;
                        attScheduledCtPbUnder21Std = attScheduledCtMst;
                        attContactCtPbUnder21Mst += attContactCt;
                        attContactCtPbUnder21Std = attContactCt;
                    } else if ((!StringUtils.isEmpty(opInd)) && (!stdAdultInd)) {
                        attScheduledCtOpUnder21Mst += attScheduledCtMst;
                        attScheduledCtOpUnder21Std = attScheduledCtMst;
                        attContactCtOpUnder21Mst += attContactCt;
                        attContactCtOpUnder21Std = attContactCt;
                    } else if ((StringUtils.isEmpty(opInd)) && (stdAdultInd)) {
                        attScheduledCtPbOver21Mst += attScheduledCtMst;
                        attScheduledCtPbOver21Std = attScheduledCtMst;
                        attContactCtPbOver21Mst += attContactCt;
                        attContactCtPbOver21Std = attContactCt;
                    } else if ((!StringUtils.isEmpty(opInd)) && (stdAdultInd)) {
                        attScheduledCtOpOver21Mst += attScheduledCtMst;
                        attScheduledCtOpOver21Std = attScheduledCtMst;
                        attContactCtOpOver21Mst += attContactCt;
                        attContactCtOpOver21Std = attContactCt;
                    }

                    // set attendance grid fields
                    m_grid.set(FIELD_CLASS_DTL_PAGE, Integer.valueOf(1));
                    m_grid.set(FIELD_CLASS_DTL_PAGE_START, Integer.valueOf(0));
                    m_grid.set(FIELD_ATT_CLASS_CONTACT_LIST, attListLocalizedOut);
                    m_grid.set(FIELD_ATT_SCHEDULED_CT_PB_UNDER21, Integer.valueOf(attScheduledCtPbUnder21Std));
                    m_grid.set(FIELD_ATT_SCHEDULED_CT_OP_UNDER21, Integer.valueOf(attScheduledCtOpUnder21Std));
                    m_grid.set(FIELD_ATT_SCHEDULED_CT_PB_OVER21, Integer.valueOf(attScheduledCtPbOver21Std));
                    m_grid.set(FIELD_ATT_SCHEDULED_CT_OP_OVER21, Integer.valueOf(attScheduledCtOpOver21Std));
                    m_grid.set(FIELD_ATT_CONTACT_CT_PB_UNDER21, Integer.valueOf(attContactCtPbUnder21Std));
                    m_grid.set(FIELD_ATT_CONTACT_CT_OP_UNDER21, Integer.valueOf(attContactCtOpUnder21Std));
                    m_grid.set(FIELD_ATT_CONTACT_CT_PB_OVER21, Integer.valueOf(attContactCtPbOver21Std));
                    m_grid.set(FIELD_ATT_CONTACT_CT_OP_OVER21, Integer.valueOf(attContactCtOpOver21Std));
                    m_grid.set(FIELD_STUDENT_NUMBER, Integer.valueOf(stdCt));
                }
            }
            // end student processing

            // if report is by assignment - process assignments at section level
            if (m_byAssign) {
                // save assignment total counts to mst oid
                m_assignCtPbUnder21RegToMstOidMap.put(mstOid, Integer.valueOf(assignCtPbUnder21RegMst));
                m_assignCtPbUnder21HcToMstOidMap.put(mstOid, Integer.valueOf(assignCtPbUnder21HcMst));
                m_assignCtOpUnder21ToMstOidMap.put(mstOid, Integer.valueOf(assignCtOpUnder21Mst));
                m_assignCtPbOver21ToMstOidMap.put(mstOid, Integer.valueOf(assignCtPbOver21Mst));
                m_assignCtOpOver21ToMstOidMap.put(mstOid, Integer.valueOf(assignCtOpOver21Mst));
            }

            // if report is by date - process attendance at section level
            if (m_byDate) {
                // save attendance total counts to mst oid
                m_attRequiredCtToMstOidMap.put(mstOid, Integer.valueOf(attRequiredCtMst));
                m_attScheduledCtPbUnder21ToMstOidMap.put(mstOid, Integer.valueOf(attScheduledCtPbUnder21Mst));
                m_attScheduledCtOpUnder21ToMstOidMap.put(mstOid, Integer.valueOf(attScheduledCtOpUnder21Mst));
                m_attScheduledCtPbOver21ToMstOidMap.put(mstOid, Integer.valueOf(attScheduledCtPbOver21Mst));
                m_attScheduledCtOpOver21ToMstOidMap.put(mstOid, Integer.valueOf(attScheduledCtOpOver21Mst));
                m_attContactCtPbUnder21ToMstOidMap.put(mstOid, Integer.valueOf(attContactCtPbUnder21Mst));
                m_attContactCtOpUnder21ToMstOidMap.put(mstOid, Integer.valueOf(attContactCtOpUnder21Mst));
                m_attContactCtPbOver21ToMstOidMap.put(mstOid, Integer.valueOf(attContactCtPbOver21Mst));
                m_attContactCtOpOver21ToMstOidMap.put(mstOid, Integer.valueOf(attContactCtOpOver21Mst));
            }
        } // end mst processing

        // add parameters for assignment/pupil maps
        addParameter(REPORT_ASSIGN_REQUIRED_CT_TO_MST_MAP, m_assignTotToMstOidMap);
        addParameter(REPORT_ASSIGN_CT_PB_UNDER21_REG_TO_MST_MAP, m_assignCtPbUnder21RegToMstOidMap);
        addParameter(REPORT_ASSIGN_CT_PB_UNDER21_HC_TO_MST_MAP, m_assignCtPbUnder21HcToMstOidMap);
        addParameter(REPORT_ASSIGN_CT_OP_UNDER21_TO_MST_MAP, m_assignCtOpUnder21ToMstOidMap);
        addParameter(REPORT_ASSIGN_CT_PB_OVER21_TO_MST_MAP, m_assignCtPbOver21ToMstOidMap);
        addParameter(REPORT_ASSIGN_CT_OP_OVER21_TO_MST_MAP, m_assignCtOpOver21ToMstOidMap);
        addParameter(REPORT_ATT_REQUIRED_CT_TO_MST_MAP, m_attRequiredCtToMstOidMap);
        addParameter(REPORT_ATT_SCHEDULED_CT_PB_UNDER21_TO_MST_MAP, m_attScheduledCtPbUnder21ToMstOidMap);
        addParameter(REPORT_ATT_SCHEDULED_CT_OP_UNDER21_TO_MST_MAP, m_attScheduledCtOpUnder21ToMstOidMap);
        addParameter(REPORT_ATT_SCHEDULED_CT_PB_OVER21_TO_MST_MAP, m_attScheduledCtPbOver21ToMstOidMap);
        addParameter(REPORT_ATT_SCHEDULED_CT_OP_OVER21_TO_MST_MAP, m_attScheduledCtOpOver21ToMstOidMap);
        addParameter(REPORT_ATT_CONTACT_CT_PB_UNDER21_TO_MST_MAP, m_attContactCtPbUnder21ToMstOidMap);
        addParameter(REPORT_ATT_CONTACT_CT_OP_UNDER21_TO_MST_MAP, m_attContactCtOpUnder21ToMstOidMap);
        addParameter(REPORT_ATT_CONTACT_CT_PB_OVER21_TO_MST_MAP, m_attContactCtPbOver21ToMstOidMap);
        addParameter(REPORT_ATT_CONTACT_CT_OP_OVER21_TO_MST_MAP, m_attContactCtOpOver21ToMstOidMap);
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.reports.on.EnrRegByCourseData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // get school year dates (schedule) based on school and selected school year
        X2Criteria schCriteria = new X2Criteria();
        schCriteria.addEqualTo(Schedule.COL_SCHOOL_OID, getSchool().getOid());
        schCriteria.addEqualTo(Schedule.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        schCriteria.addEqualTo(Schedule.COL_BUILD_SCENARIO_INDICATOR, Boolean.FALSE);
        QueryByCriteria schQuery = new QueryByCriteria(Schedule.class, schCriteria);
        Schedule sch = getBroker().getBeanByQuery(schQuery);
        m_contextDateDec31 = PlainDate
                .fromString(Integer.valueOf(sch.getDistrictContext().getSchoolYear() - 1) + "-12-31");

        m_stdAdultIndByStdOid = new HashMap<String, Boolean>();

        // loads reference table for op payer description to code map
        DataDictionaryField sskOpPayer =
                m_dictionary.findDataDictionaryFieldByAlias(OntarioAlias.ALIAS_ENR_FUNDING_PAYER);
        String enrOpPayerRtbOid = sskOpPayer.getDataFieldConfig().getReferenceTableOid();
        m_refCodeOpPayerMap = loadReferenceCodesMap(enrOpPayerRtbOid);

        // additional localization for this report
        if (m_locForLang.getSystemLocale().toString().equals(OntarioAlias.CONST_LOCALE_ENGLISH)) {
            SimpleDateFormat shortDateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_ENG);
            addParameter(REPORT_SHORT_DATE_FMT_OUTPUT, shortDateFormatOut);
        } else {
            SimpleDateFormat shortDateFormatOut = new SimpleDateFormat(CONST_DATE_FMT_STR_FR);
            addParameter(REPORT_SHORT_DATE_FMT_OUTPUT, shortDateFormatOut);
        }
    }

    /**
     * Loads reference codes by code for reference table
     *
     * @param rtbOid
     *
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> loadReferenceCodesMap(String rtbOid) {
        Map<String, ReferenceCode> rcdByCodeMap = new HashMap<String, ReferenceCode>();

        // create criteria for student contact
        X2Criteria rcdCriteria = new X2Criteria();

        // for students in student criteria
        rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);

        // select query
        QueryByCriteria rcdQuery = new QueryByCriteria(ReferenceCode.class, rcdCriteria);

        rcdByCodeMap = getBroker().getMapByQuery(rcdQuery, ReferenceCode.COL_CODE, 1024);

        // return map
        return rcdByCodeMap;
    }

    /**
     * Returns the value of a boolean is student adult indicator
     *
     * @param std SisStudent
     * @return Boolean
     */
    protected Boolean isStdAdult(SisStudent std) {
        Boolean isAdult = Boolean.FALSE;

        SisPerson psn = std.getPerson();
        if (psn.getAgeAsOfDate(m_contextDateDec31) >= OntarioAlias.CONST_ADULT_AGE.intValue()) {
            isAdult = Boolean.TRUE;
        }

        return isAdult;
    }

    /**
     * Get end date for student for class
     * -defaults to class end date
     *
     * @param enrStartEndDatesForMst - EnrRegStudentStartEndDatesForSection
     * @param stdOid - student oid
     *
     * @return PlainDate
     */
    protected PlainDate getEffEndDateForStdForMst(EnrRegStudentStartEndDatesForSection enrStartEndDatesForMst,
                                                  String stdOid) {
        List<PlainDate> stdStartDatesList = null;
        List<PlainDate> stdEndDatesList = null;
        if (enrStartEndDatesForMst.m_stdStartDatesListToStdOid.get(stdOid) != null) {
            stdStartDatesList = enrStartEndDatesForMst.m_stdStartDatesListToStdOid.get(stdOid);
            stdEndDatesList = enrStartEndDatesForMst.m_stdEndDatesListToStdOid.get(stdOid);
        }

        PlainDate effEndDateStdMst = null;

        // return null if student dates not found
        if (stdStartDatesList == null) {
            return enrStartEndDatesForMst.m_mstStartDate;
        }

        // loop thru date ranges and return true if pat record is valid
        int indexDates = stdStartDatesList.size() - 1;
        while ((indexDates >= 0) && (effEndDateStdMst == null)) {
            // if not dropped or if drop date after section end date return section end date
            PlainDate indexStdEndDate = stdEndDatesList.get(indexDates);
            if ((indexStdEndDate == null) || indexStdEndDate.after(enrStartEndDatesForMst.m_mstEndDate)) {
                effEndDateStdMst = enrStartEndDatesForMst.m_mstEndDate;
            } else {
                effEndDateStdMst = indexStdEndDate;
            }

            indexDates++;
        }

        return effEndDateStdMst;
    }
}
