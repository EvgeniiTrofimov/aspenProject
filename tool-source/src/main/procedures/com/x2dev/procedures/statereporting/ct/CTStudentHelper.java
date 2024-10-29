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
package com.x2dev.procedures.statereporting.ct;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentContextAttributes;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import jersey.repackaged.com.google.common.collect.ImmutableSet;

/**
 * Class for common student selection methods.
 *
 * @author Follett Software Company
 */
public class CTStudentHelper extends StudentHistoryHelper {
    /**
     * The Class GradeMatcher.
     */
    public class GradeMatcher {
        private int m_maxGradeLevel;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;
        private TreeMap m_sortedGradeLevels;

        /**
         * Instantiates a new grade matcher.
         */
        public GradeMatcher() {
            DataDictionary dictionary = getData().getDataDictionary();
            ModelProperty prop = new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    getData().getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                m_referenceGradeCodeMap = referenceTable.getCodeMap();
            } else {
                throw new IllegalStateException("Grade Code Reference table cannot be built");
            }

            List<String> removedCodes = new LinkedList();
            for (Entry<String, ReferenceCode> entry : m_referenceGradeCodeMap.entrySet()) {
                if (entry.getValue().getDisabledIndicator()) {
                    removedCodes.add(entry.getKey());
                }
            }
            for (String code : removedCodes) {
                m_referenceGradeCodeMap.remove(code);
            }
            ModelBroker broker = new ModelBroker(getData().getPrivilegeSet());
            m_sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            m_maxGradeLevel = StudentManager.getMaxGradeLevel(broker);
        }

        /**
         * Gets the reference code.
         *
         * @param yog the yog
         * @return the reference code
         */
        public ReferenceCode getReferenceCode(int yog) {
            ReferenceCode gradeCode = null;
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(m_maxGradeLevel, yog,
                    getData().getCurrentContext().getSchoolYear(), m_sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
            return gradeCode;
        }

        /**
         * Gets the reference code.
         *
         * @param code the code
         * @return the reference code
         */
        public ReferenceCode getReferenceCode(String code) {
            ReferenceCode gradeCode = m_referenceGradeCodeMap.get(code);
            return gradeCode;
        }

    }

    /**
     * The Class SchoolCalendarHelper.
     */
    class SchoolCalendarHelper {
        private static final String SCHOOL_CALENDAR_KEY_DELIMITER = "|";

        private Map<String, SchoolCalendar> m_mostCommonCalendar = new HashMap();
        private Map<String, SchoolCalendarInfo> m_schoolCalendarInfos = new HashMap();
        private Comparator<SisSchoolCalendarDate> m_schoolCalendarDatesComparator;
        private Map<String, Collection<SchoolCalendar>> m_schoolCalendars;

        /**
         * Gets the most common calendar code.
         *
         * @param ctx DistrictSchoolYearContext
         * @param school the school
         * @return the most common calendar code
         */
        public SchoolCalendar getMostCommonCalendarCode(DistrictSchoolYearContext ctx, SisSchool school) {
            SchoolCalendar value = null;
            String calKey = school.getOid() + ctx.getOid();
            if (m_mostCommonCalendar.containsKey(calKey)) {
                value = m_mostCommonCalendar.get(calKey);
            } else {
                Collection<SchoolCalendar> calendars = getSchoolCalendars(school.getOid(), ctx.getOid());
                if (calendars != null && !calendars.isEmpty()) {
                    value = calendars.iterator().next();
                    if (calendars.size() > 1) {
                        List<String> calendarCodes = new LinkedList();
                        for (SchoolCalendar calendar : calendars) {
                            calendarCodes.add(calendar.getCalendarId());
                        }
                        if (ctx.getOid().equals(m_data.getCurrentContext().getOid())) {
                            X2Criteria criteria = new X2Criteria();
                            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, school.getOid());
                            criteria.addIn(SisStudent.COL_CALENDAR_CODE, calendarCodes);
                            String[] columns = new String[] {SisStudent.COL_CALENDAR_CODE, "count(*)"};
                            ReportQueryByCriteria query =
                                    new ReportQueryByCriteria(SisStudent.class, columns, criteria);
                            query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
                            query.addOrderByDescending("count(*)");
                            ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);
                            try {
                                while (iterator.hasNext()) {
                                    Object[] row = (Object[]) iterator.next();
                                    String calendarCode = (String) row[0];

                                    if (!StringUtils.isEmpty(calendarCode)) {
                                        for (SchoolCalendar calendar : calendars) {
                                            if (calendarCode.equals(calendar.getCalendarId())) {
                                                value = calendar;
                                                break;
                                            }
                                        }
                                    }
                                }
                            } finally {
                                iterator.close();
                            }
                        } else {
                            X2Criteria criteria = new X2Criteria();
                            criteria.addEqualTo(StudentContextAttributes.COL_SCHOOL_OID, school.getOid());
                            criteria.addIn(StudentContextAttributes.COL_CALENDAR_CODE, calendarCodes);
                            criteria.addEqualTo(StudentContextAttributes.COL_CONTEXT_OID, ctx.getOid());
                            String[] columns = new String[] {StudentContextAttributes.COL_CALENDAR_CODE, "count(*)"};
                            ReportQueryByCriteria query =
                                    new ReportQueryByCriteria(StudentContextAttributes.class, columns, criteria);
                            query.addGroupBy(StudentContextAttributes.COL_CALENDAR_CODE);
                            query.addOrderByDescending("count(*)");
                            ReportQueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query);
                            try {
                                while (iterator.hasNext()) {
                                    Object[] row = (Object[]) iterator.next();
                                    String calendarCode = (String) row[0];
                                    if (!StringUtils.isEmpty(calendarCode)) {
                                        for (SchoolCalendar calendar : calendars) {
                                            if (calendarCode.equals(calendar.getCalendarId())) {
                                                value = calendar;
                                                break;
                                            }
                                        }
                                    }
                                }
                            } finally {
                                iterator.close();
                            }

                        }
                    }
                }
                m_mostCommonCalendar.put(calKey, value);
            }
            return value;
        }

        /**
         * Gets the school calendar dates comparator.
         *
         * @return the school calendar dates comparator
         */
        public Comparator<SisSchoolCalendarDate> getSchoolCalendarDatesComparator() {
            if (m_schoolCalendarDatesComparator == null) {
                m_schoolCalendarDatesComparator = new Comparator<SisSchoolCalendarDate>() {
                    @Override
                    public int compare(SisSchoolCalendarDate o1, SisSchoolCalendarDate o2) {
                        PlainDate date1 = o1.getDate();
                        PlainDate date2 = o2.getDate();
                        if (date1 == null) {
                            return -1;
                        } else if (date2 == null) {
                            return 1;
                        }
                        return date1.compareTo(date2);
                    }
                };
            }
            return m_schoolCalendarDatesComparator;
        }

        /**
         * Gets the school calendar info.
         *
         * @param ctx DistrictSchoolYearContext
         * @param school the school
         * @param calendarCode the calendar code
         * @return the school calendar info
         */
        public SchoolCalendarInfo getSchoolCalendarInfo(DistrictSchoolYearContext ctx,
                                                        SisSchool school,
                                                        String calendarCode) {
            String key = getSchoolCalendarInfoKey(ctx.getOid(), school.getOid(), calendarCode);
            SchoolCalendarInfo info = m_schoolCalendarInfos.get(key);
            if (info == null) {
                info = new SchoolCalendarInfo(ctx, school, calendarCode);
                m_schoolCalendarInfos.put(key, info);
            }
            return info;
        }

        /**
         * Gets the school calendars.
         *
         * @param schoolOid the school oid
         * @param ctxOid String
         * @return the school calendars
         */
        public Collection<SchoolCalendar> getSchoolCalendars(String schoolOid, String ctxOid) {
            String calendarsKey = schoolOid + ctxOid;
            if (m_schoolCalendars == null || !m_schoolCalendars.containsKey(calendarsKey)) {
                m_schoolCalendars = new HashMap<String, Collection<SchoolCalendar>>();
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, schoolOid);
                criteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, ctxOid);
                BeanQuery query = new BeanQuery(SchoolCalendar.class, criteria);
                Collection<SchoolCalendar> schoolCalendars =
                        m_data.getBroker().getCollectionByQuery(query);
                m_schoolCalendars.put(calendarsKey, schoolCalendars);
            }
            return m_schoolCalendars.get(calendarsKey);
        }

        /**
         * Gets the school calendar info key.
         *
         * @param ctxOid the ctx oid
         * @param sklOid the skl oid
         * @param calendarCode the calendar code
         * @return the school calendar info key
         */
        private String getSchoolCalendarInfoKey(String ctxOid, String sklOid, String calendarCode) {
            return SCHOOL_CALENDAR_KEY_DELIMITER + ctxOid +
                    SCHOOL_CALENDAR_KEY_DELIMITER + sklOid +
                    SCHOOL_CALENDAR_KEY_DELIMITER + calendarCode + SCHOOL_CALENDAR_KEY_DELIMITER;
        }
    }

    /**
     * The Class SchoolCalendarInfo.
     */
    class SchoolCalendarInfo {
        private Collection<SisSchoolCalendarDate> m_calendarDates;
        private ImmutableSet<PlainDate> m_inSessionDates;

        /**
         * Instantiates a new school calendar info.
         *
         * @param context the context
         * @param school the school
         * @param calendarCode the calendar code
         */
        public SchoolCalendarInfo(DistrictSchoolYearContext context, SisSchool school, String calendarCode) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, context.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, school.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_CALENDAR_ID, calendarCode);
            BeanQuery query = new BeanQuery(SchoolCalendarDate.class, criteria);
            query.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
            m_calendarDates = m_data.getBroker().getCollectionByQuery(query);
        }

        /**
         * get the first insesssion date for this span.
         *
         * @return the first in session date
         */
        public PlainDate getFirstInSessionDate() {
            PlainDate value = null;
            if (m_inSessionDates == null) {
                m_inSessionDates = getInSessionDates();
            }
            if (m_inSessionDates != null && !m_inSessionDates.isEmpty()) {
                value = m_inSessionDates.iterator().next();
            }
            return value;
        }

        /**
         * Gets the in session dates.
         *
         * @return the in session dates
         */
        public ImmutableSet<PlainDate> getInSessionDates() {
            if (m_inSessionDates == null || m_inSessionDates.isEmpty()) {
                List<PlainDate> dates = new ArrayList(m_calendarDates.size());
                for (SisSchoolCalendarDate calendarDate : m_calendarDates) {
                    if (calendarDate.getInSessionIndicator()) {
                        dates.add(calendarDate.getDate());
                    }
                }
                m_inSessionDates = ImmutableSet.copyOf(dates);
            }
            return m_inSessionDates;
        }

        /**
         * Gets the last in session date.
         *
         * @return the last in session date
         */
        public PlainDate getLastInSessionDate() {
            PlainDate value = null;
            if (m_inSessionDates == null || m_inSessionDates.isEmpty()) {
                m_inSessionDates = getInSessionDates();
            }
            if (m_inSessionDates != null && !m_inSessionDates.isEmpty()) {
                for (PlainDate date : m_inSessionDates) {
                    value = date;
                }
            }
            return value;
        }

        /**
         * Find the first insession date on or after date.
         *
         * @param testDate the test date
         * @param onOrAfter if true search date. If false, result must be after date
         * @return the next in session date
         */
        public PlainDate getNextInSessionDate(PlainDate testDate, boolean onOrAfter) {
            PlainDate value = null;
            if (m_inSessionDates == null) {
                m_inSessionDates = getInSessionDates();
            }

            if (m_inSessionDates != null && !m_inSessionDates.isEmpty()) {
                for (PlainDate date : m_inSessionDates) {
                    if (onOrAfter && !date.before(testDate)) {
                        value = date;
                        break;
                    } else if (!onOrAfter && date.after(testDate)) {
                        value = date;
                        break;
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is valid.
         *
         * @return true, if is valid
         */
        public boolean isValid() {
            return !m_calendarDates.isEmpty();
        }
    }

    /**
     * The Class StudentInfo.
     */
    public class StudentInfo {
        private String m_calCodeCurrent;
        private String m_calCodePrior;
        private Boolean m_registeredEndOfYear;
        private HashMap<PlainDate, SisSchool> m_schools;
        private SisStudent m_student;

        /**
         * Instantiates a new student info.
         *
         * @param student the student
         */
        public StudentInfo(SisStudent student) {
            m_student = student;
        }

        /**
         * Format student name according to FL standard
         * http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-175425.pdf
         *
         * @return the string
         */
        public String formatStudentLegalName() {
            StringBuilder sb = new StringBuilder(m_student.getPerson().getLastName());
            String appendage = m_student.getPerson().getNameSuffixCode();
            if (!StringUtils.isEmpty(appendage)) {
                sb.append(" ");
                sb.append(appendage);
            }
            String fname = m_student.getPerson().getFirstName();
            if (!StringUtils.isEmpty(fname)) {
                sb.append(" ");
                sb.append(fname);
            }
            String mname = m_student.getPerson().getMiddleName();
            if (!StringUtils.isEmpty(mname)) {
                sb.append(" ");
                sb.append(mname);
            }
            return sb.length() <= MAX_LEGAL_NAME_LEN ? sb.toString() : sb.substring(0, MAX_LEGAL_NAME_LEN);
        }

        /**
         * Gets the enrollment spans.
         *
         * @param limit the limit
         * @return the student enrollment spans
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentEnrollmentSpans(com.follett.fsc.core.k12.beans.Student,
         *      boolean)
         */
        public List<StudentEnrollmentSpan> getEnrollmentSpans(boolean limit) {
            List<StudentEnrollmentSpan> spans = getStudentEnrollmentSpans(m_student, limit);
            Iterator<StudentEnrollmentSpan> iterator = spans.iterator();
            while (iterator.hasNext()) {
                StudentEnrollmentSpan span = iterator.next();
                if (span.getSchool() == null) {
                    iterator.remove();
                }
            }
            return spans;
        }

        /**
         * Gets the current cal code.
         *
         * @return String
         */
        public String getCurrentCalCode() {
            if (StringUtils.isEmpty(m_calCodeCurrent)) {
                String currentCalCode = m_student.getCalendarCode();
                if (StringUtils.isEmpty(currentCalCode)) {
                    SchoolCalendar currentCal =
                            getCalendarHelper().getMostCommonCalendarCode(m_data.getCurrentContext(),
                                    m_student.getSchool());
                    if (currentCal != null) {
                        currentCal.getCalendarId();
                    }
                }
                if (StringUtils.isEmpty(currentCalCode)) {
                    currentCalCode = "Standard";
                }
                m_calCodeCurrent = currentCalCode;
            }
            return m_calCodeCurrent;
        }

        /**
         * Return calendar code for the previous year.
         *
         * @return String
         */
        public String getPriorCalCode() {
            if (StringUtils.isEmpty(m_calCodePrior)) {
                SisSchool priorSchool = null;
                String priorCalCode = null;
                DistrictSchoolYearContext priorCtx = getPriorCtx();
                Collection<StudentContextAttributes> sxaList = m_student.getContextAttributes();
                for (StudentContextAttributes sxa : sxaList) {
                    if (priorCtx.getOid().equals(sxa.getDistrictContext().getOid())) {
                        priorCalCode = sxa.getCalendarCode();
                        priorSchool = sxa.getSchool();
                        break;
                    }
                }
                if (StringUtils.isEmpty(priorCalCode)) {
                    SchoolCalendar priorCalendar = getCalendarHelper().getMostCommonCalendarCode(priorCtx,
                            priorSchool == null ? m_student.getSchool() : priorSchool);
                    if (priorCalendar != null) {
                        priorCalCode = priorCalendar.getCalendarId();
                    }
                }
                m_calCodePrior = !StringUtils.isEmpty(priorCalCode) ? priorCalCode : "Standard";
            }
            return m_calCodePrior;
        }

        /**
         * Loads all the student's SASID records for the whole current CTX.
         *
         * @return the student info
         */
        public List<StudentSasidRecord> getSasidRecords() {
            List<StudentSasidRecord> recordsList = new ArrayList<StudentSasidRecord>();
            List<StudentEnrollmentSpan> spans = getEnrollmentSpans(false);
            SchoolCalendarInfo currentCtxCalInfo =
                    getCalendarHelper().getSchoolCalendarInfo(m_data.getCurrentContext(), m_student.getSchool(),
                            getCurrentCalCode());
            // Get calendar information for the prior school year.
            StudentEnrollmentSpan previousSpan = null;
            PlainDate previousSpanLastEnrolledDate = null;
            for (StudentEnrollmentSpan span : spans) {
                StudentEnrollment firstActiveEnr = span.getFirstActiveEnrollment();
                if (firstActiveEnr == null) {
                    // Enrollment span must have active record to be considered
                    continue;
                }
                PlainDate spanFirstEnrolledDate = span.getFirstActiveEnrollment() == null ? null
                        : span.getFirstActiveEnrollment().getEnrollmentDate()
                                .after(m_data.getCurrentContext().getStartDate())
                                        ? span.getFirstActiveEnrollment().getEnrollmentDate()
                                        : m_data.getCurrentContext().getStartDate();
                if (previousSpan == null) {
                    // create
                    previousSpan = span;
                    previousSpanLastEnrolledDate = getSpanExitDate(this, span);
                    if (isSpanForCurrentYear(span, m_data.getCurrentContext())) {
                        if (isRegisteredEndOfYear().booleanValue()) {
                            // Create SASID record type 'C' based on first active enrollment
                            // and first active date
                            StudentSasidRecord recordToAdd = new StudentSasidRecord(this);
                            recordToAdd.setRecordType(SASID_TYPE_C);
                            recordToAdd.setEntryDate(spanFirstEnrolledDate);
                            recordToAdd.setExitDate(getPriorCtx().getEndDate());
                            String gradeLevel = getCalculatedGradeLevel(m_student, span);
                            if (!StringUtils.isEmpty(gradeLevel)) {
                                recordToAdd.setGradeLevel(gradeLevel);
                            }
                            recordToAdd.setSchool(firstActiveEnr.getSchool());
                            recordToAdd.setFacilityCode1(getCalculatedFacilityCode1(firstActiveEnr));
                            recordToAdd.setFacilityCode2(getCalculatedFacilityCode2(firstActiveEnr));
                            recordToAdd.setEntryEnr(firstActiveEnr);
                            if (recordToAdd.getSchool() != null
                                    && !getExcludedSchools().contains(recordToAdd.getSchool().getOid())) {
                                recordsList.add(recordToAdd);
                                recordToAdd.calculateAttendance(span, currentCtxCalInfo.getInSessionDates(),
                                        getStudentAttendances(m_student.getOid()));
                            }
                        } else {
                            // Create SASID record type 'R' based on first active enrollment
                            // and first active date
                            StudentSasidRecord recordToAdd = new StudentSasidRecord(this);
                            recordToAdd.setRecordType(SASID_TYPE_R);
                            recordToAdd.setEntryDate(spanFirstEnrolledDate);
                            String gradeLevel = getCalculatedGradeLevel(m_student, span);
                            if (!StringUtils.isEmpty(gradeLevel)) {
                                recordToAdd.setGradeLevel(gradeLevel);
                            }
                            if (firstActiveEnr != null) {
                                recordToAdd.setSchool(firstActiveEnr.getSchool());
                                recordToAdd.setEntryEnr(firstActiveEnr);
                            }
                            recordToAdd.setFacilityCode1(getCalculatedFacilityCode1(firstActiveEnr));
                            recordToAdd.setFacilityCode2(getCalculatedFacilityCode2(firstActiveEnr));
                            if (recordToAdd.getSchool() != null
                                    && !getExcludedSchools().contains(recordToAdd.getSchool().getOid())) {
                                recordsList.add(recordToAdd);
                                recordToAdd.calculateAttendance(span, currentCtxCalInfo.getInSessionDates(),
                                        getStudentAttendances(m_student.getOid()));
                            }
                        }
                    }
                } else {
                    if (isSpanForCurrentYear(span, m_data.getCurrentContext())) {
                        boolean leftDistrict = false;
                        if (previousSpanLastEnrolledDate != null) {
                            SchoolCalendarInfo priorCtxCalInfo =
                                    getCalendarHelper().getSchoolCalendarInfo(getPriorCtx(), m_student.getSchool(),
                                            getPriorCalCode());
                            PlainDate priorCtxLastInSession = priorCtxCalInfo.getLastInSessionDate();
                            if (priorCtxLastInSession != null
                                    && !previousSpanLastEnrolledDate.before(priorCtxLastInSession)) {
                                PlainDate nextInsessionDate =
                                        currentCtxCalInfo.getNextInSessionDate(previousSpanLastEnrolledDate, true);
                                if (nextInsessionDate != null && span.getFirstActiveDate() != null
                                        && nextInsessionDate.before(span.getFirstActiveDate())) {
                                    // Create SASID record type 'U' based on first inactive
                                    // enrollment and first inactive date
                                    StudentEnrollment firstInactiveEnr = previousSpan.getFirstInactiveEnrollment();
                                    StudentSasidRecord recordToAdd = new StudentSasidRecord(this);
                                    recordToAdd.setRecordType(SASID_TYPE_U);
                                    recordToAdd.setExitDate(previousSpanLastEnrolledDate);
                                    leftDistrict = true;
                                    recordToAdd.setSchool(firstInactiveEnr.getSchool());
                                    String exitTypeCode = firstInactiveEnr.getEnrollmentCode();
                                    ReferenceCode exitRefCode = getWithdrawalCode(exitTypeCode);
                                    if (exitRefCode != null) {
                                        exitTypeCode = exitRefCode.getStateCode();
                                        recordToAdd.setExitType(exitTypeCode);
                                    }
                                    String exitStatusCode = firstInactiveEnr.getReasonCode();
                                    exitRefCode = getWithdrawalReason(exitStatusCode);
                                    if (exitRefCode != null) {
                                        exitStatusCode = exitRefCode.getStateCode();
                                        recordToAdd.setExitStatus(exitStatusCode);
                                    }
                                    if (recordToAdd.getSchool() != null
                                            && !getExcludedSchools().contains(recordToAdd.getSchool().getOid())) {
                                        recordToAdd.calculateAttendance(previousSpan,
                                                currentCtxCalInfo.getInSessionDates(),
                                                getStudentAttendances(m_student.getOid()));
                                        recordsList.add(recordToAdd);
                                    }
                                }
                            } else {
                                leftDistrict = true;
                            }
                        }
                        if (leftDistrict) {
                            // Create SASID record type 'R' based on first active enrollment
                            // and first active date
                            StudentSasidRecord recordToAdd = new StudentSasidRecord(this);
                            recordToAdd.setRecordType(SASID_TYPE_R);
                            recordToAdd.setEntryDate(spanFirstEnrolledDate);
                            String gradeLevel = getCalculatedGradeLevel(m_student, span);
                            if (!StringUtils.isEmpty(gradeLevel)) {
                                recordToAdd.setGradeLevel(gradeLevel);
                            }
                            recordToAdd.setSchool(firstActiveEnr.getSchool());
                            recordToAdd.setFacilityCode1(getCalculatedFacilityCode1(firstActiveEnr));
                            recordToAdd.setFacilityCode2(getCalculatedFacilityCode2(firstActiveEnr));
                            recordToAdd.setEntryEnr(firstActiveEnr);
                            if (recordToAdd.getSchool() != null
                                    && !getExcludedSchools().contains(recordToAdd.getSchool().getOid())) {
                                recordsList.add(recordToAdd);
                                recordToAdd.calculateAttendance(span, currentCtxCalInfo.getInSessionDates(),
                                        getStudentAttendances(m_student.getOid()));
                            }
                        } else {
                            // Create SASID record type 'C' based on first active enrollment
                            // and first active date
                            StudentSasidRecord recordToAdd = new StudentSasidRecord(this);
                            recordToAdd.setRecordType(SASID_TYPE_C);
                            recordToAdd.setEntryDate(spanFirstEnrolledDate);
                            if (span.getFirstActiveEnrollment() != null && span.getFirstActiveEnrollment()
                                    .getEnrollmentDate().before(getPriorCtx().getEndDate())) {
                                recordToAdd.setExitDate(getPriorCtx().getEndDate());
                            } else {
                                recordToAdd.setExitDate(previousSpanLastEnrolledDate);
                            }
                            String gradeLevel = getCalculatedGradeLevel(m_student, span);
                            if (!StringUtils.isEmpty(gradeLevel)) {
                                recordToAdd.setGradeLevel(gradeLevel);
                            }
                            if (firstActiveEnr != null) {
                                recordToAdd.setEntryEnr(firstActiveEnr);
                                recordToAdd.setSchool(firstActiveEnr.getSchool());
                            }
                            recordToAdd.setFacilityCode1(getCalculatedFacilityCode1(firstActiveEnr));
                            recordToAdd.setFacilityCode2(getCalculatedFacilityCode2(firstActiveEnr));
                            if (recordToAdd.getSchool() != null
                                    && !getExcludedSchools().contains(recordToAdd.getSchool().getOid())) {
                                recordToAdd.calculateAttendance(span, currentCtxCalInfo.getInSessionDates(),
                                        getStudentAttendances(m_student.getOid()));
                                recordsList.add(recordToAdd);
                            }
                        }
                    }
                    previousSpan = span;
                    previousSpanLastEnrolledDate = getSpanExitDate(this, span);
                }
            }

            // end of spans
            if (previousSpan != null && previousSpan.getFirstInactiveEnrollment() != null) {
                // Create SASID record type 'U' based on first inactive enrollment and first
                // inactive date
                SchoolCalendarInfo priorCtxCalInfo =
                        getCalendarHelper().getSchoolCalendarInfo(getPriorCtx(), m_student.getSchool(),
                                getPriorCalCode());
                PlainDate priorCtxLastInSession = priorCtxCalInfo.getLastInSessionDate();
                if (priorCtxLastInSession != null
                        && !previousSpanLastEnrolledDate.before(priorCtxLastInSession)) {
                    StudentEnrollment firstInactiveEnr = previousSpan.getFirstInactiveEnrollment();
                    StudentSasidRecord recordToAdd = new StudentSasidRecord(this);
                    recordToAdd.setRecordType(SASID_TYPE_U);
                    recordToAdd.setExitDate(previousSpanLastEnrolledDate);
                    SisSchool school = firstInactiveEnr.getSchool();
                    recordToAdd.setSchool(school);
                    String exitTypeCode = firstInactiveEnr.getEnrollmentCode();
                    ReferenceCode exitRefCode = getWithdrawalCode(exitTypeCode);
                    if (exitRefCode != null) {
                        exitTypeCode = exitRefCode.getStateCode();
                        recordToAdd.setExitType(exitTypeCode);
                    }
                    String exitStatusCode = firstInactiveEnr.getReasonCode();
                    exitRefCode = getWithdrawalReason(exitStatusCode);
                    if (exitRefCode != null) {
                        exitStatusCode = exitRefCode.getStateCode();
                        recordToAdd.setExitStatus(exitStatusCode);
                    }
                    if (recordToAdd.getSchool() != null
                            && !getExcludedSchools().contains(recordToAdd.getSchool().getOid())) {
                        recordToAdd.calculateAttendance(previousSpan, currentCtxCalInfo.getInSessionDates(),
                                getStudentAttendances(m_student.getOid()));
                        recordsList.add(recordToAdd);
                    }
                }
            }
            return recordsList;
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Gets the school.
         *
         * @param date the date
         * @return the school
         */
        public SisSchool getSchool(PlainDate date) {
            if (m_schools == null) {
                m_schools = new HashMap();
            }
            SisSchool school = m_schools.get(date);
            if (school == null) {
                StudentEnrollment enroll = getEnrollmentForDate(m_student.getOid(), date,
                        StudentEnrollment.ENTRY + StudentEnrollment.STATUS_CHANGE + StudentEnrollment.YOG_CHANGE);
                if (enroll != null) {
                    school = enroll.getSchool();
                } else {
                    school = m_student.getSchool();
                }
                m_schools.put(date, school);
            }
            return school;
        }

        /**
         * Gets the ssn.
         *
         * @return the ssn
         */
        public String getSSN() {
            String ssn = m_student.getPerson().getPersonId();
            if (!StringUtils.isEmpty(ssn)) {
                ssn = ssn.replaceAll("-", "");
            }
            return ssn;
        }

        /**
         * Return if student is registered on the last day of the prior school year.
         *
         * @return Boolean
         */
        public Boolean isRegisteredEndOfYear() {
            if (m_registeredEndOfYear == null) {
                m_registeredEndOfYear = Boolean.FALSE;
                SchoolCalendarInfo priorCtxCalInfo =
                        getCalendarHelper().getSchoolCalendarInfo(getPriorCtx(), m_student.getSchool(),
                                getPriorCalCode());
                List<StudentEnrollmentSpan> spans = getEnrollmentSpans(false);
                PlainDate priorCtxLastInSession = priorCtxCalInfo.getLastInSessionDate();
                if (priorCtxLastInSession != null && spans != null && !spans.isEmpty()) {
                    for (StudentEnrollmentSpan span : spans) {
                        PlainDate lastEnrolled = getSpanExitDate(this, span);
                        if (lastEnrolled != null && priorCtxLastInSession != null
                                && lastEnrolled.before(priorCtxLastInSession)) {
                            continue;
                        }
                        PlainDate firstEnrolled = span.getFirstActiveEnrollment() == null ? null
                                : span.getFirstActiveEnrollment().getEnrollmentDate();
                        if (firstEnrolled != null && priorCtxLastInSession != null
                                && !firstEnrolled.after(priorCtxLastInSession)) {
                            m_registeredEndOfYear = Boolean.TRUE;
                            break;
                        }
                    }
                }
            }
            return m_registeredEndOfYear;
        }

        /**
         * Calculate facility code 1.
         *
         * @param entryEnr StudentEnrollment
         * @return String
         */
        private String getCalculatedFacilityCode1(StudentEnrollment entryEnr) {
            String facilityCodeState = null;
            if (entryEnr != null && StudentEnrollment.YOG_CHANGE.equals(entryEnr.getEnrollmentType())) {
                entryEnr = getEnrollmentForDate(entryEnr.getStudentOid(), entryEnr.getEnrollmentDate(),
                        StudentEnrollment.ENTRY);
            }
            if (entryEnr != null) {
                String code = (String) entryEnr.getFieldValueByBeanPath(m_fieldEnrSklOverride);
                if (!StringUtils.isEmpty(code)) {
                    String stateCode = null;
                    facilityCodeState = !StringUtils.isEmpty(
                            stateCode = m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklOverride, code))
                                    ? stateCode
                                    : code;
                }
                if (StringUtils.isEmpty(facilityCodeState)) {
                    if (entryEnr.getSchool() != null) {
                        facilityCodeState =
                                (String) entryEnr.getSchool().getFieldValueByBeanPath(m_fieldSklStateId);
                    }
                }
            }
            return !StringUtils.isEmpty(facilityCodeState) ? facilityCodeState
                    : (String) m_student.getSchool().getFieldValueByBeanPath(m_fieldSklStateId);
        }

        /**
         * Calculate facility code 2.
         *
         * @param entryEnr StudentEnrollment
         * @return String
         */
        private String getCalculatedFacilityCode2(StudentEnrollment entryEnr) {
            String facilityCodeState = null;
            if (entryEnr != null && StudentEnrollment.YOG_CHANGE.equals(entryEnr.getEnrollmentType())) {
                entryEnr = getEnrollmentForDate(entryEnr.getStudentOid(), entryEnr.getEnrollmentDate(),
                        StudentEnrollment.ENTRY);
            }
            if (entryEnr != null) {
                String code = (String) entryEnr.getFieldValueByBeanPath(m_fieldEnrSecSkl);
                if (!StringUtils.isEmpty(code)) {
                    String stateCode = null;
                    facilityCodeState = !StringUtils.isEmpty(
                            stateCode = m_data.lookupStateValue(StudentEnrollment.class, m_fieldEnrSecSkl, code))
                                    ? stateCode
                                    : code;
                }
            }
            return facilityCodeState;
        }

        /**
         * Checks if is span for current year.
         *
         * @param span StudentEnrollmentSpan
         * @param currentContext DistrictSchoolYearContext
         * @return true, if is span for current year
         */
        private boolean isSpanForCurrentYear(StudentEnrollmentSpan span, DistrictSchoolYearContext currentContext) {
            boolean value = true;
            if (span.getFirstActiveDate() != null && span.getFirstActiveDate().after(currentContext.getEndDate())) {
                value = false;
            } else if (span.getLastActiveDate() != null
                    && span.getLastActiveDate().before(currentContext.getStartDate())) {
                value = false;
            }
            return value;
        }
    }

    /**
     * The Class that represents CT SASID record.
     */
    public class StudentSasidRecord {
        private BigDecimal m_absencesCount;
        private PlainDate m_entryDate;
        private StudentEnrollment m_entryEnr;
        private PlainDate m_exitDate;
        private String m_exitStatus;
        private String m_exitType;
        private String m_facilityCode1;
        private String m_facilityCode2;
        private String m_gradeLevel;
        private List<PlainDate> m_membership;
        private String m_recordType;
        private SisSchool m_school;
        private StudentInfo m_studentInfo;

        /**
         * Instantiates a new student sasid record.
         *
         * @param studentInfo StudentInfo
         */
        public StudentSasidRecord(StudentInfo studentInfo) {
            m_studentInfo = studentInfo;
        }

        /**
         * Gets the absences count.
         *
         * @return the m_absences
         */
        public BigDecimal getAbsencesCount() {
            return m_absencesCount;
        }

        /**
         * Gets the entry date.
         *
         * @return the m_entryDate
         */
        public PlainDate getEntryDate() {
            return m_entryDate;
        }

        /**
         * Gets the entry enr.
         *
         * @return the m_entryEnr
         */
        public StudentEnrollment getEntryEnr() {
            return m_entryEnr;
        }

        /**
         * Gets the exit date.
         *
         * @return the m_exitDate
         */
        public PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * Gets the exit status.
         *
         * @return the m_exitStatus
         */
        public String getExitStatus() {
            return m_exitStatus;
        }

        /**
         * Gets the exit type.
         *
         * @return the m_exitType
         */
        public String getExitType() {
            return m_exitType;
        }

        /**
         * Gets the facility code 1.
         *
         * @return the m_facilityCode1
         */
        public String getFacilityCode1() {
            return m_facilityCode1;
        }

        /**
         * Gets the facility code 2.
         *
         * @return the m_facilityCode2
         */
        public String getFacilityCode2() {
            return m_facilityCode2;
        }

        /**
         * Gets the grade level.
         *
         * @return the m_gradeLevel
         */
        public String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * Gets the membership days.
         *
         * @return the m_membership
         */
        public List<PlainDate> getMembershipDays() {
            return m_membership;
        }

        /**
         * Gets the record type.
         *
         * @return the m_recordType
         */
        public String getRecordType() {
            return m_recordType;
        }

        /**
         * Gets the school.
         *
         * @return the school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Sets the absences count.
         *
         * @param absencesCount void
         */
        public void setAbsencesCount(BigDecimal absencesCount) {
            this.m_absencesCount = absencesCount;
        }

        /**
         * Sets the entry date.
         *
         * @param entryDate void
         */
        public void setEntryDate(PlainDate entryDate) {
            this.m_entryDate = entryDate;
        }

        /**
         * Sets the entry enr.
         *
         * @param entryEnr void
         */
        public void setEntryEnr(StudentEnrollment entryEnr) {
            this.m_entryEnr = entryEnr;
        }

        /**
         * Sets the exit date.
         *
         * @param exitDate void
         */
        public void setExitDate(PlainDate exitDate) {
            this.m_exitDate = exitDate;
        }

        /**
         * Sets the exit status.
         *
         * @param exitStatus void
         */
        public void setExitStatus(String exitStatus) {
            this.m_exitStatus = exitStatus;
        }

        /**
         * Sets the exit type.
         *
         * @param exitType void
         */
        public void setExitType(String exitType) {
            this.m_exitType = exitType;
        }

        /**
         * Sets the facility code 1.
         *
         * @param facilityCode1 void
         */
        public void setFacilityCode1(String facilityCode1) {
            this.m_facilityCode1 = facilityCode1;
        }

        /**
         * Sets the facility code 2.
         *
         * @param facilityCode2 void
         */
        public void setFacilityCode2(String facilityCode2) {
            this.m_facilityCode2 = facilityCode2;
        }

        /**
         * Sets the grade level.
         *
         * @param gradeLevel void
         */
        public void setGradeLevel(String gradeLevel) {
            this.m_gradeLevel = gradeLevel;
        }

        /**
         * Sets the membership.
         *
         * @param membership void
         */
        public void setMembership(List<PlainDate> membership) {
            this.m_membership = membership;
        }

        /**
         * Sets the record type.
         *
         * @param recordType void
         */
        public void setRecordType(String recordType) {
            this.m_recordType = recordType;
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(SisSchool school) {
            this.m_school = school;
        }

        /**
         * Calculate the membership and attendance for a record.
         *
         * @param span StudentEnrollmentSpan
         * @param inSessionDates ImmutableSet<PlainDate>
         * @param absences List<StudentAttendance>
         */
        private void calculateAttendance(StudentEnrollmentSpan span,
                                         ImmutableSet<PlainDate> inSessionDates,
                                         List<StudentAttendance> absences) {
            if (m_reportDate == null) {
                throw new IllegalStateException("Attendance cannot be calculated without a report date2");
            }
            PlainDate entryDate = getEntryDate();
            if (entryDate == null) {
                entryDate = span.getFirstActiveDate();
            }
            PlainDate exitDate = getSpanExitDate(m_studentInfo, span);
            if (exitDate == null || exitDate.after(m_reportDate)) {
                exitDate = m_reportDate;
            }
            BigDecimal portionAbsent = BigDecimal.ZERO;
            List<PlainDate> membershipDates = new ArrayList<>();
            if (absences != null && !absences.isEmpty()) {
                for (StudentAttendance att : absences) {
                    if (entryDate != null && exitDate != null && att.getAbsentIndicator()
                            && !att.getDate().before(entryDate)
                            && !att.getDate().after(exitDate)
                            && !getPresentAttendanceCodes().contains(att.getReasonCode())) {
                        portionAbsent = portionAbsent.add(att.getPortionAbsent());
                    }
                }
            }
            if (entryDate != null && exitDate != null) {
                for (PlainDate inSessionDate : inSessionDates) {
                    if (!inSessionDate.before(entryDate) && !inSessionDate.after(exitDate)) {
                        membershipDates.add(inSessionDate);
                    }
                }
            }
            setMembership(membershipDates);
            setAbsencesCount(portionAbsent);
        }

    }

    /**
     * Constants.
     */
    public static final String ALIAS_ENR_SEC_SKL = "all-enr-SecondarySchool";
    public static final String ALIAS_ENR_SKL_OVERRIDE = "all-enr-SchoolOverride";
    public static final String ALIAS_SKL_OUT_OF_DISTRICT = "all-skl-OutOfDistrict";
    public static final String ALIAS_SKL_STATE_ID = "StateId";
    public static final String INPUT_PARAM_INCLUDE_OUT_OF_DISTRICT = "includeOutOfDistrict";
    public static final int MAX_LEGAL_NAME_LEN = 42;

    public static final String SASID_TYPE_C = "C";
    public static final String SASID_TYPE_R = "R";
    public static final String SASID_TYPE_U = "U";

    private static final String CODE_ATTENDANCE_PRESENT = "Present";

    /**
     * Members.
     */
    protected StateReportData m_data;
    protected String m_fieldSklStateId;
    protected String m_fieldEnrSklOverride;
    protected String m_fieldEnrSecSkl;
    private SchoolCalendarHelper m_calendarHelper;
    private Map<Integer, DistrictSchoolYearContext> m_ctxMapById;
    private Set<String> m_excludedSchools;
    private GradeMatcher m_gradeMatcher;
    protected boolean m_preferenceMemberOnWithdrawal;
    private Set<String> m_presentAttendanceCodes;
    private PlainDate m_reportDate;
    private Map<String, StudentInfo> m_studentInfoMap = new HashMap();
    private Map<String, ReferenceCode> m_withdrawalCodes;
    private Map<String, ReferenceCode> m_withdrawalReasons;

    /**
     * Instantiates a new CT student helper.
     *
     * @param data StateReportData
     * @throws X2BaseException exception
     */
    public CTStudentHelper(StateReportData data) throws X2BaseException {
        this(data, null);
    }

    /**
     * Instantiates a new CT student helper.
     *
     * @param data the data
     * @param reportDate PlainDate
     * @throws X2BaseException the x 2 base exception
     */
    public CTStudentHelper(StateReportData data, PlainDate reportDate) throws X2BaseException {
        super(data);
        m_data = data;
        m_reportDate = reportDate;
        m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(m_data.getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        m_fieldSklStateId = m_data.translateAliasToJavaName(ALIAS_SKL_STATE_ID, true);
        m_fieldEnrSklOverride = m_data.translateAliasToJavaName(ALIAS_ENR_SKL_OVERRIDE, true);
        m_fieldEnrSecSkl = m_data.translateAliasToJavaName(ALIAS_ENR_SEC_SKL, true);
        setStudentSelectionMode(MODE_STUDENT_ACTIVE_ANY_TIME);
        setSelectionProperty(PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
        setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, data.getCurrentContext().getStartDate());
        setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, data.getCurrentContext().getEndDate());
    }

    /**
     * Gets the grade level.
     *
     * @param student the student
     * @param span StudentEnrollmentSpan
     * @return the grade level
     */
    public String getCalculatedGradeLevel(SisStudent student, StudentEnrollmentSpan span) {
        int yog = span.getEnrollmentForDate(m_reportDate, "EY") != null
                ? span.getEnrollmentForDate(m_reportDate, "EY").getYog()
                : student.getYog();
        String gradeLevel = student.getGradeLevel();
        ReferenceCode gradeCode = null;
        if (m_gradeMatcher == null) {
            m_gradeMatcher = new GradeMatcher();
        }
        if (student.getYog() != yog) {
            gradeCode = m_gradeMatcher.getReferenceCode(yog);
            if (gradeCode != null) {
                gradeLevel = gradeCode.getCode();
            }
        } else {
            gradeCode = m_gradeMatcher.getReferenceCode(gradeLevel);
        }
        if (gradeCode != null) {
            gradeLevel = gradeCode.getStateCode();
        }
        return gradeLevel;
    }

    public Set<String> getExcludedSchools() {
        if (m_excludedSchools == null) {
            m_excludedSchools = new HashSet();
            Boolean includeOutOfDistrict = (Boolean) m_data.getParameter(INPUT_PARAM_INCLUDE_OUT_OF_DISTRICT);
            if (includeOutOfDistrict != null && !includeOutOfDistrict.booleanValue()) {
                String fieldOutOfDistrict = m_data.translateAliasToJavaName(ALIAS_SKL_OUT_OF_DISTRICT, false);
                if (!StringUtils.isEmpty(fieldOutOfDistrict)) {
                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(fieldOutOfDistrict, BooleanAsStringConverter.TRUE);
                    String[] columns = new String[] {X2BaseBean.COL_OID};
                    ColumnQuery query = new ColumnQuery(SisSchool.class, columns, criteria);
                    try (QueryIterator iterator = m_data.getBroker().getReportQueryIteratorByQuery(query)) {
                        while (iterator.hasNext()) {
                            Object[] row = (Object[]) iterator.next();
                            m_excludedSchools.add((String) row[0]);
                        }
                    }
                }
            }
        }
        return m_excludedSchools;
    }

    public Set<String> getPresentAttendanceCodes() {
        if (m_presentAttendanceCodes == null) {
            DataDictionaryField field = m_data.getDataDictionaryField(StudentAttendance.class, StudentAttendance.COL_REASON_CODE);
            if (field != null && field.hasReferenceTable()) {
                m_presentAttendanceCodes = m_data.getReferenceCodes(field.getReferenceTableOid()).values().stream()
                .filter(rcd -> CODE_ATTENDANCE_PRESENT.equals(rcd.getLocalCode()))
                .map(rcd -> rcd.getCode())
                .collect(Collectors.toSet());
            } else {
                m_presentAttendanceCodes = Collections.EMPTY_SET;
            }
        }
        return m_presentAttendanceCodes;
    }

    /**
     * Gets the span exit date.
     *
     * @param info StudentInfo
     * @param span StudentEnrollmentSpan
     * @return Plain date
     */
    public PlainDate getSpanExitDate(StudentInfo info, StudentEnrollmentSpan span) {
        PlainDate exitDate = span.getLastActiveDate();
        if (exitDate != null &&
                span.getFirstInactiveEnrollment() != null &&
                span.getFirstInactiveEnrollment().getEnrollmentDate() != null &&
                exitDate.equals(span.getFirstInactiveEnrollment().getEnrollmentDate()) &&
                !m_preferenceMemberOnWithdrawal) {
            // Last Active Date should be before first member day but is not
            // Reset exit date to one day prior to beginning district context date
            SchoolCalendarInfo currentCtxCalInfo =
                    getCalendarHelper().getSchoolCalendarInfo(m_data.getCurrentContext(), info.getStudent().getSchool(),
                            info.getCurrentCalCode());
            Calendar cal = Calendar.getInstance();
            cal.setTime(currentCtxCalInfo.getFirstInSessionDate());
            cal.add(Calendar.DATE, -1);
            PlainDate newExitDate = new PlainDate(cal.getTime());
            if (newExitDate.before(exitDate)) {
                exitDate = newExitDate;
            }
        }
        return exitDate;
    }

    /**
     * Return the current student criteria. If it has not been defined, create it.
     *
     * @return X2Criteria
     */
    @Override
    public X2Criteria getStudentCriteria() {
        X2Criteria criteria = super.getStudentCriteria();
        return criteria;
    }

    /**
     * Finds the student's info instance for the given student.
     *
     * @param student the student
     * @return the student info
     */
    public StudentInfo getStudentInfo(SisStudent student) {
        StudentInfo studentInfo = m_studentInfoMap.get(student.getOid());
        if (studentInfo == null) {
            studentInfo = new StudentInfo(student);
            m_studentInfoMap.put(student.getOid(), studentInfo);
        }
        return studentInfo;
    }

    /**
     * Returns ReferenceCode of the W code.
     *
     * @param code String
     * @return Reference code
     */
    public ReferenceCode getWithdrawalCode(String code) {
        if (m_withdrawalCodes == null) {
            String withdrawalTableOid = PreferenceManager.getPreferenceValue(m_data.getOrganization(),
                    SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
            m_withdrawalCodes = m_data.getReferenceCodes(withdrawalTableOid);
        }
        return m_withdrawalCodes.get(code);
    }

    /**
     * Returns ReferenceCode of the W reason.
     *
     * @param code String
     * @return Reference code
     */
    public ReferenceCode getWithdrawalReason(String code) {
        if (m_withdrawalReasons == null) {
            String withdrawalTableOid = PreferenceManager.getPreferenceValue(m_data.getOrganization(),
                    SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_REASONS);
            m_withdrawalReasons = m_data.getReferenceCodes(withdrawalTableOid);
        }
        return m_withdrawalReasons.get(code);
    }

    /**
     * Return instance of the SchoolCalendarHelper.
     *
     * @return School calendar helper
     */
    private SchoolCalendarHelper getCalendarHelper() {
        if (m_calendarHelper == null) {
            m_calendarHelper = new SchoolCalendarHelper();
        }
        return m_calendarHelper;
    }

    /**
     * Returns prior school year.
     *
     * @return District school year context
     */
    private DistrictSchoolYearContext getPriorCtx() {
        if (m_ctxMapById == null) {
            m_ctxMapById =
                    m_data.getBroker().getMapByQuery(
                            new QueryByCriteria(SisDistrictSchoolYearContext.class),
                            DistrictSchoolYearContext.COL_SCHOOL_YEAR, 1024);
        }
        return m_ctxMapById.get(Integer.valueOf(m_data.getCurrentContext().getSchoolYear() - 1));
    }
}
