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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.DateRange;
import com.x2dev.utils.types.PlainDate;
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
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;
import java.util.Map.Entry;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Form data source for Educational Assessment Form A.
 *
 * @author X2 Development Corporation
 */
public class SpedAssessmentFormAData extends BaseFormReportJavaSource {

    /**
     * The Class EnrollmentStatistics.
     */
    class EnrollmentStatistics extends StateReportData {
        protected StudentHistoryHelper m_helper;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);

            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.FALSE);
        }
    }

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Alias that will contain form owner oid that fill the form
     */
    public static final String ALIAS_DATE = "ed-ass-a-date";
    public static final String ALIAS_OTHER_NAME = "other-name";
    public static final String ALIAS_OWNER_STAFF_OID = "owner-staff-oid";

    /**
     * Report parameter containing the owner Staff object.
     */
    public static final String PARAM_OWNER_STAFF = "ownerStaff";
    public static final String PARAM_OWNER_STUDENT = "ownerStudent";
    public static final String PARAM_EARLY_WITHDRAWAL = "excludeEarlyWithdrawal";

    /**
     * Report parameters
     */
    private static final String REPORT_DATE_DISPLAY = "formCreationDate";
    private static final String REPORT_GRADE_LEVEL = "gradeLevel";
    private static final String REPORT_GRADES_DISPLAY = "grades";
    private static final String REPORT_SCHOOLS_DISPLAY = "schools";
    private static final String REPORT_YEARS_DISPLAY = "years";

    /**
     * Codes
     */
    private static final String REF_CODES_GRADE_LEVEL_OID = "rtbGradeLevel";

    /**
     * Constants
     */
    private static final int YEARS_TO_CALCULATE = 14;
    private static final String REGEX_NUMERIC = "[0-9][0-9]";
    private static final String STRING_CARRIAGE_RETURN = "\n";
    private static final String STRING_HYPHEN = "-";

    /**
     * Variables
     */
    private MaSpedAttribHelper m_attribHelper;
    private Map<Integer, DistrictSchoolYearContext> m_contextsByYearMap;
    private Collection<DistrictSchoolYearContext> m_contextList;
    private Collection<StudentEnrollment> m_enrollmentList = new ArrayList<StudentEnrollment>();
    private List<StudentEnrollmentSpan> m_enrollmentSpansList;
    private GradeLevelHistory m_gradeHistory;
    private String m_gradeLevel;
    private HashMap<String, ReferenceCode> m_gradeRefCodesMap;
    private HashMap<String, String> m_gradeMap = new HashMap<String, String>();
    private HashMap<String, String> m_schoolNameMap = new HashMap<String, String>();
    private HashMap<String, String> m_yearMap = new HashMap<String, String>();
    private NumberFormat m_numericFormat2 = new DecimalFormat("00");
    private NumberFormat m_numericFormat4 = new DecimalFormat("0000");
    private Boolean m_earlyWithdrawal;

    private EnrollmentStatistics m_data;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        m_attribHelper = new MaSpedAttribHelper(getBroker(), true);
        X2BaseBean bean = getFormOwner();
        // owner is either student or iep depending on type of form
        if (bean != null) {
            SisStudent student = (SisStudent) (bean instanceof IepData ? ((IepData) bean).getStudent() : bean);
            addParameter(PARAM_OWNER_STUDENT, student);

            setUpEnrollmentStatistics();

            loadReferenceCodes();

            loadStudentEnrollments(student);

            setGradeLevel(student, getFormStartDate(bean));

            buildSchoolHistory(student);

            addParameter(PARAM_OWNER_STAFF, getOwnerStaff());
        }

        JRDataSource dataSource =
                m_attribHelper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());

        return dataSource;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        m_earlyWithdrawal = (Boolean) getParameter(PARAM_EARLY_WITHDRAWAL);

        // Load school years
        QueryByCriteria contextQuery = new QueryByCriteria(DistrictSchoolYearContext.class);
        contextQuery.addOrderByAscending(DistrictSchoolYearContext.COL_SCHOOL_YEAR);

        m_contextsByYearMap = getBroker().getMapByQuery(contextQuery, DistrictSchoolYearContext.COL_SCHOOL_YEAR, 64);

        m_contextList = getBroker().getCollectionByQuery(contextQuery);
    }

    /**
     * Adjust the start date of the school year context be 10/1. To will avoid entries that occur
     * slightly after
     * the school year start.
     *
     * @param context DistrictSchoolYearContext
     * @return PlainDate
     */
    private PlainDate adjustContextStart(DistrictSchoolYearContext context) {
        PlainDate date = context.getStartDate();

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        if (m_earlyWithdrawal != null && m_earlyWithdrawal.booleanValue()) {
            calendar.set(Calendar.MONTH, Calendar.OCTOBER);
        } else {
            calendar.set(Calendar.MONTH, Calendar.SEPTEMBER);
        }
        calendar.set(Calendar.DAY_OF_MONTH, 1);

        return new PlainDate(calendar.getTime());
    }

    /**
     * Build the school history display of the student.
     *
     * @param student SisStudent
     */
    private void buildSchoolHistory(SisStudent student) {
        StudentEnrollment startSpan = null;
        StudentEnrollment endSpan = null;

        m_gradeHistory =
                new GradeLevelHistory(student.getOid(), YEARS_TO_CALCULATE, student.getOrganization1(), getBroker());
        Map<DateRange, String> historyRange = m_gradeHistory.getDateRangeHistoryForStudent(student.getOid());
        int initialYear = getInitialYear(student.getOid());

        for (StudentEnrollment enrollment : m_enrollmentList) {
            if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                if (startSpan == null) {
                    startSpan = enrollment;
                } else {
                    endSpan = enrollment;
                    updateDisplay(student, startSpan, endSpan, historyRange, initialYear);

                    // Reset
                    endSpan = null;
                    startSpan = enrollment;
                }
            } else {
                endSpan = enrollment;
                updateDisplay(student, startSpan, endSpan, historyRange, initialYear);

                // Reset
                endSpan = null;

                if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                    startSpan = null;
                } else {
                    startSpan = enrollment;
                }
            }
        }

        // If there is a start without an end it is the student's current assignment
        if (startSpan != null && endSpan == null) {
            updateDisplay(student, startSpan, endSpan, historyRange, initialYear);
        }

        populateDisplayFields();
    }


    /**
     * Returns the end date to use for the enrollment span. If there is no ending enrollment record,
     * use today's date.
     *
     * @param enrollment StudentEnrollment
     * @return PlainDate
     */
    private PlainDate getEndDate(StudentEnrollment enrollment) {
        PlainDate date = getPlainDate();
        if (enrollment != null) {
            date = enrollment.getEnrollmentDate();
        }

        return date;
    }

    /**
     * Gets the form start date.
     *
     * @param bean X2BaseBean
     * @return Plain date
     * @throws X2BaseException exception
     */
    private PlainDate getFormStartDate(X2BaseBean bean) throws X2BaseException {
        PlainDate startDate = new PlainDate();
        if (bean instanceof IepData && ((IepData) bean).getStartDate() != null) {
            startDate = ((IepData) bean).getStartDate();
        } else {
            if (getFormDefinition() != null) {
                DataDictionary dictionary = getDictionary();
                DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_DATE);
                if (field != null) {
                    Object value = WebUtils.getProperty(getFormStorage(), field.getJavaName());

                    if (value instanceof PlainDate) {
                        startDate = (PlainDate) value;
                    }
                    if (value instanceof String) {
                        if (field.isString()) {
                            String format = WebUtils.generateFormat(field,
                                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()));
                            Converter baseConverter = ConverterFactory.getConverterForClass(
                                    field.getEffectiveJavaType(),
                                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                                    field.isString(), format);
                            if (baseConverter instanceof SystemStringConverter) {
                                SystemStringConverter converter = ((SystemStringConverter) baseConverter);
                                if (converter != null) {
                                    value = converter.parseSystemString((String) value);
                                    if (value instanceof PlainDate) {
                                        startDate = (PlainDate) value;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return startDate;
    }

    /**
     * Returns the first year of the student's grade level history.
     *
     * @param studentOid String
     * @return int
     */
    private int getInitialYear(String studentOid) {
        Map<Integer, String> history = m_gradeHistory.getHistoryForStudent(studentOid);

        List<Integer> years = new LinkedList<Integer>(history.keySet());

        Collections.sort(years);

        return years.get(0).intValue();
    }

    /**
     * Gets the owner staff.
     *
     * @return String
     */
    private String getOwnerStaff() {
        DataDictionary actionDictionary = getDictionary();
        X2BaseBean actionFormStorage = getFormStorage();
        String otherName = (String) actionFormStorage.getFieldValueByAlias(ALIAS_OTHER_NAME, actionDictionary);
        if (StringUtils.isEmpty(otherName)) {
            String ownerStaffOid =
                    (String) actionFormStorage.getFieldValueByAlias(ALIAS_OWNER_STAFF_OID, actionDictionary);
            if (!StringUtils.isEmpty(ownerStaffOid)) {
                SisStaff ownerStaff = (SisStaff) getBroker().getBeanByOid(SisStaff.class, ownerStaffOid);
                if (ownerStaff != null) {
                    otherName = ownerStaff.getNameView();
                    if (!StringUtils.isEmpty(ownerStaff.getSpedRole())) {
                        otherName += ", " + ownerStaff.getSpedRole();
                    }
                }
            }
        }
        return StringUtils.unNullify(otherName);
    }

    /**
     * Gets the start date of the enrollment span. If there is no starting enrollment record, get
     * the start date of
     * the student's initial year.
     *
     * @param enrollment StudentEnrollment
     * @param initialYear int
     * @return PlainDate
     */
    private PlainDate getStartDate(StudentEnrollment enrollment, int initialYear) {
        PlainDate date = null;

        if (enrollment != null) {
            date = enrollment.getEnrollmentDate();
        } else {
            DistrictSchoolYearContext context = m_contextsByYearMap.get(Integer.valueOf(initialYear));
            if (context != null) {
                date = context.getStartDate();
            }
        }

        return date;
    }

    /**
     * Gets the year the passed date falls into. If there is no valid year 0 is returned. If the
     * adjust flag is true, compare
     * against the context start date as 10/1 to avoid early year withdrawals
     *
     * @param date PlainDate
     * @param adjust boolean
     * @return int
     */
    private int getYear(PlainDate date, boolean adjust) {
        int year = 0;

        DistrictSchoolYearContext previousContext = null;
        for (DistrictSchoolYearContext context : m_contextList) {
            PlainDate startDate = context.getStartDate();
            if (adjust) {
                startDate = adjustContextStart(context);
            }

            PlainDate endDate = context.getEndDate();

            if (!date.before(startDate) && !date.after(endDate)) {
                year = context.getSchoolYear();
                break;
            } else if (date.before(startDate) && previousContext != null) {
                year = previousContext.getSchoolYear();
                break;
            }

            previousContext = context;
        }

        return year;
    }

    /**
     * Loads the Grade Level Reference Codes.
     */
    private void loadReferenceCodes() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODES_GRADE_LEVEL_OID);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

        m_gradeRefCodesMap = (HashMap<String, ReferenceCode>) getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 100);
    }

    /**
     * Loads the enrollment records for the student ordered by date/timestamp.
     *
     * @param student SisStudent
     */
    private void loadStudentEnrollments(SisStudent student) {
        // Better to use the StudentHistoryHelper as is it able to handle junk enrollment records.
        m_enrollmentSpansList = m_data.m_helper.getStudentEnrollmentSpans(student, false);

        for (StudentEnrollmentSpan enrollmentSpan : m_enrollmentSpansList) {
            StudentEnrollment entryEnrollment = enrollmentSpan.getFirstActiveEnrollment();
            if (entryEnrollment != null) {
                m_enrollmentList.add(entryEnrollment);
            }

            StudentEnrollment withdrawEnrollment = enrollmentSpan.getFirstInactiveEnrollment();
            if (withdrawEnrollment != null) {
                m_enrollmentList.add(withdrawEnrollment);
            }
        }
    }

    /**
     * Returns the grade level that falls within the date range.
     *
     * @param historyRange Map<DateRange,String>
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return String
     */
    private String lookupGrade(Map<DateRange, String> historyRange, PlainDate startDate, PlainDate endDate) {
        String gradeLevel = null;

        DateRange spanRange = new DateRange(startDate, endDate);

        int maxIntersectionDays = 0;
        for (Entry<DateRange, String> historyEntry : historyRange.entrySet()) {
            DateRange range = historyEntry.getKey();
            DateRange intersection = range.getIntersect(spanRange);

            if (intersection != null) {
                int intersectionDays =
                        DateUtils.getDayDifference(intersection.getLowerPlainDate(), intersection.getUpperPlainDate());
                if (intersectionDays > maxIntersectionDays) {
                    intersectionDays = maxIntersectionDays;
                    gradeLevel = historyEntry.getValue();
                }
            }
        }

        // Ensure grade level is not after the report grade level
        if (!StringUtils.isEmpty(gradeLevel)) {
            if (gradeLevel.matches(REGEX_NUMERIC)) {
                int numericLevel = m_gradeHistory.getNumericGradeLevel(gradeLevel).intValue();
                int currentLevel = m_gradeHistory.getNumericGradeLevel(m_gradeLevel).intValue();

                // Do not include grades that occur after the as-of grade of the IEP
                if (numericLevel > currentLevel) {
                    gradeLevel = null;
                }
            }
        }

        return gradeLevel;
    }

    /**
     * Populate Display Fields.
     */
    private void populateDisplayFields() {
        StringBuilder yearDisplay = new StringBuilder(200);
        StringBuilder gradeLevelDisplay = new StringBuilder(200);
        StringBuilder schoolDisplay = new StringBuilder(200);

        // Sort years, grades and schoolNames fields and append to the display fields
        Set<String> keys = m_yearMap.keySet();
        TreeMap keysSortedMap = new TreeMap();
        for (String key : keys) {
            keysSortedMap.put(key, key);
        }
        Set<String> keysSortedSet = keysSortedMap.keySet();
        for (String key : keysSortedSet) {
            yearDisplay.append(m_yearMap.get(key) + STRING_CARRIAGE_RETURN);
            gradeLevelDisplay.append(m_gradeMap.get(key) + STRING_CARRIAGE_RETURN);
            schoolDisplay.append(m_schoolNameMap.get(key) + STRING_CARRIAGE_RETURN);
        }

        addParameter(REPORT_YEARS_DISPLAY, yearDisplay.toString());
        addParameter(REPORT_GRADES_DISPLAY, gradeLevelDisplay.toString());
        addParameter(REPORT_SCHOOLS_DISPLAY, schoolDisplay.toString());
        if (getFormInstance() != null) {
            addParameter(REPORT_DATE_DISPLAY, new Date(getFormInstance().getCreatedTime()));
        }
        addParameter(REPORT_GRADE_LEVEL, m_gradeLevel);
    }

    /**
     * Get grade level on creation time based on iep start date, if not form creation date,
     * on most recent entry enrollment record.
     *
     * @param student SisStudent
     * @param startDate PlainDate
     */
    private void setGradeLevel(SisStudent student, PlainDate startDate) {
        // get grade level on creation time based on iep start date, if not form creation date, on
        // most recent entry enrollment record
        TreeMap<Integer, List<String>> gradeLevels = StudentManager.buildGradeLevelMap(getBroker());
        String gradeLevel = null;

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());

        BeanQuery enrollmentQuery = new BeanQuery(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);

        Collection<StudentEnrollment> enrollments = getBroker().getCollectionByQuery(enrollmentQuery);

        for (StudentEnrollment enrollment : enrollments) {
            if (startDate != null && enrollment.getEnrollmentDate().before(startDate)) {
                // student's YOG at this particular time
                int yog = enrollment.getYog();

                // get the school year from basedDate
                X2Criteria schoolYearCriteria = new X2Criteria();
                schoolYearCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, startDate);
                schoolYearCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, startDate);

                QueryByCriteria schoolYearQuery =
                        new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
                DistrictSchoolYearContext ctx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(schoolYearQuery);
                String currentContextOid = getCurrentContext().getContextId();
                int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());

                if (!StringUtils.isEmpty(currentContextOid) && currentContextOid.equalsIgnoreCase(ctx.getContextId())) {
                    gradeLevel = student.getGradeLevel();
                } else {
                    int schoolYear = ctx.getSchoolYear();
                    List<String> grades =
                            StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, gradeLevels);
                    gradeLevel = grades.get(0);
                }

                break;
            }
        }

        if (StringUtils.isEmpty(gradeLevel)) {
            gradeLevel = student.getGradeLevel();
        }

        m_gradeLevel = gradeLevel;
    }

    /**
     * Setup EnrollmentStatistics for the StudentHistoryHelper.
     *
     * @throws X2BaseException exception
     */
    private void setUpEnrollmentStatistics() throws X2BaseException {
        m_data = new EnrollmentStatistics();
        m_data.setBroker(getBroker());
        m_data.setCurrentContext(getCurrentContext());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        m_data.setSchoolContext(false);
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();
    }

    /**
     * Update the year, grade, and school displays based on the provided enrollment span.
     *
     * @param student SisStudent
     * @param startSpan StudentEnrollment
     * @param endSpan StudentEnrollment
     * @param historyRange Map<DateRange,String>
     * @param initialYear int
     */
    private void updateDisplay(SisStudent student,
                               StudentEnrollment startSpan,
                               StudentEnrollment endSpan,
                               Map<DateRange, String> historyRange,
                               int initialYear) {
        // Only process records without a start if the first enrollment record is a W or Y. W-W is
        // invalid.
        if (startSpan != null || startSpan == null && m_gradeMap.size() > 0) {
            PlainDate startDate = getStartDate(startSpan, initialYear);
            PlainDate endDate = getEndDate(endSpan);

            // DATA CLEANING:
            // If the entry date is between June 20 and Aug 30, make it Aug 30.
            // This excludes the entries in the summer from counting in the previous year.
            // EndDate will be cleaned up in getYear(endDate, true) below.
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(startDate);
            if ((calendar.get(Calendar.MONTH) == Calendar.JUNE && calendar.get(Calendar.DAY_OF_MONTH) > 20)
                    || calendar.get(Calendar.MONTH) == Calendar.JULY
                    || calendar.get(Calendar.MONTH) == Calendar.AUGUST) {

                calendar.set(Calendar.MONTH, Calendar.SEPTEMBER);
                calendar.set(Calendar.DAY_OF_MONTH, 1);
                startDate = new PlainDate(calendar.getTime());
                if (endDate.before(startDate)) {
                    startDate = endDate;
                }
            }

            if (startDate != null && endDate != null) {
                SisSchool school = student.getSchool();
                if (startSpan != null) {
                    school = startSpan.getSchool();
                } else if (endSpan != null) {
                    school = endSpan.getSchool();
                }

                int startYear = getYear(startDate, false);
                int endYear = getYear(endDate, true);

                // Iterate over included years and get grade level for each
                if (startYear > 0 && endYear > 0) {
                    // int recordCounter = 0;
                    for (int year = startYear; year <= endYear; year++) {
                        // recordCounter++;
                        DistrictSchoolYearContext context = m_contextsByYearMap.get(Integer.valueOf(year));

                        if (context != null) {
                            /*
                             * Get the start of this year's range. If the enrollment records span
                             * multiple years then
                             * use the start/end date of the school year.
                             */
                            PlainDate historyStart =
                                    context.getStartDate().after(startDate) ? context.getStartDate() : startDate;
                            PlainDate historyEnd =
                                    context.getEndDate().before(endDate) ? context.getEndDate() : endDate;

                            String gradeLevel = lookupGrade(historyRange, historyStart, historyEnd);
                            if (!StringUtils.isEmpty(gradeLevel)) {
                                if (m_gradeRefCodesMap.containsKey(gradeLevel)) {
                                    ReferenceCode referenceCode = m_gradeRefCodesMap.get(gradeLevel);
                                    String stateCode = referenceCode.getStateCode();
                                    gradeLevel = stateCode;
                                }

                                String key = m_numericFormat4.format(year);
                                // + STRING_HYPHEN + m_numericFormat2.format(recordCounter);
                                m_yearMap.put(key, m_numericFormat4.format(year));
                                m_gradeMap.put(key, gradeLevel);
                                m_schoolNameMap.put(key, school.getName());
                            }
                        }
                    }
                }
            }
        }
    }

}
