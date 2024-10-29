/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Folder;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolOrganization;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteMonthly;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteRecord;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSchool;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class OnsisGradeBySchoolExport.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnGradeBySchoolReport extends OnReportJavaSource {

    /**
     * The Class GradeInfo.
     */
    public static class GenderInfo implements Comparable<GenderInfo> {
        private String m_gender;
        private String m_grade;
        private Set<StudentInfo> m_students = new HashSet();

        /**
         * Instantiates a new grade info.
         *
         * @param grade String
         * @param gender String
         */
        public GenderInfo(String grade, String gender) {
            m_gender = gender;
            m_grade = grade;
        }

        /**
         * Adds the.
         *
         * @param student OnStudent
         * @param arrivalStatus String
         * @param enrolled boolean
         * @param fte BigDecimal
         * @param fteHc BigDecimal
         */
        public void add(OnStudent student, String arrivalStatus, boolean enrolled, BigDecimal fte, BigDecimal fteHc) {
            if (ARRIVAL_STATUS_ARRIVED.equalsIgnoreCase(arrivalStatus)) {
                m_students.add(new StudentInfo(student, student.getOenRaw(), m_grade,
                        m_gender, arrivalStatus, enrolled, fte, fteHc));
            }
        }

        /**
         * Gets the count.
         *
         * @return int
         */
        public int getCount() {
            return m_students.stream().filter(s -> isIncluded(s)).collect(Collectors.counting()).intValue();
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (StudentInfo student : getIncludedStudentInfo()) {
                accumulator = accumulator.add(student.getFte());
            }
            return accumulator;
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getHcFte() {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (StudentInfo student : getIncludedStudentInfo()) {
                accumulator = accumulator.add(student.getHcFte());
            }
            return accumulator;
        }

        /**
         * Gets the included student info.
         *
         * @return Collection
         */
        public Collection<StudentInfo> getIncludedStudentInfo() {
            return m_students.stream().filter(s -> isIncluded(s)).collect(Collectors.toList());
        }

        /**
         * Gets the student info.
         *
         * @return Collection
         */
        public Collection<StudentInfo> getStudentInfo() {
            return m_students;
        }

        /**
         * Checks if is included.
         *
         * @param info StudentInfo
         * @return true, if is included
         */
        public boolean isIncluded(StudentInfo info) {
            return ARRIVAL_STATUS_ARRIVED.equals(info.getArrivalStatus());
        }

        /**
         * Compare to.
         *
         * @param o GenderInfo
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(GenderInfo o) {
            int compareTo = StringUtils.emptyIfNull(m_grade).compareTo(StringUtils.emptyIfNull(o.m_grade));
            if (compareTo == 0) {
                compareTo = StringUtils.emptyIfNull(m_gender).compareTo(StringUtils.emptyIfNull(o.m_gender));
            }
            return compareTo;
        }
    }

    /**
     * The Class GradeInfo.
     */
    public static class GradeInfo {
        private String m_gradeLevel;
        private Map<String, GenderInfo> m_genderInfoMap = new HashMap();

        /**
         * Instantiates a new grade info.
         *
         * @param gradeLevel String
         */
        public GradeInfo(String gradeLevel) {
            m_gradeLevel = gradeLevel;
        }

        /**
         * Gets the count.
         *
         * @return int
         */
        public int getCount() {
            return m_genderInfoMap.values().stream().mapToInt(info -> info.getCount()).sum();
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (GenderInfo gender : m_genderInfoMap.values()) {
                accumulator = accumulator.add(gender.getFte());
            }
            return accumulator;
        }

        /**
         * Gets the gender info.
         *
         * @return Collection
         */
        public Collection<GenderInfo> getGenderInfo() {
            return m_genderInfoMap.values();
        }

        /**
         * Gets the gender info.
         *
         * @param gender String
         * @return Grade info
         */
        public GenderInfo getGenderInfo(String gender) {
            GenderInfo info = m_genderInfoMap.get(gender);
            if (info == null) {
                info = new GenderInfo(m_gradeLevel, gender);
                m_genderInfoMap.put(gender, info);
            }
            return info;
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getHcFte() {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (GenderInfo gender : m_genderInfoMap.values()) {
                accumulator = accumulator.add(gender.getHcFte());
            }
            return accumulator;
        }

        /**
         * Gets the student info.
         *
         * @return Collection
         */
        public Collection<StudentInfo> getStudentInfo() {
            Collection<StudentInfo> students = new ArrayList();
            m_genderInfoMap.values().stream().forEach(info -> students.addAll(info.getStudentInfo()));
            return students;
        }
    }

    /**
     * The Class SchoolCounts.
     */
    public static class SchoolCounts {
        static final String FIELD_SCHOOL = "School";
        private static List<String> s_uniqueFields = Arrays.asList(FIELD_SCHOOL);

        private static ValueByKeyResolver<SchoolCounts> s_valueResolver = new ValueByKeyResolver<SchoolCounts>() {
            @Override
            public Object getValue(String key, SchoolCounts entity) {
                switch (key) {
                    case FIELD_SCHOOL:
                        return entity.getSchool();
                    case FIELD_SCHOOL_NAME:
                        return entity.getSchool().getName();
                    default:
                        throw new IllegalStateException("Invalid key value = " + key);
                }
            }
        };

        private Set<String> m_genderCodes = new TreeSet();
        private Map<String, GradeInfo> m_gradeInfoMap = new HashMap();
        private OnSchool m_school;

        /**
         * Instantiates a new school counts.
         *
         * @param school SisSchool
         */
        public SchoolCounts(OnSchool school) {
            m_school = school;
        }

        /**
         * Gets the all gender codes.
         *
         * @return Collection
         */
        public Collection<String> getAllGenderCodes() {
            return m_genderCodes;
        }

        /**
         * Gets the bsid.
         *
         * @return String
         */
        public String getBSID() {
            return getSchool().getBsid();
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (GradeInfo gradeInfo : m_gradeInfoMap.values()) {
                accumulator = accumulator.add(gradeInfo.getFte());
            }
            return accumulator;
        }

        /**
         * Gets the fte by gender.
         *
         * @param gender String
         * @return Big decimal
         */
        public BigDecimal getFteByGender(String gender) {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (GradeInfo gradeInfo : m_gradeInfoMap.values()) {
                GenderInfo genderInfo = gradeInfo.getGenderInfo(gender);
                if (genderInfo != null) {
                    accumulator = accumulator.add(genderInfo.getFte());
                }
            }
            return accumulator;
        }

        /**
         * Gets the grade count.
         *
         * @param gradeLevel String
         * @return int
         */
        public int getGradeCount(String gradeLevel) {
            GradeInfo info = m_gradeInfoMap.get(gradeLevel);
            return info == null ? 0 : info.getCount();
        }

        /**
         * Gets the grade count by gender.
         *
         * @param gradeLevel String
         * @param gender String
         * @return int
         */
        public int getGradeCountByGender(String gradeLevel, String gender) {
            int count = 0;
            GradeInfo gradeInfo = m_gradeInfoMap.get(gradeLevel);
            if (gradeInfo != null) {
                GenderInfo genderInfo = gradeInfo.getGenderInfo(gender);
                if (genderInfo != null) {
                    count = genderInfo.getCount();
                }
            }
            return count;
        }

        /**
         * Gets the grade gender info.
         *
         * @param gradeLevel String
         * @return Collection
         */
        public Collection<GenderInfo> getGradeGenderInfo(String gradeLevel) {
            GradeInfo info = m_gradeInfoMap.get(gradeLevel);
            return info == null ? Collections.EMPTY_LIST : info.getGenderInfo();
        }

        /**
         * Gets the grade student info.
         *
         * @param gradeLevel String
         * @return Collection
         */
        public Collection<StudentInfo> getGradeStudentInfo(String gradeLevel) {
            GradeInfo info = m_gradeInfoMap.get(gradeLevel);
            return info == null ? Collections.EMPTY_LIST : info.getStudentInfo();
        }

        /**
         * Gets the high credit fte.
         *
         * @return Big decimal
         */
        public BigDecimal getHcFte() {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (GradeInfo gradeInfo : m_gradeInfoMap.values()) {
                accumulator = accumulator.add(gradeInfo.getHcFte());
            }
            return accumulator;
        }

        /**
         * Gets the hc fte by gender.
         *
         * @param gender String
         * @return Big decimal
         */
        public BigDecimal getHcFteByGender(String gender) {
            BigDecimal accumulator = BigDecimal.ZERO;
            for (GradeInfo gradeInfo : m_gradeInfoMap.values()) {
                GenderInfo genderInfo = gradeInfo.getGenderInfo(gender);
                if (genderInfo != null) {
                    accumulator = accumulator.add(genderInfo.getHcFte());
                }
            }
            return accumulator;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public OnSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the school name.
         *
         * @return String
         */
        public String getSchoolName() {
            return getSchool().getName();
        }

        /**
         * Increment.
         *
         * @param gradeLevel String
         * @param gender String
         * @param arrivalStatus String
         * @param student OnStudent
         * @param enrolled boolean
         * @param fte BigDecimal
         * @param fteHc BigDecimal
         */
        public void increment(String gradeLevel,
                              String gender,
                              String arrivalStatus,
                              OnStudent student,
                              boolean enrolled,
                              BigDecimal fte,
                              BigDecimal fteHc) {
            m_genderCodes.add(gender);
            getGradeInfo(gradeLevel).getGenderInfo(gender).add(student, arrivalStatus, enrolled, fte, fteHc);
        }

        /**
         * Gets the grade info.
         *
         * @param gradeLevel String
         * @return Grade info
         */
        private GradeInfo getGradeInfo(String gradeLevel) {
            GradeInfo info = m_gradeInfoMap.get(gradeLevel);
            if (info == null) {
                info = new GradeInfo(gradeLevel);
                m_gradeInfoMap.put(gradeLevel, info);
            }
            return info;
        }
    }

    /**
     * The Class SchoolDateRangeProvider.
     */
    class SchoolDateRangeProvider implements OnSchoolDateRangeProvider {

        X2Broker m_broker;
        DistrictSchoolYearContext m_ctxByDate;
        DataDictionary m_dictionary;
        OnSchool m_school;

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisBrokerProvider#getBroker()
         */
        @Override
        public X2Broker getBroker() {
            return m_broker;
        }

        /**
         * Gets the district dictionary.
         *
         * @return Data dictionary
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDictionaryProvider#getDistrictDictionaryProvider()
         */
        public DataDictionary getDistrictDictionary() {
            if (m_dictionary == null) {
                m_dictionary = getDictionary();
            }
            return m_dictionary;
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnDictionaryExtractorProvider#getDictionaryExtractor()
         */
        @Override
        public DictionaryExtractor getDictionaryExtractor() {
            return getDictExtractor();
        }

        /**
         * Gets the context.
         *
         * @return District school year context
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisCTXProvider#getDistrictSchoolYearContext()
         */
        @Override
        public DistrictSchoolYearContext getContext() {
            return getContextByDate(getReportDate());
        }

        /**
         * Gets the context by date.
         *
         * @param date PlainDate
         * @return District school year context
         */
        public DistrictSchoolYearContext getContextByDate(PlainDate date) {
            if (m_ctxByDate == null) {
                QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class);
                query.addOrderByDescending(DistrictSchoolYearContext.COL_START_DATE);
                Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);
                /*
                 * Walk context years in descending order until one of them starts on/before date
                 */
                DistrictSchoolYearContext found = null;
                Iterator<DistrictSchoolYearContext> iter = contexts.iterator();
                while (found == null && iter.hasNext()) {
                    DistrictSchoolYearContext ctx = iter.next();

                    // Check that context starts before date.
                    if (date.before(ctx.getStartDate())) {
                        continue;
                    }

                    /*
                     * Check date is within 370 days of startDate
                     * to detect if we've gone too far (ie no context contains the date)
                     *
                     * Don't compare vs Context End Date
                     * because contexts might not extend until next context start date
                     */
                    PlainDate extendedContextEnd = DateUtils.add(ctx.getStartDate(), 370);
                    if (date.after(extendedContextEnd)) {
                        break;
                    }

                    found = ctx;
                }
                m_ctxByDate = found;
            }

            return m_ctxByDate;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDateRangeProvider#getEndDate()
         */
        @Override
        public PlainDate getEndDate() {
            return getReportDate();
        }

        /**
         * Gets the school.
         *
         * @return School
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisSchoolProvider#getSchool()
         */
        public OnSchool getOnSchool() {
            return m_school;
        }

        /**
         * Gets the school.
         *
         * @return On school
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolProvider#getSchool()
         */
        @Override
        public OnSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDateRangeProvider#getStartDate()
         */
        @Override
        public PlainDate getStartDate() {
            return getContext().getStartDate();
        }

        /**
         * Sets the broker.
         *
         * @param broker void
         */
        public void setBroker(X2Broker broker) {
            m_broker = broker;
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(OnSchool school) {
            m_school = school;
        }
    }

    /**
     * The Class StudentInfo.
     */
    public static class StudentInfo implements Comparable<StudentInfo> {
        String m_arrivalStatus;
        boolean m_enrolled;
        BigDecimal m_fte;
        BigDecimal m_fteHc;
        String m_gender;
        String m_gradeLevel;
        String m_stdOen;
        OnStudent m_student;

        /**
         * Instantiates a new student info.
         *
         * @param student OnStudent
         * @param stdOen String
         * @param gradeLevel String
         * @param gender String
         * @param arrivalStatus String
         * @param enrolled boolean
         * @param fte BigDecimal
         * @param fteHc BigDecimal
         */
        public StudentInfo(OnStudent student, String stdOen, String gradeLevel, String gender, String arrivalStatus,
                boolean enrolled,
                BigDecimal fte,
                BigDecimal fteHc) {
            m_student = student;
            m_stdOen = stdOen;
            m_arrivalStatus = arrivalStatus;
            m_enrolled = enrolled;
            m_fte = fte;
            m_fteHc = fteHc;
            m_gender = gender;
            m_gradeLevel = gradeLevel;
        }

        /**
         * Compare to.
         *
         * @param o StudentInfo
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(StudentInfo o) {
            return m_student.getNameView().compareTo(o.m_student.getNameView());
        }

        /**
         * Gets the arrival status.
         *
         * @return String
         */
        public String getArrivalStatus() {
            return m_arrivalStatus;
        }

        /**
         * Gets the enrolled.
         *
         * @return boolean
         */
        public boolean getEnrolled() {
            return m_enrolled;
        }

        /**
         * Gets the fte.
         *
         * @return Big decimal
         */
        public BigDecimal getFte() {
            return m_fte;
        }

        /**
         * Gets the gender.
         *
         * @return String
         */
        public String getGender() {
            return m_gender;
        }

        /**
         * Gets the grade level.
         *
         * @return String
         */
        public String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * Gets the high credit fte.
         *
         * @return Big decimal
         */
        public BigDecimal getHcFte() {
            return m_fteHc;
        }

        /**
         * Gets the std oen.
         *
         * @return String
         */
        public String getStdOen() {
            return m_stdOen;
        }

        /**
         * Gets the student.
         *
         * @return student
         */
        public OnStudent getStudent() {
            return m_student;
        }

    }

    private static final long serialVersionUID = 1L;

    private static final String ARRIVAL_STATUS_ARRIVED = "Arrived";
    private static final String FIELD_ARRIVAL_STATUS = "arrivalStatus";
    private static final String FIELD_BSID = "BSID";
    private static final String FIELD_FTE = "FTE";
    private static final String FIELD_FTES_MAP = "ftesMap";
    private static final String FIELD_GENDER = "gender";
    private static final String FIELD_GENDER_MAP = "genderMap";
    private static final String FIELD_GRADE_LEVEL = "grade";
    private static final String FIELD_GRADE_LEVEL_SORT = "gradeSort";
    private static final String FIELD_GRADES_MAP = "gradesMap";
    private static final String FIELD_HCFTE = "HCFTE";
    private static final String FIELD_PRIMARY_STUDENT = "primary";
    private static final String FIELD_STUDENT_NAME = "studentName";
    private static final String FIELD_STUDENT_NAME_FIRST = "studentNameFirst";
    private static final String FIELD_STUDENT_NAME_LAST = "studentNameLast";
    private static final String FIELD_STUDENT_OEN = "stdOEN";
    private static final String FIELD_SCHOOL_NAME = "schoolName";
    private static final String INPUT_PARAM_COUNT_SECONDARY = "countSecondary";
    private static final String INPUT_PARAM_FORMAT_DET_CSV = "formatDetailCsv";
    private static final String INPUT_PARAM_FORMAT_DET_PDF = "formatDetailPdf";
    private static final String INPUT_PARAM_FORMAT_SUM_CSV = "formatSummaryCsv";
    private static final String INPUT_PARAM_FORMAT_SUM_PDF = "formatSummaryPdf";
    private static final String INPUT_PARAM_GRADE_CODES = "gradeCodes";
    private static final String INPUT_PARAM_IS_DETAIL_REPORT = "isDetailReport";
    private static final String INPUT_PARAM_ROUND_UP_FTE = "roundUpFte";
    private static final String INPUT_PARAM_SORT_OPTION = "sortOption";
    private static final String MSG_RES_ARRIVAL_STATUS = "rpt.Arrival.Status.";
    private static final String MSG_RES_GRADE = "rpt.Grade.";
    private static final String REPORT_PARAM_DATE = "reportDate";
    private static final String REPORT_PARAM_ORG_NAME = "orgName";
    private static final String REPORT_PARAM_IS_SKL_CONTEXT = "isSklContext";
    private static final List<String> ONSIS_CODES_GRADE_LEVELS =
            Arrays.asList("JK", "K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12");
    private static final String SORT_OPTION_OEN = "oen";
    private static final List SKL_CODES_ELEM = Arrays.asList("01");
    private static final String VALUE_GENDER_CODE_UNKNOWN = "Unknown";

    /**
     * Class members
     */
    private DictionaryExtractor m_dictExtractor;
    private Map<String, Map<String, Double>> m_ftesSummaryMap = new HashMap<String, Map<String, Double>>();
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private Map<String, Map<String, Integer>> m_gradesSummaryMap = new HashMap<String, Map<String, Integer>>();
    private Boolean m_includeSecondary;
    private boolean m_isDetail = false;
    private boolean m_roundUpFte;
    private Map<String, Set<OnStudent>> m_schoolStudents;
    private SchoolDateRangeProvider m_sklDateRangeProvider;

    /**
     * Gets the custom file name.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        String value = super.getCustomFileName();
        int reportFormat = getJob().getInput().getFormat();
        boolean isDetail = getParameters().get(INPUT_PARAM_IS_DETAIL_REPORT) != null
                && getParameters().get(INPUT_PARAM_IS_DETAIL_REPORT) instanceof Boolean
                && ((Boolean) getParameters().get(INPUT_PARAM_IS_DETAIL_REPORT)).booleanValue();
        if (isDetail) {
            switch (reportFormat) {
                case ToolInput.CSV_FORMAT:
                    value = "GradesBySchoolDetail.csv";
                    break;
                case ToolInput.HTML_FORMAT:
                    value = "GradesBySchoolDetail.pdf";
                    break;
                case ToolInput.PDF_FORMAT:
                    value = "GradesBySchoolDetail.pdf";
                    break;
                case ToolInput.XLS_FORMAT:
                    value = "GradesBySchoolDetail.csv";
                    break;
            }
        } else {
            switch (reportFormat) {
                case ToolInput.CSV_FORMAT:
                    value = "GradesBySchoolSummary.csv";
                    break;
                case ToolInput.HTML_FORMAT:
                    value = "GradesBySchoolSummary.pdf";
                    break;
                case ToolInput.PDF_FORMAT:
                    value = "GradesBySchoolSummary.pdf";
                    break;
                case ToolInput.XLS_FORMAT:
                    value = "GradesBySchoolSummary.csv";
                    break;
            }
        }
        return value;
    }

    /**
     * Gets the yog.
     *
     * @param student OnStudent
     * @param enrollment StudentEnrollment
     * @return int
     */
    public int getYog(OnStudent student, ToolEnrollment enrollment) {
        int yog = student.getYog();
        if (enrollment != null) {
            yog = enrollment.getYog();
        }
        return yog;
    }

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        Collection<SchoolCounts> values = getSchools().map(new Mapper<OnSchool, SchoolCounts>() {
            @Override
            public Filterable<SchoolCounts> map(OnSchool school) {
                initializeHelpersForSchool(school);
                Filterable<SchoolCounts> schoolCounts = null;
                Filterable<OnStudent> students =
                        FilterableFactory.createFilterableToolBeans(getStudents(school));
                return getSchoolCounts(schoolCounts, school, students);
            }
        }).extractSorted(Arrays.asList(FIELD_SCHOOL_NAME), true);
        ReportDataGrid grid = new ReportDataGrid();
        Map<String, Integer> genderTotalsMap = new HashMap<String, Integer>();
        String orgName = getOrganization().getName();
        for (SchoolCounts item : values) {
            List<Stream<StudentInfo>> streams = new LinkedList();
            for (String gradeLevel : getGrades()) {
                streams.add(item.getGradeStudentInfo(gradeLevel).stream().sorted());
            }
            for (Stream<StudentInfo> stream : streams) {
                for (StudentInfo info : stream.collect(Collectors.toList())) {
                    String legalFullName = getStudentNameFirstMiddleLast(info.getStudent());
                    String bsid = item.getBSID();
                    String sklName = item.getSchoolName();
                    String grade = info.getGradeLevel();
                    if (!m_isDetail) {
                        String summaryMapKey = bsid + "," + sklName;
                        Map<String, Integer> gradesMap = m_gradesSummaryMap.get(summaryMapKey);
                        if (gradesMap == null) {
                            gradesMap = new HashMap<String, Integer>();
                            gradesMap.put(grade, Integer.valueOf(0));
                            gradesMap.put("total", Integer.valueOf(0));
                            m_gradesSummaryMap.put(summaryMapKey, gradesMap);
                        } else if (!gradesMap.containsKey(grade)) {
                            gradesMap.put(grade, Integer.valueOf(0));
                        }
                        gradesMap.put(grade,
                                Integer.valueOf(gradesMap.get(grade).intValue() + 1));
                        gradesMap.put("total",
                                Integer.valueOf(gradesMap.get("total").intValue() + 1));
                        Map<String, Double> ftesMap = m_ftesSummaryMap.get(summaryMapKey);
                        if (ftesMap == null) {
                            ftesMap = new HashMap<String, Double>();
                            ftesMap.put("fte", Double.valueOf(0d));
                            ftesMap.put("hcfte", Double.valueOf(0d));
                            m_ftesSummaryMap.put(summaryMapKey, ftesMap);
                        }
                        ftesMap.put("fte", Double.valueOf(
                                Double.sum(ftesMap.get("fte").doubleValue(), info.getFte().doubleValue())));
                        ftesMap.put("hcfte", Double.valueOf(
                                Double.sum(ftesMap.get("hcfte").doubleValue(), info.getHcFte().doubleValue())));
                    } else {
                        grid.append();
                        grid.set(FIELD_BSID, bsid);
                        grid.set(FIELD_SCHOOL_NAME, sklName);
                        String gradeLocale =
                                getUserMessageResources().getMessage(getUserMessageKeyPrefix() + MSG_RES_GRADE + grade);
                        grid.set(FIELD_GRADE_LEVEL, gradeLocale);
                        if (m_gradesMap.containsKey(grade)) {
                            String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                                    ALIAS_DDX_NUM_GRADE, m_dictExtractor.getDictionary(DDX_ID_GRADES));
                            if (!StringUtils.isEmpty(numGradeLevel)) {
                                grid.set(FIELD_GRADE_LEVEL_SORT, Integer.valueOf(numGradeLevel));
                            } else {
                                grid.set(FIELD_GRADE_LEVEL_SORT, Integer.valueOf(-99));
                            }
                        }
                        String gender = info.getGender();
                        String genderMapKey = bsid + grade + gender;
                        String genderMapKeyTotal = orgName + gender;
                        Integer genderCount = genderTotalsMap.get(genderMapKey);
                        if (genderCount == null) {
                            genderCount = Integer.valueOf(1);
                            genderTotalsMap.put(genderMapKey, genderCount);
                        } else {
                            genderTotalsMap.put(genderMapKey, Integer.valueOf(genderCount.intValue() + 1));
                        }
                        Integer genderCountTotal = genderTotalsMap.get(genderMapKeyTotal);
                        if (genderCountTotal == null) {
                            genderCountTotal = Integer.valueOf(1);
                            genderTotalsMap.put(genderMapKeyTotal, genderCountTotal);
                        } else {
                            genderTotalsMap.put(genderMapKeyTotal, Integer.valueOf(genderCountTotal.intValue() + 1));
                        }

                        grid.set(FIELD_GENDER, gender);
                        grid.set(FIELD_STUDENT_NAME, legalFullName);
                        grid.set(FIELD_STUDENT_OEN, info.getStdOen());
                        grid.set(FIELD_STUDENT_NAME_LAST, info.getStudent().getLegalLastName());
                        grid.set(FIELD_STUDENT_NAME_FIRST, info.getStudent().getLegalFirstName());
                        grid.set(FIELD_PRIMARY_STUDENT, info.getEnrolled() ? "Y" : "N");
                        grid.set(FIELD_ARRIVAL_STATUS, getUserMessageResources().getMessage(
                                getUserMessageKeyPrefix() + MSG_RES_ARRIVAL_STATUS + info.getArrivalStatus()));
                        grid.set(FIELD_FTE, info.getFte());
                        grid.set(FIELD_HCFTE, info.getHcFte());
                        grid.set(FIELD_GENDER_MAP, genderTotalsMap);
                    }
                }
            }
        }
        if (!m_isDetail) {
            populateSummaryGrid(grid);
        }
        if (!grid.isEmpty()) {
            grid.beforeTop();
            addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
            if (getErrorsLog().length() > 0) {
                addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
            }
            addParameter(REPORT_PARAM_IS_SKL_CONTEXT, Boolean.valueOf(getSchools().extract().size() < 2));
        } else {
            return grid;
        }
        List<String> sortColumns = Arrays.asList(FIELD_BSID, FIELD_SCHOOL_NAME, FIELD_GRADE_LEVEL_SORT, FIELD_GENDER,
                FIELD_STUDENT_NAME_LAST, FIELD_STUDENT_NAME_FIRST);
        if (getParameter(INPUT_PARAM_SORT_OPTION) != null
                && SORT_OPTION_OEN.equals(getParameter(INPUT_PARAM_SORT_OPTION))) {
            sortColumns = Arrays.asList(FIELD_BSID, FIELD_SCHOOL_NAME, FIELD_GRADE_LEVEL_SORT, FIELD_GENDER,
                    FIELD_STUDENT_OEN);
        }
        grid.sort(sortColumns, true);
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        registerToolBeans();
        initGradesMap();
        m_roundUpFte = getParameter(INPUT_PARAM_ROUND_UP_FTE) != null
                && getParameter(INPUT_PARAM_ROUND_UP_FTE) instanceof Boolean
                && ((Boolean) getParameter(INPUT_PARAM_ROUND_UP_FTE)).booleanValue() ? true : false;
        int reportFormat = getJob().getInput().getFormat();
        m_isDetail = getParameters().get(INPUT_PARAM_IS_DETAIL_REPORT) != null
                && getParameters().get(INPUT_PARAM_IS_DETAIL_REPORT) instanceof Boolean
                && ((Boolean) getParameters().get(INPUT_PARAM_IS_DETAIL_REPORT)).booleanValue();
        if (m_isDetail) {
            switch (reportFormat) {
                case ToolInput.CSV_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_DET_CSV));
                    break;
                case ToolInput.HTML_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_DET_PDF));
                    break;
                case ToolInput.PDF_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_DET_PDF));
                    break;
                case ToolInput.XLS_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_DET_CSV));
                    break;
            }
        } else {
            switch (reportFormat) {
                case ToolInput.CSV_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_SUM_CSV));
                    break;
                case ToolInput.HTML_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_SUM_PDF));
                    break;
                case ToolInput.PDF_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_SUM_PDF));
                    break;
                case ToolInput.XLS_FORMAT:
                    this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_SUM_CSV));
                    break;
            }
        }
        SimpleDateFormat dateFormatter = new SimpleDateFormat("dd MMMMM yyyy", getUserLocale());
        addParameter(REPORT_PARAM_DATE, dateFormatter.format(getReportDate()));
        addParameter(REPORT_PARAM_ORG_NAME, getOrganization().getName());
    }

    /**
     * Calculate span end date.
     *
     * @param span the span
     * @return the plain date
     */
    private PlainDate calculateSpanEndDate(OnAnnualSpan span) {
        PlainDate endDate = span.getFirstInactiveInSessionDate();

        boolean isWithdrawal = span.isWithdrawal();
        PlainDate lastActiveDate = span.getLastActiveInSessionDate();
        PlainDate withdrawalMemberDate = span.getWithdrawalMemberDate();

        PlainDate scanStartDate = null;
        boolean falseForward = false;
        ToolSchoolCalendar calendar = span.getSchoolCalendar();
        PlainDate lastCalendarDate = calendar == null || calendar.getDates(getBroker()).isEmpty() ? null
                : calendar.getDates(getBroker()).last();
        PlainDate lastInSessionDate =
                calendar == null ? null : calendar.findFirstInSessionDate(getBroker(), scanStartDate, falseForward);

        /*
         * 1. is this is a withdrawn span
         * i.e. W record is non-null and lastActiveDate is non-null
         */
        boolean isWithdrawn = (isWithdrawal && withdrawalMemberDate != null && lastActiveDate != null);

        /*
         * 2. Does W-1 date fall after the last in-session date of the span context year
         */

        boolean isAfterLastInSession =
                isWithdrawn && lastInSessionDate != null && withdrawalMemberDate.after(lastInSessionDate);

        if (isAfterLastInSession) {
            endDate = withdrawalMemberDate;
        } else if (endDate == null) {
            endDate = lastActiveDate != null && lastInSessionDate != null && lastActiveDate.equals(lastInSessionDate)
                    ? lastCalendarDate
                    : lastActiveDate;
        }

        return endDate;
    }



    /**
     * Gets the arrival status.
     *
     * @param enrollment StudentEnrollment
     * @param ssk OnStudentSchool
     * @return String
     */
    private String getArrivalStatus(OnEnrollment enrollment, OnStudentSchool ssk) {
        String result = enrollment == null ? "" : enrollment.getArrivalStatus();
        if (StringUtils.isEmpty(result) && ssk != null) {
            result = ssk.getArrivalStatus();
        }
        return result;
    }

    /**
     * Gets the grades.
     *
     * @return List
     */
    private List<String> getGrades() {
        Object selectedGradeCodes = getParameter(INPUT_PARAM_GRADE_CODES);
        Set<String> set = selectedGradeCodes == null ? Collections.EMPTY_SET
                : Arrays.asList(((String) selectedGradeCodes).split("\\s*,\\s*")).stream()
                        .map(oid -> (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, oid))
                        .filter(Objects::nonNull)
                        .map(rcd -> rcd.getStateCode())
                        .collect(Collectors.toSet());
        return ONSIS_CODES_GRADE_LEVELS.stream()
                .filter(grade -> set.isEmpty() || set.contains(grade))
                .collect(Collectors.toList());
    }

    /**
     * Gets the school counts.
     *
     * @param schoolCounts Filterable<SchoolCounts>
     * @param school SisSchool
     * @param students Filterable<OnStudent>
     * @return Filterable
     */
    private Filterable<SchoolCounts> getSchoolCounts(Filterable<SchoolCounts> schoolCounts,
                                                     OnSchool school,
                                                     Filterable<OnStudent> students) {

        return students.fold(schoolCounts, new Folder<OnStudent, Filterable<SchoolCounts>>() {
            @Override
            public Filterable<SchoolCounts> fold(OnStudent student, Filterable<SchoolCounts> accumulator) {
                boolean isEnrolled = true;
                String gender = student.getGenderType();
                if (StringUtils.isEmpty(gender)) {
                    gender = VALUE_GENDER_CODE_UNKNOWN;
                }
                OnEnrollment recentEnr = null;
                OnStudentSchool ssk = null;
                List<AnnualSpan> annualSpans =
                        isIncludeSecondary()
                                ? student.getEnrollmentSpans(getBroker(), false, false)
                                : student.getPrimaryEnrollmentSpans(school, getBroker());
                OnAnnualSpan firstAnnualSpan = annualSpans.stream()
                        .map(span -> (OnAnnualSpan) span)
                        .filter(span -> !span.isSecondary())
                        .filter(span -> span.getSchool().getOid().equals(school.getOid()))
                        .sorted(Collections.reverseOrder())
                        .findFirst()
                        .orElse(null);
                if (firstAnnualSpan == null) {
                    firstAnnualSpan = annualSpans.stream()
                            .map(span -> (OnAnnualSpan) span)
                            .filter(span -> span.getSchool().getOid().equals(school.getOid()))
                            .findFirst()
                            .orElse(null);
                }
                if (firstAnnualSpan != null) {
                    recentEnr = (OnEnrollment) firstAnnualSpan.getRecentEnrollmentESY();
                    if (recentEnr == null && isIncludeSecondary()
                            && firstAnnualSpan.isSecondary()) {
                        ssk = (OnStudentSchool) firstAnnualSpan.getSecondary();
                        AnnualSpan primSpan = firstAnnualSpan.getBestPrimarySpanFor(getBroker(), null);
                        if (primSpan != null) {
                            recentEnr = (OnEnrollment) firstAnnualSpan.getBestPrimarySpanFor(getBroker(), null)
                                    .getRecentEnrollmentESY();
                        } else {
                            recentEnr = (OnEnrollment) student.getEnrollmentForDate(getReportDate(), "ESY",
                                    getBroker());
                        }
                    }
                }
                String gradeLevel = student.getGradeLevelState();
                isEnrolled = firstAnnualSpan == null ? false : !firstAnnualSpan.isSecondary();
                String arrivalStatus = recentEnr == null ? "" : getArrivalStatus(recentEnr, ssk);

                Double fte = Double.valueOf(0.0d);
                Double highCreditFte = Double.valueOf(0.0d);
                if (recentEnr != null
                        && SKL_CODES_ELEM
                                .contains(((OnSchool) recentEnr.getSchool(getBroker())).getSchoolLevelCode())) {
                    fte = recentEnr.getFte().doubleValue();
                } else {
                    FteMonthly montlyFteRecordFoStd = student.getFteMonthlyRecords(getBroker())
                            .stream().findFirst().orElse(null);
                    FteRecord fteRecordC = null;
                    if (montlyFteRecordFoStd != null) {
                        fte = montlyFteRecordFoStd.getFte().doubleValue();
                        highCreditFte = montlyFteRecordFoStd.getFteHc().doubleValue();
                    } else {
                        fteRecordC = student.getFteRecordForDate(getBroker(), school, getReportDate());
                        if (fteRecordC != null) {
                            fte = fteRecordC.getFte().doubleValue();
                            highCreditFte = fteRecordC.getFteHc().doubleValue();
                        }
                        if (fteRecordC == null && isEnrolled) {
                            String errorMessage = "Can't find FTE record for student: "
                                    + getStudentNameFirstMiddleLast(student)
                                    + ", OEN: "
                                    + student.getOenRaw()
                                    + ", School Name: "
                                    + student.getSchool(getBroker()).getName()
                                    + " for District Context: "
                                    + m_sklDateRangeProvider.getContextByDate(getReportDate()).getContextId();
                            logError(errorMessage);
                        }
                    }
                }
                if (m_roundUpFte) {
                    Double fteSum = Double.sum(fte.doubleValue(), highCreditFte.doubleValue());
                    if (fteSum.compareTo(Double.valueOf(1.0)) < 1) {
                        if (fte.compareTo(Double.valueOf(0.0)) == 0
                                && Float.parseFloat(highCreditFte.toString()) >= FTE_FT_FINAL_THRESHOLD) {
                            highCreditFte = Double.valueOf(1.0);
                        }
                        if (fte.compareTo(Double.valueOf(0.0)) == 1
                                && Float.parseFloat(fteSum.toString()) >= FTE_FT_FINAL_THRESHOLD) {
                            double addToFte = 1.0 - highCreditFte.doubleValue() - fte.doubleValue();
                            fte = Double.valueOf(fte.doubleValue() + addToFte);
                        }
                    }
                }
                SchoolCounts schoolCount = null;
                if (accumulator == null) {
                    schoolCount = new SchoolCounts(school);
                    accumulator = FilterableFactory.create(
                            Arrays.asList(schoolCount),
                            SchoolCounts.s_uniqueFields,
                            SchoolCounts.s_valueResolver);
                } else {
                    Collection<SchoolCounts> classSizes = accumulator
                            .filter(Arrays.asList(SchoolCounts.FIELD_SCHOOL),
                                    Arrays.asList(school))
                            .extract();
                    if (classSizes == null || classSizes.isEmpty()) {
                        schoolCount = new SchoolCounts(school);
                        Filterable<SchoolCounts> newItem = FilterableFactory.create(
                                Arrays.asList(schoolCount),
                                SchoolCounts.s_uniqueFields,
                                SchoolCounts.s_valueResolver);
                        accumulator = accumulator.concat(newItem);
                    } else {
                        schoolCount = classSizes.iterator().next();
                    }
                }
                schoolCount.increment(gradeLevel, gender, arrivalStatus, student, isEnrolled,
                        BigDecimal.valueOf(fte.doubleValue()), BigDecimal.valueOf(highCreditFte.doubleValue()));
                return accumulator;
            }
        });
    }

    /**
     * Gets the students.
     *
     * @param school OnSchool
     * @return Sets the
     */
    private Set<OnStudent> getStudents(OnSchool school) {
        if (m_schoolStudents == null) {
            ToolOrganization organization =
                    ToolBean.getBeanByOid(getBroker(), ToolOrganization.class, getOrganization().getOid(), false);
            m_schoolStudents = new HashMap<>();
            ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getHistoricalCutoffDate());
            ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, getReportDate());
            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setSchoolOids(getSchools().getKeySet().stream().collect(Collectors.toList()))
                    .setExcludeStudent(OnStudent.FIELD_EXCLUDE_FROM_REPORTING)
                    .setIncludeSecondarySpans(isIncludeSecondary());
            X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());

            // load students with filterable
            FilterableFactory.create(getBroker(), getDictExtractor(), OnStudent.class, candidateCriteria, null);

            // load enrollments and student school
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                    ToolStudent.CHILD_STUDENT_ENROLLMENTS);
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                    ToolStudent.CHILD_STUDENT_SCHOOLS);

            Set<String> allStudentsList = new HashSet();
            ToolBean.getCachedToolBeans(OnStudent.class).stream()
                    .map(student -> student.getEnrollmentSpans(getBroker(), false, false))
                    .flatMap(List::stream)
                    .filter(span -> {
                        // test school
                        if (!getSchools().getKeySet().contains(span.getSchool().getOid())) {
                            return false;
                        }

                        // test secondary
                        if (!isIncludeSecondary() && span.isSecondary()) {
                            return false;
                        }

                        // test date range
                        if (!span.getDateRange().contains(getReportDate())) {
                            return false;
                        }
                        if (span.getSpanEndDate() != null && !span.getSpanEndDate().after(getReportDate())) {
                            return false;
                        }
                        return true;
                    })
                    .forEach(span -> {
                        Set<OnStudent> students = m_schoolStudents.get(span.getSchool().getOid());
                        if (students == null) {
                            students = new HashSet();
                            m_schoolStudents.put(span.getSchool().getOid(), students);
                        }
                        students.add((OnStudent) span.getStudent());
                        allStudentsList.add(span.getStudent().getOid());
                    });

            // remove unused students
            ToolBean.filterCachedToolBeans(OnStudent.class,
                    student -> allStudentsList.contains(student.getOid()));

            X2Criteria fteMontlyCriteria = new X2Criteria();
            fteMontlyCriteria.addEqualTo(FteMonthly.FIELD_REPORT_DATE.resolve(getDictExtractor()),
                    s_aliasDateFormat.format(getReportDate()));
            ToolBean.addAndCriteria(getBroker(), FteMonthly.class, fteMontlyCriteria);
            ToolBean.preload(getBroker(), getDictExtractor(), null, FteMonthly.PARENT_STUDENT);

            X2Criteria fteCriteria = new X2Criteria();
            ToolDistrictContext context = organization.getSchoolYearContext(getBroker(), getReportDate());
            String contextId = context == null ? getCurrentContext().getContextId() : context.getContextId();
            fteCriteria.addEqualTo(FteRecord.FIELD_SCHOOL_YEAR.resolve(getDictExtractor()), contextId);
            ToolBean.addAndCriteria(getBroker(), FteRecord.class, fteCriteria);
            ToolBean.preload(getBroker(), getDictExtractor(), null, FteRecord.PARENT_STUDENT);
        }
        Set<OnStudent> students = m_schoolStudents.get(school.getOid());
        return students == null ? Collections.EMPTY_SET : students;
    }

    /**
     * Inits the grades map.
     */
    private void initGradesMap() {
        DataDictionaryField dictionaryField =
                m_dictExtractor.getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
        String referenceTableOid = null;
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            referenceTableOid = dictionaryField.getDataFieldConfig().getReferenceTableOid();
            X2Criteria refTablecriteria = new X2Criteria();
            refTablecriteria.addEqualTo(X2BaseBean.COL_OID, referenceTableOid);
            ReferenceTable gradeRefTable =
                    getBroker().getBeanByQuery(new QueryByCriteria(ReferenceTable.class, refTablecriteria));
            if (gradeRefTable != null) {
                Map<String, ReferenceCode> gradesMap = gradeRefTable.getCodeMap();
                for (Entry<String, ReferenceCode> entry : gradesMap.entrySet()) {
                    String stateGradeCode = entry.getValue().getStateCode();
                    if (!StringUtils.isEmpty(stateGradeCode)) {
                        m_gradesMap.put(stateGradeCode, entry.getValue());
                    }
                }
            }
        }
    }

    /**
     * Initialize helpers for school.
     *
     * @param school SisSchool
     */
    private void initializeHelpersForSchool(OnSchool school) {
        m_sklDateRangeProvider = new SchoolDateRangeProvider();
        m_sklDateRangeProvider.setBroker(getBroker());
        m_sklDateRangeProvider.setSchool(school);
    }

    /**
     * Checks if is include secondary.
     *
     * @return true, if is include secondary
     */
    private boolean isIncludeSecondary() {
        if (m_includeSecondary == null) {
            m_includeSecondary = getParameter(INPUT_PARAM_COUNT_SECONDARY) != null
                    && getParameter(INPUT_PARAM_COUNT_SECONDARY) instanceof Boolean
                    && ((Boolean) getParameter(INPUT_PARAM_COUNT_SECONDARY));
        }
        return m_includeSecondary.booleanValue();
    }

    /**
     * Populate summary grid.
     *
     * @param summaryGrid ReportDataGrid
     */
    private void populateSummaryGrid(ReportDataGrid summaryGrid) {
        for (String mapKey : m_gradesSummaryMap.keySet()) {
            summaryGrid.append();
            String bsid = mapKey.split(",")[0];
            String sklName = mapKey.split(",")[1];
            summaryGrid.set(FIELD_BSID, bsid);
            summaryGrid.set(FIELD_SCHOOL_NAME, sklName);
            summaryGrid.set(FIELD_GRADES_MAP, m_gradesSummaryMap.get(mapKey));
            summaryGrid.set(FIELD_FTES_MAP, m_ftesSummaryMap.get(mapKey));
        }
    }

    /**
     * Register tool beans.
     */
    private void registerToolBeans() {
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnStudentSchool.class);
        ToolBean.registerClass(FteMonthly.class);
        ToolBean.registerClass(FteRecord.class);
        ToolBean.setBroker(getBroker());

        m_dictExtractor = new DictionaryExtractor(getBroker());
        ToolBean.setDictionaryExtractor(m_dictExtractor);


        DistrictManager.setAnnualSpanFactory(
                new OnAnnualSpanFactory(getBroker()).setAnnualSpanEndDateFn(new Function<AnnualSpan, PlainDate>() {
                    @Override
                    public PlainDate apply(AnnualSpan span) {
                        return calculateSpanEndDate((OnAnnualSpan) span);
                    }
                }));
        DistrictManager.setStudentScheduleSpanFactory(new OnStudentScheduleSpanFactory());
    }

}
