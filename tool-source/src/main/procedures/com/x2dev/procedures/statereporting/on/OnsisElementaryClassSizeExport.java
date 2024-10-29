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
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Folder;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolTranscript;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchedule;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import java.util.stream.Collectors;

/**
 * The Class OnsisElementaryClassSizeExport.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnsisElementaryClassSizeExport extends ExportJavaSource {

    private static final String CONST_TOOLS_FOR_PREFIX = "ied.EXP-ON-ELEM-CLASS-SIZ.";
    private final static String COURSE_CODE_TYPE_HOMEROOM = "Homeroom";



    private static final String FIELD_BSID = "BSID";
    private static final String FIELD_CLASS_NUMBER = "Class Number";
    private static final String FIELD_CLASS_SPED = "Self Contained Special Educaton";
    private static final String FIELD_CLASS_TRACK = "Class Type/Track";
    private static final Collection<String> FIELD_GRADES =
            Arrays.asList("JK", "K", "1", "2", "3", "4", "5", "6", "7", "8");
    private static final String FIELD_GRADE_LEVEL = "Grade";
    private static final String FIELD_KINDERGARTEN_CONDITIONS = "Kindergarten Conditions";
    private static final String FIELD_STUDENT_NAME = "Student Name";
    //
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_DEBUG_DETAIL = "debugDetail";
    private static final String INPUT_PARAM_DEBUG_STUDENT_OID = "debugStudentOid";
    private static final String INPUT_PARAM_FILE_NAME = "fileName";
    private static final String INPUT_PARAM_INCLUDE_COURSE_VIEW = "includeCourseView";
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_SUPPRESS_UNASSIGNED = "suppressUnassigned";

    /**
     * The Class ClassSize.
     */
    private static class ClassSize {
        /**
         * The Enum Field.
         */
        enum ClassSizeField {
            SCHOOL, SECTION;
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(ClassSizeField.SCHOOL.toString(), ClassSizeField.SECTION.toString());

        private static ValueByKeyResolver<ClassSize> s_valueResolver = new ValueByKeyResolver<ClassSize>() {
            @Override
            public Object getValue(String key, ClassSize entity) {
                return getBeanOid((ToolBean) entity.get(ClassSizeField.valueOf(key)));
            }
        };

        /**
         * Gets the bean oid.
         *
         * @param bean X2BaseBean
         * @return String
         */
        private static String getBeanOid(ToolBean bean) {
            return (bean == null ? "null" : bean.getOid());
        }

        private Map<String, Integer> m_counts = new HashMap();
        private List<KeyValuePair<String, OnStudent>> m_studentGrades = new LinkedList();
        private int m_totalCount;
        private Map<ClassSizeField, Object> m_values = new HashMap<>();

        /**
         * Instantiates a new class size.
         *
         * @param data OnsisStateReportData
         * @param school SisSchool
         * @param section MasterSchedule
         */
        public ClassSize(OnSchool school, OnSection section) {
            // m_data = data;
            m_values.put(ClassSizeField.SCHOOL, school);
            m_values.put(ClassSizeField.SECTION, section);
        }

        /**
         * Gets the.
         *
         * @param field Field
         * @return Object
         */
        public Object get(ClassSizeField field) {
            return m_values.get(field);
        }

        /**
         * Gets the bsid.
         *
         * @return String
         */
        public String getBSID() {
            return StringUtils.unNullify(getSchool().getBsid());
        }

        /**
         * Gets the class track.
         *
         * @return String
         */
        public String getClassTrack() {
            String value = "1";
            OnSection section = getSection();
            if (section != null) {
                String override = section.getLanguageOfInstruction();
                if (!StringUtils.isEmpty(override) && "F".equals(override)) {
                    value = "3";
                }
            }
            return value;
        }

        /**
         * Gets the grade count.
         *
         * @param grade String
         * @return Object
         */
        public Object getGradeCount(String grade) {
            Integer value = m_counts.get(grade);
            return value == null ? Integer.valueOf(0) : value;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public OnSchool getSchool() {
            return (OnSchool) get(ClassSizeField.SCHOOL);
        }

        /**
         * Gets the section.
         *
         * @return Master schedule
         */
        public OnSection getSection() {
            return (OnSection) get(ClassSizeField.SECTION);
        }

        /**
         * Gets the self contained sped count.
         *
         * @return String
         */
        public String getSelfContainedSpedCount() {
            String value = "";
            if (isSelfContainedSped()) {
                value = Integer.toString(m_totalCount);
            }
            return value;
        }

        /**
         * Gets the students.
         *
         * @return List
         */
        public List<KeyValuePair<String, OnStudent>> getStudents() {
            return m_studentGrades;
        }

        /**
         * Increment.
         *
         * @param gradeLevel String
         * @param student SisStudent
         */
        public void increment(String gradeLevel, OnStudent student) {
            m_studentGrades.add(new KeyValuePair(gradeLevel, student));
            ++m_totalCount;
            if (!m_counts.containsKey(gradeLevel)) {
                m_counts.put(gradeLevel, Integer.valueOf(1));
            } else {
                m_counts.put(gradeLevel, Integer.valueOf(m_counts.get(gradeLevel).intValue() + 1));
            }
        }

        /**
         * Checks if is self contained sped.
         *
         * @return true, if is self contained sped
         */
        public boolean isSelfContainedSped() {
            boolean value = false;
            OnSection section = getSection();
            if (section != null) {
                String classType = section.getClassType();
                if (!StringUtils.isEmpty(classType) && "S".equals(classType)) {
                    value = true;
                }
            }
            return value;
        }
    }

    /**
     * The Class StudentClass.
     */
    private static class StudentClass {
        /**
         * The Enum Field.
         */
        enum StudentClassField {
            STUDENT, SCHOOL, SECTION;
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(StudentClassField.STUDENT.toString());

        private static ValueByKeyResolver<StudentClass> s_valueResolver = new ValueByKeyResolver<StudentClass>() {
            @Override
            public Object getValue(String key, StudentClass entity) {
                return entity.get(StudentClassField.valueOf(key));
            }
        };

        private Map<StudentClassField, Object> m_values = new HashMap<>();

        /**
         * Instantiates a new student class.
         *
         * @param student SisStudent
         * @param school SisSchool
         * @param section MasterSchedule
         */
        public StudentClass(OnStudent student, OnSchool school, OnSection section) {
            m_values.put(StudentClassField.STUDENT, student);
            m_values.put(StudentClassField.SCHOOL, school);
            m_values.put(StudentClassField.SECTION, section);
        }

        /**
         * Gets the.
         *
         * @param field Field
         * @return Object
         */
        public Object get(StudentClassField field) {
            return m_values.get(field);
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public OnSchool getSchool() {
            return (OnSchool) get(StudentClassField.SCHOOL);
        }

        /**
         * Gets the section.
         *
         * @return Master schedule
         */
        public OnSection getSection() {
            return (OnSection) get(StudentClassField.SECTION);
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public OnStudent getStudent() {
            return (OnStudent) get(StudentClassField.STUDENT);
        }
    }

    /**
     * Gets the debug detail.
     *
     * @param parameters Map
     * @return boolean
     */
    public static boolean getDebugDetail(Map parameters) {
        return parameters.get(INPUT_PARAM_DEBUG_DETAIL) != null
                && parameters.get(INPUT_PARAM_DEBUG_DETAIL) instanceof Boolean
                && ((Boolean) parameters.get(INPUT_PARAM_DEBUG_DETAIL)).booleanValue() ? true : false;
    }

    /**
     * Gets the debug detail.
     *
     * @param parameters Map
     * @return boolean
     */
    public static boolean getIncludeCourseView(Map parameters) {
        return parameters.get(INPUT_PARAM_INCLUDE_COURSE_VIEW) != null
                && parameters.get(INPUT_PARAM_INCLUDE_COURSE_VIEW) instanceof Boolean
                && ((Boolean) parameters.get(INPUT_PARAM_INCLUDE_COURSE_VIEW)).booleanValue() ? true : false;
    }

    /**
     * The Class SchoolDateRangeProvider.
     */
    class SchoolDateRangeProvider implements OnSchoolDateRangeProvider {
        private OnSchool m_school;

        /**
         * Instantiates a new school date range provider.
         *
         * @param school the school
         */
        public SchoolDateRangeProvider(OnSchool school) {
            m_school = school;
        }

        /**
         * Gets the broker.
         *
         * @return the broker
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnBrokerProvider#getBroker()
         */
        @Override
        public X2Broker getBroker() {
            return OnsisElementaryClassSizeExport.this.getBroker();
        }

        /**
         * Gets the context.
         *
         * @return the context
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnCTXProvider#getContext()
         */
        @Override
        public DistrictSchoolYearContext getContext() {
            return getCurrentContext();
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return the dictionary extractor
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnDictionaryExtractorProvider#getDictionaryExtractor()
         */
        @Override
        public DictionaryExtractor getDictionaryExtractor() {
            return getDictExtractor();
        }

        /**
         * Gets the end date.
         *
         * @return the end date
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnDateRangeProvider#getEndDate()
         */
        @Override
        public PlainDate getEndDate() {
            return getCurrentContext().getEndDate();
        }

        /**
         * Gets the school.
         *
         * @return the school
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolProvider#getSchool()
         */
        @Override
        public OnSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the start date.
         *
         * @return the start date
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnDateRangeProvider#getStartDate()
         */
        @Override
        public PlainDate getStartDate() {
            return getReportDate();
        }

    }

    private LocalizationMessageResources m_defaultMessageResource;
    private DictionaryExtractor m_dictExtractor;
    private final List<String> m_elemSchoolStateCodes = Arrays.asList("01", "03");
    private final List<String> m_excludedSpecialConditions = Arrays.asList("N", "T");
    private transient GradesHelper m_gradesHelper;
    private Locale m_locale;
    private PlainDate m_reportDate;
    private transient Map<String, SchoolDateRangeProvider> m_schoolDateRangeProvider;
    private Filterable<OnSchool> m_schools;
    private List<String> m_schoolOids;
    private Map<String, Set<OnStudent>> m_schoolStudents;
    private boolean m_suppressUnassigned = true;
    private Boolean m_debugDetail;


    /**
     * Gets the custom file name.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        String value = super.getCustomFileName();
        if (getParameter(INPUT_PARAM_FILE_NAME) != null) {
            value = getParameter(INPUT_PARAM_FILE_NAME).toString();
        }

        return value;
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
        Collection<ClassSize> values = getSchools().map(new Mapper<OnSchool, ClassSize>() {
            @Override
            public Filterable<ClassSize> map(OnSchool school) {
                Filterable<OnStudent> students = getStudents(school);
                return getClassSizes(school, students);
            }
        }).extract().stream()
                .sorted(Comparator.comparing((ClassSize cs) -> cs.getBSID())
                        .thenComparing(cs -> cs.getSchool().getOid()))
                .collect(Collectors.toList());
        boolean debugDetail = getDebugDetail();
        boolean includeCourseView = getIncludeCourseView(getParameters());
        DataGrid grid = new DataGrid();
        int classNumber = 1;
        for (ClassSize item : values) {
            if (!m_suppressUnassigned || item.getSection() != null) {
                String classString = String.valueOf(classNumber);
                if (includeCourseView) {
                    classString += "-" + (item.getSection() == null ? "Unassigned" : item.getSection().getCourseView());
                }
                if (debugDetail) {
                    for (KeyValuePair<String, OnStudent> pair : item.getStudents()) {
                        grid.append();
                        grid.set(FIELD_BSID, item.getBSID());
                        grid.set(FIELD_CLASS_NUMBER, classString);
                        grid.set(FIELD_STUDENT_NAME, pair.getValue().getNameView());
                        grid.set(FIELD_GRADE_LEVEL, pair.getKey());
                    }
                } else {
                    grid.append();
                    grid.set(FIELD_BSID, item.getBSID());
                    grid.set(FIELD_CLASS_NUMBER, classString);
                    if (!item.isSelfContainedSped()) {
                        for (String grade : FIELD_GRADES) {
                            grid.set(grade, item.getGradeCount(grade).toString());
                        }
                    } else {
                        grid.set(FIELD_CLASS_SPED, item.getSelfContainedSpedCount());
                    }
                    grid.set(FIELD_CLASS_TRACK, item.getClassTrack());
                    grid.set(FIELD_KINDERGARTEN_CONDITIONS, "1");
                }
                classNumber += 1;
            }
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columns = new LinkedList();
        boolean debugDetail = getDebugDetail();

        if (debugDetail) {
            columns.add(FIELD_BSID);
            columns.add(FIELD_CLASS_NUMBER);
            columns.add(FIELD_STUDENT_NAME);
            columns.add(FIELD_GRADE_LEVEL);
        } else {
            columns.add(FIELD_BSID);
            columns.add(FIELD_CLASS_NUMBER);
            columns.addAll(FIELD_GRADES);
            columns.add(FIELD_CLASS_SPED);
            columns.add(FIELD_CLASS_TRACK);
            columns.add(FIELD_KINDERGARTEN_CONDITIONS);
        }
        return columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columns = new LinkedList();
        boolean debugDetail = getDebugDetail();

        if (debugDetail) {
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_BSID));
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX +
                    FIELD_CLASS_NUMBER));
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX +
                    FIELD_STUDENT_NAME));
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_GRADE_LEVEL));
        } else {
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_BSID));
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX +
                    FIELD_CLASS_NUMBER));
            for (String grade : FIELD_GRADES) {
                columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX + "Grade " + grade));
            }
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASS_SPED));
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX + FIELD_CLASS_TRACK));
            columns.add(m_defaultMessageResource.getMessage(CONST_TOOLS_FOR_PREFIX +
                    FIELD_KINDERGARTEN_CONDITIONS));
        }
        return columns;
    }


    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
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
        try {
            m_defaultMessageResource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_locale);
        } catch (Exception e) {
            m_defaultMessageResource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
        }

        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());
        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));
        DistrictManager.setStudentScheduleSpanFactory(new OnStudentScheduleSpanFactory());

        ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getReportDate());
        ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, getReportDate());

        ToolBean.registerClass(OnSchedule.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnSection.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(ToolTranscript.class);

        if (getParameter(INPUT_PARAM_SUPPRESS_UNASSIGNED) != null
                && getParameter(INPUT_PARAM_SUPPRESS_UNASSIGNED) instanceof Boolean) {
            m_suppressUnassigned = ((Boolean) getParameter(INPUT_PARAM_SUPPRESS_UNASSIGNED)).booleanValue();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see
     *      com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        if (userData != null) {
            m_locale = userData.getLocale();
        }
    }



    /**
     * Gets the class sizes.
     *
     * @param helper OnsisStudentHistoryHelper
     * @param school SisSchool
     * @param students Filterable<SisStudent>
     * @return Filterable
     */
    private Filterable<ClassSize> getClassSizes(OnSchool school,
                                                Filterable<OnStudent> students) {
        Range<Date> reportDateRange = Range.of(getReportDate(), getReportDate());
        return students.map(new Mapper<OnStudent, StudentClass>() {
            @Override
            public Filterable<StudentClass> map(OnStudent student) {
                List<StudentScheduleSpan> studentSpans = student.getStudentScheduleSpans(getBroker()).stream()
                        .filter(span -> reportDateRange.isOverlap(span.getDateRange()))
                        .collect(Collectors.toList());
                OnSection section = null;
                if (studentSpans.size() > 1) {
                    // TODO: process error
                } else if (studentSpans.size() == 1) {
                    section = (OnSection) studentSpans.iterator().next().getSection();
                }

                StudentClass studentClass = new StudentClass(student, school, section);
                return FilterableFactory.create(
                        Arrays.asList(studentClass),
                        StudentClass.s_uniqueFields,
                        StudentClass.s_valueResolver);
            }
        })
                .fold(null, new Folder<StudentClass, Filterable<ClassSize>>() {

                    @Override
                    public Filterable<ClassSize> fold(StudentClass item, Filterable<ClassSize> accumulator) {
                        OnSchool skl = item.getSchool();
                        OnSection section = item.getSection();

                        String gradeLevel = getStudentGradeType(item.getStudent());

                        ClassSize classSize = null;
                        if (accumulator == null) {
                            classSize = new ClassSize(skl, section);
                            accumulator = FilterableFactory.create(
                                    Arrays.asList(classSize),
                                    ClassSize.s_uniqueFields,
                                    ClassSize.s_valueResolver);
                        } else {
                            Collection<ClassSize> classSizes = accumulator
                                    .filter(Arrays.asList(ClassSize.ClassSizeField.SCHOOL.toString(),
                                            ClassSize.ClassSizeField.SECTION.toString()),
                                            Arrays.asList(ClassSize.getBeanOid(skl), ClassSize.getBeanOid(section)))
                                    .extract();
                            if (classSizes == null || classSizes.isEmpty()) {
                                classSize = new ClassSize(skl, section);
                                Filterable<ClassSize> newItem = FilterableFactory.create(
                                        Arrays.asList(classSize),
                                        ClassSize.s_uniqueFields,
                                        ClassSize.s_valueResolver);
                                accumulator = accumulator.concat(newItem);
                            } else {
                                classSize = classSizes.iterator().next();
                            }
                        }
                        classSize.increment(gradeLevel, item.getStudent());

                        return accumulator;
                    }
                });
    }

    /**
     * Gets the debug detail.
     *
     * @return boolean
     */
    private boolean getDebugDetail() {
        if (m_debugDetail == null) {
            m_debugDetail = getDebugDetail(getParameters());
        }
        return m_debugDetail.booleanValue() ? true : false;
    }

    /**
     * Gets the debug student oid.
     *
     * @return the debug student oid
     */
    private String getDebugStudentOid() {
        return (String) getParameter(INPUT_PARAM_DEBUG_STUDENT_OID);
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    private DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Gets the grades helper.
     *
     * @return the grades helper
     */
    private GradesHelper getGradesHelper() {
        if (m_gradesHelper == null) {
            m_gradesHelper = new GradesHelper(getSchoolDateRangeProvider(null));
        }
        return m_gradesHelper;
    }

    /**
     * Gets the report date.
     *
     * @return Plain date
     */
    private PlainDate getReportDate() {
        if (m_reportDate == null) {
            m_reportDate = getParameter(INPUT_PARAM_REPORT_DATE) == null
                    ? new PlainDate()
                    : (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        }
        return m_reportDate;
    }

    /**
     * Gets the school date range provider.
     *
     * @param school the school
     * @return the school date range provider
     */
    private SchoolDateRangeProvider getSchoolDateRangeProvider(OnSchool school) {
        String key = school == null ? "Default" : school.getOid();
        if (m_schoolDateRangeProvider == null) {
            m_schoolDateRangeProvider = new HashMap();
        }
        if (!m_schoolDateRangeProvider.containsKey(key)) {
            m_schoolDateRangeProvider.put(key, new SchoolDateRangeProvider(school));
        }
        return m_schoolDateRangeProvider.get(key);
    }

    /**
     * Gets the schools.
     *
     * @return Filterable
     */
    private Filterable<OnSchool> getSchools() {
        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
        if (m_schools == null) {
            m_schools = FilterableFactory
                    .create(getBroker(), OnSchool.class, new X2Criteria(),
                            Arrays.asList(OnSchool.FIELD_NAME, OnSchool.FIELD_OID))
                    .filter(new Filter() {
                        @Override
                        public boolean isFiltered(Object toFilter) {
                            OnSchool school = (OnSchool) toFilter;
                            return isElementarySchool(school) && (isAllSchools ||
                                    getSchoolOids().contains(school.getOid()));
                        }
                    });
        }
        return m_schools;
    }

    /**
     * Gets the school oids.
     *
     * @return List
     */
    private List<String> getSchoolOids() {
        if (m_schoolOids == null) {
            String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
            m_schoolOids = Arrays.asList(schoolOids.split(","));
        }
        return m_schoolOids;
    }

    /**
     * Gets the student grade type.
     *
     * @param reportData
     *
     * @param student SisStudent
     * @return String
     */
    private String getStudentGradeType(OnStudent student) {
        List<OnAnnualSpan> spans = student.getEnrollmentSpans(getBroker(), false, false).stream()
                .map(span -> (OnAnnualSpan) span)
                .collect(Collectors.toList());
        String gradeLevel = spans != null && !spans.isEmpty()
                ? (spans.get(spans.size() - 1)).getGradeType(getBroker(), getDictExtractor(), getGradesHelper())
                : student.getGradeLevel();
        return gradeLevel;
    }

    /**
     * Gets the students.
     *
     * @param helper OnsisStudentHistoryHelper
     * @return Filterable
     */
    private Filterable<OnStudent> getStudents(OnSchool school) {

        if (m_schoolStudents == null) {
            m_schoolStudents = new HashMap();
            List<String> schoolOids = getSchools().getKeySet().stream().collect(Collectors.toList());

            X2Criteria crsInclusionCriteria = new X2Criteria();
            crsInclusionCriteria.addEqualTo(OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictExtractor()),
                    COURSE_CODE_TYPE_HOMEROOM);

            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setSchoolOids(schoolOids)
                    .setExcludeStudent(OnStudent.FIELD_EXCLUDE_FROM_REPORTING)
                    .setExcludeSection(OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS)
                    .setSectionLimitingCriteria(crsInclusionCriteria)
                    .setCurrentContext(getCurrentContext());
            if (!StringUtils.isBlank(getDebugStudentOid())) {
                spanCriteria.setLimitingStudentOids(Arrays.asList(getDebugStudentOid()));
            }

            X2Criteria candidateCriteria =
                    CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());

            // load students with filterable
            FilterableFactory.create(getBroker(), getDictExtractor(), OnStudent.class,
                    candidateCriteria,
                    Arrays.asList(ToolBean.FIELD_OID));
            // load enrollments and student school
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                    ToolStudent.CHILD_STUDENT_ENROLLMENTS);
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                    ToolStudent.CHILD_STUDENT_SCHOOLS);

            Set<String> allStudentsOids = new HashSet();
            ToolBean.getCachedToolBeans(OnStudent.class).stream()
                    .map(student -> student.getEnrollmentSpans(getBroker(), true, true))
                    .flatMap(List::stream)
                    .filter(span -> {
                        // test school
                        if (!schoolOids.contains(span.getSchool().getOid())) {
                            return false;
                        }

                        // test date range
                        PlainDate spanStartDate = span.getFirstActiveInSessionDate();
                        PlainDate spanEndDate = span.getSpanEndDate();
                        if (getReportDate().before(spanStartDate) ||
                                (spanEndDate != null && getReportDate().after(spanEndDate))) {
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
                        allStudentsOids.add(span.getStudent().getOid());
                    });

            // remove unused students
            ToolBean.filterCachedToolBeans(OnStudent.class,
                    student -> allStudentsOids.contains(student.getOid()));

            // preload schedules
            ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class,
                    CriteriaHelper.buildStudentScheduleCriteria(spanCriteria));
            ToolBean.addAndCriteria(getBroker(), ToolStudentScheduleChange.class,
                    CriteriaHelper.buildStudentScheduleChangeCriteria(spanCriteria));
            ToolBean.preload(getBroker(), getDictExtractor(), null,
                    ToolStudentSchedule.PARENT_STUDENT);
            ToolBean.preload(getBroker(), getDictExtractor(), null,
                    ToolStudentScheduleChange.PARENT_STUDENT);

        }
        Set<OnStudent> students = m_schoolStudents.get(school.getOid());
        return FilterableFactory.createFilterableToolBeans((students == null ? Collections.EMPTY_LIST
                : students.stream()
                        .sorted((s1, s2) -> s1.getOid().compareTo(s2.getOid()))
                        .collect(Collectors.toList())));
    }

    /**
     * Checks if is day school.
     *
     * @param school SisSchool
     * @return true, if is day school
     */
    private boolean isElementarySchool(OnSchool school) {
        return m_elemSchoolStateCodes.contains(school.getSchoolLevelCodeState())
                && !m_excludedSpecialConditions.contains(school.getSpecialCondition());
    }

}
