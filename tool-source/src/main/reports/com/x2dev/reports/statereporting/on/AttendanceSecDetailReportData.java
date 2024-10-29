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
package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentPeriodAttendance;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.reports.statereporting.on.revised.OnReportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class AttendanceSecDetailReportData.
 */
public class AttendanceSecDetailReportData extends OnReportJavaSource {

    /**
     * The Class FSLSummaryRecord.
     */
    private static class AttendanceRecord {
        private Map<AttendanceFields, Object> m_attFieldValuePairs =
                new HashMap<AttendanceFields, Object>();

        /**
         * The Enum AttendanceFields.
         */
        private enum AttendanceFields {
            boardName,
            //
            boardNumber,
            //
            schoolName,
            //
            schoolNumber,
            //
            academicYears,
            //
            programName,
            //
            currentDate,
            //
            district,
            //
            classCode,
            //
            courseCode,
            //
            classesMissed,
            //
            timesLate,
            //
            oen,
            //
            nameView,
            //
            gradeToSort,
            //
            totalClassesStudent,
            //
            totalClassesCourse,
            //
            classStartDate,
            //
            classEndDate,
            //
            courseStartDate,
            //
            courseEndDate;

            /**
             * Gets the.
             *
             * @param key String
             * @return AttendanceFields
             */
            public static AttendanceFields get(String key) {
                for (AttendanceFields field : AttendanceFields.values()) {
                    if (key.equals(field.toString())) {
                        return field;
                    }
                }

                return null;
            }
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(AttendanceFields.district.toString(),
                        AttendanceFields.boardNumber.toString(),
                        AttendanceFields.schoolName.toString(),
                        AttendanceFields.classCode.toString(),
                        AttendanceFields.courseCode.toString(),
                        AttendanceFields.oen.toString());

        private static ValueByKeyResolver<AttendanceRecord> s_valueResolver =
                new ValueByKeyResolver<AttendanceRecord>() {
                    @Override
                    public Object getValue(String key, AttendanceRecord entity) {
                        Object rerutnValue = null;
                        if (AttendanceFields.get(key) != null) {
                            rerutnValue = entity.getAttendanceField(AttendanceFields.valueOf(key));
                        }
                        return rerutnValue;
                    }
                };

        /**
         * Gets the attendance field.
         *
         * @param key AttendanceFields
         * @return Object
         */
        Object getAttendanceField(AttendanceFields key) {
            return m_attFieldValuePairs.get(key);
        }

        /**
         * Sets the attendance field.
         *
         * @param field AttendanceFields
         * @param value Object
         */
        void setAttendanceField(AttendanceFields field, Object value) {
            m_attFieldValuePairs.put(field, value);
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
            return getContextByDate(getReportDate()).getEndDate();
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
     * Constants.
     */
    public static final String STD_PERIOD_ATT_G_DAY_CODE = "G";
    public static final String STD_PERIOD_ATT_C_CODE = "C";

    /**
     * Class members.
     */
    private Set<String> m_allStudentsOidsList;
    private GradesHelper m_gradesHelper;
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private SimpleDateFormat m_formatterClassDates = new SimpleDateFormat("yyyy/MM/dd");
    private Map<String, List<ToolStudentPeriodAttendance>> m_schoolPATS;
    private SchoolDateRangeProvider m_sklDateRangeProvider;

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        Filterable<OnSchool> filterableSkls = null;
        if (isSchoolContext()) {
            filterableSkls = FilterableFactory.create(Arrays.asList(m_sklDateRangeProvider.getOnSchool()),
                    Arrays.asList(X2BaseBean.COL_OID), Filterable.PredefinedResolver.X2BASE_BEAN);
        } else {
            filterableSkls = getSchools();
        }
        Filterable<AttendanceRecord> attRecordsFilterable =
                filterableSkls.map(new Mapper<OnSchool, AttendanceRecord>() {
                    @Override
                    public Filterable<AttendanceRecord> map(OnSchool school) {
                        initializeHelpersForSchool(school);
                        return getRecords(school);
                    }
                });
        List<AttendanceRecord> sortedRecords = attRecordsFilterable.extractSorted(Arrays.asList(
                AttendanceRecord.AttendanceFields.district.toString(),
                AttendanceRecord.AttendanceFields.boardNumber.toString(),
                AttendanceRecord.AttendanceFields.schoolNumber.toString(),
                AttendanceRecord.AttendanceFields.classCode.toString(),
                AttendanceRecord.AttendanceFields.courseCode.toString(),
                AttendanceRecord.AttendanceFields.gradeToSort.toString(),
                AttendanceRecord.AttendanceFields.nameView.toString(),
                AttendanceRecord.AttendanceFields.oen.toString()), true);
        for (AttendanceRecord record : sortedRecords) {
            grid.append();
            for (AttendanceRecord.AttendanceFields field : record.m_attFieldValuePairs.keySet()) {
                grid.set(field.toString(), record.m_attFieldValuePairs.get(field));
            }
        }
        grid.beforeTop();
        if (getErrorsLog().length() > 0) {
            addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
        }
        addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
        addParameter(REPORT_PARAM_AS_OF_DATE, s_asOfDateFormat.format(getReportDate()));
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
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Gets the records.
     *
     * @param school SisSchool
     * @return Filterable
     */
    private Filterable<AttendanceRecord> getRecords(OnSchool school) {
        Map<String, AttendanceRecord> recordByKey = new HashMap<String, AttendanceRecord>();
        try {
            String schoolName = school.getName();
            String schoolNumber = school.getBsid();
            if (StringUtils.isEmpty(schoolNumber)) {
                throw new RuntimeException(
                        "BSID is not defined for school " + school.getName());
            }
            ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
            String boardNumber = board == null ? "Cannot determine board by school number" : board.getCode();
            String boardName = board == null ? "Cannot determine board by school number" : board.getDescription();
            String academicYears = m_sklDateRangeProvider.getContextByDate(getReportDate()).getContextId();
            String programName = "Aspen";
            String currentDate = m_formatter
                    .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
            Collection<ToolStudentPeriodAttendance> sectionPats =
                    getSchoolPats(school);
            if (sectionPats != null && !sectionPats.isEmpty()) {
                Map<String, List<StudentScheduleSpan>> sscSpansByStdOid =
                        new HashMap<String, List<StudentScheduleSpan>>();
                for (ToolStudentPeriodAttendance pat : sectionPats) {
                    if (pat.getAbsentIndicator() || pat.getTardyIndicator()) {
                        StudentScheduleSpan spanToOperate = null;
                        OnSection mst = (OnSection) pat.getSection(getBroker());
                        OnStudent std = (OnStudent) pat.getStudent(getBroker());
                        String key = mst.getCourseView() + mst.getCourseNumber() + std.getOid();
                        List<StudentScheduleSpan> scheduleSpans = null;
                        scheduleSpans = sscSpansByStdOid.get(std.getOid());
                        if (scheduleSpans == null) {
                            scheduleSpans = std.getStudentScheduleSpans(getBroker());
                            sscSpansByStdOid.put(std.getOid(), scheduleSpans);
                        }
                        if (scheduleSpans != null && !scheduleSpans.isEmpty()) {
                            Optional<StudentScheduleSpan> sscs = scheduleSpans.stream()
                                    .filter(span -> span.getSection().getOid().equals(mst.getOid()))
                                    .findFirst();
                            if (sscs != null && sscs.isPresent()) {
                                spanToOperate = sscs.get();
                            } else {
                                continue;
                            }
                        }
                        AttendanceRecord attRecord = recordByKey.get(key);
                        if (attRecord == null) {
                            attRecord = new AttendanceRecord();
                            recordByKey.put(key, attRecord);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.academicYears,
                                    academicYears);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.boardName, boardName);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.boardNumber, boardNumber);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.currentDate, currentDate);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.programName, programName);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.schoolName, schoolName);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.schoolNumber, schoolNumber);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.classCode,
                                    mst.getCourseView());
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.courseCode,
                                    mst.getCourseNumber());
                            attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.oen,
                                    std.getOenRaw());
                            attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.nameView,
                                    getStudentNameLastFirstMiddle(std));
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.classesMissed,
                                    Integer.valueOf(0));
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.timesLate,
                                    Integer.valueOf(0));
                            String grade = m_gradesHelper.getGradeLevel(std.getYog());
                            String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                                    ALIAS_DDX_NUM_GRADE, getDictExtractor().getDictionary(DDX_ID_GRADES));
                            if (!StringUtils.isEmpty(numGradeLevel)) {
                                attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.gradeToSort,
                                        numGradeLevel);
                            }
                        }
                        if (pat.getAbsentIndicator()) {
                            attRecord
                                    .setAttendanceField(
                                            AttendanceRecord.AttendanceFields.classesMissed,
                                            Integer.valueOf(((Integer) attRecord.getAttendanceField(
                                                    AttendanceRecord.AttendanceFields.classesMissed))
                                                            .intValue()
                                                    + 1));
                        }
                        if (pat.getTardyIndicator()) {
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.timesLate,
                                    Integer.valueOf(((Integer) attRecord
                                            .getAttendanceField(
                                                    AttendanceRecord.AttendanceFields.timesLate))
                                                            .intValue()
                                            + 1));
                        }
                        if (spanToOperate != null) {
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.courseStartDate,
                                    spanToOperate.getEntryDate() != null
                                            ? m_formatterClassDates
                                                    .format(spanToOperate.getEntryDate())
                                            : null);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.courseEndDate,
                                    spanToOperate.getExitDate() != null
                                            ? m_formatterClassDates
                                                    .format(spanToOperate.getExitDate())
                                            : null);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.totalClassesStudent,
                                    mst.getTotalNumberPeriods(spanToOperate
                                            .getEntryDate(), spanToOperate.getExitDate(), getBroker()));
                        }
                        Range sectionDateRange = mst.getDateRange(getBroker());
                        if (sectionDateRange != null) {
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.classStartDate,
                                    sectionDateRange.getStart() != null
                                            ? m_formatterClassDates
                                                    .format(sectionDateRange.getStart())
                                            : null);
                            attRecord.setAttendanceField(
                                    AttendanceRecord.AttendanceFields.classEndDate,
                                    sectionDateRange.getEnd() != null
                                            ? m_formatterClassDates
                                                    .format(sectionDateRange.getEnd())
                                            : null);
                        }
                        attRecord.setAttendanceField(
                                AttendanceRecord.AttendanceFields.totalClassesCourse,
                                mst.getTotalNumberPeriods(null, null, getBroker()));
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (recordByKey.isEmpty()) {
                recordByKey.put("___dummy___", new AttendanceRecord());
            }
            logError(e.getMessage());
        }
        return FilterableFactory.create(recordByKey.values().stream().collect(Collectors.toList()),
                AttendanceRecord.s_uniqueFields,
                AttendanceRecord.s_valueResolver);
    }

    /**
     * Gets the school pats.
     *
     * @param school OnSchool
     * @return Sets the
     */
    private Collection<ToolStudentPeriodAttendance> getSchoolPats(OnSchool school) {
        if (m_schoolPATS == null) {
            X2Criteria studentPeriodAttendanceCriteria = new X2Criteria();
            studentPeriodAttendanceCriteria.addGreaterOrEqualThan(
                    ToolStudentPeriodAttendance.FIELD_DATE.resolve(getDictExtractor()), getReportDate());
            studentPeriodAttendanceCriteria.addLessOrEqualThan(
                    ToolStudentPeriodAttendance.FIELD_DATE.resolve(getDictExtractor()),
                    m_sklDateRangeProvider.getEndDate());
            studentPeriodAttendanceCriteria.addIn(
                    ToolStudentPeriodAttendance.FIELD_SCHOOL_OID.resolve(getDictExtractor()),
                    getSchools().getKeySet().stream().collect(Collectors.toList()));
            studentPeriodAttendanceCriteria
                    .addIn(ToolStudentPeriodAttendance.FIELD_STUDENT_OID.resolve(getDictExtractor()),
                            getStudentsOids());

            X2Criteria absentDefinitionCriteria = new X2Criteria();
            absentDefinitionCriteria.addEqualTo(
                    ToolStudentPeriodAttendance.FIELD_ABSENT_INDICATOR.resolve(getDictExtractor()),
                    BooleanAsStringConverter.TRUE);
            absentDefinitionCriteria.addOrEqualTo(
                    ToolStudentPeriodAttendance.FIELD_TARDY_INDICATOR.resolve(getDictExtractor()),
                    BooleanAsStringConverter.TRUE);

            absentDefinitionCriteria.addOrEqualTo(
                    ToolStudentPeriodAttendance.FIELD_OTHER_CODE.resolve(getDictExtractor()),
                    STD_PERIOD_ATT_G_DAY_CODE);
            absentDefinitionCriteria.addOrEqualTo(
                    ToolStudentPeriodAttendance.FIELD_OTHER_CODE.resolve(getDictExtractor()), STD_PERIOD_ATT_C_CODE);
            absentDefinitionCriteria.addOrEqualTo(
                    ToolStudentPeriodAttendance.FIELD_OTHER_CODE_02.resolve(getDictExtractor()),
                    STD_PERIOD_ATT_G_DAY_CODE);
            absentDefinitionCriteria.addOrEqualTo(
                    ToolStudentPeriodAttendance.FIELD_OTHER_CODE_02.resolve(getDictExtractor()), STD_PERIOD_ATT_C_CODE);

            studentPeriodAttendanceCriteria.addAndCriteria(absentDefinitionCriteria);

            FilterableFactory.create(getBroker(), getDictExtractor(), ToolStudentPeriodAttendance.class,
                    studentPeriodAttendanceCriteria, null);

            m_schoolPATS = ToolBean.getCachedToolBeans(ToolStudentPeriodAttendance.class).stream()
                    .collect(Collectors.groupingBy(pat -> pat.getSchool(getBroker()).getOid()));
        }
        return m_schoolPATS.get(school.getOid());
    }

    /**
     * Gets the students.
     *
     * @return Sets the
     */
    private Set<String> getStudentsOids() {
        if (m_allStudentsOidsList == null) {
            m_allStudentsOidsList = new HashSet<>();
            ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getHistoricalCutoffDate());
            ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, getReportDate());
            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setSchoolOids(getSchools().getKeySet().stream().collect(Collectors.toList()))
                    .setExcludeStudent(OnStudent.FIELD_EXCLUDE_FROM_REPORTING)
                    .setExcludeSection(OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS)
                    .setIncludeSecondarySpans(false)
                    .setCurrentContext(m_sklDateRangeProvider.getContextByDate(getReportDate()));
            X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
            candidateCriteria.addNotEmpty(SisBeanPaths.STUDENT.stateId().getPath(), getBroker().getPersistenceKey());

            // load students with filterable
            FilterableFactory.create(getBroker(), getDictExtractor(), OnStudent.class, candidateCriteria, null);

            // load enrollments and student school
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                    ToolStudent.CHILD_STUDENT_ENROLLMENTS);

            DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));
            DistrictManager.setStudentScheduleSpanFactory(new OnStudentScheduleSpanFactory());
            ToolBean.getCachedToolBeans(OnStudent.class).stream()
                    .map(student -> student.getEnrollmentSpans(getBroker(), true, true))
                    .flatMap(List::stream)
                    .filter(span -> {
                        // test school
                        if (!getSchools().getKeySet().contains(span.getSchool().getOid())) {
                            return false;
                        }

                        // test date range
                        PlainDate spanStartDate = span.getFirstActiveInSessionDate();
                        PlainDate spanEndDate = span.getSpanEndDate();
                        return !getReportDate().before(spanStartDate)
                                && (spanEndDate == null || !getReportDate().after(spanEndDate));
                    })
                    .filter(span -> {
                        ToolEnrollment lastEnrolment = span.getAllEnrollmentsDescend().isEmpty() ?
                                null : span.getAllEnrollmentsDescend().get(0);
                        return lastEnrolment != null && !lastEnrolment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL);
                    })
                    .forEach(span -> {
                        m_allStudentsOidsList.add(span.getStudent().getOid());
                    });

            // preload schedules
            ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class,
                    CriteriaHelper.buildStudentScheduleCriteria(spanCriteria));
            ToolBean.addAndCriteria(getBroker(), ToolStudentScheduleChange.class,
                    CriteriaHelper.buildStudentScheduleChangeCriteria(spanCriteria));
            ToolBean.preload(getBroker(), getDictExtractor(), null, ToolStudentSchedule.PARENT_SECTION);
            ToolBean.preload(getBroker(), getDictExtractor(), null, ToolStudentScheduleChange.PARENT_SECTION);

        }
        return m_allStudentsOidsList;
    }

    /**
     * Inits the grades map.
     */
    private void initGradesMap() {
        DataDictionaryField dictionaryField =
                getDictExtractor().getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
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
     * Initialize helpers.
     *
     * @param school SisSchool
     */
    private void initializeHelpersForSchool(OnSchool school) {
        m_sklDateRangeProvider = new SchoolDateRangeProvider();
        m_sklDateRangeProvider.setBroker(getBroker());
        m_sklDateRangeProvider.setSchool(school);
        m_gradesHelper = new GradesHelper(m_sklDateRangeProvider);
    }

    /**
     * Register tool beans.
     */
    private void registerToolBeans() {
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnSection.class);
        ToolBean.registerClass(ToolStudentPeriodAttendance.class);
        ToolBean.registerClass(ToolStudentSchedule.class);
        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());
    }
}
