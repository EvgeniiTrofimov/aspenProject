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
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentAttendance;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentPeriodAttendance;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
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
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class AttendanceElemSummaryReportData.
 */
public class AttendanceElemDetailReportData extends OnReportJavaSource {

    /**
     * The Class AttendanceRecord.
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
            grade,
            //
            gradeToDisplay,
            //
            gradeToSort,
            //
            daysAbsent,
            //
            timesLate,
            //
            oen,
            //
            studentName;


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

        /** The s unique fields. */
        private static List<String> s_uniqueFields =
                Arrays.asList(AttendanceFields.district.toString(),
                        AttendanceFields.boardNumber.toString(),
                        AttendanceFields.schoolName.toString(),
                        AttendanceFields.studentName.toString(),
                        AttendanceFields.oen.toString());


        /** The s value resolver. */
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
     * Constants
     */
    private static final List SKL_CODES_ELEM = Arrays.asList("01");

    /**
     * Class members.
     */
    private GradesHelper m_gradesHelper;
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private Map<String, Set<OnStudent>> m_schoolStudents;
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
            filterableSkls = getSchools().filter(new Filter<OnSchool>() {
                @Override
                public boolean isFiltered(OnSchool skl) {
                    return SKL_CODES_ELEM.contains(skl.getSchoolLevelCode()) && !skl.getArchiveIndicator()
                            && !skl.getInactiveIndicator();
                }
            });
        }

        Filterable<AttendanceRecord> attRecordsFilterable =
                filterableSkls.map(new Mapper<OnSchool, AttendanceRecord>() {
                    @Override
                    public Filterable<AttendanceRecord> map(OnSchool school) {
                        initializeHelpersForSchool(school);
                        return getRecords(getStudents(school));
                    }
                });
        List<AttendanceRecord> sortedRecords = attRecordsFilterable.extractSorted(Arrays.asList(
                AttendanceRecord.AttendanceFields.district.toString(),
                AttendanceRecord.AttendanceFields.schoolName.toString(),
                AttendanceRecord.AttendanceFields.boardNumber.toString(),
                AttendanceRecord.AttendanceFields.gradeToSort.toString(),
                AttendanceRecord.AttendanceFields.studentName.toString(),
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
     * @param students Filterable<OnStudent>
     * @return Filterable
     */
    private Filterable<AttendanceRecord> getRecords(Set<OnStudent> students) {
        List<AttendanceRecord> records = new ArrayList<AttendanceRecord>();
        try {
            String schoolName = m_sklDateRangeProvider.getSchool().getName();
            String schoolNumber =
                    m_sklDateRangeProvider.getSchool().getBsid();
            if (StringUtils.isEmpty(schoolNumber)) {
                throw new RuntimeException(
                        "BSID is not defined for school " + m_sklDateRangeProvider.getSchool().getName());
            }
            ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
            String boardNumber = board == null ? "Cannot determine board by school number" : board.getCode();
            String boardName = board == null ? "Cannot determine board by school number" : board.getDescription();
            String academicYears = getCurrentContext().getContextId();
            String programName = "Aspen";
            String currentDate = m_formatter
                    .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
            for (OnStudent student : students) {
                List<ToolStudentAttendance> absences = student.getAttendance(getBroker()).stream()
                        .filter(att -> att.getAbsentIndicator() || att.getAbsentIndicator02())
                        .sorted(new Comparator<ToolStudentAttendance>() {
                            @Override
                            public int compare(ToolStudentAttendance att1, ToolStudentAttendance att2) {
                                return att1.getDate().compareTo(att2.getDate());
                            }
                        }).collect(Collectors.toList());


                List<ToolStudentAttendance> lates = student.getAttendance(getBroker()).stream()
                        .filter(att -> att.getTardyIndicator() || att.getTardyIndicator02())
                        .sorted(new Comparator<ToolStudentAttendance>() {
                            @Override
                            public int compare(ToolStudentAttendance att1, ToolStudentAttendance att2) {
                                return att1.getDate().compareTo(att2.getDate());
                            }
                        }).collect(Collectors.toList());
                if (!absences.isEmpty() || !lates.isEmpty()) {
                    double absencesSum =
                            absences.stream().filter(abs -> abs.getPortionAbsent() != null)
                                    .mapToDouble(absence -> absence.getPortionAbsent().doubleValue()).sum();
                    int latesSum = lates.size();
                    String grade = m_gradesHelper.getGradeLevel(student.getYog());
                    AttendanceRecord attRecord = new AttendanceRecord();
                    records.add(attRecord);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.academicYears,
                            academicYears);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.boardName, boardName);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.boardNumber, boardNumber);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.currentDate, currentDate);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.programName, programName);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.schoolName, schoolName);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.schoolNumber, schoolNumber);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.grade, grade);
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.gradeToDisplay,
                            m_gradesMap.get(grade).getDescription());
                    String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                            ALIAS_DDX_NUM_GRADE, getDictExtractor().getDictionary(DDX_ID_GRADES));
                    if (!StringUtils.isEmpty(numGradeLevel)) {
                        attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.gradeToSort, numGradeLevel);
                    }
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.daysAbsent,
                            Double.valueOf(absencesSum));
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.timesLate,
                            Integer.valueOf(latesSum));
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.studentName,
                            getStudentNameLastFirstMiddle(student));
                    attRecord.setAttendanceField(AttendanceRecord.AttendanceFields.oen, student.getOenRaw());
                }
            }
        } catch (Exception e) {
            if (records.isEmpty()) {
                records.add(new AttendanceRecord());
            }
            logError(e.getMessage());
        }
        return FilterableFactory.create(records, AttendanceRecord.s_uniqueFields, AttendanceRecord.s_valueResolver);
    }

    /**
     * Gets the students.
     *
     * @param school OnSchool
     * @return Sets the
     */
    private Set<OnStudent> getStudents(OnSchool school) {
        if (m_schoolStudents == null) {
            m_schoolStudents = new HashMap<>();
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
            Set<String> allStudentsList = new HashSet();
            ToolBean.getCachedToolBeans(OnStudent.class).stream()
                    .map(student -> student.getEnrollmentSpans(getBroker(), true, true))
                    .flatMap(List::stream)
                    .filter(span -> {
                        // test school
                        if (!getSchools().getKeySet().contains(span.getSchool().getOid())) {
                            return false;
                        }
                        PlainDate spanStartDate = span.getFirstActiveInSessionDate();
                        PlainDate spanEndDate = span.getSpanEndDate();
                        return !getReportDate().before(spanStartDate)
                                && (spanEndDate == null || !getReportDate().after(spanEndDate));
                    })
                    .filter(Objects::nonNull)
                    .filter(span -> {
                        ToolEnrollment lastEnrolment = span.getAllEnrollmentsDescend().isEmpty() ?
                                null : span.getAllEnrollmentsDescend().get(0);
                        return lastEnrolment != null && !lastEnrolment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL);
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

            X2Criteria attCriteria = new X2Criteria();
            attCriteria.addGreaterOrEqualThan(ToolStudentAttendance.FIELD_DATE.resolve(getDictExtractor()),
                    getReportDate());
            attCriteria.addLessOrEqualThan(ToolStudentAttendance.FIELD_DATE.resolve(getDictExtractor()),
                    m_sklDateRangeProvider.getEndDate());
            ToolBean.addAndCriteria(getBroker(), ToolStudentAttendance.class, attCriteria);
            ToolBean.preload(getBroker(), getDictExtractor(), null, ToolStudent.CHILD_STUDENT_ATTENDANCE);
        }

        Set<OnStudent> students = m_schoolStudents.get(school.getOid());
        return students == null ? Collections.EMPTY_SET : students;
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
