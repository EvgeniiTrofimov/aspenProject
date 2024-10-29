/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.OrganizationChild;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolBroker;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Pair;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.sis.model.beans.UserDefinedTableD;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Class OnsisDmFteSummaryNew.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisDmFteSummary extends ToolJavaSource {
    /**
     * The Class Message.
     */
    private static class Message {

        /**
         * The Enum Field.
         */
        enum Field {
            MESSAGE, TYPE;
        }

        /**
         * The Enum Type.
         */
        enum Type {
            ERROR, INFO, HEADER;
        }

        private Map<Field, Object> m_values = new HashMap<>();

        /**
         * Error.
         *
         * @param errorText String
         * @return Message
         */
        static public Message error(String errorText) {
            Message error = new Message();
            error.set(Field.MESSAGE, errorText);
            error.set(Field.TYPE, Type.ERROR);
            return error;
        }

        /**
         * Message.
         *
         * @param messageText String
         * @return Message
         */
        static public Message message(String messageText) {
            Message message = new Message();
            message.set(Field.MESSAGE, messageText);
            message.set(Field.TYPE, Type.INFO);
            return message;
        }

        /**
         * Message.
         *
         * @param headerText String
         * @return Message
         */
        static public Message header(String headerText) {
            Message message = new Message();
            message.set(Field.MESSAGE, headerText);
            message.set(Field.TYPE, Type.HEADER);
            return message;
        }

        /**
         * Gets the.
         *
         * @param field Field
         * @return Object
         */
        public Object get(Field field) {
            return m_values.get(field);
        }

        /**
         * Gets the message.
         *
         * @return String
         */
        public String getMessage() {
            return (String) get(Message.Field.MESSAGE);
        }

        /**
         * Sets the.
         *
         * @param field Field
         * @param value Object
         */
        public void set(Field field, Object value) {
            m_values.put(field, value);
        }
    }

    /**
     * The Class MyFteRecord.
     */
    public static class MyFteRecord extends FteRecord {

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = FteRecord.FULL_DEFINITION
                .expandFilters(new Predicate<ToolBean>() {
                    @Override
                    public boolean test(ToolBean bean) {
                        MyFteRecord record = (MyFteRecord) bean;
                        Range<Date> range = (Range<Date>) ToolBean.getPreference(PREFERENCE_FTE_DATE_RANGE);
                        return range.contains(record.getFteDate());
                    }
                });


        /**
         * Instantiates a new my fte record.
         *
         * @param columns the columns
         * @param data the data
         */
        public MyFteRecord(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

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
            return OnsisDmFteSummary.this.getBroker();
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

    public static final String PREFERENCE_FTE_DATE_RANGE = "fteDateRange";

    private static final String DELIMITER_LINE = "-----------------";
    private static final List<String> COURSE_CODE_TYPES_INCLUDED = Arrays.asList(
            OnSection.COURSE_CODE_TYPE_MDC,
            OnSection.COURSE_CODE_TYPE_LDC,
            OnSection.COURSE_CODE_TYPE_DCC,
            OnSection.COURSE_CODE_TYPE_PLACEHOLDER);

    private static final List<String> HIGH_SCHOOL_STATE_CODES = Arrays.asList("02", "03");


    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_DEBUG_DETAIL = "debugDetail";
    private static final String INPUT_PARAM_DEBUG_STUDENT_OID = "debugStudentOid";
    private static final String INPUT_PARAM_RUN_OPTION = "runOption";
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";

    private static final String RUN_OPTION_COMMIT = "commit";
    private static final String RUN_OPTION_CSV = "csv";
    private static final String RUN_OPTION_REVIEW = "review";

    private static final Comparator<SubmissionType> s_submissionTypeComparator = new Comparator<SubmissionType>() {

        @Override
        public int compare(SubmissionType type0, SubmissionType type1) {
            // increasing order end date
            int result = type0.getPeriodEndDate().compareTo(type1.getPeriodEndDate());
            if (result == 0) {
                // decreasing order start date
                result = type1.getPeriodStartDate().compareTo(type0.getPeriodStartDate());
            }
            return result;
        }
    };

    private List<String> m_csvOutput = new ArrayList();
    private Range<Date> m_dateRange;
    private Boolean m_debugDetail;
    private transient DictionaryExtractor m_dictExtractor;
    private transient GradesHelper m_gradesHelper;
    private boolean m_isAllSchools = true;
    private final List<Message> m_messages = new ArrayList<>();
    private PlainDate m_reportDate;
    private String m_runOption = RUN_OPTION_REVIEW;
    private int m_scaleFte = 2; // default to 2
    private int m_scaleFteHc = 2; // default to 2
    private List<OnSchool> m_schoolCandidates;
    private transient Map<String, SchoolDateRangeProvider> m_schoolDateRangeProvider;
    private List<String> m_schoolOids;
    private List<OnSchool> m_schools;
    private Map<String, Set<OnStudent>> m_schoolStudents;

    /**
     * Gets the dict extractor.
     *
     * @return the dict extractor
     */
    public DictionaryExtractor getDictExtractor() {
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
    public GradesHelper getGradesHelper() {
        if (m_gradesHelper == null) {
            m_gradesHelper = new GradesHelper(getSchoolDateRangeProvider(null));
        }
        return m_gradesHelper;
    }

    /**
     * Gets the scale fte.
     *
     * @return the scale fte
     */
    public int getScaleFte() {
        return m_scaleFte;
    }

    /**
     * Gets the scale fte hc.
     *
     * @return the scale fte hc
     */
    public int getScaleFteHc() {
        return m_scaleFteHc;
    }

    /**
     * Gets the school date range provider.
     *
     * @param school the school
     * @return the school date range provider
     */
    public SchoolDateRangeProvider getSchoolDateRangeProvider(OnSchool school) {
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
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        if (getBroker() instanceof ToolBroker) {
            ((ToolBroker) getBroker()).turnAuditOn(getUser());
        }

        String runOption = (String) getParameter(INPUT_PARAM_RUN_OPTION);
        if (!StringUtils.isEmpty(runOption)) {
            m_runOption = runOption;
        }

        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
        if (isAllSchools != null) {
            m_isAllSchools = isAllSchools;
        }

        DataDictionaryField field = OnEnrollment.FIELD_FTE.getField(getDictExtractor());
        if (field != null) {
            m_scaleFte = field.getUserDecimal();
        }
        field = OnEnrollment.FIELD_FTE_HIGH_CREDIT.getField(getDictExtractor());
        if (field != null) {
            m_scaleFteHc = field.getUserDecimal();
        }

        DistrictManager.setOrganization(getOrganization());
        DistrictManager.setDefaultCalendarIds(Arrays.asList(OnsisConstants.DEFAULT_ONSIS_CALENDAR_IDS));
        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));
        DistrictManager.setStudentScheduleSpanFactory(new OnStudentScheduleSpanFactory());

        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());

        ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, getCurrentContext().getStartDate());
        ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, new PlainDate());
        ToolBean.setPreference(ToolBean.PREFERENCE_ENR_IGNORE_FOR_SPANS, new Predicate<OnEnrollment>() {
            @Override
            public boolean test(OnEnrollment enr) {
                return enr.isFteRecord();
            }
        });
        ToolBean.setPreference(PREFERENCE_FTE_DATE_RANGE, getDateRange());

        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnSchedule.class);
        ToolBean.registerClass(OnScheduleBell.class);
        ToolBean.registerClass(OnScheduleBellPeriod.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnSchoolCalendar.class);
        ToolBean.registerClass(OnSection.class);
        ToolBean.registerClass(OnStudent.class);
        ToolBean.registerClass(OnStudentSchool.class);
        ToolBean.registerClass(OnTranscript.class);
        ToolBean.registerClass(OnTranscriptColumnDefinition.class);
        ToolBean.registerClass(MyFteRecord.class);
        ToolBean.registerClass(FteMonthly.class);
    }

    /**
     * Run.
     *
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        ArrayList<Message> messages = new ArrayList<>();
        if (RUN_OPTION_CSV.equals(m_runOption)) {
            Procedure proc = (Procedure) getJob().getTool();
            m_csvOutput.add(proc.getComment());
        }
        try {
            executeCalculations();
        } catch (Exception e) {
            addError("run():" + LoggerUtils.convertThrowableToString(e));
        }

        if (RUN_OPTION_CSV.equals(m_runOption)) {
            String output = m_csvOutput.stream().collect(Collectors.joining("\n"));

            try {
                ByteArrayInputStream inputStream = new ByteArrayInputStream(output.getBytes());
                try {
                    StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
                } finally {
                    inputStream.close();
                }
            } catch (FileNotFoundException fnfe) {
                throw new X2BaseException(fnfe);
            } catch (IOException ioe) {
                throw new X2BaseException(ioe);
            }
        } else {
            showOutput(messages);
        }
    }

    /**
     * Adds the error.
     *
     * @param errorText String
     */
    private void addError(String errorText) {
        m_messages.add(Message.error("ERROR: " + errorText));
    }

    /**
     * Adds the message.
     *
     * @param message String
     */
    private void addMessage(String message) {
        m_messages.add(Message.message(message));
    }

    /**
     * Adds the student message.
     *
     * @param student SisStudent
     * @param studentMessage String
     */
    private void addStudentMessage(SisStudent student, String studentMessage) {
        m_messages
                .add(Message.message("[" + student.getNameView() + "]: " + studentMessage));
    }

    /**
     * Adds the student message.
     *
     * @param student SisStudent
     * @param studentMessage String
     */
    private void addStudentMessage(ToolStudent student, String studentMessage) {
        m_messages
                .add(Message.message("[" + student.getNameView() + "]: " + studentMessage));
    }

    /**
     * Append line.
     *
     * @param output StringBuilder
     * @param message String
     */
    private void appendLine(StringBuilder output, String message) {
        output.append(message);
        output.append("\r\n");
    }

    /**
     * Append message block.
     *
     * @param builder StringBuilder
     * @param message String
     * @return StringBuilder
     */
    private StringBuilder appendMessageBlock(StringBuilder builder, String message) {
        return builder.append("[").append(message).append("]");
    }

    /**
     * Calculate average for alias.
     *
     * @param rawRecords the raw records
     * @param alias the alias
     * @param valuesMap the values map
     */
    private void calculateAverageForColumn(Collection<FteCalculation> rawRecords,
                                           ToolBeanColumn alias,
                                           Map<String, Object> valuesMap) {

        double average = rawRecords.stream()
                .filter(fte -> !StringUtils.isBlank(fte.getByColumn(alias)))
                .mapToDouble(fte -> Double.valueOf(fte.getByColumn(alias)))
                .average()
                .orElse(0.0);

        valuesMap.put(alias.getAlias(), BigDecimal.valueOf(average));
    }

    /**
     * Calculate fte.
     *
     * @param minutesOfInstruction the minutes of instruction
     * @param threshold the threshold
     * @return the big decimal
     */
    private BigDecimal calculateFte(long minutesOfInstruction, int threshold) {
        BigDecimal fte;
        if (minutesOfInstruction >= threshold) {
            fte = BigDecimal.valueOf(1.0d);
        } else {
            int scale = 2;
            fte = BigDecimal.valueOf(minutesOfInstruction).divide(BigDecimal.valueOf(FteCalculation.FTE_MINUTES_FT_300),
                    scale,
                    RoundingMode.HALF_DOWN);
        }
        return fte;
    }

    /**
     * Calculate fte values.
     *
     * @param submissionType the submission type
     * @param studentFteCalculations the student fte calculations
     * @param student the student
     * @param school the school
     * @param dictExtractor the dict extractor
     * @param broker the broker
     * @param debugOutput the debug output
     * @return the map
     */
    private Map<String, Object> calculateFteValues(SubmissionType submissionType,
                                                   Map<PlainDate, FteCalculation> studentFteCalculations,
                                                   OnStudent student,
                                                   OnSchool school,
                                                   DictionaryExtractor dictExtractor,
                                                   X2Broker broker,
                                                   StringBuilder debugOutput) {
        Map<String, Object> valuesMap = new HashMap<>();
        if (debugOutput != null) {
            debugOutput.append("calculateFteValues begins\n");
        }

        int scheduleMode = getScheduleMode(student, school, submissionType, broker);
        if (debugOutput != null) {
            debugOutput.append("scheduleMode: " + scheduleMode + "\n");
        }
        PlainDate countDate = submissionType.getCountDate();
        Collection<PlainDate> countDates = Arrays.asList(countDate);
        if (scheduleMode > 2) {
            countDates = submissionType.getCountDates(scheduleMode);
        }

        if (debugOutput != null) {
            debugOutput.append("countDates: ["
                    + countDates.stream().map(Object::toString).collect(Collectors.joining(",")) + "]\n");
        }
        if (countDates.isEmpty()) {
            if (debugOutput != null) {
                debugOutput.append("calculateFteValues ends no count dates\n");
            }
            return valuesMap;
        }

        Map<String, List<Pair<String, FteCalculation>>> rawMap = countDates.stream()
                .map(dateOfInterest -> Pair.of(
                        student.getBoardResidentStatus(broker, school, dateOfInterest, false, true),
                        findFteRecord(studentFteCalculations.values(), dateOfInterest, student, school,
                                dictExtractor, broker)))
                .filter(pair -> !StringUtils.isEmpty(pair.getLeft()))
                .collect(Collectors.groupingBy(pair -> pair.getLeft()));

        if (rawMap.isEmpty()) {
            if (debugOutput != null) {
                debugOutput.append("calculateFteValues ends no raw records\n");
            }
            return valuesMap;
        }

        if (debugOutput != null) {
            debugOutput.append("rawMap:\n");
            for (Entry<String, List<Pair<String, FteCalculation>>> entry : rawMap.entrySet()) {
                debugOutput.append("Board Resident Status: " + entry.getKey() + "\n");
                debugOutput.append(entry.getValue().stream().map(pair -> pair.getRight()).map(Object::toString)
                        .collect(Collectors.joining("\n")));
                debugOutput.append("\n");
            }
        }

        Collection<FteCalculation> rawRecords = Collections.EMPTY_LIST;
        String boardResidentStatus = OnsisConstants.VALUE_STU_BRD_RES_STAT_PUPIL_OF_BOARD;
        List<Pair<String, FteCalculation>> rawPairs = rawMap.get(OnsisConstants.VALUE_STU_BRD_RES_STAT_PUPIL_OF_BOARD);
        if (rawPairs == null) {
            // Find the board resident status with the most total minutes of instruction
            boardResidentStatus = rawMap.values().stream()
                    .sorted(new Comparator<List<Pair<String, FteCalculation>>>() {
                        @Override
                        public int compare(List<Pair<String, FteCalculation>> o1,
                                           List<Pair<String, FteCalculation>> o2) {
                            int o1Minutes = o1.stream().map(pair -> {
                                FteCalculation calc = pair.getRight();
                                long minutes = ((Long) calc.get(FteCalculation.Field.MINUTES_TO_SET)).longValue();
                                minutes += ((Long) calc.get(FteCalculation.Field.HIGH_CREDIT_MINUTES)).longValue();
                                return Long.valueOf(minutes);
                            }).collect(Collectors.summingLong(Long::longValue)).intValue();
                            int o2Minutes = o2.stream().map(pair -> {
                                FteCalculation calc = pair.getRight();
                                long minutes = ((Long) calc.get(FteCalculation.Field.MINUTES_TO_SET)).longValue();
                                minutes += ((Long) calc.get(FteCalculation.Field.HIGH_CREDIT_MINUTES)).longValue();
                                return Long.valueOf(minutes);
                            }).collect(Collectors.summingLong(Long::longValue)).intValue();
                            return o1Minutes - o2Minutes;
                        }
                    }).findFirst().get().get(0).getLeft();
        }
        if (OnsisConstants.VALUE_STU_BRD_RES_STAT_PUPIL_OF_BOARD.equals(boardResidentStatus)) {
            // Use only student of the board records
            rawRecords = rawPairs.stream().map(pair -> pair.getRight()).collect(Collectors.toList());
        } else {
            // Use all FTE records
            rawRecords = rawMap.values().stream()
                    .flatMap(listPairs -> listPairs.stream().map(pair -> pair.getRight()))
                    .collect(Collectors.toList());
        }
        while (rawRecords.size() < countDates.size()) {
            if (debugOutput != null) {
                debugOutput.append("Adding empty raw record\n");
            }
            rawRecords.add(getEmptyRecord(countDate, student, school));
        }

        if (debugOutput != null) {
            debugOutput.append("Final BoardResidentStatus: " + boardResidentStatus + "\n");
            debugOutput.append("Final Records: \n"
                    + rawRecords.stream().map(Object::toString).collect(Collectors.joining("\n")) + "\n");
        }

        Collection<PlainDate> residentStatusDates = new ArrayList<>(countDates);

        residentStatusDates.add(countDate);
        residentStatusDates.add(submissionType.getPeriodStartDate());

        valuesMap.put(FteMonthly.FIELD_SB_RESIDENT_STATUS.getAlias(), boardResidentStatus);

        // calculateAverageForAlias(rawRecords, ALIAS_UDD_FTE, valuesMap);
        // calculateAverageForAlias(rawRecords, ALIAS_UDD_FTE_HIGH_CREDIT, valuesMap);
        calculateAverageForColumn(rawRecords, FteMonthly.FIELD_MINUTES, valuesMap);
        calculateAverageForColumn(rawRecords, FteMonthly.FIELD_MINUTES_HIGH_CREDIT, valuesMap);

        int threshold = FteCalculation.FTE_MINUTES_THRESHOLD_210;

        long minutesOfInstruction = ((BigDecimal) valuesMap.get(FteMonthly.FIELD_MINUTES.getAlias())).longValue();
        long minutesOfInstructionHighCredit =
                ((BigDecimal) valuesMap.get(FteMonthly.FIELD_MINUTES_HIGH_CREDIT.getAlias())).longValue();

        long totalMinutesOfInstruction = minutesOfInstruction + minutesOfInstructionHighCredit;
        if (totalMinutesOfInstruction >= threshold) {
            valuesMap.put(FteMonthly.FIELD_REGISTER.getAlias(), FteCalculation.EnrolmentRegister.FT.toString());
        } else {
            valuesMap.put(FteMonthly.FIELD_REGISTER.getAlias(), FteCalculation.EnrolmentRegister.PT.toString());
        }

        BigDecimal fte = calculateFte(minutesOfInstruction, FteCalculation.FTE_MINUTES_THRESHOLD_210);
        BigDecimal highCreditFte =
                calculateHighCreditFte(fte, minutesOfInstruction, FteCalculation.FTE_MINUTES_THRESHOLD_210,
                        minutesOfInstructionHighCredit);

        int scale = 2;

        valuesMap.put(FteMonthly.FIELD_FTE.getAlias(), fte.setScale(scale, RoundingMode.HALF_UP));
        valuesMap.put(FteMonthly.FIELD_FTE_HIGH_CREDIT.getAlias(), highCreditFte.setScale(scale, RoundingMode.HALF_UP));
        valuesMap.put(FteMonthly.FIELD_MINUTES.getAlias(), Long.valueOf(minutesOfInstruction));
        valuesMap.put(FteMonthly.FIELD_MINUTES_HIGH_CREDIT.getAlias(), Long.valueOf(minutesOfInstructionHighCredit));

        ToolOrganization org = school.getOrganization1(broker);
        ToolDistrictContext currentContext = org.getCurrentContext(broker);

        valuesMap.put(FteMonthly.FIELD_SCHOOL_YEAR.getAlias(), currentContext.getContextId());
        valuesMap.put(FteMonthly.FIELD_MONTH.getAlias(), submissionType.getSubmissionPeriodCode());
        valuesMap.put(FteMonthly.FIELD_REPORT_DATE.getAlias(), submissionType.getPeriodEndDate());
        valuesMap.put(FteMonthly.FIELD_SCHEDULE_MODE.getAlias(), Integer.toString(scheduleMode));

        if (debugOutput != null) {
            debugOutput.append("calculateFteValues ends normally\n");
        }
        return valuesMap;
    }

    /**
     * Calculate high credit fte.
     *
     * @param fte the fte
     * @param minutesOfInstruction the minutes of instruction
     * @param threshold the threshold
     * @param minutesOfInstructionHighCredit the minutes of instruction high credit
     * @return the big decimal
     */
    private BigDecimal calculateHighCreditFte(BigDecimal fte,
                                              long minutesOfInstruction,
                                              int threshold,
                                              long minutesOfInstructionHighCredit) {
        BigDecimal highCreditFte;
        if (minutesOfInstruction >= threshold) {
            highCreditFte = BigDecimal.valueOf(0d);
        } else {
            long totalMinutesOfInstruction = minutesOfInstruction + minutesOfInstructionHighCredit;
            if (totalMinutesOfInstruction >= threshold) {
                highCreditFte = BigDecimal.valueOf(1.0d).subtract(fte);
            } else {
                int scale = 2;
                highCreditFte =
                        BigDecimal.valueOf(minutesOfInstructionHighCredit).divide(
                                BigDecimal.valueOf(FteCalculation.FTE_MINUTES_FT_300), scale, RoundingMode.HALF_DOWN);
            }
        }
        return highCreditFte;
    }

    /**
     * Delete bean.
     *
     * @param bean the bean
     * @param student
     */
    private void deleteBean(ToolBean bean, ToolStudent student) {
        addStudentMessage(student, "Delete " + bean.toString());

        if (RUN_OPTION_COMMIT.equals(m_runOption)) {
            ToolBean.removeCachedToolBean(FteRecord.class, bean.getOid(), null);
            getBroker().deleteBean(bean.getX2Bean());
        }
    }

    /**
     * Return an empty record.
     *
     * @param countDate the count date
     * @param student the student
     * @param school the school
     * @return FteCalculation
     */
    private FteCalculation getEmptyRecord(PlainDate countDate,
                                          OnStudent student,
                                          OnSchool school) {
        FteCalculation emptyRecord = new FteCalculation(countDate, student, school);

        emptyRecord.set(FteCalculation.Field.ADE, Integer.valueOf(0));
        emptyRecord.set(FteCalculation.Field.FTE, BigDecimal.ZERO);
        emptyRecord.set(FteCalculation.Field.HIGH_CREDIT_FTE, BigDecimal.ZERO);
        emptyRecord.set(FteCalculation.Field.MINUTES_TO_SET, Long.valueOf(0l));
        emptyRecord.set(FteCalculation.Field.HIGH_CREDIT_MINUTES, Long.valueOf(0l));

        return emptyRecord;
    }

    /**
     * The school, date, record type, FTE, minutes.
     *
     * @param udc the udc
     * @return the string
     */
    private String enrollmentDetails(UserDefinedTableC udc) {
        StringBuilder builder = new StringBuilder();

        try {
            PlainDate dateOfInterest =
                    (PlainDate) getDictExtractor()
                            .getAliasAsJavaType(udc, FteRecord.FIELD_FTE_DATE.getAlias(), FteRecord.DDX_ID);

            appendMessageBlock(builder, udc.getOid());
            appendMessageBlock(builder,
                    getSchools().stream()
                            .filter(skl -> skl.getOid().equals(udc.getSchoolOid()))
                            .map(OnSchool::getName).findFirst().orElse("No school found"));
            appendMessageBlock(builder, dateOfInterest.toString());

            FteCalculation fteCalculation = new FteCalculation(dateOfInterest, udc);

            fteCalculation.readValuesFromStore(getDictExtractor());

            String enrRegisterToSet = (String) fteCalculation.get(FteCalculation.Field.ENROLLMENT_REGISTER);
            BigDecimal fte = (BigDecimal) fteCalculation.get(FteCalculation.Field.FTE);
            BigDecimal highCreditFte = (BigDecimal) fteCalculation.get(FteCalculation.Field.HIGH_CREDIT_FTE);
            Long totalMinutesToSet = (Long) fteCalculation.get(FteCalculation.Field.MINUTES_TO_SET);
            Long highCreditMinutes = (Long) fteCalculation.get(FteCalculation.Field.HIGH_CREDIT_MINUTES);

            builder.append(", FT/PT: " + enrRegisterToSet);
            builder.append(", FTE: " + fte);
            builder.append(", FTE (HC): " + highCreditFte);
            builder.append(", Minutes: " + totalMinutesToSet);
            builder.append(", Minutes (HC): " + highCreditMinutes);
        } catch (X2BaseException e) {
            addError("enrollmentDetails:" + LoggerUtils.convertThrowableToString(e));
        }

        return builder.toString();
    }

    /**
     * Execute calculation:
     *
     * 0. Get all Student Schedule Spans for the year by student from History Helper
     *
     * 1. For a single student, walk the schedule spans,
     * capturing dates of interest into TreeSet<PlainDate> (unique and in order)
     *
     * 2. Collect current FTE values for the student into a HasMap<PlainDate, FteCalculation>
     *
     * 3. For each date, call getScheduleSpansForDate()
     * and build FteCalculation into a HasMap<PlainDate, FteCalculation>
     *
     * 4. Diff the HashMaps and ADD/UPDATE/DELETE as appropriate
     */
    private void executeCalculations() {
        OnsisConstants.clearTimeByGetters();
        Collection<OnSchool> schools = getSchools();
        for (OnSchool school : schools) {
            Collection<OnStudent> schoolStudents = getStudents(school).stream()
                    .sorted(new Comparator<OnStudent>() {

                        @Override
                        public int compare(OnStudent std1, OnStudent std2) {
                            int value = StringUtils.unNullify(std1.getStateId())
                                    .compareTo(StringUtils.unNullify(std2.getStateId()));
                            if (value == 0) {
                                value = std1.getOid().compareTo(std2.getOid());
                            }
                            return value;
                        }
                    })
                    .collect(Collectors.toList());
            addMessage("Processing school: " + school.getName() + " number of students " + schoolStudents.size());

            for (OnStudent student : schoolStudents) {
                ThreadUtils.checkInterrupt();
                Map<PlainDate, FteCalculation> studentFteCalculations = Collections.EMPTY_MAP;
                try {
                    studentFteCalculations = executeCalculationForStudent(school, student);
                } catch (Exception e) {
                    addError("executeCalculationForStudent(" + student.getNameView() + "):"
                            + LoggerUtils.convertThrowableToString(e));
                }
                try {
                    processAndStoreSubmissionValues(studentFteCalculations, school, student);
                } catch (Exception e) {
                    addError("processAndStoreSubmissionValues():" + LoggerUtils.convertThrowableToString(e));
                }
                try {
                    processAndStoreCalculations(studentFteCalculations, school, student);
                } catch (Exception e) {
                    addError("processAndStoreCalculations():" + LoggerUtils.convertThrowableToString(e));
                }
            }
        }
    }

    /**
     * Execute calculation for student.
     *
     * @param school the school
     * @param student the student
     * @return the map
     */
    private Map<PlainDate, FteCalculation> executeCalculationForStudent(OnSchool school, OnStudent student) {
        List<OnAnnualSpan> schoolSpans = student.getEnrollmentSpans(getBroker(), false, false).stream()
                .map(span -> (OnAnnualSpan) span)
                .filter(span -> span.getDateRangeSafe().isOverlap(getDateRange()))
                .collect(Collectors.toList());


        if (!schoolSpans.stream().anyMatch(span -> !span.isSecondary() && span.getSchool().equals(school))) {
            addStudentMessage(student, "No spans for: " + school.getName());
            return new HashMap<>(0);
        }

        List<OnAnnualSpan> gradeLevelSpans = schoolSpans.stream().filter(span -> {
            if (!span.isSecondary()) {
                String gradeType = span.getGradeType(getBroker(), getDictExtractor(), getGradesHelper());
                try {
                    int gradeLevel = Integer.parseInt(gradeType);
                    return gradeLevel >= 9;
                } catch (NumberFormatException nfe) {
                    addStudentMessage(student, LoggerUtils.convertThrowableToString(nfe));
                }
            }
            return false;
        }).collect(Collectors.toList());

        if (gradeLevelSpans.isEmpty()) {
            addStudentMessage(student, "No spans with grade level greater than 8: " + school.getName());
            return new HashMap<>(0);
        }

        boolean debugDetail = getDebugDetail();
        List<OnStudentScheduleSpan> scheduleSpans = getScheduleSpans(school, student);

        StringBuilder datesOfInterestDebugOutput = debugDetail ? new StringBuilder() : null;
        Collection<PlainDate> datesOfInterest =
                getDatesOfInterest(school, student, schoolSpans, scheduleSpans, datesOfInterestDebugOutput);
        if (datesOfInterest.isEmpty()) {
            addStudentMessage(student, "No dates of interest for: " + school.getName());
        }

        if (debugDetail) {
            addStudentMessage(student, "Dates of interest [" + school.getName() + "]: " + datesOfInterest);
            addStudentMessage(student, "Dates of interest detail:\n" + datesOfInterestDebugOutput.toString());
        }

        // Calculate FTE for each date of interest
        List<FteCalculation> studentFteCalculations = new ArrayList<>();
        for (PlainDate dateOfInterest : datesOfInterest) {
            FteCalculation updatedFteCalculation =
                    getFteCalculationForDateofInterest(dateOfInterest, scheduleSpans, student, school);

            studentFteCalculations.add(updatedFteCalculation);
        }

        return studentFteCalculations.stream().collect(Collectors.toMap(FteCalculation::getDate, Function.identity()));
    }

    /**
     * Find the FTE record for the given date, return empty record so the average calculates
     * correctly.
     *
     * @param studentRecords the student records
     * @param countDate the count date
     * @param student the student
     * @param school the school
     * @param dictionaryExtractor the dictionary extractor
     * @param broker the broker
     * @return FteCalculation
     */
    private FteCalculation findFteRecord(Collection<FteCalculation> studentRecords,
                                         PlainDate countDate,
                                         OnStudent student,
                                         OnSchool school,
                                         DictionaryExtractor dictionaryExtractor,
                                         X2Broker broker) {
        return studentRecords.stream()
                .sorted(new Comparator<FteCalculation>() {
                    @Override
                    public int compare(FteCalculation o1, FteCalculation o2) {
                        return o2.getDate().compareTo(o1.getDate());
                    }
                })
                .filter(fte -> fte.getDate().compareTo(countDate) <= 0)
                .findFirst()
                .orElse(getEmptyRecord(countDate, student, school));
    }

    /**
     * Find march values.
     *
     * @param submissionValues the submission values
     * @return the map
     */
    private Map findMarchValues(Collection<Map<String, Object>> submissionValues) {
        Optional<Map<String, Object>> marchValues = submissionValues.stream()
                .filter(map -> SubmissionType.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_MARCH
                        .contains(map.get(FteMonthly.FIELD_MONTH.getAlias())))
                .findAny();
        if (marchValues.isPresent()) {
            return marchValues.get();
        }

        return new HashMap<>();
    }

    /**
     * Gets the date range.
     *
     * @return the date range
     */
    private Range<Date> getDateRange() {
        if (m_dateRange == null) {
            m_dateRange = Range.of(getReportDate(), getCurrentContext().getEndDate());
        }
        return m_dateRange;
    }

    /**
     * Walk the schedule spans, capturing dates of interest into TreeSet<PlainDate>
     * (unique and in order).
     *
     * @param school SisSchool
     * @param student SisStudent
     * @param schoolSpans List<AnnualSpan>
     * @param scheduleSpans List<StudentScheduleSpan>
     * @param debugOutput the debug output
     * @return TreeSet<PlainDate>
     */
    private Collection<PlainDate> getDatesOfInterest(OnSchool school,
                                                     OnStudent student,
                                                     List<OnAnnualSpan> schoolSpans,
                                                     List<OnStudentScheduleSpan> scheduleSpans,
                                                     StringBuilder debugOutput) {
        TreeSet<PlainDate> studentProgramParticipationDates = getStudentProgramParticipationDates(school, student);

        // Enrollment Spans: E------------W..........E-----------------
        // Add/Drop: *.......*......*.....*..*.....*.......*.........*
        try {
            List<Range<Date>> enrolledDates = new ArrayList();
            Stream<PlainDate> enrolledSpansDates =
                    schoolSpans.stream()
                            .filter(span -> !span.isSecondary() && span.getFirstActiveInSessionDate() != null
                                    && (span.getSchool() == null || span.getSchool().getOid().equals(school.getOid())))
                            .map(span -> {
                                PlainDate arrivalDate = span.getArrivalDate();

                                if (debugOutput != null) {
                                    debugOutput.append("arrivalDate: " + arrivalDate + "\n");
                                }

                                TreeSet<PlainDate> datesOfInterest = new TreeSet<>();
                                if (arrivalDate == null) {
                                    return datesOfInterest.stream();
                                }

                                PlainDate firstActiveDate = span.getFirstActiveInSessionDate();
                                PlainDate lastActiveDate = span.getLastActiveInSessionDate();
                                Range<Date> activeRange = Range.of(firstActiveDate, lastActiveDate);
                                PlainDate endOfYear = getOrganization().getCurrentContext().getEndDate();
                                ToolSchoolCalendar bestCalendar = span.findBestCalendar(getBroker());
                                boolean forward = false;
                                PlainDate lastSessionDate =
                                        bestCalendar.findFirstInSessionDate(getBroker(), endOfYear, forward);

                                TreeSet<PlainDate> studentDates = studentProgramParticipationDates.stream()
                                        .filter(date -> activeRange.contains(date)
                                                && date.compareTo(arrivalDate) >= 0
                                                && date.before(lastSessionDate))
                                        .collect(Collectors.toCollection(TreeSet::new));

                                if (debugOutput != null) {
                                    debugOutput.append("Program dates: " + studentDates + "\n");
                                }

                                studentDates.add(arrivalDate);

                                String priorStatus = null;
                                for (OnEnrollment enr : span.getAllEnrollmentsAscend().stream()
                                        .filter(enr -> !enr.getEnrollmentDate().before(arrivalDate))
                                        .map(enr -> (OnEnrollment) enr)
                                        .collect(Collectors.toList())) {
                                    String enrStatus = enr.getBoardResidentStatus();
                                    if (priorStatus != null && !priorStatus.equals(enrStatus)) {
                                        studentDates.add(enr.getEnrollmentDate());
                                        if (debugOutput != null) {
                                            debugOutput
                                                    .append("Board Resident Address change: " + enr.getEnrollmentDate()
                                                            + "\n");
                                        }
                                    }
                                    priorStatus = enrStatus;
                                }
                                OnEnrollment enr = (OnEnrollment) span.getTerminatingEnrollment();
                                if (enr != null) {
                                    PlainDate terminationDate = DistrictManager.isMemberOnWithdrawal(getBroker())
                                            ? DateUtils.add(enr.getEnrollmentDate(), 1)
                                            : enr.getEnrollmentDate();
                                    studentDates.add(terminationDate);
                                    if (debugOutput != null) {
                                        debugOutput
                                                .append("Terminating enrolllment date: " + terminationDate + "\n");
                                    }
                                }
                                enrolledDates.add(Range.of(arrivalDate, enr == null ? null : enr.getEnrollmentDate()));

                                if (debugOutput != null) {
                                    debugOutput.append("Student dates: " + datesOfInterest + "\n");
                                }
                                if (scheduleSpans.isEmpty()) {
                                    return studentDates.stream();
                                }

                                datesOfInterest = scheduleSpans.stream()
                                        .filter(scheduleSpan -> activeRange.isOverlap(scheduleSpan.getDateRange()))
                                        .map(scheduleSpan -> {
                                            List<PlainDate> addDropDates = new ArrayList<>();

                                            PlainDate entryDate = scheduleSpan.getEntryDate();
                                            PlainDate exitDate = scheduleSpan.getExitDate();

                                            // Entered the section on or after the start of the
                                            // enrollment
                                            if (entryDate.compareTo(arrivalDate) >= 0) {
                                                addDropDates.add(entryDate);
                                            }

                                            // Exited the section before the end of the enrollment
                                            // span
                                            boolean after = true;
                                            PlainDate effectiveDate =
                                                    bestCalendar.findNextInSessionDate(getBroker(), exitDate, after);
                                            if (effectiveDate == null) {
                                                effectiveDate = exitDate;
                                            }

                                            if (lastActiveDate == null || effectiveDate.compareTo(lastActiveDate) < 0) {
                                                // After the student arrives and
                                                // Before the last day of school
                                                if (effectiveDate.compareTo(arrivalDate) >= 0
                                                        && effectiveDate.before(lastSessionDate)) {
                                                    addDropDates.add(effectiveDate);
                                                }
                                            }

                                            return addDropDates.stream();
                                        })
                                        .flatMap(changeDates -> changeDates.filter(addDropDate -> addDropDate != null))
                                        .collect(Collectors.toCollection(TreeSet::new));
                                if (debugOutput != null) {
                                    debugOutput.append("Schedule span dates: " + datesOfInterest + "\n");
                                }

                                datesOfInterest.addAll(studentDates);

                                return datesOfInterest.stream();
                            })
                            .flatMap(streamDates -> streamDates.filter(dateOfInterest -> dateOfInterest != null
                                    && getDateRange().contains(dateOfInterest)));

            Stream<PlainDate> secondarySpanDates = schoolSpans.stream()
                    .filter(span -> span.isSecondary() && ((OnSchool) span.getSchool()).getIsIncludeInFTEIndicator())
                    .map(span -> {
                        if (debugOutput != null) {
                            debugOutput.append("Secondary dates: [" + span.getFirstActiveInSessionDate() + ", "
                                    + span.getLastActiveInSessionDate() + "] for School: " + span.getSchool() + "\n");
                        }
                        return Stream.of(span.getFirstActiveInSessionDate(), span.getLastActiveInSessionDate())
                                .filter(Objects::nonNull)
                                .filter(date -> enrolledDates.stream().anyMatch(range -> range.contains(date)));
                    })
                    .flatMap(streamDates -> streamDates.filter(dateOfInterest -> dateOfInterest != null
                            && getDateRange().contains(dateOfInterest)));
            return Stream.concat(enrolledSpansDates, secondarySpanDates)
                    .collect(Collectors.toCollection(TreeSet::new));
        } catch (Exception e) {
            addStudentMessage(student, "Error processing: " + schoolSpans);
            addStudentMessage(student, "Error processing: " + scheduleSpans);
            addStudentMessage(student, LoggerUtils.convertThrowableToString(e));
        }

        return Collections.EMPTY_LIST;
    }

    /**
     * Gets the debug detail.
     *
     * @return boolean
     */
    private boolean getDebugDetail() {
        if (m_debugDetail == null) {
            m_debugDetail = getParameter(INPUT_PARAM_DEBUG_DETAIL) != null
                    && getParameter(INPUT_PARAM_DEBUG_DETAIL) instanceof Boolean
                    && ((Boolean) getParameter(INPUT_PARAM_DEBUG_DETAIL));
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
     * Gets the errors.
     *
     * @return List
     */
    private List<Message> getErrors() {
        return getMessagesByType(Message.Type.ERROR);
    }

    /**
     * Gets the fte calculation for dateof interest.
     *
     * @param dateOfInterest the date of interest
     * @param scheduleSpans the schedule spans
     * @param student the student
     * @param school the school
     * @return the fte calculation for dateof interest
     */
    private FteCalculation getFteCalculationForDateofInterest(PlainDate dateOfInterest,
                                                              List<OnStudentScheduleSpan> scheduleSpans,
                                                              OnStudent student,
                                                              OnSchool school) {
        boolean debugDetail = getDebugDetail();
        X2Broker broker = getBroker();

        List<AnnualSpan> enrollmentSpans = student.getEnrollmentSpans(getBroker(), false, false).stream()
                .filter(span -> Range.of(span.getSpanStartDate(), span.getLastActiveInSessionDate())
                        .contains(dateOfInterest))
                .collect(Collectors.toList());

        List<AnnualSpan> primarySpans = enrollmentSpans.stream()
                .filter(span -> !span.isSecondary() && span.getSchool().equals(school))
                .collect(Collectors.toList());
        boolean isEnrolled = !primarySpans.isEmpty();

        Set<OnSchool> secondarySchools = enrollmentSpans.stream()
                .filter(span -> span.isSecondary())
                .map(span -> (OnSchool) span.getSchool())
                .collect(Collectors.toSet());
        Set<String> includesSchoolOids =
                Stream.concat(Stream.of(school.getOid()), secondarySchools.stream().map(skl -> skl.getOid()))
                        .collect(Collectors.toSet());

        Collection<OnStudentScheduleSpan> scheduleSpansForDate = !isEnrolled ? Collections.EMPTY_LIST
                : getScheduleSpansForDate(dateOfInterest, scheduleSpans).stream()
                        .filter(span -> includesSchoolOids
                                .contains(span.getSection().getSchedule(broker).getSchoolOid()))
                        .collect(Collectors.toList());

        if (debugDetail) {
            addStudentMessage(student, "=======================================================");
            addStudentMessage(student, "Starting date of interest: " + dateOfInterest);
            addStudentMessage(student, "Is student enrolled: " + isEnrolled);
            addStudentMessage(student, "Schools with schedules included: " + school.getName()
                    + (secondarySchools.size() > 0 ? "," : "")
                    + secondarySchools.stream().map(skl -> skl.getName()).collect(Collectors.joining(",")));
        }

        ToolSchoolCalendar calendar =
                school.getCalendarByCode(broker, getCurrentContext().getOid(), student.getCalendarCode());
        if (debugDetail) {
            addStudentMessage(student, calendar == null ? "Calendar code is null"
                    : "Calendar code: " + calendar.getCalendarId() + " [" + calendar.getOid() + "]");
        }

        // Create FteCalculation
        FteCalculation updatedFteCalculation = new FteCalculation(dateOfInterest, student, school, calendar);
        updatedFteCalculation.setSchoolYear(getCurrentContext().getContextId());

        // Set boardResidentStatus
        String boardResidentStatus =
                student.getBoardResidentStatus(broker, school, dateOfInterest, false, true);
        updatedFteCalculation.setBoardResidentStatus(boardResidentStatus);

        // Build FTE Record
        OnStudentSalep pgmSAL = null;
        if (isEnrolled) {
            pgmSAL = student.getSalepPrograms(broker).stream()
                    .sorted(Comparator.comparing(OnStudentSalep::getStartDate, Comparator.reverseOrder()))
                    .filter(pgm -> pgm.getDateRange().contains(dateOfInterest))
                    .findFirst().orElse(null);
        }
        Boolean isSalEnrolled = isEnrolled && pgmSAL != null;
        updatedFteCalculation.setSAL(isSalEnrolled);
        if (isSalEnrolled.booleanValue()) {
            updatedFteCalculation.setFteForSAL(getDictExtractor(), pgmSAL);
        } else {
            StringBuilder debugOutput =
                    updatedFteCalculation.calculateCreditsAndMinutes(scheduleSpansForDate,
                            getDictExtractor(),
                            getBroker());
            if (debugDetail) {
                addStudentMessage(student, debugOutput.toString() + "\n");
                addStudentMessage(student, "FteCalculation before setFte: " + updatedFteCalculation.toString());
            }
            updatedFteCalculation.setFte(broker, getScaleFte(), getScaleFteHc());
        }

        if (debugDetail) {
            addStudentMessage(student, "FteCalculation: " + updatedFteCalculation.toString());
            addStudentMessage(student, "Finished date of interest: " + dateOfInterest);
            addStudentMessage(student, "=======================================================\n\n");
        }

        return updatedFteCalculation;
    }

    /**
     * Gets the fte monthly records.
     *
     * @param student the student
     * @param school the school
     * @return the fte monthly records
     */
    private Stream<FteMonthly> getFteMonthlyRecords(OnStudent student, OnSchool school) {
        return student.getFteMonthlyRecords(getBroker()).stream()
                .filter(fte -> school.getOid().equals(fte.getSchoolOid()))
                .filter(fte -> getCurrentContext().getContextId().equals(fte.getSchoolYear()))
                .filter(fte -> fte.getReportDate() != null && !fte.getReportDate().before(getReportDate()))
                .sorted(Comparator.comparing(FteMonthly::getReportDate));
    }

    /**
     * Gets the errors.
     *
     * @return List
     */
    private List<Message> getHeaderMessages() {
        return getMessagesByType(Message.Type.HEADER);
    }

    /**
     * Gets the info messages.
     *
     * @return List
     */
    private List<Message> getInfoMessages() {
        return getMessagesByType(Message.Type.INFO);
    }

    /**
     * Gets the messages by type.
     *
     * @param type Type
     * @return List
     */
    private List<Message> getMessagesByType(Message.Type type) {
        return m_messages.stream().filter(msg -> msg.get(Message.Field.TYPE).equals(type)).collect(Collectors.toList());
    }

    /**
     * Gets the report date.
     *
     * @return Plain date
     */
    private PlainDate getReportDate() {
        if (m_reportDate == null) {
            // new PlainDate(OrganizationManager.getTimeZone(getOrganization()))
            m_reportDate = getParameter(INPUT_PARAM_REPORT_DATE) == null
                    ? getCurrentContext().getStartDate()
                    : (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        }
        return m_reportDate;
    }

    /**
     * Gets the schedule mode.
     *
     * @param student the student
     * @param school the school
     * @param submissionType the submission type
     * @param broker the broker
     * @return the schedule mode
     */
    private int getScheduleMode(OnStudent student, OnSchool school, SubmissionType submissionType, X2Broker broker) {
        Collection<PlainDate> countDates = submissionType.getCountDates();

        countDates.add(submissionType.getPeriodStartDate());
        countDates.add(submissionType.getPeriodEndDate());

        PlainDate periodStartDate = countDates.stream().min(Date::compareTo).get();
        PlainDate periodEndDate = countDates.stream().max(Date::compareTo).get();

        Pair<Integer, Integer> modes = getScheduleModes(student, school, periodStartDate, periodEndDate, broker);
        if (modes == null) {
            return 2;
        }
        if (OnsisConstants.SUBMISSION_SCHOOL_TYPE_JUNE_SECONDARY.contains(submissionType.getSubmissionPeriodCode()) ||
                OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY_MARCH
                        .contains(submissionType.getSubmissionPeriodCode())) {
            return modes.getRight();
        }
        return modes.getLeft();
    }

    /**
     * Gets the schedule modes.
     *
     * @param student the student
     * @param school the school
     * @param overlappingStartDate the overlapping start date
     * @param overlappingEndDate the overlapping end date
     * @param broker the broker
     * @return the schedule mode
     */
    private Pair<Integer, Integer> getScheduleModes(OnStudent student,
                                                    OnSchool school,
                                                    PlainDate overlappingStartDate,
                                                    PlainDate overlappingEndDate,
                                                    X2Broker broker) {
        List<OnAnnualSpan> enrollmentSpans = student.getEnrollmentSpans(getBroker(), false, true).stream()
                .map(span -> (OnAnnualSpan) span)
                .filter(span -> school.getOid().equals(span.getSchool().getOid()))
                .filter(span -> {
                    PlainDate withdrawalOrLastActive = span.getWithdrawalMemberDate();
                    if (withdrawalOrLastActive == null) {
                        withdrawalOrLastActive = span.getLastActiveInSessionDate();
                    }

                    return !overlappingEndDate.before(span.getFirstActiveInSessionDate())
                            && (withdrawalOrLastActive == null
                                    || !overlappingStartDate.after(withdrawalOrLastActive));
                })
                .collect(Collectors.toList());

        return enrollmentSpans.stream().map(span -> (OnSchoolCalendar) span.getSchoolCalendar())
                .map(cas -> getScheduleModes(cas, broker))
                .filter(pair -> pair.getLeft() != null)
                .reduce((first, second) -> Pair.of(
                        first.getLeft().compareTo(second.getLeft()) >= 0 ? first.getLeft() : second.getLeft(),
                        first.getRight().compareTo(second.getRight()) >= 0 ? first.getRight()
                                : second.getRight()))
                .orElse(null);
    }

    /**
     * Gets the schedule modes.
     *
     * @param cas the cas
     * @param broker the broker
     * @return the schedule modes
     */
    private Pair<Integer, Integer> getScheduleModes(OnSchoolCalendar cas, X2Broker broker) {
        Integer mode1 = null;
        Integer mode2 = null;
        String strValue = cas.getScheduleMode();
        if (!StringUtils.isEmpty(strValue)) {
            mode1 = Integer.valueOf(strValue);
        }
        strValue = cas.getScheduleMode2();
        if (!StringUtils.isEmpty(strValue)) {
            mode2 = Integer.valueOf(strValue);
        }
        if (mode2 == null) {
            mode2 = mode1;
        }
        return Pair.of(mode1, mode2);
    }

    /**
     * Gets the schedule spans.
     *
     * @param school SisSchool
     * @param student SisStudent
     * @return List
     */
    private List<OnStudentScheduleSpan> getScheduleSpans(OnSchool school, OnStudent student) {
        return student.getStudentScheduleSpans(getBroker()).stream()
                .map(span -> (OnStudentScheduleSpan) span)
                .filter(span -> isContextSpan(span) && isCourseToInclude(span) && isSchoolToInclude(span))
                .collect(Collectors.toList());
    }

    /**
     * Gets the schedule spans for date.
     *
     * @param dateOfInterest the date of interest
     * @param scheduleSpans the schedule spans
     * @return the schedule spans for date
     */
    private List<OnStudentScheduleSpan> getScheduleSpansForDate(PlainDate dateOfInterest,
                                                                List<OnStudentScheduleSpan> scheduleSpans) {
        return scheduleSpans.stream()
                .filter(span -> isQualifiedSpan(span, dateOfInterest))
                .collect(Collectors.toList());

    }

    /**
     * Gets the schools.
     *
     * @return Filterable
     */
    private List<OnSchool> getSchoolCandidates() {
        if (m_schoolCandidates == null) {
            ToolBean.load(getBroker(), getDictExtractor(), OnSchool.class);
            m_schoolCandidates = ToolBean.getCachedToolBeans(OnSchool.class).stream()
                    .filter(school -> {
                        if (!m_isAllSchools && !getSchoolOids().contains(school.getOid())) {
                            return false;
                        }

                        return (m_isAllSchools || getSchoolOids().contains(school.getOid()))
                                && school.getIsIncludeInFTEIndicator()
                                && isHighSchool(school);
                    })
                    .collect(Collectors.toList());
        }
        return m_schoolCandidates;
    }

    /**
     * Gets the schools.
     *
     * @return Filterable
     */
    private List<OnSchool> getSchools() {
        if (m_schools == null) {
            m_schools = getSchoolCandidates().stream()
                    .filter(skl -> hasStudents(skl))
                    .sorted(Comparator.comparing(OnSchool::getName))
                    .collect(Collectors.toList());
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
     * Gets the student program participation dates.
     *
     * @param school the school
     * @param student the student
     * @return the student program participation dates
     */
    private TreeSet<PlainDate> getStudentProgramParticipationDates(OnSchool school,
                                                                   OnStudent student) {
        Stream<PlainDate> salDates = student.getSalepPrograms(getBroker())
                .stream()
                .flatMap(pgm -> Stream.of(pgm.getStartDate(),
                        pgm.getEndDate() == null ? null : DateUtils.add(pgm.getEndDate(), 1)));

        Stream<PlainDate> iepDates = student.getSpedPrograms(getBroker()).stream()
                .filter(sped -> getDateRange().isOverlap(sped.getDateRange()))
                .flatMap(sped -> {
                    Stream<PlainDate> details = sped.getProgramDetails(getBroker()).stream().flatMap(pgd -> {
                        String type = pgd.getType();
                        PlainDate startDate = pgd.getStartDate();
                        PlainDate endDate = pgd.getEndDate();
                        boolean isIep = pgd.getIepRequiredIndicator();
                        if (isIep && OnStudentSpedDetail.TYPE_PLACEMENT.equals(type) && startDate != null
                                && getDateRange().isOverlap(pgd.getDateRange())) {
                            return Stream.of(startDate, endDate);
                        }
                        return Stream.empty();
                    });
                    return Stream.concat(details, Stream.of(sped.getStartDate(), sped.getEndDate()));
                });

        return Stream.concat(salDates, iepDates)
                .filter(dateOfInterest -> dateOfInterest != null
                        && getDateRange().contains(dateOfInterest))
                .collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * Gets the students.
     *
     * @param school the school
     * @return the students
     */
    private Set<OnStudent> getStudents(OnSchool school) {
        if (m_schoolStudents == null) {
            m_schoolStudents = new HashMap();
            List<String> schoolOids =
                    getSchoolCandidates().stream().map(skl -> skl.getOid()).collect(Collectors.toList());
            X2Criteria inclusionCriteria = new X2Criteria();

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictExtractor()),
                    getDictExtractor()
                            .getRefCodesWithStateValue(
                                    OnSection.FIELD_COURSE_CODE_TYPE.getField(getDictExtractor()),
                                    OnSection.COURSE_CODE_TYPES_MDC_DCC_LDC)
                            .stream()
                            .map(code -> code.getCode()).collect(Collectors.toList()));
            criteria.addNotEmpty(OnSection.FIELD_MINISTRY_COURSE_CODE.resolve(getDictExtractor()),
                    getBroker().getPersistenceKey());

            inclusionCriteria.addOrCriteria(criteria);

            criteria = new X2Criteria();
            criteria.addEqualTo(OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictExtractor()),
                    OnSection.COURSE_CODE_TYPE_PLACEHOLDER);
            criteria.addNotEmpty(ToolSection.FIELD_CRS_NUMBER.resolve(getDictExtractor()),
                    getBroker().getPersistenceKey());

            inclusionCriteria.addOrCriteria(criteria);

            List<String> courseDeliveryCodes = getDictExtractor().getRefCodesWithStateValue(
                    OnSection.FIELD_COURSE_DELIVERY_TYPE_CSK.getField(getDictExtractor()),
                    Arrays.asList(OnSection.COURSE_DELIVERY_TYPE_INDEPENDENT_STUDY))
                    .stream()
                    .map(code -> code.getCode())
                    .collect(Collectors.toList());
            inclusionCriteria.addNotIn(OnSection.FIELD_COURSE_DELIVERY_TYPE_CSK.resolve(getDictExtractor()),
                    courseDeliveryCodes);

            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setSchoolOids(schoolOids)
                    .setExcludeStudent(OnStudent.FIELD_EXCLUDE_FROM_REPORTING)
                    .setExcludeSection(OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS)
                    .setExcludeCourse(OnSection.FIELD_CRS_EXCLUDE)
                    .setIncludeSecondarySpans(true)
                    .setIncludeAllSecondarySpans(false)
                    .setCurrentContext(getCurrentContext())
                    .setSectionLimitingCriteria(inclusionCriteria);
            if (!StringUtils.isBlank(getDebugStudentOid())) {
                spanCriteria.setLimitingStudentOids(Arrays.asList(getDebugStudentOid()));
            }

            X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());

            X2Criteria fteSubQueryCriteria = new X2Criteria();
            DataDictionary dictionary = getDictExtractor().getDictionary(FteRecord.DDX_ID);
            String dictionaryOid =
                    dictionary == null ? "__NO_MATCH__" : dictionary.getExtendedDictionaryOid();
            fteSubQueryCriteria.addEqualTo(FteRecord.FIELD_EXTENDED_DATA_DICTIONARY_OID.resolve(getDictExtractor()),
                    dictionaryOid);
            fteSubQueryCriteria.addIn(FteRecord.FIELD_SCHOOL_OID.resolve(getDictExtractor()), schoolOids);
            fteSubQueryCriteria.addEqualTo(FteRecord.FIELD_SCHOOL_YEAR.resolve(getDictExtractor()),
                    getCurrentContext().getContextId());
            if (!StringUtils.isBlank(getDebugStudentOid())) {
                fteSubQueryCriteria.addEqualTo(FteRecord.FIELD_STUDENT_OID.resolve(getDictExtractor()),
                        getDebugStudentOid());
            }
            SubQuery fteSubQuery =
                    new SubQuery(SisBeanPaths.USER_DEFINED_TABLE_C.getBeanType(),
                            SisBeanPaths.USER_DEFINED_TABLE_C.studentOid().getPath(), fteSubQueryCriteria);
            X2Criteria fteCriteria = new X2Criteria();
            fteCriteria.addIn(SisBeanPaths.STUDENT.oid().getPath(), fteSubQuery);
            candidateCriteria.addOrCriteria(fteCriteria);

            // load students with filterable
            FilterableFactory.create(getBroker(), getDictExtractor(), OnStudent.class, candidateCriteria, null);

            // load enrollments and student school
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                    ToolStudent.CHILD_STUDENT_ENROLLMENTS);
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                    ToolStudent.CHILD_STUDENT_SCHOOLS);

            // preload FteRecords
            ToolBean.preload(getBroker(), getDictExtractor(), null, FteRecord.PARENT_STUDENT);

            Set<String> allStudentsList = new HashSet();
            ToolBean.getCachedToolBeans(OnStudent.class).stream()
                    .map(student -> student.getEnrollmentSpans(getBroker(), false, false))
                    .flatMap(List::stream)
                    .filter(span -> {
                        // test school
                        if (!schoolOids.contains(span.getSchool().getOid())) {
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

            ToolBean.getCachedToolBeans(OnStudent.class).stream()
                    .map(student -> student.getFteRecords(getBroker()))
                    .flatMap(List::stream)
                    .filter(fte -> schoolOids.contains(fte.getSchoolOid()))
                    .filter(fte -> getCurrentContext().getContextId().equals(fte.getSchoolYear()))
                    .forEach(fte -> {
                        Set<OnStudent> students = m_schoolStudents.get(fte.getSchoolOid());
                        if (students == null) {
                            students = new HashSet();
                            m_schoolStudents.put(fte.getSchoolOid(), students);
                        }
                        students.add((OnStudent) fte.getStudent(getBroker()));
                        allStudentsList.add(fte.getStudentOid());
                    });

            // remove unused students
            ToolBean.filterCachedToolBeans(OnStudent.class,
                    student -> allStudentsList.contains(student.getOid()));

            // Remove school selection from criteria helper
            spanCriteria.setSchoolOids(null);

            // preload schedules
            ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class,
                    CriteriaHelper.buildStudentScheduleCriteria(spanCriteria));
            ToolBean.addAndCriteria(getBroker(), ToolStudentScheduleChange.class,
                    CriteriaHelper.buildStudentScheduleChangeCriteria(spanCriteria));
            ToolBean.preload(getBroker(), getDictExtractor(), null,
                    ToolStudentSchedule.PARENT_STUDENT);
            ToolBean.preload(getBroker(), getDictExtractor(), null,
                    ToolStudentScheduleChange.PARENT_STUDENT);

            // preload transripts
            ToolBean.preload(getBroker(), getDictExtractor(), null,
                    ToolTranscript.PARENT_STUDENT);

            // preload sections
            Set<String> sectionOids = Stream.concat(
                    ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream().map(ssc -> ssc.getSectionOid()),
                    ToolBean.getCachedToolBeans(ToolStudentScheduleChange.class).stream()
                            .map(ssc -> ssc.getSectionOid()))
                    .collect(Collectors.toSet());
            ToolBean.loadByOid(getBroker(), getDictExtractor(), OnSection.class, sectionOids);

            // preload special ed
            ToolBean.addAndCriteria(getBroker(), OnStudentSped.class, ToolStudentProgramParticipation
                    .getDateRangeCriteria(getBroker(), getReportDate(), getCurrentContext().getEndDate()));
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                    OnStudent.CHILD_SPED_PROGRAMS);
            ToolBean.preload(getBroker(), getDictExtractor(), null, OnStudentSped.CHILD_PROGRAM_DETAILS);

            // preload salep
            ToolBean.addAndCriteria(getBroker(), OnStudentSalep.class, ToolStudentProgramParticipation
                    .getDateRangeCriteria(getBroker(), getReportDate(), getCurrentContext().getEndDate()));
            ToolBean.preload(getBroker(), getDictExtractor(),
                    Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                    OnStudent.CHILD_SALEP_PROGRAMS);
            ToolBean.preload(getBroker(), getDictExtractor(), null,
                    OnStudentSalep.CHILD_PROGRAM_DETAILS);

            // load submission types
            ToolBean.load(getBroker(), getDictExtractor(), SubmissionType.class);

        }
        Set<OnStudent> students = m_schoolStudents.get(school.getOid());
        return students == null ? Collections.EMPTY_SET : students;
    }

    /**
     * Find the SubmissionType matching type and overlaps date.
     *
     * @param beginDate {@link PlainDate}
     * @param endDate {@link PlainDate}
     * @param submissionPeriodCodes the submission period codes
     * @param broker {@link X2Broker}
     * @return the submission type
     */
    private List<SubmissionType> getSubmissionType(PlainDate beginDate,
                                                   PlainDate endDate,
                                                   List<String> submissionPeriodCodes,
                                                   X2Broker broker) {
        Range<Date> dateRange = Range.of(beginDate, endDate);
        return ToolBean.getCachedToolBeans(SubmissionType.class).stream()
                .filter(submissionType -> submissionPeriodCodes.contains(submissionType.getSubmissionPeriodCode())
                        && dateRange.isOverlap(submissionType.getDateRange()))
                .collect(Collectors.toList());
    }

    /**
     * Checks for students.
     *
     * @param school SisSchool
     * @return true, if successful
     */
    private boolean hasStudents(OnSchool school) {
        Collection<OnStudent> students = getStudents(school);

        return !students.isEmpty();
    }

    /**
     * Checks if is actual span.
     *
     * @param span StudentScheduleSpan
     * @return true, if is actual span
     */
    private boolean isContextSpan(OnStudentScheduleSpan span) {
        return span.getExitDate().compareTo(span.getEntryDate()) >= 0 &&
                getDateRange().isOverlap(span.getDateRange());
    }

    /**
     * Checks if is course to include.
     *
     * @param span StudentScheduleSpan
     * @return true, if is course to include
     */
    private boolean isCourseToInclude(OnStudentScheduleSpan span) {
        OnSection section = (OnSection) span.getSection();
        if (section != null) {
            return COURSE_CODE_TYPES_INCLUDED.contains(section.getCourseCodeTypeState());
        }
        return false;
    }

    /**
     * Checks if is day school.
     *
     * @param school SisSchool
     * @return true, if is day school
     */
    private boolean isHighSchool(OnSchool school) {
        return HIGH_SCHOOL_STATE_CODES.contains(school.getSchoolLevelCodeState());
    }

    /**
     * Checks if is qualified span.
     *
     * @param span the span
     * @param dateOfInterest the date of interest
     * @return true, if is qualified span
     */
    private boolean isQualifiedSpan(OnStudentScheduleSpan span, PlainDate dateOfInterest) {
        if (dateOfInterest == null) {
            return false;
        }

        return span.getEntryDate() != null
                && span.getExitDate() != null
                && span.getDateRange().contains(dateOfInterest)
                && isContextSpan(span) && isCourseToInclude(span) && isSchoolToInclude(span);
    }

    /**
     * Checks if schedule span is a day school.
     *
     * @param span StudentScheduleSpan
     * @return true, if is included school
     */
    private boolean isSchoolToInclude(OnStudentScheduleSpan span) {
        ToolSection section = span.getSection();
        if (section != null) {
            ToolSchedule schedule = section.getSchedule(getBroker());
            if (schedule != null) {
                OnSchool school = (OnSchool) schedule.getSchool(getBroker());
                if (school != null) {
                    return school.getIsIncludeInFTEIndicator();
                }
            }
        }
        return false;
    }

    /**
     * FTE record is not valid if all values are zero.
     *
     * @param valuesMap the values map
     * @return true, if is valid monthly record
     */
    private boolean isValidMonthlyRecord(Map<String, Object> valuesMap) {
        BigDecimal fte = (BigDecimal) valuesMap.get(FteMonthly.FIELD_FTE.getAlias());
        BigDecimal fteHc = (BigDecimal) valuesMap.get(FteMonthly.FIELD_FTE_HIGH_CREDIT.getAlias());
        Long ade = (Long) valuesMap.get(FteMonthly.FIELD_MINUTES.getAlias());
        Long adeHc = (Long) valuesMap.get(FteMonthly.FIELD_MINUTES_HIGH_CREDIT.getAlias());
        if (fte == null || ade == null || adeHc == null ||
                (fte.intValue() == 0
                        && fteHc.intValue() == 0
                        && ade.intValue() == 0
                        && adeHc.intValue() == 0)) {
            return false;
        }

        return true;
    }

    /**
     * Load submission values.
     *
     * @param context the context
     * @param school the school
     * @param student the student
     * @param broker the broker
     * @return the sets the
     */
    private Set<String> loadSavedCodes(DistrictSchoolYearContext context,
                                       OnSchool school,
                                       OnStudent student,
                                       X2Broker broker) {
        Set<String> foundPeriodCodes = new HashSet<>();
        getFteMonthlyRecords(student, school)
                .forEach(monthlyFteRecord -> {
                    if (!StringUtils.isBlank(monthlyFteRecord.getMonth())) {
                        foundPeriodCodes.add(monthlyFteRecord.getMonth());
                    }
                });
        return foundPeriodCodes;
    }

    /**
     * Student School Year Universe (Master Dates of Interest)
     * 4. Determine student dates:
     * a) Map student enrollments by date -> Map<PlainDate, UserDefinedTableC>
     * b) Merge with actual DOI
     * ->TreeSet<PlainDate>
     *
     * Initialize Previous FTE = 0.0
     * Start broker transaction
     * For each MDOI:
     * a) Get FteCalculation and/or enrollments for that date
     * b) Is this an FTE change date?
     * YES:
     * i. Normal Student Enrollment records exist
     * a. W: must be FTE 0.0
     * b. Delete any FTECal records
     * c. E-S-Y: write FTE value (Save record if dirty)
     * ii. No Normal Student Enrollment record exist
     * a. Make sure exactly one FTECalc exists and write new FTE values
     * iii. Set Previous FTE to FTE
     * NO
     * i. DELETE any FTECalc records that exist
     * ii. Write Previous FTE value to E-S-Y
     * iii. Write FTE 0.0 to W
     *
     * @param studentFteCalculations the student fte calculations
     * @param school the school
     * @param student the student
     */
    private void processAndStoreCalculations(Map<PlainDate, FteCalculation> studentFteCalculations,
                                             OnSchool school,
                                             OnStudent student) {

        // Find actual change of FTE
        addStudentMessage(student, "=======================================================\nFind FTE changes\n");
        List<FteCalculation> changedFteCalculations = new ArrayList<>();
        FteCalculation priorFteHolder = null;
        for (FteCalculation studentFteCalculation : studentFteCalculations.values().stream()
                .sorted(Comparator.comparing(FteCalculation::getDate)).collect(Collectors.toList())) {
            boolean isChanged = priorFteHolder == null;
            PlainDate possibleDateOfInterest = studentFteCalculation.getDate();
            if (priorFteHolder != null) {
                isChanged = isChanged || priorFteHolder.isChanged(studentFteCalculation);
            }

            isChanged = isChanged || priorFteHolder.isDifferentSpan(getBroker(), studentFteCalculation);

            priorFteHolder = studentFteCalculation;
            if (isChanged) {
                changedFteCalculations.add(studentFteCalculation);
            }

            addStudentMessage(student, "Date of interest changed [" + isChanged + "]: " + possibleDateOfInterest);
        }
        addStudentMessage(student, "End FTE Changes\n=======================================================\n");

        studentFteCalculations =
                changedFteCalculations.stream().collect(Collectors.toMap(FteCalculation::getDate, Function.identity()));
        DataDictionary dictionary = getDictExtractor().getDictionary(FteRecord.DDX_ID);

        // Collect enrollments by date
        Map<PlainDate, List<FteRecord>> recordsByDate = student.getFteRecords(getBroker()).stream()
                .filter(fte -> school.getOid().equals(fte.getSchoolOid()))
                .collect(Collectors.groupingBy(FteRecord::getFteDate));

        // Collect master list of dates
        TreeSet<PlainDate> masterDates = new TreeSet<>();
        masterDates.addAll(recordsByDate.keySet());
        masterDates.addAll(studentFteCalculations.keySet());

        for (PlainDate masterDate : masterDates) {
            FteCalculation studentFteCalculation = studentFteCalculations.get(masterDate);
            List<FteRecord> recordsForDate = recordsByDate.get(masterDate);

            // FTE Change date
            if (studentFteCalculation != null) {
                if (RUN_OPTION_CSV.equals(m_runOption)) {
                    List<String> values = new ArrayList();
                    values.add(school.getName());
                    values.add(student.getNameView());
                    values.add(student.getOenRaw());
                    Map<String, String> map = studentFteCalculation.writeValuesToMap();
                    map.keySet().stream().sorted().forEach(alias -> values.add(alias + "=" + map.get(alias)));
                    m_csvOutput.add(values.stream().collect(Collectors.joining(",")));
                } else {
                    FteRecord record = null;
                    if (recordsForDate != null) {
                        Iterator<FteRecord> iterator = recordsForDate.iterator();
                        record = iterator.next();
                        while (iterator.hasNext()) {
                            FteRecord fteEnrollment = iterator.next();
                            iterator.remove();
                            recordsForDate.remove(fteEnrollment);
                            deleteBean(fteEnrollment, fteEnrollment.getStudent(getBroker()));
                        }
                    }
                    if (record == null || studentFteCalculation.isChanged(getDictExtractor(), record)) {
                        UserDefinedTableC fteRecord = null;
                        if (record == null) {
                            fteRecord = X2BaseBean.newInstance(UserDefinedTableC.class, dictionary);

                            OrganizationManager.cloneOrganizationOids(fteRecord,
                                    (OrganizationChild) student.getX2Bean());
                            fteRecord.setSchoolOid(school.getOid());
                            fteRecord.setStudentOid(student.getOid());
                        } else {
                            fteRecord = (UserDefinedTableC) record.getX2Bean();
                        }

                        try {
                            studentFteCalculation.writeValuesToStore(getDictExtractor(), fteRecord);
                            saveDirtyBean(fteRecord);
                        } catch (X2BaseException e) {
                            addError("processAndStoreCalculations:" + LoggerUtils.convertThrowableToString(e));
                        }
                    }

                }
            } else {
                // DELETE any FTECalc records that exist
                Iterator<FteRecord> iterator = recordsForDate.iterator();
                while (iterator.hasNext()) {
                    FteRecord fteEnrollment = iterator.next();
                    iterator.remove();
                    recordsForDate.remove(fteEnrollment);
                    deleteBean(fteEnrollment, fteEnrollment.getStudent(getBroker()));
                }
            }
        }
    }

    /**
     * Process and store submission values.
     *
     * @param studentFteCalculations the student fte calculations
     * @param school the school
     * @param student the student
     */
    private void processAndStoreSubmissionValues(Map<PlainDate, FteCalculation> studentFteCalculations,
                                                 OnSchool school,
                                                 OnStudent student) {
        final StringBuilder output = new StringBuilder();
        boolean debugDetail = getDebugDetail();
        List<PlainDate> datesOfInterest = studentFteCalculations.keySet().stream()
                .sorted().distinct().collect(Collectors.toList());
        Range<Date> contextDateRange = Range.of(getCurrentContext().getStartDate(), getCurrentContext().getEndDate());

        output.append("processAndStoreSubmissionValues");
        output.append("\n");
        output.append("Dates of interest used for [" + school.getName() + "]: ");
        output.append("\n");
        datesOfInterest.forEach(date -> {
            output.append(date);
            output.append(": ");
            output.append(studentFteCalculations.get(date).toString());
            output.append("\n");
        });


        if (debugDetail) {
            addStudentMessage(student, "=======================================================");
            addStudentMessage(student, output.toString());
        }
        if (datesOfInterest.isEmpty()) {
            getFteMonthlyRecords(student, school)
                    .forEach(fteMonthly -> deleteBean(fteMonthly, fteMonthly.getStudent(getBroker())));
            return;
        }

        StringBuilder output3 = new StringBuilder();
        HashMap<String, SubmissionType> submissionsOfInterest = new HashMap<>();
        try {
            List<OnAnnualSpan> schoolSpans = student.getEnrollmentSpans(getBroker(), false, true).stream()
                    .map(span -> (OnAnnualSpan) span)
                    .filter(span -> school.getOid().equals(span.getSchool().getOid()))
                    .filter(span -> span.getDateRangeSafe().isOverlap(getDateRange()))
                    .collect(Collectors.toList());
            output3.append("Span dates used: "
                    + schoolSpans.stream()
                            .map(span -> "[" + span.getFirstActiveInSessionDate() + "->"
                                    + (span.getTerminatingEnrollment() == null ? null
                                            : span.getTerminatingEnrollment().getEnrollmentDate())
                                    + "]")
                            .collect(Collectors.joining(",")));
            output3.append("\n");
            schoolSpans.stream()
                    .map(span -> getSubmissionType(span.getFirstActiveInSessionDate(),
                            span.getTerminatingEnrollment() == null ? getCurrentContext().getEndDate()
                                    : span.getTerminatingEnrollment().getEnrollmentDate(),
                            OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY,
                            getBroker()))
                    .flatMap(List::stream)
                    .filter(Objects::nonNull)
                    .forEach(submissionType -> {
                        if (contextDateRange.contains(submissionType.getPeriodEndDate())) {
                            submissionsOfInterest.put(submissionType.getSubmissionPeriodCode(),
                                    submissionType);
                        }
                    });
        } catch (Exception e) {
            addError("getEnrollmentSpansUsingParentSpansActiveIn(" + student.getNameView() + "): "
                    + LoggerUtils.convertThrowableToString(e));
        }
        if (debugDetail) {
            addStudentMessage(student, output3.toString());
            addStudentMessage(student, "submissionsOfInterest:" + submissionsOfInterest.keySet());
        }

        datesOfInterest.stream()
                .map(dateOfInterest -> getSubmissionType(dateOfInterest, dateOfInterest,
                        OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY, getBroker()))
                .flatMap(List::stream)
                .filter(Objects::nonNull)
                .forEach(submissionType -> {
                    if (contextDateRange.contains(submissionType.getPeriodEndDate())) {
                        submissionsOfInterest.put(submissionType.getSubmissionPeriodCode(),
                                submissionType);
                    }
                });

        PlainDate endDate = new PlainDate();
        Calendar asOfDateCalendar = Calendar.getInstance();
        int year = getCurrentContext().getSchoolYear();
        asOfDateCalendar.set(Calendar.YEAR, year);
        asOfDateCalendar.set(Calendar.MONTH, Calendar.MARCH);
        asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 31);
        PlainDate maxEndDate = new PlainDate(asOfDateCalendar.getTime());
        if (maxEndDate.before(endDate)) {
            endDate = maxEndDate;
        }

        Collection<Map<String, Object>> submissionValues = new ArrayList<>();
        Set<String> foundPeriodCodes = new HashSet<>();
        if (getReportDate().after(getCurrentContext().getStartDate())) {
            foundPeriodCodes =
                    loadSavedCodes(getCurrentContext(), school, student, getBroker());
        }
        if (debugDetail) {
            addStudentMessage(student, "foundPeriodCodes:" + foundPeriodCodes);
        }

        for (SubmissionType submissionType : submissionsOfInterest.values().stream()
                .sorted(s_submissionTypeComparator).collect(Collectors.toList())) {
            StringBuilder output2 = new StringBuilder();
            if (submissionType.isJuneSubmission()
                    || foundPeriodCodes.contains(submissionType.getSubmissionPeriodCode())) {
                continue;
            }

            output2.append("SubmissionType found: " + submissionType.getSubmissionPeriodCode());
            output2.append("\n");

            if (debugDetail) {
                addStudentMessage(student, output2.toString());
            }

            try {
                Map<String, Object> fte =
                        processSubmissionValues(submissionType, studentFteCalculations, school, student);

                if (fte != null) {
                    submissionValues.add(fte);
                }
            } catch (X2BaseException x2be) {
                String submissionPeriodCode = submissionType.getSubmissionPeriodCode();
                addStudentMessage(student, submissionPeriodCode + ": " + LoggerUtils.convertThrowableToString(x2be));
            }
        }

        PlainDate dateOfInterest = DateUtils.add(endDate, 1);
        if (debugDetail) {
            addStudentMessage(student, "June submission date of interest:" + dateOfInterest);
        }

        Optional<SubmissionType> option = getSubmissionType(dateOfInterest, dateOfInterest,
                OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY, getBroker())
                        .stream()
                        .filter(Objects::nonNull)
                        .map(submission -> {
                            if (debugDetail) {
                                addStudentMessage(student,
                                        "June submission candidate:" + submission.getSubmissionPeriodCode());
                            }
                            return submission;
                        })
                        .filter(sub -> sub.isJuneSubmission())
                        .filter(sub -> !"JUNSECSUS".equals(sub.getSubmissionPeriodCode()))
                        .sorted(s_submissionTypeComparator)
                        .findFirst();

        if (option.isPresent()) {
            SubmissionType submissionType = option.get();
            try {
                processJuneSubmission(submissionType, submissionValues, studentFteCalculations, school, student);
            } catch (X2BaseException x2be) {
                String submissionPeriodCode = submissionType.getSubmissionPeriodCode();
                addStudentMessage(student, submissionPeriodCode + ": " + LoggerUtils.convertThrowableToString(x2be));
            }
        }
        Map<String, List<Map<String, Object>>> studentMonthlyValues = submissionValues.stream()
                .collect(Collectors.groupingBy(fte -> (String) fte.get(FteMonthly.FIELD_MONTH.getAlias())));
        Map<String, List<FteMonthly>> recordsByMonth = getFteMonthlyRecords(student, school)
                .collect(Collectors.groupingBy(fte -> fte.getMonth()));

        DataDictionary dictionary = getDictExtractor().getDictionary(FteRecord.DDX_ID);

        Stream.concat(studentMonthlyValues.values().stream()
                .map(map -> Pair.of((String) map.get(0).get(FteMonthly.FIELD_MONTH.getAlias()),
                        (PlainDate) map.get(0).get(FteMonthly.FIELD_REPORT_DATE.getAlias()))),
                recordsByMonth.values().stream()
                        .map(monthly -> Pair.of(monthly.get(0).getMonth(), monthly.get(0).getReportDate())))
                .distinct().sorted(Comparator.comparing(Pair::getRight))
                .forEach(pair -> {
                    String month = pair.getLeft();
                    Map<String, Object> studentMonthlyValue =
                            studentMonthlyValues.get(month) == null ? null : studentMonthlyValues.get(month).get(0);
                    List<FteMonthly> recordsForMonth = recordsByMonth.get(month);
                    if (studentMonthlyValue != null) {
                        if (RUN_OPTION_CSV.equals(m_runOption)) {
                            List<String> values = new ArrayList();
                            values.add(school.getName());
                            values.add(student.getNameView());
                            values.add(student.getOenRaw());
                            values.add(studentMonthlyValue.get(FteMonthly.FIELD_REPORT_DATE.getAlias()).toString());
                            values.add((String) studentMonthlyValue.get(FteMonthly.FIELD_MONTH.getAlias()));
                            values.add(getCurrentContext().getContextId());
                            studentMonthlyValue.keySet().stream()
                                    .filter(alias -> !alias.equals(FteMonthly.FIELD_REPORT_DATE.getAlias()))
                                    .sorted()
                                    .forEach(alias -> values.add(alias + "=" + studentMonthlyValue.get(alias)));
                            m_csvOutput.add(values.stream().collect(Collectors.joining(",")));
                        } else {
                            FteMonthly record = null;
                            if (recordsForMonth != null) {
                                Iterator<FteMonthly> iterator = recordsForMonth.iterator();
                                record = iterator.next();
                                iterator.remove();
                                while (iterator.hasNext()) {
                                    FteMonthly fteMonthly = iterator.next();
                                    iterator.remove();
                                    recordsForMonth.remove(fteMonthly);
                                    deleteBean(fteMonthly, fteMonthly.getStudent(getBroker()));
                                }
                            }
                            if (record == null || record.isChanged(studentMonthlyValue)) {
                                UserDefinedTableD fteMonthly = null;
                                if (record == null) {
                                    fteMonthly = X2BaseBean.newInstance(UserDefinedTableD.class, dictionary);

                                    OrganizationManager.cloneOrganizationOids(fteMonthly,
                                            (OrganizationChild) student.getX2Bean());
                                    fteMonthly.setSchoolOid(school.getOid());
                                    fteMonthly.setStudentOid(student.getOid());
                                } else {
                                    fteMonthly = (UserDefinedTableD) record.getX2Bean();
                                }

                                try {
                                    writeValueToStore(studentMonthlyValue, fteMonthly);
                                } catch (X2BaseException e) {
                                    addError("processAndStoreCalculations:" + LoggerUtils.convertThrowableToString(e));
                                }
                            }
                        }
                    } else {
                        // DELETE any FTECalc records that exist
                        Iterator<FteMonthly> iterator = recordsForMonth.iterator();
                        while (iterator.hasNext()) {
                            FteMonthly fteMonthly = iterator.next();
                            iterator.remove();
                            recordsForMonth.remove(fteMonthly);
                            deleteBean(fteMonthly, fteMonthly.getStudent(getBroker()));
                        }
                    }
                });

        for (List<FteMonthly> recordsForMonth : recordsByMonth.values()) {
            // DELETE any FTECalc records that exist
            Iterator<FteMonthly> iterator = recordsForMonth.iterator();
            while (iterator.hasNext()) {
                FteMonthly fteMonthly = iterator.next();
                iterator.remove();
                recordsForMonth.remove(fteMonthly);
                deleteBean(fteMonthly, fteMonthly.getStudent(getBroker()));
            }
        }

    }

    /**
     * Process june submission.
     *
     * @param submissionType the submission type
     * @param submissionValues the submission values
     * @param school the school
     * @param student the student
     * @throws X2BaseException the x 2 base exception
     */
    private void processJuneSubmission(SubmissionType submissionType,
                                       Collection<Map<String, Object>> submissionValues,
                                       Map<PlainDate, FteCalculation> studentFteCalculations,
                                       OnSchool school,
                                       OnStudent student)
            throws X2BaseException {
        if (getDebugDetail()) {
            addStudentMessage(student, "Processing Submission Values: " + submissionType.getPeriodDescription());
        }

        /*
         * For semestered and non-semestered schools we report the FTE on the last in session day of
         * the school year.
         * Report the FTE as zero if the student left before the last in session day.
         * For quadmester and octomester schools report the March FTE.
         */
        int scheduleMode = getScheduleMode(student, school, submissionType, getBroker());
        if (scheduleMode > 2) {
            Map<String, Object> fte = new HashMap(findMarchValues(submissionValues));
            fte.put(FteMonthly.FIELD_MONTH.getAlias(), submissionType.getSubmissionPeriodCode());
            fte.put(FteMonthly.FIELD_REPORT_DATE.getAlias(), submissionType.getPeriodEndDate());
            if (isValidMonthlyRecord(fte)) {
                submissionValues.add(fte);
            }
        } else {
            Range<Date> dateRange = Range.of(submissionType.getPeriodStartDate(), getCurrentContext().getEndDate());
            Optional<OnAnnualSpan> primaryEnrollmentSpan = student.getEnrollmentSpans(getBroker(), false, true).stream()
                    .map(span -> (OnAnnualSpan) span)
                    .filter(span -> school.getOid().equals(span.getSchool().getOid()))
                    .filter(span -> span.getDateRangeSafe().isOverlap(dateRange))
                    .filter(span -> !span.isSecondary())
                    .findFirst();
            if (getDebugDetail()) {
                addStudentMessage(student, "June Primary enrollment span: " + primaryEnrollmentSpan);
            }
            if (!primaryEnrollmentSpan.isPresent()) {
                return;
            }

            OnAnnualSpan primarySpan = primaryEnrollmentSpan.get();
            ToolSchoolCalendar schoolCalendar = primarySpan.findBestCalendar(getBroker());
            PlainDate startingDate = null;
            boolean forward = false;
            PlainDate lastActiveInSessionDate =
                    schoolCalendar.findFirstInSessionDate(getBroker(), startingDate, forward);
            if (lastActiveInSessionDate == null) {
                throw new IllegalStateException("Calendar " + schoolCalendar.getCalendarId() + " for school "
                        + schoolCalendar.getSchool(getBroker()).getName() + " must have an in-session date");
            }
            if (getDebugDetail()) {
                addStudentMessage(student, "June lastActiveInSessionDate: " + lastActiveInSessionDate);
            }
            List<OnStudentScheduleSpan> scheduleSpans = getScheduleSpans(school, student);
            FteCalculation updatedFteCalculation =
                    getFteCalculationForDateofInterest(lastActiveInSessionDate, scheduleSpans, student, school);
            studentFteCalculations.put(lastActiveInSessionDate, updatedFteCalculation);

            StringBuilder debugOutput = null;
            if (getDebugDetail()) {
                debugOutput = new StringBuilder();
            }
            Map<String, Object> fte =
                    calculateFteValues(submissionType, studentFteCalculations, student, school,
                            getDictExtractor(), getBroker(), debugOutput);
            if (getDebugDetail()) {
                addStudentMessage(student, debugOutput.toString());
            }

            if (isValidMonthlyRecord(fte)) {
                submissionValues.add(fte);
            }
        }
    }

    /**
     * Process submission values.
     *
     * @param submissionType the submission type
     * @param studentFteCalculations the student fte calculations
     * @param school the school
     * @param student the student
     * @return the map
     * @throws X2BaseException the x 2 base exception
     */
    private Map<String, Object> processSubmissionValues(SubmissionType submissionType,
                                                        Map<PlainDate, FteCalculation> studentFteCalculations,
                                                        OnSchool school,
                                                        OnStudent student)
            throws X2BaseException {
        if (getDebugDetail()) {
            addStudentMessage(student, "=======================================================");
            addStudentMessage(student, "Processing Submission Values: " + submissionType.getPeriodDescription());
        }

        StringBuilder debugOutput = null;
        if (getDebugDetail()) {
            debugOutput = new StringBuilder();
        }
        Map<String, Object> fte = calculateFteValues(submissionType, studentFteCalculations, student, school,
                getDictExtractor(), getBroker(), debugOutput);
        if (getDebugDetail()) {
            addStudentMessage(student, debugOutput.toString());
        }

        if (!isValidMonthlyRecord(fte)) {
            return null;
        }
        addStudentMessage(student, "=======================================================\n");

        return fte;
    }

    /**
     * Save dirty bean.
     *
     * @param bean the bean
     * @throws X2BaseException the x 2 base exception
     */
    private void saveDirtyBean(X2BaseBean bean) throws X2BaseException {
        if (bean instanceof UserDefinedTableC && bean.isDirty()) {
            String message = bean.getOid() != null ? "Saving " : "Creating ";
            UserDefinedTableC udc = (UserDefinedTableC) bean;

            addStudentMessage(udc.getStudent(), message + enrollmentDetails(udc));
        }

        if (bean.isDirty() && RUN_OPTION_COMMIT.equals(m_runOption)) {
            getBroker().saveBeanForced(bean);
        }
    }

    /**
     * Show output.
     *
     * @param messages List<Message>
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws X2BaseException exception
     */
    private void showOutput(List<Message> messages) throws IOException, X2BaseException {
        StringBuilder output = new StringBuilder();
        List<Message> headers = getHeaderMessages();
        for (Message header : headers) {
            appendLine(output, header.getMessage());
        }
        if (!headers.isEmpty()) {
            appendLine(output, DELIMITER_LINE);
        }
        List<Message> errors = getErrors();
        for (Message error : errors) {
            appendLine(output, error.getMessage());
        }
        if (!errors.isEmpty()) {
            appendLine(output, DELIMITER_LINE);
        }
        List<Message> infos = getInfoMessages();
        for (Message info : infos) {
            appendLine(output, info.getMessage());
        }

        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(output.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Write the map of values to the bean.
     *
     * @param fte Map<String, Object>
     * @param store UserDefinedTableD
     * @throws X2BaseException the x 2 base exception
     */
    private void writeValueToStore(Map<String, Object> fte,
                                   UserDefinedTableD store)
            throws X2BaseException {
        OnStudent student = ToolBean.getBeanByOid(OnStudent.class, store.getStudentOid(), true);
        StringBuilder debugOutput = new StringBuilder();

        for (String alias : fte.keySet()) {
            Object newFieldValue = fte.get(alias);
            Object oldFieldValue = getDictExtractor().getAliasAsJavaType(store, alias, FteMonthly.DDX_ID);

            debugOutput.append(" " + alias + "=" + newFieldValue + ";");

            boolean isSame = (newFieldValue == null)
                    ? (oldFieldValue == null)
                    : newFieldValue.equals(oldFieldValue);
            if (!isSame) {
                getDictExtractor().setAliasAsJavaType(store, alias, newFieldValue, FteMonthly.DDX_ID);
            }

        }

        if (store.isDirty() && RUN_OPTION_COMMIT.equals(m_runOption)) {
            addStudentMessage(student, "Writing Monthly FTE:" + debugOutput);
            DataDictionary dataDictionary = getDictExtractor().getDictionary(FteMonthly.DDX_ID);
            getBroker().saveBeanForced(store, dataDictionary);
        } else {
            addStudentMessage(student, "Review Monthly FTE:" + debugOutput);
        }
    }

}
