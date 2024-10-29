/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanColumn;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

/**
 * The Class FteCalculation.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
/**
 * The Class FteCalculation.
 */
public class FteCalculation {
    public static final String COURSE_CODE_TYPE_PLE = "PLE";
    public static final String COURSE_REPEATED = "R";
    public static final String COURSE_REPEATED_EQV = "EQV";

    public static final int FTE_MINUTES_FT_300 = 300;
    public static final int FTE_MINUTES_PT_150 = 150;
    public static final int FTE_MINUTES_THRESHOLD_210 = 210;
    public static final int FTE_MINUTES_THRESHOLD_SAL_70 = 70;

    public static final BigDecimal HIGH_CREDIT_THRESHOLD = BigDecimal.valueOf(34);


    /**
     * The Enum EnrolmentRegister.
     */
    public enum EnrolmentRegister {
        FT, HT, LT, PT;
    }

    /**
     * The Enum Field.
     */
    enum Field {
        SCHOOL_YEAR, DATE, STUDENT, SCHOOL, CALENDAR, ENROLLMENT_REGISTER, FTE, HIGH_CREDIT_FTE, MINUTES_TO_SET, HIGH_CREDIT_MINUTES, IS_SPED, SAL, PREFIX, ADE, EARNED_CREDIT, POTENTIAL_CREDIT, EXEMPT_CREDIT, STORE, BOARD_RESIDENT_STATUS, IS_FOUR_YEAR;
    }

    private static final String COURSE_CODE_TYPE_ALF = "ALF";
    private static final String COURSE_CODE_TYPE_ELD = "ELD";
    private static final String COURSE_CODE_TYPE_ESL = "ESL";
    @SuppressWarnings("unused")
    private static final String COURSE_CODE_TYPE_PANA = "PANA";

    private static List<Field> s_storedFields =
            Arrays.asList(Field.SCHOOL_YEAR,
                    Field.DATE,
                    Field.ENROLLMENT_REGISTER,
                    Field.FTE,
                    Field.HIGH_CREDIT_FTE,
                    Field.MINUTES_TO_SET,
                    Field.HIGH_CREDIT_MINUTES);

    /**
     * Gets the alias for field.
     *
     * @param field Field
     * @return String
     */
    public static String getAliasForField(Field field) {
        String alias = null;
        if (field.equals(Field.ENROLLMENT_REGISTER)) {
            alias = FteRecord.FIELD_REGISTER.getAlias();
        } else if (field.equals(Field.SCHOOL_YEAR)) {
            alias = FteRecord.FIELD_SCHOOL_YEAR.getAlias();
        } else if (field.equals(Field.DATE)) {
            alias = FteRecord.FIELD_FTE_DATE.getAlias();
        } else if (field.equals(Field.FTE)) {
            alias = FteRecord.FIELD_FTE.getAlias();
        } else if (field.equals(Field.HIGH_CREDIT_FTE)) {
            alias = FteRecord.FIELD_FTE_HIGH_CREDIT.getAlias();
        } else if (field.equals(Field.MINUTES_TO_SET)) {
            alias = FteRecord.FIELD_MINUTES.getAlias();
        } else if (field.equals(Field.HIGH_CREDIT_MINUTES)) {
            alias = FteRecord.FIELD_MINUTES_HIGH_CREDIT.getAlias();
        }

        return alias;
    }

    /**
     * Gets the alias for field.
     *
     * @param field Field
     * @return String
     */
    public static ToolBeanColumn getColumnForField(Field field) {
        ToolBeanColumn column = null;
        if (field.equals(Field.ENROLLMENT_REGISTER)) {
            column = FteRecord.FIELD_REGISTER;
        } else if (field.equals(Field.SCHOOL_YEAR)) {
            column = FteRecord.FIELD_SCHOOL_YEAR;
        } else if (field.equals(Field.DATE)) {
            column = FteRecord.FIELD_FTE_DATE;
        } else if (field.equals(Field.FTE)) {
            column = FteRecord.FIELD_FTE;
        } else if (field.equals(Field.HIGH_CREDIT_FTE)) {
            column = FteRecord.FIELD_FTE_HIGH_CREDIT;
        } else if (field.equals(Field.MINUTES_TO_SET)) {
            column = FteRecord.FIELD_MINUTES;
        } else if (field.equals(Field.HIGH_CREDIT_MINUTES)) {
            column = FteRecord.FIELD_MINUTES_HIGH_CREDIT;
        }

        return column;
    }

    /**
     * Gets the field for alias.
     *
     * @param alias String
     * @return Field
     */
    public static Field getFieldForColumn(ToolBeanColumn alias) {
        Field field = null;

        if (FteMonthly.FIELD_FTE.equals(alias)) {
            field = Field.FTE;
        } else if (FteMonthly.FIELD_FTE_HIGH_CREDIT.equals(alias)) {
            field = Field.HIGH_CREDIT_FTE;
        } else if (FteMonthly.FIELD_MINUTES.equals(alias)) {
            field = Field.MINUTES_TO_SET;
        } else if (FteMonthly.FIELD_MINUTES_HIGH_CREDIT.equals(alias)) {
            field = Field.HIGH_CREDIT_MINUTES;
        }

        return field;
    }

    private Map<Field, Object> m_values = new TreeMap<>();

    /**
     * Instantiates a new fte calculation.
     *
     * @param asOfDate PlainDate
     * @param student SisStudent
     * @param school SisSchool
     */
    public FteCalculation(PlainDate asOfDate, OnStudent student, OnSchool school) {
        set(Field.DATE, asOfDate);
        set(Field.STUDENT, student);
        set(Field.SCHOOL, school);
        set(Field.PREFIX, UserDefinedTableC.OBJECT_PREFIX);
    }

    /**
     * Instantiates a new fte calculation.
     *
     * @param dateOfInterest the date of interest
     * @param udc the udc
     */
    public FteCalculation(PlainDate dateOfInterest, UserDefinedTableC udc) {
        this(dateOfInterest,
                (OnStudent) ToolBean.getBeanByOid(ToolStudent.class, udc.getStudentOid(), true),
                (OnSchool) ToolBean.getBeanByOid(ToolSchool.class, udc.getSchoolOid(), true));

        set(Field.PREFIX, UserDefinedTableC.OBJECT_PREFIX);
        set(Field.STORE, udc);
    }

    /**
     * Instantiates a new fte calculation.
     *
     * @param dateOfInterest the date of interest
     * @param student the student
     * @param school the school
     * @param calendar the calendar
     */
    public FteCalculation(PlainDate dateOfInterest, OnStudent student, OnSchool school, ToolSchoolCalendar calendar) {
        this(dateOfInterest, student, school);
        set(Field.CALENDAR, calendar);
    }

    /**
     * Calculate credits and minutes.
     *
     * @param filteredSpans the filtered spans
     * @param dictionaryExtractor the dictionary extractor
     * @param broker the broker
     * @return the string builder
     */
    public StringBuilder calculateCreditsAndMinutes(Collection<OnStudentScheduleSpan> filteredSpans,
                                                    DictionaryExtractor dictionaryExtractor,
                                                    X2Broker broker) {
        StringBuilder debugOutput = new StringBuilder();

        // 2a. Prior Non-Exempt Credits:
        // (i) Any non-exempt credits earned prior to the school year
        // (ii) Any non-exempt credits earned before the count date
        // but after the start of the school year
        List<OnTranscript> filteredTranscripts = getStudent().getStudentTranscripts(broker)
                .stream()
                .map(trn -> (OnTranscript) trn)
                .filter(trn -> {
                    OnSection section = (OnSection) trn.getSection(broker);

                    boolean isExempt = false;
                    boolean isPle = false;
                    if (section != null) {
                        isExempt = !StringUtils.isBlank(section.getLanguageProgram());
                        isExempt |= isExempt(section.getCourseNumber());
                        isPle = COURSE_CODE_TYPE_PLE.equals(section.getCourseCodeType());
                    } else {
                        isExempt = !StringUtils.isBlank(trn.getLanguageProgram());
                        isExempt |= isExempt(trn.getCourseNumber());
                        isPle = COURSE_CODE_TYPE_PLE.equals(trn.getCourseCodeType());
                    }
                    return !isPle && !isExempt;
                })
                .collect(Collectors.toList());

        BigDecimal priorNonExemptCredits = filteredTranscripts.stream().map(trn -> {
            BigDecimal totalCredit = trn.getTotalCredit();
            /*
             * If the repeat flag on the transcript record is set and
             * the mark is 50 or higher or the mark is EQV
             * then use the attempted credit instead of the earned credit.
             */
            boolean repeated = false;
            if (COURSE_REPEATED.equals(trn.getCourseRepeated())) {
                String finalGrade = trn.getFinalGrade();
                if (COURSE_REPEATED_EQV.equals(finalGrade)) {
                    repeated = true;
                }

                if (StringUtils.isNumeric(finalGrade)) {
                    BigDecimal numericFinal = new BigDecimal(finalGrade);
                    if (numericFinal.floatValue() >= 50) {
                        repeated = true;
                    }
                }
            }

            if (repeated) {
                String potentialCredit = trn.getPotentialCredit();
                if (StringUtils.isBlank(potentialCredit)) {
                    totalCredit = trn.getSchoolCourseCredit();
                } else {
                    try {
                        totalCredit = new BigDecimal(potentialCredit);
                    } catch (Exception e) {
                        // use school course if formatting error occurs
                        totalCredit = trn.getSchoolCourseCredit();
                    }
                }
            }

            if (totalCredit == null) {
                return BigDecimal.ZERO;
            }

            BigDecimal exemptCredits = trn.getCreditExempt();
            if (exemptCredits != null) {
                totalCredit = totalCredit.subtract(exemptCredits);
            }

            PlainDate completedDate = trn.getDateCompleted();
            PlainDate fteDate = getDate();
            if (completedDate == null || fteDate.before(completedDate)) {
                return BigDecimal.ZERO;
            }

            debugOutput.append("Transcript: " + trn.getSchoolYear() + "-"
                    + trn.getSchoolCourseNumber() + " Repeated: " + repeated + " Credit: "
                    + totalCredit + "\n");
            return totalCredit;
        })
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        debugOutput.append("\n");
        debugOutput.append("From Transcripts: " + filteredTranscripts.size());
        debugOutput.append("\n");
        debugOutput.append("Earned Credit: " + priorNonExemptCredits);
        debugOutput.append("\n\n");

        // 2b. Current Non-Exempt Credits:
        // (i) Any non-exempt credits for semestered courses in which the pupil was
        // enrolled as of the count date
        double allAverageDailyMinutes = 0;
        BigDecimal currentExemptCredits = BigDecimal.ZERO;
        BigDecimal currentNonExemptCredits = BigDecimal.ZERO;

        for (OnStudentScheduleSpan span : filteredSpans) {
            debugOutput.append("Processing span: " + span.toString());
            debugOutput.append("\n");
            OnSection mst = (OnSection) span.getSection();
            if (mst.getScheduleTerm(broker) == null) {
                debugOutput.append(span.toString());
                debugOutput.append(" - SKIPPING NO SCHEDULE TERM");
                debugOutput.append("\n");
                continue;
            } else if (!mst.getScheduleTerm(broker).getScheduleTermDates(broker).stream()
                    .anyMatch(tmd -> tmd.getRange().contains(getDate()))) {
                debugOutput.append(span.toString());
                debugOutput.append(" - SKIPPING ");
                debugOutput.append(mst.getScheduleTerm(broker).getScheduleTermDates(broker).stream()
                        .map(tmd -> tmd.getRange().toString())
                        .collect(Collectors.joining(",")));
                debugOutput.append(" NOT IN RANGE");
                debugOutput.append("\n");
                continue;
            }

            Set<String> bellSchedulesOids =
                    span.getSection().getSchedule(broker).getScheduleBells(broker).stream()
                            .map(bel -> (OnScheduleBell) bel)
                            .filter(bel -> {
                                Boolean isDefaultBellTime = bel.getDefaultBellTimeIndicator();
                                if (!bel.getDefaultBellTimeIndicator()) {
                                    return false;
                                }
                                if (bel.getDays() <= 0) {
                                    return false;
                                }
                                PlainDate startDate = bel.getStartDate();
                                PlainDate endDate = bel.getEndDate();
                                return (startDate == null || !getDate().before(startDate))
                                        && (endDate == null || !getDate().after(endDate));
                            }).sorted(new Comparator<OnScheduleBell>() {
                                @Override
                                public int compare(OnScheduleBell bel0, OnScheduleBell bel1) {
                                    PlainDate startDate0 = null;
                                    PlainDate startDate1 = null;

                                    startDate0 = bel0.getStartDate();
                                    startDate1 = bel1.getStartDate();
                                    if (startDate0 == null) {
                                        return startDate1 == null ? 0 : 1;
                                    } else if (startDate1 == null) {
                                        return -1;
                                    }
                                    return startDate0.compareTo(startDate1);
                                }
                            }).map(ToolBean::getOid).collect(Collectors.toSet());



            debugOutput.append("getBellSchedulesOids: " + bellSchedulesOids.toString());
            debugOutput.append("\n");
            double averageDailyCycleMinutes =
                    mst.getAverageDailyCycleMinutes(broker, getDate(), bellSchedulesOids, getCalendar(), debugOutput);
            debugOutput.append("averageDailyCycleMinutes: " + averageDailyCycleMinutes);
            debugOutput.append("\n");
            allAverageDailyMinutes += averageDailyCycleMinutes;

            boolean isExempt = !StringUtils.isBlank(mst.getLanguageProgram());
            isExempt |= isExempt(mst.getCourseNumber());

            BigDecimal courseCredit = BigDecimal.ZERO;
            if (mst.getCredit() != null) {
                courseCredit = mst.getCredit();
            }
            debugOutput.append("Potential Credits: " + courseCredit);
            debugOutput.append("\n");

            if (isExempt) {
                currentExemptCredits = currentExemptCredits.add(courseCredit);
            } else {
                currentNonExemptCredits = currentNonExemptCredits.add(courseCredit);
            }
        }

        set(Field.ADE, Integer.valueOf((int) Math.round(allAverageDailyMinutes)));
        set(Field.EARNED_CREDIT, priorNonExemptCredits);
        set(Field.MINUTES_TO_SET, Long.valueOf(Math.round(allAverageDailyMinutes)));
        set(Field.EXEMPT_CREDIT, currentExemptCredits);
        set(Field.POTENTIAL_CREDIT, currentNonExemptCredits);

        // boolean debugDetail = reportData.getDebugDetail();
        // if (debugDetail) {
        // parent.addStudentMessage(getStudent(), debugOutput.toString());
        // }

        return debugOutput;
    }

    /**
     * Equals.
     *
     * @param obj the obj
     * @return true, if successful
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        FteCalculation otherCal = (FteCalculation) obj;
        if (otherCal.m_values.size() != m_values.size()) {
            return false;
        }

        for (Entry<Field, Object> value : m_values.entrySet()) {
            Object otherField = otherCal.get(value.getKey());
            if (!otherField.equals(value.getValue())) {
                return false;
            }
        }

        return true;
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
     * Gets the board resident status.
     *
     * @return the board resident status
     */
    public String getBoardResidentStatus() {
        return (String) get(Field.BOARD_RESIDENT_STATUS);
    }

    /**
     * Gets the value.
     *
     * @param alias the alias
     * @return the by alias
     */
    public String getByColumn(ToolBeanColumn alias) {
        Field field = getFieldForColumn(alias);
        Object value = get(field);
        if (value != null) {
            return value.toString();
        }

        return null;
    }

    /**
     * Gets the calendar.
     *
     * @return the calendar
     */
    public ToolSchoolCalendar getCalendar() {
        return (ToolSchoolCalendar) get(Field.CALENDAR);
    }

    /**
     * Gets the FTE date.
     *
     * @return PlainDate
     */
    public PlainDate getDate() {
        return (PlainDate) get(Field.DATE);
    }

    /**
     * Gets the school.
     *
     * @return Sis school
     */
    public OnSchool getSchool() {
        return (OnSchool) get(Field.SCHOOL);
    }

    /**
     * Gets the store bean.
     *
     * @return X2BaseBean
     */
    public X2BaseBean getStore() {
        return (X2BaseBean) get(Field.STORE);
    }

    /**
     * Gets the student.
     *
     * @return Sis student
     */
    public OnStudent getStudent() {
        return (OnStudent) get(Field.STUDENT);
    }

    /**
     * Checks if is changed.
     *
     * @param fteCalculation2 the fte calculation 2
     * @return true, if is changed
     */
    public boolean isChanged(FteCalculation fteCalculation2) {
        FteCalculation.Field field = FteCalculation.Field.ENROLLMENT_REGISTER;
        boolean fEnrollmentRegisterChanged = compareValue(
                (Comparable) get(field),
                (Comparable) fteCalculation2.get(field),
                "") != 0;

        field = FteCalculation.Field.MINUTES_TO_SET;
        boolean fTotalMinutesChanged = compareValue(
                (Comparable) get(field),
                (Comparable) fteCalculation2.get(field),
                Long.valueOf(0)) != 0;

        field = FteCalculation.Field.HIGH_CREDIT_MINUTES;
        boolean fHighCreditMinutesChanged = compareValue(
                (Comparable) get(field),
                (Comparable) fteCalculation2.get(field),
                Long.valueOf(0)) != 0;

        field = FteCalculation.Field.FTE;
        boolean fFteChanged = compareValue(
                (Comparable) get(field),
                (Comparable) fteCalculation2.get(field),
                BigDecimal.ZERO) != 0;

        field = FteCalculation.Field.HIGH_CREDIT_FTE;
        boolean fFteHcChanged = compareValue(
                (Comparable) get(field),
                (Comparable) fteCalculation2.get(field),
                BigDecimal.ZERO) != 0;

        return fEnrollmentRegisterChanged || fTotalMinutesChanged || fFteChanged
                || fHighCreditMinutesChanged || fFteHcChanged;
    }

    /**
     * Checks if is changed.
     *
     * @param dictionaryExtractor the dictionary extractor
     * @param record the record
     * @return true, if is changed
     */
    public boolean isChanged(DictionaryExtractor dictionaryExtractor, FteRecord record) {
        for (Field field : s_storedFields) {
            ToolBeanColumn aliasForField = getColumnForField(field);
            Comparable newFieldValue = (Comparable) get(field);
            Comparable oldFieldValue = (Comparable) dictionaryExtractor.getAliasAsJavaType(record, aliasForField);
            boolean isSame = (newFieldValue == null)
                    ? (oldFieldValue == null)
                    : (oldFieldValue == null ? false : newFieldValue.compareTo(oldFieldValue) == 0);
            if (!isSame) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if is different span.
     *
     * @param broker the broker
     * @param fteCalculation2 the fte calculation 2
     * @return true, if is different span
     */
    public boolean isDifferentSpan(X2Broker broker, FteCalculation fteCalculation2) {
        Optional<OnAnnualSpan> optionSpan1 = getEnrollmentSpan(broker);
        Optional<OnAnnualSpan> optionSpan2 = fteCalculation2.getEnrollmentSpan(broker);

        if (!optionSpan1.isPresent() && !optionSpan2.isPresent()) {
            return false;
        }

        if (!optionSpan1.isPresent() || !optionSpan2.isPresent()) {
            return true;
        }

        return !optionSpan1.get().getFirstActiveEnrollment().getOid()
                .equals(optionSpan2.get().getFirstActiveEnrollment().getOid());
    }

    /**
     * Checks if is exempt.
     *
     * @param courseNumber the course number
     * @return true, if is exempt
     */
    public boolean isExempt(String courseNumber) {
        if (StringUtils.isBlank(courseNumber)) {
            return false;
        }

        if (courseNumber.startsWith(COURSE_CODE_TYPE_ALF)
                || courseNumber.startsWith(COURSE_CODE_TYPE_ELD)
                || courseNumber.startsWith(COURSE_CODE_TYPE_ESL)
        // TODO: Determine how to identify PANA courses
        // || courseNumber.startsWith(COURSE_CODE_TYPE_PANA)
        ) {
            return true;
        }

        return false;
    }

    /**
     * Checks if is sal.
     *
     * @return true, if is sal
     */
    public boolean isSAL() {
        Boolean isSalEnrolled = (Boolean) get(Field.SAL);

        return isSalEnrolled == null ? false : isSalEnrolled.booleanValue();
    }

    /**
     * Read values from store.
     *
     * @param dictionaryExtractor the dictionary extractor
     * @throws X2BaseException the x 2 base exception
     */
    public void readValuesFromStore(DictionaryExtractor dictionaryExtractor) throws X2BaseException {
        X2BaseBean store = getStore();
        if (store == null) {
            throw new RuntimeException("Missing store bean on read attempt!");
        }

        for (Field field : s_storedFields) {
            String aliasForField = getAliasForField(field);
            Object fieldValueByAlias = dictionaryExtractor.getAliasAsJavaType(store, aliasForField, FteRecord.DDX_ID);
            set(field, fieldValueByAlias);
        }
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

    /**
     * Sets the school year.
     *
     * @param schoolYear the new school year
     */
    public void setSchoolYear(String schoolYear) {
        set(Field.SCHOOL_YEAR, schoolYear);
    }

    /**
     * Implement specification driven FTE calculation.
     *
     * @param broker the broker
     * @param scaleFte the scale fte
     * @param scaleFteHc the scale fte hc
     */

    public void setFte(X2Broker broker, int scaleFte, int scaleFteHc) {
        BigDecimal priorNonExemptCredits = (BigDecimal) get(Field.EARNED_CREDIT);
        BigDecimal currentExemptCredits = (BigDecimal) get(Field.EXEMPT_CREDIT);
        BigDecimal currentNonExemptCredits = (BigDecimal) get(Field.POTENTIAL_CREDIT);

        // 2c. Total Non-Exempt Credits (Prior+Current):
        // (a) + (b) above as determined for the count date
        BigDecimal totalNonExemptCredits = priorNonExemptCredits.add(currentNonExemptCredits);

        // 2d. Current Total (Exempt + Non-Exempt) Credits:
        // (i) Any credits for semestered courses in which the pupil was enrolled
        // as of the count date
        BigDecimal currentTotalCredits = currentExemptCredits.add(currentNonExemptCredits);
        int threshold = isSAL() ? FTE_MINUTES_THRESHOLD_SAL_70 : FTE_MINUTES_THRESHOLD_210;
        // double fullTimeMinutes = threshold / 0.7; // 70% is full time
        long totalMinutesOfInstruction = ((Long) get(Field.MINUTES_TO_SET)).longValue();

        if (totalMinutesOfInstruction >= threshold) {
            set(Field.ENROLLMENT_REGISTER, EnrolmentRegister.FT.toString());
            if (isSAL()) {
                set(Field.MINUTES_TO_SET, Long.valueOf(FTE_MINUTES_FT_300));
            }
        } else {
            set(Field.ENROLLMENT_REGISTER, EnrolmentRegister.PT.toString());
            if (isSAL()) {
                set(Field.MINUTES_TO_SET, Long.valueOf(FTE_MINUTES_PT_150));
            }
        }

        // 3: CALCULATE THE HIGH-CREDIT FACTOR
        // Divide the number of high credits by the total number of credits
        // the pupil is enrolled in to calculate the high-credit factor.

        /*
         * 3a. Number of High Credits:
         *
         * If total non-exempt credits [Step 2 (c)] is less than or equal to 34,
         * then high credits = 0
         *
         * If prior non-exempt credits [Step 2 (a)] is greater than or equal to 34
         * then high credits = All current non-exempt credits [Step 2 (b)]
         *
         * OR
         *
         * If prior non-exempt credits [Step 2 (a)] is less than 34 and total non-exempt credits
         * [Step 2 (c)] is greater than 34
         * then high credits = total non-exempt credits [Step 2 (c)] minus 34
         */
        BigDecimal highCredits;
        if (totalNonExemptCredits.compareTo(HIGH_CREDIT_THRESHOLD) <= 0) {
            highCredits = BigDecimal.valueOf(0d);
        } else if (priorNonExemptCredits.compareTo(HIGH_CREDIT_THRESHOLD) >= 0) {
            highCredits = currentNonExemptCredits;
        } else {
            highCredits = totalNonExemptCredits.subtract(HIGH_CREDIT_THRESHOLD);
        }

        // Pupils who have an Individual Education Plan (IEP) on the count date are exempt from
        // the 34 credit threshold.
        boolean isSped = getStudent().getSpedPrograms(broker).stream()
                .filter(pgm -> pgm.getDateRange().contains(getDate()))
                .flatMap(pgm -> pgm.getProgramDetails(broker).stream())
                .filter(pgd -> OnStudentSpedDetail.TYPE_PLACEMENT.equals(pgd.getType()))
                .filter(pgd -> pgd.getDateRange().contains(getDate()))
                .anyMatch(pgd -> pgd.getIepRequiredIndicator());
        set(Field.IS_SPED, Boolean.valueOf(isSped));
        if (isSped) {
            highCredits = BigDecimal.valueOf(0d);
        }

        // Calculate four year
        boolean isFourYear = false;
        int grade9Year = getStudent().getGrade9Cohort();
        if (grade9Year > 1000) {
            String thisCohort = (String) this.get(Field.SCHOOL_YEAR);
            if (thisCohort != null && thisCohort.length() > 3) {
                int thisYear = Integer.valueOf(thisCohort.substring(0, 4));
                if (thisYear < grade9Year + 4) {
                    isFourYear = true;
                }
            }
        }
        set(Field.IS_FOUR_YEAR, Boolean.valueOf(isFourYear));
        if (isFourYear) {
            highCredits = BigDecimal.valueOf(0d);
        }

        // Pupils who have a OPOB status on the count date are exempt from limit.
        boolean isOtherPupilOftheBoard =
                OnsisConstants.VALUES_STU_BRD_RES_STAT_OTHER_PUPIL_OF_BOARD.contains(getBoardResidentStatus());
        if (isOtherPupilOftheBoard) {
            highCredits = BigDecimal.valueOf(0d);
        }

        // Pupils 21 or older are exempt from high credit
        try {
            Calendar asOfDateCalendar = Calendar.getInstance();
            int year = Integer.parseInt(((String) this.get(Field.SCHOOL_YEAR)).substring(0, 4));
            asOfDateCalendar.set(Calendar.YEAR, year);
            asOfDateCalendar.set(Calendar.MONTH, Calendar.DECEMBER);
            asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 31);
            PlainDate dateDecember31 = new PlainDate(asOfDateCalendar.getTime());
            if (getStudent().getAgeAsOfDate(dateDecember31) >= 21) {
                highCredits = BigDecimal.valueOf(0d);
            }
        } catch (NumberFormatException e) {
            // skip if error occurs
        }

        // 3b. Number of Regular Credits:
        // Subtract high credits [Step 3(a)] from current total credits [Step 2 (d)]
        // for the count date
        // BigDecimal regularCredits = currentTotalCredits.subtract(highCredits);

        // 3c. High-CreditFactor
        // High credits [Step 3(a)] divided by current total credits [Step 2(d)]
        // for the count date
        BigDecimal highCreditFactor = BigDecimal.valueOf(0d);
        if (!isSped) {
            if (currentTotalCredits.doubleValue() > 0) {
                highCreditFactor = highCredits.divide(currentTotalCredits, 2, RoundingMode.HALF_DOWN);
            } else if (priorNonExemptCredits.compareTo(HIGH_CREDIT_THRESHOLD) >= 0) {
                highCreditFactor = BigDecimal.valueOf(1.0d);
            }
        }

        // 4b. High-Credit Minutes:
        // Multiply the pupil's total day school minutes [Step 4(a)] by the high-credit factor
        // [Step 3(c)] for the count date
        long highCreditMinutes =
                BigDecimal.valueOf(totalMinutesOfInstruction).multiply(highCreditFactor).longValue();
        if (isSped) {
            highCreditMinutes = 0l;
        }

        // 4c. Regular Minutes (i.e. Minutes that are not High-Credit):
        // Subtract the pupil's high-credit day school minutes [Step 4(b)] from the pupil's
        // total day school minutes [Step 4(a)] for the count date
        long regularMinutes = totalMinutesOfInstruction - highCreditMinutes;

        // 5: CALCULATE DAY SCHOOL FTE AND ADE
        /*
         * 5a. Regular Day School FTE:
         * If the pupil has at least 210 regular minutes of instruction per day [Step 4(c)] then
         * regular day school FTE = 1.0
         * OR
         * If the pupil has less than 210 regular minutes of instruction per day [Step 4(c)]
         * then regular day school FTE
         * = Regular minutes of instruction per day [Step 4(c)] divided by 300
         */
        BigDecimal fte;
        if (regularMinutes >= threshold) {
            fte = BigDecimal.valueOf(1.0d);
        } else {
            fte = BigDecimal.valueOf(regularMinutes).divide(BigDecimal.valueOf(FTE_MINUTES_FT_300), 2,
                    RoundingMode.HALF_DOWN);
            if (isSAL()) {
                fte = BigDecimal.valueOf(0.5);
            }
        }

        // 5b. Regular Day School ADE:
        // The regular day school ADE is calculated by taking the average of the regular day
        // school FTE [Step 5(a)] for the October and March count dates.

        /*
         * 5c. High-Credit Day School FTE:
         *
         * If the pupil has at least 210 regular minutes of instruction per day [Step 4(c)],
         * then high-credit day school FTE = 0.0
         * [no high-credit day school FTE because the regular day school FTE = 1.0]
         * OR
         * If the pupil has at least 210 total minutes of instruction per day [Step 4(a)],
         * then high-credit day school FTE = 1.0 minus the regular day school FTE [Step 5(a)]
         * OR
         * If the pupil has less than 210 total minutes of instruction per day [Step 4(a)],
         * then high-credit day school FTE = high-credit minutes of instruction per day
         * [Step 4(b)] divided by 300
         */
        BigDecimal highCreditFte;
        if (regularMinutes >= threshold) {
            highCreditFte = BigDecimal.valueOf(0d);
        } else {
            if (totalMinutesOfInstruction >= threshold) {
                highCreditFte = BigDecimal.valueOf(1.0d).subtract(fte);
            } else {
                highCreditFte =
                        BigDecimal.valueOf(highCreditMinutes).divide(BigDecimal.valueOf(FTE_MINUTES_FT_300), 2,
                                RoundingMode.HALF_DOWN);
            }
        }

        if (isSped) {
            highCreditFte = BigDecimal.valueOf(0d);
        }

        if (!isSAL()) {
            set(Field.MINUTES_TO_SET, Long.valueOf(regularMinutes));
        }

        set(Field.FTE, fte.setScale(scaleFte, RoundingMode.HALF_UP));
        set(Field.HIGH_CREDIT_FTE, highCreditFte.setScale(scaleFteHc, RoundingMode.HALF_UP));
        set(Field.HIGH_CREDIT_MINUTES, Long.valueOf(highCreditMinutes));
    }

    /**
     * Set the FTE for SAL Student.
     *
     * @param pgmSAL
     * @param dictionaryExtractor
     */
    public void setFteForSAL(DictionaryExtractor dictionaryExtractor, OnStudentSalep pgmSAL) {
        String salCode = pgmSAL.getAttendanceType();
        if (OnStudentSalep.SAL_ATT_FULL_TIME.equals(salCode)) {
            set(Field.ENROLLMENT_REGISTER, EnrolmentRegister.FT.toString());
            set(Field.FTE, BigDecimal.valueOf(1.0));
            set(Field.HIGH_CREDIT_FTE, BigDecimal.valueOf(0.0));
            set(Field.MINUTES_TO_SET, Long.valueOf(FTE_MINUTES_FT_300));
        } else {
            set(Field.ENROLLMENT_REGISTER, EnrolmentRegister.PT.toString());
            set(Field.FTE, BigDecimal.valueOf(0.5));
            set(Field.HIGH_CREDIT_FTE, BigDecimal.valueOf(0.0));
            set(Field.MINUTES_TO_SET, Long.valueOf(FTE_MINUTES_PT_150));
        }
    }

    /**
     * Sets the board resident status.
     *
     * @param boardResidentStatus the new board resident status
     */
    public void setBoardResidentStatus(String boardResidentStatus) {
        set(Field.BOARD_RESIDENT_STATUS, boardResidentStatus);
    }

    /**
     * Sets the sal.
     *
     * @param isSalEnrolled the new sal
     */
    public void setSAL(Boolean isSalEnrolled) {
        set(Field.SAL, isSalEnrolled);
    }

    /**
     * Sets the store.
     *
     * @param bean the bean
     * @param prefix the prefix
     */
    public void setStore(X2BaseBean bean, String prefix) {
        set(Field.PREFIX, prefix);
        set(Field.STORE, bean);
    }

    /**
     * To string.
     *
     * @return the string
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder output = new StringBuilder();
        m_values.entrySet().stream().sorted(new Comparator<Map.Entry<Field, Object>>() {
            @Override
            public int compare(Entry<Field, Object> arg0, Entry<Field, Object> arg1) {
                return arg0.getKey().compareTo(arg1.getKey());
            }
        }).forEach(value -> {
            if (output.length() > 0) {
                output.append(", ");
            }
            output.append(value.getKey());
            output.append("=");
            output.append(
                    value.getValue() instanceof ToolBean ? ((ToolBean) value.getValue()).getOid() : value.getValue());
        });
        return output.toString();
    }

    /**
     * Write values to store.
     *
     * @param dictionaryExtractor the dictionary extractor
     * @throws X2BaseException the x 2 base exception
     */
    public void writeValuesToStore(DictionaryExtractor dictionaryExtractor) throws X2BaseException {
        writeValuesToStore(dictionaryExtractor, getStore());
    }

    /**
     * Write values to store.
     *
     * @param dictionaryExtractor the dictionary extractor
     * @param store the store
     * @throws X2BaseException the x 2 base exception
     */
    public void writeValuesToStore(DictionaryExtractor dictionaryExtractor, X2BaseBean store)
            throws X2BaseException {
        if (store == null) {
            throw new RuntimeException("Missing store bean on write attempt!");
        }

        for (Field field : s_storedFields) {
            String aliasForField = getAliasForField(field);
            Object newFieldValue = get(field);
            Object oldFieldValue = dictionaryExtractor.getAliasAsJavaType(store, aliasForField, FteRecord.DDX_ID);
            boolean isSame = (newFieldValue == null)
                    ? (oldFieldValue == null)
                    : newFieldValue.equals(oldFieldValue);
            if (!isSame) {
                dictionaryExtractor.setAliasAsJavaType(store, aliasForField, newFieldValue, FteRecord.DDX_ID);
            }
        }
    }

    /**
     * Write values to map.
     *
     * @return Map
     */
    public Map<String, String> writeValuesToMap() {
        Map<String, String> values = new HashMap();

        for (Field field : s_storedFields) {
            String aliasForField = getAliasForField(field);
            String newFieldValue = get(field) == null ? null : get(field).toString();
            values.put(aliasForField, newFieldValue);
        }

        return values;
    }

    /**
     * Compare value.
     *
     * @param current the current
     * @param next the next
     * @param defaultForNull the default for null
     * @return the int
     */
    private int compareValue(Comparable current, Comparable next, Comparable defaultForNull) {
        return (current == null ? defaultForNull : current).compareTo((next == null ? defaultForNull : next));
    }

    /**
     * Gets the enrollment span.
     *
     * @param broker the broker
     * @return the enrollment span
     */
    private Optional<OnAnnualSpan> getEnrollmentSpan(X2Broker broker) {
        PlainDate activeOnDate = getDate();
        return getStudent().getEnrollmentSpans(broker, false, true).stream()
                .map(span -> (OnAnnualSpan) span)
                .filter(span -> !span.getFirstActiveInSessionDate().after(activeOnDate)
                        && (span.getLastActiveInSessionDate() == null
                                || !span.getLastActiveInSessionDate().before(activeOnDate)))
                .filter(span -> span.getSchool().getOid().equals(getSchool().getOid()))
                .filter(span -> !span.isSecondary()).findFirst();
    }
}

