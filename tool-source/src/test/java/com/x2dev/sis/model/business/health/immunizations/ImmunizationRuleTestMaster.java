/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.sis.model.business.health.immunizations;

import static org.junit.Assert.fail;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.x2dev.sis.model.beans.HealthImmunizationDefinition;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.sis.model.business.health.ImmunizationRule;
import com.x2dev.sis.model.business.health.RuleSet;
import com.x2dev.sis.web.health.ImmunizationRuleException;
import com.x2dev.sis.web.health.ImmunizationRuleParser;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.InputStream;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import mockit.Delegate;
import mockit.Mock;
import mockit.MockUp;
import mockit.Mocked;
import mockit.NonStrictExpectations;

/**
 * The Class ImmunizationRuleTestMaster.
 */
public class ImmunizationRuleTestMaster {
    static String ERROR_EVALUATE_MISSING_DOSE = "error.health.evaluate.missingDose";
    static String ERROR_EVALUATE_GRADERANGE_NOT_INCLUSIVE = "error.health.evaluate.gradeRangeNotInclusive";
    static String ERROR_EVALUATE_COUNT_LOW_DOSES_GRADE_RANGE = "error.health.evaluate.countLowDosesGradeRange";
    static String ERROR_EVALUATE_DEPENDENT_DATE_SPAN = "error.health.evaluate.dependentDateSpan";
    static String ERROR_EVALUATE_COUNT_LOW_DOSES = "error.health.evaluate.countLowDoses";
    static String ERROR_EVALUATE_OUTSIDE_RANGE = "error.health.evaluate.outsideRange";
    static String ERROR_EVALUATE_MISSING_DOSE_GRADE = "error.health.evaluate.missingDoseGrade";
    static String ERROR_EVALUATE_PAST_GRADE_LEVEL = "error.health.evaluate.outsideGradeLevel";
    static String ERROR_EVALUATE_COUNT_COMBO_MISSING_GRADE = "error.health.evaluate.countComboMissingGrade";

    @Mocked
    X2Broker m_broker;
    @Mocked
    GradeLevelHistory m_studentHistory;
    @Mocked
    DistrictSchoolYearContext m_schoolYear;
    @Mocked
    SisStudent m_student;
    @Mocked
    Organization org;
    @Mocked
    LocalizationMessageResources lmr;
    @Mocked
    Student m_testStudent;

    LocalizationCache lc;

    public Map<Integer, String> m_gradeHistory;
    public Collection<DistrictSchoolYearContext> m_schoolYears = new HashSet<DistrictSchoolYearContext>();
    public Integer m_studentGradeLevel;
    public PlainDate m_startDate;
    public PlainDate m_endDate;
    public RuleSet m_completeRuleSet;
    public static String m_today;
    public String m_studentGender;

    private List<HealthImmunizationDose> m_doses;
    private String studentDOBandDosesString;

    /**
     * Set the DOB and Dose information for the student in the form of DOB, Dose1, Dose2, ... DoseN
     * and the format of the date as YYYY-MM-DD
     * Takes student information in one string
     *
     * @param studentDOBandDoses void
     */
    public void setStudentDOBandDosesString(String studentDOBandDoses) {
        this.studentDOBandDosesString = studentDOBandDoses;
    }

    /**
     * Set the DOB and Dose information for the student in the form of DOB, Dose1, Dose2, ... DoseN
     * and the format of the date as YYYY-MM-DD
     *
     * @param studentDOB Takes student DOB as YYYY-MM-DD
     * @param doses Takes doses as YYYY-MM-DD, YYYY-MM-DD...
     */
    public void setStudentDOBandDosesString(String studentDOB, String doses) {
        this.studentDOBandDosesString = studentDOB + ", " + doses;
    }

    /**
     * Set the DOB and Dose information for the student in the form of DOB, Dose1, Dose2, ... DoseN
     * and the format of the date as YYYY-MM-DD
     *
     * @param studentDOB Takes student DOB as YYYY-MM-DD
     * @param doses Takes doses as a built string array and puts them together with DOB. Expects
     *        YYYY-MM-DD for structure
     */
    public void setStudentDOBandDosesString(String studentDOB, String[] doses) {
        this.studentDOBandDosesString = studentDOB;
        for (String adose : doses) {
            this.studentDOBandDosesString += ", " + adose;
        }
    }

    public InputStream m_xmlStream;
    public String m_xmlResourceLocation;

    /**
     * Setup of the test suite including mocking out the system. When this method is called, the xml
     * path MUST be set. There is another method that allows you to pass the xml path as a
     * parameter.
     *
     * @param today void
     * @param xmlPath
     */
    public void setup(String today, String xmlPath) {
        m_today = today;
        this.m_xmlResourceLocation = xmlPath;

        // Mocking out org and org call so that we can get a result when localization is being
        // called.
        mockOrganization();

        // Mock out error messages so that we can easily figure out what error we are getting and
        // how to fix the rule/test
        mockHealthErrorMessages();

        resetToday();

        parseRuleSetForTesting();
    }

    /**
     * Take supplied rule set xml from lower level test setup and set it inside of the global rule
     * set variable.
     */
    private void parseRuleSetForTesting() {
        if (m_completeRuleSet == null) {
            if (m_xmlResourceLocation == null) {
                fail("The xmlpath MUST be set.");
            }

            m_xmlStream = ImmunizationRuleTestMaster.class.getResourceAsStream(m_xmlResourceLocation);
            if (m_xmlStream == null) {
                fail("XML path invalid or incorrect. "+m_xmlResourceLocation);
            }
            m_completeRuleSet = parseRule(m_xmlStream);
        }
    }

    /**
     * Setup the messages from the locale calls to return worthwhile error messages. This will need
     * to be modified every time a new message is encountered.
     */
    private void mockHealthErrorMessages() {
        new NonStrictExpectations(LocalizationCache.class) {
            {
                LocalizationCache.getMessages((PersistenceKey) any);
                result = lmr;
            }
        };
        new NonStrictExpectations() {
            {
                lmr.getMessage(anyString, (Object[]) any);
                result = new Delegate() {
                    @SuppressWarnings("unused")
                    String innerDelegateMethod(String message, Object[] args) {
                        if (ERROR_EVALUATE_MISSING_DOSE.equals(message)) {
                            return " Dose #" + args[0] + " is missing and must be given between " + args[1] + " "
                                    + args[2] + " and " + args[3] + " " + args[4] + " old";
                        } else if (ERROR_EVALUATE_GRADERANGE_NOT_INCLUSIVE.equals(message)) {
                            return " Dose #" + args[0] + " was not given between grades " + args[1] + " and " + args[2];
                        } else if (ERROR_EVALUATE_COUNT_LOW_DOSES_GRADE_RANGE.equals(message)) {
                            return args[0] + " doses are required and only found " + args[1] + " between grades "
                                    + args[2] + " and " + args[3];
                        } else if (ERROR_EVALUATE_DEPENDENT_DATE_SPAN.equals(message)) {
                            return "Dose #" + args[0] + " did not occur at least " + args[1] + " " + args[2]
                                    + " after dose #" + args[3];
                        } else if (ERROR_EVALUATE_COUNT_LOW_DOSES.equals(message)) {
                            return args[0] + " doses are required and only " + args[1] + " was/were found";
                        } else if (ERROR_EVALUATE_OUTSIDE_RANGE.equals(message)) {
                            return "Dose #" + args[0] + " was not given between " + args[1] + " " + args[2] + " and "
                                    + args[3] + " " + args[4] + " old";
                        } else if (ERROR_EVALUATE_MISSING_DOSE_GRADE.equals(message)) {
                            return "Dose #" + args[0] + "  is missing and must be given before grade " + args[1];
                        } else if (ERROR_EVALUATE_PAST_GRADE_LEVEL.equals(message)) {
                            return "Dose #" + args[0] + "  was not given before grade  " + args[1];
                        } else if (ERROR_EVALUATE_COUNT_COMBO_MISSING_GRADE.equals(message)) {
                            return args[0] + " doses are required before grade " + args[1] + " and between " + args[2]
                                    + " and " + args[3] + " " + args[4] + " old. Only " + args[5] + " was/were found";
                        }
                        return "Unable to match error! " + message;
                    }
                };
            }
        };
    }

    /**
     * Mocks and end mocks the organization calls for the students. This way we don't get into a
     * recursive loop with the cascading mock.
     */
    private void mockOrganization() {
        new NonStrictExpectations() {
            {
                m_studentHistory.getOrganization();
                result = org;
            }
        };
        new NonStrictExpectations() {
            {
                org.getParentOrganization();
                result = null;
            }
        };
    }

    /**
     * Sets the variable of today, then re-initializes the mock of the system time.
     *
     * @param today String
     */
    public void resetToday(String today) {
        m_today = today;
        resetToday();
    }

    /**
     * If tester supplies a date for 'today' mock the system to return that value instead of the
     * real one. Changes all basic date creations.
     */
    public void resetToday() {
        if (m_today != null) {
            /*
             * new MockUp<System>()
             * {
             *
             * @Mock long currentTimeMillis()
             * {
             * return Date.valueOf(m_today).getTime();
             * }
             * };
             */
            new MockUp<ImmunizationRule>() {
                @Mock
                PlainDate _getEvaluationDate() {
                    return new PlainDate(Date.valueOf(m_today));
                }
            };

            final Calendar todayMocked = Calendar.getInstance();
            PlainDate evaluationDate = new PlainDate(Date.valueOf(m_today));
            todayMocked.setTime(evaluationDate);
            new NonStrictExpectations() {
                { // m_schoolYear
                    m_schoolYear.getSchoolYear();
                    result = Integer.valueOf(todayMocked.get(Calendar.YEAR));
                }
            };
        }
    }

    /**
     * Generates the Rule set for the given xml input stream.
     *
     * @param xml InputStream
     * @return RuleSet
     */
    public RuleSet parseRule(InputStream xml) {
        RuleSet parsedXML = null;
        ImmunizationRuleParser parser = new ImmunizationRuleParser(m_studentHistory, m_broker);
        try {
            parsedXML = parser.parse(xml);
        } catch (ImmunizationRuleException e) {
            fail("Error parsing rule " + e);
        }
        return parsedXML;
    }

    /**
     * Takes an integer to set the students grade level, and a string of dose dates. NO ERROR
     * CHECKING ON BIRTHDATE AND DOSE DATES
     *
     * @param gradeLevel int
     * @param doses String
     */
    public void parseIntIntoStudentAndDoses(int gradeLevel, String doses) {
        buildBirthdayFromGradeLevel(gradeLevel);
        studentDOBandDosesString += ", " + doses;
        parseStudentAndDoses();
    }

    /**
     * Gets a student based on today (mocked or not) and then sets grade level. Then adds the doses
     * param on and calls the student parsing.
     * Reword
     * Students birthday is based on today, the year is calculated by taking the current year and
     * subtracting 6 years and the grade level
     *
     * @param gradeLevel int
     * @param daysBetweenDoses int[]
     */
    public void parseIntIntoStudentAndDoses(int gradeLevel, int[] daysBetweenDoses) {
        Calendar birthday = buildBirthdayFromGradeLevel(gradeLevel);
        parseDoses(birthday, daysBetweenDoses);
    }

    /**
     * Takes a calendar day for the birthday, and uses an int array to build doses. See
     * parseDoses(String, int[]) for more info
     *
     * @param birthday Calendar
     * @param daysBetweenDoses int[]
     */
    public void parseDoses(Calendar birthday, int[] daysBetweenDoses) {
        buildDoseDates(daysBetweenDoses, birthday);
        parseStudentAndDoses();
    }

    /**
     * Parses birthday into a string, then creates a dose string based on the int[]. Adds each int
     * in days onto the last date
     *
     * @param birthday String
     * @param daysBetweenDoses int[]
     */
    public void parseDoses(String birthday, int[] daysBetweenDoses) {
        studentDOBandDosesString = birthday;
        Calendar birthdayCal = Calendar.getInstance();
        birthdayCal.setTime(Date.valueOf(birthday));
        buildDoseDates(daysBetweenDoses, birthdayCal);
        parseStudentAndDoses();
    }

    /**
     * Uses the idea that the student is 6 in K. Then builds a birthday based on the int grade level
     * and today (mocked or not)
     *
     * @param gradeLevel int
     * @return Calendar
     */
    private Calendar buildBirthdayFromGradeLevel(int gradeLevel) {
        // Chance numbers to be constants for ease of reading
        Calendar today = Calendar.getInstance();
        if (m_today != null) {
            PlainDate evaluationDate = new PlainDate(Date.valueOf(m_today));
            today.setTime(evaluationDate);
        }
        if (today.get(Calendar.MONTH) > 6 && today.get(Calendar.MONTH) < 10) {
            today.set(Calendar.MONTH, 10);
        }
        today.set(Calendar.YEAR, today.get(Calendar.YEAR) - (gradeLevel + 6));
        studentDOBandDosesString = buildDateString(today);
        return today;
    }

    /**
     * Uses today as a start date, then adds doses to default string based on days in the int[].
     *
     * @param daysBetweenDoses int[]
     * @param today Calendar
     */
    private void buildDoseDates(int[] daysBetweenDoses, Calendar today) {
        for (int daysToAdd : daysBetweenDoses) {
            today.add(Calendar.DATE, daysToAdd);
            studentDOBandDosesString += ", " + buildDateString(today);
        }
    }

    /**
     * Builds string to be used for calendar.
     *
     * @param dateToBeConverted Calendar
     * @return String
     */
    private String buildDateString(Calendar dateToBeConverted) {
        int month = 1 + dateToBeConverted.get(Calendar.MONTH);
        int day = dateToBeConverted.get(Calendar.DAY_OF_MONTH);
        String returnString = dateToBeConverted.get(Calendar.YEAR) + "-";
        returnString += ((month < 10) ? "0" : "") + month + "-";
        returnString += ((day < 10) ? "0" : "") + day;
        return returnString;
    }

    /**
     * Breaks apart student and doses and handles each.
     */
    public void parseStudentAndDoses() {
        ArrayList<String> studentAndDoses = StringUtils.convertDelimitedStringToList(studentDOBandDosesString, ',');
        if (studentAndDoses != null) {
            buildStudentInformation(studentAndDoses);
            buildDoseArray(studentAndDoses);
        }
    }

    /**
     * Parses out student dob, then builds grade level, school info, and mocks those out.
     *
     * @param studentAndDoses ArrayList<String>
     */
    private void buildStudentInformation(ArrayList<String> studentAndDoses) {
        if (studentAndDoses.size() > 0) {
            // Setup the dob and gender (if specified), then create student history
            String dobString = studentAndDoses.get(0);
            m_studentGender = null;
            if (dobString.contains(":")) {
                String[] parsedDobString = dobString.split(":");
                dobString = parsedDobString[0];
                m_studentGender = parsedDobString[1];
            }

            final PlainDate dob = new PlainDate(Date.valueOf(dobString));

            new NonStrictExpectations() {
                {
                    m_student.getPerson().getDob();
                    result = dob;
                }
            };

            if (m_studentGender != null) {
                new NonStrictExpectations() {
                    {
                        m_student.getPerson().getGenderCode();
                        result = m_studentGender;
                    }
                };
            }

            Calendar cal = Calendar.getInstance();
            cal.setTime(dob);

            buildGradeHistoryBasedOnCalendar(cal);
            mockGradeLevelReturns();
            mockSchoolYearReturns();
        }
    }

    /**
     * Takes string array and parses it into dose information then stores it for later use.
     *
     * @param studentAndDoses ArrayList<String>
     */
    private void buildDoseArray(ArrayList<String> studentAndDoses) {
        m_doses = new ArrayList<HealthImmunizationDose>();
        if (studentAndDoses == null || studentAndDoses.size() < 2) {
            return;
        }
        for (int doseCount = 1; doseCount < studentAndDoses.size(); doseCount++) {
            String doseString = studentAndDoses.get(doseCount).trim();
            String dateString;
            String seriesId = null;
            if (doseString.contains(":")) {
                String[] parsedDoseString = doseString.split(":");
                dateString = parsedDoseString[0];
                seriesId = parsedDoseString[1];
            } else {
                dateString = doseString;
            }

            // If a series ID was specified, establish the necessary relationships
            HealthImmunizationSeries mySeries = null;
            if (seriesId != null) {
                final HealthImmunizationDefinition definition = new HealthImmunizationDefinition(
                        m_broker.getPersistenceKey());
                definition.setSeriesId(seriesId);

                mySeries = new HealthImmunizationSeries(m_broker.getPersistenceKey()) {
                    @Override
                    public HealthImmunizationDefinition getImmunizationDefinition() {
                        return definition;
                    }

                    @Override
                    public SisStudent getStudent() {
                        return m_student;
                    }
                };
            }
            final HealthImmunizationSeries series = mySeries;
            final HealthImmunizationDose dose = new HealthImmunizationDose(m_broker.getPersistenceKey()) {
                @Override
                public HealthImmunizationSeries getImmunizationSeries() {
                    return series;
                }

            };
            dose.setDate(new PlainDate(Date.valueOf(dateString)));
            dose.setStudentOid(m_student.getOid());
            m_doses.add(dose);
        }
    }

    /**
     * Sets the start and end date for the school year, and sets the school years for the student
     * history.
     */
    private void mockSchoolYearReturns() {
        new NonStrictExpectations() {
            {
                m_schoolYear.getStartDate();
                result = m_startDate;
                m_schoolYear.getEndDate();
                result = m_endDate;
            }
        };
        new NonStrictExpectations() {
            {
                m_studentHistory.getSchoolYears();
                result = m_schoolYears;
            }
        };
    }

    /**
     * Mocks out the student grade levels and interjects grade level parsing based on location.
     */
    private void mockGradeLevelReturns() {
        new NonStrictExpectations() {
            {
                m_studentHistory.getHistoryForStudent(anyString);
                result = m_gradeHistory;
                // Grade level is localized string, just return generics
                m_studentHistory.getNumericGradeLevel(anyString);
                result = new Delegate() {
                    @SuppressWarnings("unused")
                    Integer innerDelegateMethod(String gradeLevel) {
                        Integer value = null;
                        try {
                            value = Integer.valueOf(gradeLevel);
                        } catch (NumberFormatException ex) {
                            value = Integer.valueOf(-1);
                            // this is fine
                        }
                        return value;
                    }
                };
                // Localization issue mocked out
                m_studentHistory.getGradeLevel((Integer) any);
                result = new Delegate() {
                    @SuppressWarnings("unused")
                    String innerDelegateMethod(Integer gradeLevel) {
                        return gradeLevel.toString();
                    }
                };
                m_testStudent.getGradeLevel();
                result = "" + m_studentGradeLevel;
            }
        };

        // System.out.println("Grade: " + m_studentGradeLevel);
    }

    /**
     * Based on the calendar date, sets up the students birth year and grade levels for each year
     * Assumes student is in K at age 6
     * Future grade history is required.
     *
     * @param cal Calendar
     */
    private void buildGradeHistoryBasedOnCalendar(Calendar cal) {
        Calendar calToday = Calendar.getInstance();
        if (m_today != null) {
            PlainDate evaluationDate = new PlainDate(Date.valueOf(m_today));
            calToday.setTime(evaluationDate);
        }
        int birthYear = cal.get(Calendar.YEAR);
        int currentYear = calToday.get(Calendar.YEAR);
        m_gradeHistory = new HashMap<Integer, String>();
        m_schoolYears = new HashSet<DistrictSchoolYearContext>();
        // When student is born, they are in grade -6, through to grade 12 (less than 13)
        for (int grade = -6; grade < 13; grade++) {
            int gradeYear = birthYear + 6 + grade;
            String gradeString;
            if (grade < 10) {
                if (grade < 0) {
                    gradeString = "" + grade;
                } else {
                    gradeString = "0" + grade;
                }
            } else {
                gradeString = "" + grade;
            }
            m_gradeHistory.put(Integer.valueOf(gradeYear), gradeString);
            if (gradeYear <= currentYear) {
                m_studentGradeLevel = Integer.valueOf(grade);
                m_startDate = new PlainDate(Date.valueOf(gradeYear + "-09-01"));
                m_endDate = new PlainDate(Date.valueOf((gradeYear + 1) + "-06-01"));
                m_schoolYears.add(m_schoolYear);
            }
        }
    }

    /**
     * Gets the default student.
     *
     * @return Sis student
     */
    public SisStudent getDefaultStudent() {
        return m_student;
    }

    /**
     * Gets the doses.
     *
     * @return List
     */
    public List<HealthImmunizationDose> getDoses() {
        return m_doses;
    }

    /**
     * Gets the grade history.
     *
     * @return Map
     */
    public Map<Integer, String> getGradeHistory() {
        return m_gradeHistory;
    }

    /**
     * Evaluate default student.
     *
     * @param p_studentInformation String
     * @param passes boolean
     */
    public void evaluateDefaultStudent(String p_studentInformation, boolean passes) {
        studentDOBandDosesString = p_studentInformation;
        evaluateStudentFromMStudentInformation(passes);
    }

    /**
     * Uses studentInfomation variable to build a student and test their compliance. The param
     * passes determines if the rule should pass or fail on that given student.
     * Calling this multiple times within the same test makes the test run time grow exponentially.
     * Try to write a new test for each student.
     * Expects student information to be set before hand.
     *
     * @param passes boolean
     */
    public void evaluateStudentFromMStudentInformation(boolean passes) {
        parseStudentAndDoses();
        evaluateRule(passes);
    }

    /**
     * Uses parsed rule set and default student to evalute if the students information is in
     * compliant of the rule set or not. Also
     * takes the boolean passed in and compares. Fails the test if these do not match and tries to
     * give as much reason as possible.
     *
     * @param passes boolean
     */
    public void evaluateRule(boolean passes) {
        boolean ruleEvaluation = m_completeRuleSet.evaluate(getDefaultStudent(), getDoses());
        if (passes) {
            if (!ruleEvaluation) {
                String errorMessage = m_completeRuleSet.getError();
                if (!StringUtils.isEmpty(errorMessage)) {
                    // Clean up the error message of any html for ease of display and reading
                    errorMessage = StringUtils.replaceAll(errorMessage, "<ul>", "");
                    errorMessage = StringUtils.replaceAll(errorMessage, "<li>", "");
                    errorMessage = StringUtils.replaceAll(errorMessage, "</ul>", "");
                }
                fail("Expected rule to pass, but rule failed! Error list: " + errorMessage);
            }
        } else {
            if (ruleEvaluation) {
                fail("Expected rule to fail, but rule passed! Student information: " + studentDOBandDosesString);
            }
        }
    }
}
