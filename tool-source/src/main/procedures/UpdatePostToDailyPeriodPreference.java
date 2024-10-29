/*
 * ====================================================================
 *
 * X2 Development Corporation, a wholly owned subsidiary of Follett
 * Software Corporation
 *
 * Copyright (c) 2002-2014 Follett Software Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Corporation.
 *
 * ====================================================================
 */

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure is a utility to update the Classroom Input Period to a specified period number
 * based on the day number
 * of the current day on the school calendar. All of the setup is written into this procedure in the
 * detailed SETUP
 * object below.
 *
 * @author Follett Software Company
 */
public class UpdatePostToDailyPeriodPreference extends ProcedureJavaSource {
    /**
     * SETUP contains all data necessary to run this procedure. This is scalable so that the
     * procedure can run
     * effectively for multiple schools.
     *
     * Note - while you can have multiple schools, if the same school is specified twice, the second
     * instance of the school
     * is what will persist because the Post to Daily Attendance period preference exists once per
     * school. This is not meant
     * to run more than once for the same school. The school also must be an active, non-archive
     * school. All data must be
     * based off of the current context active schedule.
     *
     * Example:
     * 
     * private Object[][] SETUP = new Object[][]
     * {
     * {"This Is A School", "Standard Calendar Id", new Object[][] { {"1", "1"},
     * {"2", "1"},
     * {"3", "1"},
     * {"4", "1"},
     * {"5", "1"},
     * {"6", "1"},
     * {"7", "7"} }
     * },
     * {"This Is Another  School", "Student Calendar", new Object[][] { {"Day1", "1"},
     * {"Day2", "1"},
     * {"Day3", "1"},
     * {"Day4", "1"},
     * {"Day5", "1"},
     * {"Day6", "1"},
     * {"Day7", "7"} }
     * }
     * }
     *
     * The example above shows how this could be ran for multiple schools.
     *
     * Explanation of values:
     * - The first parameter is the school name.
     * - The second parameter is the calendar ID that will be used to determine what the current day
     * number is.
     * - The third parameter is an Object[][] that contains a day number Integer to period number
     * Integer value. This tells the procedure
     * what period number value to update the post to daily attendance preference with on what day.
     * If the current day
     * number is not present in this Object Array, then no preference update will take place.
     * 
     * Note - It is important to match all day/period number, school name, and calendar ID values
     * exactly, or they will not be found during the query
     * process. If a necessary object is not found, it may be cause for the preference to not get
     * updated appropriately.
     */
    private Object[][] SETUP = new Object[][] {
            /*
             * SchoolName, CalendarId, Day number to PeriodId map
             */
            {"School 1", "Standard", new Object[][] {{Integer.valueOf(1), Integer.valueOf(1)},
                    {Integer.valueOf(2), Integer.valueOf(1)},
                    {Integer.valueOf(3), Integer.valueOf(1)},
                    {Integer.valueOf(4), Integer.valueOf(1)},
                    {Integer.valueOf(5), Integer.valueOf(2)}}
            },
            {"School 2", "Standard", new Object[][] {{Integer.valueOf(1), Integer.valueOf(1)},
                    {Integer.valueOf(2), Integer.valueOf(1)},
                    {Integer.valueOf(3), Integer.valueOf(1)},
                    {Integer.valueOf(4), Integer.valueOf(1)},
                    {Integer.valueOf(5), Integer.valueOf(3)}}
            }
    };

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        for (Object[] schoolData : SETUP) {
            String schoolName = (String) schoolData[0];
            String calendarId = (String) schoolData[1];

            HashMap<Integer, Integer> dayNumberToPeriodId = new HashMap<Integer, Integer>();
            for (Object[] dayPeriodInfo : (Object[][]) schoolData[2]) {
                dayNumberToPeriodId.put((Integer) dayPeriodInfo[0], (Integer) dayPeriodInfo[1]);
            }

            /*
             * We have now pulled out all the necessary setup values. Next query for each object.
             */
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(School.COL_NAME, schoolName);
            schoolCriteria.addEqualTo(School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
            schoolCriteria.addEqualTo(School.COL_INACTIVE_INDICATOR, Boolean.FALSE);

            QueryByCriteria schoolQuery = new QueryByCriteria(School.class, schoolCriteria);

            School school = (School) getBroker().getBeanByQuery(schoolQuery);

            if (school != null) {
                X2Criteria calendarDateCriteria = new X2Criteria();
                calendarDateCriteria.addEqualTo(
                        SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_CALENDAR_ID,
                        calendarId);
                calendarDateCriteria.addEqualTo(
                        SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                        school.getOid());
                calendarDateCriteria.addEqualTo(
                        SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER
                                + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                        ((SisSchool) school).getActiveSchedule().getDistrictContextOid());
                calendarDateCriteria.addEqualTo(SchoolCalendarDate.COL_DATE, new PlainDate());

                QueryByCriteria calendarDateQuery = new QueryByCriteria(SchoolCalendarDate.class, calendarDateCriteria);

                SchoolCalendarDate calendarDate = (SchoolCalendarDate) getBroker().getBeanByQuery(calendarDateQuery);

                if (calendarDate != null) {
                    /*
                     * Now that we have the SchoolCalendarDate day number for today, we need to
                     * check the post to daily
                     * period preference value and confirm it matches the SETUP above. If not, we
                     * need to update the preference.
                     */
                    int dayNumber = calendarDate.getScheduleDayNumber();

                    String currentPeriodNumberPrefVal =
                            PreferenceManager.getPreferenceValue(school, SisPreferenceConstants.ATT_INPUT_PERIOD);

                    Integer specifiedPeriodId = dayNumberToPeriodId.get(Integer.valueOf(dayNumber));

                    if (specifiedPeriodId != null) {
                        if (specifiedPeriodId.intValue() != Integer.valueOf(currentPeriodNumberPrefVal).intValue()) {
                            /*
                             * The input period number for this calendar day number does not match
                             * the saved preference value.
                             * Update the preference.
                             */
                            List<ValidationError> errors = PreferenceManager.setPreferenceValue(school,
                                    getBroker(),
                                    SisPreferenceConstants.ATT_INPUT_PERIOD,
                                    specifiedPeriodId.toString());

                            if (errors.isEmpty()) {
                                logMessage("Updated Attendance Input Period to be " + specifiedPeriodId
                                        + " from the previous value"
                                        + " of " + currentPeriodNumberPrefVal + " for " + schoolName + ".");
                            } else {
                                logMessage("Failed to update Attendance Input Period preference value for school "
                                        + schoolName + ".");
                                for (ValidationError error : errors) {
                                    logMessage(error.toString());
                                }
                            }

                            logMessage("\r\n");
                        }
                    } else {
                        logMessage("Unable to find periodId for the current calendar day number in the setup. \r\n");
                    }
                } else {
                    logMessage("Unable to find calendar date for today with calendar Id " + calendarId + ".\r\n");
                }
            } else {
                logMessage("Unable to find matching school with name " + schoolName + ".\r\n");
            }
        }
    }
}
