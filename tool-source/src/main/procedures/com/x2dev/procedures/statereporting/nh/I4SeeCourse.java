/*
 * ==================================================================== X2 Development Corporation
 * Copyright (c) 2002-2009 X2 Development Corporation. All rights reserved. Redistribution and use
 * in source and binary forms, with or without modification, is not permitted without express
 * written agreement from X2 Development Corporation.
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.PreferenceSet;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote><font color="green" size="4"> i4see CATE Course export.</font></blockquote>
 *
 * @author X2 Development Corporation
 * @since v2.11a
 */
public class I4SeeCourse extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            MasterSchedule masterSchedule = (MasterSchedule) getBean();
            String name = masterSchedule.getCourseView() + " [Desc: "
                    + masterSchedule.getDescription() + ", Section: "
                    + masterSchedule.getSectionNumber() + ", Term: " + masterSchedule.getTermView()
                    + "]";

            return name;
        }

        /**
         * Check the Course record to see if it should be excluded from the report .
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            String javaName = getData().translateAliasToJavaName(I4SEE_COURSE_EXCLUDE, false);
            String excludeFlag;
            try {
                excludeFlag = (String) WebUtils.getProperty(getBean(), MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                        + SchoolCourse.REL_COURSE + PATH_DELIMITER + javaName);

                if (excludeFlag != null) {
                    // check enrollment count and membership days parameter.
                    if ("1".equals(excludeFlag)) {
                        // Course filtered.
                        error = new StateReportValidationError(this.toString(),
                                ((MasterSchedule) getBean()).getDescription(), " - excluded from export", "");
                    } else {
                        // No filtering.
                    }
                }
            } catch (X2BaseException e) {
                e.printStackTrace();
            }

            return error;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            setRowCount(1);
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * The Class Retriever1400BeginDate.
     */
    protected class Retriever1400BeginDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            PlainDate startDate = null;
            Collection<ScheduleTermDate> scheduleTermDates =
                    (Collection<ScheduleTermDate>) WebUtils.getProperty(entity.getBean(),
                            MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES);
            if (scheduleTermDates != null) {
                for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                    if (startDate == null || scheduleTermDate.getStartDate().before(startDate)) {
                        startDate = scheduleTermDate.getStartDate();
                    }
                }
            } else {
                /*
                 * No schedule terms defined, use the schedule start and end dates
                 */
                startDate = (PlainDate) WebUtils.getProperty(entity.getBean(),
                        MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_START_DATE);
            }
            return startDate;
        }
    }

    /**
     * The Class Retriever1410EndDate.
     */
    protected class Retriever1410EndDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Collection<ScheduleTermDate> scheduleTermDates =
                    (Collection<ScheduleTermDate>) WebUtils.getProperty(entity.getBean(),
                            MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES);
            PlainDate endDate = null;
            if (scheduleTermDates != null) {
                for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                    if (endDate == null || scheduleTermDate.getEndDate().after(endDate)) {
                        endDate = scheduleTermDate.getEndDate();
                    }
                }
            } else {
                /*
                 * No schedule terms defined, use the schedule start and end dates
                 */
                endDate = (PlainDate) WebUtils.getProperty(entity.getBean(),
                        MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_END_DATE);
            }

            return endDate;
        }
    }

    /**
     * The Class Retrieve1450PrimaryGradeId.
     */
    /*
     * Returns the state code for the primary grade
     *
     * @throws X2BaseException
     */
    protected class Retrieve1450PrimaryGradeId implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String gradeId = null;
            String javaName = translateAliasToJavaName(I4SEE_1450_PRI_GRADE_ID, true);
            gradeId = (String) WebUtils.getProperty(entity.getBean(), MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                    + SchoolCourse.REL_COURSE + PATH_DELIMITER + javaName);

            gradeId = getStateCode(field.getBeanPath(), gradeId);

            return gradeId;
        }

    }

    /**
     * Returns the sau number for the given student.
     */
    protected class Retrieve030SauNumber implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public Retrieve030SauNumber(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String sauNumber = (String) WebUtils.getProperty(getOrganization(), m_javaName);
            return sauNumber;
        }
    }

    /**
     * Returns the district number for the given student.
     */
    protected class Retrieve040DistrictNumber implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public Retrieve040DistrictNumber(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String districtNumber = (String) WebUtils.getProperty(getOrganization(), m_javaName);
            return districtNumber;
        }
    }

    /**
     * Retrieve's receiving school number from input parameters.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve050SchoolNumber implements FieldRetriever {
        private String m_javaName = null;

        /**
         * Constructor.
         *
         * @param javaName String
         */
        public Retrieve050SchoolNumber(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String schoolNumber = (String) WebUtils.getProperty(entity.getBean(), MasterSchedule.REL_SCHOOL_COURSE
                    + PATH_DELIMITER + SchoolCourse.REL_SCHOOL + PATH_DELIMITER + m_javaName);
            return schoolNumber;
            // return getParameter("receivingSchoolNumber");
        }
    }

    /**
     * Returns a educator's certification number, if one exists .
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1200EducatorId implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            Integer index = (Integer) field.getParameter();
            MasterSchedule masterSchedule = (MasterSchedule) entity.getBean();
            LinkedList<StaffCertification> staffCertifications = m_educatorIds.get(masterSchedule.getPrimaryStaffOid());
            if (staffCertifications != null && staffCertifications.size() >= index.intValue()) {
                value = staffCertifications.get(index.intValue() - 1).getCertificationNumber();
            }
            return value;
        }

    }

    /**
     * Converts TRM_BASE_TERM_COVER_MAP to TermID
     * <p>
     * CATE Valid Codes are:
     * <ul>
     * <li>1 - 1st Semester
     * <li>2 - 2nd Semester
     * <li>30 - Full Year
     * </ul>
     * .
     *
     * @author X2 Development Corporation
     */
    // protected class Retrieve1430TermId implements FieldRetriever
    // {
    // /**
    // * @see
    // com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
    // * com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
    // * com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
    // */
    // public Object getFieldValue(StateReportData data, StateReportEntity entity,
    // FieldDefinition field) throws X2BaseException
    // {
    // String fieldValue = "";
    //
    // /*
    // * Convert the TRM_BASE_TERM_COVER_MAP to one of the valid codes. The map is a string of
    // * 1's and 0's. A single digit indicates FULL YEAR. Multiple characters indicate the
    // * number of terms and whether the class is in session or not for those terms.
    // */
    // String termMap = (String) WebUtils.getProperty(entity.getBean(),
    // MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_BASE_TERM_MAP);
    // Integer termsPerYear = (Integer) WebUtils.getProperty(entity.getBean(),
    // MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_BASE_TERMS_PER_YEAR);
    //
    // try
    // {
    // /*
    // * Ensure the termMap is the correct length. This is done at the bean level, so this
    // * should never happen
    // */
    // if (termMap == null || termMap.length() != termsPerYear)
    // {
    // throw new Exception();
    // }
    //
    // /*
    // * Not knowing how to handle tri-mesters, we'll assume that 1) the CATE schools will
    // * not have them, or 2) if they do, then we'll just take the 1st and last character
    // * from the map to see which 'semester' is in session, essentially ignoring the
    // * middle term. This is true for 'quarters' as well.
    // */
    // if (!termMap.contains("0"))
    // {
    // fieldValue = "30"; // full year
    // }
    // else if (termMap.startsWith("1"))
    // {
    // fieldValue = "1";
    // }
    // else if (termMap.endsWith("1"))
    // {
    // fieldValue = "2";
    // }
    // }
    // catch (Exception e)
    // {
    // entity.addRetrievalError(field.getFieldId(), new StateReportValidationError(entity,
    // field, "ERROR", VALIDATION_INVALID_VALUE));
    // }
    //
    // return fieldValue;
    // }
    //
    // }

    /**
     * Retrieves the local class code and reduces it to MAX characters, if possible, by
     * removing leading zeros and non-numeric characters (e.g. periods, hypens, etc.)
     *
     * - 1st leading zeros are removed (e.g. '0000123-0023' == '00123-0023')
     * - 2nd duplicate hyphens are removed (e.g. '1234567-A-1' == '1234567A-1')
     * - 3rd leading zeros after a hyphen are removed (e.g. '1234567-0023' == '1234567-23')
     * - 4th periods are removed (e.g. '1234567.A-1' == '1234567A-1')
     * - Last resort ... the course ID needs to be changed.
     */
    protected class Retrieve1470LocalClassCode implements FieldRetriever {
        String m_javaName;

        /**
         * Instantiates a new retrieve 1470 local class code.
         *
         * @param javaName String
         */
        Retrieve1470LocalClassCode(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            MasterSchedule masterSchedule = (MasterSchedule) entity.getBean();
            String courseId = (String) WebUtils.getProperty(masterSchedule, m_javaName);

            /*
             * A bit of a kludge....
             * Go to the School Course record and see if there's an alias called 'i4see 1470
             * prefix'.
             * If there is, and the value is not empty, then prepend the courseID with that value.
             * This is to differentiate identical courses offered at more than one school. (E.g.
             * Nashua NH offers the same course at both North and South High Schools)
             */
            if (!StringUtils.isEmpty(m_I4see1470Prefix)) {
                try {
                    SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                    String value = (String) WebUtils.getProperty(schoolCourse, m_I4see1470Prefix);
                    if (!StringUtils.isEmpty(value)) {
                        courseId = value + courseId;
                    }
                } catch (Exception e) {
                    // ignore errors
                }
            }

            /*
             * Remove any leading zeros
             */
            while (courseId.length() > field.getMaxLength() && courseId.charAt(0) == '0') {
                courseId = courseId.substring(1);
            }

            /*
             * Remove duplicate hyphens
             */
            while (courseId.length() > field.getMaxLength() && courseId.matches("([^-]*-[^-]*)*-[^-]*")) {
                courseId = courseId.replaceFirst("-", "");
            }

            /*
             * Removed zeros after a hyphen
             */
            while (courseId.length() > field.getMaxLength() && courseId.contains("-0")) {
                courseId = courseId.replace("-0", "-");
            }

            /*
             * Removed periods
             */
            while (courseId.length() > field.getMaxLength() && courseId.contains(".")) {
                courseId = courseId.replace(".", "");
            }

            return courseId;
        }

    }

    /**
     * Returns the number of class meetings per semester. The FieldDefinition must be defined with
     * the userParameter set to "1" or "2", specifying which semester to calculate. Also the Middle
     * Of Year date must be provided and between the start and end dates of the course.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1780NumMeetingSemstr implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String semesterID = (String) field.getParameter(); // "1" or "2"
            Set<PlainDate> inSessionDates = new HashSet<PlainDate>();

            PlainDate beginDate = null;
            PlainDate endDate = null;
            PlainDate middleOfYearDate = (PlainDate) getParameter(MOY_DATE_PARAM);
            String beginDateString = entity.getFieldValue(I4SEE_1400_BEGIN_DATE);
            String endDateString = entity.getFieldValue(I4SEE_1410_END_DATE);
            try {
                beginDate = new PlainDate(m_dateFormat.parse(beginDateString));
                endDate = new PlainDate(m_dateFormat.parse(endDateString));

                String schoolOid = (String) WebUtils.getProperty(entity.getBean(),
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID);
                SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                if ("1".equals(semesterID)) {
                    if (middleOfYearDate.after(endDate)) {
                        middleOfYearDate = endDate;
                    }

                    inSessionDates = getInSessionDays(school, beginDate, DateUtils.add(middleOfYearDate, -1));
                } else {
                    if (middleOfYearDate.before(beginDate)) {
                        middleOfYearDate = beginDate;
                    }

                    inSessionDates = getInSessionDays(school, middleOfYearDate, endDate);
                }
            } catch (ParseException e) {
                StateReportValidationError error = new StateReportValidationError(entity, field, INVALID_DATES,
                        "Schedule Term not defined for course?");
                entity.addRetrievalError(I4SEE_1780_NUM_MEETING_SEMSTR_ONE, error);
            }

            return Integer.valueOf(inSessionDates.size() > 10 ? 90 : inSessionDates.size());
        }
    }

    /*
     * Alias constants
     * 1600 SAUNbrReceive Required Receiving School Administrative Unit Number
     * 1610 DistNbrReceive Required Receiving District Number
     * 1620 SchoolNbrReceive Required Receiving School Number
     * 1200 EducatorId Required Teacher Certification Id
     * 1201 EducatorId2 BlanksAllowed(if NA) Teacher Certification Id 2
     * 1202 EducatorId3 BlanksAllowed(if NA) Teacher Certification Id 3
     * 1740 ProgramID Required Program ID
     * 1400 BeginDate Required Begin date of course
     * 1410 EndDate Required End date of course
     * 1420 SchoolYear Required School Year
     * 1430 TermId Required Term Id
     * 1440 Credits Required Credits
     * 1460 RoomNbr BlanksAllowed(if NA) Room Number
     * 1470 LocalClassCode Required Local Class Code
     * 1480 LocalClassName Required Local Class Name
     * 1770 LengthInMinutes Required Length In Minutes
     * 1780 NumMeetingSemstrOne BlanksAllowed(if NA) Number Meeting Semester One
     * 1790 NumMeetingSemstrTwo BlanksAllowed(if NA) Number Meetings Semester Two
     * 1300 CourseId BlanksAllowed(if NA) CourseId
     */
    private static final String I4SEE_030_SAU_NUMBER = "i4see 030";
    private static final String I4SEE_040_DISTRICT_NUMBER = "i4see 040";

    private static final String I4SEE_050_SCHOOL_NUMBER = "i4see 050";
    private static final String I4SEE_1600_SAU_NBR_RECEIVE = "i4see 1600";
    private static final String I4SEE_1610_DIST_NBR_RECEIVE = "i4see 1610";

    private static final String I4SEE_1620_SCHOOL_NBR_RECEIVE = "i4see 1620";
    private static final String I4SEE_1200_EDUCATOR_ID = "i4see 1200";
    private static final String I4SEE_1201_EDUCATOR_ID2 = "i4see 1201";
    private static final String I4SEE_1202_EDUCATOR_ID3 = "i4see 1202";
    private static final String I4SEE_1740_PROGRAM_ID = "i4see 1740";
    private static final String I4SEE_1400_BEGIN_DATE = "i4see 1400";
    private static final String I4SEE_1410_END_DATE = "i4see 1410";
    private static final String I4SEE_1420_SCHOOL_YEAR = "i4see 1420";
    private static final String I4SEE_1430_TERM_ID = "i4see 1430";
    private static final String I4SEE_1440_CREDITS = "i4see 1440";
    private static final String I4SEE_1450_PRI_GRADE_ID = "i4see 1450";
    private static final String I4SEE_1460_ROOM_NBR = "i4see 1460";
    private static final String I4SEE_1470_LOCAL_CLASS_CODE = "i4see 1470";
    private static final String I4SEE_1470_PREFIX = "i4see 1470 prefix";
    private static final String I4SEE_1480_LOCAL_CLASS_NAME = "i4see 1480";
    private static final String I4SEE_1770_LENGTH_IN_MINUTES = "i4see 1770";
    private static final String I4SEE_1780_NUM_MEETING_SEMSTR_ONE = "i4see 1780";
    private static final String I4SEE_1790_NUM_MEETING_SEMSTR_TWO = "i4see 1790";
    private static final String I4SEE_1300_COURSE_ID = "i4see 1300";
    private static final String I4SEE_1305_SUBJECT_CODE = "i4see 1305";
    private static final String I4SEE_1320_SECTION_ID = "i4see 1320";
    private static final String I4SEE_COURSE_EXCLUDE = "i4see Course Exclude";

    /*
     * Field aliases for "adjusted" fields (these fields may have manually entered values that
     * override/adjust calculated values). Note: The alias for the adjusted status is optional (is
     * does not have to be defined in the Data Dictionary).
     */
    private static final String ADJUSTED_SCHOOL_NUMBER_FIELD = "i4see Adj School Number";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /**
     * Hash map capacity multiplier
     */
    private final static double HASHMAP_CAPACITY_MULTIPLIER = 1.5;

    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";

    /**
     * Name for the middle of year date parameter. The corresponding value is a PlainDate object.
     */
    public static final String MOY_DATE_PARAM = "moyDate";

    /*
     * Other internal constants
     */
    private static final String REGEX_NUMERIC = "[0-9]*";
    // private static final String VALIDATION_INVALID_VALUE = "Invalid value";
    private static final String INVALID_DATES = "Invalid date";

    /**
     * Name for the report type parameter. The corresponding value is an integer.
     */
    public static final String REPORT_TYPE = "reportType";
    /**
     * Value for REPORT_TYPE parameter indicating Beginning of Year report
     */
    public static final Integer REPORT_TYPE_NONCATE = Integer.valueOf(0);
    /**
     * Value for REPORT_TYPE parameter indicating End of Year report
     */
    public static final Integer REPORT_TYPE_CATE = Integer.valueOf(0);

    /**
     * bit values to <code>OR</code> together to index the <code>NH_STATE_RACE_CODES</code> table
     */
    private static final int WHITE_RACE_IND = 1;
    private static final int BLACK_RACE_IND = 2;
    private static final int ASIAN_RACE_IND = 4;
    private static final int AMERICAN_INDIAN_RACE_IND = 8;
    private static final int PACIFIC_ISLANDER_RACE_IND = 16;
    private static final int HISPANIC_RACE_IND = 32;

    /**
     * Map of NH Race Reference codes to a bit-value for mapping into the NH_STATE_RACE_CODES table
     * below. The Hispanic indicator is a remnant from when it was considered a race. Having a
     * record with the hispanic race code is the same has having the HispanicIndicator on the PERSON
     * record set to true.
     */
    static final int[] NH_TO_X2_RACE_CODES = new int[] {0, // not
                                                           // used
            AMERICAN_INDIAN_RACE_IND, // 1 - American Indian
            ASIAN_RACE_IND, // 2 - Asian
            HISPANIC_RACE_IND, // 3 - Hispanic
            BLACK_RACE_IND, // 4 - Black
            WHITE_RACE_IND, // 5 - White
            PACIFIC_ISLANDER_RACE_IND, // 6 - Pacific Islander
            0, // not used
            0, // not used
            0 // not used (nor is it
              // valid)
    };

    /**
     * The DOE uses an arbitrary code for its combinations of races/ethnicities. X2 instead uses a
     * bitmap with the following values:
     *
     * <pre>
     * - WHITE: 1
     * - BLACK: 2
     * - ASIAN: 4
     * - AMERICAN INDIAN: 8
     * - PACIFIC ISLANDER: 16
     * - HISPANIC: 32   NOTE:  this is not a race, but an attribute to a race
     * </pre>
     *
     * The X2 bitmap is translated to a state code using the following array. (NH values from
     * http://www.ed.state.nh.us/education/doe/racecodes.asp)
     */
    protected static final int[] NH_STATE_RACE_CODES = new int[] {0, // X2
                                                                     // value
                                                                     // Hispanic
                                                                     // Hawaiian
                                                                     // American
                                                                     // Asian
                                                                     // Black
                                                                     // White
            // Pacific Indian/Alaska
            5, // 1 x
            4, // 2 x
            15, // 3 x x
            2, // 4 x
            13, // 5 x x
            12, // 6 x x
            15, // 7 x x x
            1, // 8 x
            10, // 9 x x
            8, // 10 x x
            21, // 11 x x x
            7, // 12 x x
            19, // 13 x x x
            18, // 14 x x x
            28, // 15 x x x x
            6, // 16 x
            17, // 17 x x
            16, // 18 x x
            27, // 19 x x x
            14, // 20 x x
            29, // 21 x x x
            31, // 22 x x x
            32, // 23 x x x x
            11, // 24 x x
            23, // 25 x x x
            22, // 26 x x x
            30, // 27 x x x x
            20, // 28 x x x
            29, // 29 x x x x
            31, // 30 x x x x
            33, // 31 x x x x x
            3, // 32 x
            3, // 33 x x
            36, // 34 x x
            45, // 35 x x x
            35, // 36 x x
            43, // 37 x x x
            42, // 38 x x x
            45, // 39 x x x x
            34, // 40 x x
            40, // 41 x x x
            39, // 42 x x x
            51, // 43 x x x x
            38, // 44 x x x
            49, // 45 x x x x
            48, // 46 x x x x
            28, // 47 x x x x x
            58, // 48 x x
            47, // 49 x x x
            46, // 50 x x x
            57, // 51 x x x x
            44, // 52 x x x
            59, // 53 x x x x
            61, // 54 x x x x
            62, // 55 x x x x x
            41, // 56 x x x
            53, // 57 x x x x
            52, // 58 x x x x
            60, // 59 x x x x x
            50, // 60 x x x x
            59, // 61 x x x x x
            61, // 62 x x x x x
            63}; // 63
                 // x
                 // x
                 // x
                 // x
                 // x
                 // x

    /**
     * Supporting instance variables. These are protected rather than private so they can be
     * accessed by the inner classes.
     */
    protected Map<String, Integer> m_absences;

    protected String m_adjustedSchoolCode;

    protected boolean m_calculateTotals;

    protected SimpleDateFormat m_dateFormat;

    protected EnrollmentManager m_enrollmentManager;

    protected PlainDate m_firstDayDate;

    protected Set m_firstDayMembers;

    protected TreeMap m_gradeLevelMap;

    protected String m_I4see1470Prefix;

    protected boolean m_includeStudentNames;

    protected Converter m_integerConverter;

    protected Map<String, Collection<Race>> m_raceCodeMap;

    private HashMap<String, Map<String, String>> m_fieldToRefTable;

    protected String m_reportStatusField;

    protected Integer m_reportType;

    protected Set<String> m_activeScheduleOids;

    protected Map<String, SisSchool> m_schoolMap;

    protected Collection m_suspensionInCodes;

    protected Collection m_suspensionOutCodes;

    protected Map<String, LinkedList<StaffCertification>> m_educatorIds;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return MasterSchedule.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "I4SEE CATE Course";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * The Receiving School Administrative Unit Number is the same as the School Administrative Unit
     * Number (i4see 030).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see030_SchoolAdminUnitNumber() {
        String javaName = translateAliasToJavaName(I4SEE_030_SAU_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_030_SAU_NUMBER,
                LABEL_PREFIX_CHAR + I4SEE_030_SAU_NUMBER + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve030SauNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * The the School District Number (i4see 040).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see040_SchoolDistrictNumber() {
        String javaName = translateAliasToJavaName(I4SEE_040_DISTRICT_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_040_DISTRICT_NUMBER,
                LABEL_PREFIX_CHAR + I4SEE_040_DISTRICT_NUMBER + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve040DistrictNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * Gets the i 4 see 050 school number.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see050_SchoolNumber() {
        String javaName = translateAliasToJavaName(I4SEE_050_SCHOOL_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_050_SCHOOL_NUMBER,
                LABEL_PREFIX_CHAR + I4SEE_050_SCHOOL_NUMBER + PATH_DELIMITER + "from xml input definition.",
                null,
                false,
                5,
                5,
                REGEX_NUMERIC,
                null,
                new Retrieve050SchoolNumber(javaName),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1200 educator id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1200_EducatorId() {
        String javaName = translateAliasToJavaName(I4SEE_1200_EDUCATOR_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1200_EDUCATOR_ID,
                MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                20,
                null,
                null,
                new Retrieve1200EducatorId(),
                null,
                Integer.valueOf(1));
        return field;
    }

    /**
     * Gets the i 4 see 1201 educator id 2.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1201_EducatorId2() {
        String javaName = translateAliasToJavaName(I4SEE_1201_EDUCATOR_ID2, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1201_EDUCATOR_ID2,
                MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                20,
                null,
                null,
                new Retrieve1200EducatorId(),
                null,
                Integer.valueOf(2));
        return field;
    }

    /**
     * Gets the i 4 see 1202 educator id 3.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1202_EducatorId3() {
        String javaName = translateAliasToJavaName(I4SEE_1202_EDUCATOR_ID3, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1202_EDUCATOR_ID3,
                MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                10,
                null,
                null,
                new Retrieve1200EducatorId(),
                null,
                Integer.valueOf(3));
        return field;
    }

    /**
     * Gets the i 4 see 1300 course id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1300_CourseId() {
        String javaName = translateAliasToJavaName(I4SEE_1300_COURSE_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1300_COURSE_ID,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                10,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1305 subject code.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1305_SubjectCode() {
        String javaName = translateAliasToJavaName(I4SEE_1305_SUBJECT_CODE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1305_SUBJECT_CODE,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                10,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1320 section id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1320_SectionId() {
        String javaName = translateAliasToJavaName(I4SEE_1320_SECTION_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1320_SECTION_ID,
                javaName,
                null,
                false,
                1,
                10,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1400 begin date.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1400_BeginDate() {
        String javaName = translateAliasToJavaName(I4SEE_1400_BEGIN_DATE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1400_BEGIN_DATE,
                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES
                        + PATH_DELIMITER + javaName,
                null,
                false,
                8,
                10,
                null,
                m_dateFormat,
                new Retriever1400BeginDate(),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1410 end date.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1410_EndDate() {
        String javaName = translateAliasToJavaName(I4SEE_1410_END_DATE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1410_END_DATE,
                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE_TERM_DATES
                        + PATH_DELIMITER + javaName,
                null,
                false,
                8,
                10,
                null,
                m_dateFormat,
                new Retriever1410EndDate(),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1420 school year.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1420_SchoolYear() {
        String javaName = translateAliasToJavaName(I4SEE_1420_SCHOOL_YEAR, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1420_SCHOOL_YEAR,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_SCHOOL
                        + PATH_DELIMITER + SisSchool.REL_ORGANIZATION1 + PATH_DELIMITER
                        + SisOrganization.REL_CURRENT_CONTEXT + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                4,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1430 term id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1430_TermId() {
        String javaName = translateAliasToJavaName(I4SEE_1430_TERM_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1430_TERM_ID,
                MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER + javaName,
                null,
                true,
                1,
                2,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1440 credits.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1440_Credits() {
        String javaName = translateAliasToJavaName(I4SEE_1440_CREDITS, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1440_CREDITS,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                5,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1450 primary graded id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1450_PrimaryGradedId() {
        String javaName = translateAliasToJavaName(I4SEE_1450_PRI_GRADE_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1450_PRI_GRADE_ID,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + javaName,
                null,
                true,
                1,
                5,
                REGEX_NUMERIC,
                null,
                new Retrieve1450PrimaryGradeId(),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1460 room nbr.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1460_RoomNbr() {
        String javaName = translateAliasToJavaName(I4SEE_1460_ROOM_NBR, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1460_ROOM_NBR,
                MasterSchedule.REL_PRIMARY_ROOM + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                10,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1470 local class code.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1470_LocalClassCode() {
        String javaName = translateAliasToJavaName(I4SEE_1470_LOCAL_CLASS_CODE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1470_LOCAL_CLASS_CODE,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                10,
                null,
                null,
                new Retrieve1470LocalClassCode(javaName),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1480 local class name.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1480_LocalClassName() {
        String javaName = translateAliasToJavaName(I4SEE_1480_LOCAL_CLASS_NAME, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1480_LOCAL_CLASS_NAME,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                50,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * The Receiving School Administrative Unit Number is the same as the School Administrative Unit
     * Number (i4see 1600). Note: data is the same as i4see 030
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1600_ReceivingSchoolAdminUnitNumber() {
        String javaName = translateAliasToJavaName(I4SEE_030_SAU_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_030_SAU_NUMBER,
                LABEL_PREFIX_CHAR + I4SEE_1600_SAU_NBR_RECEIVE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve030SauNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * The Receiving School District Number is the same as the School District Number (i4see 040).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1610_ReceivingSchoolDistrictNumber() {
        String javaName = translateAliasToJavaName(I4SEE_040_DISTRICT_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_040_DISTRICT_NUMBER,
                LABEL_PREFIX_CHAR + I4SEE_1610_DIST_NBR_RECEIVE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve040DistrictNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * Gets the i 4 see 1620 receiving school number.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1620_ReceivingSchoolNumber() {
        FieldDefinition field = new FieldDefinition(I4SEE_1620_SCHOOL_NBR_RECEIVE,
                LABEL_PREFIX_CHAR + I4SEE_1620_SCHOOL_NBR_RECEIVE + ".from xml input definition",
                (String) getParameter("receivingSchoolNumber"),
                false,
                5,
                5,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1740 program id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1740_ProgramId() {
        String javaName = translateAliasToJavaName(I4SEE_1740_PROGRAM_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1740_PROGRAM_ID,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                0,
                10,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1770 length in minutes.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1770_LengthInMinutes() {
        String javaName = translateAliasToJavaName(I4SEE_1770_LENGTH_IN_MINUTES, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1770_LENGTH_IN_MINUTES,
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                3,
                null,
                null,
                null, // new Retrieve1770LengthInMinutes(),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1780 num meeting semstr one.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1780_NumMeetingSemstrOne() {
        String javaName = translateAliasToJavaName(I4SEE_1780_NUM_MEETING_SEMSTR_ONE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1780_NUM_MEETING_SEMSTR_ONE,
                javaName,
                null,
                false,
                0,
                3,
                null,
                null,
                new Retrieve1780NumMeetingSemstr(),
                null,
                "1");
        return field;
    }

    /**
     * Gets the i 4 see 1790 num meeting semstr two.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1790_NumMeetingSemstrTwo() {
        String javaName = translateAliasToJavaName(I4SEE_1790_NUM_MEETING_SEMSTR_TWO, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1790_NUM_MEETING_SEMSTR_TWO,
                javaName,
                null,
                false,
                0,
                3,
                null,
                null,
                new Retrieve1780NumMeetingSemstr(),
                null,
                "2");
        return field;
    }

    /**
     * Initialize the data module. Initialize necessary working resources. Define query for students
     * to load. Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * Load state codes for reference tables
         */
        String javaNamePrimaryGradeId = translateAliasToJavaName(I4SEE_1450_PRI_GRADE_ID, true);
        String[] fields = {MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + javaNamePrimaryGradeId
        };
        loadStateReferenceCodes(fields);

        /*
         * Get core parameters
         */
        m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        m_reportType = (Integer) getParameter(REPORT_TYPE);

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_integerConverter = ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER,
                Locale.getDefault(), true);

        /*
         * Create the export Field Definitions
         */
        createFieldDefinitions();

        /*
         * Load Schools
         */
        loadSchools();

        /*
         * Load teacher certification
         */
        loadEducatorIds();

        /*
         * Load Active Schedules
         */
        loadActiveSchedules();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria masterScheduleCriteria = getMasterScheduleCriteria();
            QueryByCriteria masterScheduleQuery = new QueryByCriteria(MasterSchedule.class,
                    masterScheduleCriteria);

            masterScheduleQuery.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);
            masterScheduleQuery.addOrderByAscending(MasterSchedule.COL_SECTION_NUMBER);
            masterScheduleQuery.addOrderByAscending(MasterSchedule.COL_TERM_VIEW);

            // Set the query to be used for student selection.
            setQuery(masterScheduleQuery);
            setEntityClass(I4SeeEntity.class);
        }
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param beginDate PlainDate
     * @param endDate PlainDate
     * @return Set of PlainDate objects
     */
    protected Set<PlainDate> getInSessionDays(SisSchool school, PlainDate beginDate, PlainDate endDate) {
        Set<PlainDate> inSessionDates = new HashSet<PlainDate>();

        Map<String, Set<PlainDate>> calendarData = m_enrollmentManager.getCalendarLookup(school,
                beginDate, endDate);

        for (String calendarID : calendarData.keySet()) {
            inSessionDates.addAll(calendarData.get(calendarID));
        }

        return inSessionDates;
    }

    /**
     * Create and set the field definitions for the export.
     *
     * @return void
     */
    private void createFieldDefinitions() {
        /*
         * Set the field definition array
         */
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(
                m_includeStudentNames ? 26 : 25);

        if (m_reportType.equals(REPORT_TYPE_CATE)) // CATE Course
        {
            fieldDefinitions.add(getI4see1600_ReceivingSchoolAdminUnitNumber());
            fieldDefinitions.add(getI4see1610_ReceivingSchoolDistrictNumber());
            fieldDefinitions.add(getI4see1620_ReceivingSchoolNumber());
            fieldDefinitions.add(getI4See1200_EducatorId());
            fieldDefinitions.add(getI4See1201_EducatorId2());
            fieldDefinitions.add(getI4See1202_EducatorId3());
            fieldDefinitions.add(getI4See1740_ProgramId());
            fieldDefinitions.add(getI4See1400_BeginDate());
            fieldDefinitions.add(getI4See1410_EndDate());
            fieldDefinitions.add(getI4See1420_SchoolYear());
            fieldDefinitions.add(getI4See1430_TermId());
            fieldDefinitions.add(getI4See1440_Credits());
            fieldDefinitions.add(getI4See1460_RoomNbr());
            fieldDefinitions.add(getI4See1470_LocalClassCode());
            fieldDefinitions.add(getI4See1480_LocalClassName());
            fieldDefinitions.add(getI4See1770_LengthInMinutes());
            fieldDefinitions.add(getI4See1780_NumMeetingSemstrOne());
            fieldDefinitions.add(getI4See1790_NumMeetingSemstrTwo());
            fieldDefinitions.add(getI4See1300_CourseId());
        } else // Submission Course
        {
            fieldDefinitions.add(getI4see030_SchoolAdminUnitNumber());
            fieldDefinitions.add(getI4see040_SchoolDistrictNumber());
            fieldDefinitions.add(getI4see050_SchoolNumber());
            fieldDefinitions.add(getI4See1200_EducatorId());
            fieldDefinitions.add(getI4See1305_SubjectCode());
            fieldDefinitions.add(getI4See1320_SectionId());
            fieldDefinitions.add(getI4See1400_BeginDate());
            fieldDefinitions.add(getI4See1410_EndDate());
            fieldDefinitions.add(getI4See1420_SchoolYear());
            fieldDefinitions.add(getI4See1430_TermId());
            fieldDefinitions.add(getI4See1440_Credits());
            fieldDefinitions.add(getI4See1450_PrimaryGradedId());
            fieldDefinitions.add(getI4See1460_RoomNbr());
            fieldDefinitions.add(getI4See1470_LocalClassCode());
            fieldDefinitions.add(getI4See1480_LocalClassName());
        }

        setFieldDefinitions(fieldDefinitions);
    }

    /**
     * Returns the criteria that retrieves all Master Schedule records that should be included in
     * the export.
     *
     * @return Criteria
     */
    private Criteria getMasterScheduleCriteria() {
        /*
         * Get everything from the master schedule that is in the school's active schedules
         */
        X2Criteria masterScheduleCriteria = new X2Criteria();

        String javaName = translateAliasToJavaName(I4SEE_1740_PROGRAM_ID, true);
        if (m_reportType.equals(REPORT_TYPE_CATE)) {
            /*
             * Limit the criteria to items that contain a CIP (i4see 1740)
             */
            masterScheduleCriteria.addNotNull(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    javaName);
        } else {
            /*
             * Limit the criteria to items that do NOT contain a CIP (i4see 1740)
             */
            masterScheduleCriteria.addIsNull(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    javaName);
            X2Criteria emptyCriteria = new X2Criteria();
            emptyCriteria.addEmpty(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    javaName, getBroker().getPersistenceKey());
            masterScheduleCriteria.addOrCriteria(emptyCriteria);
        }
        masterScheduleCriteria.addIn(MasterSchedule.COL_SCHEDULE_OID, m_activeScheduleOids);

        if (isSchoolContext()) {
            masterScheduleCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER
                    + Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, getSchool().getOid());

        } else {
            masterScheduleCriteria.addEqualTo(
                    MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL
                            + ModelProperty.PATH_DELIMITER + SisSchool.COL_ORGANIZATION1_OID,
                    getOrganization().getOid());
            masterScheduleCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER
                    + Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            masterScheduleCriteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER
                    + Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        return masterScheduleCriteria;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_adjustedSchoolCode = translateAliasToJavaName(ADJUSTED_SCHOOL_NUMBER_FIELD, true);
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        m_firstDayDate = getCurrentContext().getStartDate();
        m_I4see1470Prefix = translateAliasToJavaName(I4SEE_1470_PREFIX, false);
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        m_activeScheduleOids = new HashSet<String>();
        Collection<SisSchool> schools = m_schoolMap.values();
        for (SisSchool school : schools) {
            if (school.getActiveSchedule() != null) {
                m_activeScheduleOids.add(school.getActiveSchedule().getOid());
            }
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID,
                getBroker().getCount(schoolQuery));
    }

    /**
     * Load educator ids.
     */
    private void loadEducatorIds() {
        QueryByCriteria query = new QueryByCriteria(StaffCertification.class, new Criteria());
        m_educatorIds = getBroker().getGroupedCollectionByQuery(query, StaffCertification.COL_STAFF_OID,
                getBroker().getCount(query));
    }

    /**
     * *********************************************************************
     *
     * COMMON STATE REPORTING CODE THAT NEEDS TO BE PUT IN A COMMON PLACE
     *
     * *********************************************************************.
     *
     * @param beanPaths String[]
     */
    private void loadStateReferenceCodes(String[] beanPaths) {
        m_fieldToRefTable =
                new HashMap<String, Map<String, String>>((int) (beanPaths.length * HASHMAP_CAPACITY_MULTIPLIER));

        for (String beanPath : beanPaths) {
            m_fieldToRefTable.put(beanPath, getStateCodeReferenceMap(beanPath));
        }
    }

    /**
     * Returns the reference table's state code for a field's reference code.
     *
     * @param beanPath String
     * @param enrollmentCode String
     * @return String
     */
    protected String getStateCode(String beanPath, String enrollmentCode) {
        return m_fieldToRefTable.get(beanPath).get(enrollmentCode);
    }

    /**
     * Returns a map of base reference codes to their state reference code equivalents for the
     * reference table used by the given property. If the property doesn't use a
     * reference table then an empty map is returned.
     *
     * @param propertyName String
     * @return A Map of String keys to String values
     */
    private Map getStateCodeReferenceMap(String propertyName) {
        HashMap baseToStateCodes = null;

        ModelProperty property =
                new ModelProperty(MasterSchedule.class.getName(), propertyName, getBroker().getPersistenceKey());
        DataDictionaryField field = property.getField();
        if (field.hasReferenceTable()) {
            Collection codes = field.getReferenceTable().getReferenceCodes(getBroker());
            baseToStateCodes = new HashMap((int) (codes.size() * HASHMAP_CAPACITY_MULTIPLIER));
            Iterator codeIterator = codes.iterator();
            while (codeIterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) codeIterator.next();
                baseToStateCodes.put(code.getCode(), code.getStateCode());
            }
        } else {
            /*
             * If the propertyName is for the student enrollment codes, these need to be pulled
             * from reference table via the codes specified in the system preferences
             */
            if (propertyName.equals(SisStudent.REL_ENROLLMENTS + "." + StudentEnrollment.COL_ENROLLMENT_CODE)) {
                /*
                 * get the preferences for the organization and the two OIDs for withdrawal codes
                 * and entry codes
                 */
                PreferenceSet preferenceSet = PreferenceManager.getPreferenceSet(getOrganization());
                String[] sysPrefDefs = {SisPreferenceConstants.ENROLLMENT_ENTRY_CODES,
                        SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES};

                baseToStateCodes = new HashMap();
                for (String sysPrefDef : sysPrefDefs) {
                    String refTableOid = preferenceSet.getPreferenceValue(sysPrefDef);
                    ReferenceTable refTable =
                            (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, refTableOid);
                    Collection codes = refTable.getReferenceCodes(getBroker());
                    Iterator codeIterator = codes.iterator();
                    while (codeIterator.hasNext()) {
                        ReferenceCode code = (ReferenceCode) codeIterator.next();
                        baseToStateCodes.put(code.getCode(), code.getStateCode());
                    }
                }
            }
        }

        return baseToStateCodes;
    }

}
