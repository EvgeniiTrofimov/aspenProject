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
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote><font color="green" size="4"> i4see CATE Course export.</font></blockquote>
 *
 * @author X2 Development Corporation
 * @since v2.11a
 */
public class NHI4SeeCourse extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        private NHI4SeeCourse m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
            // public no argument constructor for dynamic instantiation.
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
            MasterSchedule masterSchedule = (MasterSchedule) getBean();

            String excludeFlag;
            try {
                excludeFlag =
                        (String) WebUtils.getProperty(getBean(), MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                                + SchoolCourse.REL_COURSE + PATH_DELIMITER + m_data.m_excludeCourse);

                if (excludeFlag != null) {
                    // check enrollment count and membership days parameter.
                    if ("1".equals(excludeFlag)) {
                        // Course filtered.
                        error = new StateReportValidationError(this.toString(), masterSchedule.getDescription(),
                                " - excluded from export", "");
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

            m_data = (NHI4SeeCourse) getData();

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
            String indexString = (String) field.getParameter();
            Integer index = Integer.valueOf(indexString);

            MasterSchedule masterSchedule = (MasterSchedule) entity.getBean();

            LinkedList<StaffCertification> staffCertifications = m_educatorIds.get(masterSchedule.getPrimaryStaffOid());

            if (staffCertifications != null && staffCertifications.size() >= index.intValue()) {
                value = staffCertifications.get(index.intValue() - 1).getCertificationNumber();
            }

            return value;
        }
    }

    /**
     * The Class Retrieve1400BeginDate.
     */
    protected class Retrieve1400BeginDate implements FieldRetriever {

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
            ScheduleTerm term = masterSchedule.getScheduleTerm();
            Pair<PlainDate, PlainDate> startEndTermDates = getStartEndDateForTerm(term);
            return startEndTermDates == null ? null : startEndTermDates.getLeft();
        }
    }

    /**
     * The Class Retrieve1440Credits.
     */
    protected class Retrieve1440Credits implements FieldRetriever {

        private static final String CALC_ID_I4SEE1440 = "I4SEE1440";

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
            Object credits = null;
            if (masterSchedule != null &&
                    masterSchedule.getSchoolCourse() != null) {
                credits = masterSchedule.getSchoolCourse().getFieldValueByBeanPath(m_fieldCskCredits);
            }
            return credits;
        }
    }

    /**
     * The Class Retrieve1410EndDate.
     */
    protected class Retrieve1410EndDate implements FieldRetriever {

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
            ScheduleTerm term = masterSchedule.getScheduleTerm();
            Pair<PlainDate, PlainDate> startEndTermDates = getStartEndDateForTerm(term);
            return startEndTermDates == null ? null : startEndTermDates.getRight();
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
            String code = (String) WebUtils.getProperty(entity.getBean(), MasterSchedule.REL_SCHOOL_COURSE
                    + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER + m_primaryGradeId);

            String gradeId = code;
            if (!StringUtils.isEmpty(code)) {
                gradeId = getStateCode(field.getBeanPath(), code);
            }
            // gradeId = lookupStateValue(Course.class, m_primaryGradeId, gradeId);

            return gradeId;
        }

    }

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
            String courseNumber = null;
            if (masterSchedule != null &&
                    masterSchedule.getSchoolCourse() != null &&
                    masterSchedule.getSchoolCourse().getCourse() != null) {
                courseNumber = masterSchedule.getSchoolCourse().getCourse().getNumber();
                if (!StringUtils.isEmpty(courseNumber) && courseNumber.length() > 15) {
                    courseNumber = courseNumber.substring(0, 16);
                }
            }
            return courseNumber;
        }
    }

    /**
     * The Class Retrieve1620SchoolNumber.
     */
    /*
     * Retrieves receiving School Number data from the current entity
     */
    protected class Retrieve1620SchoolNumber implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            if (StringUtils.isEmpty(m_receivingSchoolNumber)) {
                MasterSchedule section = (MasterSchedule) entity.getBean();

                if (section != null) {
                    SchoolCourse schoolCourse = section.getSchoolCourse();

                    if (schoolCourse != null) {
                        SisSchool school = schoolCourse.getSchool();

                        value = (String) school.getFieldValueByBeanPath(m_schoolNumberField);
                    }
                }
            } else {
                value = m_receivingSchoolNumber;
            }

            return value;
        }

    }

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
    protected class Retrieve1470LocalClassCodeCate implements FieldRetriever {

        private static final String CALC_ID = "LOCAL_CODE";

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
            String courseNumber = null;
            if (masterSchedule != null) {
                courseNumber = (String) masterSchedule.getFieldValueByBeanPath(m_mstLocalClass);
                if (!StringUtils.isEmpty(courseNumber) && courseNumber.length() > 15) {
                    courseNumber = courseNumber.substring(0, 16);
                }
            }
            if (StringUtils.isEmpty(courseNumber)) {
                courseNumber = masterSchedule.getCourseView();
            }
            return courseNumber;
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

        private static final String EXPORT_FIELD_TERM_ID = "Term ID";
        protected List<String> m_validTerms = new ArrayList<>(Arrays.asList(S1, S2, FY));

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

            PlainDate scheduleBeginDate = null;
            PlainDate scheduleEndDate = null;
            String beginDateString = entity.getFieldValue(I4SEE_1400_TABLE_REFERENCE);
            String endDateString = entity.getFieldValue(I4SEE_1410_TABLE_REFERENCE);
            String termId = entity.getFieldValue(EXPORT_FIELD_TERM_ID);
            termId = termId == null ? "" : termId;
            try {

                if (beginDateString != null && endDateString != null) {
                    scheduleBeginDate = new PlainDate(m_dateFormat.parse(beginDateString));
                    scheduleEndDate = new PlainDate(m_dateFormat.parse(endDateString));

                    String schoolOid = (String) WebUtils.getProperty(entity.getBean(),
                            MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID);
                    SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                    if (!m_validTerms.contains(termId)) {
                        StateReportValidationError error =
                                new StateReportValidationError(entity, field, "Invalid term",
                                        "Term should be 1, 2 or 30");
                        entity.addRetrievalError(ALIAS_I4SEE_1780_NUM_MEETING_SEMSTR_ONE, error);
                    } else if ((termId.equals(S1) && S1.equals(semesterID))
                            || (termId.equals(S2) && S2.equals(semesterID))) {
                        inSessionDates = getInSessionDays(school, scheduleBeginDate, scheduleEndDate);
                    } else if (termId.equals(FY)) {
                        if (S1.equals(semesterID)) {
                            inSessionDates = getInSessionDays(school, scheduleBeginDate, getS1EndDate(school));
                        } else if (S2.equals(semesterID)) {
                            inSessionDates = getInSessionDays(school, getS2StartDate(school), scheduleEndDate);
                        }
                    }
                } else {
                    StateReportValidationError error = new StateReportValidationError(entity, field, INVALID_DATES,
                            "Schedule Term not defined for course?");
                    entity.addRetrievalError(ALIAS_I4SEE_1780_NUM_MEETING_SEMSTR_ONE, error);
                }
            } catch (ParseException e) {
                StateReportValidationError error = new StateReportValidationError(entity, field, INVALID_DATES,
                        "Schedule Term not defined for course?");
                entity.addRetrievalError(ALIAS_I4SEE_1780_NUM_MEETING_SEMSTR_ONE, error);
            }

            return Integer.valueOf(inSessionDates.size());
        }

        /**
         * Gets the s 1 end date.
         *
         * @param school SisSchool
         * @return Plain date
         */
        private PlainDate getS1EndDate(SisSchool school) {

            Map<String, Pair<PlainDate, PlainDate>> termStartEndDatesMap =
                    m_schoolTermStartEndDatesMap.get(school.getOid());
            Pair<PlainDate, PlainDate> startEndDates = termStartEndDatesMap.get(S1);
            return startEndDates == null ? null : startEndDates.getRight();
        }

        /**
         * Gets the s 2 start date.
         *
         * @param school SisSchool
         * @return Plain date
         */
        private PlainDate getS2StartDate(SisSchool school) {
            Map<String, Pair<PlainDate, PlainDate>> termStartEndDatesMap =
                    m_schoolTermStartEndDatesMap.get(school.getOid());
            Pair<PlainDate, PlainDate> startEndDates = termStartEndDatesMap.get(S2);
            return startEndDates == null ? null : startEndDates.getLeft();
        }
    }

    /**
     * The Class Retrieve SCED Common Course Code.
     */
    protected class Retrieve1800SCEDCode implements FieldRetriever {
        private static final String CALC_ID_I4SEE1800 = "I4SEE1800";
        private static final String SCED_PREFIX = "SCED";
        private static final int SCED_LENGTH = 5;

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
            MasterSchedule mst = (MasterSchedule) entity.getBean();
            Course crs = null;
            String code = null;
            if (mst.getSchoolCourse() != null && (crs = mst.getSchoolCourse().getCourse()) != null) {
                code = StringUtils
                        .unNullify(data.lookupStateValue(Course.class, m_fieldCrsScedCode,
                                (String) crs.getFieldValueByBeanPath(m_fieldCrsScedCode)));
                if (!StringUtils.isEmpty(code)) {
                    if (code.length() < SCED_LENGTH) {
                        code = StringUtils.padLeft(code, SCED_LENGTH, '0');
                    }
                    code = SCED_PREFIX + code + StringUtils.unNullify(
                            data.lookupStateValue(Course.class, Course.COL_ACADEMIC_LEVEL, crs.getAcademicLevel()));
                }
            }
            return code;
        }
    }

    /**
     * Input Definition Parameters
     */
    public static final String RECEIVING_SCHOOL_NUMBER_PARAM = "receivingSchoolNumber";
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";
    public static final String REPORT_TYPE_PARAM = "reportType";

    /**
     * Retriever Parameters
     */
    private static final String CALC_ID_I4SEE1200 = "I4SEE1200";
    private static final String CALC_ID_I4SEE1201 = "I4SEE1201";
    private static final String CALC_ID_I4SEE1202 = "I4SEE1202";
    private static final String CALC_ID_I4SEE1400 = "I4SEE1400";
    private static final String CALC_ID_I4SEE1410 = "I4SEE1410";
    private static final String CALC_ID_I4SEE1450 = "I4SEE1450";
    private static final String CALC_ID_I4SEE1470 = "I4SEE1470";
    private static final String CALC_ID_I4SEE1620 = "I4SEE1620";
    private static final String CALC_ID_I4SEE1780 = "I4SEE1780";
    private static final String CALC_ID_I4SEE1790 = "I4SEE1790";

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

    // COURSE
    private static final String ALIAS_CRS_1800_SCED_CODE = "i4see 1800";
    private static final String ALIAS_I4SEE_1450_PRI_GRADE_ID = "i4see 1450";
    private static final String ALIAS_I4SEE_1740_PROGRAM_ID = "i4see 1740";
    private static final String ALIAS_I4SEE_COURSE_EXCLUDE = "i4see Course Exclude";

    // SCHEDULE_MASTER
    private static final String ALIAS_I4SEE_1470_LOCAL_CLASS_CODE = "i4see 1470";
    private static final String ALIAS_I4SEE_1470_PREFIX = "i4see 1470 prefix";
    private static final String ALIAS_I4SEE_1780_NUM_MEETING_SEMSTR_ONE = "i4see 1780";

    // SCHOOL
    private static final String ALIAS_I4SEE_050_SCHOOL_NUMBER = "i4see 050";
    private static final String ALIAS_I4SEE_1430_SCHEDULE_ID = "i4see 1430";


    // STUDENT
    private static final String ALIAS_ADJUSTED_SCHOOL_NUMBER_FIELD = "i4see Adj School Number";

    // CSK
    private static final String ALIAS_CSK_I4SEE_1440 = "i4see 1440";

    /*
     * Other internal constants
     */
    public static final Integer REPORT_TYPE_NONCATE = Integer.valueOf(0);
    public static final Integer REPORT_TYPE_CATE = Integer.valueOf(0);

    private final static double HASHMAP_CAPACITY_MULTIPLIER = 1.5;

    private static final String INVALID_DATES = "Invalid date";

    private static final String I4SEE_1400_TABLE_REFERENCE = "Begin Date";
    private static final String I4SEE_1410_TABLE_REFERENCE = "End Date";

    /**
     * bit values to <code>OR</code> together to index the <code>NH_STATE_RACE_CODES</code> table
     */
    private static final int WHITE_RACE_IND = 1;
    private static final int BLACK_RACE_IND = 2;
    private static final int ASIAN_RACE_IND = 4;
    private static final int AMERICAN_INDIAN_RACE_IND = 8;
    private static final int PACIFIC_ISLANDER_RACE_IND = 16;
    private static final int HISPANIC_RACE_IND = 32;

    // valid grade state codes
    private final static String S1 = "1";
    private final static String S2 = "2";
    private final static String FY = "30";

    private static final String TERM_CODE_S1 = "S1";
    private static final String TERM_CODE_S2 = "S2";
    private static final String TERM_CODE_FY = "FY";

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
            0 // not used (nor is it valid)
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
            63 // 63 x x x x x x
    };

    /**
     * Supporting instance variables. These are protected rather than private so they can be
     * accessed by the inner classes.
     */
    protected Map<String, Integer> m_absences;
    protected Set<String> m_activeScheduleOids = new HashSet<String>();
    protected String m_adjustedSchoolCode;
    protected boolean m_calculateTotals;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected Map<String, LinkedList<StaffCertification>> m_educatorIds;
    protected EnrollmentManager m_enrollmentManager;
    protected String m_excludeCourse;
    protected Set<Object> m_fieldDefinitionsAdded = new HashSet();
    protected String m_fieldCrsScedCode;
    protected String m_fieldCskCredits;
    protected HashMap<String, Map<String, String>> m_fieldToRefTable;
    protected PlainDate m_firstDayDate;
    protected boolean m_includeStudentNames;
    protected String m_I4see1470Prefix;
    protected String m_mstLocalClass;
    protected Map<String, Map<String, String>> m_ownerStateCodeTermIDMap = null;
    protected String m_primaryGradeId;
    protected String m_programId;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected String m_receivingSchoolNumber;
    protected String m_reportStatusField;
    protected Integer m_reportType;
    protected String m_scheduleTermIdField;
    protected Map<String, SisSchool> m_schoolMap;
    protected String m_schoolNumberField;
    protected Map<String, Map<String, Pair<PlainDate, PlainDate>>> m_schoolTermStartEndDatesMap = null;
    protected List<String> m_s1s2Terms = new ArrayList<>(Arrays.asList(S1, S2));
    protected Map<String, Pair<PlainDate, PlainDate>> m_termStartEndMap = new HashMap<>();

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
        String title = "I4SEE Submission Course";
        if (REPORT_TYPE_CATE.equals(m_reportType)) {
            title = "I4SEE CATE Course";
        }

        return title;
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
        loadStateReferenceCodes();

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

        // load school term start end date map. delete schoole where hasn't term s1 and s2
        if (m_reportType.equals(REPORT_TYPE_CATE)) { // CATE Course
            loadSchoolTermStartEndDatesMapAndDeleteInvalidSchool();
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria masterScheduleCriteria = getMasterScheduleCriteria();
            applyInputCriteria(masterScheduleCriteria, true, null);

            QueryByCriteria masterScheduleQuery = new QueryByCriteria(MasterSchedule.class, masterScheduleCriteria);
            applyInputSort(masterScheduleQuery, null);

            // Set the query to be used for student selection.
            setQuery(masterScheduleQuery);
            setEntityClass(I4SeeEntity.class);
        }
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();

            if (m_reportType.equals(REPORT_TYPE_CATE)) // CATE Course
            {
                calcs.put(CALC_ID_I4SEE1200, new Retrieve1200EducatorId());
                calcs.put(CALC_ID_I4SEE1201, new Retrieve1200EducatorId());
                calcs.put(CALC_ID_I4SEE1202, new Retrieve1200EducatorId());
                calcs.put(CALC_ID_I4SEE1400, new Retrieve1400BeginDate());
                calcs.put(CALC_ID_I4SEE1410, new Retrieve1410EndDate());
                calcs.put(CALC_ID_I4SEE1620, new Retrieve1620SchoolNumber());
                calcs.put(CALC_ID_I4SEE1780, new Retrieve1780NumMeetingSemstr());
                calcs.put(CALC_ID_I4SEE1790, new Retrieve1780NumMeetingSemstr());
                calcs.put(Retrieve1470LocalClassCodeCate.CALC_ID, new Retrieve1470LocalClassCodeCate());
                calcs.put(Retrieve1440Credits.CALC_ID_I4SEE1440, new Retrieve1440Credits());
            } else // Submission Course
            {
                calcs.put(CALC_ID_I4SEE1200, new Retrieve1200EducatorId());
                calcs.put(CALC_ID_I4SEE1400, new Retrieve1400BeginDate());
                calcs.put(CALC_ID_I4SEE1410, new Retrieve1410EndDate());
                calcs.put(CALC_ID_I4SEE1450, new Retrieve1450PrimaryGradeId());
                calcs.put(CALC_ID_I4SEE1470, new Retrieve1470LocalClassCode());
                calcs.put(Retrieve1800SCEDCode.CALC_ID_I4SEE1800, new Retrieve1800SCEDCode());
            }
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            super.addValidators(validators);
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

        // String javaName = translateAliasToJavaName(ALIAS_I4SEE_1740_PROGRAM_ID, true);
        if (REPORT_TYPE_CATE.equals(m_reportType)) {
            /*
             * Limit the criteria to items that contain a CIP (i4see 1740)
             */
            masterScheduleCriteria.addNotNull(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER + m_programId);
        }
        masterScheduleCriteria.addIn(MasterSchedule.COL_SCHEDULE_OID, m_activeScheduleOids);

        if (!isSchoolContext()) {
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
        // System Parameters
        m_firstDayDate = getCurrentContext().getStartDate();
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        // Load Parameters
        m_reportType = Integer.valueOf(0);
        if (getParameter(REPORT_TYPE_PARAM) != null) {
            m_reportType = (Integer) getParameter(REPORT_TYPE_PARAM);
        }
        if (getParameter(REPORT_TYPE_PARAM) != null) {
            m_reportType = (Integer) getParameter(REPORT_TYPE_PARAM);
        }

        m_receivingSchoolNumber = (String) getParameter(RECEIVING_SCHOOL_NUMBER_PARAM);

        // Load Alias database field Names
        m_adjustedSchoolCode = translateAliasToJavaName(ALIAS_ADJUSTED_SCHOOL_NUMBER_FIELD, true);
        m_I4see1470Prefix = translateAliasToJavaName(ALIAS_I4SEE_1470_PREFIX, false);
        m_excludeCourse = translateAliasToJavaName(ALIAS_I4SEE_COURSE_EXCLUDE, false);
        m_primaryGradeId = translateAliasToJavaName(ALIAS_I4SEE_1450_PRI_GRADE_ID, true);
        m_mstLocalClass = translateAliasToJavaName(ALIAS_I4SEE_1470_LOCAL_CLASS_CODE, true);
        m_programId = translateAliasToJavaName(ALIAS_I4SEE_1740_PROGRAM_ID, true);
        m_schoolNumberField = translateAliasToJavaName(ALIAS_I4SEE_050_SCHOOL_NUMBER, true);
        m_scheduleTermIdField = translateAliasToJavaName(ALIAS_I4SEE_1430_SCHEDULE_ID, true);
        m_fieldCrsScedCode = translateAliasToJavaName(ALIAS_CRS_1800_SCED_CODE, true);
        m_fieldCskCredits = translateAliasToJavaName(ALIAS_CSK_I4SEE_1440, true);
    }

    /**
     * invalid if map is empty or hasn't any m_validTerms.
     *
     * @param stateCodeTermIDMap Map<String,String>
     * @return true, if is valid state code term ID map
     */
    private boolean isValidStateCodeTermIDMap(Map<String, String> stateCodeTermIDMap) {
        boolean isValid = true;
        if (stateCodeTermIDMap == null || stateCodeTermIDMap.size() != m_s1s2Terms.size()) {
            isValid = false;
        } else {
            for (String key : stateCodeTermIDMap.keySet()) {
                if (!m_s1s2Terms.contains(key)) {
                    isValid = false;
                    break;
                }
            }
        }
        return isValid;
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        Collection<SisSchool> schools = m_schoolMap.values();

        for (SisSchool school : schools) {
            if (school.getActiveSchedule() != null) {
                m_activeScheduleOids.add(school.getActiveSchedule().getOid());
            }
        }
    }

    /**
     * Load Education Id's.
     */
    private void loadEducatorIds() {
        QueryByCriteria query = new QueryByCriteria(StaffCertification.class, new Criteria());

        m_educatorIds = getBroker().getGroupedCollectionByQuery(query, StaffCertification.COL_STAFF_OID,
                getBroker().getCount(query));
    }

    /**
     * load map where each school has start and end dates for terms where state code are 1,2,30
     * delete/validate school if hasn't.
     */
    private void loadSchoolTermStartEndDatesMapAndDeleteInvalidSchool() {
        Boolean validateSchool = (Boolean) getParameter("validateSchool");
        if (validateSchool == null) {
            validateSchool = Boolean.FALSE;
        }
        m_schoolTermStartEndDatesMap = new HashMap<String, Map<String, Pair<PlainDate, PlainDate>>>();
        loadOwnerStateCodeTermIDMap();
        Set<String> schoolForDelete = new TreeSet<String>();
        for (SisSchool school : m_schoolMap.values()) {
            Map<String, Pair<PlainDate, PlainDate>> termStartEndDatesMap =
                    new HashMap<String, Pair<PlainDate, PlainDate>>();
            m_schoolTermStartEndDatesMap.put(school.getOid(), termStartEndDatesMap);

            Map<String, String> stateCodeTermIDMap = m_ownerStateCodeTermIDMap.get(school.getOid().trim());
            boolean isValid = isValidStateCodeTermIDMap(stateCodeTermIDMap);
            if (!isValid) {
                stateCodeTermIDMap = m_ownerStateCodeTermIDMap.get(getOrganization().getOid().trim());
                isValid = isValidStateCodeTermIDMap(stateCodeTermIDMap);
            }

            if (!isValid) {
                stateCodeTermIDMap = new HashMap<String, String>();
                stateCodeTermIDMap.put(S1, TERM_CODE_S1);
                stateCodeTermIDMap.put(S2, TERM_CODE_S2);
                stateCodeTermIDMap.put(FY, TERM_CODE_FY);
            }
            Map<String, String> codeStateTermIDMap = new HashMap<String, String>();
            for (Entry<String, String> entry : stateCodeTermIDMap.entrySet()) {
                codeStateTermIDMap.put(entry.getValue(), entry.getKey());
            }


            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, school.getOid());
            criteria.addIn(m_scheduleTermIdField, stateCodeTermIDMap.values());
            criteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            criteria.addIn(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER + X2BaseBean.COL_OID, m_activeScheduleOids);
            QueryByCriteria queryByCriteria = new QueryByCriteria(ScheduleTerm.class, criteria);
            Collection<ScheduleTerm> terms = getBroker().getCollectionByQuery(queryByCriteria);


            // validate
            // min one record for state code
            if (terms.size() < m_s1s2Terms.size()) {

                if (validateSchool.booleanValue()) {
                    addSetupError("Can not find all terms where term state codes are " + m_s1s2Terms.toString(),
                            "School - " + school.getName());
                }

                schoolForDelete.add(school.getOid());

            }

            // each code should has valid state code
            for (ScheduleTerm term : terms) {
                String termCode = (String) term.getFieldValueByBeanPath(m_scheduleTermIdField);
                if (!codeStateTermIDMap.containsKey(termCode)) {

                    if (validateSchool.booleanValue()) {
                        addSetupError("Can not find appropriate state code for term code",
                                "School - " + school.getName()
                                        + " Term " + termCode);
                    }

                    schoolForDelete.add(school.getOid());
                }
            }

            for (ScheduleTerm term : terms) {
                boolean invalidTerm = false;
                String termCode = (String) term.getFieldValueByBeanPath(m_scheduleTermIdField);
                Pair<PlainDate, PlainDate> startEndTermDates = getStartEndDateForTerm(term);
                if (startEndTermDates != null) {
                    if (startEndTermDates.getLeft() == null || startEndTermDates.getRight() == null) {
                        schoolForDelete.add(school.getOid());
                        invalidTerm = true;
                    }
                    termStartEndDatesMap.put(codeStateTermIDMap.get(termCode), startEndTermDates);
                } else {
                    invalidTerm = true;
                }
                if (invalidTerm && validateSchool.booleanValue()) {
                    addSetupError("Can not find term start or end date", "School - " + school.getName() + " Term "
                            + termCode);
                }
            }


        }

        for (String schoolOid : schoolForDelete) {
            SisSchool school = m_schoolMap.get(schoolOid);
            m_schoolMap.remove(schoolOid);
            m_activeScheduleOids.remove(school.getActiveScheduleOid());
        }
    }

    /**
     * load map where key is owner oid (school or district) and value map where key is valid state
     * codes for schedule term code.
     */
    private void loadOwnerStateCodeTermIDMap() {
        DataDictionaryField ddxFieldTermId = getDataDictionaryField(ScheduleTerm.class, m_scheduleTermIdField);
        ReferenceTable rtbTermId = ddxFieldTermId.getReferenceTable();
        m_ownerStateCodeTermIDMap = new HashMap<>();
        if (rtbTermId != null) {
            for (ReferenceCode refCode : rtbTermId.getReferenceCodes()) {
                if (!refCode.getDisabledIndicator() && m_s1s2Terms.contains(refCode.getStateCode())) {
                    String ownerOid = refCode.getOwnerOid().trim();
                    Map<String, String> stateCodeTermIDMap = m_ownerStateCodeTermIDMap.get(ownerOid);
                    if (stateCodeTermIDMap == null) {
                        stateCodeTermIDMap = new HashMap<>();
                        m_ownerStateCodeTermIDMap.put(ownerOid, stateCodeTermIDMap);
                    }
                    stateCodeTermIDMap.put(refCode.getStateCode(), refCode.getCode());

                }
            }
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {


        X2Criteria criteria = new X2Criteria();
        criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        }

        QueryByCriteria schoolQuery =
                new QueryByCriteria(SisSchool.class, criteria);



        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

    /**
     * Loads State Reference Codes.
     */
    private void loadStateReferenceCodes() {
        String[] beanPaths = {MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + m_primaryGradeId};

        m_fieldToRefTable =
                new HashMap<String, Map<String, String>>((int) (beanPaths.length * HASHMAP_CAPACITY_MULTIPLIER));

        for (String beanPath : beanPaths) {
            m_fieldToRefTable.put(beanPath, getStateCodeReferenceMap(beanPath));
        }
    }

    /**
     * Gets the start end date for term.
     *
     * @param term ScheduleTerm
     * @return Pair
     */
    private Pair<PlainDate, PlainDate> getStartEndDateForTerm(ScheduleTerm term) {

        Pair<PlainDate, PlainDate> startEndTermDates = null;

        if (term != null) {
            startEndTermDates = m_termStartEndMap.get(term.getOid());
            if (startEndTermDates == null) {
                Collection<ScheduleTermDate> scheduleTermDates = term.getScheduleTermDates();
                PlainDate startDate = null;
                PlainDate endDate = null;
                if (scheduleTermDates != null) {
                    for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                        if (startDate == null || scheduleTermDate.getStartDate().before(startDate)) {
                            startDate = scheduleTermDate.getStartDate();
                        }
                        if (endDate == null || scheduleTermDate.getEndDate().after(endDate)) {
                            endDate = scheduleTermDate.getEndDate();
                        }
                    }
                } else {
                    startDate = term.getSchedule().getStartDate();
                    endDate = term.getSchedule().getEndDate();
                }

                startEndTermDates = Pair.of(startDate, endDate);
                m_termStartEndMap.put(term.getOid(), startEndTermDates);
            }
        }

        return startEndTermDates;
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

            /*
             * Iterator codeIterator = codes.iterator();
             * while (codeIterator.hasNext())
             * {
             * ReferenceCode code = (ReferenceCode) codeIterator.next();
             * baseToStateCodes.put(code.getCode(), code.getStateCode());
             * }
             */ Collection<ReferenceCode> referenceCodes = field.getReferenceTable().getReferenceCodes(getBroker());
            baseToStateCodes = new HashMap((int) (referenceCodes.size() * HASHMAP_CAPACITY_MULTIPLIER));

            if (referenceCodes != null) {
                for (ReferenceCode referenceCode : referenceCodes) {
                    baseToStateCodes.put(referenceCode.getCode(), referenceCode.getStateCode());
                }
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
                    Collection<ReferenceCode> referenceCodes = refTable.getReferenceCodes(getBroker());

                    if (referenceCodes != null) {
                        for (ReferenceCode referenceCode : referenceCodes) {
                            baseToStateCodes.put(referenceCode.getCode(), referenceCode.getStateCode());
                        }
                    }

                    // Collection codes = refTable.getReferenceCodes(getBroker());
                    // Iterator codeIterator = codes.iterator();
                    /*
                     * while (codeIterator.hasNext())
                     * {
                     * ReferenceCode code = (ReferenceCode) codeIterator.next();
                     * baseToStateCodes.put(code.getCode(), code.getStateCode());
                     * }
                     */ }
            }
        }

        return baseToStateCodes;
    }

}
