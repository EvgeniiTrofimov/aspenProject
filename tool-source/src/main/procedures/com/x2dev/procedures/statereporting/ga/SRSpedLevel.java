/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.sis.model.beans.IepTeamMember;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for Student Record Special Education
 * Level export.
 *
 * @author X2 Development Corporation
 */
public class SRSpedLevel extends StateReportData {
    /**
     * Entity class for Student Record Special Education Level export.
     *
     * @author X2 Development Corporation
     */
    public static class SRSpedLevelEntity extends StateReportEntity {
        /**
         * List of events this IEP has
         */
        DataGrid m_spedEvents = new DataGrid(14);

        /**
         * The IEP related to the student
         */
        IepData m_iep;

        /**
         * The state report data
         */
        SRSpedLevel m_sriep;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SRSpedLevelEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Student student = (Student) getBean();

            String name = student.getNameView() +
                    " [Local ID: " + student.getLocalId() +
                    ", GTID: " + student.getFieldValueByAlias(ALIAS_GTID) +
                    "]";
            return name;
        }

        /**
         * Initialize.
         *
         * Fill in the event code and event dates for:
         * - [01] Babies Can't Wait
         * - [02] Parent Consent to Evaluation
         * - [03] Initial Evaluation
         * - [04] Initial Eligibility Determination
         * - [05] Initial IEP Meeting
         * - [06] Initial IEP Placement/Transition Service Begin
         * - [07] IEP Annual Review
         * - [08] Re-Eligibility Determination
         * - [09] Special Education Exit
         * - [10] Parent Revoked Consent
         * - [11] Student Not Eligible for Initial Placement
         * - [12] Parent Refused Initial Placement
         * - [13] District Verified That Student Was Incorrectly Reported as SWD in a Prior Year
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_sriep = (SRSpedLevel) data;
            Student student = (Student) bean;
            IepData iep = m_sriep.m_iepMap.get(student.getOid());

            if (iep != null) {
                m_iep = iep;

                if (iep.getFieldValueByAlias(ALIAS_BABIES_NO_WAIT, m_sriep.m_iepDataDictionary) != null) // 01
                {
                    addToGrid("01", convertStringToDate(
                            iep.getFieldValueByAlias(ALIAS_BABIES_NO_WAIT, m_sriep.m_iepDataDictionary).toString()));
                }
                if (iep.getConsentReceivedDate() != null) // 02
                {
                    addToGrid("02", iep.getConsentReceivedDate());
                }
                if (iep.getFieldValueByAlias(ALIAS_INIT_EVAL_DATE, m_sriep.m_iepDataDictionary) != null) // 03
                {
                    addToGrid("03", convertStringToDate(
                            iep.getFieldValueByAlias(ALIAS_INIT_EVAL_DATE, m_sriep.m_iepDataDictionary).toString()));
                }
                if (iep.getInitialEligibilityDate() != null) // 04
                {
                    addToGrid("04", iep.getInitialEligibilityDate());
                }
                if (iep.getFieldValueByAlias(ALIAS_INIT_MTG_DATE, m_sriep.m_iepDataDictionary) != null) // 05
                {
                    addToGrid("05", convertStringToDate(
                            iep.getFieldValueByAlias(ALIAS_INIT_MTG_DATE, m_sriep.m_iepDataDictionary).toString()));
                }
                if (iep.getFieldValueByAlias(ALIAS_INIT_PLACE_DATE, m_sriep.m_iepDataDictionary) != null) // 06
                {
                    addToGrid("06", convertStringToDate(
                            iep.getFieldValueByAlias(ALIAS_INIT_PLACE_DATE, m_sriep.m_iepDataDictionary).toString()));
                }
                if (iep.getLastReviewDate() != null) // 07
                {
                    addToGrid("07", iep.getLastReviewDate());
                }
                if (iep.getLastEligibilityDate() != null) // 08
                {
                    addToGrid("08", iep.getLastEligibilityDate());
                }
                String notEligibleDate =
                        (String) iep.getFieldValueByAlias(ALIAS_NOT_ELIGIBLE, m_sriep.m_iepDataDictionary);
                if (!StringUtils.isEmpty(notEligibleDate)) // 11
                {
                    addToGrid("11", convertStringToDate(notEligibleDate));
                }
                String parentRefusedPaceDate = (String) iep.getFieldValueByAlias(ALIAS_PARENT_REFUSE_INIT_PLACEMENT,
                        m_sriep.m_iepDataDictionary);
                if (!StringUtils.isEmpty(parentRefusedPaceDate)) // 12
                {
                    addToGrid("12", convertStringToDate(parentRefusedPaceDate));
                }
                String parentConsProv =
                        (String) iep.getFieldValueByAlias(ALIAS_IEP_PAR_CONS_PROV, m_sriep.m_iepDataDictionary);
                if (!StringUtils.isEmpty(parentConsProv)) {
                    addToGrid("14", convertStringToDate(parentConsProv));
                } else if (iep.getFieldValueByAlias(ALIAS_IEP_PAR_CONS_PROV) != null) {
                    addToGrid("14", convertStringToDate((String) iep.getFieldValueByAlias(ALIAS_IEP_PAR_CONS_PROV)));
                }
                String reevWaiver =
                        (String) iep.getFieldValueByAlias(ALIAS_IEP_REEV_WAIVER, m_sriep.m_iepDataDictionary);
                if (!StringUtils.isEmpty(reevWaiver)) {
                    addToGrid("15", convertStringToDate(reevWaiver));
                } else if (iep.getFieldValueByAlias(ALIAS_IEP_REEV_WAIVER) != null) {
                    addToGrid("15", convertStringToDate((String) iep.getFieldValueByAlias(ALIAS_IEP_REEV_WAIVER)));
                }
            }

            // For code 09 and 10, it's either/or (they'll never be both)
            String spedExitReason = (String) student.getFieldValueByAlias(ALIAS_SPED_EXIT_REASON);
            if (!StringUtils.isEmpty(spedExitReason)) {
                ReferenceCode refCode = m_sriep.m_exitReasonMap.get(spedExitReason);
                String stateCode = refCode.getStateCode();
                if (stateCode != null && stateCode.matches("09|10")) {
                    addToGrid(stateCode, student.getSpedExitDate());
                }
            }
            String spedMarkedIncorrectly = (String) student.getFieldValueByAlias(ALIAS_SPED_INCORRECTLY);
            if (!StringUtils.isEmpty(spedMarkedIncorrectly)) // 13
            {
                addToGrid("13", convertStringToDate(spedMarkedIncorrectly));

            }

            setRowCount(m_spedEvents.getRows().size());
        }

        /**
         * Add event to the entity's list of events to report.
         *
         * @param stateCode the state code of the event
         * @param date the date when the event happened
         */
        private void addToGrid(String stateCode, Date date) {
            SRSpedLevel srIep = (SRSpedLevel) getData();
            if (date != null) {
                if (m_sriep.m_includeAllDates || (!date.before(srIep.m_startDate) && !date.after(srIep.m_endDate))) {
                    m_spedEvents.append();
                    m_spedEvents.set(GRID_CODE, stateCode);
                    m_spedEvents.set(GRID_DATE, ((SRSpedLevel) getData()).m_srDateFormat.format(date)); // yyyyMMdd
                                                                                                        // format
                }
            } else {
                FieldDefinition fieldDefinition = srIep.getFieldDefinition(FORMAT_FIELD_EVENT_CODE);
                StateReportValidationError error = new StateReportValidationError(this, fieldDefinition,
                        "Student has a Special Education Exit Reason, but does not have a Special Education Exit Date",
                        " Student = " + STYLE_BOLD + getEntityName() + STYLE_END);
                addRetrievalError(FORMAT_FIELD_EVENT_CODE, error);
            }
        }

        /**
         * Convert String in "yyyy-MM-dd" format into a java.util.Date object
         *
         * @param sDate date in text (yyyy-MM-dd) format
         * @return Date object
         */
        private Date convertStringToDate(String sDate) {
            Date result = null;
            try {
                result = ((SRSpedLevel) getData()).m_sqlDateFormat.parse(sDate);
            } catch (ParseException e) {
                e.printStackTrace();
            }
            return result;
        }
    }

    /**
     * Constants for reporting information.
     */
    // The following three will share with SpedReferralProcedure for workflow execution, hence scope
    // public
    public static final String ALIAS_NOT_ELIGIBLE = "DOE SPED StudentNotEligibleDat"; // code 11
    public static final String ALIAS_PARENT_REFUSE_INIT_PLACEMENT = "DOE SPED ParentRefuseInitPlace"; // code
                                                                                                      // 12
    public static final String ALIAS_SPED_INCORRECTLY = "DOE SPED StudentAsSWDIncorrect"; // code 13

    protected static final String IEP_DDX_ID = "SPED-GA-IEP";
    protected static final String ALIAS_BABIES_NO_WAIT = "babies-no-wait";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_GTID = "GTID";
    protected static final String ALIAS_IEP_PAR_CONS_PROV = "all-iep-ParentalConsentProv";
    protected static final String ALIAS_IEP_REEV_WAIVER = "all-iep-ReevaluationWaiver";
    protected static final String ALIAS_INIT_EVAL_DATE = "initEvalDate"; // extended fieldA017
    protected static final String ALIAS_INIT_MTG_DATE = "initMtgDate"; // extended fieldA016
    protected static final String ALIAS_INIT_PLACE_DATE = "initPlaceDate"; // extended fielda018
    protected static final String ALIAS_PRIMARY_EXCEPTIONALITY = "DOE Primary Exceptionality";
    protected static final String ALIAS_SPED_EXIT_REASON = "DOE SPED EXIT REASON";
    protected static final String DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    protected static final String DOE_SCHOOL = "DOE School";

    protected static final String FORMAT_FIELD_EVENT_CODE = "Event Code";
    protected static final String FORMAT_FIELD_EVENT_DATE = "Event Date";
    protected static final String GRID_DATE = "date";
    protected static final String GRID_CODE = "code";
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    protected static final String PARAM_INCLUDE_ALL_DATES = "includeAllDates";
    protected static final String PARAM_START_DATE = "startDate";
    protected static final String PARAM_QUERY_BY = "queryBy";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_SORT = "sort";

    protected final SimpleDateFormat m_sqlDateFormat = new SimpleDateFormat("yyyy-MM-dd");
    protected final SimpleDateFormat m_srDateFormat = new SimpleDateFormat("yyyyMMdd");

    /**
     * Local variables for reporting information.
     */
    protected String m_activeCode;
    protected String m_overrideSchoolCodeField;
    protected String m_schoolCodeField;
    protected Map<String, Map<PlainDate, Collection<IepMeetingAttendance>>> m_attendanceParent;
    protected Map<String, Map<PlainDate, Collection<IepMeetingAttendance>>> m_attendanceStudent;
    protected PlainDate m_endDate;
    protected Map<String, ReferenceCode> m_exitReasonMap;
    protected String m_fieldPrimaryExceptionality;
    protected String m_fieldExcludeSkl;
    protected String m_fieldExcludeStd;
    protected DataDictionary m_iepDataDictionary;
    protected Map<String, IepData> m_iepMap;
    protected Map<String, Map<PlainDate, Collection<IepMeeting>>> m_iepMeetings;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected boolean m_includeAllDates;
    protected PlainDate m_reportDate;
    protected PlainDate m_startDate;

    /**
     * Retrieve the Event Code or Event Date from the entity's grid (which depends on row).
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEvent implements FieldRetriever {
        private static final String PARAM_EVENT_CODE = "C";
        private static final String PARAM_EVENT_DATE = "D";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            String value = null;

            SRSpedLevelEntity e = (SRSpedLevelEntity) entity;
            int rowNumber = entity.getCurrentRow();
            e.m_spedEvents.gotoRow(rowNumber);

            if (param.equals(PARAM_EVENT_CODE)) {
                value = (String) e.m_spedEvents.get(GRID_CODE);
            } else if (param.equals(PARAM_EVENT_DATE)) {
                value = (String) e.m_spedEvents.get(GRID_DATE);
            }
            return value;
        }
    }

    /**
     * Retrieve Y or N if the Student or Parent was present at an Initial or Review IEP meeting.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveConferencePresence implements FieldRetriever, FieldValidator {

        private static final String FORMAT_DATE = "yyyyMMdd";
        private static final String PARAM_PARENT = "P";
        private static final String PARAM_STUDENT = "S";
        Map<String, Integer> m_gradeLevelLookup;
        SimpleDateFormat m_dateFormat;

        /**
         * Instantiates a new retrieve conference presence.
         */
        public RetrieveConferencePresence() {
            m_gradeLevelLookup = StudentManager.buildNumericGradeLevelMap(getBroker());
            m_dateFormat = new SimpleDateFormat(FORMAT_DATE);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Boolean result = null;
            String param = (String) field.getParameter();
            IepData iep = ((SRSpedLevelEntity) entity).m_iep;

            if (iep != null) {
                String eventCode = entity.getFieldValue(FORMAT_FIELD_EVENT_CODE);
                if (eventCode.matches("05|07")) {
                    // STUDENT PRESENT AT CONFERENCE NEEDS TO BE NOT EMPTY IF STUDENT IS 16+ YEARS
                    // OLD OR 9 <= GRADE <= 12
                    if (param.equals(PARAM_STUDENT)) {
                        SisStudent student = (SisStudent) entity.getBean();
                        SisPerson person = student.getPerson();
                        int age = person.getAgeAsOfDate(m_reportDate);
                        String gradeLevelAsString = student.getGradeLevel();
                        Integer gradeLevel = m_gradeLevelLookup.get(gradeLevelAsString);
                        if (16 <= age || (gradeLevel != null &&
                                9 <= gradeLevel.intValue() &&
                                gradeLevel.intValue() <= 12)) {
                            result = Boolean.FALSE;
                        }
                    }

                    // PARENT PRESENT AT CONFERENCE NEEDS TO HAVE 'Y'/'N'
                    if (param.equals(PARAM_PARENT)) {
                        result = Boolean.FALSE;
                    }

                    if (param.equals(PARAM_PARENT)
                            && isPresent(iep, m_attendanceParent, entity.getFieldValue(FORMAT_FIELD_EVENT_DATE))) {
                        result = Boolean.TRUE;
                    }

                    if (param.equals(PARAM_STUDENT) && result != null
                            && isPresent(iep, m_attendanceStudent, entity.getFieldValue(FORMAT_FIELD_EVENT_DATE))) {
                        result = Boolean.TRUE;
                    }
                }
            }

            return result;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String attendanceValue = entity.getFieldValue(field.getFieldId());
            if (!StringUtils.isEmpty(attendanceValue)) {
                PlainDate date = convertDate(entity.getFieldValue(FORMAT_FIELD_EVENT_DATE));
                if (date != null) {
                    int numMeetings = 0;
                    IepData iep = ((SRSpedLevelEntity) entity).m_iep;
                    Map<PlainDate, Collection<IepMeeting>> dateMeetingsMap = m_iepMeetings.get(iep.getStudentOid());
                    if (dateMeetingsMap != null) {
                        Collection<IepMeeting> meetings = dateMeetingsMap.get(date);
                        if (meetings != null) {
                            numMeetings = meetings.size();
                        }
                    }
                    if (numMeetings != 1) {
                        String error = "Meeting Search Error";
                        String message = "A single meeting should be associated with " + date
                                + ". There were " + numMeetings + " meetings found.";
                        errors.add(new StateReportValidationError(entity, field, error, message));
                    }
                }
            }
            return errors;
        }

        /**
         * @param dateString
         * @return
         */
        private PlainDate convertDate(String dateString) {
            PlainDate date = null;
            try {
                date = new PlainDate(m_dateFormat.parse(dateString));
            } catch (ParseException pe) {
                // return null
            }
            return date;
        }

        /**
         * @param iep
         * @param dateString
         * @param m_attendanceStudent
         * @return
         */
        private boolean isPresent(IepData iep,
                                  Map<String, Map<PlainDate, Collection<IepMeetingAttendance>>> studentMap,
                                  String dateString) {
            boolean result = false;
            Map<PlainDate, Collection<IepMeetingAttendance>> attendanceMap = studentMap.get(iep.getStudentOid());
            if (attendanceMap != null) {
                PlainDate date = convertDate(dateString);
                if (date != null) {
                    Collection<IepMeetingAttendance> attendances = attendanceMap.get(date);
                    if (attendances != null && !attendances.isEmpty()) {
                        result = true;
                    }
                }
            }
            return result;
        }
    }

    /**
     * Retrieve the school from the entity. This will be the
     * school on report date or last withdrawal.
     * Optionally, a student can have an override school code assigned.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String overrideSchoolCodeField = ((SRSpedLevel) data).m_overrideSchoolCodeField;
            String schoolCodeField = ((SRSpedLevel) data).m_schoolCodeField;
            value = (String) getProperty(student, overrideSchoolCodeField);
            if (StringUtils.isEmpty(value)) {
                SisSchool school = student.getSchool();
                if (school != null && schoolCodeField != null) {
                    value = (String) getProperty(school, schoolCodeField);
                }
            }
            return value;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * String to date.
     *
     * @param dateIn String
     * @return Date
     */
    Date stringToDate(String dateIn) {
        Date date = null;

        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(dateIn);
        } catch (Exception ex) {
            // DO NoTHING
        }

        return date;
    }

    /**
     * Validate whether the event codes and dates follows the rules .
     *
     * @author Follett Software Company
     */
    protected class ValidateEvent implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRSpedLevel sriep = (SRSpedLevel) data;
            SisStudent student = (SisStudent) entity.getBean();
            IepData iep = sriep.m_iepMap.get(student.getOid());

            String date11String = (String) iep.getFieldValueByAlias(ALIAS_NOT_ELIGIBLE, sriep.m_iepDataDictionary);
            String date12String =
                    (String) iep.getFieldValueByAlias(ALIAS_PARENT_REFUSE_INIT_PLACEMENT, sriep.m_iepDataDictionary);
            PlainDate date04Date = iep.getInitialEligibilityDate();
            String date05String = (String) iep.getFieldValueByAlias(ALIAS_INIT_MTG_DATE, sriep.m_iepDataDictionary);
            String date06String = (String) iep.getFieldValueByAlias(ALIAS_INIT_PLACE_DATE, sriep.m_iepDataDictionary);
            PlainDate date07Date = iep.getLastReviewDate();
            PlainDate date08Date = iep.getLastEligibilityDate();
            String date0910String = (String) student.getFieldValueByAlias(ALIAS_NOT_ELIGIBLE);

            if ("11".equals(value)) {
                // E5941: EVENT CODE '04' is required when EVENT CODE '11' is reported.
                if (!StringUtils.isEmpty(date11String) && date04Date == null) {
                    String error = "E5941";
                    String message = "EVENT CODE '04' (" + date04Date
                            + ") is required when EVENT CODE '11' is reported (" + date11String + ")";
                    errors.add(new StateReportValidationError(entity, field, error, message));
                }
                // E5943: The EVENT DATE for EVENT CODE '11' must be the same date as EVENT CODE
                // '04'
                if (date11String != null && date04Date != null && !(date11String.equals(date04Date.toString()))) {
                    String error = "E5943";
                    String message = "The EVENT DATE for EVENT CODE '11' (" + date11String
                            + ") must be the same date as EVENT CODE '04', " + date04Date + ".";
                    errors.add(new StateReportValidationError(entity, field, error, message));
                }
                // E5945: When EVENT CODE '11' is reported, the following EVENT CODEs are invalid:
                // 05, 06, 07, 08, 09, 10, and 12.
                if ((date11String != null) && (date05String != null || date06String != null ||
                        date07Date != null || date08Date != null || date0910String != null || date12String != null)) {
                    String error = "E5945";
                    String String05 = (date05String == null) ? "" : " 05 (" + date05String + ")";
                    String string06 = (date06String == null) ? "" : " 06 (" + date06String + ")";
                    String string07 = (date07Date == null) ? "" : " 07 (" + date07Date + ")";
                    String string08 = (date08Date == null) ? "" : " 08 (" + date08Date + ")";
                    String string0910 = (date0910String == null) ? "" : " 09|10 (" + date0910String + ")";
                    String string12 = (date12String == null) ? "" : " 12 (" + date12String + ")";

                    String message = "When EVENT CODE '11' is reported, the following EVENT CODEs are invalid:"
                            + String05 + string06 + string07 + string08 + string0910 + string12;

                    errors.add(new StateReportValidationError(entity, field, error, message));
                }

            }
            if ("12".equals(value)) {
                // E5942: The EVENT CODEs '04' and '05' are required when EVENT CODE '12' is
                // reported.
                if (!StringUtils.isEmpty(date12String) && (date04Date == null || date05String == null)) {
                    String error = "E5942";
                    String message = "The EVENT CODEs '04' (" + date04Date + ") and '05' (" + date05String
                            + ") are required when EVENT CODE '12', " + date12String + ", is reported.";
                    errors.add(new StateReportValidationError(entity, field, error, message));
                }

                // E5944: The EVENT DATE for EVENT CODE '12' must be the same date or later than the
                // date of EVENT CODE '05'.
                if (date12String != null && date05String != null) {
                    if (stringToDate(date12String).compareTo(stringToDate(date05String)) < 0) {
                        String error = "E5944";
                        String message = "The EVENT DATE for EVENT CODE '12' (" + date12String
                                + ") must be the same date or later than the date of EVENT CODE '05', " + date05String
                                + ".";
                        errors.add(new StateReportValidationError(entity, field, error, message));
                    }
                }

                // E5946: When EVENT CODE '12' is reported, the following EVENT
                // CODEs are invalid: 06, 07, 08, 09, 10, and 11.
                if ((date12String != null) && (date06String != null || date07Date != null || date08Date != null
                        || date0910String != null || date11String != null)) {
                    String error = "E5945";
                    String string06 = (date06String == null) ? "" : " 06 (" + date06String + ")";
                    String string07 = (date07Date == null) ? "" : " 07 (" + date07Date + ")";
                    String string08 = (date08Date == null) ? "" : " 08 (" + date08Date + ")";
                    String string0910 = (date0910String == null) ? "" : " 09|10 (" + date0910String + ")";
                    String string11 = (date12String == null) ? "" : " 11 (" + date11String + ")";

                    String message = "When EVENT CODE '12' is reported, the following EVENT CODEs are invalid:"
                            + string06 + string07 + string08 + string0910 + string11;

                    errors.add(new StateReportValidationError(entity, field,
                            error, message));
                }
            }

            // E5947: The EVENT DATE of verification for EVENT CODE '13' must after the date of the
            // last prior EVENT DATE. // do nothing for this

            /*
             * Event 13 Should only be used to correct misreporting of a student as SWD in a PRIOR
             * year.
             * It is not intended to be used for misreporting in the current school year. Current
             * SWD data incorrectly reported
             * should be deleted. This event will require comments before signoff.
             */

            return errors;
        }
    }

    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        initializeFields();

        // Set the report date.
        m_reportDate = new PlainDate(); // (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        if (m_startDate == null) {
            m_startDate = getOrganization().getCurrentContext().getStartDate();
        }
        m_endDate = getOrganization().getCurrentContext().getEndDate();
        m_includeAllDates = false;
        if (getParameter(PARAM_INCLUDE_ALL_DATES) != null &&
                getParameter(PARAM_INCLUDE_ALL_DATES) instanceof Boolean) {
            m_includeAllDates = ((Boolean) getParameter(PARAM_INCLUDE_ALL_DATES)).booleanValue();
        }

        // Lookup aliases
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        Criteria studentCriteria = getStudentCriteria();
        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);
        Integer sort = (Integer) getParameter(PARAM_SORT);
        switch (sort != null ? sort.intValue() : 0) {
            case 0: // Name
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 1: // YOG
                studentQuery.addOrderByAscending(Student.COL_YOG);
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 2: // School
                studentQuery.addOrderByAscending(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_NAME);
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 3: // LASID
                studentQuery.addOrderByAscending(Student.COL_LOCAL_ID);
                break;

            case 4: // SASID
                studentQuery.addOrderByAscending(Student.COL_STATE_ID);
                break;

            default:
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;
        }

        // Set the query to be used for student selection.
        setQuery(studentQuery);
        setEntityClass(SRSpedLevelEntity.class);

        // Get the IEP data dictionary
        Criteria ddxCriteria = new Criteria();
        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, IEP_DDX_ID);
        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        ExtendedDataDictionary ddxIep = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        m_iepDataDictionary = DataDictionary.getDistrictDictionary(ddxIep, getBroker().getPersistenceKey());

        // Preload the Previous IEPs for students first (this is because we'll put in the ACTIVE
        // ones after)
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria iepCriteria = new Criteria();
        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal()));
        iepCriteria.addIn(IepData.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria previousIepQuery = new QueryByCriteria(IepData.class, iepCriteria);
        previousIepQuery.addOrderByAscending(IepData.COL_START_DATE);
        previousIepQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        m_iepMap = getBroker().getMapByQuery(previousIepQuery, IepData.COL_STUDENT_OID, 128);

        // Preload the DISCARDED IEPs for students second
        Criteria discardedIepCriteria = new Criteria();
        discardedIepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(StatusCode.DISCARDED.ordinal()));
        discardedIepCriteria.addIn(IepData.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria discardedIepQuery = new QueryByCriteria(IepData.class, discardedIepCriteria);
        discardedIepQuery.addOrderByAscending(IepData.COL_START_DATE);
        discardedIepQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        QueryIterator discardedIterator = getBroker().getIteratorByQuery(discardedIepQuery);
        try {
            while (discardedIterator.hasNext()) {
                IepData iep = (IepData) discardedIterator.next();
                m_iepMap.put(iep.getStudentOid(), iep);
            }
        } finally {
            discardedIterator.close();
        }

        // Preload the Active IEPs for students next
        // Any student with an Active IEP would override a Previous or DISCARDED IEP, if the student
        // had one
        Criteria activeIepCriteria = new Criteria();
        activeIepCriteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(StatusCode.ACTIVE.ordinal()));
        activeIepCriteria.addIn(IepData.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria activeIepQuery = new QueryByCriteria(IepData.class, activeIepCriteria);
        QueryIterator activeIterator = getBroker().getIteratorByQuery(activeIepQuery);
        try {
            while (activeIterator.hasNext()) {
                IepData iep = (IepData) activeIterator.next();
                m_iepMap.put(iep.getStudentOid(), iep);
            }
        } finally {
            activeIterator.close();
        }

        // load meetings
        X2Criteria iepMeetingCriteria = new X2Criteria();
        iepMeetingCriteria.addIn(IepMeeting.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria iepMeetingQuery = new QueryByCriteria(IepMeeting.class, iepMeetingCriteria);
        m_iepMeetings = getBroker().getGroupedCollectionByQuery(iepMeetingQuery,
                new String[] {IepMeeting.COL_STUDENT_OID,
                        IepMeeting.COL_DATE},
                new int[] {128, 10});

        // get attendance criteria
        X2Criteria iepAttendanceCriteria = new X2Criteria();
        iepAttendanceCriteria.addEqualTo(IepMeetingAttendance.COL_PRESENT_INDICATOR,
                Boolean.TRUE);
        iepAttendanceCriteria.addIn(IepMeetingAttendance.COL_STUDENT_OID, studentSubQuery);

        X2Criteria iepParentAttendanceCriteria = iepAttendanceCriteria.copy();
        iepParentAttendanceCriteria.addIn(IepMeetingAttendance.REL_TEAM_MEMBER +
                PATH_DELIMITER + IepTeamMember.COL_MEMBER_ROLE_CODE,
                getCodes(IepTeamMember.class, IepTeamMember.COL_MEMBER_ROLE_CODE, "P"));
        QueryByCriteria iepParentQuery = new QueryByCriteria(IepMeetingAttendance.class, iepParentAttendanceCriteria);
        m_attendanceParent = getBroker().getGroupedCollectionByQuery(iepParentQuery,
                new String[] {IepMeetingAttendance.COL_STUDENT_OID,
                        IepMeetingAttendance.REL_IEP_MEETING + PATH_DELIMITER + IepMeeting.COL_DATE},
                new int[] {128, 10});

        X2Criteria iepStudentAttendanceCriteria = iepAttendanceCriteria.copy();
        iepStudentAttendanceCriteria.addIn(IepMeetingAttendance.REL_TEAM_MEMBER +
                PATH_DELIMITER + IepTeamMember.COL_MEMBER_ROLE_CODE,
                getCodes(IepTeamMember.class, IepTeamMember.COL_MEMBER_ROLE_CODE, "S"));
        QueryByCriteria iepStudentQuery = new QueryByCriteria(IepMeetingAttendance.class, iepStudentAttendanceCriteria);
        m_attendanceStudent = getBroker().getGroupedCollectionByQuery(iepStudentQuery,
                new String[] {IepMeetingAttendance.COL_STUDENT_OID,
                        IepMeetingAttendance.REL_IEP_MEETING + PATH_DELIMITER + IepMeeting.COL_DATE},
                new int[] {128, 10});

        // Preload the Special Education Exit Reasons codes
        Criteria exitReasonCriteria = new Criteria();
        DataDictionaryField reasonField = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryFieldByAlias(ALIAS_SPED_EXIT_REASON);
        exitReasonCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, reasonField.getReferenceTableOid());
        QueryByCriteria exitReasonQuery = new QueryByCriteria(ReferenceCode.class, exitReasonCriteria);
        m_exitReasonMap = getBroker().getMapByQuery(exitReasonQuery, ReferenceCode.COL_CODE, 4);

        // Add any retrievers
        RetrieveConferencePresence conferencePresenceRetriever = new RetrieveConferencePresence();
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("GA-SR-CLEAN", new RetrieveStripNameChar());
        calcs.put("GA-SR-SCHOOL", new RetrieveSchool());
        calcs.put("GA-SR-EVENT", new RetrieveEvent());
        calcs.put("GA-SR-PRESENCE", conferencePresenceRetriever);
        super.addCalcs(calcs);

        // Add any validators
        HashMap vals = new HashMap<String, FieldValidator>();
        vals.put("GA-SR-EVENT", new ValidateEvent());
        vals.put("GA-SR-PRESENCE", conferencePresenceRetriever);
        super.addValidators(vals);

    }

    /**
     * Initialize fields needed for this export.
     */
    private void initializeFields() {
        m_fieldPrimaryExceptionality = translateAliasToJavaName(ALIAS_PRIMARY_EXCEPTIONALITY, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_OVERRIDE_SCHOOL, true);
        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL, true);
        m_fieldExcludeSkl = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldExcludeStd = translateAliasToJavaName(ALIAS_EXCLUDE_STD, true);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Gets the a set of reference codes from the reference table for a particular field. Throw an
     * IllegalStateException if no codes are found.
     *
     * @param beanClass Class
     * @param beanPath String
     * @param stateCode String
     * @return Sets the
     */
    private Set<String> getCodes(Class beanClass, String beanPath, String stateCode) {
        Set<String> codes = new HashSet();
        ModelProperty prop = new ModelProperty(beanClass, beanPath, getBroker().getPersistenceKey());
        DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());
        if (!StringUtils.isEmpty(field.getReferenceTableOid())) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, stateCode);
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

            String[] columns = new String[] {ReferenceCode.COL_CODE};

            ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] record = (Object[]) iterator.next();
                    String code = (String) record[0];
                    codes.add(code);
                }
            } finally {
                iterator.close();
            }
        } else {
            throw new IllegalStateException("No reference codes found for " + beanClass.getName() + " field " + beanPath
                    + " with state reference code value " + stateCode);
        }
        return codes;
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Students to include.
         *
         * 1. The student is active and in an active school.
         * or
         * 2. The student has (E,W) enrollment records within the school year.
         *
         */

        // Select students with enrollment activity (E,W) in the school this year.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        X2Criteria enrollCriteria2 = new X2Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        X2Criteria activityCriteria = new X2Criteria();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_startDate);
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        activityCriteria.addAndCriteria(enrollCriteria2);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, m_activeCode);

        primaryCriteria.addOrCriteria(enrollCriteria);

        return primaryCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria userCriteria = new X2Criteria();

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            userCriteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            userCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            userCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        userCriteria.addNotEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + m_fieldExcludeSkl, Boolean.TRUE);
        userCriteria.addNotEqualTo(m_fieldExcludeStd, Boolean.TRUE);

        /*
         * Check student selection criteria user input.
         */
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }
}
