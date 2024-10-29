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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NHI4SeeAcademic.
 */
public class NHI4SeeAcademic extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeAcademic export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        /**
         * Placeholders for calculated unmapped fields. These can be written back to the database
         * in postProcess if update flag is set. Also, holds some calculated values that have
         * been overridden with default or related values.
         *
         * Map key should be field alias constant.
         */
        private Map<String, Object> m_updateValues = null;

        List<StudentEnrollmentSpan> enrollmentSpans;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
            // public no argument constructor for dynamic instantiation.
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

            enrollmentSpans = ((NHI4SeeAcademic) data).m_helper.getStudentEnrollmentSpans((Student) bean, true);

            setRowCount(enrollmentSpans.size());
        }

        /**
         * Check enrollment membership count and membership days parameter to determine if the
         * student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            boolean requireMemberDay = ((Boolean) getData().getParameter(REQUIRE_MEMBER_DAY_PARAM)).booleanValue();

            /*
             * Get membership days parameter
             */
            double membershipCountAsDouble = 0;

            /*
             * Get membership count
             */
            NHI4SeeAcademic i4seeData = (NHI4SeeAcademic) getData();
            String membershipCount = i4seeData.getMembershipDays(this);

            if (membershipCount != null) {
                try {
                    membershipCountAsDouble = Double.parseDouble(membershipCount);
                } catch (NumberFormatException nfe) {
                    // invalid format, will be reported elsewhere.
                }
            }

            // check enrollment count and membership days parameter.
            if ((requireMemberDay && membershipCountAsDouble > 0) || !requireMemberDay) {
                // No filtering.
            } else {
                // Student filtered.
                error = new StateReportValidationError(this, null, "0 member days - excluded from export", "");
            }

            return error;
        }

        /**
         * Returns a field value saved before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored and retrieved before
         * reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @return Object
         */
        public Object getUpdateValue(String doeId) {
            Object value = null;
            if (m_updateValues != null) {
                value = m_updateValues.get(doeId);
            }
            return value;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";

            return name;
        }

        /**
         * Returns the entry enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment
         */
        public StudentEnrollment getEntry() {
            StudentEnrollment entry = null;
            int index = getCurrentRow();

            if (enrollmentSpans != null && index >= 0 && index < enrollmentSpans.size()) {
                entry = enrollmentSpans.get(index).getFirstActiveEnrollment();
            }

            return entry;
        }

        /**
         * Returns the entry enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return Student enrollment span
         */
        public StudentEnrollmentSpan getCurrentSpan() {
            StudentEnrollmentSpan span = null;
            int index = getCurrentRow();

            if (enrollmentSpans != null && index >= 0 && index < enrollmentSpans.size()) {
                span = enrollmentSpans.get(index);
            }

            return span;
        }

        /**
         * Sets a field value before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored
         * and retrieved before reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @param value Object
         */
        public void setUpdateValue(String doeId, Object value) {
            if (m_updateValues == null) {
                m_updateValues = new HashMap<String, Object>();
            }
            m_updateValues.put(doeId, value);
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
    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- User-referencable code
    // ---------------------------------------------------------------------------------------------

    /*
     * Field aliases for "adjusted" fields (these fields may have manually entered values that
     * override/adjust calculated values).
     */
    private static final String ADJUSTED_SCHOOL_NUMBER_FIELD = "i4see Adj School Number";

    /*
     * Field alias for the adjusted district code on the SCHOOL table. This alias is optional.
     */
    // private static final String ADJUSTED_DISTRICT_CODE_FIELD = "i4see ADJUSTED DISTRICT";
    /*
     * Field alias constants. These field aliases are all for the STUDENT table.
     */
    private static final String STUDENT_NAME = "name view";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String I4SEE_STATUS_FIELD = "i4see Status";

    /*
     * Field aliases used in retrieval of i4see 700-737
     */
    private static final String COURSE_I4SEE_AP_ID_FIELD = "i4see AP ID";
    private static final String TRANSCRIPT_AP_EXAM_TAKEN_FIELD = "AP Exam Taken";

    /*
     * AP Exam results
     */
    private static final String I4SEE_NO_AP_COURSE = "";

    /*
     * Header names for the i4see AP fields
     */


    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "include student names" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";



    protected Map<String, Map<String, Transcript>> m_studentTranscriptMap;
    /*
     * Other internal constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_adjustedSchoolCode;
    protected boolean m_includeStudentNames;
    protected Converter m_integerConverter;
    protected String m_reportStatusField;
    protected Map<String, Schedule> m_scheduleMap;
    protected Map<String, SisSchool> m_schoolMap;
    protected DateFormat m_dateFormat;
    protected String m_i4seeCourseApId;
    protected String m_i4seeApExamTaken;
    protected EnrollmentManager m_enrollmentManager;
    protected PlainDate m_firstDayDate;
    protected Set m_firstDayMembers;
    protected PlainDate m_reportDate;
    protected HashMap m_schoolsToCalendars;
    protected StudentHistoryHelper m_helper;


    /**
     * Returns the district number for the given student.
     */
    protected class RetrieveAPInfo implements FieldRetriever {

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
            String code = (String) field.getParameter();

            String apValue = I4SEE_NO_AP_COURSE;

            SisStudent student = (SisStudent) entity.getBean();

            Map<String, Transcript> transcriptMap = m_studentTranscriptMap.get(student.getOid());

            if (transcriptMap != null) {
                Transcript transcript = transcriptMap.get(code);

                if (transcript != null) {
                    String apExamStatus = (String) WebUtils.getProperty(transcript, m_i4seeApExamTaken);

                    // return directly what is in the field with no interpretation
                    if (!StringUtils.isEmpty(apExamStatus)) {
                        apValue = apExamStatus;
                    }
                }
            }

            return apValue;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
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

        m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        /*
         * Set the field definition array
         */
        if (m_includeStudentNames) {
            getFieldDefinitions().add(0, getName());
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */

            m_helper = new StudentHistoryHelper(this);

            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(I4SeeEntity.class);

            /*
             * If no errors so far, continue with query.
             */
            if (getSetupErrors().size() == 0) {
                // Build a map of calculations/retrievers
                HashMap calcs = new HashMap<String, FieldRetriever>();
                HashMap validators = new HashMap<String, FieldRetriever>();

                calcs.put("I4SEE700", new RetrieveAPInfo());

                super.addCalcs(calcs);
                super.addValidators(validators);
            }

            /*
             * Load Schools
             */
            loadSchools();

            /*
             * Load active schedules
             */
            loadActiveSchedules();

            X2Criteria criteria = new X2Criteria();

            criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            criteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ORGANIZATION1_OID,
                    getOrganization().getOid());
            criteria.addNotEmpty(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_i4seeCourseApId, getBroker().getPersistenceKey());

            SubQuery studentSub = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
            criteria.addIn(Transcript.COL_STUDENT_OID, studentSub);

            QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);

            m_studentTranscriptMap = getBroker().getNestedMapByQuery(query,
                    Transcript.COL_STUDENT_OID,
                    Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                            SchoolCourse.REL_COURSE + PATH_DELIMITER +
                            m_i4seeCourseApId,
                    (int) (getBroker().getCount(m_helper.getStudentQuery(false)) * 1.5),
                    64);
        }
    }

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "I4SEE ACADEMIC";
    }

    /**
     * Returns the number of days the student has been a member from the start of school to the
     * report date.
     *
     * @param i4see I4SeeEntity
     * @return String
     */
    public String getMembershipDays(I4SeeEntity i4see) {
        return i4see.getCurrentSpan().getMembershipDays() + "";
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
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME,
                SisStudent.COL_NAME_VIEW,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
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
        m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_firstDayDate, getOrganization());
        m_i4seeCourseApId = translateAliasToJavaName(COURSE_I4SEE_AP_ID_FIELD, true);
        m_i4seeApExamTaken = translateAliasToJavaName(TRANSCRIPT_AP_EXAM_TAKEN_FIELD, true);
        m_reportStatusField = translateAliasToJavaName(I4SEE_STATUS_FIELD, true);
        m_schoolsToCalendars = new HashMap();
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        m_scheduleMap = new HashMap();
        Collection<SisSchool> schools = m_schoolMap.values();
        for (SisSchool school : schools) {
            m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
        }
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }
}
