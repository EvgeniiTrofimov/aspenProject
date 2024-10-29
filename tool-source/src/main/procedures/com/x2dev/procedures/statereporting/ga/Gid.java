/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Georgia state report for Guid Student State ID export.
 * This class implements the data export for GA Guid export.
 *
 * @author X2 Development Corporation
 */
public class Gid extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the GA Guid Student ID export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class GidEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public GidEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize.
         * Increment count of records for trailer record count.
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
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [ID: " + student.getLocalId() +
                    " [Gid: " + student.getStateId() +
                    "]";
            return name;
        }
    }

    /*
     * Report values and aliases.
     */
    private static final String DOE_EXCLUDE_STD_ALIAS = "DOE EXCLUDE STD";
    private static final String DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    private static final String DOE_SCHOOL = "DOE School";

    /*
     * Parameters
     */
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_ENROLLMENT_STATUS = "enrollmentStatus";
    private static final String PARAM_WITHDRAW_AFTER_DATE = "withdrawAfterDate";
    private static final String PARAM_WITHOUT_GTID_ONLY = "withoutGtidOnly";
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_END_DATE = "endDate";

    /*
     * Other internal constants
     */
    private static final String STUDENT_ALIAS_GTID = "GTID";
    private static final String STUDENT_ENROLLMENT_STATUS_ACTIVE = "Active";
    private static final String ENROLLMENT_STATUS_ACTIVE = "active";
    private static final String ENROLLMENT_STATUS_NONACTIVE = "non-active";
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String STRING_SPACE = " ";
    private static final String REF_CODES_RACE = "rtbRaceCodes";

    /*
     * Export Retriever Field Names
     */
    private static final String GID_CLEAN_NAME = "GID-CLEAN-NAME";
    private static final String GID_RACE = "GID-RACE";
    private static final String GID_GUARDIAN_NAME = "GID-GUARDIAN-NAME";
    private static final String GID_ENTRY_DATE = "GID-ENTRY-DATE";
    private static final String STUDENT_CLASS_SCHOOL = "GA-STD-CLS-SKL";

    /*
     * Instance variables.
     */
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected String m_excludeStudentField;
    protected Date m_reportDate;
    protected String m_overrideSchoolCodeField;
    protected String m_enrollmentStatus;
    protected Date m_withdrawAfterDate;
    protected Boolean m_withoutGtidOnly;
    protected String m_queryString;
    protected int m_queryBy;
    protected Date m_startDate;
    protected Date m_endDate;
    protected String m_schoolCodeField;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, Collection<StudentEnrollment>> m_enrollmentMap;

    /**
     * Retrieve the student Entry Date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEntryDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<StudentEnrollment> enrollments = m_enrollmentMap.get(student.getOid());

            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                        PlainDate entryDate = enrollment.getEnrollmentDate();
                        if (entryDate != null) {
                            if ((entryDate.equals(m_startDate) || entryDate.after(m_startDate))
                                    && entryDate.before(m_endDate)) {
                                value = entryDate;
                                break;
                            }
                        }
                    }
                }
            }

            return value;
        }
    }

    /**
     * Get Student Guardian Name.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGuardianName implements FieldRetriever {

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
            String guardianName = null;
            SisStudent student = (SisStudent) entity.getBean();
            StudentContact primaryContact = student.getPrimaryContact();

            if (primaryContact != null) {
                Person primaryContactPerson = primaryContact.getPerson();
                if (primaryContactPerson != null) {
                    guardianName =
                            primaryContactPerson.getFirstName() + STRING_SPACE + primaryContactPerson.getLastName();
                    if (guardianName.length() > 60) {
                        guardianName = guardianName.substring(0, 60);
                    }
                }
            }

            return guardianName;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
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
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll(EMPTY_STRING);
            } else {
                cleanValue = EMPTY_STRING;
            }

            return cleanValue;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In GA, this is:
     * "W" - White
     * "B" - Black
     * "S" - Asian
     * "I" - Indian/Native/Alaskan
     * "P" - Pacific
     *
     * Ex: "SNS" searches for the Asian code, returns "S" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);

            String raceCode = falseChar;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(requestCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = trueChar;
                        break;
                    }
                }
            }

            return raceCode;
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
            String overrideSchoolCodeField = ((Gid) data).m_overrideSchoolCodeField;
            String schoolCodeField = ((Gid) data).m_schoolCodeField;
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
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        m_enrollmentStatus = (String) getParameter(PARAM_ENROLLMENT_STATUS);
        m_withdrawAfterDate = (Date) getParameter(PARAM_WITHDRAW_AFTER_DATE);
        m_withoutGtidOnly = (Boolean) getParameter(PARAM_WITHOUT_GTID_ONLY);
        m_startDate = getStringDateParameter(PARAM_START_DATE);
        m_endDate = getStringDateParameter(PARAM_END_DATE);
        m_reportDate = ((Date) getParameter(PARAM_REPORT_DATE));
        m_queryString = (String) getParameter(PARAM_QUERY_STRING);
        m_queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();

        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            int sort = ((Integer) getParameter(PARAM_SORT)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(GidEntity.class);

            loadMaps(studentCriteria);

            // Add any retrievers.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(GID_CLEAN_NAME, new RetrieveStripNameChar());
            calcs.put(GID_RACE, new RetrieveRace());
            calcs.put(GID_GUARDIAN_NAME, new RetrieveGuardianName());
            calcs.put(GID_ENTRY_DATE, new RetrieveEntryDate());
            calcs.put(STUDENT_CLASS_SCHOOL, new RetrieveSchool());
            super.addCalcs(calcs);
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Get a Plain Date form a String Date Parameter.
     *
     * @param dateParameterName String
     * @return Plain date
     */
    private PlainDate getStringDateParameter(String dateParameterName) {
        PlainDate plainDate = null;
        String stringDate = (String) getParameter(dateParameterName);

        if (!StringUtils.isEmpty(stringDate)) {
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            try {
                plainDate = new PlainDate(dateFormat.parse(stringDate));
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }

        return plainDate;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria studentCriteria = new X2Criteria();

        studentCriteria.addNotEqualTo(m_excludeStudentField, BooleanAsStringConverter.TRUE);

        /*
         * Include Student Enrollment Status
         */
        if (ENROLLMENT_STATUS_ACTIVE.equals(m_enrollmentStatus)) {
            studentCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, STUDENT_ENROLLMENT_STATUS_ACTIVE);
        } else if (ENROLLMENT_STATUS_NONACTIVE.equals(m_enrollmentStatus)) {
            studentCriteria.addNotEqualTo(SisStudent.COL_ENROLLMENT_STATUS, STUDENT_ENROLLMENT_STATUS_ACTIVE);

            if (m_withdrawAfterDate != null) {
                studentCriteria.addEqualTo(
                        SisStudent.REL_ENROLLMENTS + PATH_DELIMITER + StudentEnrollment.COL_ENROLLMENT_TYPE,
                        StudentEnrollment.WITHDRAWAL);
                studentCriteria.addGreaterOrEqualThan(
                        SisStudent.REL_ENROLLMENTS + PATH_DELIMITER + StudentEnrollment.COL_ENROLLMENT_DATE,
                        m_withdrawAfterDate);
            }
        }

        /*
         * Student without Gtid Only
         */
        if (m_withoutGtidOnly.booleanValue()) {
            String gtidFieldName = translateAliasToJavaName(STUDENT_ALIAS_GTID, false);
            studentCriteria.addIsNull(gtidFieldName);
        }

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        /*
         * Check student selection criteria user input.
         */
        switch (m_queryBy) {
            case 1: // YOG
                studentCriteria.addEqualTo(SisStudent.COL_YOG, m_queryString);
                break;

            case 2: // LASID
                studentCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, m_queryString);
                break;

            case 3: // SASID
                studentCriteria.addEqualTo(SisStudent.COL_STATE_ID, m_queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentCriteria, m_queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return studentCriteria;
    }

    /**
     * Initialize instance variables, aliases, lookups.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_excludeStudentField = translateAliasToJavaName(DOE_EXCLUDE_STD_ALIAS, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_OVERRIDE_SCHOOL, true);
        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL, true);
    }

    /**
     * Load maps of supporting data.
     *
     * @param studentCriteria Criteria
     */
    private void loadMaps(Criteria studentCriteria) {
        // Get race code reference codes for use in the race retriever.
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODES_RACE);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

        // Load the race codes for all students included in the export.
        SubQuery studentQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
        criteria = new Criteria();
        criteria.addIn(Race.COL_PERSON_OID, studentQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, criteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

        // Student OID subquery for remaining maps.
        studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Load a map of student enrollment records.
        criteria = new Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentQuery);
        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, criteria);
        enrollmentQuery.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        enrollmentQuery.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        enrollmentQuery.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
        m_enrollmentMap =
                getBroker().getGroupedCollectionByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 100);
    }

}
