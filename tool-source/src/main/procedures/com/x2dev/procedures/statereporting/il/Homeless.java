/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Homeless Data Collection
 *
 * Search through generic form data that uses the Homeless extended
 * dictionary and belongs to active students.
 *
 * @author X2 Development Corporation
 */
public class Homeless extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL. This must be a
     * public static inner class with a public no argument constructor so it can
     * be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class HomelessEntity extends StateReportEntity {
        /**
         * Effective enrollment of the student.
         */
        StudentEnrollment m_enrollment = null;

        /**
         * Homeless data.
         */
        Homeless m_hData = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * The student for this entity
         */
        SisStudent m_student = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public HomelessEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Filter only records from students that are active by report date.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            StudentEnrollment recentEnrollment = getEffectiveEnrollment();
            if (recentEnrollment == null) {
                error = new StateReportValidationError(getEntityName(), "Enrollments", "No student enrollment records",
                        "");
            } else if (recentEnrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                error = new StateReportValidationError(getEntityName(), "Enrollment date", "Before report date", "");
            }
            return error;
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if (m_enrollment != null && m_enrollment.getSchool() != null) {
                school = (String) m_enrollment.getSchool().getFieldValueByBeanPath(m_hData.m_fieldSchoolCode);
            }

            return school;
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_student.getNameView() +
                    " [LASID: " + m_student.getLocalId() +
                    ", SASID: " + m_student.getStateId() + "]";
            return name;
        }

        /**
         * Initialize and increment counter
         *
         * If there is no recent entry enrollment record, ignore it.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_hData = (Homeless) data;
            GenericFormData genericFormData = (GenericFormData) bean;
            FormInstance instance = m_hData.m_formMap.get(genericFormData.getOid());
            m_student = (SisStudent) instance.getOwnerObject();
            m_enrollment = m_hData.m_helper.getEnrollmentForDate(m_student.getOid(), m_hData.m_reportDate, "E");

            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_hData.m_excludeSklField))) {
                m_rcdtsMap = lookupOverrides();

                // keep count of records
                m_hData.m_totalHomelessRecords++;
            } else {
                setRowCount(0);
            }
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

        /**
         * Lookup district and school codes, including overrides on the enrollment record.
         *
         * @return Map
         */
        private Map<String, String> lookupOverrides() {
            Map<String, String> calcValueMap = new HashMap<String, String>();
            Homeless hData = (Homeless) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (hData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = hData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode = hData.lookupStateValue(StudentEnrollment.class, hData.m_fieldServiceSchoolCode,
                            serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
        }

    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";

    /*
     * Parameters
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_QUERY_BY = "queryBy";

    /**
     * Map of student enrollments by student oid, used in retrieving a student's RCDTS info
     */
    protected Map<String, List<StudentEnrollment>> m_enrollmentMap;

    /*
     * Instance variables
     */
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_excludeStdField;
    protected String m_excludeSklField;
    protected String m_fieldDistrictCode;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected int m_totalHomelessRecords;
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();
    protected String m_fieldRcdtsForServingSchool;

    /**
     * Map of form instances by the storage's oid (which is generic form data), used in retrieving
     * the student's information
     */
    protected Map<String, FormInstance> m_formMap;

    /**
     * Map of night-time residences codes by refrence code, used in retrieving the residence field
     */
    Map<String, ReferenceCode> m_residencesMap;

    /**
     * Retrieve the state code for this field.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrievePrimaryNightResidence implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String result = "";
            GenericFormData gfd = (GenericFormData) entity.getBean();
            String value = (String) getProperty(gfd, field.getBeanPath());
            ReferenceCode code = m_residencesMap.get(value);
            if (code != null) {
                result = code.getStateCode();
            }
            return result;
        }
    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRCDTS implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            HomelessEntity hEntity = (HomelessEntity) entity;
            String rcdts = null;
            if (param.equals("H") && hEntity.getEffectiveEnrollment() != null
                    && hEntity.getEffectiveEnrollment().getSchool() != null) {
                Homeless hData = (Homeless) data;
                rcdts = (String) hEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(hData.m_fieldSchoolCode);
            } else if (param.equals("S")) {
                rcdts = hEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = hEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve the generic form data's owner's (Student) values by its beanpath (from calc param).
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStudentInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String beanPath = (String) field.getParameter();
            return ((HomelessEntity) entity).m_student.getFieldValueByBeanPath(beanPath);
        }

    }

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("Homeless");
        heading.append(',');
        heading.append(m_totalHomelessRecords);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(m_reportDate));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));
        initializeFields();
        m_helper = new StudentHistoryHelper(this);
        /*
         * Get only records for active students who have a Homeless form instance
         */
        SubQuery studentSubQuery = getStudentSubQuery();

        /*
         * Create map of form instances
         */
        Criteria instanceCriteria = new Criteria();
        instanceCriteria.addIn(FormInstance.COL_OWNER_OBJECT_OID, studentSubQuery);
        instanceCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER +
                FormDefinition.COL_ID, "HOMELESS");
        QueryByCriteria instanceQuery = new QueryByCriteria(FormInstance.class, instanceCriteria);
        m_formMap = getBroker().getMapByQuery(instanceQuery, FormInstance.COL_STORAGE_OBJECT_OID, 64);

        Criteria formCriteria = new Criteria();
        SubQuery instanceSub = new SubQuery(FormInstance.class, FormInstance.COL_STORAGE_OBJECT_OID, instanceCriteria);
        formCriteria.addIn(X2BaseBean.COL_OID, instanceSub);
        QueryByCriteria formQuery = new QueryByCriteria(GenericFormData.class, formCriteria);

        setQuery(formQuery);
        setEntityClass(HomelessEntity.class);

        /*
         * Reference codes for row 9 - Primary Night-time Residence
         */
        X2Criteria residencesCrit = new X2Criteria();
        residencesCrit.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                "Night-time Residences");
        QueryByCriteria residencesQuery = new QueryByCriteria(ReferenceCode.class, residencesCrit);
        m_residencesMap = getBroker().getMapByQuery(residencesQuery, ReferenceCode.COL_CODE, 10);

        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
        m_enrollmentMap =
                getBroker().getGroupedCollectionByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 64);

        // Additional rule for secondary OUTPLACEMENT school
        X2Criteria secondaryOutplacementCriteria = new X2Criteria();
        secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                "OUTPLACEMENT");
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());
        secondaryOutplacementCriteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_reportDate);
        X2Criteria sskEndDate = new X2Criteria();
        sskEndDate.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, m_reportDate);
        X2Criteria sskEndDateNull = new X2Criteria();
        sskEndDateNull.addEmpty(StudentSchool.COL_END_DATE, getBroker().getPersistenceKey());
        sskEndDate.addOrCriteria(sskEndDateNull);
        secondaryOutplacementCriteria.addAndCriteria(sskEndDate);

        QueryByCriteria secondaryOutplacementQuery =
                new QueryByCriteria(StudentSchool.class, secondaryOutplacementCriteria);
        QueryIterator iter = getBroker().getIteratorByQuery(secondaryOutplacementQuery);

        try {
            while (iter.hasNext()) {
                StudentSchool item = (StudentSchool) iter.next();
                m_secondaryOutplacementSchoolMap.put(item.getStudentOid(),
                        (String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool));
            }
        } finally {
            iter.close();
        }

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("HOMELESS-STD-RCDTS", new RetrieveRCDTS());
        calcs.put("HOMELESS-STD-INFO", new RetrieveStudentInfo());
        calcs.put("HOMELESS-NIGHT-RES", new RetrievePrimaryNightResidence());
        super.addCalcs(calcs);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER
                + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
    }

    /**
     * Generate the filename for the report.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Get criteria for students that were active before the report date.
     *
     * @return criteria for students that were active before report date
     */
    private Criteria getReportingCriteria() {
        List<String> enrollTypes = new ArrayList<String>();
        enrollTypes.add(StudentEnrollment.WITHDRAWAL);
        enrollTypes.add(StudentEnrollment.ENTRY);

        PlainDate startDate = getOrganization().getCurrentContext().getStartDate();

        // With Enrollment records within the active date range and of the type E,W.
        X2Criteria activityCriteria = new X2Criteria();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollTypes);

        // Look up school or organization level enrollment records.
        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activityCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT
                    + PATH_DELIMITER + m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        // build the query for students to report.
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);

        if (isSchoolContext()) {
            activeCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activeCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            activeCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activeCriteria.addNotEqualTo(m_excludeStdField, BooleanAsStringConverter.TRUE);
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addOrCriteria(activeCriteria);
        reportingCriteria.addOrCriteria(enrollCriteria);

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all programs that should be included in the export.
     *
     * @return Criteria
     */
    private SubQuery getStudentSubQuery() {

        /*
         * Get all homeless form instances
         */
        X2Criteria homelessCriteria = new X2Criteria();
        homelessCriteria.addEqualTo(FormInstance.REL_FORM_DEFINITION + PATH_DELIMITER + FormDefinition.COL_ID,
                "HOMELESS");
        SubQuery homelessStudentSubQuery =
                new SubQuery(FormInstance.class, FormInstance.COL_OWNER_OBJECT_OID, homelessCriteria);

        /*
         * Get all students who have a Homeless form instance
         */
        Criteria studentCriteria = getReportingCriteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, homelessStudentSubQuery);

        // Check student selection criteria user input
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                studentCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG,
                        queryString);
                break;

            case 2: // LASID
                studentCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                studentCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
    }
}
