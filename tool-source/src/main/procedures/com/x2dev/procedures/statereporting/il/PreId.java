/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
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
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;



/**
 * Illinois state report for Pre-ID labels export.
 * This class implements the data export for SAT Pre-ID labels export.
 *
 * @author X2 Development Corporation
 */
public class PreId extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the SAT Pre-ID labels export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */

    public static class PreIdEntity extends StateReportEntity {
        /*
         * Member Variables
         */
        private Map<String, Object> m_enrollmentValues = new HashMap<String, Object>();

        boolean m_excludedSchool = false;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public PreIdEntity() {
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
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";
            return name;
        }


        /**
         * Initialize.
         * If there are enrollments, setHomeServiceSchool() and setMayData() records.
         * Whether there are enrollments or not setTbeTpiData()
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports#initialize()
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisStudent student = (SisStudent) bean;

            PreId preidData = (PreId) data;
            List<StudentEnrollment> enrollments = preidData.m_enrollmentMap.get(student.getOid());
            preidData.m_totalEnrollmentCount++;

            if (enrollments != null) {
                setSchoolDistrict(enrollments, data);

                if (m_excludedSchool) {
                    setRowCount(0);
                    return;
                }

                setMayData(enrollments, data);
            } else {
                setRowCount(0);
            }

            preidData.m_numOfRecords = preidData.m_numOfRecords + getRowCount();
        }

        /**
         * Returns values from the m_enrollmentValues, given a key.
         *
         * @param key String
         * @return Object
         */
        public Object getEnrollmentValues(String key) {
            return m_enrollmentValues.get(key);
        }

        /**
         * Set "DOE IN HOME DISTRICT MAY 1" and "DOE IN HOME SCHOOL MAY 1" in
         * m_enrollmentValues.
         *
         * For each enrollment record of a student, if the record is before or on
         * the first day of may of the current year and the record is not listed as a
         * Withdrawal, then the home and serving school should be compared to see
         * if they are the same. Also the home and serving district should be compared
         * to see if they are the same.
         *
         *
         * @param enrollments List<StudentEnrollment>
         * @param data StateReportData
         */
        private void setMayData(List<StudentEnrollment> enrollments, StateReportData data) {
            PreId preid = (PreId) getData();
            boolean inHomeSchoolMay1 = false;
            boolean inHomeDistrictMay1 = false;

            PlainDate mayDate = data.getOrganization().getCurrentContext().getStartDate();
            Calendar cal = Calendar.getInstance();
            cal.setTime(mayDate);
            cal.set(Calendar.MONTH, Calendar.MAY);
            cal.set(Calendar.DAY_OF_MONTH, 1);
            mayDate = new PlainDate(cal.getTimeInMillis());

            for (StudentEnrollment enrollment : enrollments) {
                // see if date is before May 1 of previous year.
                if (!mayDate.before(enrollment.getEnrollmentDate())) {
                    // use most recent enrollment record, which will be first
                    if (!StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                        String homeSchool = null;
                        if (enrollment.getSchool() != null) {
                            homeSchool = (String) enrollment.getSchool().getFieldValueByAlias(PreId.ALIAS_SCHOOL_ID);
                        }

                        String serviceSchoolCode = null;
                        if (preid.m_secondaryOutplacementSchoolMap.containsKey(enrollment.getStudentOid())) {
                            String serviceSchoolRefCode =
                                    preid.m_secondaryOutplacementSchoolMap.get(enrollment.getStudentOid());
                            if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                                serviceSchoolCode =
                                        preid.lookupStateValue(StudentEnrollment.class, preid.m_fieldSchoolService,
                                                serviceSchoolRefCode);
                            }
                        } else if (enrollment.getSchool() != null) {
                            serviceSchoolCode =
                                    (String) enrollment.getSchool().getFieldValueByAlias(PreId.ALIAS_SCHOOL_ID);
                        }
                        if ((homeSchool == null && serviceSchoolCode == null) ||
                                (homeSchool != null && homeSchool.equals(serviceSchoolCode))) {
                            inHomeSchoolMay1 = true;
                        }

                        String homeDistrict = (String) enrollment.getFieldValueByAlias(PreId.ALIAS_DISTRICT_HOME);
                        String serviceDistrict = (String) enrollment.getFieldValueByAlias(PreId.ALIAS_DISTRICT_SERVICE);
                        if ((homeDistrict == null && serviceDistrict == null) ||
                                (homeDistrict != null && homeDistrict.equals(serviceDistrict))) {
                            inHomeDistrictMay1 = true;
                        }
                    }
                    break;
                }
            }
            m_enrollmentValues.put(PreId.ALIAS_SCHOOL_HOME_MAY, Boolean.valueOf(inHomeSchoolMay1));
            m_enrollmentValues.put(PreId.ALIAS_SCHOOL_DISTRICT_MAY, Boolean.valueOf(inHomeDistrictMay1));
        }

        /**
         * Lookup district and school codes. Put in m_enrollmentValues
         *
         *
         * @param enrollments List<StudentEnrollment>
         * @param preidData StateReportData
         */
        private void setSchoolDistrict(List<StudentEnrollment> enrollments, StateReportData preidData) {
            for (StudentEnrollment enrollment : enrollments) {
                m_excludedSchool = false;
                if (enrollment.getSchool() != null &&
                        BooleanAsStringConverter.TRUE.equals(enrollment.getSchool()
                                .getFieldValueByBeanPath(((PreId) preidData).m_excludeSklField))) {
                    m_excludedSchool = true;
                }
                if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                    // Map<String, Object> overrideValues = lookupOverrides(enrollment);
                    // m_enrollmentValues.putAll(overrideValues);

                    PreId preid = (PreId) getData();

                    // Get override school codes if they exist.
                    String homeDistrictCode = (String) enrollment.getFieldValueByAlias(PreId.ALIAS_DISTRICT_HOME);
                    String serviceDistrictCode = (String) enrollment.getFieldValueByAlias(PreId.ALIAS_DISTRICT_SERVICE);
                    String homeSchoolCode = null;
                    if (enrollment.getSchool() != null) {
                        homeSchoolCode = (String) enrollment.getSchool().getFieldValueByAlias(PreId.ALIAS_SCHOOL_ID);
                    }



                    // Apply default value to school and district codes.
                    if (StringUtils.isEmpty(homeDistrictCode)) {
                        homeDistrictCode =
                                (String) preid.getOrganization().getFieldValueByAlias(PreId.ALIAS_DISTRICT_ID);
                    }

                    if (StringUtils.isEmpty(serviceDistrictCode)) {
                        serviceDistrictCode =
                                (String) preid.getOrganization().getFieldValueByAlias(PreId.ALIAS_DISTRICT_ID);
                    }



                    String serviceSchoolCode = null;
                    if (preid.m_secondaryOutplacementSchoolMap.containsKey(enrollment.getStudentOid())) {
                        String serviceSchoolRefCode =
                                preid.m_secondaryOutplacementSchoolMap.get(enrollment.getStudentOid());
                        if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                            serviceSchoolCode =
                                    preid.lookupStateValue(StudentEnrollment.class, preid.m_fieldSchoolService,
                                            serviceSchoolRefCode);
                        }
                    } else if (enrollment.getSchool() != null) {
                        serviceSchoolCode = (String) enrollment.getSchool().getFieldValueByAlias(PreId.ALIAS_SCHOOL_ID);
                    }

                    // Add the final codes to the retrieval map.
                    m_enrollmentValues.put(PreId.ALIAS_DISTRICT_HOME, homeDistrictCode);
                    m_enrollmentValues.put(PreId.ALIAS_DISTRICT_SERVICE, serviceDistrictCode);
                    m_enrollmentValues.put(PreId.ALIAS_SCHOOL_HOME, homeSchoolCode);
                    m_enrollmentValues.put(PreId.ALIAS_SCHOOL_SERVICE, serviceSchoolCode);

                    break;
                }
            }
        }
    }

    /**
     * Class RetrieveEnrollment is the FieldRetriever for interacting with the m_enrollmentValues.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollment implements FieldRetriever {

        /**
         * Returns the value from the m_enrollmentValues using the given key or
         * field.getParameter()
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            PreIdEntity ent = (PreIdEntity) entity;
            return ent.getEnrollmentValues((String) field.getParameter());
        }
    }

    /**
     * Class RetrieveFirstYearUS is the FieldRetriever for returning first year in US.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFirstYearUS implements FieldRetriever {
        private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");

        /**
         * Returns whether this was the student's first year in the US or not.
         *
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Boolean firstYearUS = Boolean.FALSE;
            PreIdEntity ent = (PreIdEntity) entity;
            String entryDateStr = (String) getProperty(ent.getBean(), field.getBeanPath());
            if (!StringUtils.isEmpty(entryDateStr)) {
                try {

                    // Find current year, subtract one year from it, to yield "oneYearAgo"
                    Calendar oneYearAgo = Calendar.getInstance();
                    oneYearAgo.add(Calendar.YEAR, -1);

                    // Convert the entryDateStr into a calendar instance
                    Calendar entryDate = Calendar.getInstance();
                    entryDate.setTimeInMillis(dateFormat.parse(entryDateStr).getTime());

                    if (entryDate.after(oneYearAgo)) {
                        firstYearUS = Boolean.TRUE;
                    }
                } catch (ParseException pe) {
                    // Do nothing, the date is invalid, treat as null.
                }
            }
            return firstYearUS;
        }
    }


    /**
     * Class to return specified override field.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveOverride implements FieldRetriever {
        private static final String PARAM_TESTING_SCHOOL = "testingSchool";

        /**
         * Returns specified override field
         *
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            Object value = data.getProperty(entity.getBean(), field.getBeanPath());

            PreIdEntity piEntity = (PreIdEntity) entity;

            if (StringUtils.isEmpty((String) value)) {
                if (PARAM_TESTING_SCHOOL.equals(param)) {
                    value = piEntity.getEnrollmentValues(ALIAS_SCHOOL_HOME);
                } else {
                    value = getParameter(param);
                }
            }

            return value;
        }
    }

    /*
     * Aliases
     */

    protected static final String ALIAS_DISTRICT_HOME = "DOE DISTRICT HOME";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DISTRICT_SERVICE = "DOE DISTRICT SERVICE";
    protected static final String ALIAS_ELL_TYPE = "DOE ELL TYPE";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_DISTRICT_MAY = "DOE IN HOME DISTRICT MAY 1";
    protected static final String ALIAS_SCHOOL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_SCHOOL_HOME_MAY = "DOE IN HOME SCHOOL MAY 1";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SCHOOL_SERVICE = "DOE SCHOOL SERVICE";


    /*
     * Parameters
     */
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_GRADE_LEVEL = "gradeLevel";

    /*
     * Instance variables.
     */
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_excludeSklField;
    protected int m_numOfRecords;
    protected int m_totalEnrollmentCount = 0;
    protected String m_fieldDistrictCode;
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();
    protected String m_fieldRcdtsForServingSchool;

    // TODO check if variable names are used. Remove if not.

    protected String m_grade;
    protected String m_fieldSchoolService;
    protected Map<String, List<StudentEnrollment>> m_enrollmentMap;
    protected Collection<DistrictSchoolYearContext> m_contexts;

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("SAT Pre-ID");
        heading.append(',');
        heading.append(m_numOfRecords);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(new Date()));
        heading.append(',');
        if (getOrganization() != null) {
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Job parameters.
         */
        m_grade = (String) getParameter(PARAM_GRADE_LEVEL);

        m_fieldSchoolService = translateAliasToJavaName(ALIAS_SCHOOL_SERVICE, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);

        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            Integer sort = (Integer) getParameter(PARAM_SORT);
            switch (sort == null ? 0 : sort.intValue()) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + School.COL_NAME);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 3: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(PreIdEntity.class);

            // set enrollment map
            loadEnrollments(studentCriteria);

            // Additional rule for secondary OUTPLACEMENT school
            Criteria secondaryOutplacementCriteria = new X2Criteria();
            SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, getStudentCriteria());
            secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                    "OUTPLACEMENT");
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
            secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                    getOrganization().getCurrentContextOid());
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

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PREID-FIRST_YEAR_US", new RetrieveFirstYearUS());
            calcs.put("PREID-ENROLLMENT", new RetrieveEnrollment());
            calcs.put("PREID-OVERRIDE", new RetrieveOverride());
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
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * This method looks up the reference table for student grade level.
     * Then returns all reference code values that match the state code values passed.
     * 
     * @param gradeStateCodes array of state code values.
     *
     * @return List of grade level reference code values.
     *         A null indicates the reference table was not found.
     */
    private ArrayList<String> getGradeCodesForGrades(List<String> gradeStateCodes) {
        ArrayList<String> refCodes = null;

        // Get maps of grade level codes by state code.
        String gradeLevelRefTableOid = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField gradeLevelField =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);
        if (gradeLevelField != null) {
            gradeLevelRefTableOid = gradeLevelField.getReferenceTableOid();
        } else {
            addSetupError("Grade level reference code lookup", "No grade level field found.");
        }

        if (gradeLevelRefTableOid != null) {
            Criteria gradeCriteria = new Criteria();
            gradeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, gradeLevelRefTableOid);
            gradeCriteria.addIn(ReferenceCode.COL_STATE_CODE, gradeStateCodes);
            ReportQueryByCriteria gradeQuery = new ReportQueryByCriteria(ReferenceCode.class,
                    new String[] {ReferenceCode.COL_CODE}, gradeCriteria);
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(gradeQuery);
            try {
                refCodes = new ArrayList<String>();
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    refCodes.add((String) row[0]);
                }
            } finally {
                iterator.close();
            }
            if (refCodes.size() == 0) {
                refCodes = null;
                addSetupError("Grade level reference code lookup",
                        "No reference codes for grade levels: " + gradeStateCodes.toString());
            }
        } else {
            addSetupError("Grade level reference code lookup", "No grade level reference table.");
        }

        return refCodes;
    }

    /**
     * Generate the filename for the report.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        if (getOrganization() != null) {
            fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(new Date()));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Compose and return a studentCriteria .
     *
     * @return studentCriteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria studentCriteria = new X2Criteria();

        /*
         * Active students
         */
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        studentCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        Integer queryBy = (Integer) getParameter(PARAM_QUERY_BY);
        switch (queryBy == null ? 0 : queryBy.intValue()) {
            case 1: // YOG
                studentCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                studentCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                studentCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Testing criteria by test type.
         * Generally this is by grade level.
         */
        ArrayList<String> gradeStateCodes = new ArrayList<String>();
        gradeStateCodes.add(m_grade);
        gradeStateCodes = getGradeCodesForGrades(gradeStateCodes);
        if (gradeStateCodes != null) {
            studentCriteria.addIn(SisStudent.COL_GRADE_LEVEL, gradeStateCodes);
        }

        return studentCriteria;
    }

    /**
     * Load the enrollments using the composed studentCriteria.
     *
     * @param studentCriteria Criteria
     */
    private void loadEnrollments(Criteria studentCriteria) {
        // report query - student enrollment
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria criteria = new Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentsSubQuery);
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
        m_enrollmentMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 100);

        // report query - district school year context
        query = new QueryByCriteria(DistrictSchoolYearContext.class, null);
        m_contexts = getBroker().getCollectionByQuery(query);
    }
}
