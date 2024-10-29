/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Early Childhood.
 *
 * @author X2 Development Corporation
 */
public class EarlyChildhood extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class EarlyChildhoodEntity extends StateReportEntity {
        /**
         * EarlyChildhood data.
         */
        EarlyChildhood m_ecData = null;

        /**
         * The effective Entry student enrollment record for report date.
         */
        StudentEnrollment m_enrollment = null;
        String m_schoolId = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public EarlyChildhoodEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if (m_enrollment != null && m_enrollment.getSchool() != null) {
                school = (String) m_enrollment.getSchool().getFieldValueByBeanPath(m_ecData.m_fieldSchoolCode);
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
         * Returns the SchoolId.
         *
         * @return String
         */
        public String getSchoolId() {
            return m_schoolId;
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

            m_ecData = (EarlyChildhood) data;
            SisStudent student = (SisStudent) bean;

            // Do not report students not active on report date.
            m_enrollment = m_ecData.m_helper.getEnrollmentForDate(student.getOid(), m_ecData.m_reportDate, "EW");

            // Based in "Report Date" the export shall include only student in Grade Level "14".
            int yog = 0;
            if (m_enrollment != null) {
                yog = m_enrollment.getYog();
                if (yog == 0) {
                    yog = student.getYog();
                }
            }

            if (m_enrollment == null ||
                    !m_ecData.m_activeCode.equals(m_enrollment.getStatusCode()) ||
                    m_ecData.getGradeLevel(student, yog) == null ||
                    !"14".equals(m_ecData.getGradeLevel(student, yog).getStateCode())) {
                setRowCount(0);
            }


            // Get effective "E"ntry enrollment record for this date.
            m_enrollment = m_ecData.m_helper.getEnrollmentForDate(student.getOid(), m_ecData.m_reportDate, "E");
            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_ecData.m_excludeSklField))) {
                m_rcdtsMap = lookupOverrides();

                // keep count of records
                m_ecData.m_totalStudentCount++;
            } else {
                setRowCount(0);
            }
            if (m_enrollment != null) {
                SisSchool school = m_enrollment.getSchool();
                if (school != null) {
                    m_schoolId = (String) school.getFieldValueByBeanPath(m_ecData.m_fieldSchoolCode);
                }
            }
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
                    ", SASID: " + student.getStateId() + "]";

            return name;
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
            EarlyChildhood ecData = (EarlyChildhood) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (ecData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = ecData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            ecData.lookupStateValue(StudentEnrollment.class, ecData.m_fieldServiceSchoolCode,
                                    serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
        }
    }

    /**
     * Retrieve the RCDTS of the student's home school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS.
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
            EarlyChildhoodEntity ecEntity = (EarlyChildhoodEntity) entity;
            String rcdts = null;
            if (param.equals("H") && ecEntity.getEffectiveEnrollment() != null
                    && ecEntity.getEffectiveEnrollment().getSchool() != null) {
                EarlyChildhood ecData = (EarlyChildhood) data;
                rcdts = (String) ecEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(ecData.m_fieldSchoolCode);
            }
            return rcdts;
        }
    }

    /**
     * Validate field, should be not empty only if appropriate fields are YES.
     *
     * @author Follett Software Company
     */
    protected class ValidationYes implements FieldValidator {

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

            String fieldParameter = (String) field.getParameter();
            List<String> checkingFields = Arrays.asList(fieldParameter.split("-"));

            if (checkingFields.contains("EI")) {
                if (!StringUtils.isEmpty(value) &&
                        !"01".equals(entity.getFieldValue("Early Intervention ("))) {
                    StateReportValidationError error = new StateReportValidationError(entity, field, "Invalid data.",
                            "Data is valid only when Early Intervention = Yes");
                    errors.add(error);
                }
            }
            if (checkingFields.contains("CFC") &&
                    !"01".equals(entity.getFieldValue("Referral by CFC"))) {
                StateReportValidationError error = new StateReportValidationError(entity, field, "Invalid data.",
                        "Data is valid only when Referral by CFC = Yes");
                errors.add(error);
            }

            return errors;
        }

    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";

    /*
     * Parameters
     */
    private static final String PARAM_REPORT_DATE = "reportDate";

    /*
     * Instance variables
     */
    protected String m_activeCode;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_excludeSklField;
    protected String m_excludeStdField;
    protected String m_fieldDistrictCode;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected Map<String, ReferenceCode> m_referenceGradeCodeMap;
    protected PlainDate m_reportDate;
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();

    /**
     * Helper class:
     * For student selection by enrollment.
     */
    protected StudentHistoryHelper m_helper;

    /**
     * Keep track of number of students
     */
    protected int m_totalStudentCount;

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        setEntityClass(EarlyChildhoodEntity.class);

        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        // Set the query to be used for student selection.
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);

        setQuery(studentQuery);

        // Additional rule for secondary OUTPLACEMENT school
        X2Criteria secondaryOutplacementCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
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

        calcs.put("ECE-RCDTS", new RetrieveRCDTS());

        super.addCalcs(calcs);

        // Build a map of validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put("VAL-YES", new ValidationYes());
        super.addValidators(validators);
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
        heading.append("Early Childhood");
        heading.append(',');
        heading.append(m_totalStudentCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        if (m_reportDate != null) {
            heading.append(m_dateFormat.format(m_reportDate));
        }
        heading.append(',');
        if (getOrganization() != null) {
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");

        return heading.toString();
    }

    /**
     * Calculate grade code based on students yog.
     *
     * @param student SisStudent
     * @param yog int
     * @return Reference code
     */
    protected ReferenceCode getGradeLevel(SisStudent student, int yog) {
        ReferenceCode gradeCode = null;
        if (yog == student.getYog()) {
            String gradeLevel = student.getGradeLevel();
            gradeCode = m_referenceGradeCodeMap.get(gradeLevel);
        } else {
            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
        }
        return gradeCode;
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        if (getOrganization() != null) {
            fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        fileName.append("_");
        if (m_reportDate != null) {
            fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        }
        fileName.append("_");
        fileName.append("001.txt");

        return fileName.toString();
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        loadGradeCodes();
    }

    /**
     * Load grade codes.
     */
    private void loadGradeCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceGradeCodeMap = referenceTable.getCodeMap();
    }
}
