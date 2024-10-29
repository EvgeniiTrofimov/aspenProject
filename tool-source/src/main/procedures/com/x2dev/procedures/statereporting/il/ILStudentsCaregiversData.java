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
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export data module for IL Students Caregivers export.
 */
public class ILStudentsCaregiversData extends StateReportData {

    /**
     * The Class ILStudentsCaregiversEntity.
     */
    public static class ILStudentsCaregiversEntity extends StateReportEntity {
        /**
         * ILStudentsCaregiversData data.
         */
        ILStudentsCaregiversData m_ilData = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        private List<StudentContact> m_stdCaregivers = null;

        /**
         * The effective Entry student enrollment record for report date.
         */
        private StudentEnrollment m_enrollment = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ILStudentsCaregiversEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current caregiver.
         *
         * @return Student contact
         */
        public StudentContact getCurrentCaregiver() {
            return m_stdCaregivers.get(getCurrentRow());
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if (m_enrollment != null && m_enrollment.getSchool() != null) {
                school = (String) m_enrollment.getSchool().getFieldValueByBeanPath(m_ilData.m_fieldSchoolCode);
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
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_ilData = (ILStudentsCaregiversData) data;
            SisStudent student = (SisStudent) getBean();

            m_enrollment = m_ilData.m_helper.getEnrollmentForDate(student.getOid(),
                    m_ilData.m_reportDate, "E");
            m_stdCaregivers = m_ilData.m_caregivers.get(student.getOid());
            if ((m_stdCaregivers != null) && (m_stdCaregivers.size() > 0) &&
                    (m_enrollment != null && m_enrollment.getSchool() != null &&
                            !BooleanAsStringConverter.TRUE.equals(m_enrollment.getSchool()
                                    .getFieldValueByBeanPath(m_ilData.m_excludeSklField)))) {
                m_rcdtsMap = lookupOverrides();
                setRowCount(m_stdCaregivers.size());
            } else {
                setRowCount(0);
            }
        }


        /**
         * Lookup district and school codes, including overrides on the enrollment record.
         *
         * @return Map
         */
        private Map<String, String> lookupOverrides() {
            Map<String, String> calcValueMap = new HashMap<String, String>();
            ILStudentsCaregiversData scData = (ILStudentsCaregiversData) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (scData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = scData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            scData.lookupStateValue(StudentEnrollment.class, scData.m_fieldServiceSchoolCode,
                                    serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
        }
    }

    /**
     * Retriever for caregiver specific information.
     */
    protected class RetrieveCaregiver implements FieldRetriever {
        private final String PARAM_SID = "SID";
        private final String PARAM_LAST_NAME = "LAST_NAME";
        private final String PARAM_FIRST_NAME = "FIRST_NAME";
        private final String PARAM_DOB = "DOB";
        private final String PARAM_RELATION = "RELATION";
        private final String PARAM_RACE = "RACE";
        private final String PARAM_EDUCATION = "EDUCATION";
        private final String PARAM_EMPLOYMENT = "EMPLOYMENT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = EMPTY_STRING;
            String parameter = (String) field.getParameter();
            ILStudentsCaregiversEntity ilEntity = (ILStudentsCaregiversEntity) entity;
            StudentContact caregiver = ilEntity.getCurrentCaregiver();
            if (PARAM_SID.equals(parameter)) {
                value = caregiver.getContact().getFieldValueByAlias(ALIAS_CAREGIVER_SID);
            } else if (PARAM_LAST_NAME.equals(parameter)) {
                value = caregiver.getPerson().getLastName();
            } else if (PARAM_FIRST_NAME.equals(parameter)) {
                value = caregiver.getPerson().getFirstName();
            } else if (PARAM_DOB.equals(parameter)) {
                value = caregiver.getPerson().getDob();
            } else if (PARAM_RELATION.equals(parameter)) {
                String code = caregiver.getRelationshipCode();
                value = data.lookupStateValue(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE, code);
            } else if (PARAM_RACE.equals(parameter)) {
                value = getRaceCode(caregiver.getPerson());
            } else if (PARAM_EDUCATION.equals(parameter)) {
                value = caregiver.getContact().getFieldValueByAlias(ALIAS_CAREGIVER_EDUCATION);
            } else if (PARAM_EMPLOYMENT.equals(parameter)) {
                value = caregiver.getContact().getFieldValueByAlias(ALIAS_CAREGIVER_EMPLOYMENT);
            }

            return value;
        }

        /**
         * Algoritm of retrieving Race code was taken from StudentDemographics.java
         *
         * @param person Person
         * @return String
         */
        private String getRaceCode(Person person) {
            String raceCode = "";
            if ((person != null) && person.getHispanicLatinoIndicator()) {
                raceCode = "11";
            } else {
                Collection<Race> races = person.getRaces();
                if (races != null) {
                    if (races.size() > 1) {
                        raceCode = "17";
                    } else {
                        for (Race race : races) {
                            if (m_raceCodes.containsKey(race.getRaceCode())) {
                                ReferenceCode refCode = m_raceCodes.get(race.getRaceCode());
                                raceCode = refCode.getStateCode() != null ? refCode.getStateCode() : "";
                            }
                        }
                    }
                }
            }

            return raceCode;
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
    protected class RetrieveRcdts implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            ILStudentsCaregiversEntity scEntity = (ILStudentsCaregiversEntity) entity;
            String rcdts = null;
            if (param.equals("H") && scEntity.getEffectiveEnrollment() != null
                    && scEntity.getEffectiveEnrollment().getSchool() != null) {
                ILStudentsCaregiversData sdData = (ILStudentsCaregiversData) data;
                rcdts = (String) scEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
            } else if (param.equals("S")) {
                rcdts = scEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = scEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_CAREGIVER = "DOE CAREGIVER";
    protected static final String ALIAS_CAREGIVER_EDUCATION = "DOE CAREGIVER EDUCATION";
    protected static final String ALIAS_CAREGIVER_EMPLOYMENT = "DOE CAREGIVER EMPLOYMENT";
    protected static final String ALIAS_CAREGIVER_SID = "DOE CAREGIVER SID";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_STATE_COURSE_ID = "DOE STATE COURSE ID";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";

    protected static final String PARAM_REPORT_DATE = "reportDate";

    protected String m_excludeSklField;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldCovergiever;
    protected String m_fieldRcdtsForServingSchool;

    /**
     * Helper class:
     * For student selection by enrollment.
     * For Student schedule span.
     */
    protected StudentHistoryHelper m_helper;
    protected StudentHistoryHelper m_helperSched;

    protected PlainDate m_reportDate;
    /**
     * A map of reference codes for race codes, for use in the race code retriever.
     */
    protected Map<String, ReferenceCode> m_raceCodes;
    protected Map<String, List<StudentContact>> m_caregivers;

    /**
     * A map of reference codes for RCDTS serving school codes, for use in RCDTS retriever
     */
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        setEntityClass(ILStudentsCaregiversEntity.class);

        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        if (getSetupErrors().size() == 0) {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            applyInputCriteria(m_helper.getStudentCriteria(), true, null);

            SubQuery subQuery =
                    new SubQuery(StudentContact.class, StudentContact.COL_STUDENT_OID, getCaregiversCriteria());
            m_helper.getStudentCriteria().addIn(X2BaseBean.COL_OID, subQuery);
            setQuery(m_helper.getStudentQuery(false));

            QueryByCriteria query = new QueryByCriteria(StudentContact.class, getCaregiversCriteria());
            m_caregivers = getBroker().getGroupedCollectionByQuery(query, StudentContact.COL_STUDENT_OID, 50);

            // Get race code reference codes for use in the race retriever.
            DataDictionaryField raceCodeField = getDataDictionaryField(Race.class, Race.COL_RACE_CODE);
            if ((raceCodeField != null) && !StringUtils.isEmpty(raceCodeField.getReferenceTableOid())) {
                m_raceCodes = getReferenceCodes(raceCodeField.getReferenceTableOid());
            }

            // Additional rule for secondary OUTPLACEMENT school
            X2Criteria secondaryOutplacementCriteria = new X2Criteria();
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
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

            // Build a map of calculations/retrievers.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-RCDTS", new RetrieveRcdts());
            calcs.put("STD-CAREGIVER", new RetrieveCaregiver());
            super.addCalcs(calcs);
        }

    }

    /**
     * Make caregivers criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getCaregiversCriteria() {
        X2Criteria criteria = new X2Criteria();
        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_CAREGIVER);
        String fieldName = field.getDatabaseName();
        if (fieldName != null) {
            criteria.addEqualTo(fieldName, BooleanAsStringConverter.TRUE);
        }
        return criteria;

    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);

        // Needed to avoid NullPointerException when field.getDatabaseName() is called.
        m_fieldCovergiever = translateAliasToJavaName(ALIAS_CAREGIVER, true);

        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);

        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
    }
}
