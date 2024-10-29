/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Washington State CEDARS
 * Student Special Education Programs file.
 *
 * @author X2 Development Corporation
 *
 */
public class WASpedSupport extends StateReportData {

    /**
     * The Class WASpedSupportEntity.
     */
    public static class WASpedSupportEntity extends StateReportEntity {
        /**
         * A map with the students home and serving districts.
         */
        Map<String, Object> m_enrollmentDistricts = new HashMap<String, Object>(4);

        /**
         * The current program record for the student.
         */
        StudentProgramParticipation m_program = null;

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
         * Return a map with home and serving districts.
         *
         * @return Map<String, String>
         */
        public Map<String, Object> getEnrollment() {
            return m_enrollmentDistricts;
        }

        /**
         * Return the student program record for this student special ed. information.
         *
         * @return StudentProgramParticipation
         */
        public StudentProgramParticipation getProgram() {
            return m_program;
        }

        /**
         * Initialize the entity.
         * Check student enrollment active status and sped status on the date.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            WASpedSupport ssData = (WASpedSupport) data;

            // Check that the student is active on report date.
            SisStudent student = (SisStudent) bean;
            StudentEnrollment enrollment =
                    ssData.m_helper.getEnrollmentForDate(student.getOid(), ssData.m_reportDate, "ESYW");
            if (enrollment == null
                    || !StudentManager.isActiveStudent(ssData.getOrganization(), enrollment.getStatusCode())) {
                setRowCount(0);
            }
            enrollment = ssData.m_helper.getEnrollmentForDate(student.getOid(), ssData.m_reportDate, "E");

            // Get enrollment for override district lookup.
            enrollment = ssData.m_helper.getEnrollmentForDate(student.getOid(), ssData.m_reportDate, "ES");
            if (enrollment == null) {
                setRowCount(0);
            }

            // Check for a program participation for special Ed.
            List<StudentProgramParticipation> programs = ssData.m_programsMap.get(student.getOid());
            if (programs == null || programs.size() == 0) {
                setRowCount(0);
            } else {
                m_program = programs.get(0);
            }

            // If the student is reportable, lookup district and add to total counts.
            if (getRowCount() > 0) {
                String districtHome = null;
                String districtHomeId = null;
                String districtServ = null;
                String districtServId = null;
                PlainDate districtEnrollmentDate = null;
                if (enrollment != null) {
                    districtHome = (String) enrollment.getFieldValueByBeanPath(ssData.m_fieldDistrictHome);
                    districtHomeId =
                            ssData.lookupReferenceCodeByBeanPath(StudentEnrollment.class, ssData.m_fieldDistrictHome,
                                    districtHome, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    districtServ = (String) enrollment.getFieldValueByBeanPath(ssData.m_fieldDistrictServe);
                    districtServId =
                            ssData.lookupReferenceCodeByBeanPath(StudentEnrollment.class, ssData.m_fieldDistrictServe,
                                    districtServ, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    districtEnrollmentDate = enrollment.getEnrollmentDate();
                }
                if (StringUtils.isEmpty(districtHomeId)) {
                    districtHome = student.getOrganization1().getName();
                    districtHomeId =
                            (String) student.getOrganization1().getFieldValueByBeanPath(ssData.m_fieldDistrictId);
                }
                if (StringUtils.isEmpty(districtServId)) {
                    districtServ = student.getOrganization1().getName();
                    districtServId =
                            (String) student.getOrganization1().getFieldValueByBeanPath(ssData.m_fieldDistrictId);
                }
                m_enrollmentDistricts.put(PARAM_HOME_NAME, districtHome);
                m_enrollmentDistricts.put(PARAM_HOME_CODE, districtHomeId);
                m_enrollmentDistricts.put(PARAM_SERV_NAME, districtServ);
                m_enrollmentDistricts.put(PARAM_SERV_CODE, districtServId);
                m_enrollmentDistricts.put(PARAM_ENTRY_DATE, districtEnrollmentDate);

                // Only count home students.
                if (ssData.m_districtId.equals(districtServId)) {
                    String gradeCode =
                            ssData.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                                    student.getGradeLevel(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    int age = student.getPerson().getAgeAsOfDate(ssData.m_ageAsOfDate);
                    int group = 0;

                    if ("PK".equals(gradeCode)) {
                        if (age < 3) {
                            group = 1;
                        } else {
                            group = 2;
                        }
                    } else {
                        group = 3;
                    }
                    if (group > 0) {
                        int[] counts = ssData.m_statistics.get(districtHomeId);
                        if (counts == null) {
                            counts = new int[3];
                            counts[group - 1] = 1;
                            ssData.m_statistics.put(districtHomeId, counts);
                        } else {
                            counts[group - 1]++;
                        }
                    }
                }
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
    }

    /**
     * Retrieve student age on report date.
     */
    protected class RetrieveAge implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Integer age = null;
            SisStudent student = (SisStudent) entity.getBean();
            if (student != null) {
                SisPerson person = student.getPerson();
                if (person != null) {
                    age = Integer.valueOf(person.getAgeAsOfDate(m_reportDate));
                }
            }
            return age;
        }
    }

    /**
     * Retrieve the students enrollment districts.
     */
    protected class RetrieveEnrollment implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            WASpedSupportEntity ssEntity = (WASpedSupportEntity) entity;
            Map<String, Object> districtMap = ssEntity.getEnrollment();

            return districtMap.get(parameter);
        }
    }

    /**
     * Retrieve the students program record fields.
     */
    protected class RetrieveProgram implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            WASpedSupportEntity ssEntity = (WASpedSupportEntity) entity;
            StudentProgramParticipation program = ssEntity.getProgram();
            String beanPath = getResolvedAliasBeanPath(parameter);
            Object value = data.getPropertyAsJavaType(program, beanPath);
            if ("[DOE DISABILITY]".equals(parameter)) {
                value = data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class, beanPath, (String) value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /*
     * Constants: Aliases, Parameters, Codes.
     */
    protected static final String ALIAS_DISTRICT_HOME = "DOE DISTRICT HOME";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DISTRICT_SERVE = "DOE DISTRICT SERVE";
    protected static final String ALIAS_EXLCUDE_PGM = "DOE EXCLUDE PGM";
    protected static final String CHAR_TAB = "\t";
    protected static final String CODE_SPED = "SPED";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_ENTRY_DATE = "ENTRY_DATE";
    protected static final String PARAM_HOME_NAME = "HOME_NAME";
    protected static final String PARAM_HOME_CODE = "HOME_CODE";
    protected static final String PARAM_SERV_NAME = "SERV_NAME";
    protected static final String PARAM_SERV_CODE = "SERV_CODE";


    /*
     * Instance variables.
     */
    protected PlainDate m_ageAsOfDate;
    protected String m_districtId;
    protected String m_excludeProgram;
    protected String m_fieldDistrictHome;
    protected String m_fieldDistrictId;
    protected String m_fieldDistrictServe;
    protected StudentHistoryHelper m_helper;
    protected Map<String, List<StudentProgramParticipation>> m_programsMap;
    protected PlainDate m_reportDate;
    protected Map<String, int[]> m_statistics = new HashMap<String, int[]>();

    /**
     * Add calculated totals to the heading to print first.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder buffer = new StringBuilder();
        buffer.append("Home District Id").append(CHAR_TAB)
                .append("0-2").append(CHAR_TAB)
                .append("3-5 PK").append(CHAR_TAB)
                .append("K-21").append(ExportJavaSource.FORMAT_EOL_WINDOWS);
        for (String districtId : m_statistics.keySet()) {
            int[] counts = m_statistics.get(districtId);
            buffer.append(districtId).append(CHAR_TAB)
                    .append(Integer.toString(counts[0])).append(CHAR_TAB)
                    .append(Integer.toString(counts[1])).append(CHAR_TAB)
                    .append(Integer.toString(counts[2])).append(ExportJavaSource.FORMAT_EOL_WINDOWS);
        }
        buffer.append(ExportJavaSource.FORMAT_EOL_WINDOWS).append(super.getHeading());

        return buffer.toString();
    }

    /**
     * Initialize the data module.
     * Initialize program map.
     * Set up field retrievers
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();

        if (getSetupErrors().size() == 0) {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

            // Add a criteria for students with SPED programs.
            X2Criteria programCriteria = getProgramCriteria();
            SubQuery programSubQuery = new SubQuery(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_STUDENT_OID, programCriteria);
            X2Criteria criteria = m_helper.getStudentCriteria();
            criteria.addIn(X2BaseBean.COL_OID, programSubQuery);

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(WASpedSupportEntity.class);

            HashMap retrievers = new HashMap<String, FieldRetriever>();
            retrievers.put("P223H-AGE", new RetrieveAge());
            retrievers.put("P223H-ENROLLMENT", new RetrieveEnrollment());
            retrievers.put("P223H-PROGRAM", new RetrieveProgram());
            super.addCalcs(retrievers);
        }
    }

    /**
     * Lookup alias fields, prepare local data.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        m_fieldDistrictId = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldDistrictHome = translateAliasToJavaName(ALIAS_DISTRICT_HOME, true);
        m_fieldDistrictServe = translateAliasToJavaName(ALIAS_DISTRICT_SERVE, true);
        m_excludeProgram = translateAliasToJavaName(ALIAS_EXLCUDE_PGM, false);

        m_districtId = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictId);

        // The Age date is SEPT 1 near the beginning of the school year.
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(0);
        calendar.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
        calendar.set(Calendar.MONTH, Calendar.SEPTEMBER);
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        m_ageAsOfDate = new PlainDate(calendar.getTimeInMillis());
    }

    /**
     * Returns the Student Programs Participation selection criteria.
     * <p>
     * The program must have a program code which contains "SPED" in
     * the local code of the program codes reference table.
     *
     * @return X2Criteria
     */
    private X2Criteria getProgramCriteria() {
        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_SPED);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }

        if (reportableCodes != null && reportableCodes.size() > 0) {
            criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

            X2Criteria criteria2 = new X2Criteria();
            X2Criteria criteria3 = new X2Criteria();
            criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
            criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                    getCurrentContext().getStartDate());
            criteria2.addOrCriteria(criteria3);
            criteria.addAndCriteria(criteria2);

            // Check school selection.
            if (isSchoolContext()) {
                criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            // Check exclude flag.
            if (!StringUtils.isEmpty(m_excludeProgram)) {
                criteria.addNotEqualTo(m_excludeProgram, BooleanAsStringConverter.TRUE);
            }

            // Apply user selection criteria.
            applyInputCriteria(criteria, false, StudentProgramParticipation.REL_STUDENT);

            // Load a map for later use.
            QueryByCriteria programsQuery = new QueryByCriteria(StudentProgramParticipation.class, criteria);
            programsQuery.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
            programsQuery.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
            m_programsMap = getBroker().getGroupedCollectionByQuery(programsQuery,
                    StudentProgramParticipation.COL_STUDENT_OID, 100);
        } else {
            addSetupError("No reportable program codes", "Student Programs, reference table, local code, SPED");
        }

        return criteria;
    }
}
