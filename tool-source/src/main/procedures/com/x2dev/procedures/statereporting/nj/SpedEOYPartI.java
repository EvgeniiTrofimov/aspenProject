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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedEOYPartI.
 */
public class SpedEOYPartI extends StateReportData {

    /**
     * The Class SpedEOYPartIEntity.
     */
    public static class SpedEOYPartIEntity extends StateReportEntity {
        private Iterator m_spedGeneralDetailsIterator;
        private SpedEOYPartI m_spedEOYData;
        private Map m_currentSpedMap;
        private boolean m_hasPrintedHeader;

        /**
         * Instantiates a new sped EOY part I entity.
         */
        public SpedEOYPartIEntity() {
            // no argument constructor for dynamic instantiation
        }

        /**
         * Gets the row name.
         *
         * @return String
         */
        public String getRowName() {
            String name = null;

            if (!m_hasPrintedHeader) {
                name = "Category";

                // set m_currentMap to have heading values
                m_currentSpedMap = new HashMap<String, Object>();
                m_currentSpedMap.put(m_spedEOYData.CODE_INITIAL_CLASSIFICATION,
                        m_spedEOYData.CODE_INITIAL_CLASSIFICATION);
                m_currentSpedMap.put(m_spedEOYData.CODE_REEVALUATION, m_spedEOYData.CODE_REEVALUATION);
                m_currentSpedMap.put(m_spedEOYData.CODE_RETURN_TO_GENERAL_EDUCATION,
                        m_spedEOYData.CODE_RETURN_TO_GENERAL_EDUCATION);
                m_currentSpedMap.put(m_spedEOYData.CODE_HOME_INSTRUCTION, m_spedEOYData.CODE_HOME_INSTRUCTION);

                m_hasPrintedHeader = true;
            } else if (m_spedGeneralDetailsIterator != null && m_spedGeneralDetailsIterator.hasNext()) {
                Map.Entry pairs = (Map.Entry) m_spedGeneralDetailsIterator.next();
                name = (String) pairs.getKey();
                m_currentSpedMap = (Map<String, Object>) pairs.getValue();
            }

            if (!StringUtils.isEmpty(name)) {
                name = name.replaceAll(",", "");
            }

            return name;
        }

        /**
         * Gets the row value.
         *
         * @param key String
         * @return Object
         */
        public Object getRowValue(String key) {
            Object value = null;
            if (m_currentSpedMap != null && m_currentSpedMap.containsKey(key)) {
                value = m_currentSpedMap.get(key);
            }

            return value;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_spedEOYData = (SpedEOYPartI) data;
            m_hasPrintedHeader = false;

            // initialize iterators
            m_spedGeneralDetailsIterator = m_spedEOYData.m_spedGeneralDetails.entrySet().iterator();

            int detailSize = m_spedEOYData.m_spedGeneralDetails.size() + 1;
            setRowCount(detailSize);
        }
    }

    /**
     * The Class RetrieveSpedData.
     */
    protected class RetrieveSpedData implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            SpedEOYPartIEntity spedEOYPartIEntity = (SpedEOYPartIEntity) entity;

            if ("name".equals(param)) {
                value = spedEOYPartIEntity.getRowName();
            } else {
                value = spedEOYPartIEntity.getRowValue(param);
            }

            return value;
        }
    }

    /**
     * Aliases.
     */
    protected final String ALIAS_RESIDENT_SCHOOL = "DOE RESIDING SCHOOL";
    protected final String ALIAS_IEP_CURRENT_EVAL_DATE = "iep-sped-current-eval-date";

    /**
     * Codes.
     */
    protected final String CODE_TOTAL_AGE = "Total";
    protected final String CODE_INITIAL_CLASSIFICATION = "Initial Classification";
    protected final String CODE_REEVALUATION = "Reevaluation";
    protected final String CODE_RETURN_TO_GENERAL_EDUCATION = "Return to General Education";
    protected final String CODE_HOME_INSTRUCTION = "Home Instruction";

    protected final String REF_CODE_RETURN_TO_GENERAL_EDUCATION = "Returned to General Ed";

    protected final Integer IEP_STATUS_CODE_ACTIVE = Integer.valueOf(1);
    protected final Integer IEP_STATUS_CODE_PREVIOUS = Integer.valueOf(2);

    /**
     * Supporting instance variables.
     */
    protected final ArrayList<String> m_districtCodes =
            new ArrayList(Arrays.asList("1040", "1160", "1510", "2300", "4350"));
    protected final ArrayList<String> m_notEnrolledStatuses = new ArrayList(Arrays.asList("PreReg"));
    protected PlainDate m_districtSchoolEndDate;
    protected PlainDate m_districtSchoolStartDate;
    protected String m_fieldResidingSchool;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportingDate;
    protected Map<String, Map<String, Integer>> m_spedGeneralDetails;

    protected Set<String> m_studentOidActiveIEP = null;

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        // initialize fields
        m_fieldResidingSchool = translateAliasToJavaName(ALIAS_RESIDENT_SCHOOL, true);

        // initialize maps
        m_spedGeneralDetails = new HashMap<String, Map<String, Integer>>();

        m_districtSchoolStartDate = getCurrentContext().getStartDate();
        m_districtSchoolEndDate = getCurrentContext().getEndDate();

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(m_districtSchoolStartDate);
        calendar.set(Calendar.MONTH, Calendar.OCTOBER);
        calendar.set(Calendar.DATE, 15);
        m_reportingDate = new PlainDate(calendar.getTime());

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_districtSchoolStartDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_districtSchoolEndDate);

        // Exclude students that are in PreReg status
        X2Criteria studentCritiera = m_helper.getStudentCriteria();
        studentCritiera.addNotIn(Student.COL_ENROLLMENT_STATUS, m_notEnrolledStatuses);

        m_studentOidActiveIEP = getStudentOidsWithActiveIEPs(studentCritiera);


        QueryIterator iepIterator = null;

        // get iterator containing students who given initial classification this year
        Criteria iepInitialClassificationCriteria = getIepInitialClassificationCriteria(studentCritiera);
        QueryByCriteria iepInitialClassificationQuery =
                new QueryByCriteria(IepData.class, iepInitialClassificationCriteria);
        iepIterator = getBroker().getIteratorByQuery(iepInitialClassificationQuery);
        calculateSpedGeneralDetails(iepIterator, CODE_INITIAL_CLASSIFICATION);

        // get iterator containing students who were evaluated this year
        Criteria iepReevaluationCriteria = getIepReevaluationCriteria(studentCritiera);
        QueryByCriteria iepReevaluationQuery = new QueryByCriteria(IepData.class, iepReevaluationCriteria);
        iepIterator = getBroker().getIteratorByQuery(iepReevaluationQuery);
        calculateSpedGeneralDetails(iepIterator, CODE_REEVALUATION);

        // get iterator containing students who exited sped going back to general education this
        // year
        Criteria iepReturnGeneralEducationCriteria = getIepReturnGeneralEducationCriteria(studentCritiera);
        QueryByCriteria iepReturnGeneralEducationQuery =
                new QueryByCriteria(IepData.class, iepReturnGeneralEducationCriteria);
        iepIterator = getBroker().getIteratorByQuery(iepReturnGeneralEducationQuery);
        calculateSpedGeneralDetails(iepIterator, CODE_RETURN_TO_GENERAL_EDUCATION);

        // get iterator containing students who were evaluated this year
        Criteria iepHomeInstructionCriteria = getIepHomeInstructionCriteria(studentCritiera);
        QueryByCriteria iepHomeInstructionQuery = new QueryByCriteria(IepData.class, iepHomeInstructionCriteria);
        iepIterator = getBroker().getIteratorByQuery(iepHomeInstructionQuery);
        calculateSpedGeneralDetails(iepIterator, CODE_HOME_INSTRUCTION);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
            QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

            setQuery(query);
            setEntityClass(SpedEOYPartIEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SPED-EOY-DATA", new RetrieveSpedData());

            super.addCalcs(calcs);
        }
    }

    /**
     * This method returns the district code by splitting the countyDistrictSchoolCode code.
     *
     * @param countyDistrictSchoolCode String
     * @return String
     */
    protected String getDistrictCode(String countyDistrictSchoolCode) {
        String districtCode = null;
        if (!StringUtils.isEmpty(countyDistrictSchoolCode) && countyDistrictSchoolCode.length() > 6) {
            districtCode = countyDistrictSchoolCode.substring(2, 6);
        }

        return districtCode;
    }

    /**
     * This method calculates Sped General Details.
     *
     * @param iepIterator QueryIterator
     * @param key String
     */
    private void calculateSpedGeneralDetails(QueryIterator iepIterator, String key) {
        while (iepIterator.hasNext()) {
            IepData currentIep = (IepData) iepIterator.next();

            SisStudent student = currentIep.getStudent();
            if (student != null) {
                StudentEnrollment enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_districtSchoolEndDate,
                        StudentEnrollment.ENTRY);
                if (enrollment != null) {
                    String currentCountyDistrictSchoolCode =
                            (String) enrollment.getFieldValueByBeanPath(m_fieldResidingSchool);
                    currentCountyDistrictSchoolCode =
                            lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldResidingSchool,
                                    currentCountyDistrictSchoolCode,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    String currentCountyDistrictCode = null;
                    currentCountyDistrictCode = getDistrictCode(currentCountyDistrictSchoolCode);

                    if (!StringUtils.isEmpty(currentCountyDistrictCode) &&
                            m_districtCodes.contains(currentCountyDistrictCode)) {
                        // get student's primary disability code
                        IepDisability primaryDisability = currentIep.getPrimaryDisability();
                        String disabilityCode = null;
                        if (primaryDisability != null) {
                            disabilityCode = primaryDisability.getDisabilityCode();
                        }

                        if (!StringUtils.isEmpty(disabilityCode)) {
                            // get map of counts for each disability
                            Map<String, Integer> disabilityCounts = new TreeMap<String, Integer>();
                            if (m_spedGeneralDetails.containsKey(disabilityCode)) {
                                disabilityCounts = m_spedGeneralDetails.get(disabilityCode);
                            }

                            // update student count
                            int studentCount = 0;
                            if (disabilityCounts.containsKey(key)) {
                                studentCount = disabilityCounts.get(key).intValue();
                            }
                            disabilityCounts.put(key, Integer.valueOf(++studentCount));
                            m_spedGeneralDetails.put(disabilityCode, disabilityCounts);
                        }
                    }
                }
            }
        }
    }

    /**
     * Criteria for students who received home instruction this year and had an IEP.
     *
     * @param studentCriteria X2Criteria
     * @return Criteria
     */
    private Criteria getIepHomeInstructionCriteria(X2Criteria studentCriteria) {
        X2Criteria iepCriteria = new X2Criteria();
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        iepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);

        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, IEP_STATUS_CODE_ACTIVE);

        // get all students with at least one absence of "Home Instruction"
        X2Criteria attendanceCriteria = new X2Criteria();
        attendanceCriteria.addEqualTo(StudentAttendance.COL_REASON_CODE, CODE_HOME_INSTRUCTION);
        attendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_districtSchoolStartDate);
        attendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_districtSchoolEndDate);
        SubQuery attendanceSubQuery =
                new SubQuery(StudentAttendance.class, StudentAttendance.COL_STUDENT_OID, attendanceCriteria);

        // return criteria for ieps of active students with attendance of "Home Instruction"
        iepCriteria.addIn(IepData.COL_STUDENT_OID, attendanceSubQuery);

        return iepCriteria;
    }

    /**
     * Criteria for students who received initial consent to be evaluated for sped this year
     * and were active this year.
     *
     * @param studentCriteria X2Criteria
     * @return Criteria
     */
    private Criteria getIepInitialClassificationCriteria(X2Criteria studentCriteria) {
        X2Criteria iepCriteria = new X2Criteria();
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        iepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);

        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, IEP_STATUS_CODE_ACTIVE);

        // include iep if Initial Eligibility date was during this year
        iepCriteria.addGreaterOrEqualThan(IepData.COL_INITIAL_ELIGIBILITY_DATE, m_districtSchoolStartDate);
        iepCriteria.addLessOrEqualThan(IepData.COL_INITIAL_ELIGIBILITY_DATE, m_districtSchoolEndDate);

        return iepCriteria;
    }

    /**
     * Criteria for students who received last sped evaluation this year
     * and were active this year.
     *
     * @param studentCriteria X2Criteria
     * @return Criteria
     */
    private Criteria getIepReevaluationCriteria(X2Criteria studentCriteria) {
        X2Criteria iepCriteria = new X2Criteria();
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        iepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);

        // TODO This might not be correct. We might look into this next year.
        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, IEP_STATUS_CODE_ACTIVE);

        // include iep if next evaluation date was during this year
        X2Criteria iepEvaluationCriteria = new X2Criteria();
        iepEvaluationCriteria.addGreaterOrEqualThan(IepData.COL_NEXT_EVALUATION_DATE, m_districtSchoolStartDate);
        iepEvaluationCriteria.addLessOrEqualThan(IepData.COL_NEXT_EVALUATION_DATE, m_districtSchoolEndDate);

        // also include iep if current evaluation date was during this year
        ExtendedDataDictionary extendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_IEP_CURRENT_EVAL_DATE);
        if (field != null) {
            X2Criteria iepInitialEligibilityCriteria = new X2Criteria();
            iepInitialEligibilityCriteria.addGreaterOrEqualThan(field.getJavaName(), m_districtSchoolStartDate);
            iepInitialEligibilityCriteria.addLessOrEqualThan(field.getJavaName(), m_districtSchoolEndDate);
            iepEvaluationCriteria.addOrCriteria(iepInitialEligibilityCriteria);
        }

        iepCriteria.addAndCriteria(iepEvaluationCriteria);

        return iepCriteria;
    }

    /**
     * Criteria for students who have exited sped and were active this year.
     *
     * @param studentCriteria X2Criteria
     * @return Criteria
     */
    private Criteria getIepReturnGeneralEducationCriteria(X2Criteria studentCriteria) {
        X2Criteria iepCriteria = new X2Criteria();
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        iepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);

        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, IEP_STATUS_CODE_PREVIOUS);

        // include iep if exit date was during this year
        iepCriteria.addGreaterOrEqualThan(IepData.COL_EXIT_DATE, m_districtSchoolStartDate);
        iepCriteria.addLessOrEqualThan(IepData.COL_EXIT_DATE, m_districtSchoolEndDate);
        iepCriteria.addEqualTo(IepData.COL_EXIT_REASON, REF_CODE_RETURN_TO_GENERAL_EDUCATION);

        return iepCriteria;
    }

    /**
     * Get a list of Student Oid's have have an Active IEP.
     *
     * @param studentCriteria X2Criteria
     * @return Set
     */
    private Set getStudentOidsWithActiveIEPs(X2Criteria studentCriteria) {
        X2Criteria iepCriteria = new X2Criteria();
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        iepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);
        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, IEP_STATUS_CODE_ACTIVE);

        QueryByCriteria activeIepQuery = new QueryByCriteria(IepData.class, iepCriteria);

        HashMap activeIeps = (HashMap) getBroker().getMapByQuery(activeIepQuery, IepData.COL_STUDENT_OID, 128);

        return activeIeps.keySet();
    }

}
