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
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
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
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedEOYPartII.
 */
public class SpedEOYPartII extends StateReportData {

    /**
     * The Class SpedEOYPartIIEntity.
     */
    public static class SpedEOYPartIIEntity extends StateReportEntity {
        private Iterator m_spedExitDetailsIterator;
        private SpedEOYPartII m_spedEOYData;
        private Map m_currentSpedMap;
        private Iterator m_currentExitReasonIterator;

        private final String CODE_AGE_14 = "Age 14";
        private final String CODE_AGE_15 = "Age 15";
        private final String CODE_AGE_16 = "Age 16";
        private final String CODE_AGE_17 = "Age 17";
        private final String CODE_AGE_18 = "Age 18";
        private final String CODE_AGE_19 = "Age 19";
        private final String CODE_AGE_20 = "Age 20";
        private final String CODE_AGE_21 = "Age 21";

        /**
         * Instantiates a new sped EOY part II entity.
         */
        public SpedEOYPartIIEntity() {
            // no argument constructor for dynamic instantiation
        }

        /**
         * Gets the row name.
         *
         * @return String
         */
        public String getRowName() {
            String name = null;

            if (m_spedExitDetailsIterator.hasNext() &&
                    (m_currentExitReasonIterator == null || !m_currentExitReasonIterator.hasNext())) {
                // set export format to be used
                Map.Entry pairs = (Map.Entry) m_spedExitDetailsIterator.next();

                // set name to be disability value
                name = (String) pairs.getKey();

                // set m_currentExitReasonIterator
                Map<String, Map<String, Integer>> attendanceValuesMap =
                        (Map<String, Map<String, Integer>>) pairs.getValue();
                m_currentExitReasonIterator = attendanceValuesMap.entrySet().iterator();

                // set m_currentMap to have heading values
                m_currentSpedMap = new HashMap<String, Object>();
                m_currentSpedMap.put(CODE_AGE_14, CODE_AGE_14);
                m_currentSpedMap.put(CODE_AGE_15, CODE_AGE_15);
                m_currentSpedMap.put(CODE_AGE_16, CODE_AGE_16);
                m_currentSpedMap.put(CODE_AGE_17, CODE_AGE_17);
                m_currentSpedMap.put(CODE_AGE_18, CODE_AGE_18);
                m_currentSpedMap.put(CODE_AGE_19, CODE_AGE_19);
                m_currentSpedMap.put(CODE_AGE_20, CODE_AGE_20);
                m_currentSpedMap.put(CODE_AGE_21, CODE_AGE_21);
                m_currentSpedMap.put(m_spedEOYData.CODE_TOTAL_AGE, m_spedEOYData.CODE_TOTAL_AGE);
            } else if (m_currentExitReasonIterator != null && m_currentExitReasonIterator.hasNext()) {
                Map.Entry pairs = (Map.Entry) m_currentExitReasonIterator.next();
                // name = "\t" + (String) pairs.getKey();
                name = (String) pairs.getKey();
                m_currentSpedMap = (Map<String, Object>) pairs.getValue();
            }

            if (!StringUtils.isEmpty(name)) {
                name = name.replaceAll(",", "");
            }

            // do the same thing using "else if" for m_spedExitSummaryIterator
            // and m_spedGeneralDetailsIterator

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

            m_spedEOYData = (SpedEOYPartII) data;

            // initialize iterators
            m_spedExitDetailsIterator = m_spedEOYData.m_spedExitDetails.entrySet().iterator();

            setRowCount(70);
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
            SpedEOYPartIIEntity spedEOYPartIIEntity = (SpedEOYPartIIEntity) entity;

            if ("name".equals(param)) {
                value = spedEOYPartIIEntity.getRowName();
            } else {
                value = spedEOYPartIIEntity.getRowValue(param);
            }

            return value;
        }
    }

    /**
     * Aliases.
     */
    protected final String ALIAS_RESIDENT_SCHOOL = "DOE RESIDING SCHOOL";

    /**
     * Codes.
     */
    protected final String CODE_TOTAL_AGE = "Total";

    protected final Integer IEP_STATUS_CODE_ACTIVE = Integer.valueOf(1);
    protected final Integer IEP_STATUS_CODE_PREVIOUS = Integer.valueOf(2);

    /**
     * Supporting instance variables.
     */
    protected final ArrayList<String> m_districtCodes =
            new ArrayList(Arrays.asList("1040", "1160", "1510", "2300", "4350"));
    protected PlainDate m_districtSchoolEndDate;
    protected PlainDate m_districtSchoolStartDate;
    protected String m_fieldResidingSchool;
    protected PlainDate m_reportingDate;
    protected Map<String, Map<String, Object>> m_spedGeneralDetails;
    protected Map<String, Map<String, Map<String, Integer>>> m_spedExitDetails;
    protected Map<String, Map<String, Integer>> m_spedExitSummary;
    protected StudentHistoryHelper m_helper;

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        // initialize fields
        m_fieldResidingSchool = translateAliasToJavaName(ALIAS_RESIDENT_SCHOOL, true);

        // initialize maps
        m_spedExitDetails = new HashMap<String, Map<String, Map<String, Integer>>>();

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

        Criteria iepExitedCriteria = getIepExitedCriteria(m_helper.getStudentCriteria());

        QueryByCriteria iepExitedQuery = new QueryByCriteria(IepData.class, iepExitedCriteria);
        QueryIterator iepExitedIterator = getBroker().getIteratorByQuery(iepExitedQuery);
        calculateSpedExitDetails(iepExitedIterator);
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
            QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

            setQuery(query);
            setEntityClass(SpedEOYPartIIEntity.class);

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
     * @param iepExitedIterator QueryIterator
     */
    private void calculateSpedExitDetails(QueryIterator iepExitedIterator) {
        while (iepExitedIterator.hasNext()) {
            IepData currentExitedIep = (IepData) iepExitedIterator.next();
            SisStudent student = currentExitedIep.getStudent();
            SisPerson person = null;

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
                        IepDisability primaryDisability = currentExitedIep.getPrimaryDisability();
                        String disabilityCode = null;
                        if (primaryDisability != null) {
                            disabilityCode = primaryDisability.getDisabilityCode();
                        }

                        if (!StringUtils.isEmpty(disabilityCode)) {
                            // get student's basis of exit
                            String exitReason = currentExitedIep.getExitReason();

                            // get exit reason map
                            Map<String, Map<String, Integer>> exitReasonMap =
                                    new HashMap<String, Map<String, Integer>>();
                            if (m_spedExitDetails.containsKey(disabilityCode)) {
                                exitReasonMap = m_spedExitDetails.get(disabilityCode);
                            }

                            // update exit reason map if there is an exit reason
                            if (!StringUtils.isEmpty(exitReason)) {
                                // get student's age of October 15 last year
                                // default to zero, which is an invalid age.
                                int studentAge = 0;
                                if (student != null) {
                                    person = student.getPerson();
                                    if (person != null) {
                                        studentAge = person.getAgeAsOfDate(m_reportingDate);
                                    }
                                }

                                // get age map
                                Map<String, Integer> ageMap = new HashMap<String, Integer>();
                                if (exitReasonMap.containsKey(exitReason)) {
                                    ageMap = exitReasonMap.get(exitReason);
                                }

                                // update age count
                                int studentCount = 0;
                                String studentAgeString = "Age " + (Integer.valueOf(studentAge)).toString();
                                int totalStudentCount = 0;
                                if (ageMap.containsKey(studentAgeString)) {
                                    studentCount = ageMap.get(studentAgeString).intValue();
                                }
                                if (ageMap.containsKey(CODE_TOTAL_AGE)) {
                                    totalStudentCount = ageMap.get(CODE_TOTAL_AGE).intValue();
                                }

                                // update maps associated with exit summary
                                ageMap.put(studentAgeString, Integer.valueOf(++studentCount));
                                ageMap.put(CODE_TOTAL_AGE, Integer.valueOf(++totalStudentCount));
                                exitReasonMap.put(exitReason, ageMap);
                                m_spedExitDetails.put(disabilityCode, exitReasonMap);
                            }

                        }
                    }
                }
            }
        }
    }

    /**
     * Criteria for students who have exited sped and were active this year.
     *
     * @param studentCriteria X2Criteria
     * @return Criteria
     */
    private Criteria getIepExitedCriteria(X2Criteria studentCriteria) {
        X2Criteria iepCriteria = new X2Criteria();
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        iepCriteria.addEqualTo(IepData.COL_STATUS_CODE, IEP_STATUS_CODE_PREVIOUS);

        // include iep if exit date was during this year
        iepCriteria.addGreaterOrEqualThan(IepData.COL_EXIT_DATE, m_districtSchoolStartDate);
        iepCriteria.addLessOrEqualThan(IepData.COL_EXIT_DATE, m_districtSchoolEndDate);
        iepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);

        return iepCriteria;
    }

}
