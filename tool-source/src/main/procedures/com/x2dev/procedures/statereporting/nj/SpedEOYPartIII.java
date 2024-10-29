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
import com.follett.fsc.core.k12.beans.Race;
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
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedEOYPartIII.
 */
public class SpedEOYPartIII extends StateReportData {

    /**
     * The Class SpedEOYPartIIIEntity.
     */
    public static class SpedEOYPartIIIEntity extends StateReportEntity {
        private Iterator m_spedExitSummaryIterator;
        private SpedEOYPartIII m_spedEOYData;
        private Map m_currentSpedMap;
        private boolean m_hasPrintedHeader = false;

        /**
         * Instantiates a new sped EOY part III entity.
         */
        public SpedEOYPartIIIEntity() {
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
                name = "Basis of Exit";

                // set m_currentMap to have heading values
                m_currentSpedMap = new HashMap<String, Object>();
                m_currentSpedMap.put("Native American M", "Native American Male");
                m_currentSpedMap.put("Native American F", "Native American Female");
                m_currentSpedMap.put("Asian M", "Asian Male");
                m_currentSpedMap.put("Asian F", "Asian Female");
                m_currentSpedMap.put("Pacific Islander M", "Pacific Islander Male");
                m_currentSpedMap.put("Pacific Islander F", "Pacific Islander Female");
                m_currentSpedMap.put("Black M", "Black Male");
                m_currentSpedMap.put("Black F", "Black Female");
                m_currentSpedMap.put(m_spedEOYData.CODE_RACE_HISPANIC + " M",
                        m_spedEOYData.CODE_RACE_HISPANIC + " Male");
                m_currentSpedMap.put(m_spedEOYData.CODE_RACE_HISPANIC + " F",
                        m_spedEOYData.CODE_RACE_HISPANIC + " Female");
                m_currentSpedMap.put("White M", "White Male");
                m_currentSpedMap.put("White F", "White Female");
                m_currentSpedMap.put(m_spedEOYData.CODE_RACE_MULTI + " M", m_spedEOYData.CODE_RACE_MULTI + " Male");
                m_currentSpedMap.put(m_spedEOYData.CODE_RACE_MULTI + " F", m_spedEOYData.CODE_RACE_MULTI + " Female");
                m_currentSpedMap.put(m_spedEOYData.CODE_TOTAL_RACE, m_spedEOYData.CODE_TOTAL_RACE);
                m_currentSpedMap.put(m_spedEOYData.CODE_LEP_PROGRAM + Boolean.TRUE, "LEP Yes");
                m_currentSpedMap.put(m_spedEOYData.CODE_LEP_PROGRAM + Boolean.FALSE, "LEP No");
                m_currentSpedMap.put(m_spedEOYData.CODE_TOTAL_LEP, m_spedEOYData.CODE_TOTAL_LEP);

                m_hasPrintedHeader = true;
            } else if (m_spedExitSummaryIterator != null && m_spedExitSummaryIterator.hasNext()) {
                Map.Entry pairs = (Map.Entry) m_spedExitSummaryIterator.next();
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

            m_spedEOYData = (SpedEOYPartIII) data;

            // initialize iterators
            m_spedExitSummaryIterator = m_spedEOYData.m_spedExitSummary.entrySet().iterator();

            setRowCount(20);
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
            SpedEOYPartIIIEntity spedEOYPartIIIEntity = (SpedEOYPartIIIEntity) entity;

            if ("name".equals(param)) {
                value = spedEOYPartIIIEntity.getRowName();
            } else {
                value = spedEOYPartIIIEntity.getRowValue(param);
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
    protected final String CODE_BLANK = " ";
    protected final String CODE_LEP_PROGRAM = "LEP";
    protected final String CODE_RACE_HISPANIC = "Hispanic or Latino";
    protected final String CODE_RACE_MULTI = "Two or More Races";
    protected final String CODE_TOTAL_LEP = "LEP Total";
    protected final String CODE_TOTAL_RACE = "Race Total";
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
    protected Map<String, Map<String, Integer>> m_spedExitSummary;
    protected StudentHistoryHelper m_helper;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        // initialize fields
        m_fieldResidingSchool = translateAliasToJavaName(ALIAS_RESIDENT_SCHOOL, true);

        // initialize maps
        m_spedExitSummary = new HashMap<String, Map<String, Integer>>();

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

        calculateSpedExitSummary(iepExitedIterator);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
            QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

            setQuery(query);
            setEntityClass(SpedEOYPartIIIEntity.class);

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
    private void calculateSpedExitSummary(QueryIterator iepExitedIterator) {
        while (iepExitedIterator.hasNext()) {
            IepData currentExitedIep = (IepData) iepExitedIterator.next();
            SisStudent student = currentExitedIep.getStudent();

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

                            if (!StringUtils.isEmpty(disabilityCode)) {
                                // get student's basis of exit
                                String exitReason = currentExitedIep.getExitReason();
                                setExitSummary(student, exitReason);
                            }
                        }
                    } else {
                        // AppGlobals.getLog().log(Level.SEVERE, "Student : " +
                        // student.getNameView());
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

    /**
     * Get a Person's Race.
     *
     * @param person SisPerson
     * @return String
     */
    private String getRace(SisPerson person) {
        String race = null;

        if (person != null) {
            // If the student is hispanic, this should be the only race considered
            if (person.getHispanicLatinoIndicator()) {
                race = CODE_RACE_HISPANIC;
            }

            // Otherwise consider the student's race(s)
            else {
                ArrayList<Race> raceCollection = new ArrayList<Race>(person.getRaces());
                if (!raceCollection.isEmpty()) {
                    // If the student is more than one race mark as MULTI_RACE
                    if (raceCollection.size() > 1) {
                        race = CODE_RACE_MULTI;
                    }
                    // Otherwise record the state value of their race.
                    // Note these values should match the race values defined in
                    // StudentRegister.class
                    else {
                        race = raceCollection.get(0).getRaceCode();
                    }
                }
            }
        }

        return race;
    }

    /**
     * Determine if the Student participates in LEP Program .
     *
     * @param student SisStudent
     * @return Boolean
     */
    private Boolean isLep(SisStudent student) {
        Collection<StudentProgramParticipation> programs = student.getProgramParticipation();
        for (StudentProgramParticipation program : programs) {
            if (program.getEndDate() == null ||
                    !m_districtSchoolStartDate.after(program.getEndDate())) {
                String programCode = program.getProgramCode();
                if (!StringUtils.isEmpty(programCode)) {
                    programCode = lookupStateValue(StudentProgramParticipation.class, "programCode", programCode);
                    if (CODE_LEP_PROGRAM.equals(programCode)) {
                        return Boolean.TRUE;
                    }
                }
            }
        }
        return Boolean.FALSE;
    }

    /**
     * Set Exit Summary.
     *
     * @param student SisStudent
     * @param exitReason String
     */
    private void setExitSummary(SisStudent student, String exitReason) {
        if (student.getPerson() != null) {
            SisPerson person = student.getPerson();
            Boolean isLep = isLep(student);
            String race = getRace(person);
            String gender = person.getGenderCode();

            Map<String, Integer> raceCountMap = new TreeMap<String, Integer>();
            if (m_spedExitSummary.containsKey(exitReason)) {
                raceCountMap = m_spedExitSummary.get(exitReason);
            }

            // update student race counts and total race count
            int studentCount = 0;
            int totalStudentCount = 0;
            if (raceCountMap.containsKey(race + CODE_BLANK + gender)) {
                studentCount = raceCountMap.get(race + CODE_BLANK + gender).intValue();
            }
            if (raceCountMap.containsKey(CODE_TOTAL_RACE)) {
                totalStudentCount = raceCountMap.get(CODE_TOTAL_RACE).intValue();
            }
            raceCountMap.put(race + CODE_BLANK + gender, Integer.valueOf(++studentCount));
            raceCountMap.put(CODE_TOTAL_RACE, Integer.valueOf(++totalStudentCount));

            // update student lep counts and total lep count
            int lepCount = 0;
            int lepTotalCount = 0;
            if (raceCountMap.containsKey(CODE_LEP_PROGRAM + isLep)) {
                lepCount = raceCountMap.get(CODE_LEP_PROGRAM + isLep).intValue();
            }
            if (raceCountMap.containsKey(CODE_TOTAL_LEP)) {
                lepTotalCount = raceCountMap.get(CODE_TOTAL_LEP).intValue();
            }
            raceCountMap.put(CODE_LEP_PROGRAM + isLep, Integer.valueOf(++lepCount));
            raceCountMap.put(CODE_TOTAL_LEP, Integer.valueOf(++lepTotalCount));

            // update sped exit summary
            m_spedExitSummary.put(exitReason, raceCountMap);
        }
    }

}
