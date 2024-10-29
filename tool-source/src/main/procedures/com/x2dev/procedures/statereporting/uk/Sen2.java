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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
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
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class Sen2.
 */
public class Sen2 extends StateReportData {

    /**
     * The Class Sen2Entity.
     */
    public static class Sen2Entity extends StateReportEntity {
        List<String> m_keyList;
        Sen2 m_sen2Data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public Sen2Entity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_sen2Data = (Sen2) data;
            setRowCount(m_sen2Data.m_pupilCounts.size());
            m_keyList = new ArrayList<String>(m_sen2Data.m_pupilCounts.keySet());
        }

        /**
         * Gets the row name.
         *
         * @return String
         */
        protected String getRowName() {
            return m_keyList.get(getCurrentRow());
        }

        /**
         * Gets the row pupil count.
         *
         * @return Integer
         */
        protected Integer getRowPupilCount() {
            return m_sen2Data.m_pupilCounts.get(getRowName());
        }

    }

    /**
     * The Class RetrievePupilData.
     */
    protected class RetrievePupilData implements FieldRetriever {

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
            Sen2Entity sen2Entity = (Sen2Entity) entity;

            if ("name".equals(param)) {
                value = sen2Entity.getRowName();
            } else if ("count".equals(param)) {
                value = sen2Entity.getRowPupilCount();
            }

            return value;
        }

    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_censusEndDate = (PlainDate) getParameter(CENSUS_DATE);

        m_censusStartDate = getCurrentContext().getStartDate();
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(m_censusStartDate);
        calendar.set(Calendar.MONTH, Calendar.AUGUST);
        calendar.set(Calendar.DAY_OF_MONTH, 31);
        m_censusStartDate = new PlainDate(calendar.getTime());

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_censusEndDate);
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

        m_senProvisionBeanPath = translateAliasToJavaName(ALIAS_DFE_SEN_PROVISION, true);
        m_senAltProvisionBeanPath = translateAliasToJavaName(ALIAS_DFE_AP_TYPE, true);
        m_senAltProvisionRefCodeMap = getRefCodes(StudentProgramParticipation.class, m_senAltProvisionBeanPath);
        loadSenStatementCodes();
        loadSenRefCode();
        if (m_senRefCode != null) {
            loadCurrentSenMap(m_helper.getStudentCriteria());
            loadCurrentAPMap(m_currentSenMap);
            loadFirstTimeSenMap(m_helper.getStudentCriteria());
            loadNoStatementSenMap(m_helper.getStudentCriteria());
            loadSenExitMap(m_helper.getStudentCriteria());
        } else {
            addSetupError("No reportable program codes", "Student Programs, reference table, local code: HML, SPED");
        }

        initializePupilCounts();

        for (SisStudent student : students) {
            StudentEnrollment enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_censusEndDate, "EWS");
            if (enrollment == null) {
                continue;
            }
            SisSchool school = enrollment.getSchool();
            if (school == null) {
                school = student.getSchool();
            }

            String schoolPhase = (String) school.getFieldValueByAlias(ALIAS_DFE_SCHOOL_PHASE);
            schoolPhase = lookupReferenceCodeByAlias(ALIAS_DFE_SCHOOL_PHASE, schoolPhase,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String governance = (String) school.getFieldValueByAlias(ALIAS_DFE_GOVERNANCE);
            governance = lookupReferenceCodeByAlias(ALIAS_DFE_GOVERNANCE, governance,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String eyCategory = (String) school.getFieldValueByAlias(ALIAS_DFE_EY_CATEGORY);
            eyCategory = lookupReferenceCodeByAlias(ALIAS_DFE_EY_CATEGORY, eyCategory,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String schoolType = (String) school.getFieldValueByAlias(ALIAS_DFE_SCHOOL_TYPE);
            schoolType = lookupReferenceCodeByAlias(ALIAS_DFE_SCHOOL_TYPE, schoolType,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String specialOrganisation = (String) school.getFieldValueByAlias(ALIAS_DFE_SPECICIAL_ORGANISATION);
            specialOrganisation = lookupReferenceCodeByAlias(ALIAS_DFE_SPECICIAL_ORGANISATION, specialOrganisation,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            // check that student has program and statement, otherwise don't count

            int ageOnCensusStartDate = student.getPerson().getAgeAsOfDate(m_censusStartDate);
            if (m_currentSenMap.containsKey(student.getOid()) && ageOnCensusStartDate <= 19) {
                /*
                 * Please state the number of children, as of census date, for
                 * whom the authority maintains a statement of special educational
                 * needs under the provisions of the Education Act 1996. The age
                 * breakdown refers to age as the previous August 31st.
                 */

                if (ageOnCensusStartDate < 5) {
                    add(CENSUS_FIELD_C11___a);
                } else if (5 <= ageOnCensusStartDate && ageOnCensusStartDate <= 10) {
                    add(CENSUS_FIELD_C11___b);
                } else if (11 <= ageOnCensusStartDate && ageOnCensusStartDate <= 15) {
                    add(CENSUS_FIELD_C11___c);
                } else if (16 <= ageOnCensusStartDate && ageOnCensusStartDate <= 19) {
                    add(CENSUS_FIELD_C11___d);
                }

                if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_NM_SPECIAL_SCHOOL.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // non-maintained special schools
                    add(CENSUS_FIELD_C12__if);
                }

                else if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_IND_SCHOOL.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // independent special schools
                    add(CENSUS_FIELD_C12__ig);
                }

                else if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_HOSPITAL.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // hospital schools (including foundation schools)
                    add(CENSUS_FIELD_C12__ii);
                }

                else if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_ACADEMY.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // academies (excluding special academies recorded at l below)
                    add(CENSUS_FIELD_C12__ik);
                }

                else if (CODE_NON_MAINTAINED.equals(governance)
                        && (CODE_PRIVATE.equals(eyCategory) || CODE_VOLUNTARY.equals(eyCategory))) {
                    // non-maintained early years settings in the private and voluntary sector
                    add(CENSUS_FIELD_C12__ia);
                }

                else if (!CODE_NON_MAINTAINED.equals(governance)) // TODO resource provisioned.
                {
                    // resourced provision in LA maintained mainstream schools
                    add(CENSUS_FIELD_C12__ib);
                }

                // TODO SEN units in LA maintained mainstream schools?
                /*
                 * else if (false)
                 * {
                 * add(CENSUS_FIELD_C12__ic); // TODO make default
                 * }
                 */
                else if (!CODE_SPECIAL.equals(schoolPhase)
                        && (!CODE_NON_MAINTAINED.equals(governance) || CODE_FOUNDATION.equals(governance))) {
                    // LA maintained mainstream schools (including foundation schools)
                    add(CENSUS_FIELD_C12__id);
                }

                else if (CODE_SPECIAL.equals(schoolPhase)
                        && (!CODE_NON_MAINTAINED.equals(governance) || CODE_FOUNDATION.equals(governance))) {
                    // LA maintained special schools (including foundation schools)
                    add(CENSUS_FIELD_C12__ie);
                }

                else if (CODE_INDEPENDENT.equals(governance) && !CODE_SPECIAL.equals(schoolType)) {
                    // other independent schools
                    add(CENSUS_FIELD_C12__ih);
                }

                else if (CODE_PUPIL_REFERRAL.equals(schoolPhase)) {
                    // pupil referral units
                    add(CENSUS_FIELD_C12__ij);
                }

                else if (CODE_ACADEMIES.equals(schoolType) && !CODE_SPECIAL.equals(schoolPhase)) {
                    // special academies
                    add(CENSUS_FIELD_C12__il);
                }

                else if (CODE_ACADEMIES.equals(schoolType) && m_currentSenAPMap.containsKey(student.getOid())) {
                    // AP academies
                    add(CENSUS_FIELD_C12__im);
                }

                else if (!CODE_NON_MAINTAINED.equals(governance)) // TODO free
                {
                    // mainstream free schools
                    add(CENSUS_FIELD_C12__in);
                }

                else if (CODE_SPECIAL.equals(schoolPhase)) // TODO free
                {
                    // special free schools
                    add(CENSUS_FIELD_C12__io);
                }

                else if (m_currentSenAPMap.containsKey(student.getOid())) // TODO AP, free
                {
                    // AP free schools
                    add(CENSUS_FIELD_C12__ip);
                }
            }

            if (m_firstTimeSenMap.containsKey(student.getOid()) && ageOnCensusStartDate <= 19) {
                if (ageOnCensusStartDate < 5) {
                    add(CENSUS_FIELD_C21___a);
                }

                else if (5 <= ageOnCensusStartDate && ageOnCensusStartDate <= 10) {
                    add(CENSUS_FIELD_C21___b);
                }

                else if (11 <= ageOnCensusStartDate && ageOnCensusStartDate <= 15) {
                    add(CENSUS_FIELD_C21___c);
                }

                else if (16 <= ageOnCensusStartDate && ageOnCensusStartDate <= 19) {
                    add(CENSUS_FIELD_C21___d);
                }

                if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_NM_SPECIAL_SCHOOL.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // non-maintained special schools
                    add(CENSUS_FIELD_C22__if);
                }

                else if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_IND_SCHOOL.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // independent special schools
                    add(CENSUS_FIELD_C22__ig);
                }

                else if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_HOSPITAL.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // hospital schools (including foundation schools)
                    add(CENSUS_FIELD_C22__ii);
                }

                else if (m_currentSenAPMap.containsKey(student.getOid()) &&
                        CODE_AP_ACADEMY.equals(m_currentSenAPMap.get(student.getOid()))) {
                    // academies (excluding special academies recorded at l below)
                    add(CENSUS_FIELD_C22__ik);
                }

                else if (CODE_NON_MAINTAINED.equals(governance)
                        && (CODE_PRIVATE.equals(eyCategory) || CODE_VOLUNTARY.equals(eyCategory))) {
                    // non-maintained early years settings in the private and voluntary sector
                    add(CENSUS_FIELD_C22__ia);
                }

                else if (!CODE_NON_MAINTAINED.equals(governance)) {
                    // resourced provision in LA maintained mainstream schools
                    add(CENSUS_FIELD_C22__ib);
                }

                // TODO SEN units in LA maintained mainstream schools?
                /*
                 * else if (false)
                 * {
                 * // SEN units in LA maintained mainstream schools
                 * add(CENSUS_FIELD_C22__ic);
                 * }
                 */
                else if (!CODE_SPECIAL.equals(schoolPhase)
                        && (!CODE_NON_MAINTAINED.equals(governance) || CODE_FOUNDATION.equals(governance))) {
                    // LA maintained mainstream schools (including foundation schools)
                    add(CENSUS_FIELD_C22__id);
                }

                else if (CODE_SPECIAL.equals(schoolPhase)
                        && (!CODE_NON_MAINTAINED.equals(governance) || CODE_FOUNDATION.equals(governance))) {
                    // LA maintained special schools (including foundation schools)
                    add(CENSUS_FIELD_C22__ie);
                }

                else if (CODE_INDEPENDENT.equals(governance) && !CODE_SPECIAL.equals(schoolType)) {
                    // other independent schools
                    add(CENSUS_FIELD_C22__ih);
                }

                else if (CODE_PUPIL_REFERRAL.equals(schoolPhase)) {
                    // pupil referral units
                    add(CENSUS_FIELD_C22__ij);
                }

                else if (CODE_ACADEMIES.equals(schoolType) && CODE_SPECIAL.equals(schoolPhase)) // TODO
                                                                                                // mainstream
                                                                                                // ?
                {
                    // special academies
                    add(CENSUS_FIELD_C22__il);
                }

                else if (CODE_ACADEMIES.equals(schoolType) && m_currentSenAPMap.containsKey(student.getOid())) {
                    // AP academies
                    add(CENSUS_FIELD_C22__im);
                }

                else if (!CODE_NON_MAINTAINED.equals(governance)) // TODO free
                {
                    // mainstream free schools
                    add(CENSUS_FIELD_C22__in);
                }

                else if (CODE_SPECIAL.equals(schoolPhase)) // TODO free
                {
                    // special free schools
                    add(CENSUS_FIELD_C22__io);
                }

                // TODO AP, free
                /*
                 * else if (false)
                 * {
                 * // AP free schools
                 * add(CENSUS_FIELD_C22__ip);
                 * }
                 */
            }

            if (m_noStatementSenMap.containsKey(student.getOid())) {
                add(CENSUS_FIELD_C23);
            }

            // TODO 2.4
            /*
             * if (false)
             * {
             * // TODO if transfer from mainstream schools out.
             * if (false)
             * {
             * add(CENSUS_FIELD_C24___a);
             * }
             * 
             * // TODO if transfer to mainstream schools
             * else if (false)
             * {
             * add(CENSUS_FIELD_C24___b);
             * }
             * }
             */

            if (m_senExitMap.containsKey(student.getOid()) &&
                    !m_currentSenMap.containsKey(student.getOid()) &&
                    ageOnCensusStartDate < 16) {
                // students who have exited SEN, are not currently enrolled in SEN, and are less
                // than 16 years of age.
                String senExitResult = null;
                for (StudentProgramParticipation spp : m_senExitMap.get(student.getOid())) {
                    senExitResult = (String) spp.getFieldValueByAlias(ALIAS_DFE_SEN_EXIT_RESULTS);
                    if (!StringUtils.isEmpty(senExitResult)) {
                        senExitResult = lookupReferenceCodeByAlias(ALIAS_DFE_SEN_EXIT_RESULTS, senExitResult,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                    break;
                }

                if (CODE_SEN_EXIT_TRANSFER.equals(senExitResult)) {
                    // transfered to another LA
                    add(CENSUS_FIELD_C25___a);
                }

                else if (CODE_SEN_EXIT_NO_STATEMENT.equals(senExitResult)) {
                    // special needs being met without a statement
                    add(CENSUS_FIELD_C25___b);
                }

                else {
                    // special needs being for another reason
                    add(CENSUS_FIELD_C25___c);
                }
            }

            if (m_senExitMap.containsKey(student.getOid()) &&
                    !StudentManager.isActiveStudent(getOrganization(), enrollment.getStatusCode()) &&
                    ageOnCensusStartDate >= 16) {
                // students who have exited SEN, school and are 16 years of age or older
                add(CENSUS_FIELD_C26);
            }

        }

        int totalOf11 = getCount(CENSUS_FIELD_C11___a) +
                getCount(CENSUS_FIELD_C11___b) +
                getCount(CENSUS_FIELD_C11___c) +
                getCount(CENSUS_FIELD_C11___d);
        m_pupilCounts.put(CENSUS_FIELD_C11___e, Integer.valueOf(totalOf11));

        int totalOf12i = getCount(CENSUS_FIELD_C12__ia) +
                getCount(CENSUS_FIELD_C12__ib) +
                getCount(CENSUS_FIELD_C12__ic) +
                getCount(CENSUS_FIELD_C12__id) +
                getCount(CENSUS_FIELD_C12__ie) +
                getCount(CENSUS_FIELD_C12__if) +
                getCount(CENSUS_FIELD_C12__ig) +
                getCount(CENSUS_FIELD_C12__ih) +
                getCount(CENSUS_FIELD_C12__ii) +
                getCount(CENSUS_FIELD_C12__ij) +
                getCount(CENSUS_FIELD_C12__ik) +
                getCount(CENSUS_FIELD_C12__il) +
                getCount(CENSUS_FIELD_C12__im) +
                getCount(CENSUS_FIELD_C12__in) +
                getCount(CENSUS_FIELD_C12__io) +
                getCount(CENSUS_FIELD_C12__ip);
        m_pupilCounts.put(CENSUS_FIELD_C12_ii, Integer.valueOf(totalOf12i));

        int totalOf21 = getCount(CENSUS_FIELD_C21___a) +
                getCount(CENSUS_FIELD_C21___b) +
                getCount(CENSUS_FIELD_C21___c) +
                getCount(CENSUS_FIELD_C21___d);
        m_pupilCounts.put(CENSUS_FIELD_C21___e, Integer.valueOf(totalOf21));

        int totalOf22i = getCount(CENSUS_FIELD_C22__ia) +
                getCount(CENSUS_FIELD_C22__ib) +
                getCount(CENSUS_FIELD_C22__ic) +
                getCount(CENSUS_FIELD_C22__id) +
                getCount(CENSUS_FIELD_C22__ie) +
                getCount(CENSUS_FIELD_C22__if) +
                getCount(CENSUS_FIELD_C22__ig) +
                getCount(CENSUS_FIELD_C22__ih) +
                getCount(CENSUS_FIELD_C22__ii) +
                getCount(CENSUS_FIELD_C22__ij) +
                getCount(CENSUS_FIELD_C22__ik) +
                getCount(CENSUS_FIELD_C22__il);
        m_pupilCounts.put(CENSUS_FIELD_C22_ii, Integer.valueOf(totalOf22i));

        Criteria criteria = new Criteria();
        criteria.addEqualTo(X2BaseBean.COL_OID, getOrganization().getOid());
        QueryByCriteria query = new QueryByCriteria(Organization.class, criteria);

        setQuery(query);

        setEntityClass(Sen2Entity.class);

        HashMap calcMap = new HashMap<String, FieldRetriever>();
        calcMap.put("SEN2-PUPIL-DATA", new RetrievePupilData());
        super.addCalcs(calcMap);
    }

    HashMap<String, Collection<StudentProgramParticipation>> m_firstTimeSenMap =
            new HashMap<String, Collection<StudentProgramParticipation>>();
    HashMap<String, String> m_currentSenAPMap = new HashMap<String, String>();
    HashMap<String, Collection<StudentProgramParticipation>> m_currentSenMap =
            new HashMap<String, Collection<StudentProgramParticipation>>();
    HashMap<String, Collection<StudentProgramParticipation>> m_noStatementSenMap =
            new HashMap<String, Collection<StudentProgramParticipation>>();
    HashMap<String, Collection<StudentProgramParticipation>> m_senExitMap =
            new HashMap<String, Collection<StudentProgramParticipation>>();
    TreeMap<String, Integer> m_pupilCounts = new TreeMap<String, Integer>();
    StudentHistoryHelper m_helper;
    PlainDate m_censusStartDate;
    PlainDate m_censusEndDate;
    List<String> m_senCodes = new ArrayList<String>();
    List<String> m_senNoStatementCodes = new ArrayList<String>();
    String m_senProvisionBeanPath;
    String m_senAltProvisionBeanPath;
    HashMap<String, ReferenceCode> m_schoolPhaseRefCodes = new HashMap<String, ReferenceCode>();
    HashMap<String, ReferenceCode> m_governanceRefCodes = new HashMap<String, ReferenceCode>();
    HashMap<String, ReferenceCode> m_eyCategoryRefCodes = new HashMap<String, ReferenceCode>();
    HashMap<String, ReferenceCode> m_schoolTypeRefCodes = new HashMap<String, ReferenceCode>();
    HashMap<String, ReferenceCode> m_specialOrganisationRefCodes = new HashMap<String, ReferenceCode>();
    HashMap<String, ReferenceCode> m_senAltProvisionRefCodeMap = new HashMap<String, ReferenceCode>();
    String m_senRefCode = null;

    // Field Aliases
    String ALIAS_DFE_SCHOOL_PHASE = "DFE SCHOOL PHASE";
    String ALIAS_DFE_GOVERNANCE = "DFE GOVERNANCE";
    String ALIAS_DFE_EY_CATEGORY = "DFE EY CATEGORY";
    String ALIAS_DFE_SCHOOL_TYPE = "DFE SCHOOL TYPE";
    String ALIAS_DFE_SPECICIAL_ORGANISATION = "DFE SPECIAL ORGANISATION";
    String ALIAS_DFE_SEN_PROVISION = "DFE SEN PROVISION";
    String ALIAS_DFE_AP_TYPE = "DFE AP TYPE";
    String ALIAS_DFE_SEN_EXIT_RESULTS = "DFE SEN EXIT RESULTS";

    // Codes
    String CODE_SEN_NOT_NEEDED = "N";
    String CODE_SEN_STATEMENT = "S";


    // Local code for special education
    String CODE_SPECIAL_EDUCATION = "SE";

    // School table codes
    // field DFE SCHOOL PHASE codes - state
    String CODE_PUPIL_REFERRAL = "PR";
    String CODE_SPECIAL = "SP";
    // field DFE GOVERNANCE codes - state
    String CODE_FOUNDATION = "FO";
    String CODE_INDEPENDENT = "IN";
    String CODE_NON_MAINTAINED = "NM";
    // fieldDFE EY CATEGORY codes - state
    String CODE_PRIVATE = "PRIV";
    String CODE_VOLUNTARY = "VOLY";
    // field DFE SPECIAL ORGANISATION codes - state
    String CODE_HOSPITAL_SPECIAL_SCHOOL = "H";
    // field DFE SCHOOL TYPE codes - state
    String CODE_ACADEMIES = "49";

    // Student Program Participation codes
    // Field DFE AP Type codes - state
    String CODE_AP_ACADEMY = "ACD";
    String CODE_AP_HOSPITAL = "HSP";
    String CODE_AP_IND_SCHOOL = "IND";
    String CODE_AP_NM_SPECIAL_SCHOOL = "NMS";
    String CODE_AP_NOT_A_SCHOOL = "NOT";
    // Field DFE SEN Exit Results - state
    String CODE_SEN_EXIT_TRANSFER = "XFR";
    String CODE_SEN_EXIT_NO_STATEMENT = "NST";
    String CODE_SEN_EXIT_OTHER = "OTH";

    String CENSUS_DATE = "censusDate";

    // Census Fields -- These are maintained by Tree Map, sorting them alpha-numerically.
    // Any new or changed fields can be written in their appropriate alpha-numeric order to preserve
    // this order.
    String CENSUS_FIELD_C11___a = "1.1     a - Under age 5";
    String CENSUS_FIELD_C11___b = "1.1     b - Aged 5 to 10";
    String CENSUS_FIELD_C11___c = "1.1     c - Aged 11 to 15";
    String CENSUS_FIELD_C11___d = "1.1     d - Aged 16 to 19";
    String CENSUS_FIELD_C11___e = "1.1     e - TOTAL (a+b+c+d)";
    String CENSUS_FIELD_C12__ia = "1.2   i a - non-maintained early years";
    String CENSUS_FIELD_C12__ib = "1.2   i b - resourced provision, maintained";
    String CENSUS_FIELD_C12__ic = "1.2   i c - SEN units maintained";
    String CENSUS_FIELD_C12__id = "1.2   i d - maintained mainstream";
    String CENSUS_FIELD_C12__ie = "1.2   i e - maintained special";
    String CENSUS_FIELD_C12__if = "1.2   i f - non-maintained special";
    String CENSUS_FIELD_C12__ig = "1.2   i g - independent special";
    String CENSUS_FIELD_C12__ih = "1.2   i h - other independent";
    String CENSUS_FIELD_C12__ii = "1.2   i i - hospital schools";
    String CENSUS_FIELD_C12__ij = "1.2   i j - maintained pupil referral units";
    String CENSUS_FIELD_C12__ik = "1.2   i k - mainstream academies";
    String CENSUS_FIELD_C12__il = "1.2   i l - special academies";
    String CENSUS_FIELD_C12__im = "1.2   i m - AP academies";
    String CENSUS_FIELD_C12__in = "1.2   i n - mainstream free schools";
    String CENSUS_FIELD_C12__io = "1.2   i o - special free schools";
    String CENSUS_FIELD_C12__ip = "1.2   i p - AP free schools";
    String CENSUS_FIELD_C12_ii = "1.2  ii   - Total of 1.2.i";
    String CENSUS_FIELD_C12iiia = "1.2 iii a - pupils excluded, not placed";
    String CENSUS_FIELD_C12iiib = "1.2 iii b - other section 319";
    String CENSUS_FIELD_C12iiic = "1.2 iii c - other section 7";
    String CENSUS_FIELD_C12iiid = "1.2 iii d - awaiting provision";
    String CENSUS_FIELD_C21___a = "2.1     a - Under age 5";
    String CENSUS_FIELD_C21___b = "2.1     b - Aged 5 to 10";
    String CENSUS_FIELD_C21___c = "2.1     c - Aged 11 to 15";
    String CENSUS_FIELD_C21___d = "2.1     d - Aged 16 to 19";
    String CENSUS_FIELD_C21___e = "2.1     e - TOTAL (a+b+c+d)";
    String CENSUS_FIELD_C22__ia = "2.2   i a - non-maintained early years";
    String CENSUS_FIELD_C22__ib = "2.2   i b - resourced provision, maintained";
    String CENSUS_FIELD_C22__ic = "2.2   i c - SEN units maintained";
    String CENSUS_FIELD_C22__id = "2.2   i d - maintained mainstream";
    String CENSUS_FIELD_C22__ie = "2.2   i e - maintained special";
    String CENSUS_FIELD_C22__if = "2.2   i f - non-maintained special";
    String CENSUS_FIELD_C22__ig = "2.2   i g - independent special";
    String CENSUS_FIELD_C22__ih = "2.2   i h - other independent";
    String CENSUS_FIELD_C22__ii = "2.2   i i - hospital schools";
    String CENSUS_FIELD_C22__ij = "2.2   i j - maintained pupil referral units";
    String CENSUS_FIELD_C22__ik = "2.2   i k - mainstream academies";
    String CENSUS_FIELD_C22__il = "2.2   i l - special academies";
    String CENSUS_FIELD_C22__im = "2.2   i m - AP academies";
    String CENSUS_FIELD_C22__in = "2.2   i n - mainstream free schools";
    String CENSUS_FIELD_C22__io = "2.2   i o - special free schools";
    String CENSUS_FIELD_C22__ip = "2.2   i p - AP free schools";
    String CENSUS_FIELD_C22_ii = "2.2  ii   - Total of 2.2.i";
    String CENSUS_FIELD_C22iiia = "2.2 iii a - pupils excluded, not placed";
    String CENSUS_FIELD_C22iiib = "2.2 iii b - other section 319";
    String CENSUS_FIELD_C22iiic = "2.2 iii c - other section 7";
    String CENSUS_FIELD_C22iiid = "2.2 iii d - awaiting provision";
    String CENSUS_FIELD_C23 = "2.3       - children assessed, no statement";
    String CENSUS_FIELD_C24___a = "2.4     a - transfered from mainstream";
    String CENSUS_FIELD_C24___b = "2.4     b - transfered to mainstream";
    String CENSUS_FIELD_C25___a = "2.5     a - transfered to another LA";
    String CENSUS_FIELD_C25___b = "2.5     b - special needs met without statement";
    String CENSUS_FIELD_C25___c = "2.5     c - other";
    String CENSUS_FIELD_C26 = "2.6       - aged 16 over left SEN and school";

    /**
     * Increments the pupil count indicated by the "key" of map m_pupilCounts by 1.
     *
     * @param key String
     */
    private void add(String key) {
        Integer value = m_pupilCounts.get(key);
        if (value == null) {
            value = Integer.valueOf(1);
        } else {
            value = Integer.valueOf(value.intValue() + 1);
        }
        m_pupilCounts.put(key, value);
    }

    /**
     * Returns the count of pupils from the map m_pupilCounts indicated by the "key."
     *
     * @param key String
     * @return int
     */
    private int getCount(String key) {
        Integer value = m_pupilCounts.get(key);
        int count = 0;
        if (value != null) {
            count = value.intValue();
        }
        return count;
    }

    /**
     * Return the state code found in the passed in refCodeMap corresponding to the passed in code.
     *
     * @param refCodeMap HashMap<String,ReferenceCode>
     * @param code String
     * @return String state code
     */
    private String getStateRefCode(HashMap<String, ReferenceCode> refCodeMap, String code) {
        String stateCode = code;
        if (refCodeMap != null && refCodeMap.containsKey(code)) {
            ReferenceCode refCode = refCodeMap.get(code);
            stateCode = refCode.getStateCode();
        }

        return stateCode;
    }

    /**
     * Return the reference codes for the field specified by the passed in bean path.
     *
     * @param beanClass Class
     * @param beanPath String
     * @return a hash map of reference code keyed by their codes
     */
    private HashMap<String, ReferenceCode> getRefCodes(Class beanClass, String beanPath) {
        DataDictionaryField field = getDataDictionaryField(beanClass, beanPath);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        BeanQuery refCodeQuery = new BeanQuery(ReferenceCode.class, criteria);
        HashMap<String, ReferenceCode> refCodeMap =
                (HashMap<String, ReferenceCode>) getBroker().getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 16);

        return refCodeMap;
    }

    /**
     * Initialize the pupil counts to "0" Using a tree map entries will be sorted alpha-numerically.
     * Any new fields can follow the naming convention to preserve ordering appearance.
     */
    private void initializePupilCounts() {
        List<String> pupilCountKeys = Arrays.asList(
                CENSUS_FIELD_C11___a,
                CENSUS_FIELD_C11___b,
                CENSUS_FIELD_C11___c,
                CENSUS_FIELD_C11___d,
                CENSUS_FIELD_C11___e,
                CENSUS_FIELD_C12__ia,
                CENSUS_FIELD_C12__ib,
                CENSUS_FIELD_C12__ic,
                CENSUS_FIELD_C12__id,
                CENSUS_FIELD_C12__ie,
                CENSUS_FIELD_C12__if,
                CENSUS_FIELD_C12__ig,
                CENSUS_FIELD_C12__ih,
                CENSUS_FIELD_C12__ii,
                CENSUS_FIELD_C12__ij,
                CENSUS_FIELD_C12__ik,
                CENSUS_FIELD_C12__il,
                CENSUS_FIELD_C12__im,
                CENSUS_FIELD_C12__in,
                CENSUS_FIELD_C12__io,
                CENSUS_FIELD_C12__ip,
                CENSUS_FIELD_C12_ii,
                CENSUS_FIELD_C12iiia,
                CENSUS_FIELD_C12iiib,
                CENSUS_FIELD_C12iiic,
                CENSUS_FIELD_C12iiid,
                CENSUS_FIELD_C21___a,
                CENSUS_FIELD_C21___b,
                CENSUS_FIELD_C21___c,
                CENSUS_FIELD_C21___d,
                CENSUS_FIELD_C21___e,
                CENSUS_FIELD_C22__ia,
                CENSUS_FIELD_C22__ib,
                CENSUS_FIELD_C22__ic,
                CENSUS_FIELD_C22__id,
                CENSUS_FIELD_C22__ie,
                CENSUS_FIELD_C22__if,
                CENSUS_FIELD_C22__ig,
                CENSUS_FIELD_C22__ih,
                CENSUS_FIELD_C22__ii,
                CENSUS_FIELD_C22__ij,
                CENSUS_FIELD_C22__ik,
                CENSUS_FIELD_C22__il,
                CENSUS_FIELD_C22__im,
                CENSUS_FIELD_C22__in,
                CENSUS_FIELD_C22__io,
                CENSUS_FIELD_C22__ip,
                CENSUS_FIELD_C22_ii,
                CENSUS_FIELD_C22iiia,
                CENSUS_FIELD_C22iiib,
                CENSUS_FIELD_C22iiic,
                CENSUS_FIELD_C22iiid,
                CENSUS_FIELD_C23,
                CENSUS_FIELD_C24___a,
                CENSUS_FIELD_C24___b,
                CENSUS_FIELD_C25___a,
                CENSUS_FIELD_C25___b,
                CENSUS_FIELD_C25___c,
                CENSUS_FIELD_C26);

        for (String key : pupilCountKeys) {
            m_pupilCounts.put(key, Integer.valueOf(0));
        }
    }

    /**
     * Using the current SEN student students, as found in the senSppMap, determine which have
     * Alternative Provisions
     * and add the type of Alternate Provision to the map m_currentSenAPMap.
     *
     * @param senSppMap HashMap<String,Collection<StudentProgramParticipation>>
     */
    private void loadCurrentAPMap(HashMap<String, Collection<StudentProgramParticipation>> senSppMap) {
        for (String studentOid : senSppMap.keySet()) {
            // For each student, go through the student program participation records. If an
            // alternate
            // provision value is found, put the state value in the alternate provision map and
            // continue on to the
            // next student.
            Collection<StudentProgramParticipation> studentSppCollection = senSppMap.get(studentOid);
            for (StudentProgramParticipation spp : studentSppCollection) {
                String altProvision = (String) spp.getFieldValueByAlias(ALIAS_DFE_AP_TYPE);
                if (!StringUtils.isEmpty(altProvision)) {
                    if (m_senAltProvisionRefCodeMap != null && m_senAltProvisionRefCodeMap.containsKey(altProvision)) {
                        altProvision = getStateRefCode(m_senAltProvisionRefCodeMap, altProvision);
                    }
                    m_currentSenAPMap.put(studentOid, altProvision);
                    break;
                }
            }
        }
    }

    /**
     * Load a map by studentOid of current student program participation special education records.
     *
     * @param studentCriteria X2Criteria
     */
    private void loadCurrentSenMap(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Load program records for reporting students and programs into a map.
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        criteria.addEqualTo(m_senProvisionBeanPath, CODE_SEN_STATEMENT);
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, m_senRefCode);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_censusEndDate);
        X2Criteria criteria2 = new X2Criteria();
        X2Criteria criteria3 = new X2Criteria();
        criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
        criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_censusEndDate);
        criteria2.addOrCriteria(criteria3);
        criteria.addAndCriteria(criteria2);
        BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
        query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
        m_currentSenMap = (HashMap<String, Collection<StudentProgramParticipation>>) getBroker()
                .getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);

        Iterator<Map.Entry<String, Collection<StudentProgramParticipation>>> iterator =
                m_currentSenMap.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, Collection<StudentProgramParticipation>> entry = iterator.next();
            String studentOid = entry.getKey();
            StudentEnrollment enrollment = m_helper.getEnrollmentForDate(studentOid, m_censusEndDate, "EWSY");
            if (!StudentManager.isActiveStudent(getOrganization(), enrollment.getStatusCode())) {
                iterator.remove();
            }
        }
    }

    /**
     * Load a map by studentOid of current student program participation special education records
     * being the first
     * and only record.
     *
     * @param studentCriteria X2Criteria
     */
    private void loadFirstTimeSenMap(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Load program records for reporting students and programs into a map.
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        criteria.addEqualTo(m_senProvisionBeanPath, CODE_SEN_STATEMENT);
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, m_senRefCode);
        BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
        query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        query.addOrderBy(StudentProgramParticipation.COL_START_DATE, true);
        m_firstTimeSenMap = (HashMap<String, Collection<StudentProgramParticipation>>) getBroker()
                .getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);

        // remove any student who has special education needs who is not being considered for first
        // time since
        // census start.
        Iterator<Map.Entry<String, Collection<StudentProgramParticipation>>> iterator =
                m_firstTimeSenMap.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, Collection<StudentProgramParticipation>> entry = iterator.next();
            Collection<StudentProgramParticipation> sppCollection = entry.getValue();
            for (StudentProgramParticipation spp : sppCollection) {
                if (spp.getStartDate().before(m_censusStartDate)) {
                    iterator.remove();
                    break;
                }
            }
        }
    }

    /**
     * Load a map by studentOid of current student program participation special education records
     * having no
     * SEN statement.
     *
     * @param studentCriteria X2Criteria
     */
    private void loadNoStatementSenMap(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Load program records for reporting students and programs into a map.
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        criteria.addIn(m_senProvisionBeanPath, m_senNoStatementCodes);
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, m_senRefCode);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_censusEndDate);
        X2Criteria criteria2 = new X2Criteria();
        X2Criteria criteria3 = new X2Criteria();
        criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
        criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_censusEndDate);
        criteria2.addOrCriteria(criteria3);
        criteria.addAndCriteria(criteria2);
        BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
        m_noStatementSenMap = (HashMap<String, Collection<StudentProgramParticipation>>) getBroker()
                .getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);
    }

    /**
     * Load a map by StudentOid of student program participation records for students who have
     * exited special education.
     *
     * @param studentCriteria X2Criteria
     */
    private void loadSenExitMap(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // set start date of last calendar year
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(m_censusStartDate);
        calendar.set(Calendar.MONTH, Calendar.JANUARY);
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        PlainDate startOfLastCalendarYear = new PlainDate(calendar.getTime());

        // set end date of last calendar year
        calendar.set(Calendar.MONTH, Calendar.DECEMBER);
        calendar.set(Calendar.DAY_OF_MONTH, 31);
        PlainDate endOfLastCalendarYear = new PlainDate(calendar.getTime());

        // Load program records for reporting students and programs into a map.
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, m_senRefCode);
        criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, startOfLastCalendarYear);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, endOfLastCalendarYear);
        BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
        query.addOrderByDescending(StudentProgramParticipation.COL_END_DATE);
        m_senExitMap = (HashMap<String, Collection<StudentProgramParticipation>>) getBroker()
                .getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);
    }

    /**
     * Load the SEN reference code.
     */
    private void loadSenRefCode() {
        // Get program codes that are relevant to this export, map by local code.
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_SPECIAL_EDUCATION);
        BeanQuery senRefCodeQuery = new BeanQuery(ReferenceCode.class, criteria);
        ReferenceCode refCode = (ReferenceCode) getBroker().getBeanByQuery(senRefCodeQuery);

        if (refCode != null) {
            m_senRefCode = refCode.getCode();
        }
    }

    /**
     * Load valid SEN statement codes from the SEN Provision Codes. Include all but the value
     * indicating
     * "No Special Education Needed"
     */
    private void loadSenStatementCodes() {
        DataDictionaryField senProvisionField =
                getDataDictionaryField(StudentProgramParticipation.class, m_senProvisionBeanPath);
        if (senProvisionField != null && !StringUtils.isEmpty(senProvisionField.getReferenceTableOid())) {
            Map<String, ReferenceCode> senCodesMap = getReferenceCodes(senProvisionField.getReferenceTableOid());
            for (ReferenceCode referenceCode : senCodesMap.values()) {
                String stateCode = referenceCode.getStateCode();
                if (!CODE_SEN_NOT_NEEDED.equals(stateCode) && !CODE_SEN_STATEMENT.equals(stateCode)) {
                    m_senNoStatementCodes.add(stateCode);
                }
            }
        }
    }
}
