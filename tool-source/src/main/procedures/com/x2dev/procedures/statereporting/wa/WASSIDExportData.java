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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Export Data Module for WA SSID Export.
 *
 * @author X2 Development Corporation
 */
public class WASSIDExportData extends StateReportData {
    /**
     * Entity class for WA SSID Export.
     *
     */
    public static class WASSIDEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        WASSIDExportData m_sdData;
        String m_servingSchoolId;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WASSIDEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the StudentEnrollmentSpan or StudentSchool record for the current index.
         *
         * @return Object
         */
        public String getServingSchoolId() {
            return m_servingSchoolId;
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
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }

        /**
         * Intitialize.
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

            m_sdData = (WASSIDExportData) data;
            Student student = (Student) bean;
            List<StudentEnrollmentSpan> enrollmentSpans = m_sdData.m_helper.getStudentEnrollmentSpans(student, true);
            processDistrictServe(enrollmentSpans);
            removeNoShows(enrollmentSpans);

            removeExcludedSchoolRecords(enrollmentSpans);

            PlainDate tempDate = null;
            String tempSchoolId = null;

            for (StudentEnrollmentSpan span : enrollmentSpans) {
                StudentEnrollment activeEnrollment = span.getFirstActiveEnrollment();
                // Check the span for entry type (internal or external)
                StudentEnrollment inactiveEnrollment = null;

                if (activeEnrollment != null) {
                    inactiveEnrollment = span.getFirstInactiveEnrollment();
                    PlainDate inactiveEnrDate = null;

                    if (inactiveEnrollment != null &&
                            (inactiveEnrDate = inactiveEnrollment.getEnrollmentDate()) != null &&
                            (inactiveEnrDate.equals(m_sdData.m_beginIntervalDate)
                                    || inactiveEnrDate.after(m_sdData.m_beginIntervalDate))
                            &&
                            (inactiveEnrDate.equals(m_sdData.m_endIntervalDate)
                                    || inactiveEnrDate.before(m_sdData.m_endIntervalDate))) {
                        if (tempDate == null || inactiveEnrDate.after(tempDate)) {
                            tempDate = inactiveEnrDate;
                            tempSchoolId = inactiveEnrollment.getSchool() == null
                                    ? null
                                    : (String) inactiveEnrollment.getSchool()
                                            .getFieldValueByBeanPath(m_sdData.m_fieldSchoolServe);
                        }
                    } else {
                        if (tempDate == null || activeEnrollment.getEnrollmentDate().after(tempDate)) {
                            tempDate = activeEnrollment.getEnrollmentDate();
                            tempSchoolId = activeEnrollment.getSchool() == null
                                    ? null
                                    : (String) activeEnrollment.getSchool()
                                            .getFieldValueByBeanPath(m_sdData.m_fieldSchoolServe);
                        }
                    }
                }
            }
            m_servingSchoolId = tempSchoolId;
            if (enrollmentSpans.isEmpty()) {
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
         * remove any enrollment spans that represent enrollments of a district student
         * with service in a different district.
         *
         * @param enrollmentSpans List<StudentEnrollmentSpan>
         */
        private void processDistrictServe(List<StudentEnrollmentSpan> enrollmentSpans) {
            Iterator<StudentEnrollmentSpan> spanIterator = enrollmentSpans.iterator();
            while (spanIterator.hasNext()) {
                StudentEnrollmentSpan span = spanIterator.next();
                String districtServe =
                        (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_sdData.m_fieldDistrictServe);
                if (!StringUtils.isEmpty(districtServe)) {
                    districtServe = m_sdData.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                            m_sdData.m_fieldDistrictServe, districtServe,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (!StringUtils.isEmpty(districtServe) && !districtServe.equals(m_sdData.m_districtCode)) {
                    spanIterator.remove();
                }
            }
        }

        /**
         * Remove enrollments that were in the excluded schools.
         *
         * @param enrollmentSpans List<StudentEnrollmentSpan>
         */
        private void removeExcludedSchoolRecords(List<StudentEnrollmentSpan> enrollmentSpans) {
            Iterator<StudentEnrollmentSpan> spanIterator = enrollmentSpans.iterator();
            while (spanIterator.hasNext()) {
                StudentEnrollmentSpan span = spanIterator.next();
                StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    if (checkSchoolExists(enrollment.getSchoolOid())) {
                        spanIterator.remove();
                    }
                }
            }
        }

        /**
         * remove any enrollment spans that have a no-show withdrawal code.
         * this is identified by having NS in the local code of the withdrawal code.
         *
         * @param enrollmentSpans List<StudentEnrollmentSpan>
         */
        private void removeNoShows(List<StudentEnrollmentSpan> enrollmentSpans) {
            Iterator<StudentEnrollmentSpan> spanIterator = enrollmentSpans.iterator();
            while (spanIterator.hasNext()) {
                /*
                 * Check if the span is a no-show span. Do not include no-show spans.
                 * A no-show span represents an enrollment where the student never showed up.
                 * It is identified by a withdrawal code that has NS in the local code of the
                 * reference table.
                 */
                StudentEnrollmentSpan span = spanIterator.next();
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    String withdrawalCode = enrollment.getEnrollmentCode();
                    withdrawalCode = getData().lookupReferenceCodeByRefTbl(m_sdData.m_refTableWithdrawalCode,
                            withdrawalCode, ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (CODE_NO_SHOW.equals(withdrawalCode)) {
                        spanIterator.remove();
                    }
                }
            }
        }
    }

    /**
     * Retrieve serving school code.
     */
    protected class RetrieveServingSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((WASSIDEntity) entity).getServingSchoolId();
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DISTRICT_SERVE = "DOE DISTRICT SERVE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_NON_PRIMARY = "DOE NON PRIMARY";

    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL ID";
    protected static final String ALIAS_STD_SASID = "DOE SASID";

    /**
     * Parameters
     */
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";

    /**
     * Other constants
     */
    protected static final String CODE_NO_SHOW = "NS";
    protected static final String ENR_STATE_CODE_1 = "1";
    protected static final String ENR_STATE_CODE_2 = "2";

    /*
     * Instance variables
     */
    protected PlainDate m_beginIntervalDate;
    protected String m_districtCode;
    protected PlainDate m_endIntervalDate;
    protected String m_fieldDistrictId;
    protected String m_fieldDistrictServe;
    protected String m_fieldSchoolServe;
    protected StudentHistoryHelper m_helper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected String m_fieldStdSASID;
    protected String m_excludeSchool;
    protected static Map m_excludeSchoolMap;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_beginIntervalDate = getOrganization().getCurrentContext().getStartDate();
        m_endIntervalDate = getOrganization().getCurrentContext().getEndDate();

        initializeFields();

        // had to initialize to null as it is made static for allowing it to be accessed for
        // removing secondary schools.
        m_excludeSchoolMap = null;

        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            loadSchoolExcludeMap();
        }
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endIntervalDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_beginIntervalDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);

        m_helper.getStudentCriteria().addEmpty(m_fieldStdSASID, getBroker().getPersistenceKey());

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(WASSIDEntity.class);


            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SERV-SCHOOL", new RetrieveServingSchool());
            super.addCalcs(calcs);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldDistrictId = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldDistrictServe = translateAliasToJavaName(ALIAS_DISTRICT_SERVE, true);
        m_fieldSchoolServe = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldStdSASID = translateAliasToJavaName(ALIAS_STD_SASID, true);

        m_districtCode = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictId);

        m_refTableEnrollmentCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
    }

    /**
     * Populate map of excluded schools.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Check if school should be reported.
     *
     * @param schoolOid String
     * @return true, if successful
     */
    public static boolean checkSchoolExists(String schoolOid) {
        boolean returnValue = false;
        if (m_excludeSchoolMap != null) {
            returnValue = m_excludeSchoolMap.containsKey(schoolOid);
        }
        return returnValue;
    }

}
