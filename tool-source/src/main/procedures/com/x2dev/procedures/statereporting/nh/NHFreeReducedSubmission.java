/*
 * ====================================================================
 * X2 Development Corporation
 * Copyright (c) 2002-2010 X2 Development Corporation. All rights reserved. Redistribution and use
 * in source and binary forms, with or without modification, is not permitted without express
 * written agreement from X2 Development Corporation.
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote style="color:green; font-size:16pt">i4see Student Class export.</blockquote>
 *
 * Procedure ID:&nbsp; <code>nhi4seeStudentClass</code>
 * <p>
 *
 * @author X2 Development Corporation
 * @since v3.0
 */
public class NHFreeReducedSubmission extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NHFreeReducedSubmissionEntity extends StateReportEntity {
        StudentEnrollment m_enrollment;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NHFreeReducedSubmissionEntity() {
            // public no argument constructor for dynamic instantiation.
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
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";
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
            NHFreeReducedSubmission frData = (NHFreeReducedSubmission) getData();
            m_enrollment = frData.m_helper.getEnrollmentForDate(bean.getOid(), frData.m_reportDate, "ESY");
            if (m_enrollment == null || !frData.includeSchool(m_enrollment.getSchoolOid())
                    || (frData.m_isEOY && !m_enrollment.getEnrollmentDate().after(frData.m_eoyDate))) {
                setRowCount(0);
            } else if (!m_enrollment.getEnrollmentType().equals("E")
                    || (data.getSchool() != null
                            && !data.getSchool().getOid().equals(m_enrollment.getSchool().getOid()))) {
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
         * Gets the enrollment.
         *
         * @return Student enrollment
         */
        public StudentEnrollment getEnrollment() {
            return m_enrollment;
        }
    }

    /**
     * Returns the enrollment information from the current schedule span.
     */
    protected class RetrieveStudentInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            NHFreeReducedSubmission frData = (NHFreeReducedSubmission) data;
            SisStudent student = (SisStudent) entity.getBean();
            StudentEnrollment enr = ((NHFreeReducedSubmissionEntity) entity).getEnrollment();
            String parameter = (String) field.getParameter();
            Object value = null;

            if (enr != null) {
                if (CALC_PARAM_DIST_NBR.equals(parameter)) {
                    value = enr.getSchool().getOrganization1().getFieldValueByAlias(ALIAS_I4SEE_040);
                } else if (CALC_PARAM_SAU_NBR.equals(parameter)) {
                    value = enr.getSchool().getOrganization1().getFieldValueByAlias(ALIAS_I4SEE_030);
                } else if (CALC_PARAM_SKL_NBR.equals(parameter)) {
                    value = enr.getSchool().getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                } else if (CALC_PARAM_FR_IND.equals(parameter)) {
                    String frLocalCode = (String) student.getFieldValueByBeanPath(frData.m_stdFrInd);
                    if (!StringUtils.isEmpty(frLocalCode)) {
                        value = lookupReferenceCodeByBeanPath(SisStudent.class, frData.m_stdFrInd, frLocalCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            }
            return value;
        }
    }

    /**
     * Input Definition Parameters
     */
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String REPORT_DATE_PARAM = "reportDate";
    protected static final String PARAM_INCLUDE_STUDENT_NAMES = "includeStudentName";
    protected static final String PARAM_IS_EOY = "isEOY";

    /**
     * Oter Constants
     */
    private static final String STUDENT_NAME = "Name View";

    /**
     * Retriever Parameters
     */
    private static final String CALC_ID_STD_INFO = "STD-INFO";
    private static final String CALC_PARAM_DIST_NBR = "DIST_NBR";
    private static final String CALC_PARAM_FR_IND = "FR_IND";
    private static final String CALC_PARAM_SAU_NBR = "SAU_NBR";
    private static final String CALC_PARAM_SKL_NBR = "SKL_NBR";
    /**
     * Alias constants
     */
    // ORGANIZATION
    private static final String ALIAS_I4SEE_030 = "i4see 030";
    private static final String ALIAS_I4SEE_040 = "i4see 040";

    // SCHOOL
    private static final String ALIAS_EXCLUDE_SCHOOL = "i4see EXCLUDE SCHOOL";
    private static final String ALIAS_SKL_STATE_ID = "StateId";


    // STUDENT
    private static final String ALIAS_STD_FR_IND = "i4see 480";

    private List<String> FR_VALID_CODES = Arrays.asList(new String[] {"1", "2", "3"});

    /**
     * Supporting instance variables.
     */
    protected String m_excludeSchool;
    protected Map m_excludeSchoolMap;
    protected StudentHistoryHelper m_helper;
    protected boolean m_includeStudentNames = false;
    protected boolean m_removeExcludedSkl = false;
    protected boolean m_isEOY = false;
    protected PlainDate m_reportDate;
    protected PlainDate m_eoyDate;
    protected String m_stdFrInd;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return Student.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "NH Free and Reduced Submission";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        if (m_removeExcludedSkl) {
            return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
        }
        return true;
    }

    /**
     * Initialize the data module. Initialize necessary working resources. Define query for students
     * to load. Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadSchoolExcludeMap();

            /**
             * Add the Student Name field if requested
             */
            if (m_includeStudentNames) {
                getFieldDefinitions().add(0, getName());
            }


            /*
             * Build query object that will be used to retrieve export students.
             */
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.getStudentCriteria().addIn(m_stdFrInd, getValidFRCodes());

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(NHFreeReducedSubmissionEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_STD_INFO, new RetrieveStudentInfo());
            super.addCalcs(calcs);

        }
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    private FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME, SisStudent.COL_NAME_VIEW,
                null, false, 1, 32, null,
                null, null, null, null);
        return field;
    }

    /**
     * Gets the valid FR codes.
     *
     * @return List of local codes of Free and Reduced Indicator.
     */
    private List<String> getValidFRCodes() {
        List<String> localCodes = new ArrayList<String>();
        if (m_stdFrInd != null) {
            DataDictionaryField frDataField = getDataDictionaryField(SisStudent.class, m_stdFrInd);

            if (frDataField != null && !StringUtils.isEmpty(frDataField.getReferenceTableOid())) {
                X2Criteria frCodesCriteria = new X2Criteria();
                frCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, frDataField.getReferenceTableOid());
                frCodesCriteria.addIn(ReferenceCode.COL_STATE_CODE, FR_VALID_CODES);

                QueryByCriteria frCodesQuery = new QueryByCriteria(ReferenceCode.class, frCodesCriteria);

                localCodes.addAll(
                        getBroker().getGroupedCollectionByQuery(frCodesQuery, ReferenceCode.COL_CODE, 512).keySet());

            }
        }

        return localCodes;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_removeExcludedSkl = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        m_stdFrInd = translateAliasToJavaName(ALIAS_STD_FR_IND, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);

        if (getParameter(REPORT_DATE_PARAM) != null) {
            m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        }
        m_isEOY = getParameter(PARAM_IS_EOY) != null ? ((Boolean) getParameter(PARAM_IS_EOY)).booleanValue() : false;
        Calendar cal = Calendar.getInstance();
        cal.set(getCurrentContext().getSchoolYear() - 1, Calendar.OCTOBER, 1);
        m_eoyDate = new PlainDate(cal.getTimeInMillis());
        if (getParameter(PARAM_INCLUDE_STUDENT_NAMES) != null) {
            m_includeStudentNames = ((Boolean) getParameter(PARAM_INCLUDE_STUDENT_NAMES)).booleanValue();
        }
    }

    /**
     * Build a map of schools with the alias set to exclude school.
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);

        BeanQuery query = new BeanQuery(School.class, schoolCriteria);

        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

}
