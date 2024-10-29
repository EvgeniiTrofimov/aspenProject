/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Georgia state export for batch assessment user role.
 */
public class BatchAssessment extends StateReportData {

    public static class BatchAssessmentEntity extends StateReportEntity {
        private BatchAssessment m_data;
        private List<String> m_schoolCodes = null;
        private List<String> m_userRoles = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public BatchAssessmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the school code for the current row.
         *
         * @return String.
         */
        public String getRowSchoolCode() {
            String sklCode = null;
            if (m_schoolCodes != null && getCurrentRow() < m_schoolCodes.size()) {
                sklCode = m_schoolCodes.get(getCurrentRow());
            }
            return sklCode;
        }

        /**
         * Returns the user role for the current row.
         *
         * @return String.
         */
        public String getRowUserRole() {
            String userRole = null;
            if (m_userRoles != null && getCurrentRow() < m_userRoles.size()) {
                userRole = m_userRoles.get(getCurrentRow());
            }
            return userRole;
        }

        /**
         * Initialize.
         * Increment count of records for trailer record count.
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
            m_data = (BatchAssessment) data;
            m_schoolCodes = new ArrayList<String>();
            m_userRoles = new ArrayList<String>();
            Staff staff = (Staff) bean;

            int rowCount = 0;
            if (!StringUtils.isEmpty((String) staff.getFieldValueByBeanPath(m_data.m_userRoleField))) {
                m_userRoles.add((String) staff.getFieldValueByBeanPath(m_data.m_userRoleField));
                m_schoolCodes.add((String) staff.getSchool().getFieldValueByBeanPath(m_data.m_doeSchoolField));
                rowCount++;
            }

            X2Criteria mtcCriteria = new X2Criteria();
            mtcCriteria.addEqualTo(
                    ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER
                            + Schedule.REL_DISTRICT_CONTEXT + PATH_DELIMITER + X2BaseBean.COL_OID,
                    m_data.getCurrentContext().getOid());
            mtcCriteria.addEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + X2BaseBean.COL_OID, staff.getOid());
            mtcCriteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, Boolean.TRUE);
            QueryByCriteria mtcQuery = new QueryByCriteria(ScheduleTeacher.class, mtcCriteria);
            Collection<ScheduleTeacher> mtcs = m_data.getBroker().getCollectionByQuery(mtcQuery);

            List<String> distinctMtcSklCodes = new ArrayList<String>();
            for (ScheduleTeacher mtc : mtcs) {
                String sklCode =
                        (String) mtc.getSection().getSchoolCourse().getSchool()
                                .getFieldValueByBeanPath(m_data.m_doeSchoolField);
                if (!distinctMtcSklCodes.contains(sklCode)) {
                    distinctMtcSklCodes.add(sklCode);
                    m_schoolCodes.add(sklCode);
                    m_userRoles.add("Teacher");
                    rowCount++;
                }
            }

            setRowCount(rowCount);
        }
    }



    /*
     * Parameters
     */
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";


    /*
     * Aliases
     */
    private static final String ALIAS_USER_ROLE = "all-stf-BatchAssessmentUserRole";
    private static final String ALIAS_DOE_SCHOOL = "DOE School";
    private static final String ALIAS_DOE_EXCLUDE_STAFF = "DOE EXCLUDE STF";

    /*
     * Instance variables.
     */
    protected String m_userRoleField;
    protected String m_doeSchoolField;


    public class RoleRetriever implements FieldRetriever {
        private static final String CALC_ID = "ROLE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return ((BatchAssessmentEntity) entity).getRowUserRole();
        }
    }

    public class SchoolCodeRetriever implements FieldRetriever {
        private static final String CALC_ID = "SCHOOL-CODE";
        private final List DISTRICT_ROLES = Arrays.asList("District", "District Technology Coordinator");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String sklCode = ((BatchAssessmentEntity) entity).getRowSchoolCode();
            String userRole = ((BatchAssessmentEntity) entity).getRowUserRole();

            if (DISTRICT_ROLES.contains(userRole)) {
                return "";
            } else if (!StringUtils.isEmpty(sklCode)) {
                return sklCode;
            }
            return value;
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_userRoleField = translateAliasToJavaName(ALIAS_USER_ROLE, true);
        m_doeSchoolField = translateAliasToJavaName(ALIAS_DOE_SCHOOL, true);

        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            Criteria staffCriteria = getStaffCriteria();
            QueryByCriteria staffQuery = new QueryByCriteria(Staff.class, staffCriteria);
            staffQuery.addOrderByAscending(Staff.COL_NAME_VIEW);

            // Set the query to be used for staff selection.
            setQuery(staffQuery);
            setEntityClass(BatchAssessmentEntity.class);

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RoleRetriever.CALC_ID, new RoleRetriever());
            calcs.put(SchoolCodeRetriever.CALC_ID, new SchoolCodeRetriever());
            super.addCalcs(calcs);
        }
    }

    /**
     * Returns the criteria that retrieves all staff that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStaffCriteria() {
        X2Criteria staffCriteria = new X2Criteria();

        /*
         * Active staff
         */
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        staffCriteria.addEqualTo(Staff.COL_STATUS, activeCode);
        staffCriteria.addNotEqualTo(translateAliasToJavaName(ALIAS_DOE_EXCLUDE_STAFF, true), Boolean.TRUE);

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            staffCriteria.addEqualTo(Staff.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            staffCriteria.addEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            staffCriteria.addEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        /*
         * Check staff selection criteria user input.
         */
        selectStaffCriteria(staffCriteria);

        return staffCriteria;
    }

    /**
     * Implementing the select student filter based on parameters.
     *
     * @param staffCriteria X2Criteria
     */
    private void selectStaffCriteria(X2Criteria staffCriteria) {
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // Local Staff ID
                staffCriteria.addEqualTo(Staff.COL_LOCAL_ID, queryString);
                break;

            case 2: // Snapshot
                addRecordSetCriteria(staffCriteria, queryString);
                break;

            default:
                // Take all staff in the district
                break;
        }
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
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

}
