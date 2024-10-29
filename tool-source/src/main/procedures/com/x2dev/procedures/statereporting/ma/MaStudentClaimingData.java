/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class MaStudentClaimingData.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MaStudentClaimingData extends StateReportData {

    /**
     * The Class StudentClaimingEntity.
     */
    public static class StudentClaimingEntity extends StateReportEntity {

        private PlainDate m_entryDate;
        private PlainDate m_exitDate;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentClaimingEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {

            Student student = (Student) getBean();
            String name = " [" + student.getStateId() + "]"
                    + " [" + student.getLocalId() + "]"
                    + " [" + student.getNameView() + "]";

            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            MaStudentClaimingData spData = (MaStudentClaimingData) data;
            SisStudent student = (SisStudent) bean;

            List<StudentEnrollment> enrollments = spData.m_helper.getStudentEnrollments(student);
            if (enrollments != null && !enrollments.isEmpty()) {
                for (StudentEnrollment enrollment : enrollments) {
                    if (enrollment.getEnrollmentType().equals(ENROLLMENT_WITHDRAW_TYPE)) {
                        if (m_exitDate == null) {
                            m_exitDate = enrollment.getEnrollmentDate();
                        }
                    } else if (spData.m_enrollmentStatuses.contains(enrollment.getStatusCode())) {
                        m_entryDate = enrollment.getEnrollmentDate();
                        // Only set exit date if it is after entry date
                        break;
                    }
                }
            }
            if (m_entryDate == null) {
                setRowCount(0);
            }
        }

        /**
         * To string.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * Student history helper with dates for student enrollments query.
     */
    private static class StudentHistoryDatesHelper extends StudentHistoryHelper {

        /**
         * Instantiates a new student history dates helper.
         *
         * @param data StateReportData
         */
        public StudentHistoryDatesHelper(StateReportData data) {
            super(data);
        }

        /**
         * Returns a criteria that finds enrollment records for students returned by the passed
         * subquery.
         *
         * @param studentSubQuery SubQuery
         * @return X2Criteria
         */
        @Override
        protected X2Criteria buildEnrollmentCriteria(SubQuery studentSubQuery) {
            X2Criteria studentEnrollmentCriteria = super.buildEnrollmentCriteria(studentSubQuery);
            PlainDate begin = (PlainDate) getSelectionProperty(PROPERTY_BEGIN_DATE);
            PlainDate end = (PlainDate) getSelectionProperty(PROPERTY_END_DATE);
            if (begin != null) {
                studentEnrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, begin);
            }
            if (end != null) {
                studentEnrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, end);
            }
            return studentEnrollmentCriteria;
        }
    }

    /**
     * Retriever to get district id.
     */
    protected class DistrictId implements FieldRetriever {
        public static final String CALC_AUTH_DIST = "CUR_AUTH_DIST";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            Object districtId = ((SisStudent) entity.getBean()).getSchool().getFieldValueByAlias("skl-sif-district-id");
            if (districtId == null) {
                districtId =
                        ((SisStudent) entity.getBean()).getOrganization1().getFieldValueByAlias("DOE District ID");
            }

            return districtId;
        }
    }

    /**
     * Retriever to get enrollment enrydate.
     */
    protected class EnrollmentEntryDate implements FieldRetriever {
        public static final String CALC_ENR_ENTRYDATE = "ENR_ENTRYDATE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return m_entryDate == null ? ((StudentClaimingEntity) entity).m_entryDate : m_entryDate;
        }
    }

    /**
     * Retriever to get enrollment exitdate.
     */
    protected class EnrollmentExitDate implements FieldRetriever {
        public static final String CALC_ENR_EXITDATE = "ENR_EXITDATE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return ((StudentClaimingEntity) entity).m_exitDate;
        }
    }

    protected static final String PARAM_REMOVE_HEADER = "removeHeader";

    private static final String ENROLLMENT_WITHDRAW_TYPE = "W";

    private static final String PARAM_DATE_BEGIN = "beginDate";
    private static final String PARAM_DATE_END = "endDate";
    private static final String PARAM_DATE_ENTRY = "entryDate";
    private static final String PARAM_ENROLLMENT_STATUSES = "enrollmentStatuses";

    private List<String> m_enrollmentStatuses = new ArrayList<>();
    private PlainDate m_entryDate;
    private StudentHistoryHelper m_helper;
    private boolean m_removeHeader = false;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        return m_removeHeader ? null : super.getHeading();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        if (getParameter(PARAM_REMOVE_HEADER) != null && ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()) {
            m_removeHeader = true;
        }

        if (getParameter(PARAM_DATE_ENTRY) != null) {
            m_entryDate = (PlainDate) getParameter(PARAM_DATE_ENTRY);
        }

        initEnrollmentStatuses();
        initStudentHelper();

        if (getSetupErrors().size() == 0) {
            Map<String, FieldRetriever> retrives = new HashMap<String, FieldRetriever>();
            retrives.put(DistrictId.CALC_AUTH_DIST, new DistrictId());
            retrives.put(EnrollmentEntryDate.CALC_ENR_ENTRYDATE, new EnrollmentEntryDate());
            retrives.put(EnrollmentExitDate.CALC_ENR_EXITDATE, new EnrollmentExitDate());
            addCalcs(retrives);

            // Assign the custom entity class, if there is one.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(StudentClaimingEntity.class);
        }
    }

    /**
     * Load selected enrollment statuses from input parameters.
     *
     * @return inlcude inactive or not
     */
    private void initEnrollmentStatuses() {
        String enrollmentStatuses = (String) getParameter(PARAM_ENROLLMENT_STATUSES);
        if (!StringUtils.isEmpty(enrollmentStatuses)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(enrollmentStatuses.split(",")));

            ColumnQuery query = new ColumnQuery(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE}, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_enrollmentStatuses.add((String) row[0]);
                }
            }
        }
        if (m_enrollmentStatuses.isEmpty()) {
            addSetupError("Enrollment Statuses not found", enrollmentStatuses);
        }
    }

    /**
     * Initialize Student Helper.
     */
    private void initStudentHelper() {
        m_helper = new StudentHistoryDatesHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_INACTIVE, Boolean.TRUE);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getParameter(PARAM_DATE_BEGIN));
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getParameter(PARAM_DATE_END));
    }
}

