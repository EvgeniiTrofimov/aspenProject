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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure class for "IL-export-1.3.14: Students with same SID" export.
 */
public class ILStudentsSameSIDData extends StateReportData {
    /**
     * One entity stands for one student and produce a row
     * for each another student with same State ID as entity student.
     */
    public static class ILStudentsSameSIDDataEntity extends StateReportEntity {
        private final List<SisStudent> m_students = new ArrayList<SisStudent>();

        /**
         * The effective Entry student enrollment record for report date.
         */
        protected StudentEnrollment m_enrollment = null;

        /**
         * ILStudentsSameSIDData data.
         */
        ILStudentsSameSIDData m_ilData = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ILStudentsSameSIDDataEntity() {
            // public no argument constructor for dynamic instantiation.
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
         * Gets the current student.
         *
         * @return Sis student
         */
        public SisStudent getCurrentStudent() {
            return m_students.get(getCurrentRow());
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
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder name = new StringBuilder();
            SisStudent student = (SisStudent) getBean();

            name.append(student.getNameView());
            name.append(" [LASID: ");
            name.append(student.getLocalId());
            name.append(", SASID: ");
            name.append(student.getStateId());
            name.append("] || ");

            SisStudent curStudent = getCurrentStudent();
            name.append(curStudent.getNameView());
            name.append(" [LASID: ");
            name.append(curStudent.getLocalId());
            name.append(", SASID: ");
            name.append(curStudent.getStateId());
            name.append("]");

            return name.toString();
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_ilData = (ILStudentsSameSIDData) data;
            SisStudent student = (SisStudent) getBean();

            m_enrollment = m_ilData.m_helper.getEnrollmentForDate(student.getOid(),
                    m_ilData.m_reportDate, "E");

            if (!m_ilData.m_processedStd.contains(student) &&
                    (m_ilData.m_stdMap.get(student.getStateId()) != null) &&
                    m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_ilData.m_excludeSklField))) {
                m_students.addAll(m_ilData.m_stdMap.get(student.getStateId()));
                m_students.remove(student);
                m_ilData.m_processedStd.add(student);
                m_ilData.m_processedStd.addAll(m_ilData.m_stdMap.get(student.getStateId()));
                m_rcdtsMap = lookupOverrides();

                setRowCount(m_students.size());
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
            ILStudentsSameSIDData sssData = (ILStudentsSameSIDData) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (sssData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode =
                        sssData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            sssData.lookupStateValue(StudentEnrollment.class, sssData.m_fieldServiceSchoolCode,
                                    serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
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
            ILStudentsSameSIDDataEntity sssEntity = (ILStudentsSameSIDDataEntity) entity;
            String rcdts = null;
            if (param.equals("H") && sssEntity.getEffectiveEnrollment() != null
                    && sssEntity.getEffectiveEnrollment().getSchool() != null) {
                ILStudentsSameSIDData sssData = (ILStudentsSameSIDData) data;
                rcdts = (String) sssEntity.getEffectiveEnrollment().getSchool()
                        .getFieldValueByBeanPath(sssData.m_fieldSchoolCode);
            } else if (param.equals("S")) {
                rcdts = sssEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = sssEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve information about another student with same SID.
     */
    public class DublicateStudentRetriever implements FieldRetriever {
        public static final String SID = "SID";
        public static final String STUDENT_LAST_NAME = "LAST_NAME";
        public static final String STUDENT_FIRST_NAME = "FIRST_NAME";
        public static final String STUDENT_DOB = "DOB";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ILStudentsSameSIDDataEntity ilEntity = (ILStudentsSameSIDDataEntity) entity;
            String parameter = (String) field.getParameter();
            Object value = EMPTY_STRING;
            SisStudent currentStudent = ilEntity.getCurrentStudent();
            if (SID.equals(parameter)) {
                value = currentStudent.getStateId();
            } else if (STUDENT_FIRST_NAME.equals(parameter)) {
                value = currentStudent.getPerson().getFirstName();
            } else if (STUDENT_LAST_NAME.equals(parameter)) {
                value = currentStudent.getPerson().getLastName();
            } else if (STUDENT_DOB.equals(parameter)) {
                value = currentStudent.getPerson().getDob();
            }
            return value;
        }

    }

    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVE";

    protected String m_excludeSklField;
    protected Map<String, List<SisStudent>> m_stdMap = null;
    /**
     * Place to store already processed students
     */
    protected List<SisStudent> m_processedStd = null;
    protected String m_fieldHomeSchoolCode;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected PlainDate m_reportDate;
    protected StudentHistoryHelper m_helper;

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
        m_reportDate = new PlainDate();

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        if (getSetupErrors().size() == 0) {
            // need to find State ID's that meet more than once
            X2Criteria sasidCriteria = new X2Criteria();
            SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_STATE_ID, sasidCriteria);
            subQuery.addGroupBy(SisStudent.COL_STATE_ID);

            Criteria havingCriteria = new Criteria();
            havingCriteria.addSql("count(STD_ID_STATE) > 1");
            subQuery.setHavingCriteria(havingCriteria);

            X2Criteria stdCriteria = new X2Criteria();
            stdCriteria.addIn(SisStudent.COL_STATE_ID, subQuery);
            QueryByCriteria stdQuery = new QueryByCriteria(SisStudent.class, stdCriteria);
            applyInputCriteria(stdCriteria, isSchoolContext(), null);

            m_stdMap = getBroker().getGroupedCollectionByQuery(stdQuery, SisStudent.COL_STATE_ID, 10);
            setQuery(stdQuery);
            setEntityClass(ILStudentsSameSIDDataEntity.class);
            m_processedStd = new ArrayList();

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

            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-DUBLICATE", new DublicateStudentRetriever());
            calcs.put("STD-RCDTS", new RetrieveRcdts());
            super.addCalcs(calcs);
        }
    }

    /**
     * Initialize aliases and other report data.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
    }
}
