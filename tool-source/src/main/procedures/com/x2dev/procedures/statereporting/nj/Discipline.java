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
package com.x2dev.procedures.statereporting.nj;


import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for Discipline export.
 *
 * @author X2 Development Corporation
 */

public class Discipline extends StateReportData {
    /**
     * Entity class for Student Record Student Level export.
     *
     * @author X2 Development Corporation
     */

    public static class DisciplineEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */

        public DisciplineEntity() {
            // no argument constructor
        }

        /**
         * Constants for reporting information.
         */
        protected static final String LOCAL_CODE_IN_SCHOOL_SUSPENSION = "ISS";
        protected static final String LOCAL_CODE_OUT_SCHOOL_SUSPENSION = "OSS";

        /**
         * Local variables for reporting information.
         */
        protected int m_daysOutOfSchoolSuspension;
        protected int m_daysInSchoolSuspension;

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            Discipline stdData = (Discipline) data;
            Student student = (Student) bean;
            Collection<ConductAction> conductActions = stdData.m_conductActionMap.get(student.getOid());
            m_daysOutOfSchoolSuspension = 0;
            m_daysInSchoolSuspension = 0;
            String actionCode;

            if (conductActions != null) {
                for (ConductAction action : conductActions) {

                    actionCode = data.lookupReferenceCodeByBeanPath(ConductAction.class, ConductAction.COL_ACTION_CODE,
                            action.getActionCode(), ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (LOCAL_CODE_IN_SCHOOL_SUSPENSION.equals(actionCode)) {
                        if (action.getActionPenaltyTime() != null) {
                            m_daysInSchoolSuspension += action.getActionPenaltyTime().intValue();
                        }
                    } else if (LOCAL_CODE_OUT_SCHOOL_SUSPENSION.equals(actionCode)) {
                        if (action.getActionPenaltyTime() != null) {
                            m_daysOutOfSchoolSuspension += action.getActionPenaltyTime().intValue();
                        }
                    }
                }
            }
        }

        /**
         * Returns the number of days of in-school suspension for the current student.
         *
         * @return int
         */
        public int getDaysInSchoolSuspension() {
            return m_daysInSchoolSuspension;
        }

        /**
         * Returns the number of days of out-of-school suspension for the current student.
         *
         * @return int
         */
        public int getDaysOutOfSchoolSuspension() {
            return m_daysOutOfSchoolSuspension;
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
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";


    protected static final String PARAM_START_DATE = "startDate";
    protected static final String PARAM_END_DATE = "endDate";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Collection<ConductAction>> m_conductActionMap;
    protected String m_excludeStdField;
    protected PlainDate m_startDate;
    protected PlainDate m_endDate;

    /**
     * Retrieves the days in suspension from the Discipline entity.
     */
    protected class RetrieveSuspension implements FieldRetriever {
        private final String PARAM_DAYS_IN_SCHOOL = "INSCHOOL";
        private final String PARAM_DAYS_OUT_OF_SCHOOL = "OUTSCHOOL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            Object value = null;

            if (PARAM_DAYS_IN_SCHOOL.equals(param)) {
                value = Integer.valueOf(((DisciplineEntity) entity).getDaysInSchoolSuspension());
            } else if (PARAM_DAYS_OUT_OF_SCHOOL.equals(param)) {
                value = Integer.valueOf(((DisciplineEntity) entity).getDaysOutOfSchoolSuspension());
            }

            return value;
        }
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(PARAM_END_DATE);

        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);

        Criteria criteria = getStudentCriteria();
        // ? create student Query ?
        QueryByCriteria query = new QueryByCriteria(Student.class, criteria);
        applyInputSort(query, null);
        loadStudentConductActionData(criteria);

        setQuery(query);

        setEntityClass(DisciplineEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("DISC-SUSPENSION", new RetrieveSuspension());

        HashMap validators = new HashMap<String, FieldRetriever>();
        super.addCalcs(calcs);
        super.addValidators(validators);
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria conductActionCriteria = new Criteria();
        conductActionCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);
        conductActionCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_endDate);
        SubQuery conductIncidentQuery =
                new SubQuery(ConductIncident.class, ConductIncident.COL_STUDENT_OID, conductActionCriteria);

        Criteria reportingCriteria = new Criteria();
        reportingCriteria.addIn(X2BaseBean.COL_OID, conductIncidentQuery);

        return reportingCriteria;
    }

    /**
     * Loads the student Conduct Action data required by this export.
     *
     * @param studentCriteria Criteria
     */
    private void loadStudentConductActionData(Criteria studentCriteria) {

        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(ConductAction.COL_STUDENT_OID, studentsSubQuery);

        QueryByCriteria conductActionQuery = new QueryByCriteria(ConductAction.class, criteria);

        m_conductActionMap =
                getBroker().getGroupedCollectionByQuery(conductActionQuery, ConductAction.COL_STUDENT_OID, 1024);
    }
}
