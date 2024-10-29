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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FLEducationIdentifierData.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLEducationIdentifierData extends StateReportData {

    /**
     * Field retriever for Race fields.
     */
    protected class RetrieveRaceFields implements FieldRetriever {

        public static final String CALC_ID = "RACE";

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
            SisStudent student = (SisStudent) entity.getBean();
            Object value = null;
            Collection<Race> races = getStudentRaceMap().get(student.getPersonOid());

            if (races != null) {
                List<String> raceCodeArray = new ArrayList<String>();
                for (Race race : races) {
                    String raceCode = race.getRaceCode();
                    raceCodeArray.add(raceCode);
                }

                value = Boolean.valueOf(raceCodeArray.contains(field.getParameter().toString()));
            }

            return value;
        }
    }

    /**
     * The Class RetrieveSchool.
     */
    class RetrieveSchool implements FieldRetriever {
        public static final String CALC_ID = "STD_SCHOOL";

        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";

        private String m_fieldSchoolNumber;

        /**
         * Instantiates a new retrieve school.
         */
        public RetrieveSchool() {
            m_fieldSchoolNumber = FLEducationIdentifierData.super.translateAliasToJavaName(ALIAS_SCHOOL_NUMBER, true);
        }

        /**
         * Gets the field value.
         *
         * @param data the data
         * @param entity the entity
         * @param field the field
         * @return the field value
         * @throws X2BaseException the x 2 base exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            SisStudent student = (SisStudent) entity.getBean();

            StudentEnrollment enroll = m_helper.getEnrollmentForDate(student.getOid(), getCurrentContext().getEndDate(),
                    StudentEnrollment.ENTRY + StudentEnrollment.STATUS_CHANGE + StudentEnrollment.YOG_CHANGE);

            SisSchool school = null;
            if (enroll != null) {
                school = enroll.getSchool();
            } else {
                school = student.getSchool();
            }
            if (school != null) {
                value = school.getFieldValueByBeanPath(m_fieldSchoolNumber);
            }

            return value;
        }
    }

    private static final String ALIAS_FL_EDUCATION_ID = "all-psn-StateEducationId";
    private static final String PARAM_EMPTY_FLEID = "emptyFleidIndicator";

    protected StudentHistoryHelper m_helper;

    private Map<String, Collection<Race>> m_raceMap;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        Boolean fleidIndicator = (Boolean) getParameter(PARAM_EMPTY_FLEID);
        if (fleidIndicator != null && fleidIndicator.booleanValue()) {
            String fleidField = this.translateAliasToJavaName(ALIAS_FL_EDUCATION_ID, true);
            if (!StringUtils.isEmpty(fleidField)) {
                m_helper.getStudentCriteria().addEmpty(
                        SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + fleidField,
                        getBroker().getPersistenceKey());
            }
        }

        setQuery(m_helper.getStudentQuery(false));

        Map<String, FieldRetriever> calcs = new HashMap<>();
        calcs.put(RetrieveRaceFields.CALC_ID, new RetrieveRaceFields());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        addCalcs(calcs);
    }

    /**
     * Gets the student race map.
     *
     * @return the student race map
     */
    private Map<String, Collection<Race>> getStudentRaceMap() {
        if (m_raceMap == null) {
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_helper.getStudentCriteria());
            X2Criteria raceCriteria = new X2Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, studentSubQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 1000);
        }
        return m_raceMap;
    }
}
