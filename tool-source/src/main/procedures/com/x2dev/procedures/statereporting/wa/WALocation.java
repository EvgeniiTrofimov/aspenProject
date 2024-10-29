/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.wa;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 *
 * This class implements the data export for WA Location export.
 *
 * @author X2 Development Corporation
 *
 */
public class WALocation extends StateReportData {
    /**
     * Entity class for WA Location export.
     *
     */
    public static class LocationEntity extends StateReportEntity {

        List<PlainDate> termStartDates = new ArrayList<PlainDate>();
        List<PlainDate> termEndDates = new ArrayList<PlainDate>();

        /**
         * Safe method to get DOE SCHOOL CODE parameter of a school.
         *
         * @param school SisSchool
         * @return String
         */
        private static String getDoeSchoolCode(SisSchool school) {
            String res = "";
            if (school != null) {
                Object ocode = school.getFieldValueByAlias(ALIAS_SCHOOL_CODE);
                if (ocode != null) {
                    res = ocode.toString();
                }
            }
            return res;
        }

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public LocationEntity() {
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
            SisSchool school = (SisSchool) getBean();
            String name = school.getName();
            String code = getDoeSchoolCode(school);
            return String.format("%1s [%2s]", name, code);
        }


        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            SisSchool school = (SisSchool) getBean();
            return school;
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
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisSchool school = (SisSchool) bean;
            Schedule activeSchedule = school.getActiveSchedule();

            if (activeSchedule != null) {
                loadSortedTermDates(activeSchedule);
                if (termStartDates != null && termStartDates.size() > 0) {
                    m_startDate = termStartDates.get(0);
                }
                if (termEndDates != null && termEndDates.size() > 0) {
                    m_endDate = termEndDates.get(0);
                }
            }
        }

        /**
         * Load sorted term dates.
         *
         * @param activeSchedule Schedule
         */
        private void loadSortedTermDates(Schedule activeSchedule) {
            Collection<ScheduleTerm> scheduleTerms = activeSchedule.getScheduleTerms();
            termStartDates = new ArrayList<PlainDate>();
            termEndDates = new ArrayList<PlainDate>();

            for (ScheduleTerm scheduleTerm : scheduleTerms) {
                Collection<ScheduleTermDate> scheduleTermDates = scheduleTerm.getScheduleTermDates();
                for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                    termStartDates.add(scheduleTermDate.getStartDate());
                    termEndDates.add(scheduleTermDate.getEndDate());
                }
            }
            Collections.sort(termEndDates, Collections.reverseOrder());
            Collections.sort(termStartDates);
        }
    }

    /*
     * Constants: Aliases
     */
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL ID";
    protected static final String PARAM_START_DATE = "START_DATE";
    protected static final String PARAM_END_DATE = "END_DATE";
    protected static PlainDate m_startDate = null;
    protected static PlainDate m_endDate = null;
    /*
     * Instance variables
     */
    protected String m_excludeSchool;
    protected String m_fieldSchoolCode;

    /*
     * User input parameters
     */
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";

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
        /*
         * Get core parameters
         */
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            setEntityClass(LocationEntity.class);

            X2Criteria criteria = new X2Criteria();
            criteria.addNotEmpty(m_fieldSchoolCode, getBroker().getPersistenceKey());

            if (getParameter(PARAM_EXCLUDE_SCHOOL) != null) {
                if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
                    criteria.addNotEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
                }
            }
            if (isSchoolContext()) {
                criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
            } else {
                criteria.addNotEqualTo(School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            }

            // add criteria from input definition
            applyInputCriteria(criteria, false, null);

            // create query - use the appropriate class
            QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
            applyInputSort(query, null);
            setQuery(query);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("LOC-COURSE", new RetrieveCourse());
            super.addCalcs(calcs);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
    }

    /**
     * The Class RetrieveCourse.
     */
    protected class RetrieveCourse implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            Object value = null;

            if (PARAM_END_DATE.equals(parameter)) {
                value = m_endDate;
            }
            if (PARAM_START_DATE.equals(parameter)) {
                value = m_startDate;
            }
            return value;
        }
    }
}
