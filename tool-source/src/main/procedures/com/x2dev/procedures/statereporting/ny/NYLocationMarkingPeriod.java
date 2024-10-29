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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Location Marking Period export.
 *
 * @author X2 Development Corporation
 */

public class NYLocationMarkingPeriod extends StateReportData {
    /**
     * Entity class for Location Marking Period export.
     *
     * @author X2 Development Corporation
     */

    public static class LocationMarkingPeriodEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public LocationMarkingPeriodEntity() {
            // no argument constructor
        }
    }

    /**
     * Retrieves the Term Information.
     */
    protected class RetrieveTermInfo implements FieldRetriever {

        protected static final String CALC_ID = "TERM-INFO";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleTerm term = (ScheduleTerm) ((LocationMarkingPeriodEntity) entity).getBean();
            if (term != null) {
                if (PARAM_MARKING_PERIOD_CODE.equals(param)) {
                    int ival = 0;
                    if (term != null && term.getGradeTermMap() != null) {
                        int idx = 0;
                        for (char test : term.getGradeTermMap().toCharArray()) {
                            ++idx;
                            if ('1' == test) {
                                ival = idx;
                            }
                        }
                    }
                    value = ival == 0 ? null : Integer.toString(ival);
                } else if (PARAM_TERM_CODE.equals(param)) {
                    String termCode = term.getCode();
                    String stateTermCode =
                            data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (stateTermCode == null) {
                        stateTermCode = termCode;
                    }
                    value = stateTermCode;
                }
            }
            return value;
        }
    }

    /**
     * Retrieves the Term Date Information.
     */
    protected class RetrieveTermDateInfo implements FieldRetriever {

        protected static final String CALC_ID = "TERM-DATE-INFO";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleTerm term = (ScheduleTerm) ((LocationMarkingPeriodEntity) entity).getBean();
            if (term != null) {
                Collection<ScheduleTermDate> termDates = term.getScheduleTermDates();
                if (termDates != null && !termDates.isEmpty()) {
                    if (PARAM_MARKING_PERIOD_START.equals(param)) {
                        ScheduleTermDate scheduleTermDate =
                                termDates.stream().min(Comparator.comparing(ScheduleTermDate::getStartDate))
                                        .orElse(null);
                        value = scheduleTermDate != null ? scheduleTermDate.getStartDate() : null;
                    } else if (PARAM_MARKING_PERIOD_END.equals(param)) {
                        ScheduleTermDate scheduleTermDate =
                                termDates.stream().max(Comparator.comparing(ScheduleTermDate::getEndDate)).orElse(null);
                        value = scheduleTermDate != null ? scheduleTermDate.getEndDate() : null;
                    }
                }
            }
            return value;
        }
    }

    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    protected static final String PARAM_MARKING_PERIOD_CODE = "MARKING-PERIOD-CODE";
    protected static final String PARAM_MARKING_PERIOD_END = "MARKING-PERIOD-END";
    protected static final String PARAM_MARKING_PERIOD_START = "MARKING-PERIOD-START";
    protected static final String PARAM_TERM_CODE = "TERM-CODE";

    protected String m_orgOid;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()) {
            return null;
        }
        return super.getHeading();
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addNotEqualTo(School.COL_ARCHIVE_INDICATOR, BooleanAsStringConverter.TRUE);
        schoolCriteria.addNotEqualTo(School.COL_INACTIVE_INDICATOR, BooleanAsStringConverter.TRUE);
        String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        schoolCriteria.addNotEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
        if (isSchoolContext()) {
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        }

        SubQuery schoolSubQuery = new SubQuery(School.class, X2BaseBean.COL_OID, schoolCriteria);

        X2Criteria scheduleTermCriteria = new X2Criteria();
        scheduleTermCriteria.addIn(ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER
                + Schedule.COL_SCHOOL_OID, schoolSubQuery);

        scheduleTermCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER
                + Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + ModelProperty.PATH_DELIMITER
                + SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        applyInputCriteria(scheduleTermCriteria, false, null);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {

            // � create Query �
            QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, scheduleTermCriteria);
            applyInputSort(query, null);
            setQuery(query);
            setEntityClass(LocationMarkingPeriodEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveTermInfo.CALC_ID, new RetrieveTermInfo());
            calcs.put(RetrieveTermDateInfo.CALC_ID, new RetrieveTermDateInfo());
            HashMap validators = new HashMap<String, FieldRetriever>();

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }
}
