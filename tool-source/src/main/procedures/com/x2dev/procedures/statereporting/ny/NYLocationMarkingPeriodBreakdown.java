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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.GradeTermDate;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Location Marking Period export.
 *
 * @author X2 Development Corporation
 */

public class NYLocationMarkingPeriodBreakdown extends StateReportData {
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

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            GradeTermDate termDate = (GradeTermDate) ((LocationMarkingPeriodEntity) entity).getBean();
            GradeTerm gradeTerm = termDate.getGradeTerm();
            if (gradeTerm != null) {
                // DO NOT REMOVE COMMENTED CODE
                // Currently we are using a brute force/hack to solve this problem. The intelligent
                // solution is partially
                // in the code already.
                if (PARAM_MARKING_PERIOD_CODE.equals(param)) {
                    value = getManuallyDefinedTermInfo(gradeTerm.getGradeTermId(), true);
                    // value = lookupReferenceCodeByBeanPath(GradeTerm.class,
                    // GradeTerm.COL_GRADE_TERM_ID,
                    // gradeTerm.getGradeTermId(),
                    // ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_TERM_CODE.equals(param)) {
                    value = getManuallyDefinedTermInfo(gradeTerm.getGradeTermId(), false);
                    // value = getScheduleTermFromGradeTermDate(termDate);
                }
            }
            return value;
        }
    }

    /**
     * T1- Marking period/Grade Term=1; Schedule term/Term=FYE=3
     * T2-Marking period/Grade Term=2; Schedule term/Term=FYE=3
     * T3-Marking period/Grade Term=3; Schedule term/Term=FYE=3
     *
     * High School and Middle Schools: Quarter based Grade Terms/Marking Periods; Multiple schedule
     * terms/terms-but, all based on Quarters(4xyear)
     * Q1-Grade Term/Marking Period=1; Schedule Term/Term=1
     * Q2-Grade Term/Marking Period=2; Schedule Term/Term=2
     * Q3-Grade Term/Marking Period=3; Schedule Term/Term=3
     * Q4-Grade Term/Marking Period=4; Schedule Term/Term=4
     *
     * S1-Grade Term=2, Schedule Term=2
     * S2-Grade Term=4; Schedule Term=4
     *
     * FY-Grade Term/Marking Period=4; Schedule Term/Term=4.
     *
     * @param term String
     * @param gradeTerm boolean
     * @return String
     */
    protected String getManuallyDefinedTermInfo(String term, boolean gradeTerm) {
        if (!StringUtils.isEmpty(term)) {
            String firstChar = term.substring(0, 1);
            String returnChar = term.substring(term.length() - 1, term.length());
            if ("T".equals(firstChar)) {
                if (gradeTerm) {
                    return returnChar;
                }
                return "3";
            } else if ("Q".equals(firstChar)) {
                return returnChar;
            } else if ("S".equals(firstChar)) {
                if ("1".equals(returnChar)) {
                    return "2";
                }
            }
            return "4";
        }
        return null;
    }

    /**
     * Gets the schedule term from grade term date.
     *
     * @param termDate GradeTermDate
     * @return Schedule term
     */
    protected ScheduleTerm getScheduleTermFromGradeTermDate(GradeTermDate termDate) {
        // String value = null;
        PlainDate startDate = termDate.getStartDate();
        PlainDate endDate = termDate.getEndDate();
        ScheduleTerm returnTerm = null;
        ScheduleTermDate returnTermDate = null;
        for (ScheduleTermDate term : m_scheduleTermDateList) {
            if (term.getStartDate() != null && term.getEndDate() != null) {
                if (!term.getStartDate().after(startDate) &&
                        !term.getEndDate().before(endDate)) {
                    if (returnTermDate != null) {
                        if (term.getStartDate().after(returnTermDate.getStartDate())
                                && term.getEndDate().before(returnTermDate.getEndDate())) {
                            returnTermDate = term;
                        }
                    } else {
                        returnTermDate = term;
                    }
                }
            }
        }
        returnTerm = returnTermDate.getScheduleTerm();
        // if(returnTerm != null)
        // {
        // value = lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE,
        // returnTerm.getCode(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
        // }
        return returnTerm;
    }

    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_MARKING_PERIOD_CODE = "MARKING-PERIOD-CODE";
    protected static final String PARAM_TERM_CODE = "TERM-CODE";

    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    protected String m_orgOid;
    protected String m_excludeSchool;

    protected Map<String, GradeTerm> m_gradeTermList;
    protected ArrayList<GradeTermDate> m_gradeTermDateList;
    protected Map<String, ScheduleTerm> m_scheduleTermList;
    protected ArrayList<ScheduleTermDate> m_scheduleTermDateList;
    protected ArrayList<LocationDataContainer> m_containers = new ArrayList<LocationDataContainer>();

    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
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

        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            getBuildScheduleTermDateMap();
            getBuildScheduleTermMap();
            getBuildGradeTermMap();
            getBuildGradeTermDateMap();

            matchGradeTermAndScheduleTerm();

            Criteria schoolCriteria = new Criteria();
            schoolCriteria.addNotEqualTo(School.COL_ARCHIVE_INDICATOR, BooleanAsStringConverter.TRUE);
            schoolCriteria.addNotEqualTo(School.COL_INACTIVE_INDICATOR, BooleanAsStringConverter.TRUE);

            SubQuery schoolSubQuery = new SubQuery(School.class, X2BaseBean.COL_OID, schoolCriteria);

            Criteria gradeTermDatesCriteria = new Criteria();
            gradeTermDatesCriteria.addIn(GradeTermDate.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                    + X2BaseBean.COL_OID, schoolSubQuery);

            gradeTermDatesCriteria.addGreaterOrEqualThan(GradeTermDate.COL_START_DATE,
                    getCurrentContext().getStartDate());

            applyInputCriteria(gradeTermDatesCriteria, false, null);
            if (isSchoolContext()) {
                gradeTermDatesCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
            }

            if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
                gradeTermDatesCriteria.addNotEqualTo(GradeTermDate.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                        + m_excludeSchool, BooleanAsStringConverter.TRUE);
            }
            // … create Query …
            QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, gradeTermDatesCriteria);
            applyInputSort(query, null);
            setQuery(query);
            setEntityClass(LocationMarkingPeriodEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TERM-INFO", new RetrieveTermInfo());
            HashMap validators = new HashMap<String, FieldRetriever>();

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * The Class LocationDataContainer.
     */
    protected class LocationDataContainer {
        ScheduleTerm c_scheduleTerm;
        ScheduleTermDate c_scheduleTermDate;
        GradeTerm c_gradeTerm;
        GradeTermDate c_gradeTermDate;

        /**
         * Instantiates a new location data container.
         *
         * @param st ScheduleTerm
         * @param std ScheduleTermDate
         * @param gt GradeTerm
         * @param gtd GradeTermDate
         */
        public LocationDataContainer(ScheduleTerm st, ScheduleTermDate std, GradeTerm gt, GradeTermDate gtd) {
            c_scheduleTerm = st;
            c_scheduleTermDate = std;
            c_gradeTerm = gt;
            c_gradeTermDate = gtd;
        }
    }

    /**
     * Match grade term and schedule term.
     */
    private void matchGradeTermAndScheduleTerm() {
        Iterator<GradeTermDate> gradeTermIterator = m_gradeTermDateList.iterator();
        // ArrayList<LocationDataContainer> container = new ArrayList<LocationDataContainer>();
        while (gradeTermIterator.hasNext()) {
            GradeTermDate gradeTermDate = gradeTermIterator.next();
            ScheduleTerm scheduleTerm = getScheduleTermFromGradeTermDate(gradeTermDate);
            if (scheduleTerm != null) {
                m_containers.add(new LocationDataContainer(scheduleTerm, null, null, gradeTermDate));
                m_scheduleTermList.remove(scheduleTerm.getCode());
            }
        }
        m_scheduleTermList.get(null);
    }

    /**
     * Initialize fields.
     */
    protected void initializeFields() {
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
    }


    /**
     * This method builds the marking periods for later use.
     *
     * @return void
     */
    protected void getBuildGradeTermDateMap() {
        m_gradeTermDateList = new ArrayList<GradeTermDate>();
        Criteria gradeTermDatesCriteria = new Criteria();
        gradeTermDatesCriteria.addNotEqualTo(
                GradeTermDate.REL_SCHOOL + ModelProperty.PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                BooleanAsStringConverter.TRUE);
        gradeTermDatesCriteria.addNotEqualTo(
                GradeTermDate.REL_SCHOOL + ModelProperty.PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                BooleanAsStringConverter.TRUE);

        gradeTermDatesCriteria.addGreaterOrEqualThan(GradeTermDate.COL_START_DATE, getCurrentContext().getStartDate());

        applyInputCriteria(gradeTermDatesCriteria, false, null);
        if (isSchoolContext()) {
            gradeTermDatesCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        }

        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            gradeTermDatesCriteria.addNotEqualTo(
                    GradeTermDate.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_excludeSchool,
                    BooleanAsStringConverter.TRUE);
        }

        m_gradeTermDateList = new ArrayList<GradeTermDate>();
        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, gradeTermDatesCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        while (iterator.hasNext()) {
            m_gradeTermDateList.add((GradeTermDate) iterator.next());
        }
    }

    /**
     * This method builds the marking periods for later use.
     *
     * @return void
     */
    protected void getBuildScheduleTermDateMap() {
        m_scheduleTermDateList = new ArrayList<ScheduleTermDate>();
        Criteria scheduleTermDatesCriteria = new Criteria();
        scheduleTermDatesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL
                + ModelProperty.PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR, BooleanAsStringConverter.TRUE);
        scheduleTermDatesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL
                + ModelProperty.PATH_DELIMITER + School.COL_INACTIVE_INDICATOR, BooleanAsStringConverter.TRUE);

        scheduleTermDatesCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_START_DATE,
                getCurrentContext().getStartDate());

        applyInputCriteria(scheduleTermDatesCriteria, false, null);
        if (isSchoolContext()) {
            scheduleTermDatesCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        }

        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            scheduleTermDatesCriteria.addNotEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL
                    + ModelProperty.PATH_DELIMITER + m_excludeSchool, BooleanAsStringConverter.TRUE);
        }
        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, scheduleTermDatesCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        while (iterator.hasNext()) {
            m_scheduleTermDateList.add((ScheduleTermDate) iterator.next());
        }
    }

    /**
     * This method builds the marking periods for later use.
     *
     * @return void
     */
    protected void getBuildScheduleTermMap() {
        m_scheduleTermList = new HashMap<String, ScheduleTerm>();
        Criteria scheduleTermCriteria = new Criteria();
        scheduleTermCriteria
                .addNotEqualTo(ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL +
                        ModelProperty.PATH_DELIMITER + m_excludeSchool, BooleanAsStringConverter.TRUE);
        scheduleTermCriteria.addGreaterOrEqualThan(
                ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_DISTRICT_CONTEXT +
                        ModelProperty.PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(getCurrentContext().getSchoolYear()));
        scheduleTermCriteria.addNotEqualTo(
                ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                BooleanAsStringConverter.TRUE);
        scheduleTermCriteria.addNotEqualTo(
                ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                BooleanAsStringConverter.TRUE);
        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, scheduleTermCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        while (iterator.hasNext()) {
            ScheduleTerm scheduleTerm = (ScheduleTerm) iterator.next();
            if (!m_scheduleTermList.containsKey(scheduleTerm.getCode())) {
                m_scheduleTermList.put(scheduleTerm.getCode(), scheduleTerm);
            }
        }
    }

    /**
     * This method builds the marking periods for later use.
     *
     * @return void
     */
    protected void getBuildGradeTermMap() {
        m_gradeTermList = new HashMap<String, GradeTerm>();
        Criteria gradeTermCriteria = new Criteria();
        QueryByCriteria query = new QueryByCriteria(GradeTerm.class, gradeTermCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        while (iterator.hasNext()) {
            GradeTerm gradeTerm = (GradeTerm) iterator.next();
            if (!m_gradeTermList.containsKey(gradeTerm.getGradeTermId())) {
                m_gradeTermList.put(gradeTerm.getGradeTermId(), gradeTerm);
            }
        }
    }
}
