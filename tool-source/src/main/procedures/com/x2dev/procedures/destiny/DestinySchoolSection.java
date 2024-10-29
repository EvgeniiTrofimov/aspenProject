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

package com.x2dev.procedures.destiny;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.GuidManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export format procedure to export Schedule Master information to Destiny.
 *
 * @author X2 Development Corporation
 */
public class DestinySchoolSection extends StateReportData {
    private static final String SCHOOL_PARAMETER = "SCHOOL";
    private static final String BEANOID_PARAMETER = "BEANOID";
    private static final String EXPRESSION_FIELD_PARAMETER = "EXPRESSION_FIELD";

    private static final String SCHEDULE_DISPLAY_CALC_ID = "SCHEDULEDISPLAY";
    private static final String START_DATE_CALC_ID = "STARTDATE";
    private static final String END_DATE_CALC_ID = "ENDDATE";
    private static final String OID_CALC_ID = "OID";
    private static final String COURSE_VIEW_CALC_ID = "COURSEVIEW";

    /**
     * The Class SchoolSectionEntity.
     *
     * @author X2 Development Corporation
     */
    public static class SchoolSectionEntity extends StateReportEntity {
        DestinySchoolSection m_schoolSection = null;
        List<ScheduleSection> m_sections = new ArrayList<ScheduleSection>();

        /**
         * Instantiates a new school section entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public SchoolSectionEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Get the current schedule section for the current row.
         *
         * @return Schedule section
         */
        public ScheduleSection getScheduleSection() {
            return m_sections.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_schoolSection = (DestinySchoolSection) data;

            MasterSchedule master = (MasterSchedule) bean;

            if (master.getScheduleTerm() != null) {
                List<ScheduleTermDate> termDates =
                        (List<ScheduleTermDate>) master.getScheduleTerm().getScheduleTermDates();

                if (termDates.size() > 1) {
                    Comparator comparator = new Comparator() {
                        @Override
                        public int compare(Object o1, Object o2) {
                            ScheduleTermDate term1 = (ScheduleTermDate) o1;
                            ScheduleTermDate term2 = (ScheduleTermDate) o2;

                            return term1.getStartDate().compareTo(term2.getStartDate());
                        }
                    };
                    Collections.sort((List) termDates, comparator);
                    int count = 0;
                    for (ScheduleTermDate termDate : termDates) {
                        ScheduleSection section = new ScheduleSection(master.getOid().concat(String.valueOf(count)),
                                master.getCourseView().concat("-").concat(String.valueOf(count)),
                                (String) master.getFieldValueByBeanPath(
                                        (String) data.getParameter(EXPRESSION_FIELD_PARAMETER)),
                                termDate.getStartDate(), termDate.getEndDate());
                        m_sections.add(section);
                        count++;
                    }
                } else if (termDates.size() == 1 && !master.hasSameScheduleCrossTerm(data.getBroker())) {
                    ScheduleTermDate termDate = termDates.get(0);
                    int count = 0;
                    while (count < master.getMasterTerms().size()) {
                        ScheduleSection section = new ScheduleSection(master.getOid().concat(String.valueOf(count)),
                                master.getCourseView().concat("-").concat(String.valueOf(count)),
                                (String) master.getFieldValueByBeanPath(
                                        (String) data.getParameter(EXPRESSION_FIELD_PARAMETER)),
                                termDate.getStartDate(), termDate.getEndDate());
                        m_sections.add(section);
                        count++;
                    }
                } else if (termDates.size() == 1 && master.hasSameScheduleCrossTerm(data.getBroker())) {
                    ScheduleTermDate termDate = termDates.get(0);
                    ScheduleSection section = new ScheduleSection(master.getOid(),
                            master.getCourseView(),
                            (String) master
                                    .getFieldValueByBeanPath((String) data.getParameter(EXPRESSION_FIELD_PARAMETER)),
                            termDate.getStartDate(), termDate.getEndDate());
                    m_sections.add(section);
                } else {
                    ScheduleSection section = new ScheduleSection(master.getOid(),
                            master.getCourseView(),
                            (String) master
                                    .getFieldValueByBeanPath((String) data.getParameter(EXPRESSION_FIELD_PARAMETER)),
                            master.getSchedule().getStartDate(), master.getSchedule().getEndDate());
                    m_sections.add(section);
                }
            } else {
                ScheduleSection section = new ScheduleSection(master.getOid(),
                        master.getCourseView(),
                        (String) master.getFieldValueByBeanPath((String) data.getParameter(EXPRESSION_FIELD_PARAMETER)),
                        master.getSchedule().getStartDate(), master.getSchedule().getEndDate());
                m_sections.add(section);
            }

            setRowCount(m_sections.size());
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
    }

    protected String m_beanOid;

    /**
     *
     * Inner class to possibly break apart multi-term sections into
     * multiple sections.
     */
    protected static class ScheduleSection {
        String m_oid;
        String m_courseView;
        String m_scheduleDisplay;
        PlainDate m_startDate;
        PlainDate m_endDate;

        /**
         * Instantiates a new schedule section.
         *
         * @param oid String
         * @param courseView String
         * @param scheduleDisplay String
         * @param startDate PlainDate
         * @param endDate PlainDate
         */
        protected ScheduleSection(String oid, String courseView, String scheduleDisplay, PlainDate startDate,
                PlainDate endDate) {
            m_oid = oid;
            m_courseView = courseView;
            m_scheduleDisplay = scheduleDisplay;
            m_startDate = startDate;
            m_endDate = endDate;
        }

        /**
         * Gets the course view.
         *
         * @return courseView
         */
        public String getCourseView() {
            return m_courseView;
        }

        /**
         * Gets the oid.
         *
         * @return oid
         */
        public String getOid() {
            return m_oid;
        }

        /**
         * Gets the schedule display.
         *
         * @return scheduleDisplay
         */
        public String getScheduleDisplay() {
            return m_scheduleDisplay;
        }

        /**
         * Gets the start date.
         *
         * @return startDate
         */
        public PlainDate getStartDate() {
            return m_startDate;
        }

        /**
         * Gets the end date.
         *
         * @return endDate
         */
        public PlainDate getEndDate() {
            return m_endDate;
        }
    }


    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @SuppressWarnings("rawtypes")
    @Override
    public Class getBeanClass() {
        return MasterSchedule.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "DestinySchoolSection";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Returns criteria finding the schedule masters associated with the current district
     * context and the current active schedule.
     *
     * @param school SisSchool
     * @return Criteria
     */
    private Criteria getSectionCriteria(SisSchool school) {
        X2Criteria criteria = new X2Criteria();

        if (school != null) {
            criteria.addEqualTo(MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    school.getActiveScheduleOid());
        } else {
            criteria.addEqualTo(
                    MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
        }

        if (!StringUtils.isEmpty(m_beanOid)) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_beanOid);
        }

        return criteria;
    }

    /**
     * Initialize the) data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void initialize() {
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            SisSchool school = null;

            if (getParameter(SCHOOL_PARAMETER) != null) {
                school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, (String) getParameter(SCHOOL_PARAMETER));
            }
            if (getParameter(BEANOID_PARAMETER) != null) {
                m_beanOid = (String) getParameter(BEANOID_PARAMETER);
            }

            /*
             * Build query object that will be used to retrieve export students.
             */
            QueryByCriteria sectionQuery = new QueryByCriteria(MasterSchedule.class, getSectionCriteria(school));

            // Set the query to be used for selection.
            setQuery(sectionQuery);
            setEntityClass(SchoolSectionEntity.class);


            // Add any retrievers or validators.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(COURSE_VIEW_CALC_ID, new RetrieveCourseView());
            calcs.put(SCHEDULE_DISPLAY_CALC_ID, new RetrieveScheduleDisplay());
            calcs.put(START_DATE_CALC_ID, new RetrieveStartDate());
            calcs.put(END_DATE_CALC_ID, new RetrieveEndDate());
            calcs.put(OID_CALC_ID, new RetrieveOid());
            super.addCalcs(calcs);
        }
    }

    /**
     * Retriever class to retrieve the courseview for the master schedule object.
     */
    protected class RetrieveCourseView implements FieldRetriever {

        /**
         * Returns the Schedule Display string for the Master Schedule.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = "";
            ScheduleSection section = ((SchoolSectionEntity) entity).getScheduleSection();
            value = section.getCourseView();
            return value;
        }
    }

    /**
     * Retriever class to retrieve the oid for the master schedule object.
     */
    protected class RetrieveOid implements FieldRetriever {

        /**
         * Returns the Schedule Display string for the Master Schedule.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = "";
            ScheduleSection section = ((SchoolSectionEntity) entity).getScheduleSection();
            value = GuidManager.oidToGuid(section.getOid());
            return value;
        }
    }


    /**
     * Retriever class to retrieve the schedule display for the master schedule object.
     */
    protected class RetrieveScheduleDisplay implements FieldRetriever {

        /**
         * Returns the Schedule Display string for the Master Schedule.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = "";
            ScheduleSection section = ((SchoolSectionEntity) entity).getScheduleSection();
            value = section.getScheduleDisplay();
            return value;
        }
    }

    /**
     * Retriever class to return the start date for the schedule section.
     */
    protected class RetrieveStartDate implements FieldRetriever {

        /**
         * Returns the start date of this section. If there is more than one term then we
         * return the earliest term start date.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            ScheduleSection section = ((SchoolSectionEntity) entity).getScheduleSection();
            value = section.getStartDate();
            return value;
        }
    }

    /**
     * Retriever class to retrieve end date of the schedule section.
     *
     */
    protected class RetrieveEndDate implements FieldRetriever {

        /**
         * Returns the end date of this section. If there are multiple terms then it will return the
         * latest
         * end date of the set of terms.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            ScheduleSection section = ((SchoolSectionEntity) entity).getScheduleSection();
            value = section.getEndDate();
            return value;
        }
    }
}
