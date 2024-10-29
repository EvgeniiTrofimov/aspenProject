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
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export format procedure to export Section Enrollment information to Destiny.
 *
 * @author X2 Development Corporation
 */
public class DestinySectionEnrollment extends StateReportData {
    /**
     * Static for the school parameter to determine school for export.
     */
    private static final String SCHOOL_PARAMETER = "SCHOOL";
    private static final String OID_CALC_ID = "OID";

    /**
     * The Class SchoolSectionEntity.
     *
     * @author X2 Development Corporation
     */
    public static class SchoolSectionEntity extends StateReportEntity {
        DestinySectionEnrollment m_sectionEnrollment = null;
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

            m_sectionEnrollment = (DestinySectionEnrollment) data;
            StudentSchedule schedule = (StudentSchedule) bean;

            if (schedule.getSection().getScheduleTerm() != null) {
                List<ScheduleTermDate> termDates =
                        (List<ScheduleTermDate>) schedule.getSection().getScheduleTerm().getScheduleTermDates();

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
                    while (count < termDates.size()) {
                        ScheduleSection section =
                                new ScheduleSection(schedule.getSection().getOid().concat(String.valueOf(count)));
                        m_sections.add(section);
                        count++;
                    }

                } else if (termDates.size() == 1) {
                    ScheduleSection section = new ScheduleSection(schedule.getSection().getOid());
                    m_sections.add(section);
                } else {
                    ScheduleSection section = new ScheduleSection(schedule.getSection().getOid());
                    m_sections.add(section);
                }
            } else {
                ScheduleSection section = new ScheduleSection(schedule.getSection().getOid());
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

    /**
     *
     * Inner class to possibly break apart multi-term sections into
     * multiple sections.
     */
    protected static class ScheduleSection {
        String m_oid;

        /**
         * Instantiates a new schedule section.
         *
         * @param oid String
         */
        protected ScheduleSection(String oid) {
            m_oid = oid;
        }

        /**
         * Gets the oid.
         *
         * @return oid
         */
        public String getOid() {
            return m_oid;
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
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @SuppressWarnings("rawtypes")
    @Override
    public Class getBeanClass() {
        return StudentSchedule.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "DestinySectionEnrollment";
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
     * Returns criteria finding the student schedules associated with the current district
     * context and the current active schedule.
     *
     * @param school SisSchool
     * @return Criteria
     */
    private Criteria getSectionEnrollmentCriteria(SisSchool school) {
        X2Criteria criteria = new X2Criteria();

        if (school != null) {
            criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    school.getActiveScheduleOid());
        } else {
            criteria.addEqualTo(
                    StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
        }

        return criteria;
    }

    /**
     * Initialize the data module.
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

            /*
             * Build query object that will be used to retrieve export students.
             */
            QueryByCriteria studentScheduleQuery =
                    new QueryByCriteria(StudentSchedule.class, getSectionEnrollmentCriteria(school));

            // Set the query to be used for selection.
            setQuery(studentScheduleQuery);
            setEntityClass(SchoolSectionEntity.class);

            // Add any retrievers or validators.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(OID_CALC_ID, new RetrieveOid());
            super.addCalcs(calcs);
        }
    }
}
