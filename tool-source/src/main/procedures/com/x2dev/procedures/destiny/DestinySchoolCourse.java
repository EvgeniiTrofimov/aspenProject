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
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export format procedure to export School Course information to Destiny.
 *
 * @author X2 Development Corporation
 */
public class DestinySchoolCourse extends StateReportData {
    /**
     * Statics for School parameter and beanOid parameter.
     */
    private static final String SCHOOL_PARAMETER = "SCHOOL";
    private static final String BEANOID_PARAMETER = "BEANOID";

    /**
     * The Class SchoolCourseEntity.
     *
     * @author X2 Development Corporation
     */
    public static class SchoolCourseEntity extends StateReportEntity {
        DestinySchoolCourse m_schoolCourse = null;

        /**
         * Instantiates a new school course entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public SchoolCourseEntity() {
            // public no argument constructor for dynamic instantiation.
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

            m_schoolCourse = (DestinySchoolCourse) data;
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
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @SuppressWarnings("rawtypes")
    @Override
    public Class getBeanClass() {
        return SchoolCourse.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "DestinySchoolCourse";
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
     * Returns criteria finding the school courses associated with the current district
     * context.
     *
     * @param school School
     * @return Criteria
     */
    private Criteria getCourseCriteria(School school) {
        X2Criteria criteria = new X2Criteria();

        if (school != null) {
            criteria.addEqualTo(SchoolCourse.COL_SCHOOL_OID, school.getOid());
        }

        criteria.addEqualTo(SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + Course.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());

        if (!StringUtils.isEmpty(m_beanOid)) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_beanOid);
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
            School school = null;

            if (getParameter(SCHOOL_PARAMETER) != null) {
                school = (School) getBroker().getBeanByOid(School.class, (String) getParameter(SCHOOL_PARAMETER));
            }

            if (getParameter(BEANOID_PARAMETER) != null) {
                m_beanOid = (String) getParameter(BEANOID_PARAMETER);
            }

            /*
             * Build query object that will be used to retrieve export students.
             */
            QueryByCriteria schoolCourseQuery = new QueryByCriteria(SchoolCourse.class, getCourseCriteria(school));

            // Set the query to be used for selection.
            setQuery(schoolCourseQuery);
            setEntityClass(SchoolCourseEntity.class);

            // Add any retrievers or validators.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            super.addCalcs(calcs);
        }
    }
}
