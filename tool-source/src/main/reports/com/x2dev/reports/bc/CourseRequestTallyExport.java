/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.CourseRequest;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the Course Request Tally export that lists for each secondary school (school Type
 * = 'High/Secondary')
 * in the user's security domain, all courses of grade level '08', '09', '10', '11', '12' or 'SU'
 * offered in the
 * current or next school year with a tally of the number of students who have selected each course
 * as a primary
 * course option. Alternate course options will not be counted in both Next Year and Current Year
 * courses.
 * Cross-enrolled student's primary course options will be included in the tally.
 *
 * @author X2 Development Corporation
 */
public class CourseRequestTallyExport extends ExportJavaSource {

    /**
     * The Enum CSK_REQUEST_TALLY_FIELDS.
     */
    // Grid fields
    private enum CSK_REQUEST_TALLY_FIELDS {
        FIELD_SKL("School", "skl"), FIELD_CSK_CODE("Course Code", "cskCode"), FIELD_CSK_DESC("Course Description",
                "cskDesc"), FIELD_STD_TALLY("Tally", "stdTally");

        private String m_fieldId;
        private String m_fieldName;

        /**
         * Instantiates a new csk request tally fields.
         *
         * @param fieldName String
         * @param id String
         */
        private CSK_REQUEST_TALLY_FIELDS(String fieldName, String id) {
            m_fieldName = fieldName;
            m_fieldId = id;
        }

        /**
         * Gets the field id.
         *
         * @return String
         */
        public String getFieldId() {
            return m_fieldId;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }
    }

    /*
     * Included grade levels
     */
    private static final String[] GRADE_LEVELS =
            {"Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Grade SU", "08", "09", "10", "11", "12"};

    /*
     * General constants
     */
    private static final String SCHOOL_CODE_HIGH = "HighSecondary";
    private static final String OPTION_CURRENT_YEAR = "currYear";
    private static final String OPTION_SCHEDULED = "scheduled";
    private static final String PARAM_CURRENT_YEAR = "yearSelection";
    private static final String PARAM_SCHEDULED = "scheduledSelection";


    private Collection<String> m_schoolOids;

    /**
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();
        ReportQueryByCriteria reqQuery = getCourseRequestQuery();

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reqQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String schoolName = (String) row[1];
                String courseNum = (String) row[2];
                String courseName = (String) row[3];
                String tally = row[4].toString();

                grid.append();
                grid.set(CSK_REQUEST_TALLY_FIELDS.FIELD_SKL.getFieldId(), schoolName);
                grid.set(CSK_REQUEST_TALLY_FIELDS.FIELD_CSK_CODE.getFieldId(), courseNum);
                grid.set(CSK_REQUEST_TALLY_FIELDS.FIELD_CSK_DESC.getFieldId(), courseName);
                grid.set(CSK_REQUEST_TALLY_FIELDS.FIELD_STD_TALLY.getFieldId(), tally);
            }
        } finally {
            iterator.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columnNames = new ArrayList(CSK_REQUEST_TALLY_FIELDS.values().length);

        for (CSK_REQUEST_TALLY_FIELDS field : CSK_REQUEST_TALLY_FIELDS.values()) {
            columnNames.add(field.getFieldId());
        }

        return columnNames;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columnUserNames = new ArrayList(CSK_REQUEST_TALLY_FIELDS.values().length);

        for (CSK_REQUEST_TALLY_FIELDS field : CSK_REQUEST_TALLY_FIELDS.values()) {
            columnUserNames.add(field.getFieldName());
        }

        return columnUserNames;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setLineSeparator(FORMAT_EOL_WINDOWS);
        setUseValueWrappers(false);

        m_schoolOids = getSchoolOids();
    }

    /**
     * Returns collection with export school oids.
     *
     * @return Collection<String>
     */
    private Collection<String> getSchoolOids() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            criteria.addEqualTo(SisSchool.COL_SCHOOL_TYPE_CODE, SCHOOL_CODE_HIGH);
            criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
        }

        return getBroker().getSubQueryCollectionByQuery(new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria));
    }

    /**
     * Builds the query for the course requests included in the export.
     *
     * @return ReportQueryByCriteria
     */
    private ReportQueryByCriteria getCourseRequestQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(CourseRequest.COL_SCHOOL_OID, m_schoolOids);
        criteria.addIn(CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_GRADE_LEVEL,
                Arrays.asList(GRADE_LEVELS));
        criteria.addNotEqualTo(CourseRequest.COL_ALTERNATE_INDICATOR, Boolean.TRUE);

        /*
         * Check current vs. next year
         */
        String yearParam = (String) getParameter(PARAM_CURRENT_YEAR);
        String sheduleParam = (String) getParameter(PARAM_SCHEDULED);
        if (OPTION_CURRENT_YEAR.equals(yearParam)) {
            criteria.addEqualTo(CourseRequest.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

            if (OPTION_SCHEDULED.equals(sheduleParam)) {
                /*
                 * Only include requests that are for courses that have a scheduled section
                 */
                X2Criteria sectionCriteria = new X2Criteria();
                sectionCriteria.addIn(Section.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_SCHOOL_OID, m_schoolOids);
                sectionCriteria.addEqualTo(Section.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

                SubQuery scheduledCourses =
                        new SubQuery(MasterSchedule.class, MasterSchedule.COL_SCHOOL_COURSE_OID, sectionCriteria);
                criteria.addIn(CourseRequest.COL_SCHOOL_COURSE_OID, scheduledCourses);
            }
        } else {
            int year = getCurrentContext().getSchoolYear();
            year = year + 1;
            criteria.addEqualTo(CourseRequest.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
                    DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(year));
        }

        /*
         * Define query
         */
        String[] columns = {CourseRequest.COL_SCHOOL_OID,
                CourseRequest.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER,
                CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DESCRIPTION,
                "COUNT(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(CourseRequest.class, columns, criteria);
        query.addGroupBy(CourseRequest.COL_SCHOOL_OID);
        query.addGroupBy(CourseRequest.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        query.addGroupBy(CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);
        query.addGroupBy(CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DESCRIPTION);

        query.addOrderByAscending(CourseRequest.COL_SCHOOL_OID);
        query.addOrderByAscending(CourseRequest.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
        query.addOrderByAscending(CourseRequest.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DESCRIPTION);

        return query;
    }
}
