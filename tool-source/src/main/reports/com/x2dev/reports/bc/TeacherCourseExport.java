/*
 * ====================================================================
 *
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports TeacherCourse information (unique combination of staff and school course) for BC's GDE.
 *
 * @author Follett Software Company
 */
public class TeacherCourseExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_SCHOOL_ID = "School Number";
    private static final String FIELD_TEACHER_ID = "Teacher ID";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_DEPARTMENT_DESCRIPTION = "Department";
    private static final String FIELD_COURSE_ID = "Course ID";
    private static final String FIELD_COURSE_CODE = "Course Code";
    private static final String FIELD_COURSE_TYPE = "Course Type";
    private static final String FIELD_COURSE_TITLE = "Title";
    private static final String FIELD_COURSE_DESCRIPTION = "Course Description";

    // Other constants
    private static final int FIELD_COUNT = 9;

    private List<String> m_columns;
    private Map<String, ReferenceCode> m_departmentCodeMap;
    private Map<String, Set<String>> m_teacherCourseMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();

        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, buildCriteria());
        query.addOrderByAscending(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + Staff.COL_NAME_VIEW);
        query.addOrderByAscending(ScheduleTeacher.COL_STAFF_OID);

        QueryIterator schedules = getBroker().getIteratorByQuery(query);

        try {
            while (schedules.hasNext()) {
                ScheduleTeacher schedule = (ScheduleTeacher) schedules.next();

                if (isValidSection(schedule) && !isTeacherCourseInMap(schedule)) {
                    Section section = schedule.getSection();
                    SchoolCourse course = section.getSchoolCourse();
                    Course rootCourse = course.getCourse().getRootCourse();

                    grid.append();

                    grid.set(FIELD_SCHOOL_ID, section.getSchedule().getSchool().getSchoolId());
                    grid.set(FIELD_TEACHER_ID, schedule.getStaff().getLocalId());
                    grid.set(FIELD_TEACHER_NAME, wrap(schedule.getStaff().getNameView()));
                    grid.set(FIELD_COURSE_ID, course.getOid());
                    grid.set(FIELD_COURSE_CODE, course.getNumber());
                    grid.set(FIELD_COURSE_TYPE, rootCourse.getFieldB002());
                    grid.set(FIELD_COURSE_TITLE, wrap(course.getDescription()));
                    grid.set(FIELD_COURSE_DESCRIPTION, wrap(rootCourse.getDescription()));

                    /*
                     * Lookup department reference code description, if it exists
                     */
                    String departmentCodeDescription = schedule.getSection().getSchoolCourse().getDepartmentCode();
                    if (m_departmentCodeMap.containsKey(departmentCodeDescription)) {
                        departmentCodeDescription = m_departmentCodeMap.get(departmentCodeDescription).getDescription();
                    }
                    grid.set(FIELD_DEPARTMENT_DESCRIPTION, wrap(departmentCodeDescription));
                }
            }
        } finally {
            schedules.close();
        }
        return grid;
    }

    /**
     * @see com.x2dev.reports.bc.GdeExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * @see com.x2dev.reports.bc.GdeExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * @see com.x2dev.reports.bc.GdeExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * @see com.x2dev.reports.bc.GdeExportJavaSource#getHeader()
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
        super.initialize();

        m_teacherCourseMap = new HashMap<String, Set<String>>();

        /*
         * Define fields
         */
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_SCHOOL_ID);
        m_columns.add(FIELD_TEACHER_ID);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_DEPARTMENT_DESCRIPTION);
        m_columns.add(FIELD_COURSE_ID);
        m_columns.add(FIELD_COURSE_CODE);
        m_columns.add(FIELD_COURSE_TYPE);
        m_columns.add(FIELD_COURSE_TITLE);
        m_columns.add(FIELD_COURSE_DESCRIPTION);

        /*
         * Load department picklist
         */
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dataDictionary.findDataDictionaryField(SchoolCourse.class.getName(), SchoolCourse.COL_DEPARTMENT_CODE);
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_departmentCodeMap = referenceTable.getCodeMap(getBroker());
            }
        }
    }

    /**
     * Builds the criteria for returning the schedule teacher information.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        Criteria criteria = new X2Criteria();
        criteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.COL_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);

        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL, ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_SCHOOL_OID));

        return criteria;
    }

    /**
     * Checks to see if teacher/course combination has been reported.
     *
     * @param schedule ScheduleTeacher
     * @return boolean
     */
    private boolean isTeacherCourseInMap(ScheduleTeacher schedule) {
        boolean result = true;
        boolean isStaffInMap = m_teacherCourseMap.containsKey(schedule.getStaff().getLocalId());
        boolean isNotNull = schedule.getSection() != null && schedule.getSection().getSchoolCourse() != null;

        if (!isStaffInMap && isNotNull) {
            Set<String> courses = new HashSet<String>();
            courses.add(schedule.getSection().getSchoolCourse().getOid());

            m_teacherCourseMap.put(schedule.getStaff().getLocalId(), courses);

            result = false;
        }

        if (isStaffInMap && isNotNull) {
            boolean isCourceInSet = m_teacherCourseMap.get(schedule.getStaff().getLocalId())
                    .contains(schedule.getSection().getSchoolCourse().getOid());
            if (!isCourceInSet) {
                m_teacherCourseMap.get(schedule.getStaff().getLocalId())
                        .add(schedule.getSection().getSchoolCourse().getOid());
                result = false;
            }
        }

        return result;
    }

    /**
     * Returns true if the passed teacher schedule record is properly defined (with a valid
     * associated section).
     *
     * @param staffSchedule ScheduleTeacher
     * @return boolean
     */
    private boolean isValidSection(ScheduleTeacher staffSchedule) {
        return staffSchedule.getSection() != null && staffSchedule.getSection().getSchedule() != null;
    }

}
