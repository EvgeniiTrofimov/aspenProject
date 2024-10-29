/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentScheduleGroup;
import com.x2dev.sis.model.business.ElementaryScheduleManager;
import com.x2dev.sis.web.schedule.ScheduleUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * These report displays the student program assignment.
 *
 * @author X2 Development Corporation
 */
public class ElementaryStudentProgramVerificationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the enumerated "filter" report parameter. The value is an Integer.
     */
    public static final String FILTER_BY_PARAM = "filterBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    // report param
    public static final String SCHOOL_YEAR_CONTEXT = "schoolYearContext";

    // Data grid columns
    private static final String COL_STUDENT_NAME = "studentName";
    private static final String COL_STUDENT_GRADE_LEVEL = "studentGradeLevel";
    private static final String COL_STUDENT_PLATOON = "studentPlatoon";
    private static final String COL_STUDENT_ID = "studentId";
    private static final String COL_STUDENT_HOMEROOM = "studentHomeroom";
    private static final String COL_STUDENT_PROGRAM = "studentProgramAssigned";

    private static final String CONSTANT_NOT_ASSIGNED = "Not Assigned";
    private static final String NEXT_GRADE_ALIAS = "Next Grade";

    private Schedule m_schedule;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {

        // Get the selection criteria from the user input
        X2Criteria additionStudentCriteria = new X2Criteria();
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0: // All
                // No additional criteria (this is the case for "All")
                break;

            case 1: // YOG
                additionStudentCriteria.addEqualTo(SisStudent.COL_YOG, getParameter(QUERY_STRING_PARAM));
                break;

            case 2: // Guidance
                DataDictionaryField counselorField = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(Student.ALIAS_COUNSELOR);
                if (counselorField != null) {
                    additionStudentCriteria.addEqualTo(counselorField.getJavaName(), getParameter(QUERY_STRING_PARAM));
                }
                break;

            case 3: // Homeroom
                String studentHomeRoomField = ElementaryScheduleManager.getStudentHomeroomField(m_schedule);
                additionStudentCriteria.addEqualTo(studentHomeRoomField, getParameter(QUERY_STRING_PARAM));
                break;

            default:
                // No additional criteria (this is the case for "All")
                break;
        }

        int filterBy = ((Integer) getParameter(FILTER_BY_PARAM)).intValue();

        /*
         * Gather all the student schedule group
         */
        X2Criteria studentScheduleGroup = new X2Criteria();
        studentScheduleGroup.addEqualTo(StudentScheduleGroup.COL_DISTRICT_CONTEXT_OID,
                m_schedule.getDistrictContextOid());
        studentScheduleGroup.addEqualTo(StudentScheduleGroup.COL_SCHOOL_OID, m_schedule.getSchoolOid());

        QueryByCriteria studentGroupQuery = new QueryByCriteria(StudentScheduleGroup.class, studentScheduleGroup);
        studentGroupQuery.addOrderByAscending(StudentScheduleGroup.COL_PARENT_STUDENT_SCHEDULE_GROUP_OID);
        Collection<StudentScheduleGroup> scheduleGroups = getBroker().getCollectionByQuery(studentGroupQuery);

        ReportDataGrid grid = new ReportDataGrid(1000, 20);
        for (StudentScheduleGroup group : scheduleGroups) {

            X2Criteria studentInGroupCriteria = ElementaryScheduleManager.getStudentScheduleGroupMemberCriteria(
                    m_schedule, group, false);

            if (!additionStudentCriteria.isEmpty()) {
                studentInGroupCriteria.addAndCriteria(additionStudentCriteria);
            }

            QueryByCriteria studentsInGroupQuery = new QueryByCriteria(SisStudent.class, studentInGroupCriteria);
            Collection<SisStudent> students = getBroker().getCollectionByQuery(studentsInGroupQuery);

            for (SisStudent student : students) {

                if (filterBy == 0 || (filterBy == 1 && group.getProgram() == null)) {
                    grid.append();

                    grid.set(COL_STUDENT_PROGRAM,
                            group.getProgram() == null ? CONSTANT_NOT_ASSIGNED : group.getProgram().getNumber());
                    grid.set(COL_STUDENT_ID, student.getLocalId());
                    grid.set(COL_STUDENT_NAME, student.getNameView());
                    grid.set(COL_STUDENT_PLATOON, group.getParentStudentScheduleGroup() != null ? group.getId() : "");
                    if (m_schedule.getDistrictContextOid()
                            .equals(m_schedule.getSchool().getOrganization1().getCurrentContextOid())) {
                        grid.set(COL_STUDENT_HOMEROOM, student.getHomeroom());
                        grid.set(COL_STUDENT_GRADE_LEVEL, student.getGradeLevel());
                    } else {
                        grid.set(COL_STUDENT_HOMEROOM, student.getNextHomeroom());
                        grid.set(COL_STUDENT_GRADE_LEVEL, student.getFieldValueByAlias(NEXT_GRADE_ALIAS));
                    }
                }
            }
        }

        // Get the sort order from the user input
        List<String> columns = new ArrayList<String>();
        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // Name
                columns.add(COL_STUDENT_NAME);
                columns.add(COL_STUDENT_ID);
                break;

            case 1: // Grade Level
                columns.add(COL_STUDENT_GRADE_LEVEL);
                columns.add(COL_STUDENT_NAME);
                break;

            default:
                columns.add(COL_STUDENT_NAME);
                columns.add(COL_STUDENT_ID);
                break;
        }
        grid.sort(columns, true);
        grid.beforeTop();

        addParameter(SCHOOL_YEAR_CONTEXT, m_schedule.getDistrictContext());
        return grid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {

        m_schedule = ScheduleUtils.getSchedule(userData);
    }
}
