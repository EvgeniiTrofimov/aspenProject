/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java class that export class's roster for school(s) for Google AppsDirectorySync.(Google class
 * room)
 *
 * @author Follett Software Company
 */

public class GoogleDirectorySyncRosterExport extends ExportJavaSource {
    private static final String COLUMN_SCHOOL_ID = "school_id";
    private static final String COLUMN_CLASS_ID = "class_id";
    private static final String COLUMN_STUDENT_ID = "student_id";

    private List<String> m_columns;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(m_columns.size());
        X2Criteria criteria = new X2Criteria();

        String schoolOid = (String) super.getParameter(ToolInput.SCHOOL_OID_PARAM);

        boolean isAllSchool = StringUtils.isEmpty(schoolOid);
        if (isAllSchool) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addNotNull(SisSchool.COL_ACTIVE_SCHOOL_SCHED_OID);
            SubQuery subQuery = new SubQuery(SisSchool.class, SisSchool.COL_ACTIVE_SCHOOL_SCHED_OID, schoolCriteria);
            Collection<String> activeSchoolScheOids = getBroker().getSubQueryCollectionByQuery(subQuery);
            X2Criteria schoolSchedule = new X2Criteria();
            schoolSchedule.addIn(X2BaseBean.COL_OID, activeSchoolScheOids);

            SubQuery subQuerySchoolSchedule = new SubQuery(SchoolScheduleContext.class,
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, schoolSchedule);
            criteria.addIn(MasterSchedule.COL_SCHEDULE_OID, subQuerySchoolSchedule);
        } else {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(X2BaseBean.COL_OID, schoolOid);
            SisSchool school = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
            criteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, school.getActiveScheduleOid());
        }

        criteria.addNotEmpty(MasterSchedule.COL_PRIMARY_STAFF_OID, getBroker().getPersistenceKey());
        criteria.addEqualTo(
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);


        SubQuery subQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, criteria);
        Collection<String> masterScheduleOids = getBroker().getSubQueryCollectionByQuery(subQuery);

        X2Criteria studentScheduleCriteria = new X2Criteria();
        studentScheduleCriteria.addIn(StudentSchedule.COL_SECTION_OID, masterScheduleOids);

        X2Criteria studentStatusCriteria = new X2Criteria();
        studentStatusCriteria.addEqualTo(
                StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS,
                PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE_NO_PRIMARY));
        X2Criteria studentStatusOrCriteria = new X2Criteria();
        studentStatusOrCriteria.addEqualTo(
                StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS,
                PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE));
        studentStatusCriteria.addOrCriteria(studentStatusOrCriteria);

        studentScheduleCriteria.addAndCriteria(studentStatusCriteria);

        QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);

        Collection<StudentSchedule> studentSchedules = getBroker().getCollectionByQuery(studentScheduleQuery);

        for (StudentSchedule studentSchedule : studentSchedules) {
            grid.append();
            grid.set(COLUMN_SCHOOL_ID, studentSchedule.getSchedule().getSchool().getSchoolId());
            grid.set(COLUMN_CLASS_ID, studentSchedule.getSection().getCourseView());
            grid.set(COLUMN_STUDENT_ID, studentSchedule.getStudent().getLocalId());
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setIncludeHeaderRow(true);
        /*
         * Load the columns
         */
        m_columns = new ArrayList<String>();
        m_columns.add(COLUMN_SCHOOL_ID);
        m_columns.add(COLUMN_CLASS_ID);
        m_columns.add(COLUMN_STUDENT_ID);
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
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
}
