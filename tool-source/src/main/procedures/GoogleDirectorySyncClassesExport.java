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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.LoggerUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java class that export school(s) class for Google AppsDirectorySync.(Google class room)
 *
 * @author Follett Software Company
 */

public class GoogleDirectorySyncClassesExport extends ExportJavaSource {
    private static final String COLUMN_SCHOOL_ID = "school_id";
    private static final String COLUMN_CLASS_ID = "class_id";
    private static final String COLUMN_COURSE_NAME = "course_name";
    private static final String COLUMN_SECTION_NAME = "section_name";
    private static final String COLUMN_PERIOID_NAME = "period_name";
    private static final String COLUMN_STAFF_NAME = "staff_id";

    private StringBuilder m_validationErrors;
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

        if (StringUtils.isEmpty(schoolOid)) {
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

        criteria.addNotEmpty(MasterSchedule.COL_STAFF_VIEW, getBroker().getPersistenceKey());
        criteria.addEqualTo(
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        QueryByCriteria query = new QueryByCriteria(MasterSchedule.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                MasterSchedule masterSchedule = (MasterSchedule) iterator.next();
                try {
                    addBeanToGrid(grid, masterSchedule);
                } catch (Exception e) {
                    logValidationError(masterSchedule, LoggerUtils.convertThrowableToString(e));
                }
            }
        } finally {
            iterator.close();
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
        m_validationErrors = new StringBuilder(512);
        /*
         * Load the columns
         */
        m_columns = new ArrayList<String>();
        m_columns.add(COLUMN_SCHOOL_ID);
        m_columns.add(COLUMN_CLASS_ID);
        m_columns.add(COLUMN_COURSE_NAME);
        m_columns.add(COLUMN_SECTION_NAME);
        m_columns.add(COLUMN_PERIOID_NAME);
        m_columns.add(COLUMN_STAFF_NAME);
    }

    /**
     * Adds the necessary values from the given bean to a new record in the grid.
     *
     * @param grid DataGrid
     * @param bean MasterSchedule
     * @throws Exception exception
     */
    private void addBeanToGrid(DataGrid grid, MasterSchedule bean) throws Exception {
        grid.append();

        grid.set(COLUMN_SCHOOL_ID, bean.getSchoolCourse().getSchool().getSchoolId());
        grid.set(COLUMN_CLASS_ID, bean.getCourseView());
        grid.set(COLUMN_COURSE_NAME, bean.getDescription());
        grid.set(COLUMN_SECTION_NAME, bean.getCourseView());
        if (!StringUtils.isEmpty(bean.getScheduleTrackId())) {
            grid.set(COLUMN_PERIOID_NAME, bean.getScheduleTrackId());
        } else {
            grid.set(COLUMN_PERIOID_NAME, bean.getScheduleDisplay());
        }
        grid.set(COLUMN_STAFF_NAME, bean.getPrimaryStaff().getPerson().getUser().getLoginName());

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

    /**
     * Logs a validation error with the given message.
     *
     * @param message String
     */
    private void logValidationError(String message) {
        m_validationErrors.append(message);
        m_validationErrors.append('\n');
    }

    /**
     * Logs a validation error with the given student schedule.
     *
     * @param masterSchedule MasterSchedule
     * @param message String
     */
    private void logValidationError(MasterSchedule masterSchedule, String message) {
        logValidationError(masterSchedule.getOid() + ":" + message);
    }
}
