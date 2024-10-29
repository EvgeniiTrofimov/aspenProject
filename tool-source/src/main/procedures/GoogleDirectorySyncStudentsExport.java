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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.LoggerUtils;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java class that export school(s) students for Google AppsDirectorySync.(Google class room)
 *
 * @author Follett Software Company
 */

public class GoogleDirectorySyncStudentsExport extends ExportJavaSource {
    private static final String COLUMN_SCHOOL_ID = "school_id";
    private static final String COLUMN_STUDENT_ID = "student_id";
    private static final String COLUMN_FIRST_NAME = "first_name";
    private static final String COLUMN_LAST_NAME = "last_name";
    private static final String COLUMN_GRADE_LEVEL = "grade_level";
    private static final String COLUMN_USER_NAME = "user_name";


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

        if (!StringUtils.isEmpty(schoolOid)) {
            criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, schoolOid);
        }
        X2Criteria enrollmentStatus = new X2Criteria();
        enrollmentStatus.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, PreferenceManager
                .getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE));
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE_NO_PRIMARY));
        enrollmentStatus.addOrCriteria(orCriteria);
        criteria.addAndCriteria(enrollmentStatus);

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                try {
                    addBeanToGrid(grid, student);
                } catch (Exception e) {
                    logValidationError(student, LoggerUtils.convertThrowableToString(e));
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
        m_columns.add(COLUMN_STUDENT_ID);
        m_columns.add(COLUMN_FIRST_NAME);
        m_columns.add(COLUMN_LAST_NAME);
        m_columns.add(COLUMN_GRADE_LEVEL);
        m_columns.add(COLUMN_USER_NAME);
    }

    /**
     * Adds the necessary values from the given bean to a new record in the grid.
     *
     * @param grid DataGrid
     * @param bean SisStudent
     * @throws Exception exception
     */
    private void addBeanToGrid(DataGrid grid, SisStudent bean) throws Exception {
        grid.append();
        grid.set(COLUMN_SCHOOL_ID, bean.getSchool().getOid());
        grid.set(COLUMN_STUDENT_ID, bean.getLocalId());
        grid.set(COLUMN_FIRST_NAME, bean.getPerson().getFirstName());
        grid.set(COLUMN_LAST_NAME, bean.getPerson().getLastName());
        grid.set(COLUMN_GRADE_LEVEL, bean.getGradeLevel());
        grid.set(COLUMN_USER_NAME, bean.getPerson().getUser().getLoginName());
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
     * @param studnet SisStudent
     * @param message String
     */
    private void logValidationError(SisStudent studnet, String message) {
        logValidationError(studnet.getOid() + ":" + message);
    }
}
