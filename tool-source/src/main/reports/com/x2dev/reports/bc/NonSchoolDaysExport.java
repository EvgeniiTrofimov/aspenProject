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
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports non-school days information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class NonSchoolDaysExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_SCHOOL_YEAR = "School Year";
    private static final String FIELD_DAY_TYPE = "Reason";
    private static final String FIELD_DAY_IN_SESSION = "Instructional Day";
    private static final String FIELD_DAY_DATE = "Non-School Date";

    // General constants
    private static final String DATE_FORMAT = "dd-MMM-yyyy";
    private static final String PARAM_CALENDAR_TYPE = "Standard";

    private List<String> m_columns;
    private final int m_field_count = 5;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(m_field_count) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }

        };
        SimpleDateFormat formatter = new SimpleDateFormat(DATE_FORMAT);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, buildCriteria());
        query.addOrderByAscending(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID);
        query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

        QueryIterator schoolDates = getBroker().getIteratorByQuery(query);
        try {
            while (schoolDates.hasNext()) {
                SchoolCalendarDate day = (SchoolCalendarDate) schoolDates.next();
                boolean deleteRow = false;
                try {
                    grid.append();
                    deleteRow = true;
                    grid.set(FIELD_SCHOOL_NUMBER, day.getSchoolCalendar().getSchool().getSchoolId());
                    grid.set(FIELD_SCHOOL_YEAR,
                            String.valueOf(day.getSchoolCalendar().getDistrictContext().getSchoolYear()));
                    grid.set(FIELD_DAY_TYPE, day.getScheduleDayType());
                    grid.set(FIELD_DAY_IN_SESSION, day.getInSessionIndicator() ? "Y" : "N");
                    grid.set(FIELD_DAY_DATE, formatter.format(day.getDate()));
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(day.getOid());
                    strBldr.append("].");

                    // deleteRow is true if an incomplete row has been added to the grid from
                    // grid.append()
                    if (!deleteRow) {
                        strBldr.append("Null encountered before adding to export.");
                    } else {
                        strBldr.append("Null encountered when setting Columns.");
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }
            }
        } finally {
            schoolDates.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
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

        // Set columns
        m_columns = new ArrayList<String>(m_field_count);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_SCHOOL_YEAR);
        m_columns.add(FIELD_DAY_TYPE);
        m_columns.add(FIELD_DAY_IN_SESSION);
        m_columns.add(FIELD_DAY_DATE);

    }

    /**
     * Builds the criteria for returning the school calendar date records.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.REL_SCHOOL,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID));
        criteria.addNotEmpty(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, getBroker().getPersistenceKey());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_CALENDAR_ID, PARAM_CALENDAR_TYPE);

        return criteria;
    }
}
