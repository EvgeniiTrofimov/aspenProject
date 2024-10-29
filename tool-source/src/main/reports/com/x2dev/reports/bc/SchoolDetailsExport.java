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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.x2dev.sis.model.beans.SchoolRoom;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Exports school details for BC's GDE.
 *
 * @author Follett Software Company
 */
public class SchoolDetailsExport extends GdeExportJavaSource {
    // Grid fields
    private static final String FIELD_DISTRICT_NUMBER = "District Number";
    private static final String FIELD_DISTRICT_NAME = "District Name";
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_SCHOOL_NAME = "School Name";
    private static final String FIELD_SCHOOL_MINISTRY_NUMBER = "Ministry School Number";
    private static final String FIELD_SCHOOL_TYPE = "School Configuration";
    private static final String FIELD_SCHOOL_ROOMS_NUMBER = "Number of Rooms";

    private List<String> m_columns;
    private final int m_fieldCount = 7;
    private Map<String, String> m_roomCountMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(m_fieldCount) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        Criteria criteria = getSchoolCriteria();
        loadRoomCount();

        QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
        QueryIterator schools = getBroker().getIteratorByQuery(query);
        try {
            while (schools.hasNext()) {
                SisSchool school = (SisSchool) schools.next();
                boolean deleteRow = false;
                try {
                    grid.append();
                    deleteRow = true;

                    grid.set(FIELD_DISTRICT_NUMBER, school.getParentOrganization().getId());
                    grid.set(FIELD_DISTRICT_NAME, school.getParentOrganization().getName());
                    grid.set(FIELD_SCHOOL_NUMBER, school.getSchoolId());
                    grid.set(FIELD_SCHOOL_NAME, school.getName());
                    grid.set(FIELD_SCHOOL_MINISTRY_NUMBER, school.getSchoolId());
                    grid.set(FIELD_SCHOOL_TYPE, school.getSchoolTypeCode());
                    grid.set(FIELD_SCHOOL_ROOMS_NUMBER, m_roomCountMap.get(school.getOid()));
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(school.getOid());
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
            schools.close();
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
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        // Set columns
        m_columns = new ArrayList<String>(m_fieldCount);
        m_columns.add(FIELD_DISTRICT_NUMBER);
        m_columns.add(FIELD_DISTRICT_NAME);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_SCHOOL_NAME);
        m_columns.add(FIELD_SCHOOL_MINISTRY_NUMBER);
        m_columns.add(FIELD_SCHOOL_TYPE);
        m_columns.add(FIELD_SCHOOL_ROOMS_NUMBER);
    }

    /**
     * Loads the number of rooms per school.
     */
    private void loadRoomCount() {
        m_roomCountMap = new HashMap<String, String>(4096);

        /*
         * Build criteria
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(SchoolRoom.REL_SCHOOL, SchoolRoom.COL_SCHOOL_OID));

        /*
         * Generate query
         */
        String[] columns = new String[] {SchoolRoom.COL_SCHOOL_OID, "COUNT(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SchoolRoom.class, columns, criteria);
        query.addGroupBy(SchoolRoom.COL_SCHOOL_OID);

        /*
         * Iterate over records and populate map
         */
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String schoolOid = (String) row[0];
                String count = row[1].toString();

                m_roomCountMap.put(schoolOid, count);
            }
        } finally {
            iterator.close();
        }
    }
}
