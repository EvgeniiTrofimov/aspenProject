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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainTime;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports incident information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class IncidentInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_INC_DATE = "Date Time";
    private static final String FIELD_INC_DESCRIPTION = "Description";
    private static final String FIELD_INC_TYPE = "Incident Type";
    private static final String FIELD_INC_LOCATION = "Location";
    private static final String FIELD_INC_OCCURED_TIME = "Incident Occurred Time";
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STD_GRADE = "Grade";
    private static final String FIELD_STD_HOMEROOM = "Homeroom";
    private static final String FIELD_STD_HOMEROOM_TEACHER = "Teacher Name";
    private static final String FIELD_ACT_CODE = "Action type";

    // Other constants
    private static final int FIELD_COUNT = 12;

    private Map<String, ConductAction> m_actionMap;
    private List<String> m_columns;
    private DateFormat m_timeFormatter;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        SimpleDateFormat dateFormatter = new SimpleDateFormat("MM/dd/yyyy");

        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(ConductIncident.class, buildCriteria());
        QueryIterator incidents = getBroker().getIteratorByQuery(query);
        try {
            while (incidents.hasNext()) {
                ConductIncident incident = (ConductIncident) incidents.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = incident.getStudent();

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");
                    String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());

                    grid.append();
                    deleteRow = true;

                    // Fill grid data list with export information
                    grid.set(FIELD_INC_DATE, dateFormatter.format(incident.getIncidentDate()));
                    grid.set(FIELD_INC_DESCRIPTION, incident.getDescription());
                    grid.set(FIELD_INC_TYPE, incident.getIncidentCode());
                    grid.set(FIELD_INC_LOCATION, incident.getIncidentLocation());
                    grid.set(FIELD_INC_OCCURED_TIME, formatTime(incident.getIncidentTime()));
                    grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                    grid.set(FIELD_STD_LAST_NAME, student.getPerson().getLastName());
                    grid.set(FIELD_STD_FIRST_NAME, student.getPerson().getFirstName());
                    grid.set(FIELD_STD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_STD_HOMEROOM, homeroom);

                    // Add teacher name if it exists in homeroom to staff map
                    Map<String, Staff> staffMap = getHomeroomToStaffMap(incident.getSchool());
                    if (staffMap != null) {
                        Staff staff = staffMap.get(homeroom);
                        if (staff != null) {
                            grid.set(FIELD_STD_HOMEROOM_TEACHER, staff.getNameView());
                        }
                    }

                    // Add action type for the first conduct action
                    ConductAction action = m_actionMap.get(incident.getOid());
                    if (action != null) {
                        grid.set(FIELD_ACT_CODE, action.getActionCode());
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(incident.getOid());
                    SisStudent student = incident.getStudent();
                    if (student != null) {
                        strBldr.append("] for the Student with Local ID: [");
                        strBldr.append(student.getLocalId());
                        strBldr.append("].");
                    } else {
                        strBldr.append("] as it has no related Student.");
                    }

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
            incidents.close();
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
    protected List<String> getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List<String> getColumnUserNames() {
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
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_INC_DATE);
        m_columns.add(FIELD_INC_DESCRIPTION);
        m_columns.add(FIELD_INC_TYPE);
        m_columns.add(FIELD_INC_LOCATION);
        m_columns.add(FIELD_INC_OCCURED_TIME);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_LAST_NAME);
        m_columns.add(FIELD_STD_FIRST_NAME);
        m_columns.add(FIELD_STD_GRADE);
        m_columns.add(FIELD_STD_HOMEROOM);
        m_columns.add(FIELD_STD_HOMEROOM_TEACHER);
        m_columns.add(FIELD_ACT_CODE);

        m_timeFormatter = new SimpleDateFormat("hh:mm a");
    }

    /**
     * Builds export criteria and load action lookup map.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria incidentCriteria = new X2Criteria();
        X2Criteria actionCriteria = new X2Criteria();

        /*
         * Restrict by school
         */
        incidentCriteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(ConductIncident.REL_SCHOOL, ConductIncident.COL_SCHOOL_OID));
        actionCriteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.REL_SCHOOL,
                ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_SCHOOL_OID));

        /*
         * Restrict by date range
         */
        DistrictSchoolYearContext context = getCurrentContext();

        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, context.getStartDate());
        incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, context.getEndDate());
        actionCriteria.addGreaterOrEqualThan(
                ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE,
                context.getStartDate());
        actionCriteria.addLessOrEqualThan(
                ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE, context.getEndDate());

        loadActionMap(actionCriteria);

        return incidentCriteria;
    }


    /**
     * Formats the time for use in the export.
     *
     * @param time PlainTime
     * @return String
     */
    private String formatTime(PlainTime time) {
        String value = "";

        if (time != null) {
            value = m_timeFormatter.format(time);
        }

        return value;
    }

    /**
     * Loads the first action per incident into a map keyed to the incident OID.
     *
     * @param criteria Criteria
     */
    private void loadActionMap(Criteria criteria) {
        m_actionMap = new HashMap<String, ConductAction>(4096);

        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);
        query.addOrderByAscending(ConductAction.COL_INCIDENT_OID);
        query.addOrderByAscending(ConductAction.COL_ACTION_START_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            String lastIncidentOid = null;
            while (iterator.hasNext()) {
                ConductAction action = (ConductAction) iterator.next();
                String incidentOid = action.getIncidentOid();

                if (!ObjectUtils.match(incidentOid, lastIncidentOid)) {
                    m_actionMap.put(incidentOid, action);
                }

                lastIncidentOid = incidentOid;
            }
        } finally {
            iterator.close();
        }
    }
}
