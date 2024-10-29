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
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports staff information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class StaffInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_USER_NAME = "User Name";
    private static final String FIELD_STAFF_INDICATOR = "Teaching Staff";
    private static final String FIELD_STAFF_ID = "Teacher Id";
    private static final String FIELD_STAFF_NAMEVIEW = "Name";
    private static final String FIELD_STAFF_PREFIX = "Prefix";
    private static final String FIELD_STAFF_LASTNAME = "Last Name";
    private static final String FIELD_STAFF_FIRSTNAME = "First Name";
    private static final String FIELD_STAFF_EMAIL = "Email Address";

    // Other constants
    private static final int FIELD_COUNT = 9;
    private static final String TEACHER_TYPE = "Teacher";

    private List<String> m_columns;
    private Map<String, User> m_userMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(FIELD_COUNT) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(SisStaff.class, buildCriteria());
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStaff staff = (SisStaff) iterator.next();
                boolean deleteRow = false;
                try {
                    Person person = staff.getPerson();

                    grid.append();
                    deleteRow = true;

                    grid.set(FIELD_SCHOOL_NUMBER, staff.getSchool().getSchoolId());
                    grid.set(FIELD_STAFF_ID, staff.getLocalId());
                    grid.set(FIELD_STAFF_NAMEVIEW, staff.getNameView());
                    grid.set(FIELD_STAFF_PREFIX, person.getNameTitleCode());
                    grid.set(FIELD_STAFF_LASTNAME, person.getLastName());
                    grid.set(FIELD_STAFF_FIRSTNAME, person.getFirstName());
                    grid.set(FIELD_STAFF_EMAIL, person.getEmail01());

                    /*
                     * Set teacher indicator
                     */
                    String teacherIndicator = TEACHER_TYPE.equals(staff.getStaffType()) ? "Y" : "N";
                    grid.set(FIELD_STAFF_INDICATOR, teacherIndicator);

                    /*
                     * Set user name
                     */
                    User user = m_userMap.get(person.getOid());
                    if (user != null) {
                        grid.set(FIELD_USER_NAME, user.getLoginName());
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(staff.getOid());
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
            iterator.close();
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
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_USER_NAME);
        m_columns.add(FIELD_STAFF_INDICATOR);
        m_columns.add(FIELD_STAFF_ID);
        m_columns.add(FIELD_STAFF_NAMEVIEW);
        m_columns.add(FIELD_STAFF_PREFIX);
        m_columns.add(FIELD_STAFF_LASTNAME);
        m_columns.add(FIELD_STAFF_FIRSTNAME);
        m_columns.add(FIELD_STAFF_EMAIL);

        loadUsers();
    }

    /**
     * Builds the criteria used to return the staff to include in the export.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(SisStaff.REL_SCHOOL, SisStaff.COL_SCHOOL_OID));

        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        criteria.addEqualTo(SisStaff.COL_STATUS, activeCode);

        return criteria;
    }

    /**
     * Loads the users that are flagged as Staff records for quick lookup.
     */
    private void loadUsers() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(User.REL_PERSON + ModelProperty.PATH_DELIMITER + Person.COL_STAFF_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(User.class, criteria);

        m_userMap = getBroker().getMapByQuery(query, User.COL_PERSON_OID, 2048);
    }
}
