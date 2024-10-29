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
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.StaffSchoolAssociation;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.User.LoginStatus;
import com.follett.fsc.core.k12.beans.UserSchoolAssociation;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports user security information (staff/school combinations) for BC's GDE. Staff with user
 * accounts
 * are exported and the staff primary school, secondary schools, and user schools are included.
 *
 * @author Follett Software Company
 */
public class UserSecurityInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_USER_ACCOUNT = "User Account";
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_TEACHER_ID = "Teacher ID";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";

    // Other constants
    private static final int FIELD_COUNT = 4;

    private String m_activeCode;
    private List<String> m_columns;
    private Collection<String> m_schoolOids;
    private Map<String, SisStaff> m_staffByPsnOid;

    private Map<String, Collection<StaffSchoolAssociation>> m_staffSchools;
    private Map<String, Collection<UserSchoolAssociation>> m_userSchools;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid() {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        QueryByCriteria query = new QueryByCriteria(SisUser.class, buildCriteria());
        query.addOrderByAscending(SisUser.COL_NAME_VIEW);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SisUser user = (SisUser) iterator.next();

                // Collection of SKL_OIDs to track if school has been entered for user
                Collection<String> schoolOids = new LinkedList<>();

                addStaffToGrid(grid, schoolOids, user, m_staffByPsnOid.get(user.getPersonOid()));
                addStaffSchoolToGrid(grid, schoolOids, user, m_staffSchools.get(user.getPersonOid()));
                addUserSchoolToGrid(grid, schoolOids, user, m_userSchools.get(user.getPersonOid()));
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
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_USER_ACCOUNT);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_TEACHER_ID);
        m_columns.add(FIELD_TEACHER_NAME);

        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

        loadStaff();
        loadStaffSchools();
        loadUserSchools();
    }

    /**
     * Appends the staff and their primary school to the grid.
     *
     * @param grid DataGrid
     * @param schoolOids Collection<String>
     * @param user SisUser
     * @param staff SisStaff
     */
    private void addStaffToGrid(DataGrid grid, Collection<String> schoolOids, SisUser user, SisStaff staff) {
        boolean deleteRow = false;
        try {
            if (staff != null && !schoolOids.contains(staff.getSchoolOid())) {
                School school = staff.getSchool();

                grid.append();
                deleteRow = true;
                grid.set(FIELD_SCHOOL_NUMBER, school.getSchoolId());
                grid.set(FIELD_TEACHER_ID, staff.getLocalId());
                grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                grid.set(FIELD_USER_ACCOUNT, user.getLoginName());

                schoolOids.add(school.getOid());
            }
        } catch (NullPointerException npe) {
            StringBuilder strBldr = new StringBuilder();
            strBldr.append("Unable to export ");
            strBldr.append(user.getClass().getName());
            strBldr.append(" with OID: [");
            strBldr.append(user.getOid());
            strBldr.append("].");

            // deleteRow is true if an incomplete row has been added to the grid from grid.append()
            if (!deleteRow) {
                strBldr.append("Null encountered before adding to export.");
            } else {
                strBldr.append("Null encountered when setting Columns.");
                grid.deleteRow(); // Delete the incomplete row that was appended to the grid.
            }

            strBldr.append("\n\n\nNullPointerException: \n");
            strBldr.append(ExceptionUtils.getStackTrace(npe));
            logToolMessage(Level.WARNING, strBldr.toString(), false);
        }
    }

    /**
     * Appends the staff and their primary school to the grid.
     *
     * @param grid DataGrid
     * @param schoolOids Collection<String>
     * @param user SisUser
     * @param staffSchools Collection<StaffSchoolAssociation>
     */
    private void addStaffSchoolToGrid(DataGrid grid,
                                      Collection<String> schoolOids,
                                      SisUser user,
                                      Collection<StaffSchoolAssociation> staffSchools) {
        boolean deleteRow = false;
        try {
            if (!CollectionUtils.isEmpty(staffSchools)) {
                for (StaffSchoolAssociation staffSchool : staffSchools) {
                    if (!schoolOids.contains(staffSchool.getSchoolOid())) {
                        Staff staff = staffSchool.getStaff();
                        School school = staffSchool.getSchool();

                        grid.append();
                        deleteRow = true;
                        grid.set(FIELD_SCHOOL_NUMBER, school.getSchoolId());
                        grid.set(FIELD_TEACHER_ID, staff.getLocalId());
                        grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                        grid.set(FIELD_USER_ACCOUNT, user.getLoginName());

                        schoolOids.add(school.getOid());
                    }
                }
            }
        } catch (NullPointerException npe) {
            StringBuilder strBldr = new StringBuilder();
            strBldr.append("Unable to export ");
            strBldr.append(user.getClass().getName());
            strBldr.append(" with OID: [");
            strBldr.append(user.getOid());
            strBldr.append("].");

            // deleteRow is true if an incomplete row has been added to the grid from grid.append()
            if (!deleteRow) {
                strBldr.append("Null encountered before adding to export.");
            } else {
                strBldr.append("Null encountered when setting Columns.");
                grid.deleteRow(); // Delete the incomplete row that was appended to the grid.
            }

            strBldr.append("\n\n\nNullPointerException: \n");
            strBldr.append(ExceptionUtils.getStackTrace(npe));
            logToolMessage(Level.WARNING, strBldr.toString(), false);
        }
    }

    /**
     * Appends the staff and their primary school to the grid.
     *
     * @param grid DataGrid
     * @param schoolOids Collection<String>
     * @param user SisUser
     * @param userSchools Collection<UserSchoolAssociation>
     */
    private void addUserSchoolToGrid(DataGrid grid,
                                     Collection<String> schoolOids,
                                     SisUser user,
                                     Collection<UserSchoolAssociation> userSchools) {
        boolean deleteRow = false;
        try {
            if (!CollectionUtils.isEmpty(userSchools)) {
                for (UserSchoolAssociation userSchool : userSchools) {
                    if (!schoolOids.contains(userSchool.getSchoolOid())) {
                        Staff staff = m_staffByPsnOid.get(userSchool.getUser().getPersonOid());
                        School school = userSchool.getSchool();

                        if (staff != null && school != null) {
                            grid.append();
                            deleteRow = true;
                            grid.set(FIELD_SCHOOL_NUMBER, school.getName());
                            grid.set(FIELD_TEACHER_ID, staff.getLocalId());
                            grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                            grid.set(FIELD_USER_ACCOUNT, user.getLoginName());

                            schoolOids.add(school.getOid());
                        }
                    }
                }
            }
        } catch (NullPointerException npe) {
            StringBuilder strBldr = new StringBuilder();
            strBldr.append("Unable to export ");
            strBldr.append(user.getClass().getName());
            strBldr.append(" with OID: [");
            strBldr.append(user.getOid());
            strBldr.append("].");

            // deleteRow is true if an incomplete row has been added to the grid from grid.append()
            if (!deleteRow) {
                strBldr.append("Null encountered before adding to export.");
            } else {
                strBldr.append("Null encountered when setting Columns.");
                grid.deleteRow(); // Delete the incomplete row that was appended to the grid.
            }

            strBldr.append("\n\n\nNullPointerException: \n");
            strBldr.append(ExceptionUtils.getStackTrace(npe));
            logToolMessage(Level.WARNING, strBldr.toString(), false);
        }
    }

    /**
     * Builds the criteria for returning user accounts to include in the export. Note that the
     * school is not
     * checked here but will instead be checked when the staff information is processed.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisUser.REL_PERSON + PATH_DELIMITER + Person.COL_STAFF_INDICATOR, Boolean.TRUE);
        criteria.addEqualTo(SisUser.COL_LOGIN_STATUS, Integer.valueOf(LoginStatus.ENABLED.ordinal()));

        return criteria;
    }



    /**
     * Loads staff into a Map keyed to the Person OID.
     */
    private void loadStaff() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(SisStaff.REL_SCHOOL, SisStaff.COL_SCHOOL_OID));
        criteria.addEqualTo(SisStaff.REL_PERSON + PATH_DELIMITER + SisPerson.COL_USER_INDICATOR, Boolean.TRUE);
        criteria.addEqualTo(SisStaff.COL_STATUS, m_activeCode);

        QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);

        m_staffByPsnOid = getBroker().getMapByQuery(query, SisStaff.COL_PERSON_OID, 2048);
    }

    /**
     * Loads the staff school associations into a map keyed to the Person OID.
     */
    private void loadStaffSchools() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StaffSchoolAssociation.COL_SCHOOL_OID, m_schoolOids);
        criteria.addEqualTo(StaffSchoolAssociation.REL_STAFF + PATH_DELIMITER +
                SisStaff.REL_PERSON + PATH_DELIMITER + SisPerson.COL_USER_INDICATOR, Boolean.TRUE);
        criteria.addEqualTo(StaffSchoolAssociation.REL_STAFF + PATH_DELIMITER +
                SisStaff.COL_STATUS, m_activeCode);
        criteria.addEqualTo(StaffSchoolAssociation.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
                SisDistrictSchoolYearContext.COL_CONTEXT_ID, getCurrentContext().getOid());

        QueryByCriteria query = new QueryByCriteria(StaffSchoolAssociation.class, criteria);

        m_staffSchools = getBroker().getGroupedCollectionByQuery(query,
                StaffSchoolAssociation.REL_STAFF + PATH_DELIMITER + Staff.COL_PERSON_OID,
                2048);
    }

    /**
     * Loads the user school associations into a map keyed to the Person OID.
     */
    private void loadUserSchools() {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(UserSchoolAssociation.COL_SCHOOL_OID, m_schoolOids);
        criteria.addEqualTo(UserSchoolAssociation.REL_USER + PATH_DELIMITER +
                SisUser.REL_PERSON + PATH_DELIMITER + SisPerson.COL_STAFF_INDICATOR, Boolean.TRUE);
        criteria.addEqualTo(UserSchoolAssociation.REL_USER + PATH_DELIMITER +
                SisUser.COL_LOGIN_STATUS, Integer.valueOf(LoginStatus.ENABLED.ordinal()));

        QueryByCriteria query = new QueryByCriteria(UserSchoolAssociation.class, criteria);

        m_userSchools = getBroker().getGroupedCollectionByQuery(query,
                UserSchoolAssociation.REL_USER + PATH_DELIMITER + SisUser.COL_PERSON_OID,
                2048);
    }

}
