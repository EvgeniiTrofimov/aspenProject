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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Group;
import com.follett.fsc.core.k12.beans.GroupMember;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.groups.GroupMemberType;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports school team information (Groups) for BC's GDE.
 *
 * @author Follett Software Company
 */
public class SchoolTeamExport extends GdeExportJavaSource {

    /**
     * The Enum EXPORT_FIELDS.
     */
    /*
     * Reflect field amount with field ID, field user friendly name and boolean parameter which
     * contains true if field is ID and false otherwise.
     * In constructor we will use Y/N chars to reflect if field ID or not
     */
    enum EXPORT_FIELDS {
        FIELD_DISTRICT_NAME("districtName", "District Name"), FIELD_SCHOOL_NUMBER("schoolNumber",
                "School Number"), FIELD_SCHOOL_NAME("schoolName", "School Name"), FIELD_STUDENT_NUMBER("studentNumber",
                        "Student Number"), FIELD_STUDENT_LEGAL_LAST_NAME("studentLegalLastName",
                                "Student Legal Last Name"), FIELD_STUDENT_LEGAL_FIRST_NAME("studentLegalFirstName",
                                        "Student Legal First Name"), FIELD_GRADE("grade", "Grade"), FIELD_HOMEROOM(
                                                "homeroom", "Homeroom"), FIELD_TEACHER_NAME("teacherName",
                                                        "Teacher Name"), FIELD_TEAM_DESCRIPTION("teacherDescription",
                                                                "Team Description"), FIELD_TEAM_MEMBERSHIP_START_DATE(
                                                                        "teamMembershipStartDate",
                                                                        "Team Membership Start Date");

        private String m_fieldId;
        private String m_fieldName;

        /**
         * Gets the field id.
         *
         * @return String
         */
        public String getFieldId() {
            return m_fieldId;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Instantiates a new export fields.
         *
         * @param fieldId String
         * @param fieldName String
         */
        private EXPORT_FIELDS(String fieldId, String fieldName) {
            m_fieldId = fieldId;
            m_fieldName = fieldName;
        }
    }

    // General constants
    private static final String GROUP_CATEGORY_LOCAL_CODE = "Team";

    private PlainDate m_endDate = null;
    private SimpleDateFormat m_formatter = new SimpleDateFormat("dd-MMM-yyyy");
    private Collection<String> m_schoolOids;
    private PlainDate m_startDate = null;
    private Collection<String> m_teamCategories;

    /**
     * @see com.x2dev.reports.bc.GdeExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(EXPORT_FIELDS.values().length) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        SubQuery groupSubquery = getGroupSubQuery();
        X2Criteria gropuMemberCriteria = getGroupMemberCriteria(groupSubquery);

        Map<String, SisStudent> students = getStudentsByGroupMember(gropuMemberCriteria);

        QueryByCriteria gropuMembersQuery = new QueryByCriteria(GroupMember.class, gropuMemberCriteria);
        QueryIterator groupMembers = getBroker().getIteratorByQuery(gropuMembersQuery);
        try {
            while (groupMembers.hasNext()) {
                GroupMember groupMember = (GroupMember) groupMembers.next();
                SisStudent student = students.get(groupMember.getObjectOid());

                if (student != null) {
                    fillInDataGridRow(groupMember, student, grid);
                }
            }
        } finally {
            groupMembers.close();
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
        List<String> columnNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            columnNames.add(field.getFieldId());
        }

        return columnNames;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columnUserNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            columnUserNames.add(field.getFieldName());
        }

        return columnUserNames;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        m_startDate = getCurrentContext().getStartDate();
        m_endDate = getCurrentContext().getEndDate();

        loadTeamCategories();

        SubQuery schools = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, getSchoolCriteria());
        m_schoolOids = getBroker().getSubQueryCollectionByQuery(schools);
    }


    /**
     * Fill 1 row in grid with appropriate data.
     *
     * @param groupMember GroupMember
     * @param student SisStudent
     * @param grid DataGrid
     */
    private void fillInDataGridRow(GroupMember groupMember, SisStudent student, DataGrid grid) {
        boolean deleteRow = false;
        try {
            Group group = groupMember.getGroup();
            School school = group.getSchool();

            if (school == null) {
                school = student.getSchool();
            }

            if (m_schoolOids.contains(school.getOid())) {
                SisPerson person = student.getPerson();

                grid.append();
                deleteRow = true;

                String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                grid.set(EXPORT_FIELDS.FIELD_DISTRICT_NAME.getFieldId(), school.getParentOrganization().getName());
                grid.set(EXPORT_FIELDS.FIELD_SCHOOL_NUMBER.getFieldId(), school.getSchoolId());
                grid.set(EXPORT_FIELDS.FIELD_SCHOOL_NAME.getFieldId(), school.getName());
                grid.set(EXPORT_FIELDS.FIELD_STUDENT_NUMBER.getFieldId(), student.getLocalId());
                grid.set(EXPORT_FIELDS.FIELD_STUDENT_LEGAL_LAST_NAME.getFieldId(), person.getLastName());
                grid.set(EXPORT_FIELDS.FIELD_STUDENT_LEGAL_FIRST_NAME.getFieldId(), person.getFirstName());
                grid.set(EXPORT_FIELDS.FIELD_GRADE.getFieldId(),
                        StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                grid.set(EXPORT_FIELDS.FIELD_HOMEROOM.getFieldId(), student.getHomeroom());
                grid.set(EXPORT_FIELDS.FIELD_TEAM_DESCRIPTION.getFieldId(), group.getName());
                grid.set(EXPORT_FIELDS.FIELD_TEAM_MEMBERSHIP_START_DATE.getFieldId(), formatDate(group.getStartDate()));

                Staff staff = group.getAdultResponsible();
                if (staff != null) {
                    grid.set(EXPORT_FIELDS.FIELD_TEACHER_NAME.getFieldId(), staff.getNameView());
                }
            }
        } catch (NullPointerException npe) {
            StringBuilder strBldr = new StringBuilder();
            strBldr.append("Unable to export ");
            strBldr.append(GroupMember.class.getName());
            strBldr.append(" with OID: [");
            strBldr.append(groupMember.getOid());
            strBldr.append("].");
            if (student != null) {
                strBldr.append("] for the Student with Local ID: [");
                strBldr.append(student.getLocalId());
                strBldr.append("].");
            } else {
                strBldr.append("] as it has no related Student.");
            }

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
     * Formats the date as a string. If the date is null, empty string is returned.
     *
     * @param date PlainDate
     * @return String
     */
    private String formatDate(PlainDate date) {
        String value = "";

        if (date != null) {
            value = m_formatter.format(date);
        }

        return value;
    }

    /**
     * Return criteria for pulling the group members included in the export.
     *
     * @param groupQuery SubQuery
     * @return X2Criteria
     */
    private X2Criteria getGroupMemberCriteria(SubQuery groupQuery) {
        X2Criteria groupMemberCriteria = new X2Criteria();
        groupMemberCriteria.addIn(GroupMember.COL_GROUP_OID, groupQuery);
        groupMemberCriteria.addEqualTo(GroupMember.COL_MEMBER_TYPE,
                Integer.toString(GroupMemberType.SINGLE_PERSON.getTypeIndex()));

        return groupMemberCriteria;
    }

    /**
     * Compose sub-query for groups based on categories flagged as "Team" and the appropriate date
     * range.
     * 
     * @return SubQuery
     */
    private SubQuery getGroupSubQuery() {
        X2Criteria groupCriteria = new X2Criteria();

        if (!CollectionUtils.isEmpty(m_teamCategories)) {
            groupCriteria.addIn(Group.COL_CATEGORY, m_teamCategories);
        } else {
            addNoMatchCriteria(groupCriteria);
        }

        /*
         * Start date criteria (handle if date is null)
         */
        X2Criteria groupEmptyStartDateCriteria = new X2Criteria();
        groupEmptyStartDateCriteria.addIsNull(Group.COL_START_DATE);

        X2Criteria groupStartDateCriteria = new X2Criteria();
        groupStartDateCriteria.addLessOrEqualThan(Group.COL_START_DATE, m_endDate);
        groupStartDateCriteria.addOrCriteria(groupEmptyStartDateCriteria);

        /*
         * End date criteria (handle if date is null)
         */
        X2Criteria groupEmptyEndDateCriteria = new X2Criteria();
        groupEmptyEndDateCriteria.addIsNull(Group.COL_END_DATE);

        X2Criteria groupEndDateCriteria = new X2Criteria();
        groupEndDateCriteria.addGreaterOrEqualThan(Group.COL_END_DATE, m_startDate);
        groupEndDateCriteria.addOrCriteria(groupEmptyEndDateCriteria);

        groupCriteria.addAndCriteria(groupStartDateCriteria);
        groupCriteria.addAndCriteria(groupEndDateCriteria);

        return new SubQuery(Group.class, X2BaseBean.COL_OID, groupCriteria);
    }

    /**
     * Query students involved as group members (by GroupMember criteria) and load them into a
     * lookup map by
     * person OID.
     *
     * @param groupMemberCriteria X2Criteria
     * @return Map<String, SisStudent>
     */
    private Map<String, SisStudent> getStudentsByGroupMember(X2Criteria groupMemberCriteria) {
        SubQuery groupMemberSubquery = new SubQuery(GroupMember.class, GroupMember.COL_OBJECT_OID, groupMemberCriteria);

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(SisStudent.COL_PERSON_OID, groupMemberSubquery);

        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        QueryByCriteria personQuery = new QueryByCriteria(SisStudent.class, criteria);

        return getBroker().getMapByQuery(personQuery, SisStudent.COL_PERSON_OID, 500);
    }

    /**
     * Load category codes with a local code of "Team" into a collection.
     */
    private void loadTeamCategories() {
        m_teamCategories = new LinkedList<String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(Group.class, Group.COL_CATEGORY, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());

        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTable.getOid());
                criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, GROUP_CATEGORY_LOCAL_CODE);

                m_teamCategories = getBroker().getSubQueryCollectionByQuery(
                        new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria));
            }
        }
    }
}
