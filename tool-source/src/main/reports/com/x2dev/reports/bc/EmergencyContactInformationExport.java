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
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports emergency contact information for BC's GDE. All contacts of active students are exported.
 *
 * @author Follett Software Company
 */
public class EmergencyContactInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STD_GRADE_LEVEL = "Grade";
    private static final String FIELD_STD_HOMEROOM = "Homeroom";
    private static final String FIELD_STD_HOMEROOM_TEACHER = "Teacher Name";
    private static final String FIELD_CNT_TYPE = "Contact Type";
    private static final String FIELD_CNT_LEVEL_CODE = "Contact Level Code";
    private static final String FIELD_CNT_RELATIONSHIP_CODE = "Relationship Code";
    private static final String FIELD_CNT_RELATIONSHIP_DESC = "Relationship Description";
    private static final String FIELD_CNT_LAST_NAME = "Last Name";
    private static final String FIELD_CNT_FIRST_NAME = "First Name";
    private static final String FIELD_CNT_ADDRESS = "Address";
    private static final String FIELD_CNT_HOME_PHONE = "Home Phone";
    private static final String FIELD_CNT_CELL_PHONE = "Cellular Phone";
    private static final String FIELD_CNT_WORK_PHONE = "Business Phone";
    private static final String FIELD_CNT_WORK_PHONE_EXT = "Business Phone Extension";

    // Student Contact aliases
    private static final String ALIAS_WORK_PHONE_EXT = "psn-business-phone-ext";

    // Other constants
    private static final int FIELD_COUNT = 17;
    private static final String ILLEGAL_PHONE_CHARACTERS = "[_\\s\\-\\(\\)x]";

    private List<String> m_columns;
    private Pattern m_illegalPhoneCharacters;
    private Map<String, ReferenceCode> m_relationshipCodeMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
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

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, buildCriteria());
        query.addOrderByAscending(StudentContact.REL_STUDENT + PATH_DELIMITER + Student.REL_SCHOOL + PATH_DELIMITER
                + School.COL_SCHOOL_ID);
        query.addOrderByAscending(StudentContact.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
        query.addOrderByAscending(StudentContact.COL_STUDENT_OID);
        query.addOrderByAscending(StudentContact.REL_CONTACT + PATH_DELIMITER + Contact.REL_PERSON + PATH_DELIMITER
                + Person.COL_LAST_NAME);

        QueryIterator contacts = getBroker().getIteratorByQuery(query);
        try {
            int progressCount = 0;

            while (contacts.hasNext()) {
                StudentContact contact = (StudentContact) contacts.next();
                boolean deleteRow = false;

                try {
                    Person person = contact.getPerson();
                    SisStudent student = (SisStudent) contact.getStudent();

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                    if (++progressCount % 10000 == 0) {
                        AppGlobals.getLog().info(
                                "GDE Emergency Contact Information processed " + progressCount + " student contacts");
                    }

                    grid.append();
                    deleteRow = true;

                    // Fill grid data list with export information
                    grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                    grid.set(FIELD_STD_LAST_NAME, student.getPerson().getLastName());
                    grid.set(FIELD_STD_FIRST_NAME, student.getPerson().getFirstName());
                    grid.set(FIELD_STD_GRADE_LEVEL, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_STD_HOMEROOM, student.getHomeroom(getCurrentContext().getOid(), getBroker()));

                    grid.set(FIELD_CNT_TYPE, contact.getContact().getContactTypeCode());
                    grid.set(FIELD_CNT_LEVEL_CODE, String.valueOf(contact.getEmergencyPriority()));
                    grid.set(FIELD_CNT_RELATIONSHIP_CODE, contact.getRelationshipCode());
                    grid.set(FIELD_CNT_LAST_NAME, person.getLastName());
                    grid.set(FIELD_CNT_FIRST_NAME, person.getFirstName());
                    grid.set(FIELD_CNT_ADDRESS, formGridAddressLine(person.getPhysicalAddress()));
                    grid.set(FIELD_CNT_HOME_PHONE, stripNonNumericPhoneCharacters(person.getPhone01()));
                    grid.set(FIELD_CNT_CELL_PHONE, stripNonNumericPhoneCharacters(person.getPhone03()));
                    grid.set(FIELD_CNT_WORK_PHONE, stripNonNumericPhoneCharacters(person.getPhone02()));
                    grid.set(FIELD_CNT_WORK_PHONE_EXT, person.getFieldValueByAlias(ALIAS_WORK_PHONE_EXT));

                    // Add teacher name if it exists in homeroom to staff map
                    Map<String, Staff> staffMap =
                            getHomeroomToStaffMap(student.getSchool(getCurrentContext().getOid(), getBroker()));
                    if (staffMap != null) {
                        Staff staff = staffMap.get(student.getHomeroom(getCurrentContext().getOid()));
                        if (staff != null) {
                            grid.set(FIELD_STD_HOMEROOM_TEACHER, staff.getNameView());
                        }
                    }

                    // Relationship description
                    ReferenceCode relationshipCode = m_relationshipCodeMap.get(contact.getRelationshipCode());
                    if (relationshipCode != null) {
                        grid.set(FIELD_CNT_RELATIONSHIP_DESC, relationshipCode.getDescription());
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(contact.getOid());
                    Student student = contact.getStudent();
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
            contacts.close();
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

        // Formatting options
        m_illegalPhoneCharacters = Pattern.compile(ILLEGAL_PHONE_CHARACTERS);

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_LAST_NAME);
        m_columns.add(FIELD_STD_FIRST_NAME);
        m_columns.add(FIELD_STD_GRADE_LEVEL);
        m_columns.add(FIELD_STD_HOMEROOM);
        m_columns.add(FIELD_STD_HOMEROOM_TEACHER);
        m_columns.add(FIELD_CNT_TYPE);
        m_columns.add(FIELD_CNT_LEVEL_CODE);
        m_columns.add(FIELD_CNT_RELATIONSHIP_CODE);
        m_columns.add(FIELD_CNT_RELATIONSHIP_DESC);
        m_columns.add(FIELD_CNT_LAST_NAME);
        m_columns.add(FIELD_CNT_FIRST_NAME);
        m_columns.add(FIELD_CNT_ADDRESS);
        m_columns.add(FIELD_CNT_HOME_PHONE);
        m_columns.add(FIELD_CNT_CELL_PHONE);
        m_columns.add(FIELD_CNT_WORK_PHONE);
        m_columns.add(FIELD_CNT_WORK_PHONE_EXT);

        // Fill relationship reference code map if reference table exists
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE,
                getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_relationshipCodeMap = referenceTable.getCodeMap();
            }
        }
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL,
                StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID));

        StudentContextReportHelper helper =
                new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        // Select contacts only for active students
        criteria.addAndCriteria(helper.getActiveStudentCriteria(StudentContact.REL_STUDENT + PATH_DELIMITER));

        if (getSchool() != null) {
            Criteria secondaryCriteria = StudentManager.getSecondaryStudentCriteria(
                    StudentContact.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS +
                            PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey());

            criteria.addOrCriteria(secondaryCriteria);
        }

        return criteria;
    }

    /**
     * Forms grid address String.
     *
     * @param address Address
     * @return String
     */
    private String formGridAddressLine(Address address) {
        String addressDisplay = "";

        if (address != null) {
            if (!StringUtils.isEmpty(address.getAddressLine01())) {
                addressDisplay += address.getAddressLine01() + " ";
            }

            if (!StringUtils.isEmpty(address.getAddressLine02())) {
                addressDisplay += address.getAddressLine02() + " ";
            }

            if (!StringUtils.isEmpty(address.getAddressLine03())) {
                addressDisplay += address.getAddressLine03();
            }
        }

        return addressDisplay.trim();
    }

    /**
     * Returns the passed phone value stripped of any non-numeric characters.
     *
     * @param phoneValue String
     * @return String
     */
    private String stripNonNumericPhoneCharacters(String phoneValue) {
        String cleanValue = "";

        if (phoneValue != null) {
            Matcher matcher = m_illegalPhoneCharacters.matcher(phoneValue);
            cleanValue = matcher.replaceAll("");
        }

        return cleanValue;
    }
}
