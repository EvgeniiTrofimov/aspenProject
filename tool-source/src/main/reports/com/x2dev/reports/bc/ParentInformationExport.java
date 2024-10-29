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
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports parent information for BC's GDE. Parents are based on relationship codes defined as a
 * hidden parameter
 * on the input screen.
 *
 * @author Follett Software Company
 */
public class ParentInformationExport extends GdeExportJavaSource {
    // Input parameters
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";

    // Grid fields
    private static final String FIELD_SKL_ID = "School Number";
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STD_GRADE = "Grade";
    private static final String FIELD_STD_HOMEROOM = "Homeroom";
    private static final String FIELD_USR_NAME = "User Name";
    private static final String FIELD_PNT_SURNAME = "Surname";
    private static final String FIELD_PNT_FIRST_NAME = "First Name";
    private static final String FIELD_PNT_PREFIX = "Prefix";
    private static final String FIELD_PNT_TYPE = "Parent Type";
    private static final String FIELD_PNT_TYPE_DESC = "Parent Type Description";
    private static final String FIELD_PNT_WORK_PHONE = "Business Phone";
    private static final String FIELD_PNT_WORK_PHONE_EXT = "Business Phone Extension";
    private static final String FIELD_PNT_HOME_PHONE = "Home Phone";
    private static final String FIELD_PNT_CELL_PHONE = "Cellular Phone";
    private static final String FIELD_PNT_EMAIL = "Email Address";
    private static final String FIELD_PNT_EMPLOYMENT_PLACE = "Place of Employment";
    private static final String FIELD_PNT_APARTMENT = "Apartment";
    private static final String FIELD_PNT_STREET_NUMBER = "Street Number";
    private static final String FIELD_PNT_STREET_NAME = "Street Name";
    private static final String FIELD_PTN_COMMUNITY = "Community";
    private static final String FIELD_PNT_POSTAL_CODE = "Postal Code";
    private static final String FIELD_PNT_RURAL_CODE = "Rural Code";
    private static final String FIELD_PNT_POSTAL_BOX = "Postal Box";
    private static final String FIELD_PNT_LOT_NUMBER = "Lot Number";
    private static final String FIELD_PNT_CONCESSION_NUMBER = "Concession Number";
    private static final String FIELD_PNT_MAILING_ADDRESS = "Mailing Address";
    private static final String FIELD_PNT_CUSTODY_STATUS = "Custody Status";
    private static final String FIELD_PNT_HOME_LANGUAGE = "Home Language";
    private static final String FIELD_PNT_LIVES_WITH_STD = "Living With Student";

    // Other constants
    private static final int FIELD_COUNT = 31;
    private static final String ILLEGAL_PHONE_CHARACTERS = "[_\\s\\-\\(\\)x]";

    // Aliases
    private static final String ALIAS_ADR_CONCESSION_NUMBER = "adr-concession-number";
    private static final String ALIAS_ADR_LOT_NUMBER = "adr-lot-number";
    private static final String ALIAS_ADR_POSTAL_BOX = "adr-postal-box";
    private static final String ALIAS_ADR_RURAL_ROUTE = "adr-rural-route";
    private static final String ALIAS_PNT_BUSINESS_PHONE_EXT = "psn-business-phone-ext";
    private static final String ALIAS_PNT_EMPLOYMENT_PLACE = "psn-place-of-employment";
    private static final String ALIAS_PNT_HAS_CUSTODY = "ctj-has-custody";
    private static final String ALIAS_PNT_HOME_LANGUAGE = "psn-home-language";

    private List<String> m_columns;
    private Map<String, StringBuilder> m_custodyDisplayMap;
    private StudentContextReportHelper m_helper;
    private Pattern m_illegalPhoneCharacters;
    private Map<String, ReferenceCode> m_relationCodesMap;
    private Collection<String> m_schoolOids;
    private Map<String, SisUser> m_userMap;
    private Selection m_schoolOidsSelection;

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

        // Append rows for both primary students and secondary
        appendToGrid(grid, true);
        appendToGrid(grid, false);

        // Build the sort columns to preserve requirements of by school, localid, oid, and priority
        List<String> sortColumns = new ArrayList<String>();
        sortColumns.add(StudentContact.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship() + PATH_DELIMITER
                + SisSchool.COL_SCHOOL_ID);
        sortColumns.add(StudentContact.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
        sortColumns.add(StudentContact.COL_STUDENT_OID);
        sortColumns.add(StudentContact.COL_EMERGENCY_PRIORITY);
        grid.sort(sortColumns, true);

        /*
         * Cleanup temporary data
         */
        if (!getTemporaryBeans().isEmpty()) {
            for (X2BaseBean bean : getTemporaryBeans()) {
                getBroker().deleteBean(bean);
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Append data to the grid for primary or secondary students.
     *
     * @param grid DataGrid
     * @param isPrimary boolean
     */
    private void appendToGrid(DataGrid grid, boolean isPrimary) {
        QueryByCriteria query = new QueryByCriteria(StudentContact.class, buildCriteria(isPrimary));
        query.addOrderByAscending(StudentContact.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship()
                + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(StudentContact.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
        query.addOrderByAscending(StudentContact.COL_STUDENT_OID);
        query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        QueryIterator contacts = getBroker().getIteratorByQuery(query);
        try {
            while (contacts.hasNext()) {
                StudentContact contact = (StudentContact) contacts.next();
                boolean deleteRow = false;
                try {
                    Person person = contact.getPerson();

                    if (person != null) {
                        SisStudent student = (SisStudent) contact.getStudent();

                        String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                        String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                        String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());
                        SisSchool school = student.getSchool(getCurrentContext().getOid(), getBroker());

                        grid.append();
                        deleteRow = true;

                        grid.set(FIELD_SKL_ID, school.getSchoolId());
                        grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                        grid.set(FIELD_STD_LAST_NAME,
                                student.getPerson() != null ? student.getPerson().getLastName() : "");
                        grid.set(FIELD_STD_FIRST_NAME,
                                student.getPerson() != null ? student.getPerson().getFirstName() : "");
                        grid.set(FIELD_STD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                        grid.set(FIELD_STD_HOMEROOM, homeroom);

                        grid.set(FIELD_PNT_SURNAME, person.getLastName());
                        grid.set(FIELD_PNT_FIRST_NAME, person.getFirstName());
                        grid.set(FIELD_PNT_PREFIX, person.getNameTitleCode());
                        grid.set(FIELD_PNT_TYPE, contact.getRelationshipCode());
                        grid.set(FIELD_PNT_MAILING_ADDRESS,
                                buildMailingAddressLine(person.getResolvedMailingAddress()));
                        grid.set(FIELD_PNT_HOME_LANGUAGE, person.getFieldValueByAlias(ALIAS_PNT_HOME_LANGUAGE));
                        grid.set(FIELD_PNT_LIVES_WITH_STD, String.valueOf(contact.getLivesWithIndicator()));
                        grid.set(FIELD_PNT_WORK_PHONE, stripNonNumericPhoneCharacters(person.getPhone02()));
                        grid.set(FIELD_PNT_WORK_PHONE_EXT, person.getFieldValueByAlias(ALIAS_PNT_BUSINESS_PHONE_EXT));
                        grid.set(FIELD_PNT_HOME_PHONE, stripNonNumericPhoneCharacters(person.getPhone01()));
                        grid.set(FIELD_PNT_CELL_PHONE, stripNonNumericPhoneCharacters(person.getPhone03()));
                        grid.set(FIELD_PNT_EMAIL, person.getEmail01());
                        grid.set(FIELD_PNT_EMPLOYMENT_PLACE, person.getFieldValueByAlias(ALIAS_PNT_EMPLOYMENT_PLACE));

                        /*
                         * Physical address information
                         */
                        Address address = person.getPhysicalAddress();
                        if (address != null) {
                            grid.set(FIELD_PNT_APARTMENT, address.getAddressLine02());
                            grid.set(FIELD_PNT_STREET_NUMBER, String.valueOf(address.getStreetNumber()));
                            grid.set(FIELD_PNT_STREET_NAME, buildPhysicalAddressLine(address));
                            grid.set(FIELD_PTN_COMMUNITY, address.getCity());
                            grid.set(FIELD_PNT_POSTAL_CODE, address.getPostalCode());
                            grid.set(FIELD_PNT_RURAL_CODE, address.getFieldValueByAlias(ALIAS_ADR_RURAL_ROUTE));
                            grid.set(FIELD_PNT_POSTAL_BOX, address.getFieldValueByAlias(ALIAS_ADR_POSTAL_BOX));
                            grid.set(FIELD_PNT_LOT_NUMBER, address.getFieldValueByAlias(ALIAS_ADR_LOT_NUMBER));
                            grid.set(FIELD_PNT_CONCESSION_NUMBER,
                                    address.getFieldValueByAlias(ALIAS_ADR_CONCESSION_NUMBER));
                        }

                        /*
                         * Get user information
                         */
                        SisUser user = m_userMap.get(person.getOid());
                        if (user != null) {
                            grid.set(FIELD_USR_NAME, user.getLoginName());
                        }

                        /*
                         * Pull the description of the relationship code
                         */
                        ReferenceCode code = m_relationCodesMap.get(contact.getRelationshipCode());
                        if (code != null) {
                            grid.set(FIELD_PNT_TYPE_DESC, code.getDescription());
                        }

                        /*
                         * Custody status
                         */
                        StringBuilder custodyDisplay = m_custodyDisplayMap.get(student.getOid());
                        if (custodyDisplay != null) {
                            grid.set(FIELD_PNT_CUSTODY_STATUS, custodyDisplay.toString());
                        }
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

        // Report helper
        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_SKL_ID);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_LAST_NAME);
        m_columns.add(FIELD_STD_FIRST_NAME);
        m_columns.add(FIELD_STD_GRADE);
        m_columns.add(FIELD_STD_HOMEROOM);
        m_columns.add(FIELD_USR_NAME);
        m_columns.add(FIELD_PNT_SURNAME);
        m_columns.add(FIELD_PNT_FIRST_NAME);
        m_columns.add(FIELD_PNT_PREFIX);
        m_columns.add(FIELD_PNT_TYPE);
        m_columns.add(FIELD_PNT_TYPE_DESC);
        m_columns.add(FIELD_PNT_WORK_PHONE);
        m_columns.add(FIELD_PNT_WORK_PHONE_EXT);
        m_columns.add(FIELD_PNT_HOME_PHONE);
        m_columns.add(FIELD_PNT_CELL_PHONE);
        m_columns.add(FIELD_PNT_EMAIL);
        m_columns.add(FIELD_PNT_EMPLOYMENT_PLACE);
        m_columns.add(FIELD_PNT_APARTMENT);
        m_columns.add(FIELD_PNT_STREET_NUMBER);
        m_columns.add(FIELD_PNT_STREET_NAME);
        m_columns.add(FIELD_PTN_COMMUNITY);
        m_columns.add(FIELD_PNT_POSTAL_CODE);
        m_columns.add(FIELD_PNT_RURAL_CODE);
        m_columns.add(FIELD_PNT_POSTAL_BOX);
        m_columns.add(FIELD_PNT_LOT_NUMBER);
        m_columns.add(FIELD_PNT_CONCESSION_NUMBER);
        m_columns.add(FIELD_PNT_MAILING_ADDRESS);
        m_columns.add(FIELD_PNT_CUSTODY_STATUS);
        m_columns.add(FIELD_PNT_HOME_LANGUAGE);
        m_columns.add(FIELD_PNT_LIVES_WITH_STD);

        // Get export school oids
        m_schoolOids = getSchoolOids();

        // Fill relationship reference code map if reference table exists
        m_relationCodesMap = new HashMap<String, ReferenceCode>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE,
                getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                m_relationCodesMap = referenceTable.getCodeMap(getBroker());
            }
        }

        loadUsers();

        // Load for both primary and secondary students
        m_custodyDisplayMap = new HashMap<>(4096);
        loadStudentCustody(true);
        loadStudentCustody(false);
    }

    /**
     * Adds the criteria to filter students based on school and enrollments status.
     *
     * @param criteria Criteria
     * @param prefix String
     * @param activeOnly boolean
     * @return Criteria
     * @Param criteria
     */
    private void addSchoolCriteria(Criteria criteria, String prefix, boolean activeOnly) {
        X2Criteria schoolCriteria = new X2Criteria();
        X2Criteria schoolsCriteria = new X2Criteria();

        if (!m_schoolOids.isEmpty() && m_schoolOids.size() > 1) {
            criteria.addNotNull(prefix + m_helper.getSchoolOidField());

            Selection selObject = getSelectionObject();

            Criteria subCriteria = new Criteria();
            subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selObject.getOid());
            subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                    + X2BaseBean.COL_OID);

            schoolsCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));

            schoolCriteria.addAndCriteria(schoolsCriteria.copyWithAdjustedPath(
                    prefix + m_helper.getSchoolRelationship(),
                    prefix + m_helper.getSchoolOidField()));
        } else if (!m_schoolOids.isEmpty()) {
            criteria.addEqualTo(prefix + m_helper.getSchoolOidField(), m_schoolOids.iterator().next());
        }

        if (activeOnly) {
            schoolCriteria.addAndCriteria(m_helper.getActiveStudentCriteria(prefix));
        }

        if (!schoolCriteria.isEmpty()) {
            criteria.addAndCriteria(schoolCriteria);
        }
    }

    /**
     * Adds the criteria to filter students based on secondary school and enrollments status.
     *
     * @param criteria Criteria
     * @param prefix String
     * @param activeOnly boolean
     * @return Criteria
     * @Param criteria
     */
    private void addSecondarySchoolCriteria(Criteria criteria, String prefix, boolean activeOnly) {
        X2Criteria schoolsCriteria = new X2Criteria();

        if (!m_schoolOids.isEmpty() && m_schoolOids.size() > 1) {
            Selection selObject = getSelectionObject();

            Criteria subCriteria = new Criteria();

            subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selObject.getOid());
            subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                    + X2BaseBean.COL_OID);

            schoolsCriteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
        } else if (!m_schoolOids.isEmpty()) {
            criteria.addEqualTo(prefix + m_helper.getSchoolOidField(), m_schoolOids.iterator().next());
        }

        String secondaryRelationship = prefix + SisStudent.REL_STUDENT_SCHOOLS;
        String secondaryColumn = secondaryRelationship + ModelProperty.PATH_DELIMITER + StudentSchool.COL_SCHOOL_OID;
        String secondaryPrefix = prefix + SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER;
        Criteria secondaryCriteria = StudentManager.getSecondaryStudentCriteria(
                secondaryPrefix,
                getCurrentContext().getOid(),
                null,
                null,
                null,
                getBroker().getPersistenceKey());
        if (!m_schoolOids.isEmpty() && m_schoolOids.size() > 1) {
            secondaryCriteria.addAndCriteria(schoolsCriteria.copyWithAdjustedPath(
                    secondaryRelationship,
                    secondaryColumn));
        } else if (!m_schoolOids.isEmpty()) {
            secondaryCriteria.addEqualTo(secondaryColumn, m_schoolOids.iterator().next());
        }



        if (activeOnly) {
            secondaryCriteria.addAndCriteria(m_helper.getActiveStudentCriteria(prefix));
        }

        if (!secondaryCriteria.isEmpty()) {
            criteria.addAndCriteria(secondaryCriteria);
        }
    }

    /**
     * Builds export criteria either for primary or secondary enrollment.
     *
     * @param isPrimary boolean
     * @return Criteria
     */
    private Criteria buildCriteria(boolean isPrimary) {
        X2Criteria criteria = new X2Criteria();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField custodyField = dictionary.findDataDictionaryFieldByAlias(ALIAS_PNT_HAS_CUSTODY);

        if (custodyField != null) {
            criteria.addEqualTo(custodyField.getJavaName(), BooleanAsStringConverter.TRUE);

            if (isPrimary) {
                addSchoolCriteria(criteria, StudentContact.REL_STUDENT + PATH_DELIMITER, false);
            } else {
                addSecondarySchoolCriteria(criteria, StudentContact.REL_STUDENT + PATH_DELIMITER, false);
            }
        } else {
            addNoMatchCriteria(criteria);
        }

        return criteria;
    }

    /**
     * Builds mailing address grid line. This combines lines 01, 02, and 03.
     *
     * @param address Address
     * @return String
     */
    private String buildMailingAddressLine(Address address) {
        StringBuilder addressLine = new StringBuilder(200);

        if (address != null) {
            if (!StringUtils.isEmpty(address.getAddressLine01())) {
                addressLine.append(address.getAddressLine01());
                addressLine.append(" ");
            }

            if (!StringUtils.isEmpty(address.getAddressLine02())) {
                addressLine.append(address.getAddressLine02());
                addressLine.append(" ");
            }

            if (!StringUtils.isEmpty(address.getAddressLine03())) {
                addressLine.append(address.getAddressLine03());
                addressLine.append(" ");
            }
        }

        return addressLine.toString();
    }

    /**
     * Builds physical address grid line. This is a combination of the address line 01 components
     * except for
     * street number.
     *
     * @param address Address
     * @return String
     */
    private String buildPhysicalAddressLine(Address address) {
        StringBuilder addressLine = new StringBuilder(200);

        if (address != null) {
            if (!StringUtils.isEmpty(address.getStreetLetter())) {
                addressLine.append(address.getStreetLetter());
                addressLine.append(" ");
            }

            if (!StringUtils.isEmpty(address.getStreetPreDirection())) {
                addressLine.append(address.getStreetPreDirection());
                addressLine.append(" ");
            }

            if (!StringUtils.isEmpty(address.getStreetName())) {
                addressLine.append(address.getStreetName());
                addressLine.append(" ");
            }

            if (!StringUtils.isEmpty(address.getStreetType())) {
                addressLine.append(address.getStreetType());
                addressLine.append(" ");
            }

            if (!StringUtils.isEmpty(address.getStreetPostDirection())) {
                addressLine.append(address.getStreetPostDirection());
            }
        }

        return addressLine.toString();

    }

    /**
     * Get or set the selection objects for the temporary query.
     *
     * @return Selection
     */
    private Selection getSelectionObject() {
        if (m_schoolOidsSelection == null && m_schoolOids != null && m_schoolOids.size() > 1) {
            m_schoolOidsSelection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
            for (String oid : m_schoolOids) {
                SelectionObject object = X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
                object.setObjectOid(oid);
                m_schoolOidsSelection.addToSelectionObjects(object);
            }
            getBroker().saveBean(m_schoolOidsSelection);

            getTemporaryBeans().add(m_schoolOidsSelection);
            getTemporaryBeans().addAll(m_schoolOidsSelection.getSelectionObjects());
        }
        return m_schoolOidsSelection;
    }

    /**
     * Returns collection with export school OIDs.
     *
     * @return Collection<String>
     */
    private Collection<String> getSchoolOids() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        SubQuery schools = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria);
        return getBroker().getSubQueryCollectionByQuery(schools);
    }

    /**
     * Combines the relationship codes of all contacts with custody as a display into a Map keyed to
     * the student OID.
     *
     * @param isPrimary boolean
     */
    private void loadStudentCustody(boolean isPrimary) {
        QueryByCriteria query = new QueryByCriteria(StudentContact.class, buildCriteria(isPrimary));
        query.addOrderByAscending(StudentContact.COL_STUDENT_OID);
        query.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            Student lastStudent = null;
            StringBuilder contactDisplay = new StringBuilder(100);

            while (iterator.hasNext()) {
                StudentContact contact = (StudentContact) iterator.next();
                Student student = contact.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    contactDisplay = new StringBuilder(100);
                    m_custodyDisplayMap.put(student.getOid(), contactDisplay);
                }

                if (contactDisplay.length() > 0) {
                    contactDisplay.append(" & ");
                }

                contactDisplay.append(contact.getRelationshipCode());

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the user accounts flagged as contacts into a map keyed to the Person OID.
     */
    private void loadUsers() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisUser.REL_PERSON + PATH_DELIMITER + Person.COL_CONTACT_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SisUser.class, criteria);

        m_userMap = getBroker().getMapByQuery(query, SisUser.COL_PERSON_OID, 2048);
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
