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
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the Public Health export which gives a standardized format of students'
 * information used
 * by Health Service Providers.
 *
 * @author X2 Development Corporation
 */
public class PublicHealthExport extends ExportJavaSource {

    /**
     * The Enum HEALTH_EXPORT_FIELDS.
     */
    // Grid fields
    private enum HEALTH_EXPORT_FIELDS {
        FIELD_CARD_NUMBER("Health Card Number", "healthNo"), FIELD_STD_NUMBER("Student Number",
                "stdNo"), FIELD_STD_LAST_NAME("Last Name", "stdLast"), FIELD_STD_FIRST_NAME("First Name",
                        "stdFirst"), FIELD_SKL_ID("School Identifier", "sklId"), FIELD_SKL_ORG_NAME("School Board",
                                "orgName"), FIELD_SKL_NAME("School Name", "sklName"), FIELD_SKL_YEAR("School Year",
                                        "sklYear"), FIELD_STD_GRADE("Grade", "stdGrade"), FIELD_STD_CLASS("Class",
                                                "stdClass"), FIELD_STD_LAST_NAME_2("Alternate Last Name",
                                                        "stdLast2"), FIELD_STD_FIRST_NAME_2("Alternate First Name",
                                                                "stdFirst2"), FIELD_STD_GENDER("Gender",
                                                                        "stdGender"), FIELD_STD_DOB("Date of Birth",
                                                                                "stdDOB"), FIELD_STD_APPART_NUMBER(
                                                                                        "Unit No",
                                                                                        "stdUnit"), FIELD_STREET_NUMBER(
                                                                                                "Street Number",
                                                                                                "streetNo"), FIELD_STREET_NAME(
                                                                                                        "Street Name",
                                                                                                        "streetName"), FIELD_POST_BOX(
                                                                                                                "P.O. Box",
                                                                                                                "postBox"), FIELD_RURAL_ROUTE(
                                                                                                                        "Rural Route",
                                                                                                                        "route"), FIELD_POST_CODE(
                                                                                                                                "Postal Code",
                                                                                                                                "postalCode"), FIELD_CITY(
                                                                                                                                        "City",
                                                                                                                                        "stdCity"), FIELD_STD_PROVINCE(
                                                                                                                                                "Province/Territory",
                                                                                                                                                "stdProvince"), FIELD_STD_TELEPHONE(
                                                                                                                                                        "Telephone Number",
                                                                                                                                                        "stdTelNo"), FIELD_STD_LANGUAGE(
                                                                                                                                                                "Preferred Language",
                                                                                                                                                                "stdLang"), FIELD_STD_ABORIG_STATUS(
                                                                                                                                                                        "Aboriginal Status",
                                                                                                                                                                        "stdAborig"), FIELD_PAR_LAST_NAME(
                                                                                                                                                                                "Parent/Guardian Last Name",
                                                                                                                                                                                "parLast"), FIELD_PAR_FIRST_NAME(
                                                                                                                                                                                        "Parent/Guardian First Name",
                                                                                                                                                                                        "parFirst"), FIELD_PAR_TYPE(
                                                                                                                                                                                                "Parent Type",
                                                                                                                                                                                                "parType"), FIELD_PAR_TELEPHONE(
                                                                                                                                                                                                        "Parent Telephone Number",
                                                                                                                                                                                                        "parTelNo"), FIELD_PAR_EMAIL(
                                                                                                                                                                                                                "Parent E-mail Address",
                                                                                                                                                                                                                "parEmail"), FIELD_PAR_LAST_NAME_2(
                                                                                                                                                                                                                        "Second Parent/Guardian Last Name",
                                                                                                                                                                                                                        "par2Last"), FIELD_PAR_FIRST_NAME_2(
                                                                                                                                                                                                                                "Second Parent/Guardian First Name",
                                                                                                                                                                                                                                "par2First"), FIELD_PAR_2_RELATION(
                                                                                                                                                                                                                                        "Second Parent/Guardian Relationship Type",
                                                                                                                                                                                                                                        "par2Rels"), FIELD_PAR_TELEPHONE_2(
                                                                                                                                                                                                                                                "Second Parent Phone Number",
                                                                                                                                                                                                                                                "par2TelNo"), FIELD_PAR_EMAIL_2(
                                                                                                                                                                                                                                                        "Second Parent E-mail Address",
                                                                                                                                                                                                                                                        "par2Email"), FIELD_STD_REG_DATE(
                                                                                                                                                                                                                                                                "Student Registration Date",
                                                                                                                                                                                                                                                                "stdRegDate"), FIELD_STD_IS_INTERNATIONAL(
                                                                                                                                                                                                                                                                        "International Student Flag",
                                                                                                                                                                                                                                                                        "isStdIter"), FIELD_DOCTOR(
                                                                                                                                                                                                                                                                                "Doctor",
                                                                                                                                                                                                                                                                                "doctor"), FIELD_DOCTOR_PHONE(
                                                                                                                                                                                                                                                                                        "Doctor Phone",
                                                                                                                                                                                                                                                                                        "doctorTelNo");

        private String m_fieldId;
        private String m_fieldName;

        /**
         * Instantiates a new health export fields.
         *
         * @param fieldName String
         * @param id String
         */
        private HEALTH_EXPORT_FIELDS(String fieldName, String id) {
            m_fieldName = fieldName;
            m_fieldId = id;
        }

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
    }

    /*
     * Input parameters
     */
    private static final String SCHOOL_OIDS_PARAM = "schoolOids";

    /*
     * Aliases
     */
    private static final String ALIAS_ABORIG_STATUS = "psn-indian-ancestry";
    private static final String ALIAS_ALT_LAST_NAME = "psn-surname";
    private static final String ALIAS_ALT_FIRST_NAME = "psn-preferred-first-name";
    private static final String ALIAS_CITIZENSHIP = "std-citizenship-status";
    private static final String ALIAS_PO_BOX = "adr-postal-box";
    private static final String ALIAS_RURAL_ROUTE = "adr-rural-route";

    /*
     * Relationship constants
     */
    private static final String RELATIONSHIP_DOCTOR = "Doctor";

    /**
     * The Enum RELATIONSHIP_PRIORITY.
     */
    private enum RELATIONSHIP_PRIORITY {
        MOTHER, FATHER, PARENT, STEP_MOTHER, STEP_FATHER
    }

    /*
     * Other constants
     */
    private static final String VALUE_DELIMITER = ",";
    private static final String VALUE_WRAPPER = "\"";

    /*
     * Member variables
     */
    private Map<String, StudentContact> m_contactsPrimary;
    private Map<String, StudentContact> m_contactsSecondary;
    private Map<String, StudentContact> m_doctorMap;
    private Map<String, List<StudentEnrollment>> m_enrollmentsMap;
    private Collection<String> m_schoolOids;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd");
        DataGrid grid = new DataGrid();

        X2Criteria studentCriteria = buildStudentCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentCriteria);
        query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        query.addOrderByAscending(SisStudent.COL_LOCAL_ID);

        fillEnrollmentsMap(studentCriteria);

        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            int progress = 0;
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                SisPerson person = student.getPerson();

                if (person != null) {
                    if (++progress % 10000 == 0) {
                        AppGlobals.getLog().info("PublicHealthExport processed " + progress + " student records.");
                    }

                    grid.append();

                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_CARD_NUMBER.getFieldId(), student.getMedicaidId());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_NUMBER.getFieldId(), student.getLocalId());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_LAST_NAME.getFieldId(), person.getLastName());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_FIRST_NAME.getFieldId(), person.getFirstName());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_SKL_ID.getFieldId(), student.getSchool().getSchoolId());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_SKL_ORG_NAME.getFieldId(),
                            wrap(student.getSchool().getParentOrganization().getName()));
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_SKL_NAME.getFieldId(), wrap(student.getSchool().getName()));
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_SKL_YEAR.getFieldId(),
                            String.valueOf(getCurrentContext().getSchoolYear()));
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_GRADE.getFieldId(), student.getGradeLevel());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_CLASS.getFieldId(), student.getHomeroom());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_LAST_NAME_2.getFieldId(),
                            person.getFieldValueByAlias(ALIAS_ALT_LAST_NAME));
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_FIRST_NAME_2.getFieldId(),
                            person.getFieldValueByAlias(ALIAS_ALT_FIRST_NAME));
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_GENDER.getFieldId(), person.getGenderCode());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_DOB.getFieldId(),
                            formatter.format(person.getDob() != null ? person.getDob() : ""));
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_IS_INTERNATIONAL.getFieldId(),
                            wrap((String) person.getFieldValueByAlias(ALIAS_CITIZENSHIP)));
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_TELEPHONE.getFieldId(), person.getPhone01());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_LANGUAGE.getFieldId(), student.getHomeLanguageCode());
                    grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_ABORIG_STATUS.getFieldId(),
                            wrap((String) student.getFieldValueByAlias(ALIAS_ABORIG_STATUS)));

                    /*
                     * Address fields
                     */
                    SisAddress address = person.getPhysicalAddress();
                    if (address != null) {
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_APPART_NUMBER.getFieldId(),
                                wrap(address.getAddressLine02()));
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_STREET_NUMBER.getFieldId(),
                                String.valueOf(address.getStreetNumber()));
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_STREET_NAME.getFieldId(), wrap(address.getStreetName()));
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_POST_BOX.getFieldId(),
                                address.getFieldValueByAlias(ALIAS_PO_BOX));
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_RURAL_ROUTE.getFieldId(),
                                address.getFieldValueByAlias(ALIAS_RURAL_ROUTE));
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_POST_CODE.getFieldId(), address.getPostalCode());
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_CITY.getFieldId(), wrap(address.getCity()));
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_PROVINCE.getFieldId(), address.getState());
                    }

                    /*
                     * Primary contact
                     */
                    StudentContact contact1 = m_contactsPrimary.get(student.getOid());
                    if (contact1 != null) {
                        Person contactPerson = contact1.getPerson();
                        if (contactPerson != null) {
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_LAST_NAME.getFieldId(),
                                    contactPerson.getLastName());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_FIRST_NAME.getFieldId(),
                                    contactPerson.getFirstName());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_TYPE.getFieldId(), contact1.getRelationshipCode());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_TELEPHONE.getFieldId(), contactPerson.getPhone01());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_EMAIL.getFieldId(), contactPerson.getEmail01());
                        }
                    }

                    /*
                     * Secondary contact
                     */
                    StudentContact contact2 = m_contactsSecondary.get(student.getOid());
                    if (contact2 != null) {
                        Person contactPerson = contact2.getPerson();
                        if (contactPerson != null) {
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_LAST_NAME_2.getFieldId(),
                                    contactPerson.getLastName());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_FIRST_NAME_2.getFieldId(),
                                    contactPerson.getFirstName());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_2_RELATION.getFieldId(),
                                    contact2.getRelationshipCode());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_TELEPHONE_2.getFieldId(),
                                    contactPerson.getPhone01());
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_PAR_EMAIL_2.getFieldId(), contactPerson.getEmail01());
                        }
                    }

                    /*
                     * Doctor
                     */
                    StudentContact doctor = m_doctorMap.get(student.getOid());
                    if (doctor != null) {
                        Person contactPerson = doctor.getPerson();
                        if (contactPerson != null) {
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_DOCTOR.getFieldId(),
                                    wrap(doctor.getContact().getNameView()));
                            grid.set(HEALTH_EXPORT_FIELDS.FIELD_DOCTOR_PHONE.getFieldId(),
                                    doctor.getPerson().getPhone02());
                        }
                    }

                    /*
                     * Registration date
                     */
                    List<StudentEnrollment> enrollments = m_enrollmentsMap.get(student.getOid());
                    PlainDate regDate = enrollments != null && enrollments.iterator().hasNext()
                            ? enrollments.iterator().next().getEnrollmentDate() : null;
                    if (regDate != null) {
                        grid.set(HEALTH_EXPORT_FIELDS.FIELD_STD_REG_DATE.getFieldId(), formatter.format(regDate));
                    }
                }
            }
        } finally {
            students.close();
        }

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
        List<String> columnNames = new ArrayList(HEALTH_EXPORT_FIELDS.values().length);

        for (HEALTH_EXPORT_FIELDS field : HEALTH_EXPORT_FIELDS.values()) {
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
        List<String> columnUserNames = new ArrayList(HEALTH_EXPORT_FIELDS.values().length);

        for (HEALTH_EXPORT_FIELDS field : HEALTH_EXPORT_FIELDS.values()) {
            columnUserNames.add(field.getFieldName());
        }

        return columnUserNames;
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
        setLineSeparator(FORMAT_EOL_WINDOWS);
        setUseValueWrappers(false);
        setUseEscapes(false);

        m_schoolOids = getSchoolOids();
        loadContacts();
    }

    /**
     * Get student criteria.
     *
     * @return X2Criteria
     */
    private X2Criteria buildStudentCriteria() {
        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addIn(SisStudent.COL_SCHOOL_OID, m_schoolOids);

        stdCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        return stdCriteria;
    }

    /**
     * Get the enrollments map for all students in particular school .
     *
     * @param studentCriteria X2Criteria
     */
    private void fillEnrollmentsMap(X2Criteria studentCriteria) {
        ArrayList typeCodes = new ArrayList(2);
        typeCodes.add(StudentEnrollment.ENTRY);

        Criteria criteria = new Criteria();
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);

        SubQuery students = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, students);

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);

        m_enrollmentsMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 1024);
    }

    /**
     * Returns the priority for inclusion in the export based on the relationship code. If the
     * relationship is not valid for the export -1 is returned.
     *
     * @param relationship String
     * @return int
     */
    private int getExportPriority(String relationship) {
        int exportPriority = 99;

        if (!StringUtils.isEmpty(relationship)) {
            String value = relationship.toUpperCase().replaceAll(" ", "_");
            try {
                RELATIONSHIP_PRIORITY priority = RELATIONSHIP_PRIORITY.valueOf(value);
                exportPriority = priority != null ? priority.ordinal() : 99;
            } catch (IllegalArgumentException iae) {
                // The relationship is not in the priority enum
                exportPriority = 99;
            }

        }

        return exportPriority;
    }

    /**
     * Returns collection with export school oids.
     *
     * @return Collection<String>
     */
    private Collection<String> getSchoolOids() {
        X2Criteria criteria = new X2Criteria();

        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(SCHOOL_OIDS_PARAM);

            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                criteria.addIn(X2BaseBean.COL_OID, oidList);
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return getBroker().getSubQueryCollectionByQuery(new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria));
    }

    /**
     * Loads the primary and secondary contacts for the students.
     */
    private void loadContacts() {
        m_contactsPrimary = new HashMap<String, StudentContact>(16384);
        m_contactsSecondary = new HashMap<String, StudentContact>(16384);
        m_doctorMap = new HashMap<String, StudentContact>(16384);

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentContact.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                m_schoolOids);

        criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                StudentContact.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);
        query.addOrderByAscending(StudentContact.COL_STUDENT_OID);
        query.addOrderByDescending(StudentContact.COL_EMERGENCY_PRIORITY);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            Student lastStudent = null;
            Map<Integer, StudentContact> priorityMap = new HashMap<Integer, StudentContact>(16);

            while (iterator.hasNext()) {
                StudentContact contact = (StudentContact) iterator.next();
                Student student = contact.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    setStudentContacts(lastStudent, priorityMap);

                    // Reset list
                    priorityMap = new HashMap<Integer, StudentContact>(16);
                }

                if (RELATIONSHIP_DOCTOR.equals(contact.getRelationshipCode())) {
                    m_doctorMap.put(student.getOid(), contact);
                } else if (contact.getLivesWithIndicator()) {
                    priorityMap.put(Integer.valueOf(getExportPriority(contact.getRelationshipCode())), contact);
                }

                lastStudent = student;
            }

            setStudentContacts(lastStudent, priorityMap);
        } finally {
            iterator.close();
        }
    }

    /**
     * Sets the first 2 contacts of the student into the maps.
     *
     * @param student Student
     * @param priorityMap Map<Integer,StudentContact>
     */
    private void setStudentContacts(Student student, Map<Integer, StudentContact> priorityMap) {
        if (student != null) {
            List<Integer> priorities = new LinkedList(priorityMap.keySet());
            Collections.sort(priorities);

            int count = 0;

            for (Integer priority : priorities) {
                if (count == 0) {
                    m_contactsPrimary.put(student.getOid(), priorityMap.get(priority));
                } else if (count == 1) {
                    m_contactsSecondary.put(student.getOid(), priorityMap.get(priority));
                }

                count++;
            }
        }
    }

    /**
     * Wraps the value in double-quotes if the value contains a comma or a double-quote.
     *
     * @param value String
     * @return String
     */
    private String wrap(String value) {
        String formattedValue = value;

        if (!StringUtils.isEmpty(formattedValue) &&
                (formattedValue.contains(VALUE_DELIMITER) || formattedValue.contains(VALUE_WRAPPER))) {
            // Escape double-quotes with 2 double-quotes
            formattedValue = StringUtils.replaceAll(formattedValue, VALUE_WRAPPER, VALUE_WRAPPER + VALUE_WRAPPER);

            // Wrap value in quotes
            formattedValue = VALUE_WRAPPER + formattedValue + VALUE_WRAPPER;
        }

        return formattedValue;
    }
}
