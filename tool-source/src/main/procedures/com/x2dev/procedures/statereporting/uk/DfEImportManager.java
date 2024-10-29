/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.IdManager;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class DfEImportManager.
 */
public class DfEImportManager extends DfEManager {
    private static final String DEFAULT_CONTACT_TYPE = "Student";
    private static final String DFE_PROGRAM_CODE_FRL = "FRL";
    private static final String DFE_PROGRAM_CODE_SE = "SE";
    private static final String DFE_PROGRAM_CODE_NAW = "NAW";

    /**
     * Constructor for DfEManager.
     *
     * @param broker X2Broker
     * @param locale Locale
     */
    public DfEImportManager(X2Broker broker, Locale locale) {
        super(broker, locale);
    }

    /**
     * Remove all Contacts of a student and the Contact objects.
     *
     * @param studentOid String
     * @param studentAddressOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void deleteContacts(String studentOid, String studentAddressOid, HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_CONTACTS)) {
            // Get StudentContact
            X2Criteria studentContactCriteria = new X2Criteria();
            studentContactCriteria.addEqualTo(StudentContact.COL_STUDENT_OID, studentOid);
            QueryByCriteria studentContactQuery = new QueryByCriteria(StudentContact.class, studentContactCriteria);
            Collection<StudentContact> studentContacts = m_broker.getCollectionByQuery(studentContactQuery);

            // If the student doesn't have any contacts then there is no need to continue.
            if (studentContacts.size() > 0) {
                X2Criteria contactCriteria = new X2Criteria();
                contactCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_STUDENT_OID,
                        studentOid);
                QueryByCriteria contactQuery = new QueryByCriteria(Contact.class, contactCriteria);
                Collection<Contact> contacts = m_broker.getCollectionByQuery(contactQuery);

                SubQuery contactPersonSubQuery = new SubQuery(Contact.class, Contact.COL_PERSON_OID, contactCriteria);

                X2Criteria personCriteria = new X2Criteria();
                personCriteria.addEqualTo(Person.COL_CONTACT_INDICATOR, Boolean.valueOf(true));
                personCriteria.addIn(X2BaseBean.COL_OID, contactPersonSubQuery);
                QueryByCriteria personQuery = new QueryByCriteria(Person.class, personCriteria);
                Collection<Person> people = m_broker.getCollectionByQuery(personQuery);

                X2Criteria addressCriteria = new X2Criteria();
                addressCriteria.addIn(Address.REL_PEOPLE_PHYSICAL + "." + X2BaseBean.COL_OID, contactPersonSubQuery);
                addressCriteria.addNotEqualTo(X2BaseBean.COL_OID, studentAddressOid);
                QueryByCriteria addressQuery = new QueryByCriteria(Address.class, addressCriteria);
                Collection<Address> addresses = m_broker.getCollectionByQuery(addressQuery);

                // Create a list of selected Oids.
                ArrayList studentContactOids = new ArrayList();
                ArrayList contactOids = new ArrayList();
                ArrayList personOids = new ArrayList();
                ArrayList addressOids = new ArrayList();
                for (StudentContact studentContact : studentContacts) {
                    studentContactOids.add(studentContact.getOid());
                }
                for (Contact contact : contacts) {
                    contactOids.add(contact.getOid());
                }
                for (Person person : people) {
                    personOids.add(person.getOid());
                }
                for (Address address : addresses) {
                    addressOids.add(address.getOid());
                }

                // Delete all student Contacts and Contact related objects
                if (addressOids.size() > 0) {
                    X2Criteria addressCriteria2 = new X2Criteria();
                    addressCriteria2.addIn(X2BaseBean.COL_OID, addressOids);
                    QueryByCriteria addressQuery2 = new QueryByCriteria(Address.class, addressCriteria2);
                    m_broker.deleteByQuery(addressQuery2);
                }
                if (personOids.size() > 0) {
                    X2Criteria personCriteria2 = new X2Criteria();
                    personCriteria2.addIn(X2BaseBean.COL_OID, personOids);
                    QueryByCriteria personQuery2 = new QueryByCriteria(Person.class, personCriteria2);
                    m_broker.deleteByQuery(personQuery2);
                }
                if (contactOids.size() > 0) {
                    X2Criteria contactCriteria2 = new X2Criteria();
                    contactCriteria2.addIn(X2BaseBean.COL_OID, contactOids);
                    QueryByCriteria contactQuery2 = new QueryByCriteria(Contact.class, contactCriteria2);
                    m_broker.deleteByQuery(contactQuery2);
                }
                if (studentContactOids.size() > 0) {
                    X2Criteria studentContactCriteria2 = new X2Criteria();
                    studentContactCriteria2.addIn(X2BaseBean.COL_OID, studentContactOids);
                    QueryByCriteria studentContactQuery2 =
                            new QueryByCriteria(StudentContact.class, studentContactCriteria2);
                    m_broker.deleteByQuery(studentContactQuery2);
                }

            }

        }
    }

    /**
     * Delete a DfE Pupil's Assessments (Student Assessments) records.
     *
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void deleteStudentAssessments(String studentOid, HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
            if (studentOid != null) {
                X2Criteria studentAssessmentCriteria = new X2Criteria();
                studentAssessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, studentOid);

                QueryByCriteria studentAssessmentQuery =
                        new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria);

                m_broker.deleteByQuery(studentAssessmentQuery);
            }
        }
    }

    /**
     * Delete a DfE Pupil's Attendance (Student School) records.
     *
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void deleteStudentAttendanceHistories(String studentOid, HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
            if (studentOid != null) {
                X2Criteria studentSchoolCriteria = new X2Criteria();
                studentSchoolCriteria.addEqualTo(StudentSchool.COL_STUDENT_OID, studentOid);
                studentSchoolCriteria.addEqualTo(StudentSchool.COL_SCHOOL_OID, m_cTFSchool.getOid());

                QueryByCriteria studentSchoolQuery = new QueryByCriteria(StudentSchool.class, studentSchoolCriteria);

                m_broker.deleteByQuery(studentSchoolQuery);
            }
        }
    }

    /**
     * Delete all existing Disability for a student.
     *
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void deleteStudentDisabilities(String studentOid, HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
            if (studentOid != null) {
                X2Criteria disabilityCriteria = new X2Criteria();
                disabilityCriteria.addEqualTo(IepDisability.COL_STUDENT_OID, studentOid);

                QueryByCriteria disabilityQuery = new QueryByCriteria(IepDisability.class, disabilityCriteria);

                m_broker.deleteByQuery(disabilityQuery);
            }
        }
    }

    /**
     * Delete a DfE Pupil's School History (Student Enrollment) records.
     *
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void deleteStudentSchoolHistories(String studentOid, HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
            if (studentOid != null) {
                X2Criteria studentEnrollmentCriteria = new X2Criteria();
                studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, studentOid);

                QueryByCriteria studentEnrollmentQuery =
                        new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);

                m_broker.deleteByQuery(studentEnrollmentQuery);
            }
        }
    }

    /**
     * Delete all existing StudentProgramParticipation for a student.
     *
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void deleteStudentProgramParticipations(String studentOid, HashMap<String, Boolean> activeSections) {
        if (studentOid != null) {
            ArrayList programCodes = new ArrayList();

            if (isSectionActive(activeSections, PARAM_FSM_HISTORY)) {
                String fRLProgramCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_FRL);
                programCodes.add(fRLProgramCode);
            }
            if (isSectionActive(activeSections, PARAM_SEN_HISTORY)) {
                String sEProgramCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_SE);
                programCodes.add(sEProgramCode);
            }
            if (isSectionActive(activeSections, PARAM_NAW_DETAILS)) {
                String nAWProgramCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_NAW);
                programCodes.add(nAWProgramCode);
            }

            if (programCodes.size() > 0) {
                X2Criteria studentProgramParticipationCriteria = new X2Criteria();
                studentProgramParticipationCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, studentOid);
                studentProgramParticipationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCodes);

                QueryByCriteria studentProgramParticipationQuery =
                        new QueryByCriteria(StudentProgramParticipation.class, studentProgramParticipationCriteria);

                m_broker.deleteByQuery(studentProgramParticipationQuery);
            }

        }
    }

    /**
     * Generate a new Student Id
     *
     * If the District level Preference, Student category is not selected then a null is returned.
     *
     * @param schoolOid String
     * @return String
     */
    public String generateStudentId(String schoolOid) {
        School school = (School) m_broker.getBeanByOid(School.class, schoolOid);
        String localId = null;

        try {
            localId = IdManager.generateId(m_broker,
                    school.getOrganization1().getRootOrganization(),
                    SystemPreferenceDefinition.ID_AUTO_ASSIGN,
                    SystemPreferenceDefinition.ID_NEXT_NUMBER,
                    SystemPreferenceDefinition.ID_LENGTH,
                    SystemPreferenceDefinition.ID_PREFIX,
                    SystemPreferenceDefinition.ID_INCREMENT,
                    false);
        } catch (InvalidPreferenceException e) {
            e.printStackTrace();
        }

        return localId;
    }

    /**
     * Save a DfE Address to Aspen Address Table.
     *
     * @param dfEAddress DfEAddress
     * @param address Address
     * @param activeSections HashMap<String,Boolean>
     * @return Address
     */
    public Address saveAddress(DfEAddress dfEAddress, Address address, HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_ADDRESS_PHONE_EMAIL)) {
            if (address == null) {
                address = X2BaseBean.newInstance(Address.class, m_broker.getPersistenceKey());
            }

            address.setOrganization1Oid(DEFAULT_ORGANIZATION);
            if (dfEAddress.hasBS7666Address()) {
                String streetNumber = dfEAddress.getPAON();
                if (streetNumber != null) {
                    streetNumber = streetNumber.trim();
                    if (StringUtils.isNumeric(streetNumber)) {
                        int streetNum = Integer.parseInt(streetNumber);
                        address.setStreetNumber(streetNum);
                    } else {
                        if (streetNumber.length() <= STRING_10_MAX_LENGTH) {
                            address.setStreetLetter(streetNumber);
                        } else {
                            address.setStreetLetter(streetNumber.substring(0, STRING_10_MAX_LENGTH));
                        }
                    }
                }

                String streetName = dfEAddress.getStreet();
                if (streetName != null) {
                    streetName = streetName.trim();
                    if (streetName.length() <= STRING_50_MAX_LENGTH) {
                        address.setStreetName(streetName);
                    } else {
                        address.setStreetName(streetName.substring(0, STRING_50_MAX_LENGTH));
                    }
                }

                // Used in Student Address view field
                String streetAddress = dfEAddress.getPAON() + " " + dfEAddress.getStreet();
                if (streetAddress != null) {
                    streetAddress = streetAddress.trim();
                    if (streetAddress.length() <= STRING_50_MAX_LENGTH) {
                        address.setAddressLine01(streetAddress);
                    } else {
                        address.setAddressLine01(streetAddress.substring(0, STRING_50_MAX_LENGTH));
                    }
                }

                String sAON = dfEAddress.getSAON();
                if (sAON != null) {
                    sAON = sAON.trim();
                    if (sAON.length() <= DB_FIELD_B_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_SAON, sAON, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_SAON, sAON.substring(0, DB_FIELD_B_MAX_LENGTH),
                                getDataDictionary());
                    }
                }

                String locality = dfEAddress.getLocality();
                if (locality != null) {
                    locality = locality.trim();
                    if (locality.length() <= DB_FIELD_B_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_LOCALITY, locality, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_LOCALITY, locality.substring(0, DB_FIELD_B_MAX_LENGTH),
                                getDataDictionary());
                    }
                }

                String administrativeArea = dfEAddress.getAdministrativeArea();
                if (administrativeArea != null) {
                    administrativeArea = administrativeArea.trim();
                    if (administrativeArea.length() <= DB_FIELD_C_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_ADMIN_AREA, administrativeArea, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_ADMIN_AREA,
                                administrativeArea.substring(0, DB_FIELD_C_MAX_LENGTH), getDataDictionary());
                    }
                }

                String postTown = dfEAddress.getPostTown();
                if (postTown != null) {
                    postTown = postTown.trim();
                    if (postTown.length() <= DB_FIELD_C_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_POST_TOWN, postTown, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_POST_TOWN, postTown.substring(0, DB_FIELD_C_MAX_LENGTH),
                                getDataDictionary());
                    }
                }

                String uniquePropertyReferenceNumber = dfEAddress.getUniquePropertyReferenceNumber();
                if (uniquePropertyReferenceNumber != null) {
                    if (uniquePropertyReferenceNumber.length() <= DB_FIELD_B_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM, uniquePropertyReferenceNumber,
                                getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM,
                                uniquePropertyReferenceNumber.substring(0, DB_FIELD_B_MAX_LENGTH), getDataDictionary());
                    }
                }

                String town = dfEAddress.getTown();
                if (town != null) {
                    town = town.trim();
                    if (town.length() <= STRING_40_MAX_LENGTH) {
                        address.setCity(town);
                    } else {
                        address.setCity(town.substring(0, STRING_40_MAX_LENGTH));
                    }
                }

            } else {
                String addressLine01 = dfEAddress.getAddressLine1();
                if (addressLine01 != null) {
                    addressLine01 = addressLine01.trim();
                    if (addressLine01.length() <= STRING_50_MAX_LENGTH) {
                        address.setAddressLine01(addressLine01);
                    } else {
                        address.setAddressLine01(addressLine01.substring(0, STRING_50_MAX_LENGTH));
                    }
                }

                String addressLine02 = dfEAddress.getAddressLine2();
                if (addressLine02 != null) {
                    addressLine02 = addressLine02.trim();
                    if (addressLine02.length() <= STRING_50_MAX_LENGTH) {
                        address.setAddressLine02(addressLine02);
                    } else {
                        address.setAddressLine02(addressLine02.substring(0, STRING_50_MAX_LENGTH));
                    }
                }

                String addressLine03 = dfEAddress.getAddressLine3();
                if (addressLine03 != null) {
                    addressLine03 = addressLine03.trim();
                    if (addressLine03.length() <= STRING_50_MAX_LENGTH) {
                        address.setAddressLine03(addressLine03);
                    } else {
                        address.setAddressLine03(addressLine03.substring(0, STRING_50_MAX_LENGTH));
                    }
                }

                String addressLine04 = dfEAddress.getAddressLine4();
                if (addressLine04 != null) {
                    addressLine04 = addressLine04.trim();
                    if (addressLine04.length() <= STRING_40_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4, addressLine04, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4,
                                addressLine04.substring(0, STRING_40_MAX_LENGTH), getDataDictionary());
                    }
                }

                String addressLine05 = dfEAddress.getAddressLine5();
                if (addressLine05 != null) {
                    addressLine05 = addressLine05.trim();
                    if (addressLine05.length() <= STRING_20_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5, addressLine05, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5,
                                addressLine05.substring(0, STRING_20_MAX_LENGTH), getDataDictionary());
                    }
                }
            }

            String postCode = dfEAddress.getPostCode();
            if (postCode != null) {
                postCode = postCode.trim();
                if (postCode.length() <= STRING_20_MAX_LENGTH) {
                    address.setPostalCode(postCode);
                } else {
                    address.setPostalCode(postCode.substring(0, STRING_20_MAX_LENGTH));
                }
            } else {
                String zip = dfEAddress.getZip();
                if (zip != null) {
                    zip = zip.trim();
                    if (zip.length() <= STRING_20_MAX_LENGTH) {
                        address.setPostalCode(zip);
                    } else {
                        address.setPostalCode(zip.substring(0, STRING_20_MAX_LENGTH));
                    }
                }
            }

            String county = dfEAddress.getCounty();
            if (county != null) {
                county = county.trim();
                if (county.length() <= STRING_40_MAX_LENGTH) {
                    address.setCounty(county);
                } else {
                    address.setCounty(county.substring(0, STRING_40_MAX_LENGTH));
                }
            }

            String country = dfEAddress.getCountry();
            if (country != null) {
                if (country.length() <= STRING_40_MAX_LENGTH) {
                    address.setCountry(country);
                } else {
                    address.setCountry(country.substring(0, STRING_40_MAX_LENGTH));
                }
            }

            String easting = dfEAddress.getEasting();
            if (easting != null) {
                easting = easting.trim();
                if (easting.length() <= DB_FIELD_A_MAX_LENGTH) {
                    address.setFieldValueByAlias(ALIAS_NAME_EASTING, easting, getDataDictionary());
                } else {
                    address.setFieldValueByAlias(ALIAS_NAME_EASTING, easting.substring(0, DB_FIELD_A_MAX_LENGTH),
                            getDataDictionary());
                }
            }

            String northing = dfEAddress.getNorthing();
            if (northing != null) {
                northing = northing.trim();
                if (northing.length() <= DB_FIELD_A_MAX_LENGTH) {
                    address.setFieldValueByAlias(ALIAS_NAME_NORTHING, northing, getDataDictionary());
                } else {
                    address.setFieldValueByAlias(ALIAS_NAME_NORTHING, northing.substring(0, DB_FIELD_A_MAX_LENGTH),
                            getDataDictionary());
                }
            }

            m_broker.saveBeanForced(address);
        }

        return address;
    }

    /**
     * Save a DfE Contact to Aspen Contact Table.
     *
     * @param dfEContact DfEContact
     * @param contactPersonOid String
     * @param addressView String
     * @return Contact
     */
    public Contact saveContact(DfEContact dfEContact, String contactPersonOid, String addressView) {
        Contact contact = X2BaseBean.newInstance(Contact.class, m_broker.getPersistenceKey());
        contact.setOrganization1Oid(DEFAULT_ORGANIZATION);
        contact.setAddressView(addressView);
        contact.setNameView(dfEContact.getSurname() + ", " + dfEContact.getForename());
        contact.setPersonOid(contactPersonOid);
        contact.setContactTypeCode(DEFAULT_CONTACT_TYPE);
        m_broker.saveBeanForced(contact);

        return contact;
    }

    /**
     * Save a DfE Contact to Aspen Person Table.
     *
     * @param dfEContact DfEContact
     * @param contactAddressOid String
     * @return Person
     */
    public Person saveContactPerson(DfEContact dfEContact, String contactAddressOid) {
        Person contactPerson = X2BaseBean.newInstance(Person.class, m_broker.getPersistenceKey());
        contactPerson.setOrganization1Oid(DEFAULT_ORGANIZATION);
        contactPerson.setFieldValueByAlias(ALIAS_NAME_ORDER, Integer.toString(dfEContact.getOrder()),
                getDataDictionary());
        String personTitleCode = dfEContact.getTitle();
        contactPerson.setNameTitleCode(personTitleCode);
        contactPerson.setLastName(dfEContact.getSurname());
        contactPerson.setFirstName(dfEContact.getForename());
        // Translate DfE Gender code.
        String genderCode = lookupRefCodeByStateCode(Person.class, Person.COL_GENDER_CODE, dfEContact.getGender());
        contactPerson.setGenderCode(genderCode);
        String responsibleStr = DB_FALSE;
        if (dfEContact.getResponsible() != null && dfEContact.getResponsible().booleanValue()) {
            responsibleStr = DB_TRUE;
        }
        contactPerson.setFieldValueByAlias(ALIAS_NAME_RESPONSIBLE, responsibleStr, getDataDictionary());
        contactPerson.setMiddleName(dfEContact.getMiddleNames());
        contactPerson.setPhysicalAddressOid(contactAddressOid);
        contactPerson.setMailingAddressOid(contactAddressOid);
        contactPerson.setStudentIndicator(false);
        contactPerson.setContactIndicator(true);
        contactPerson.setEmail01(dfEContact.getEmail());

        ArrayList<DfETelephone> telephones = dfEContact.getTelephones();
        for (int l = 0; l < 3; l++) {
            if (l >= telephones.size()) {
                break;
            }

            DfETelephone telephone = telephones.get(l);
            if (l == 0) {
                contactPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1, telephone.getTelephoneType(),
                        getDataDictionary());
                contactPerson.setPhone01(telephone.getTelephoneNumber());
            } else if (l == 1) {
                contactPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2, telephone.getTelephoneType(),
                        getDataDictionary());
                contactPerson.setPhone02(telephone.getTelephoneNumber());
            } else if (l == 2) {
                contactPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3, telephone.getTelephoneType(),
                        getDataDictionary());
                contactPerson.setPhone03(telephone.getTelephoneNumber());
            }
        }
        m_broker.saveBeanForced(contactPerson);

        return contactPerson;
    }

    /**
     * Save DfE Contacts's to Aspen Person, Address, Contact, StudentContact Tables .
     *
     * @param dfEPupil DfEPupil
     * @param studentOid String
     * @param studentAddressOid String
     * @param studentAddressLine01 String
     * @param activeSections HashMap<String,Boolean>
     */
    public void saveContacts(DfEPupil dfEPupil,
                             String studentOid,
                             String studentAddressOid,
                             String studentAddressLine01,
                             HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_CONTACTS)) {
            // Re-add All Contacts, ContactPerson's and ContactAddresses
            ArrayList<DfEContact> contacts = dfEPupil.getContacts();
            for (int j = 0; j < contacts.size(); j++) {
                DfEContact dfEContact = contacts.get(j);

                // Save ContactAddress
                String contactAddressOid = null;
                String addressView = null;
                Boolean addressAsPupil = dfEContact.getAddressAsPupil();
                boolean livesWithStudent = false;
                if (addressAsPupil != null && addressAsPupil.booleanValue() == true) {
                    livesWithStudent = true;
                    contactAddressOid = studentAddressOid;
                    addressView = studentAddressLine01;
                } else {
                    DfEAddress contactDfEAddress = dfEContact.getDfEAddress();

                    Address contactAddress = null;
                    if (contactDfEAddress != null) {
                        contactAddress = saveAddress(contactDfEAddress, null, activeSections);
                        contactAddressOid = contactAddress.getOid();

                        addressView = contactAddress.getAddressLine01();
                    }
                }

                // Save ContactPerson
                Person contactPerson = saveContactPerson(dfEContact, contactAddressOid);
                String contactPersonOid = contactPerson.getOid();

                // Save Contact
                Contact contact = saveContact(dfEContact, contactPersonOid, addressView);
                String contactOid = contact.getOid();

                // Save StudentContact
                saveStudentContact(dfEContact, contactOid, studentOid, livesWithStudent);
            }
        }
    }

    /**
     * Save a DfE Pupil to Aspen Person, Address, Student, StudentProgramParticipation, Disability,
     * Contact & StudentContact Tables .
     *
     * @param dfEPupil DfEPupil
     * @param schoolOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void saveDfEPupil(DfEPupil dfEPupil, String schoolOid, HashMap<String, Boolean> activeSections) {
        try {
            m_broker.beginTransaction();

            // Save StudentAddress
            DfEAddress dfEAddress = dfEPupil.getDfEAddress();
            Address studentAddress = null;
            String studentAddressOid = null;
            String studentAddressLine1 = null;
            if (dfEAddress != null) {
                studentAddress = saveAddress(dfEAddress, null, activeSections);

                if (studentAddress != null) {
                    studentAddressOid = studentAddress.getOid();
                    studentAddressLine1 = studentAddress.getAddressLine01();
                }
            }

            // Save StudentPerson
            Person studentPerson = saveStudentPerson(dfEPupil, null, studentAddressOid, activeSections);
            String studentPeriodOid = studentPerson.getOid();

            // Save Student
            Student student =
                    saveStudent(dfEPupil, null, studentPeriodOid, studentAddressLine1, schoolOid, activeSections);
            String studentOid = student.getOid();

            // Save StudentProgramParticiation
            saveStudentProgramParticipation(dfEPupil, studentOid, activeSections);

            // Save StudentDisability
            saveStudentDisability(dfEPupil, studentOid, activeSections);

            // Save All Contacts, ContactPerson's and ContactAddress's
            saveContacts(dfEPupil, studentOid, studentAddressOid, studentAddressLine1, activeSections);

            // Add Student Enrollment records
            saveStudentSchoolHistories(dfEPupil, studentOid, activeSections);

            // Add Student Assessment records
            saveStudentAssessments(dfEPupil, studentOid, schoolOid, activeSections);

            // Add Student Attendance Histories records
            saveStudentAttendanceHistories(dfEPupil, studentOid, activeSections);

            m_broker.commitTransaction();
        } catch (RuntimeException re) {
            m_broker.rollbackTransaction();
            throw re;
        }
    }

    /**
     * Save a DfE Pupil to Aspen Student Table.
     *
     * @param dfEPupil DfEPupil
     * @param student Student
     * @param studentPersonOid String
     * @param addressView String
     * @param schoolOid String
     * @param activeSections HashMap<String,Boolean>
     * @return Student
     */
    public Student saveStudent(DfEPupil dfEPupil,
                               Student student,
                               String studentPersonOid,
                               String addressView,
                               String schoolOid,
                               HashMap<String, Boolean> activeSections) {
        // If the student object is null, create new student else update the existing student
        if (student == null) {
            student = X2BaseBean.newInstance(Student.class, m_broker.getPersistenceKey());
        }

        if (student.getLocalId() == null) {
            String localId = generateStudentId(schoolOid);
            student.setLocalId(localId);
        }

        student.setPersonOid(studentPersonOid);
        student.setOrganization1Oid(DEFAULT_ORGANIZATION);
        student.setSchoolOid(schoolOid);
        student.setFieldValueByAlias(ALIAS_NAME_APPLICATION_REFERENCE, dfEPupil.getApplicationReference(),
                getDataDictionary());
        student.setFieldValueByAlias(ALIAS_NAME_UPN, dfEPupil.getUniquePupilNumber(), getDataDictionary());
        student.setFieldValueByAlias(ALIAS_NAME_ULN, dfEPupil.getUniqueLearnerNumber(), getDataDictionary());
        student.setFieldValueByAlias(ALIAS_NAME_UCI, dfEPupil.getUniqueCandidateIdentifier(), getDataDictionary());

        if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
            student.setFieldValueByAlias(ALIAS_NAME_FORMER_UPN, dfEPupil.getFormerUniquePupilNumber(),
                    getDataDictionary());
            student.setFieldValueByAlias(ALIAS_NAME_SEN_PUPIL_PROVISION, dfEPupil.getSENProvision(),
                    getDataDictionary());
            if (dfEPupil.getFSMReviewDate() != null) {
                String fSMReviewDateStr = m_dateFormat.format(dfEPupil.getFSMReviewDate());
                student.setFieldValueByAlias(ALIAS_NAME_FSM_REVIEW_DATE, fSMReviewDateStr, getDataDictionary());
            }
            student.setGradeLevel(dfEPupil.getNCYearActual());
            // Translate DfE Enroll Status code.
            String enrollStatus =
                    lookupRefCodeByStateCode(Student.class, Student.COL_ENROLLMENT_STATUS, dfEPupil.getEnrollStatus());
            student.setEnrollmentStatus(enrollStatus);
            // Translate DfE Language code.
            String languageCode = lookupRefCodeByStateCode(Student.class, Student.COL_HOME_LANGUAGE_CODE,
                    dfEPupil.getFirstLanguageCode());
            student.setHomeLanguageCode(languageCode);
        }

        student.setNameView(dfEPupil.getSurname() + ", " + dfEPupil.getForename());
        student.setAddressView(addressView);

        if (isSectionActive(activeSections, PARAM_LOOKED_AFTER)) {
            String inCare = DB_FALSE;
            if (dfEPupil.getInCare() != null && dfEPupil.getInCare().booleanValue()) {
                inCare = DB_TRUE;
            }
            student.setFieldValueByAlias(ALIAS_NAME_IN_CARE, inCare, getDataDictionary());
            // No translation of DfE Care Authority Code (LEA)
            student.setFieldValueByAlias(ALIAS_NAME_CARE_AUTHORITY, dfEPupil.getCareAuthority(), getDataDictionary());
        }

        // TODO Remove later
        // Should reference by alias, clob in alias file not specific fields
        if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
            student.setFieldValueByAlias(ALIAS_NAME_ATTENDANCE_XML, dfEPupil.getAttendanceXML(), getDataDictionary());
        }

        if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
            student.setFieldValueByAlias(ALIAS_NAME_ASSESSMENTS_XML, dfEPupil.getAssessmentsXML(), getDataDictionary());
        }

        if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
            student.setFieldValueByAlias(ALIAS_NAME_SCHOOL_HISTORY_XML, dfEPupil.getSchoolHistoryXML(),
                    getDataDictionary());
        }

        m_broker.saveBeanForced(student);

        return student;
    }


    /**
     * Save a DfE Pupil Attendance data to Student School table.
     *
     * @param dfEPupil DfEPupil
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void saveStudentAttendanceHistories(DfEPupil dfEPupil,
                                               String studentOid,
                                               HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
            // Including all StageAssessments
            ArrayList<DfEAttendance> attendances = dfEPupil.getAttendances();

            String cTFSchoolOid = m_cTFSchool.getOid();

            // Save Attendance data to Student School table.
            for (int i = 0; i < attendances.size(); i++) {
                DfEAttendance dfEAttendance = attendances.get(i);

                // Create Student Enrollment Entry
                StudentSchool studentSchool = X2BaseBean.newInstance(StudentSchool.class, m_broker.getPersistenceKey());
                studentSchool.setSchoolOid(cTFSchoolOid);
                studentSchool.setStudentOid(studentOid);
                studentSchool.setType(StudentSchool.FORMER);

                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_YEAR, dfEAttendance.getYear());
                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_LEA, dfEAttendance.getLEA());
                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_ESTAB, dfEAttendance.getEstab());
                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SCHOOL_NAME, dfEAttendance.getSchoolName());
                String sessionsPossibleStr = String.valueOf(dfEAttendance.getSessionsPossible());
                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_POSSIBLE, sessionsPossibleStr);
                String sessionsAuthorisedStr = String.valueOf(dfEAttendance.getSessionsAuthorised());
                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_AUTHORIZED, sessionsAuthorisedStr);
                String sessionsAttendedStr = String.valueOf(dfEAttendance.getSessionsAttended());
                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_ATTENDED, sessionsAttendedStr);
                String sessionsUnauthorisedStr = String.valueOf(dfEAttendance.getSessionsUnauthorised());
                studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_UNAUTHORIZED, sessionsUnauthorisedStr);
                if (dfEAttendance.getAttendanceStartDate() != null) {
                    String startDateStr = m_dateFormat.format(dfEAttendance.getAttendanceStartDate());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_START_DATE, startDateStr);
                }
                if (dfEAttendance.getAttendanceMarks() != null) {
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_MARKS, dfEAttendance.getAttendanceMarks());
                }

                m_broker.saveBeanForced(studentSchool);
            }
        }
    }


    /**
     * save a DfE Pupil's Stage Assessment (Student Assessment) records.
     *
     * @param dfEPupil DfEPupil
     * @param studentOid String
     * @param schoolOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void saveStudentAssessments(DfEPupil dfEPupil,
                                       String studentOid,
                                       String schoolOid,
                                       HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
            // Including all StageAssessments
            ArrayList<DfEStageAssessment> stageAssessments = dfEPupil.getStageAssessments();

            if (m_sATAssessmentDefinition != null) {
                String assessmentDefinitionOid = m_sATAssessmentDefinition.getOid();

                DataDictionary extendedDataDictionary = null;

                // Save Stage Assessment data to Student Assessment table.
                for (int i = 0; i < stageAssessments.size(); i++) {
                    DfEStageAssessment dfEStageAssessment = stageAssessments.get(i);

                    // Create Student Enrollment Entry
                    StudentAssessment studentAssessment =
                            X2BaseBean.newInstance(StudentAssessment.class, m_broker.getPersistenceKey());
                    studentAssessment.setSchoolOid(schoolOid);
                    studentAssessment.setStudentOid(studentOid);
                    studentAssessment.setAssessmentDefinitionOid(assessmentDefinitionOid);
                    studentAssessment.setDate(dfEStageAssessment.getResultDate());

                    if (extendedDataDictionary == null) {
                        extendedDataDictionary = DataDictionary.getDistrictDictionary(
                                studentAssessment.getExtendedDataDictionary(), m_broker.getPersistenceKey());
                    }

                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_STAGE, dfEStageAssessment.getStage(),
                            extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_LOCALE, dfEStageAssessment.getLocale(),
                            extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_YEAR_TAKEN, dfEStageAssessment.getYear(),
                            extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_SUBJECT, dfEStageAssessment.getSubject(),
                            extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_METHOD, dfEStageAssessment.getMethod(),
                            extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_COMPONENT, dfEStageAssessment.getComponent(),
                            extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT_STATUS,
                            dfEStageAssessment.getResultStatus(), extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT_QUALIFIER,
                            dfEStageAssessment.getResultQualifier(), extendedDataDictionary);
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT, dfEStageAssessment.getResult(),
                            extendedDataDictionary);
                    String resultDateStr = m_dateFormat.format(dfEStageAssessment.getResultDate());
                    studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT_DATE, resultDateStr,
                            extendedDataDictionary);

                    m_broker.saveBeanForced(studentAssessment);
                }
            }
        }
    }

    /**
     * Save a DfE Contact to Aspen StudentContact Table.
     *
     * @param dfEContact DfEContact
     * @param contactOid String
     * @param studentOid String
     * @param livesWithStudent boolean
     */
    public void saveStudentContact(DfEContact dfEContact,
                                   String contactOid,
                                   String studentOid,
                                   boolean livesWithStudent) {
        StudentContact studentContact = X2BaseBean.newInstance(StudentContact.class, m_broker.getPersistenceKey());
        studentContact.setContactOid(contactOid);
        studentContact.setStudentOid(studentOid);
        studentContact.setLivesWithIndicator(livesWithStudent);
        // Translate DfE Relationship code.
        String relationshipCode = lookupRefCodeByStateCode(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE,
                dfEContact.getRelationshipCode());
        studentContact.setRelationshipCode(relationshipCode);

        m_broker.saveBeanForced(studentContact);
    }

    /**
     * Save a DfE Pupil Disability information to Aspen Student Disability Table.
     *
     * @param dfEPupil DfEPupil
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void saveStudentDisability(DfEPupil dfEPupil, String studentOid, HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
            ArrayList<String> disabilities = dfEPupil.getDisabilities();
            for (int i = 0; i < disabilities.size(); i++) {
                String dfFDisabilityCode = disabilities.get(i);

                IepDisability disability = X2BaseBean.newInstance(IepDisability.class, m_broker.getPersistenceKey());
                disability.setStudentOid(studentOid);
                // Translate DfE Disability code.
                String disabilityCode = lookupRefCodeByStateCode(IepDisability.class, IepDisability.COL_DISABILITY_CODE,
                        dfFDisabilityCode);
                disability.setDisabilityCode(disabilityCode);
                disability.setPrimaryIndicator(false);
                disability.setTransportationIndicator(false);

                m_broker.saveBeanForced(disability);
            }
        }
    }

    /**
     * Save a DfE Pupil to Aspen Person Table.
     *
     * @param dfEPupil DfEPupil
     * @param studentPerson Person
     * @param studentAddressOid String
     * @param activeSections HashMap<String,Boolean>
     * @return Person
     */
    public Person saveStudentPerson(DfEPupil dfEPupil,
                                    Person studentPerson,
                                    String studentAddressOid,
                                    HashMap<String, Boolean> activeSections) {
        // Save StudentPerson
        boolean isNew = false;
        if (studentPerson == null) {
            isNew = true;
            studentPerson = X2BaseBean.newInstance(Person.class, m_broker.getPersistenceKey());
        }

        // Core Fields
        studentPerson.setOrganization1Oid(DEFAULT_ORGANIZATION);
        String lastName = dfEPupil.getSurname();
        if (lastName != null) {
            lastName = lastName.trim();
            if (lastName.length() <= STRING_50_MAX_LENGTH) {
                studentPerson.setLastName(lastName);
            } else {
                studentPerson.setLastName(lastName.substring(0, STRING_50_MAX_LENGTH));
            }
        }
        String firstName = dfEPupil.getForename();
        if (firstName != null) {
            firstName = firstName.trim();
            if (firstName.length() <= STRING_32_MAX_LENGTH) {
                studentPerson.setFirstName(firstName);
            } else {
                studentPerson.setFirstName(firstName.substring(0, STRING_32_MAX_LENGTH));
            }
        }
        String middleName = dfEPupil.getMiddleNames();
        if (middleName != null) {
            middleName = middleName.trim();
            if (middleName.length() <= STRING_32_MAX_LENGTH) {
                studentPerson.setMiddleName(middleName);
            } else {
                studentPerson.setMiddleName(middleName.substring(0, STRING_32_MAX_LENGTH));
            }
        }
        studentPerson.setDob(dfEPupil.getBirthDate());
        // Translate DfE Gender code.
        String genderCode = lookupRefCodeByStateCode(Person.class, Person.COL_GENDER_CODE, dfEPupil.getGender());
        studentPerson.setGenderCode(genderCode);
        studentPerson.setStudentIndicator(true);

        // Address
        if (isNew) {
            studentPerson.setPhysicalAddressOid(studentAddressOid);
            studentPerson.setMailingAddressOid(studentAddressOid);
        }

        // Basic Details Fields
        if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
            studentPerson.setFieldValueByAlias(ALIAS_NAME_PREFERRED_SURNAME, dfEPupil.getPreferredSurname(),
                    getDataDictionary());
            studentPerson.setFieldValueByAlias(ALIAS_NAME_PREFERRED_FORENAME, dfEPupil.getPreferredForename(),
                    getDataDictionary());
            studentPerson.setFieldValueByAlias(ALIAS_NAME_FORMER_SURNAME, dfEPupil.getFormerSurname(),
                    getDataDictionary());
            studentPerson.setFieldValueByAlias(ALIAS_NAME_FORMER_FORENAME, dfEPupil.getFormerForename(),
                    getDataDictionary());
            // No translation of DfE Ethnicity code, save as is.
            studentPerson.setFieldValueByAlias(ALIAS_NAME_ETHNICITY, dfEPupil.getEthnicity(), getDataDictionary());
            String ethnicitySourceFieldName = translateAliasToJavaName(ALIAS_NAME_ETHNICITY_SOURCE, true);
            if (ethnicitySourceFieldName != null) {
                String ethnicitySourceCode =
                        lookupRefCodeByStateCode(Person.class, ethnicitySourceFieldName, dfEPupil.getEthnicitySource());
                studentPerson.setFieldValueByAlias(ALIAS_NAME_ETHNICITY_SOURCE, ethnicitySourceCode,
                        getDataDictionary());
            }
            String medicalFlagStr = DB_FALSE;
            if (dfEPupil.getMedicalFlag() != null && dfEPupil.getMedicalFlag().booleanValue()) {
                medicalFlagStr = DB_TRUE;
            }
            studentPerson.setFieldValueByAlias(ALIAS_NAME_MEDICAL_FLAG, medicalFlagStr, getDataDictionary());
        }

        // Phone and Email Fields
        if (isSectionActive(activeSections, PARAM_ADDRESS_PHONE_EMAIL)) {
            ArrayList<DfETelephone> telephones = dfEPupil.getTelephones();
            for (int l = 0; l < 3; l++) {
                if (l >= telephones.size()) {
                    break;
                }

                DfETelephone telephone = telephones.get(l);
                String telephoneType = telephone.getTelephoneType();
                String telephoneNumber = telephone.getTelephoneNumber();

                if (l == 0) {
                    if (telephoneType != null) {
                        studentPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1, telephoneType,
                                getDataDictionary());
                    }

                    if (telephoneNumber != null) {
                        telephoneNumber = telephoneNumber.trim();
                        if (telephoneNumber.length() <= STRING_20_MAX_LENGTH) {
                            studentPerson.setPhone01(telephoneNumber);
                        } else {
                            studentPerson.setPhone01(telephoneNumber.substring(0, STRING_20_MAX_LENGTH));
                        }
                    }
                } else if (l == 1) {
                    if (telephoneType != null) {
                        studentPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2, telephoneType,
                                getDataDictionary());
                    }

                    if (telephoneNumber != null) {
                        telephoneNumber = telephoneNumber.trim();
                        if (telephoneNumber.length() <= STRING_20_MAX_LENGTH) {
                            studentPerson.setPhone02(telephoneNumber);
                        } else {
                            studentPerson.setPhone02(telephoneNumber.substring(0, STRING_20_MAX_LENGTH));
                        }
                    }
                } else if (l == 2) {
                    if (telephoneType != null) {
                        studentPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3, telephoneType,
                                getDataDictionary());
                    }

                    if (telephoneNumber != null) {
                        telephoneNumber = telephoneNumber.trim();
                        if (telephoneNumber.length() <= STRING_20_MAX_LENGTH) {
                            studentPerson.setPhone03(telephoneNumber);
                        } else {
                            studentPerson.setPhone03(telephoneNumber.substring(0, STRING_20_MAX_LENGTH));
                        }
                    }
                }
            }

            String email = dfEPupil.getEmail();
            if (email != null) {
                email = email.trim();
                if (email.length() <= STRING_100_MAX_LENGTH) {
                    studentPerson.setEmail01(email);
                } else {
                    studentPerson.setEmail01(email.substring(0, STRING_100_MAX_LENGTH));
                }
            }
        }

        m_broker.saveBeanForced(studentPerson);

        return studentPerson;
    }

    /**
     * Save a DfE Pupil SEN information to Aspen Student Table.
     *
     * @param dfEPupil DfEPupil
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void saveStudentProgramParticipation(DfEPupil dfEPupil,
                                                String studentOid,
                                                HashMap<String, Boolean> activeSections) {
        // FMS History fields
        if (isSectionActive(activeSections, PARAM_FSM_HISTORY)) {
            ArrayList<DfEFSMInstance> fSMInstances = dfEPupil.getFSMInstances();
            for (int i = 0; i < fSMInstances.size(); i++) {
                DfEFSMInstance dfEFSMInstance = fSMInstances.get(i);

                StudentProgramParticipation studentProgramParticipation =
                        X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                studentProgramParticipation.setStudentOid(studentOid);
                // Translate DfE Program code.
                String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_FRL);
                studentProgramParticipation.setProgramCode(programCode);
                studentProgramParticipation.setStartDate(dfEFSMInstance.getFSMStartDate());
                studentProgramParticipation.setEndDate(dfEFSMInstance.getFSMEndDate());
                // Translate DfE UK Country Code.
                String uKCountryFieldName = translateAliasToJavaName(ALIAS_NAME_FSM_UK_COUNTRY, true);
                if (uKCountryFieldName != null) {
                    String uKCountryRefCode = lookupRefCodeByStateCode(StudentProgramParticipation.class,
                            uKCountryFieldName, dfEFSMInstance.getUKCountry());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_FSM_UK_COUNTRY, uKCountryRefCode,
                            getDataDictionary());
                }

                m_broker.saveBeanForced(studentProgramParticipation);
            }
        }

        // SEN History fields
        if (isSectionActive(activeSections, PARAM_SEN_HISTORY)) {
            ArrayList<DfESENNeed> sENNeeds = dfEPupil.getSENNeeds();
            for (int i = 0; i < sENNeeds.size(); i++) {
                DfESENNeed sENNeed = sENNeeds.get(i);

                StudentProgramParticipation studentProgramParticipation =
                        X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                studentProgramParticipation.setStudentOid(studentOid);
                // Translate DfE Program code.
                String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_SE);
                studentProgramParticipation.setProgramCode(programCode);
                studentProgramParticipation.setStartDate(dfEPupil.getSENStartDate());
                // No translation of DfE SEN Provision code, save as is.
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_PROVISION, dfEPupil.getSENProvision(),
                        getDataDictionary());
                // No translation of DfE SEN Type code, save as is.
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_TYPE, sENNeed.getType(),
                        getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_RANK, sENNeed.getTypeRank(),
                        getDataDictionary());
                m_broker.saveBeanForced(studentProgramParticipation);
            }
        }

        // NAW Details fields
        if (isSectionActive(activeSections, PARAM_NAW_DETAILS)) {
            ArrayList<DfENAWDetail> nAWDetails = dfEPupil.getNAWDetails();
            if (nAWDetails.size() > 0) {
                // Only interested in the first one.
                DfENAWDetail nAWDetail = nAWDetails.get(0);

                StudentProgramParticipation studentProgramParticipation =
                        X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                studentProgramParticipation.setStudentOid(studentOid);
                // Translate DfE Program code.
                String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_NAW);
                studentProgramParticipation.setProgramCode(programCode);
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SPEAK_WELSH, nAWDetail.getSpeakWelsh(),
                        getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_HOME_WELSH, nAWDetail.getHomeWelsh(),
                        getDataDictionary());
                // Translate DfE National Identity Code.
                String nationalIdentityFieldName = translateAliasToJavaName(ALIAS_NAME_NATIONAL_IDENTITY, true);
                if (nationalIdentityFieldName != null) {
                    String nationalIdentityRefCode = lookupRefCodeByStateCode(StudentProgramParticipation.class,
                            nationalIdentityFieldName, nAWDetail.getNationalIdentity());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_NATIONAL_IDENTITY,
                            nationalIdentityRefCode, getDataDictionary());
                }
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_WELSH_SOURCE, nAWDetail.getWelshSource(),
                        getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_EAL_ACQUISITION,
                        nAWDetail.getEALAcquisition(), getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_LANGUAGE_SOURCE,
                        nAWDetail.getLanguageSource(), getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_CURR_TEACH_METHOD,
                        nAWDetail.getSENCurrTeachingMethods(), getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_GROUPING_SUPPORT,
                        nAWDetail.getSENGroupingAndSupport(), getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_SPEC_RESOURCES,
                        nAWDetail.getSENSpecialisedResources(), getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_ADVICE_ASSESSMENT,
                        nAWDetail.getSENAdviceAndAssessment(), getDataDictionary());

                m_broker.saveBeanForced(studentProgramParticipation);
            }
        }
    }

    /**
     * save a DfE Pupil's School History (Student Enrollment) records.
     *
     * @param dfEPupil DfEPupil
     * @param studentOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void saveStudentSchoolHistories(DfEPupil dfEPupil,
                                           String studentOid,
                                           HashMap<String, Boolean> activeSections) {
        if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
            ArrayList<DfESchoolHistory> schoolHistories = dfEPupil.getSchoolHistories();

            for (int i = 0; i < schoolHistories.size(); i++) {
                DfESchoolHistory dfESchoolHistory = schoolHistories.get(i);

                String lEA = dfESchoolHistory.getLEA();
                String estab = dfESchoolHistory.getEstab();
                String schoolName = dfESchoolHistory.getSchoolName();
                School school = getSchoolByEstab(estab);
                boolean isCTFSchool = false;

                // If the school is not in our LEA then get the dummy "CTF School".
                if (school == null) {
                    school = m_cTFSchool;
                    isCTFSchool = true;
                }
                String schoolOid = school.getOid();

                PlainDate entryDate = dfESchoolHistory.getEntryDate();
                if (entryDate != null) {
                    // Create Student Enrollment Entry
                    StudentEnrollment studentEnrollment =
                            X2BaseBean.newInstance(StudentEnrollment.class, m_broker.getPersistenceKey());
                    studentEnrollment.setSchoolOid(schoolOid);
                    studentEnrollment.setStudentOid(studentOid);
                    studentEnrollment.setEnrollmentDate(entryDate);
                    studentEnrollment.setEnrollmentType(StudentEnrollment.ENTRY);

                    // If the School is out-of-LEA then save the LEA and School information on the
                    // Student Enrollment record.
                    if (isCTFSchool) {
                        if (lEA.length() <= STRING_10_MAX_LENGTH) {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID, lEA, getDataDictionary());
                        } else {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID,
                                    lEA.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                        }
                        if (estab.length() <= STRING_10_MAX_LENGTH) {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB, estab,
                                    getDataDictionary());
                        } else {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                    estab.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                        }
                        if (schoolName.length() <= STRING_50_MAX_LENGTH) {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME, schoolName,
                                    getDataDictionary());
                        } else {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME,
                                    schoolName.substring(0, STRING_50_MAX_LENGTH), getDataDictionary());
                        }
                    }

                    m_broker.saveBeanForced(studentEnrollment);
                }

                PlainDate leavingDate = dfESchoolHistory.getLeavingDate();
                String leavingReason = dfESchoolHistory.getLeavingReason();
                if (leavingDate != null) {
                    // Create Student Enrollment Withdrawal
                    StudentEnrollment studentEnrollment =
                            X2BaseBean.newInstance(StudentEnrollment.class, m_broker.getPersistenceKey());
                    studentEnrollment.setSchoolOid(schoolOid);
                    studentEnrollment.setStudentOid(studentOid);
                    studentEnrollment.setEnrollmentDate(leavingDate);
                    studentEnrollment.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
                    if (leavingReason.length() <= STRING_10_MAX_LENGTH) {
                        studentEnrollment.setEnrollmentCode(leavingReason);
                    } else {
                        studentEnrollment.setEnrollmentCode(leavingReason.substring(0, STRING_10_MAX_LENGTH));
                    }

                    // If the School is out-of-LEA then save the LEA and School information on the
                    // Student Enrollment record.
                    if (isCTFSchool) {
                        if (lEA.length() <= STRING_10_MAX_LENGTH) {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID, lEA, getDataDictionary());
                        } else {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID,
                                    lEA.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                        }
                        if (estab.length() <= STRING_10_MAX_LENGTH) {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB, estab,
                                    getDataDictionary());
                        } else {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                    estab.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                        }
                        if (schoolName.length() <= STRING_50_MAX_LENGTH) {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME, schoolName,
                                    getDataDictionary());
                        } else {
                            studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME,
                                    schoolName.substring(0, STRING_50_MAX_LENGTH), getDataDictionary());
                        }
                    }

                    m_broker.saveBeanForced(studentEnrollment);
                }
            }
        }
    }

    /**
     * Update a DfE Pupil's Aspen Person, Address, Student, StudentProgramParticipation, Disability,
     * Contact & StudentContact Tables .
     *
     * @param dfEPupil DfEPupil
     * @param student Student
     * @param schoolOid String
     * @param activeSections HashMap<String,Boolean>
     */
    public void updateDfEPupil(DfEPupil dfEPupil,
                               Student student,
                               String schoolOid,
                               HashMap<String, Boolean> activeSections) {
        try {
            m_broker.beginTransaction();

            String studentOid = student.getOid();
            String studentPersonOid = student.getPersonOid();
            Person studentPerson = student.getPerson();

            // Save StudentPerson
            studentPerson = saveStudentPerson(dfEPupil, studentPerson, null, activeSections);
            String studentAddressOid = studentPerson.getPhysicalAddressOid();

            // Save StudentAddress
            DfEAddress dfEAddress = dfEPupil.getDfEAddress();
            Address studentAddress = null;
            String studentAddressLine1 = null;
            if (dfEAddress != null) {
                studentAddress = studentPerson.getPhysicalAddress();
                studentAddress = saveAddress(dfEAddress, studentAddress, activeSections);

                studentAddressOid = studentAddress.getOid();
                studentAddressLine1 = studentAddress.getAddressLine01();

                // This scenario can happen if they didn't have an address on the first import but
                // later provided their address information on a second import.
                if (studentPerson.getPhysicalAddressOid() == null) {
                    studentPerson.setPhysicalAddressOid(studentAddressOid);
                    studentPerson = saveStudentPerson(dfEPupil, studentPerson, null, activeSections);
                }
            }

            saveStudent(dfEPupil, student, studentPersonOid, studentAddressLine1, schoolOid, activeSections);

            // Remove any existing StudentProgramParticipation records
            deleteStudentProgramParticipations(studentOid, activeSections);
            // Re-add StudentProgramParticiation
            saveStudentProgramParticipation(dfEPupil, studentOid, activeSections);

            // Remove any existing StudentProgramParticipation records
            deleteStudentDisabilities(studentOid, activeSections);
            // Re-add StudentDisability
            saveStudentDisability(dfEPupil, studentOid, activeSections);

            // Remove all Contacts, ContactPerson's and ContactAddresses
            deleteContacts(studentOid, studentAddressOid, activeSections);
            // Re-add All Contacts, ContactPerson's and ContactAddresses
            saveContacts(dfEPupil, studentOid, studentAddressOid, studentAddressLine1, activeSections);

            // Remove any existing Student Enrollment records
            deleteStudentSchoolHistories(studentOid, activeSections);
            // Re-add Student Enrollment records
            saveStudentSchoolHistories(dfEPupil, studentOid, activeSections);

            // Remove any existing Student Assessment records
            deleteStudentAssessments(studentOid, activeSections);
            // Re-add Student Assessment records
            saveStudentAssessments(dfEPupil, studentOid, schoolOid, activeSections);

            // Remove any existing Student Attendance Histories records
            deleteStudentAttendanceHistories(studentOid, activeSections);
            // Re-add Student Attendance Histories records
            saveStudentAttendanceHistories(dfEPupil, studentOid, activeSections);

            m_broker.commitTransaction();
        } catch (RuntimeException re) {
            m_broker.rollbackTransaction();
            throw re;
        }

    }


}
