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

package com.x2dev.procedures.statereporting.va;

import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.CalculatedFieldManager;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.presentation.AddressParser;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Java source for Brookline staff import. Each record has staff, person and staff school
 * association. This import will create or update those three beans as necessary.
 *
 * @author X2 Development Corporation
 */
public class StaffImport extends TextImportJavaSource
{

    private static final String DATE_FORMAT = "yyyy/MM/dd";
    private static final String SKIP_PARAM = "skipFirst";
    /**
     * Name for the "enrollment status" report parameter. This value is a String.
     */
    public static final String ENROLLMENT_STATUS_PARAM = "status";

    /*
     * aliases
     */
    private static final String DOE_LICENSE_PREFIX = "DOE LICENSE PREFIX";
    private static final String DOE_LICENSE_NUMBER = "DOE LICENSE NUMBER";


    /*
     * Input order
     */
    private static final int INDEX_FIRST_NAME = 0;
    private static final int INDEX_MIDDLE_NAME = 1;
    private static final int INDEX_LAST_NAME = 2;
    private static final int INDEX_SCHOOL_NAME = 3;
    private static final int INDEX_LASID = 4;
    private static final int INDEX_STAFF_TYPE = 5;
    private static final int INDEX_GENDER_CODE = 6;
    private static final int INDEX_DOB = 7;
    private static final int INDEX_STATUS = 8;
    private static final int INDEX_HIRE_DATE = 9;
    private static final int INDEX_BARGAINING_UNIT = 10;
    private static final int INDEX_ADDRESS_LINE01 = 11;
    private static final int INDEX_ADDRESS_LINE02 = 12;
    private static final int INDEX_ADDRESS_LINE03 = 13;
    private static final int INDEX_HISP_LATINO_IND = 14;
    private static final int INDEX_RACES = 15;
    private static final int INDEX_PRIMARY_EMAIL = 16;
    private static final int INDEX_USERID = 17;
    private static final int INDEX_SOCSEC = 18;
    private static final int INDEX_CITY = 19;
    private static final int INDEX_STATE = 20;
    private static final int INDEX_ZIP = 21;
    private static final int INDEX_HIGHQUAL = 22;
    private static final int INDEX_LICENSCENBR = 23;
    private static final int INDEX_ISSUE_DATE = 24;
    private static final int INDEX_EXPIRATION_DATE = 25;


    private SimpleDateFormat m_simpleDateFormat = new SimpleDateFormat(DATE_FORMAT);
    private CalculatedFieldManager m_calculatedFieldManager = null;
    private DateConverter m_dateConverter = null;
    private ModelBroker m_modelBroker;
    private String m_organizationOid;
    private Map<String, SisStaff> m_staffById = null;
    private Map<String, SisSchool> m_schoolByName = null;
    private int m_insertLicCount = 0;
    private int m_matchLicCount = 0;
    private int m_skipLicCount = 0;
    private int m_updateLicCount = 0;

    /**
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount()
    {
        return 26;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#getImportStatistics()
     */
    @Override
    protected StringBuilder getImportStatistics() {
        MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        Integer insertCount = Integer.valueOf(m_insertLicCount);
        Integer matchCount = Integer.valueOf(m_matchLicCount);
        Integer skipCount = Integer.valueOf(m_skipLicCount);
        Integer updateCount = Integer.valueOf(m_updateLicCount);
        Integer total = Integer.valueOf(m_matchLicCount + m_insertLicCount + m_skipLicCount);

        StringBuilder buffer = super.getImportStatistics();
        int i = buffer.lastIndexOf("Records matched");
        i -= 2;
        buffer.replace(i, i, "\n\n\nStaff record statistics:");

        buffer.append('\n');
        buffer.append('\n');
        buffer.append("License record statistics:");
        buffer.append('\n');
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.matchCount", matchCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.updateCount", updateCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.insertCount", insertCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.skipCount", skipCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.rule", skipCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.total", total));
        buffer.append('\n');
        return buffer;
    }

    /**
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#importRecord(java.util.List, int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber)
    {
        if (!(lineNumber == 1 && isSkipFirst())) {

            try
            {
                importStaff(record, lineNumber);
            } catch (Exception e)
            {
                String location = "";
                if (e.getStackTrace().length > 0)
                {
                    location = " at " + e.getStackTrace()[0].toString();
                }

                logInvalidRecord(lineNumber, e.getClass().getSimpleName() + " importing data: " + e.getMessage()
                        + location);
                incrementSkipCount();
            }
        }

    }

    /**
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize()
    {
        m_dateConverter =
                (DateConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());

        m_modelBroker = new ModelBroker(getPrivilegeSet());

        m_organizationOid = ((SisOrganization) getOrganization()).getOid();

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class);
        m_schoolByName = getBroker().getMapByQuery(schoolQuery, SisSchool.COL_NAME, 30);

        QueryByCriteria query = new QueryByCriteria(SisStaff.class);
        m_staffById = getBroker().getMapByQuery(query, SisStaff.COL_LOCAL_ID, 5000);

        // This should be initialized after model broker has been initialized.
        m_calculatedFieldManager = new CalculatedFieldManager(m_modelBroker);
    }

    /**
     * Increments the number of license beans that have been created by one.
     *
     * @see #incrementInsertCount(int)
     * @see #getInsertCount()
     */
    protected void incrementInsertLicenseCount() {
        m_insertLicCount++;
    }

    /**
     * Increments the number of License beans that have been created by the passed value.
     *
     * @param inserted int
     * @see #incrementInsertCount()
     * @see #getInsertCount()
     */
    protected void incrementInsertLicenseCount(int inserted) {
        m_insertLicCount += inserted;
    }

    /**
     * Increments the number of License records in the source file that have been matched by one.
     *
     * @see #incrementMatchCount(int)
     * @see #getMatchCount()
     */
    protected void incrementMatchLicenseCount() {
        m_matchLicCount++;
    }

    /**
     * Increments the number of License records in the source file that have been matched by the
     * passed
     * value.
     *
     * @param matched int
     * @see #incrementMatchCount()
     * @see #getMatchCount()
     */
    protected void incrementMatchLicenseCount(int matched) {
        m_matchLicCount += matched;
    }

    /**
     * Increments the number of License records in the source file that have been skipped by one.
     *
     * @see #incrementSkipCount(int)
     * @see #getSkipCount()
     */
    protected void incrementSkipLicenseCount() {
        m_skipLicCount++;
    }

    /**
     * Increments the number of License records in the source file that have been skipped by the
     * passed
     * value.
     *
     * @param skipped int
     * @see #incrementSkipCount()
     * @see #getSkipCount()
     */
    protected void incrementSkipLicenseCount(int skipped) {
        m_skipLicCount += skipped;
    }

    /**
     * Increments the number of existing License beans that have been updated by one. Records that
     * were
     * updated are a subset of the records that matched.
     *
     * @see #incrementUpdateCount(int)
     * @see #getUpdateCount()
     */
    protected void incrementUpdateLicenseCount() {
        m_updateLicCount++;
    }

    /**
     * Increments the number of existing License beans that have been updated by the passed value.
     * Records
     * that were updated are a subset of the records that matched.
     *
     * @param updated int
     * @see #getUpdateCount()
     * @see #incrementUpdateCount()
     */
    protected void incrementUpdateLicenseCount(int updated) {
        m_updateLicCount += updated;
    }

    private boolean isSkipFirst() {
        Boolean skip = (Boolean) getParameter(SKIP_PARAM);
        return skip == null ? false : skip.booleanValue();

    }

    /**
     * Imports the staff data from the given record. This includes the following beans:
     * <ul>
     * <li>Staff
     * <li>Person
     * <li>Address
     * <ul>
     * This method will update existing beans or create new ones depending upon whether or not the
     * ID is found in the database.
     *
     * @param record
     * @param lineNumber
     *
     * @throws X2BaseException
     */
    private void importStaff(List<String> record, int lineNumber) throws X2BaseException
    {
        String localId = record.get(INDEX_LASID);

        /*
         * Retrieve the existing beans or create new ones
         */
        SisStaff staff = (m_staffById.get(localId));
        if (staff == null)
        {
            SisAddress address = X2BaseBean.newInstance(SisAddress.class, getBroker().getPersistenceKey());
            SisPerson person = X2BaseBean.newInstance(SisPerson.class, getBroker().getPersistenceKey());

            staff = X2BaseBean.newInstance(SisStaff.class, getBroker().getPersistenceKey());

            setAddressFields(address, record);
            person.setPhysicalAddressOid(address.getOid());


            setPersonFields(person, record);
            staff.setPersonOid(person.getOid());

            setRaceFields(person, record);

            staff.setOrganization1Oid(((SisOrganization) getOrganization()).getOid());
            setStaffFields(staff, record);

            // call setCertificationFields after staff saved
            setCertificationFields(staff, record);

        }
        else
        {
            incrementMatchCount();
            SisPerson person = staff.getPerson();
            SisAddress address = (person.getPhysicalAddress());

            setAddressFields(address, record);
            setPersonFields(person, record);
            setRaceFields(person, record);
            setStaffFields(staff, record);
            setCertificationFields(staff, record);
        }
    }

    /**
     * Converts the source date from the input file format to a PlainDate.
     *
     * @param sourceDate a date string formatted as MM/dd/yyyy
     *
     * @return A PlainDate, this value may be null if the source date is empty
     */
    private PlainDate convertDate(String sourceDate)
    {
        PlainDate plainDate = null;

        if (!StringUtils.isEmpty(sourceDate))
        {
            plainDate = m_dateConverter.stringToJava(sourceDate);
        }

        return plainDate;
    }

    /**
     * Sets the address fields as given by the input.
     *
     * @param address
     * @param record
     */
    private void setAddressFields(SisAddress address, List<String> record)
    {
        if (address != null)
        {
            address.setOrganization1Oid(m_organizationOid);

            address.setAddressLine01(record.get(INDEX_ADDRESS_LINE01));
            address.setAddressLine02(record.get(INDEX_ADDRESS_LINE02));
            address.setAddressLine03(record.get(INDEX_ADDRESS_LINE03));

            address.setCity(record.get(INDEX_CITY));
            address.setState(record.get(INDEX_STATE));
            address.setPostalCode(record.get(INDEX_ZIP));


            AddressParser.parseLine01(getOrganization(), address.getAddressLine01(), address, true, m_modelBroker);
            AddressParser.parseLine03(getOrganization(), address.getAddressLine03(), address, true, m_modelBroker);

            m_modelBroker.saveBeanForced(address);
        }
    }

    /**
     * Important!!! Staff input object should has oid
     * check does certification exist. If exist - update, if doesn't exist - create certification<br>
     *
     * @param staff
     * @param record
     * @throws X2BaseException
     */
    private void setCertificationFields(SisStaff staff, List<String> record) throws X2BaseException
    {
        StaffCertification certification = null;
        for (StaffCertification staffCertification : staff.getCertifications()) {
            String licenseNumber = (String) staffCertification.getFieldValueByAlias(DOE_LICENSE_NUMBER);
            String recordLicence = record.get(INDEX_LICENSCENBR);
            if (licenseNumber != null && licenseNumber.equals(recordLicence)) {
                certification = staffCertification;
                break;
            }
        }
        if (certification == null) {
            certification = X2BaseBean.newInstance(StaffCertification.class, getBroker().getPersistenceKey());
            certification.setStaffOid(staff.getOid());
            certification.setPrimaryIndicator(true);
        } else {
            incrementMatchLicenseCount();
        }

        String licenceNumber = record.get(INDEX_LICENSCENBR);
        String prefix = null;
        if (!StringUtils.isEmpty(licenceNumber)) {
            String[] splittedLicence = licenceNumber.split("-");
            if (splittedLicence.length > 1) {
                prefix = splittedLicence[0];
            }
        }

        String recordIssueDate = record.get(INDEX_ISSUE_DATE);
        String recordExpirationDate = record.get(INDEX_EXPIRATION_DATE);
        PlainDate issueDate = null;
        PlainDate expirationDate = null;
        try {
            issueDate = new PlainDate(m_simpleDateFormat.parse(recordIssueDate));
            expirationDate = new PlainDate(m_simpleDateFormat.parse(recordExpirationDate));
        } catch (ParseException e) {
            throw new X2BaseException(e);
        }

        certification.setFieldValueByAlias(DOE_LICENSE_NUMBER, licenceNumber);
        certification.setFieldValueByAlias(DOE_LICENSE_PREFIX, prefix);
        certification.setIssueDate(issueDate);
        certification.setExpirationDate(expirationDate);
        if (certification.isDirty()) {
            if (certification.isNew()) {
                incrementInsertLicenseCount();
            } else {
                incrementUpdateLicenseCount();
            }
            m_modelBroker.saveBeanForced(certification);
        }
    }

    /**
     * Sets the person fields as given by the input.
     *
     * @param person
     * @param record
     */
    private void setPersonFields(SisPerson person, List<String> record) throws X2BaseException
    {
        if (person != null)
        {
            person.setOrganization1Oid(m_organizationOid);

            String dateOfBirth = record.get(INDEX_DOB);
            person.setDob(convertDate(dateOfBirth));

            person.setFirstName(record.get(INDEX_FIRST_NAME));
            person.setLastName(record.get(INDEX_LAST_NAME));
            person.setMiddleName(record.get(INDEX_MIDDLE_NAME));

            person.setEmail01(record.get(INDEX_PRIMARY_EMAIL).trim());
            person.setEmail02(record.get(INDEX_USERID).trim());

            person.setGenderCode(record.get(INDEX_GENDER_CODE));

            person.setHispanicLatinoIndicator("0".equals(record.get(INDEX_HISP_LATINO_IND)) ? false : true);
            person.setStaffIndicator(true);

            person.setPersonId(record.get(INDEX_SOCSEC));

            m_calculatedFieldManager.refreshCalculatedFields(person);
            m_modelBroker.saveBeanForced(person);
        }
    }

    /**
     * Creates race beans for the given person. Existing race beans are deleted.
     *
     * @param person
     * @param record
     */
    private void setRaceFields(SisPerson person, List<String> record)
    {
        // Remove all the race records
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Race.COL_PERSON_OID, person.getOid());

        QueryByCriteria query = new QueryByCriteria(Race.class, criteria);
        m_modelBroker.deleteByQuery(query);

        String raceCode = record.get(INDEX_RACES);

        if (raceCode.contains("&") && !"Amer. Ind. & Alaska Nat.".equals(raceCode))
        {
            String[] races = raceCode.split("&");
            for (String race : races)
            {
                Race raceBean = X2BaseBean.newInstance(Race.class, getBroker().getPersistenceKey());
                raceBean.setRaceCode(race);
                raceBean.setPersonOid(person.getOid());
                m_modelBroker.saveBeanForced(raceBean);
            }
        }
        else
        {
            Race raceBean = X2BaseBean.newInstance(Race.class, getBroker().getPersistenceKey());
            raceBean.setRaceCode(raceCode);
            raceBean.setPersonOid(person.getOid());
            m_modelBroker.saveBeanForced(raceBean);
        }
    }

    /**
     * Sets the staff fields as given by the input.
     *
     * @param staff
     * @param record
     */
    private void setStaffFields(SisStaff staff, List<String> record) throws X2BaseException
    {
        if (staff != null)
        {
            staff.setOrganization1Oid(m_organizationOid);
            staff.setLocalId(record.get(INDEX_LASID));
            staff.setStaffType(record.get(INDEX_STAFF_TYPE));
            staff.setStatus(record.get(INDEX_STATUS));

            staff.setStateId(record.get(INDEX_LASID));
            staff.setFieldA003(record.get(INDEX_HIGHQUAL));


            staff.setMedicaidId(record.get(INDEX_LICENSCENBR));


            String schoolName = record.get(INDEX_SCHOOL_NAME);


            if (!StringUtils.isEmpty(schoolName))
            {
                String schoolOid = m_schoolByName.get(schoolName).getOid();
                if (schoolOid != null)
                {
                    staff.setSchoolOid(schoolOid);
                }
            }

            String hireDate = record.get(INDEX_HIRE_DATE);
            staff.setHireDate(convertDate(hireDate));

            staff.setBargainingUnit(record.get(INDEX_BARGAINING_UNIT));

            m_calculatedFieldManager.refreshCalculatedFields(staff);
            if (staff.isDirty()) {
                if (staff.isNew()) {
                    incrementInsertCount();
                } else {
                    incrementUpdateCount();
                }
                m_modelBroker.saveBeanForced(staff);
            }
        }
    }
}
