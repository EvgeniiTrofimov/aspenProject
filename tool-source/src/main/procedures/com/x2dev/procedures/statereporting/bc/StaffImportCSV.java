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

package com.x2dev.procedures.statereporting.bc;

import com.follett.fsc.core.k12.beans.OrganizationChild;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.CalculatedFieldManager;
import com.follett.fsc.core.k12.business.IdManager;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.presentation.AddressParser;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for BC staff import. Each record has staff, person and staff school
 * association. This import will create or update those three beans as necessary.
 *
 * @author X2 Development Corporation
 */
public class StaffImportCSV extends TextImportJavaSource {
    /*
     * Input order
     */
    private static final int INDEX_FIRST_NAME = 0;
    private static final int INDEX_LAST_NAME = 1;
    private static final int INDEX_GENDER_CODE = 2;
    private static final int INDEX_HOME_PHONE = 3;
    private static final int INDEX_WORK_PHONE = 4;
    private static final int INDEX_CELL_PHONE = 5;
    private static final int INDEX_EMAIL = 6;
    private static final int INDEX_STAFF_TYPE = 7;
    private static final int INDEX_STATUS = 8;
    private static final int INDEX_HOME_ADDRESS_1 = 9;
    private static final int INDEX_HOME_ADDRESS_2 = 10;
    private static final int INDEX_HOME_ADDRESS_3 = 11;
    private static final int INDEX_SCHOOL_NAME = 12;
    private static final int INDEX_DISTRICT_NAME = 13;

    private static final String STAFF_INDICATOR_VALUE = "true";

    private CalculatedFieldManager m_calculatedFieldManager = null;
    private Map<String, SisOrganization> m_districtByName = null;
    private ModelBroker m_modelBroker;
    private List<SisPerson> m_personsMatched = null;
    private Map<String, SisSchool> m_schoolByName = null;
    private Map<String, SisPerson> m_staffPersonByEmail = null;
    private Map<String, List<String>> m_recordByPsnOid = null;

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 14;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        super.importData(sourceFile);
        updateExistingRecords();
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#importRecord(java.util.List, int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) {
        try {
            importStaff(record, lineNumber);
        } catch (Exception e) {
            String location = "";
            if (e.getStackTrace().length > 0) {
                location = " at " + e.getStackTrace()[0].toString();
            }

            logInvalidRecord(lineNumber,
                    e.getClass().getSimpleName() + " importing data: " + e.getMessage() + location);
            incrementSkipCount();
        }
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_modelBroker = new ModelBroker(getPrivilegeSet());

        QueryByCriteria districtQuery = new QueryByCriteria(SisOrganization.class);
        m_districtByName = m_modelBroker.getMapByQuery(districtQuery, SisOrganization.COL_NAME, 70);

        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class);
        m_schoolByName = m_modelBroker.getMapByQuery(schoolQuery, SisSchool.COL_NAME, 2500);

        Criteria staffPersonByEmailCriteria = new Criteria();
        staffPersonByEmailCriteria.addEqualTo(SisPerson.COL_STAFF_INDICATOR, STAFF_INDICATOR_VALUE);
        staffPersonByEmailCriteria.addEqualTo(
                OrganizationChild.ORGANIZATION_OID_COLS[getOrganization().getOrganizationDefinition().getLevel()],
                getOrganization().getOid());
        QueryByCriteria staffPersonByEmailQuery = new QueryByCriteria(SisPerson.class, staffPersonByEmailCriteria);
        m_staffPersonByEmail = m_modelBroker.getMapByQuery(staffPersonByEmailQuery, SisPerson.COL_EMAIL01, 5000);

        // This should be initialized after model broker has been initialized.
        m_calculatedFieldManager = new CalculatedFieldManager(m_modelBroker);
    }

    /**
     * This method will update existing beans or create new ones depending upon whether or not the
     * email is found in the database.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws X2BaseException exception
     */
    private void importStaff(List<String> record, int lineNumber) throws X2BaseException {
        if (StringUtils.isEmpty(record.get(INDEX_LAST_NAME)) ||
                StringUtils.isEmpty(record.get(INDEX_GENDER_CODE)) ||
                StringUtils.isEmpty(record.get(INDEX_EMAIL)) ||
                StringUtils.isEmpty(record.get(INDEX_DISTRICT_NAME))) {
            logInvalidRecord(lineNumber, "error.state.report.missingvalue");
            incrementSkipCount();
        } else {
            String email = record.get(INDEX_EMAIL);

            if (!m_staffPersonByEmail.containsKey(email)) {
                SisAddress address = X2BaseBean.newInstance(SisAddress.class, getBroker().getPersistenceKey());
                SisPerson person = X2BaseBean.newInstance(SisPerson.class, getBroker().getPersistenceKey());
                SisStaff staff = X2BaseBean.newInstance(SisStaff.class, getBroker().getPersistenceKey());

                setAddressFields(address, record);
                person.setPhysicalAddressOid(address.getOid());

                setPersonFields(person, record);
                staff.setPersonOid(person.getOid());

                setStaffFields(staff, record);

                incrementInsertCount();
            } else {
                if (m_personsMatched == null) // first duplicate
                {
                    int capacity = m_staffPersonByEmail.size() / lineNumber > 10
                            ? m_staffPersonByEmail.size() / lineNumber : 10;

                    m_personsMatched = new ArrayList<SisPerson>(capacity);
                    m_recordByPsnOid = new HashMap<String, List<String>>(capacity);
                }

                SisPerson person = m_staffPersonByEmail.get(email);

                // Prep for updateExisting
                m_personsMatched.add(person);
                m_recordByPsnOid.put(person.getOid(), record);
            }
        }
    }

    /**
     * Release resources.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        m_calculatedFieldManager = null;
        m_districtByName.clear();
        m_modelBroker = null;
        if (m_personsMatched != null) {
            m_personsMatched.clear();
        }
        m_schoolByName.clear();
        m_staffPersonByEmail.clear();
        if (m_recordByPsnOid != null) {
            for (List<String> record : m_recordByPsnOid.values()) {
                record.clear();
            }
            m_recordByPsnOid.clear();
        }
    }

    /**
     * Sets the address fields as given by the input.
     *
     * @param address SisAddress
     * @param record List<String>
     */
    private void setAddressFields(SisAddress address, List<String> record) {
        if (address != null) {
            setOrganizationFields(address, record);
            address.setAddressLine01(record.get(INDEX_HOME_ADDRESS_1));
            address.setAddressLine02(record.get(INDEX_HOME_ADDRESS_2));
            address.setAddressLine03(record.get(INDEX_HOME_ADDRESS_3));

            AddressParser.parseLine01(getOrganization(), address.getAddressLine01(), address, true, m_modelBroker);
            AddressParser.parseLine03(getOrganization(), address.getAddressLine03(), address, true, m_modelBroker);

            m_modelBroker.saveBeanForced(address);
        }
    }

    /**
     * Sets the Organization fields as given by the input.
     *
     * @param orgChild OrganizationChild
     * @param record List<String>
     */
    private void setOrganizationFields(OrganizationChild orgChild, List<String> record) {
        SisOrganization district = m_districtByName.get(record.get(INDEX_DISTRICT_NAME));
        SisSchool school = m_schoolByName.get(record.get(INDEX_SCHOOL_NAME));
        if (school != null) {
            OrganizationManager.cloneOrganizationOids(orgChild, school);
        } else if (district != null) {
            OrganizationManager.setOrganizationOids(orgChild, district);
        }
    }

    /**
     * Sets the person fields as given by the input.
     *
     * @param person SisPerson
     * @param record List<String>
     * @throws X2BaseException exception
     */
    private void setPersonFields(SisPerson person, List<String> record) throws X2BaseException {
        if (person != null) {
            setOrganizationFields(person, record);

            person.setFirstName(record.get(INDEX_FIRST_NAME));
            person.setLastName(record.get(INDEX_LAST_NAME));
            person.setGenderCode(record.get(INDEX_GENDER_CODE));

            person.setPhone01(record.get(INDEX_HOME_PHONE));
            person.setPhone02(record.get(INDEX_WORK_PHONE));
            person.setPhone03(record.get(INDEX_CELL_PHONE));
            person.setEmail01(record.get(INDEX_EMAIL));

            person.setStaffIndicator(true);

            m_calculatedFieldManager.refreshCalculatedFields(person);
            m_modelBroker.saveBeanForced(person);
        }
    }

    /**
     * Sets the staff fields as given by the input.
     *
     * @param staff SisStaff
     * @param record List<String>
     * @throws X2BaseException exception
     */
    private void setStaffFields(SisStaff staff, List<String> record) throws X2BaseException {
        if (staff != null) {
            if (StringUtils.isEmpty(staff.getLocalId())) {
                String localId = IdManager.generateStaffLocalId(m_modelBroker, getOrganization());
                staff.setLocalId(localId);
            }

            setOrganizationFields(staff, record);
            staff.setStaffType(record.get(INDEX_STAFF_TYPE));
            staff.setStatus(record.get(INDEX_STATUS));

            String schoolName = record.get(INDEX_SCHOOL_NAME);

            if (!StringUtils.isEmpty(schoolName)) {
                String schoolOid = m_schoolByName.get(schoolName).getOid();
                if (schoolOid != null) {
                    staff.setSchoolOid(schoolOid);
                }
            }

            m_calculatedFieldManager.refreshCalculatedFields(staff);
            m_modelBroker.saveBeanForced(staff);
        }
    }

    /**
     * Updates existing records if they exist.
     *
     * @throws X2BaseException exception
     */
    private void updateExistingRecords() throws X2BaseException {
        if (m_personsMatched != null && !m_personsMatched.isEmpty() &&
                m_recordByPsnOid != null && !m_recordByPsnOid.isEmpty()) {
            // Grab all matching staff
            Criteria staffBeanByPsnOidCriteria = new Criteria();
            staffBeanByPsnOidCriteria.addIn(SisStaff.COL_PERSON_OID, m_recordByPsnOid.keySet());
            QueryByCriteria staffBeanByPsnOidQuery = new QueryByCriteria(SisStaff.class, staffBeanByPsnOidCriteria);
            Map<String, SisStaff> staffBeanByPsnOid = m_modelBroker.getMapByQuery(staffBeanByPsnOidQuery,
                    SisStaff.COL_PERSON_OID, m_recordByPsnOid.size());

            // Grab matching addresses
            Criteria addressByOidCriteria = new Criteria();
            Collection<String> addressOids =
                    CollectionUtils.getPropertyCollection(m_personsMatched, SisPerson.COL_PHYSICAL_ADDRESS_OID);
            addressByOidCriteria.addIn(X2BaseBean.COL_OID, addressOids);
            QueryByCriteria addressByOidQuery = new QueryByCriteria(SisAddress.class, addressByOidCriteria);
            Map<String, SisAddress> addressByOid =
                    m_modelBroker.getMapByQuery(addressByOidQuery, X2BaseBean.COL_OID, m_recordByPsnOid.size());

            for (SisPerson person : m_personsMatched) {
                SisAddress address = addressByOid.get(person.getPhysicalAddressOid());
                SisStaff staff = staffBeanByPsnOid.get(person.getOid());
                List<String> record = m_recordByPsnOid.get(person.getOid());

                setAddressFields(address, record);
                setPersonFields(person, record);
                setStaffFields(staff, record);

                incrementMatchCount();
                incrementUpdateCount();
            }
        }
    }
}
