/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.sis.model.beans.StaffDegree;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang.ArrayUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Import the Staff Roster Data.
 *
 * @author X2 Development Corporation
 */
public class EpimsStaffImport extends TextImportJavaSource {
    /**
     * Name for the "Clear Degrees" parameter. The value is an Boolean.
     */
    public static final String CLEAR_DEGREES_PARAM = "clearDegrees";

    /**
     * Name for the "Clear License" parameter. The value is an Boolean.
     */
    public static final String CLEAR_LICENSE_PARAM = "clearLicense";

    /**
     * Name for the "Run Options" parameter. The value is a Boolean.
     */
    private static final String PARAM_COMMIT = "commit";

    /**
     * Name for the "Value Wrapped with quotes" parameter. The value is a Boolean.
     */
    private static final String PARAM_VALUE_WRAPPER = "valueWrapper";

    /*
     * Input order
     */
    private static final int SR01_MEPID_INDEX = 0;
    private static final int SR02_LOCAL_ID_INDEX = 1;
    private static final int SR03_LICENSE_INDEX = 2;
    private static final int SR04_FIRST_NAME_INDEX = 3;
    private static final int SR05_MIDDLE_NAME_INDEX = 4;
    private static final int SR06_LAST_NAME_INDEX = 5;
    private static final int SR07_DOB_INDEX = 6;
    private static final int SR08_RACE_ETHNICITY = 7;
    private static final int SR11_HIRE_DATE_INDEX = 10;
    private static final int SR18_DEGREE_TYPE_1_INDEX = 17;
    private static final int SR27_EXIT_DATE_INDEX = 26;

    private static final int FIELD_COUNT = 34;

    private static final String ALIAS_FORMAT = "SR%02d";

    private boolean m_commit;
    private DateConverter m_converter;
    private DataDictionary m_dictionary;
    private Map<String, Map<String, String>> m_fieldToRefTable;
    private ModelBroker m_modelBroker;
    protected Map<String, Collection<Race>> m_raceCodeMap;

    /*
     * The DOE uses an arbitrary code for its combinations of races/ethnicities. X2 instead uses
     * a bitmap with the following values:
     *
     * - WHITE: 1
     * - BLACK: 2
     * - ASIAN: 4
     * - AMERICAN INDIAN: 8
     * - PACIFIC ISLANDER: 16
     * - HISPANIC: 32 (this value is actually used as a bitmask by the DOE)
     *
     * The X2 bitmap is translated to a state code using the following array.
     *
     * (DOE values from http://www.doe.mass.edu/infoservices/data/guides/masscodes.html)
     */
    protected static final int[] STATE_RACE_CODES = new int[]
    // X2 Values: { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    // 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }
    {0, 1, 2, 6, 3, 7, 10, 16, 4, 8, 11, 17, 13, 19, 23, 26, 5, 9, 12, 18, 14, 29, 22, 26, 15, 21, 24, 27, 25, 28, 30,
            31};

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return FIELD_COUNT;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        boolean valueWrapper = ((Boolean) getParameter(PARAM_VALUE_WRAPPER)).booleanValue();

        setValueWrappingMode(valueWrapper ? VALUE_WRAPPING_MODE.REQUIRED : VALUE_WRAPPING_MODE.NONE);

        m_converter = (DateConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_modelBroker = new ModelBroker(getPrivilegeSet());

        loadReferenceTables();

        /*
         * Delete all degree and license records
         */
        boolean clearDegrees = ((Boolean) getParameter(CLEAR_DEGREES_PARAM)).booleanValue();
        if (clearDegrees) {
            QueryByCriteria degreeQuery = new QueryByCriteria(StaffDegree.class, null);
            m_modelBroker.deleteByQuery(degreeQuery);
        }

        boolean clearLicense = ((Boolean) getParameter(CLEAR_LICENSE_PARAM)).booleanValue();
        if (clearLicense) {
            QueryByCriteria licenseQuery = new QueryByCriteria(StaffCertification.class, null);
            m_modelBroker.deleteByQuery(licenseQuery);
        }

        Boolean commit = (Boolean) getParameter(PARAM_COMMIT);
        m_commit = commit != null ? commit.booleanValue() : true; // default: commit & review
                                                                  // changes

        Criteria raceCriteria = new Criteria();

        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = m_modelBroker.getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#importRecord(java.util.List, int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        StringBuilder report = new StringBuilder(200);

        String mepid = record.get(SR01_MEPID_INDEX);
        String localid = record.get(SR02_LOCAL_ID_INDEX);
        String firstName = record.get(SR04_FIRST_NAME_INDEX);
        String middleName = record.get(SR05_MIDDLE_NAME_INDEX);
        String lastName = record.get(SR06_LAST_NAME_INDEX);
        String dob = record.get(SR07_DOB_INDEX);

        report.append("[" + mepid + "] " + lastName + ", " + firstName + " [" + middleName + "]\n");
        report.append("DOB: " + dob + "\n");
        report.append("Local ID: " + localid + "\n");

        /*
         * Retrieve staff with state ID
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SisStaff.COL_STATE_ID, mepid);

        QueryByCriteria query = new QueryByCriteria(SisStaff.class, criteria);
        Collection<SisStaff> staffMembers = m_modelBroker.getCollectionByQuery(query);

        if (staffMembers.size() == 1) {
            SisStaff staffMember = staffMembers.iterator().next();

            report.append(getStaffReport(record, staffMember));

            incrementMatchCount();
            incrementUpdateCount();

            setPersonFields(staffMember.getPerson(), record, report);
            setStaffCertification(staffMember, record, report);
            setStaffFields(staffMember, record, report);
            setStaffDegrees(staffMember, record, report);

            if (!m_commit) {
                logInvalidRecord(lineNumber, report.toString());
            }
        } else {
            incrementSkipCount();

            if (staffMembers.size() > 1) {
                report.append(" - MULTIPLE (" + staffMembers.size() + ") MATCHES FOR MEPID!\n");
            } else {
                report.append(" - NO MATCHES FOR MEPID!\n");
            }

            logInvalidRecord(lineNumber, report.toString());
        }
    }

    /**
     * Create race records from the state code. We use the Aspen bit map.
     *
     * @param person <code>SiSPerson</code> record in need of race records.
     * @param stateCode Code to translate into race records.
     * @param report Log builder.
     *
     * @return True on success
     */
    private boolean createRaceRecords(SisPerson person, String stateCode, StringBuilder report) {
        boolean success = false;
        int stateRaceCode = Integer.valueOf(stateCode).intValue();
        Map<String, String> stateToBaseCodes = m_fieldToRefTable.get(Race.COL_RACE_CODE);

        person.setHispanicLatinoIndicator(false);
        if (STATE_RACE_CODES.length < stateRaceCode) {
            stateRaceCode -= 32;
            person.setHispanicLatinoIndicator(true);
            report.append("Race contained Hispanic/Latino Indicator\n");
        }

        int index = ArrayUtils.indexOf(STATE_RACE_CODES, stateRaceCode);
        if (index > 0) {
            for (String raceCode : stateToBaseCodes.keySet()) {
                int value = Integer.parseInt(raceCode);
                int match = value & index;
                if (match > 0) {
                    Race race = X2BaseBean.newInstance(Race.class, m_modelBroker.getPersistenceKey());
                    String aspenCode = stateToBaseCodes.get(raceCode);

                    race.setPersonOid(person.getOid());
                    race.setRaceCode(aspenCode);
                    report.append("Add race record: " + aspenCode + " (" + raceCode + ")\n");

                    if (m_commit) {
                        m_modelBroker.saveBeanForced(race);
                    }
                }
            }
        }

        return success;
    }

    /**
     * Translate value to state code.
     *
     * @param column Column name or alias used as key in the map.
     * @param value Value to search for.
     *
     * @return The code for the value passed.
     */
    private String getStaffDegreeColumn(String column, String value) {
        String code = value;
        if (m_fieldToRefTable.containsKey(column)) {
            Map<String, String> stateToBaseCodes = m_fieldToRefTable.get(column);
            if (stateToBaseCodes.containsKey(value)) {
                code = stateToBaseCodes.get(value);
            }
        }
        return code;
    }

    /**
     * Returns a little report of the current record in the database.
     *
     * @param record Data record.
     * @param staff SisStaff found.
     * @return A report about the staff in the database
     */
    private String getStaffReport(List<String> record, SisStaff staff) {
        StringBuilder report = new StringBuilder(100);
        SisPerson person = staff.getPerson();
        if (person != null) {
            String firstName = record.get(SR04_FIRST_NAME_INDEX).trim();
            String lastName = record.get(SR06_LAST_NAME_INDEX).trim();

            if (!firstName.equals(person.getFirstName()) ||
                    !lastName.equals(person.getLastName())) {
                report.append("NAME MISMATCH!\n");
            }

            report.append("[" + staff.getStateId() + "] " + person.getLastName() + ", "
                    + person.getFirstName() + " [" + person.getMiddleName() + "]\n");
            PlainDate date = person.getDob();
            if (date != null) {
                String dob = date.toString();
                report.append("DOB: " + dob + "\n");
            }
            report.append("Local ID: " + staff.getLocalId() + "\n");
        } else {
            report.append("NO PERSON!");
        }

        return report.toString();
    }

    /**
     * Returns a map of state reference codes to their base reference code equivalents for the
     * reference table used by the given student property (represented by an alias). If the student
     * property doesn't use a reference table then an empty map is returned.
     *
     * @param field DataDictionaryField
     * @return A Map of String keys to String values
     */
    private Map<String, String> getStateToBaseMap(DataDictionaryField field) {
        HashMap<String, String> stateToBaseCodes = null;

        if (field.hasReferenceTable()) {
            Collection codes = field.getReferenceTable().getReferenceCodes();
            stateToBaseCodes = new HashMap<String, String>((int) (codes.size() * 1.5));
            Iterator codeIterator = codes.iterator();
            while (codeIterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) codeIterator.next();
                if (!StringUtils.isEmpty(code.getStateCode())) {
                    stateToBaseCodes.put(code.getStateCode(), code.getCode());
                }
            }
        } else {
            stateToBaseCodes = new HashMap<String, String>();
        }

        return stateToBaseCodes;
    }

    /**
     * Loads the reference tables for default values and state-to-base code translation.
     */
    private void loadReferenceTables() {
        m_fieldToRefTable = new HashMap<String, Map<String, String>>(8);

        for (int i = 8; i < FIELD_COUNT; i++) {
            int human = i + 1;
            String alias = String.format(ALIAS_FORMAT, Integer.valueOf(human));

            DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);

            if (field != null && field.hasReferenceTable()) {
                m_fieldToRefTable.put(alias, getStateToBaseMap(field));
            }
        }

        /*
         * Load the reference tables by fields
         */
        HashMap<String, String> fieldClassMap = new HashMap<String, String>(7);

        fieldClassMap.put(Race.COL_RACE_CODE, Race.class.getName());
        fieldClassMap.put(StaffDegree.COL_TYPE, StaffDegree.class.getName());
        fieldClassMap.put(StaffDegree.COL_INSTITUTION, StaffDegree.class.getName());
        fieldClassMap.put(StaffDegree.COL_DEGREE, StaffDegree.class.getName());

        for (String column : fieldClassMap.keySet()) {
            String className = fieldClassMap.get(column);
            DataDictionaryField field = m_dictionary.findDataDictionaryField(className, column);
            if (field != null && field.hasReferenceTable()) {
                m_fieldToRefTable.put(column, getStateToBaseMap(field));
            }
        }
    }

    /**
     * Set the value on the bean by alias.
     *
     * @param alias String
     * @param value String
     * @param bean X2BaseBean
     * @return String
     */
    private String setAliasField(String alias, String value, X2BaseBean bean) {
        String savedValue = value;
        if (m_fieldToRefTable.containsKey(alias)) {
            Map<String, String> stateToBaseCodes = m_fieldToRefTable.get(alias);
            if (stateToBaseCodes.containsKey(value)) {
                String stateCode = stateToBaseCodes.get(value);
                savedValue = stateCode;
                bean.setFieldValueByAlias(alias, stateCode);
            } else {
                savedValue = "";
                bean.setFieldValueByAlias(alias, "");
            }
        } else {
            bean.setFieldValueByAlias(alias, value);
        }

        return savedValue;
    }

    /**
     * Sets the person fields contained in the input.
     *
     * @param person SisPerson attached to staff found.
     * @param record Data record.
     * @param report Log builder.
     */
    private void setPersonFields(SisPerson person, List<String> record, StringBuilder report) {
        if (person != null) {
            String firstName = record.get(SR04_FIRST_NAME_INDEX);
            String middleName = record.get(SR05_MIDDLE_NAME_INDEX);
            String lastName = record.get(SR06_LAST_NAME_INDEX);
            String stateRaceCode = record.get(SR08_RACE_ETHNICITY);

            person.setFirstName(firstName.trim());
            person.setMiddleName(middleName.trim());
            person.setLastName(lastName.trim());

            person.setDob(m_converter.stringToJava(record.get(SR07_DOB_INDEX)));

            Collection<Race> raceBeans = m_raceCodeMap.get(person.getOid());
            if (raceBeans == null) {
                createRaceRecords(person, stateRaceCode, report);
            }

            if (m_commit) {
                m_modelBroker.saveBeanForced(person);
            }
        }
    }

    /**
     * Creates primary staff certification.
     *
     * @param staff SisStaff found.
     * @param record Data record.
     * @param report Log builder.
     */
    private void setStaffCertification(SisStaff staff, List<String> record, StringBuilder report) {
        Collection<StaffCertification> staffCertifications = staff.getCertifications(getBroker());
        if (staffCertifications.isEmpty()) {
            StaffCertification staffCertification =
                    X2BaseBean.newInstance(StaffCertification.class, getBroker().getPersistenceKey());

            staffCertification.setCertificationNumber(record.get(SR03_LICENSE_INDEX));
            staffCertification.setPrimaryIndicator(true);
            staffCertification.setStaffOid(staff.getOid());

            if (m_commit) {
                m_modelBroker.saveBeanForced(staffCertification);
            }
            report.append("Adding Staff Certification Number: " + record.get(SR03_LICENSE_INDEX) + "\n");
        }
    }

    /**
     * Create the staff degrees records, up to three.
     *
     * @param staff SisStaff found.
     * @param record Data record.
     * @param report Log builder.
     */
    private void setStaffDegrees(SisStaff staff, List<String> record, StringBuilder report) {
        Collection<StaffDegree> staffDegrees = staff.getDegrees(getBroker());
        if (staffDegrees.isEmpty()) {
            int i = SR18_DEGREE_TYPE_1_INDEX;
            while (i < SR27_EXIT_DATE_INDEX) {
                if (!record.get(i).equals("000")) {
                    String type = getStaffDegreeColumn(StaffDegree.COL_TYPE, record.get(i));
                    String inst = getStaffDegreeColumn(StaffDegree.COL_INSTITUTION, record.get(i + 1));
                    String subj = getStaffDegreeColumn(StaffDegree.COL_DEGREE, record.get(i + 2));

                    StaffDegree degree = X2BaseBean.newInstance(StaffDegree.class, getBroker().getPersistenceKey());

                    degree.setStaffOid(staff.getOid());
                    degree.setDegree(subj);
                    degree.setType(type);
                    degree.setInstitution(inst);

                    if (m_commit) {
                        m_modelBroker.saveBeanForced(degree);
                    }

                    String alias = String.format(ALIAS_FORMAT, Integer.valueOf(i + 1));
                    report.append(alias + ": " + type + "\n");

                    alias = String.format(ALIAS_FORMAT, Integer.valueOf(i + 2));
                    report.append(alias + ": " + inst + "\n");

                    alias = String.format(ALIAS_FORMAT, Integer.valueOf(i + 3));
                    report.append(alias + ": " + subj + "\n");
                }
                i = i + 3;
            }
        }
    }

    /**
     * Sets the staff fields contained in the input (SR08-SR17).
     *
     * @param staff SisStaff found.
     * @param record Data record.
     * @param report Log builder.
     */
    private void setStaffFields(SisStaff staff, List<String> record, StringBuilder report) {
        staff.setLocalId(record.get(SR02_LOCAL_ID_INDEX));

        for (int i = 7; i < FIELD_COUNT; i++) {
            String savedValue = null;
            if (SR18_DEGREE_TYPE_1_INDEX <= i && i < SR27_EXIT_DATE_INDEX) {
                // Degree fields
                continue;
            }

            int human = i + 1;
            String alias = String.format(ALIAS_FORMAT, Integer.valueOf(human));
            String value = record.get(i);
            switch (i) {
                case SR11_HIRE_DATE_INDEX:
                    PlainDate hireDate = m_converter.stringToJava(value);
                    if (hireDate != null) {
                        savedValue = hireDate.toString();
                        staff.setHireDate(hireDate);
                    }
                    break;

                case SR27_EXIT_DATE_INDEX:
                    PlainDate exitDate = m_converter.stringToJava(value);
                    if (exitDate != null) {
                        value = exitDate.toString();
                        savedValue = setAliasField(alias, value, staff);
                    }
                    break;

                default:
                    savedValue = setAliasField(alias, value, staff);
                    break;
            }

            report.append(alias + ": " + value + " [" + savedValue + "]\n");
        }

        if (m_commit) {
            m_modelBroker.saveBeanForced(staff);
        }
    }
}
