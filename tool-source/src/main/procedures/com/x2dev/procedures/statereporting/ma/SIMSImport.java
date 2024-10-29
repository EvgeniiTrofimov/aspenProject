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

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Import SIMS data.
 *
 * @author X2 Development Corporation
 */
public class SIMSImport extends TextImportJavaSource {
    /*
     * Input order
     */
    private static final int INDEX_LASID_KEY = 0;
    private static final int INDEX_SASID_KEY = 1;
    private static final int INDEX_FIRST_NAME_KEY = 2;
    private static final int INDEX_MIDDLE_NAME_KEY = 3;
    private static final int INDEX_LAST_NAME_KEY = 4;

    private static final int FIELD_COUNT = 52;

    private static final int STARTING_ALIAS_INDEX = 9;
    private static final String ALIAS_FORMAT = "DOE %02d";

    private DataDictionary m_dictionary;
    private Map<String, Map<String, String>> m_fieldToRefTable;
    private ModelBroker m_modelBroker;
    private Map<String, X2BaseBean> m_studentByLasid;
    private Map<String, X2BaseBean> m_studentBySasid;

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
        m_modelBroker = new ModelBroker(getPrivilegeSet());
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        /*
         * Retrieve students
         */
        Criteria studentCriteria = new Criteria();
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

        m_studentByLasid = m_modelBroker.getMapByQuery(studentQuery, SisStudent.COL_LOCAL_ID, 1000);
        m_studentBySasid = m_modelBroker.getMapByQuery(studentQuery, SisStudent.COL_STATE_ID, 1000);

        loadReferenceTables();
        setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
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
        StringBuffer report = new StringBuffer();

        String sasid = record.get(INDEX_SASID_KEY);
        String lasid = record.get(INDEX_LASID_KEY);
        SisStudent student = (SisStudent) m_studentBySasid.get(sasid);

        if (student == null) {
            report.append("State ID " + sasid + " not found");
            student = (SisStudent) m_studentByLasid.get(lasid);
            if (student == null) {
                report.append("Local ID " + lasid + " not found");
            } else {
                report.append("Found by local ID " + lasid + " state ID was " + student.getStateId());
            }
        } else {
            report.append("Found by state ID " + lasid + " => " + student.getLocalId() + "\n");

        }
        if (student != null) {
            SisPerson person = student.getPerson();
            report.append(person.getFirstName() + " " + person.getMiddleName() + " " + person.getLastName() + "\n");
            report.append("DOB: " + person.getDob() + "\n");

            if (person.getFirstName().equals(record.get(INDEX_FIRST_NAME_KEY)) &&
                    person.getMiddleName().equals(record.get(INDEX_MIDDLE_NAME_KEY)) &&
                    person.getLastName().equals(record.get(INDEX_LAST_NAME_KEY))) {
                SisSchool school = (SisSchool) getSchool();
                if (school.equals(student.getSchool())) {
                    report.append(updateStudent(record, student));
                    incrementMatchCount();
                    incrementUpdateCount();
                } else {
                    report.append("SisStudent not a member of " + school.getName() + "\n");
                    report.append("SisStudent a member of " + student.getSchool().getName() + "\n");
                    incrementSkipCount();
                }
            } else {
                for (int i = 0; i < FIELD_COUNT; i++) {
                    report.append(reportOnStudent(record, student, i));
                }
                incrementSkipCount();
            }
        }
        logInvalidRecord(lineNumber, report.toString());
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

        for (int i = STARTING_ALIAS_INDEX; i < FIELD_COUNT; i++) {
            int human = i + 1;
            String alias = String.format(ALIAS_FORMAT, Integer.valueOf(human));

            DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);

            if (field != null && field.hasReferenceTable()) {
                m_fieldToRefTable.put(alias, getStateToBaseMap(field));
            }
        }
    }

    /**
     * Returns a little report of the current column record in the database.
     *
     * @param record Data record.
     * @param student SisStudent found.
     * @param i Record index.
     *
     * @return A report about the student in the database
     */
    private String reportOnStudent(List<String> record, SisStudent student, int i) {
        StringBuilder report = new StringBuilder(1000);

        int human = i + 1;
        String alias = String.format(ALIAS_FORMAT, Integer.valueOf(human));
        String original = (String) student.getFieldValueByAlias(alias);
        String value = record.get(i);

        String need = " NEEDS";
        if (original != null) {
            if (original.equals(value)) {
                need = " does not need";
            }
        } else {
            report.append(alias + " must be setup.\n");
        }
        String log = alias + " was " + original + need + " changing " + value + "\n";
        report.append(log);
        if (m_fieldToRefTable.containsKey(alias)) {
            Map<String, String> stateToBaseCodes = m_fieldToRefTable.get(alias);
            if (stateToBaseCodes.containsKey(value)) {
                String stateCode = stateToBaseCodes.get(value);
                report.append(alias + " has ref table " + value + " maps to " + stateCode + "\n");
            } else {
                report.append(alias + " has ref table " + value + " WAS NOT FOUND!\n");
            }
            report.append("");
        }

        return report.toString();
    }

    /**
     * Update the student bean and save with data record.
     *
     * @param record Data record.
     * @param student SisStudent found.
     *
     * @return A report of what happened.
     */
    private String updateStudent(List<String> record, SisStudent student) {
        StringBuilder report = new StringBuilder(100);
        for (int i = STARTING_ALIAS_INDEX; i < FIELD_COUNT; i++) {
            int human = i + 1;
            String alias = String.format(ALIAS_FORMAT, Integer.valueOf(human));
            String value = record.get(i);

            if (m_fieldToRefTable.containsKey(alias)) {
                Map<String, String> stateToBaseCodes = m_fieldToRefTable.get(alias);
                if (stateToBaseCodes.containsKey(value)) {
                    String stateCode = stateToBaseCodes.get(value);
                    student.setFieldValueByAlias(alias, stateCode);
                } else {
                    report.append(alias + " has ref table " + value + " WAS NOT FOUND!\n");
                    student.setFieldValueByAlias(alias, "");
                }
            } else {
                if (human == 15) // Special DOE 15 is stored in the school
                {
                    SisSchool school = student.getSchool();
                    String stateCode = (String) school.getFieldValueByAlias(alias);
                    if (stateCode == null || stateCode.equals("")) {
                        school.setFieldValueByAlias(alias, value);
                        student.setFieldValueByAlias("DOE Adjusted School", "");
                    } else {
                        if (!stateCode.equals(value)) {
                            report.append("SisSchool state code: " + stateCode + " adjusted to " + value + "\n");
                            student.setFieldValueByAlias("DOE Adjusted School", value);
                        }
                    }
                } else {
                    student.setFieldValueByAlias(alias, value);
                }
            }
        }
        m_modelBroker.saveBeanForced(student);
        return report.toString();
    }
}
