/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/* DEBUG */

/**
 * Procedure to import the EIS State ID for Tennessee.
 *
 * @author Follett Software
 */
public class EISStateImport extends TextImportJavaSource {
    @SuppressWarnings("unused")
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    // General Parameters
    public static final String PARAM_SKIP_ROWS = "skipRows";

    // Database field name parameter where the data is to be imported or matched upon
    private static final String PARAM_STUDENT_EIS_STATE_ID = "studentEisStateId";
    private static final String PARAM_STUDENT_PIN = "studentPin";

    // Fields in the input file
    private static final String PARAM_ONLY_FOR_REVIEW = "onlyForReview";

    private int m_fieldCount;
    private int m_indexSsn;
    private int m_indexStdFirstName;
    private int m_indexStdMidName;
    private int m_indexStdLastName;
    private int m_indexStudentKey;
    private int m_indexStudentPin;
    @SuppressWarnings("unused")
    private int m_indexStudentStateId;
    private Integer m_skipRows;
    private boolean m_onlyForReview;

    private Map<String, SisStudent> m_studentsByPin;
    private Map<String, SisStudent> m_studentsBySsn;

    private ModelBroker m_broker;

    private String m_aliasStudentEisStateId;
    private String m_aliasStudentPin;
    private String m_beanPathStudentStateId;

    private static final String DISABLE_BEAN_PATH = "-9999";

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.x2dev.sis.tools.ToolJavaSource#getBroker()
     */
    @Override
    public X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(getPrivilegeSet());
        }
        return m_broker;
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return m_fieldCount;
    }

    /**
     * Return StringBuilder which contain the text that would show to user when the import procedure
     * finished.
     *
     * @return StringBuilder
     */
    @Override
    protected StringBuilder getImportStatistics() {
        StringBuilder messageToReturn = new StringBuilder(312);
        String mode = m_onlyForReview ? "Review Mode." : "Commit Mode.";
        messageToReturn.append(mode);
        messageToReturn.append('\n');
        messageToReturn.append('\n');
        StringBuilder bufferMessage = super.getImportStatistics();
        messageToReturn.append(bufferMessage);
        return messageToReturn;
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
        if (lineNumber > getSkipRows()) {
            String ssn = record.get(m_indexSsn).replaceAll("[^0-9]", "");
            String eisStateId = stripNonNumericChars(record.get(m_indexStudentKey));
            String studentPin = stripNonNumericChars(record.get(m_indexStudentPin));

            // TODO: We were instructed to temporarily disable EdFi id update. To reverse, switch
            // the following lines:
            // String stdStateId = record.get(m_indexStudentStateId);
            String stdStateId = "";

            String fName = record.get(m_indexStdFirstName);
            String mName = record.get(m_indexStdMidName);
            String lName = record.get(m_indexStdLastName);

            if (!StringUtils.isEmpty(ssn) && !StringUtils.isEmpty(studentPin)) {
                logInvalidRecord(lineNumber, "Only PIN or SSN can exist (Check imported file). First Name: "
                        + fName
                        + ", Middle Name: "
                        + mName
                        + ", Last Name: "
                        + lName);
                incrementSkipCount();
            } else {
                SisStudent student = null;
                student = getStudent(ssn, studentPin);

                if (student != null) {
                    boolean invalidUpdate = false;
                    String pin = (String) student.getFieldValueByAlias(m_aliasStudentPin);

                    if (!StringUtils.isEmpty(student.getPerson().getPersonId()) && !StringUtils.isEmpty(pin)) {
                        logInvalidRecord(lineNumber, "Only PIN or SSN can exist (Check Student Table). First Name: "
                                + student.getPerson().getFirstName()
                                + ", Middle Name: "
                                + student.getPerson().getMiddleName()
                                + ", Last Name: "
                                + student.getPerson().getLastName());

                        incrementSkipCount();
                    } else {
                        if (!StringUtils.isEmpty(eisStateId)) {
                            String currentEisStateId = (String) student.getFieldValueByAlias(m_aliasStudentEisStateId);
                            if (!StringUtils.isEmpty(currentEisStateId) && !eisStateId.equals(currentEisStateId)) {
                                invalidUpdate = true;
                                logInvalidRecord(lineNumber, "EIS ID Mismatch. New EIS ID: "
                                        + eisStateId
                                        + ", Current EIS ID: "
                                        + currentEisStateId
                                        + ", First Name: "
                                        + fName
                                        + ", Middle Name: "
                                        + mName
                                        + ", Last Name: "
                                        + lName);
                            } else {
                                setFieldValue(student, m_aliasStudentEisStateId, eisStateId);
                            }
                        }
                        if (!StringUtils.isEmpty(stdStateId)) {
                            if (!StringUtils.isEmpty(student.getStateId())
                                    && !stdStateId.equals(student.getStateId())) {
                                invalidUpdate = true;
                                logInvalidRecord(lineNumber, "EdFi ID Mismatch. New EdFi ID: "
                                        + stdStateId
                                        + ", Current EdFi ID: "
                                        + student.getStateId()
                                        + ", First Name: "
                                        + fName
                                        + ", Middle Name: "
                                        + mName
                                        + ", Last Name: "
                                        + lName);
                            } else {
                                student.setFieldValueByBeanPath(m_beanPathStudentStateId, stdStateId);
                            }
                        }

                        if (invalidUpdate) {
                            this.incrementSkipCount();
                        } else {
                            saveRecord(student, eisStateId, lineNumber);
                        }
                    }
                } else {
                    logInvalidRecord(lineNumber, "No student found for EIS State ID: "
                            + eisStateId
                            + ", First Name: "
                            + fName
                            + ", Middle Name: "
                            + mName
                            + ", Last Name: "
                            + lName);

                    incrementSkipCount();
                }
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        List<String> record = readInputHeading();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty mpStateId = new ModelProperty(SisStudent.class, SisStudent.COL_STATE_ID, dictionary);

        m_beanPathStudentStateId = mpStateId.getBeanPath();
        // Gets the input parameters
        m_aliasStudentEisStateId = (String) getParameter(PARAM_STUDENT_EIS_STATE_ID);
        m_aliasStudentPin = (String) getParameter(PARAM_STUDENT_PIN);

        m_indexStudentKey = getRecordIndex(record, "STUDENT_KEY");
        m_indexSsn = getRecordIndex(record, "STUDENT_SSN");
        m_indexStudentPin = getRecordIndex(record, "STUDENT_PIN");
        m_indexStdFirstName = getRecordIndex(record, "FIRST_NAME");
        m_indexStdMidName = getRecordIndex(record, "MIDDLE_NAME");
        m_indexStdLastName = getRecordIndex(record, "LAST_NAME");

        /*
         * "Do we check only for review?"
         * Default: Yes (true)
         */
        Boolean onlyForReview = (Boolean) getParameter(PARAM_ONLY_FOR_REVIEW);
        m_onlyForReview = onlyForReview != null ? onlyForReview.booleanValue() : true; // default:
                                                                                       // add
                                                                                       // reference
                                                                                       // codes

        m_studentsByPin = new HashMap<String, SisStudent>();
        m_studentsBySsn = new HashMap<String, SisStudent>();

        loadStudentsByPinAndSsn();

        setUseValueDelimiters(true);
        this.setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#logInvalidRecord(int,
     *      java.lang.String)
     */
    @Override
    protected void logInvalidRecord(int lineNumber, String key) {
        if (lineNumber > getSkipRows()) {
            super.logInvalidRecord(lineNumber, key);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#logInvalidRecord(int,
     *      java.lang.String, java.lang.Object)
     */
    @Override
    protected void logInvalidRecord(int lineNumber, String key, Object parameter) {
        if (lineNumber > getSkipRows()) {
            super.logInvalidRecord(lineNumber, key, parameter);
        }
    }

    /**
     * @param record
     * @param string
     * @return
     */
    private int getRecordIndex(List<String> record, String key) {
        int value = record.indexOf(key);
        if (value < 0) {
            throw new IllegalStateException("The heading list " + record + " does not contain " + key + ".");
        }
        return value;
    }

    private int getSkipRows() {
        if (m_skipRows == null) {
            m_skipRows = ((Integer) getParameter(PARAM_SKIP_ROWS));
            if (m_skipRows == null) {
                m_skipRows = Integer.valueOf(1); // Headings in first field
            }
        }
        return m_skipRows.intValue();
    }

    /**
     * Returns the student matching first on SSN then student Pin number.
     *
     * @param ssn String
     * @param studentPin String
     * @return student
     */
    private SisStudent getStudent(String ssn, String studentPin) {
        SisStudent student = m_studentsBySsn.get(ssn);

        if (student == null) {
            student = m_studentsByPin.get(studentPin);
        }

        return student;
    }

    /**
     * Loads a map of students keyed on Student Pin numbers.
     */
    private void loadStudentsByPinAndSsn() {
        BeanQuery query = new BeanQuery(SisStudent.class);
        QueryIterator iterator = m_broker.getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();

                String pin = stripNonNumericChars((String) student.getFieldValueByAlias(m_aliasStudentPin));
                if (!StringUtils.isEmpty(pin)) {
                    m_studentsByPin.put(pin, student);
                }

                if (student.getPerson() != null) {
                    String ssn = stripNonNumericChars(student.getPerson().getPersonId());
                    if (!StringUtils.isEmpty(ssn)) {
                        m_studentsBySsn.put(ssn, student);
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Read a list of column headings from the input file.
     *
     * @return List
     */
    private List<String> readInputHeading() {
        // Set temporary field count - needed to use splitLine()
        m_fieldCount = 100;

        File sourceFile = (File) getParameter(FILE_KEY);
        List<String> record = new ArrayList();
        setValueWrappingMode(VALUE_WRAPPING_MODE.OPTIONAL);
        setUseValueDelimiters(true);
        setValueWrapper('"');

        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(sourceFile), 4096);
            for (int skip = 0; skip < getSkipRows() - 1; ++skip) {
                reader.readLine(); // skip all but header
            }
            String line = reader.readLine();
            record = splitLine(line, 1);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    // Do nothing
                }
            }
        }
        if (record != null) {
            m_fieldCount = record.size();
        }
        return record;
    }

    /**
     * Save record.
     *
     * @param student SisStudent
     */
    private void saveRecord(SisStudent student, String eisStateId, int lineNumber) {
        if (student.isDirty()) {
            if (!m_onlyForReview) {
                m_broker.saveBeanForced(student);
            }
            logInvalidRecord(lineNumber, "Student with EIS State ID: "
                    + eisStateId
                    + " updated.");

            incrementUpdateCount();
        } else {
            incrementMatchCount();
        }
    }

    /**
     * Gets the selected field from the passed bean.
     *
     * @param bean X2BaseBean
     * @param path String
     * @param value String
     */
    private void setFieldValue(X2BaseBean bean, String path, String value) {
        if (bean != null && path != null && !path.equals(DISABLE_BEAN_PATH)) {
            bean.setFieldValueByAlias(path, value);
        }
    }

    /**
     * Returns numeric values without invalid characters.
     *
     * @param value String
     * @return String
     */
    private String stripNonNumericChars(String value) {
        String result = null;
        if (!StringUtils.isEmpty(value)) {
            result = value.replaceAll("[^0-9]", "");
        }
        return result;
    }
}
