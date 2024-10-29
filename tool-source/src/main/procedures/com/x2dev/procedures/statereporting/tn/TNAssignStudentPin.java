/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class TNAssignStudentPin.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class TNAssignStudentPin extends ProcedureJavaSource {

    /**
     * The Class PINGenerator.
     */
    static class PINGenerator {
        private Iterator<Long> m_iterator;
        private long m_minValue;
        private long m_maxValue;
        private long m_nextUsed = 0;
        private long m_nextValue;
        private Set<Long> m_usedValues = new TreeSet();

        /**
         * Instantiates a new PIN generator.
         *
         * @param minValue long
         * @param maxValue long
         */
        public PINGenerator(long minValue, long maxValue) {
            super();
            m_minValue = minValue;
            m_maxValue = maxValue;
            m_nextValue = m_minValue;
        }

        /**
         * Adds the used values.
         *
         * @param broker X2Broker
         * @param beanClass Class
         * @param criteria X2Criteria
         * @param column String
         */
        public void addUsedValues(X2Broker broker, Class beanClass, X2Criteria criteria, String column) {
            String[] columns = new String[] {column};
            ColumnQuery query = new ColumnQuery(beanClass, columns, criteria);
            try (QueryIterator iterator = broker.getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    addUsedValue((String) row[0]);
                }
            }
        }

        /**
         * Gets the next value.
         *
         * @return String
         */
        public String getNextValue() {
            long next;
            while (m_nextValue >= m_nextUsed) {
                if (m_nextValue == m_nextUsed) {
                    ++m_nextValue;
                    if (m_nextValue > m_maxValue) {
                        throw new TNAssignStudentPinException(
                                "The values for the PIN generator are exhausted.  Range is from " + m_minValue + " to "
                                        + m_maxValue + ".");
                    }
                }
                m_nextUsed = getNextUsed();
            }
            next = m_nextValue++;
            return Long.toString(next);
        }

        /**
         * Adds the used value.
         *
         * @param numberString String
         */
        private void addUsedValue(String numberString) {
            try {
                Long usedValue = Long.parseLong(numberString);
                if (usedValue >= m_minValue && usedValue <= m_maxValue) {
                    addUsedValue(usedValue);
                }
            } catch (NumberFormatException e) {
                // do nothing - skip ill-formed strings
            }
        }

        /**
         * Adds the used value.
         *
         * @param value Long
         */
        private void addUsedValue(Long value) {
            m_usedValues.add(value);
        }

        /**
         * Gets the next used.
         *
         * @return long
         */
        private long getNextUsed() {
            if (m_iterator == null) {
                m_iterator = m_usedValues.iterator();
            }
            if (m_iterator.hasNext()) {
                return m_iterator.next().longValue();
            }
            return m_maxValue + 1;
        }

    }

    /**
     * The Class TNAssignStudentPinException.
     */
    public static class TNAssignStudentPinException extends RuntimeException {

        /**
         * Instantiates a new TN assign student pin exception.
         *
         * @param message String
         */
        public TNAssignStudentPinException(String message) {
            super(message);
        }
    }

    private static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    private static final String ALIAS_PSN_ARCHIVE_SSN = "all-psn-archiveSSN";
    private static final String ALIAS_STD_PREVIOUS_PIN = "DOE PREVIOUS PIN";
    private static final String ALIAS_STD_STUDENT_PIN = "DOE PIN";

    private static final String INPUT_PARAM_MIN_NUMBER = "minNumber";
    private static final String INPUT_PARAM_MAX_NUMBER = "maxNumber";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_SKIP_PIN_REF_TABLE_NAME = "skipPinRefTableName";
    private static final String INPUT_PARAM_UPDATE_RECORDS = "updateRecords";

    private ModelBroker m_broker;
    private String m_fieldExcludeStd;
    private String m_fieldPreviousPIN;
    private String m_fieldStudentPIN;
    private PINGenerator m_generator;
    private List<String> m_infoMessages = new LinkedList();

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    public X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(getPrivilegeSet());
        }
        return m_broker;
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        try {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            m_fieldExcludeStd = translateAlias(dictionary, ALIAS_EXCLUDE_STUDENT, true);
            m_fieldPreviousPIN = translateAlias(dictionary, ALIAS_STD_PREVIOUS_PIN, true);
            m_fieldStudentPIN = translateAlias(dictionary, ALIAS_STD_STUDENT_PIN, true);
            String fieldArchiveSSN = translateAlias(dictionary, ALIAS_PSN_ARCHIVE_SSN, false);
            if (fieldArchiveSSN == null) {
                logMessage("SSN will not be archived.  If you would like to archive create alias "
                        + ALIAS_PSN_ARCHIVE_SSN);
            }

            long minValue = getNumber(INPUT_PARAM_MIN_NUMBER, "00001");
            long maxValue = getNumber(INPUT_PARAM_MAX_NUMBER, "99999");
            logMessage("Range for PINs is " + minValue + " to " + maxValue);

            boolean updateStudents = false;
            if (getParameter(INPUT_PARAM_UPDATE_RECORDS) != null
                    && getParameter(INPUT_PARAM_UPDATE_RECORDS) instanceof Boolean) {
                updateStudents = ((Boolean) getParameter(INPUT_PARAM_UPDATE_RECORDS)).booleanValue();
            }
            logMessage(updateStudents ? "PINs will be committed to student" : "Test Mode - PINs are not saved");


            m_generator = new PINGenerator(minValue, maxValue);
            m_generator.addUsedValues(getBroker(), SisStudent.class, getStudentPinCriteria(), m_fieldStudentPIN);
            m_generator.addUsedValues(getBroker(), SisStudent.class, getPreviousPinCriteria(), m_fieldPreviousPIN);
            m_generator.addUsedValues(getBroker(), ReferenceCode.class, getUsedPinsCriteria(), ReferenceCode.COL_CODE);


            int totalStudents = 0;
            BeanQuery query = new BeanQuery(SisStudent.class, getStudentCriteria());
            query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
            try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    SisStudent bean = (SisStudent) iterator.next();
                    String pin = m_generator.getNextValue();

                    List<ValidationError> errors = new ArrayList();
                    if (updateStudents) {
                        String ssn = bean.getPerson().getPersonId();
                        if (!StringUtils.isEmpty(ssn)) {
                            bean.getPerson().setPersonId("");
                            if (fieldArchiveSSN != null) {
                                bean.getPerson().setFieldValueByBeanPath(fieldArchiveSSN, ssn);
                            }
                            errors.addAll(getBroker().saveBean(bean.getPerson()));
                        }
                        bean.setFieldValueByBeanPath(m_fieldStudentPIN, pin);
                        errors.addAll(getBroker().saveBean(bean));
                    }
                    infoMessage("Assigning PIN " + pin + " to " + bean.getNameView()
                            + (errors.isEmpty() ? "" : " has FAILED"));
                    errors.stream().forEach(err -> infoMessage("Validation Error: " + err.toString()));
                    ++totalStudents;
                }
            }
            logMessage("Total students assigned PINs is " + totalStudents + "\n");
            outputInfoMessages();
        } catch (TNAssignStudentPinException e) {
            logMessage("Processing terminated with exception");
            logMessage("Exception message: " + e.getMessage());
        }
    }

    /**
     * Check reference table exists.
     *
     * @param refTableName String
     */
    private void checkReferenceTableExists(String refTableName) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTableName);
        ReferenceTable refTable = getBroker().getBeanByQuery(new BeanQuery(ReferenceTable.class, criteria));
        if (refTable == null) {
            throw new TNAssignStudentPinException(
                    "The skip pin reference table named " + refTableName + " must exist.");
        }
    }

    /**
     * Gets the number.
     *
     * @param param String
     * @param suffix String
     * @return long
     */
    private long getNumber(String param, String suffix) {
        String numberString = (String) getParameter(param);
        Long value = null;
        try {
            value = Long.parseLong(numberString);
        } catch (NumberFormatException e) {
            // do nothing
        }
        if (value == null) {
            numberString = "9" + getOrganization().getId() + suffix;
            value = Long.parseLong(numberString);
        }
        return value.longValue();
    }

    /**
     * Gets the previous pin criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getPreviousPinCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(m_fieldPreviousPIN, getBroker().getPersistenceKey());
        return criteria;
    }

    /**
     * Gets the school oids.
     *
     * @return List
     */
    private List<String> getSchoolOids() {
        String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
        return Arrays.asList(schoolOids.split(","));
    }

    /**
     * Gets the student criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                Student.COL_ENROLLMENT_STATUS));
        criteria.addIn(SisStudent.COL_SCHOOL_OID, getSchoolOids());
        criteria.addEmpty(m_fieldStudentPIN, getBroker().getPersistenceKey());
        criteria.addNotEqualTo(m_fieldExcludeStd, BooleanAsStringConverter.TRUE);

        return criteria;
    }

    /**
     * Gets the student pin criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStudentPinCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(m_fieldStudentPIN, getBroker().getPersistenceKey());
        return criteria;
    }

    /**
     * Gets the used pins criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getUsedPinsCriteria() {
        String refTableName = (String) getParameter(INPUT_PARAM_SKIP_PIN_REF_TABLE_NAME);
        if (StringUtils.isEmpty(refTableName)) {
            throw new TNAssignStudentPinException("The skip pin reference table name must be provided.");
        }
        checkReferenceTableExists(refTableName);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                refTableName);
        return criteria;
    }

    /**
     * Info message.
     *
     * @param key String
     */
    private void infoMessage(String msg) {
        m_infoMessages.add(msg);
    }

    /**
     * Output info messages.
     */
    private void outputInfoMessages() {
        m_infoMessages.stream().forEach(msg -> logMessage(msg));
    }

    /**
     * Translate alias.
     *
     * @param dictionary DataDictionary
     * @param alias String
     * @return String
     */
    private String translateAlias(DataDictionary dictionary, String alias, boolean required) {
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field == null && required) {
            throw new TNAssignStudentPinException("The alias [" + alias + "] could not be found.");
        }
        return field == null ? null : field.getJavaName();
    }
}
