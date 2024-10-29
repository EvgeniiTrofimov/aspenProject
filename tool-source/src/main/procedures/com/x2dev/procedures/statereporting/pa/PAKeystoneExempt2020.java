/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2021 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */


package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * The Class PAKeystoneExempt2020.
 */
public class PAKeystoneExempt2020 extends StateReportData {

    /**
     * The Class PAKeystoneExempt2020Entity.
     */
    /* Entity class */
    public static class PAKeystoneExempt2020Entity extends StateReportEntity {
        PAKeystoneExempt2020 m_data;
        SisStudent m_student;
        List<KeystoneExemptRow> m_rows;

        /**
         * Instantiates a new PA district student entity.
         */
        public PAKeystoneExempt2020Entity() {
            // Public no argument constructor for dynamic instantiation.
        }


        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", School ID: " + student.getSchool().getSchoolId() +
                    "] ";

            return name;
        }

        /**
         * Gets the current row object.
         *
         * @return Keystone exempt row
         */
        public KeystoneExemptRow getRow() {
            return m_rows.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (PAKeystoneExempt2020) data;
            m_student = (SisStudent) getBean();
            m_rows = new ArrayList<KeystoneExemptRow>();

            if (m_student.getFieldValueByBeanPath(m_data.m_fieldKeystoneExemptAlg) == BooleanAsStringConverter.TRUE) {
                String indicator;
                if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystonePassedAlg) == BooleanAsStringConverter.TRUE) {
                    indicator = "Y";
                } else {
                    indicator = "N";
                }

                String comment;
                if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystoneFirstAlg) == BooleanAsStringConverter.TRUE) {
                    comment = "FIRST";
                } else if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystoneRepeatAlg) == BooleanAsStringConverter.TRUE) {
                    comment = "REPEAT";
                } else {
                    comment = "";
                }
                m_rows.add(m_data.new KeystoneExemptRow("ALG", indicator, comment));
            }
            if (m_student.getFieldValueByBeanPath(m_data.m_fieldKeystoneExemptBio) == BooleanAsStringConverter.TRUE) {
                String indicator;
                if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystonePassedBio) == BooleanAsStringConverter.TRUE) {
                    indicator = "Y";
                } else {
                    indicator = "N";
                }

                String comment;
                if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystoneFirstBio) == BooleanAsStringConverter.TRUE) {
                    comment = "FIRST";
                } else if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystoneRepeatBio) == BooleanAsStringConverter.TRUE) {
                    comment = "REPEAT";
                } else {
                    comment = "";
                }
                m_rows.add(m_data.new KeystoneExemptRow("BIO", indicator, comment));
            }
            if (m_student.getFieldValueByBeanPath(m_data.m_fieldKeystoneExemptLit) == BooleanAsStringConverter.TRUE) {
                String indicator;
                if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystonePassedLit) == BooleanAsStringConverter.TRUE) {
                    indicator = "Y";
                } else {
                    indicator = "N";
                }

                String comment;
                if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystoneFirstLit) == BooleanAsStringConverter.TRUE) {
                    comment = "FIRST";
                } else if (m_student
                        .getFieldValueByBeanPath(m_data.m_fieldKeystoneRepeatLit) == BooleanAsStringConverter.TRUE) {
                    comment = "REPEAT";
                } else {
                    comment = "";
                }
                m_rows.add(m_data.new KeystoneExemptRow("LIT", indicator, comment));
            }

            setRowCount(m_rows.size());
        }
    }

    /**
     * Row class for retrievers
     */
    protected class KeystoneExemptRow {
        private String m_categorySetCode;
        private String m_indicator;
        private String m_comment;

        /**
         * @param categorySetCode
         * @param indicator
         * @param comment
         */
        public KeystoneExemptRow(String categorySetCode, String indicator, String comment) {
            this.m_categorySetCode = categorySetCode;
            this.m_indicator = indicator;
            this.m_comment = comment;
        }

        /**
         * @return the m_categorySetCode
         */
        public String getCategorySetCode() {
            return m_categorySetCode;
        }

        /**
         * @return the m_indicator
         */
        public String getIndicator() {
            return m_indicator;
        }

        /**
         * @return the m_comment
         */
        public String getComment() {
            return m_comment;
        }

    }

    /**
     * Field retriever for category.
     */
    protected class RetrieveCategory implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PAKeystoneExempt2020Entity stdEntity = (PAKeystoneExempt2020Entity) entity;
            return stdEntity.getRow().getCategorySetCode();
        }
    }

    /**
     * Field retriever for comment.
     */
    protected class RetrieveComment implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PAKeystoneExempt2020Entity stdEntity = (PAKeystoneExempt2020Entity) entity;
            return stdEntity.getRow().getComment();
        }
    }

    /**
     * Field retriever for end date.
     */
    protected class RetrieveEndDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return "2020-06-30";
        }
    }

    /**
     * Field retriever for indicator.
     */
    protected class RetrieveIndicator implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PAKeystoneExempt2020Entity stdEntity = (PAKeystoneExempt2020Entity) entity;
            return stdEntity.getRow().getIndicator();
        }
    }
    /**
     * Field retriever for primary measure type.
     */
    protected class RetrievePrimaryMeasureType implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return "Indicator";
        }
    }

    private static final String ALIAS_KEYSTONE_EXEMPT_ALG = "keystoneExempt-ALG";
    private static final String ALIAS_KEYSTONE_EXEMPT_BIO = "keystoneExempt-BIO";
    private static final String ALIAS_KEYSTONE_EXEMPT_LIT = "keystoneExempt-LIT";

    private static final String ALIAS_KEYSTONE_FIRST_ALG = "keystoneFirst-ALG";
    private static final String ALIAS_KEYSTONE_FIRST_BIO = "keystoneFirst-BIO";
    private static final String ALIAS_KEYSTONE_FIRST_LIT = "keystoneFirst-LIT";

    private static final String ALIAS_KEYSTONE_PASSED_ALG = "keystonePassed-ALG";
    private static final String ALIAS_KEYSTONE_PASSED_BIO = "keystonePassed-BIO";
    private static final String ALIAS_KEYSTONE_PASSED_LIT = "keystonePassed-LIT";

    private static final String ALIAS_KEYSTONE_REPEAT_ALG = "keystoneRepeat-ALG";
    private static final String ALIAS_KEYSTONE_REPEAT_BIO = "keystoneRepeat-BIO";
    private static final String ALIAS_KEYSTONE_REPEAT_LIT = "keystoneRepeat-LIT";

    private static final String PARAM_INCLUDE_HEADER = "includeHeader";

    protected String m_fieldKeystoneExemptAlg;
    protected String m_fieldKeystoneExemptBio;
    protected String m_fieldKeystoneExemptLit;
    protected String m_fieldKeystoneFirstAlg;
    protected String m_fieldKeystoneFirstBio;
    protected String m_fieldKeystoneFirstLit;
    protected String m_fieldKeystonePassedAlg;
    protected String m_fieldKeystonePassedBio;
    protected String m_fieldKeystonePassedLit;
    protected String m_fieldKeystoneRepeatAlg;
    protected String m_fieldKeystoneRepeatBio;
    protected String m_fieldKeystoneRepeatLit;

    protected boolean includeHeader;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_startDate = PlainDate.fromString("2019-08-01");
    protected PlainDate m_endDate = PlainDate.fromString("2020-06-30");

    @Override
    public String getHeading() {
        String heading = EMPTY_STRING;
        if (!StringUtils.isEmpty(m_heading) && includeHeader) {
            heading = m_heading;
            if (!m_heading.endsWith(ExportJavaSource.FORMAT_EOL_UNIX) &&
                    !m_heading.endsWith(ExportJavaSource.FORMAT_EOL_WINDOWS)) {
                heading += ExportJavaSource.FORMAT_EOL_WINDOWS;
            }
        }

        return heading;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {

        initializeFields();

        /*
         * Build helper object
         */

        if (getSetupErrors().size() == 0) {
            includeHeader = (Boolean) getParameter(PARAM_INCLUDE_HEADER);

            m_helper = new StudentHistoryHelper(this);

            // No matches
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            X2Criteria studentCriteria = m_helper.getStudentCriteria();

            X2Criteria algCriteria = new X2Criteria();
            algCriteria.addEqualTo(m_fieldKeystoneExemptAlg, BooleanAsStringConverter.TRUE);

            X2Criteria bioCriteria = new X2Criteria();
            bioCriteria.addEqualTo(m_fieldKeystoneExemptBio, BooleanAsStringConverter.TRUE);

            X2Criteria litCriteria = new X2Criteria();
            litCriteria.addEqualTo(m_fieldKeystoneExemptLit, BooleanAsStringConverter.TRUE);

            X2Criteria orCriteria = new X2Criteria();
            orCriteria.addOrCriteria(algCriteria);
            orCriteria.addOrCriteria(bioCriteria);
            orCriteria.addOrCriteria(litCriteria);

            studentCriteria.addAndCriteria(orCriteria);

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(PAKeystoneExempt2020Entity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("CATEGORY", new RetrieveCategory());
            calcs.put("COMMENT", new RetrieveComment());
            calcs.put("END_DATE", new RetrieveEndDate());
            calcs.put("INDICATOR", new RetrieveIndicator());
            calcs.put("PRIM_MEASURE_TYPE", new RetrievePrimaryMeasureType());
            super.addCalcs(calcs);
        }

    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldKeystoneExemptAlg = translateAliasToJavaName(ALIAS_KEYSTONE_EXEMPT_ALG, true);
        m_fieldKeystoneExemptBio = translateAliasToJavaName(ALIAS_KEYSTONE_EXEMPT_BIO, true);
        m_fieldKeystoneExemptLit = translateAliasToJavaName(ALIAS_KEYSTONE_EXEMPT_LIT, true);
        m_fieldKeystoneFirstAlg = translateAliasToJavaName(ALIAS_KEYSTONE_FIRST_ALG, true);
        m_fieldKeystoneFirstBio = translateAliasToJavaName(ALIAS_KEYSTONE_FIRST_BIO, true);
        m_fieldKeystoneFirstLit = translateAliasToJavaName(ALIAS_KEYSTONE_FIRST_LIT, true);
        m_fieldKeystonePassedAlg = translateAliasToJavaName(ALIAS_KEYSTONE_PASSED_ALG, true);
        m_fieldKeystonePassedBio = translateAliasToJavaName(ALIAS_KEYSTONE_PASSED_BIO, true);
        m_fieldKeystonePassedLit = translateAliasToJavaName(ALIAS_KEYSTONE_PASSED_LIT, true);
        m_fieldKeystoneRepeatAlg = translateAliasToJavaName(ALIAS_KEYSTONE_REPEAT_ALG, true);
        m_fieldKeystoneRepeatBio = translateAliasToJavaName(ALIAS_KEYSTONE_REPEAT_BIO, true);
        m_fieldKeystoneRepeatLit = translateAliasToJavaName(ALIAS_KEYSTONE_REPEAT_LIT, true);
    }
}
