/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.CRDCDataHelper.CRDCFinalData;
import com.x2dev.procedures.statereporting.CRDCDataHelper.CRDCFinalEntity;
import com.x2dev.procedures.statereporting.CRDCDataHelper.Dataset;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data Module for CRDC School Part 1 and CRDC School Part 2 Export.
 *
 * @author X2 Development Corporation
 */
public class CRDCSchData extends CRDCFinalData {
    /**
     * Implementation of StateReportEntity to be used for CRDC School Part 1
     * and CRDC School Part 2 Export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SchoolCRDCEntity extends CRDCFinalEntity {
        private static final int NES_SCHOOL_ID_MIN_LENGTH = 10;

        SisSchool m_school;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_school.getName();

            return name;
        }

        /**
         * Initialize the entity for the student bean provided.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_school = (SisSchool) bean;
            String ncesId = (String) m_school.getFieldValueByBeanPath(((CRDCSchData) data).m_fieldNCESSchoolId);
            if (ncesId.length() < NES_SCHOOL_ID_MIN_LENGTH) {
                setRowCount(0);
            }
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

    }

    /**
     * The Class RetrieverApSelfSelAll.
     */
    public class RetrieverApSelfSelAll implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return null;
        }

    }

    /**
     * The Class RetrieverApTypes.
     */
    public class RetrieverApTypes implements FieldRetriever {
        private static final String m_filter = "SchedAP={AP Mathematics,AP Science,AP Other,AP Computer Science}";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            int count = 0;
            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows =
                    crdcData.m_crdcDataHelper.getFilteredRows(Dataset.SECTION, m_filter, filtersByFieldName);
            String fieldCourseOid =
                    crdcData.m_crdcDataHelper.getBeanPath(Dataset.SECTION.getProcedureId(), "CourseOid");
            if (!rows.isEmpty() && fieldCourseOid != null) {
                Set<String> courseOids = new HashSet();
                for (ExportFormatRow row : rows) {
                    String courseOid = (String) row.getFieldValueByBeanPath(fieldCourseOid);
                    courseOids.add(courseOid);
                }
                count = courseOids.size();
            }
            return Integer.toString(count);
        }

    }

    /**
     * The Class RetrieverExists.
     */
    public class RetrieverExists implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.SCHOOL,
                    (String) field.getParameter(), filtersByFieldName);

            // Should be one row always (school dataset filtered by entity oid)
            if (!rows.isEmpty()) {
                ExportFormatRow row = rows.iterator().next();
                String fieldName = (String) field.getParameter();
                String procedureId = m_crdcDataHelper.getProcedureIdByRow(row);
                String beanPath = m_crdcDataHelper.getBeanPath(procedureId, fieldName);

                value = (String) row.getFieldValueByBeanPath(beanPath);
            }

            return value;
        }
    }

    /**
     * The Class RetrieverMstCount.
     */
    public class RetrieverMstCount implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Integer value = Integer.valueOf(0);

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.SECTION,
                    (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = Integer.valueOf(rows.size());
            }

            return value;
        }

    }

    /**
     * The Class RetrieverNCESId.
     */
    public class RetrieverNCESId implements FieldRetriever {
        private static final String ALIAS_NCES_ID = "all-skl-NCESSchoolID";
        private String m_fieldNCESId;

        /**
         * Instantiates a new retriever NCES id.
         */
        public RetrieverNCESId() {
            m_fieldNCESId = CRDCSchData.this.translateAliasToJavaName(ALIAS_NCES_ID, true);
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return entity.getBean().getFieldValueByBeanPath(m_fieldNCESId);
        }

    }

    /**
     * The Class RetrieverSklExists.
     */
    public class RetrieverSklExists implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.SCHOOL,
                    (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * The Class RetrieverStdAPCount.
     */
    public class RetrieverStdAPCount implements FieldRetriever {

        static final String DEFAULT_FILTER =
                "ActivePart1=Y,SchedAP={AP Mathematics,AP Science,AP Other,AP Computer Science},";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Integer value = Integer.valueOf(0);

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                    DEFAULT_FILTER + (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = Integer.valueOf(rows.size());
            }

            return value;
        }

    }

    /**
     * The Class RetrieverStdCount.
     */
    public class RetrieverStdCount implements FieldRetriever {

        static final String DEFAULT_FILTER = "ActivePart1=Y,";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Integer value = Integer.valueOf(0);

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());

            String fieldId = field.getFieldId();
            List<ExportFormatRow> rows;

            // S-72575.  If the student was enrolled at anytime during the reporting year, they should be counted for
            // these data elements if they qualify without considering of enrollment on Oct.
            if (fieldId.contains("COUR-4a")) {
                rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT, 
                        (String) field.getParameter(), filtersByFieldName);
            } else {
                rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                        DEFAULT_FILTER + (String) field.getParameter(), filtersByFieldName);
            }
            
           

            if (!rows.isEmpty()) {
                value = Integer.valueOf(rows.size());
            }

            return value;
        }

    }

    /**
     * The Class RetrieverStdCountK12.
     */
    public class RetrieverStdCountK12 implements FieldRetriever {

        private String m_filter;

        /**
         * Instantiates a new retriever std count K 12.
         *
         * @param filter String
         */
        public RetrieverStdCountK12(String filter) {
            m_filter = filter;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Integer value = Integer.valueOf(0);

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                    m_filter + (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = Integer.valueOf(rows.size());
            }

            return value;
        }
    }

    /**
     * The Class RetrieverStdExists.
     */
    public class RetrieverStdExists implements FieldRetriever {
        static final String DEFAULT_FILTER = "ActivePart1=Y,";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                    DEFAULT_FILTER + (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = true;
            }

            return Boolean.valueOf(value);
        }
    }

    /**
     * The Class RetrieverStdSum.
     */
    public class RetrieverStdSum implements FieldRetriever {
        static final String DEFAULT_FILTER = "ActivePart1=Y,";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Integer value = Integer.valueOf(0);

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            String fieldId = field.getFieldId();
            List<ExportFormatRow> rows;

            // S-72303.  If the student was enrolled at anytime during the reporting year, they should be counted for
            // these data elements if they qualify without considering of enrollment on Oct.
            if (fieldId.contains("DISC")) {
                rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                        (String) field.getParameter(), filtersByFieldName);
            } else {
                rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STUDENT,
                        DEFAULT_FILTER + (String) field.getParameter(), filtersByFieldName);
            }

            if (!rows.isEmpty()) {
                StringTokenizer tokenizer = new StringTokenizer((String) field.getParameter(), " =><");
                if (tokenizer.hasMoreTokens()) {
                    String fieldName = tokenizer.nextToken();
                    String beanPath = crdcData.m_crdcDataHelper
                            .getBeanPath(rows.get(0).getDefinition().getProcedureId(), fieldName);
                    int sum = 0;
                    for (ExportFormatRow row : rows) {
                        String fieldValue = (String) row.getFieldValueByBeanPath(beanPath);
                        if (!StringUtils.isEmpty(fieldValue)) {
                            try {
                                sum += Integer.parseInt(fieldValue);
                            } catch (NumberFormatException e) {
                                // Skip on parse error
                            }
                        }
                    }
                    value = Integer.valueOf(sum);
                }
            }

            return value;
        }
    }

    /**
     * The Class RetrieverStfCount.
     */
    public class RetrieverStfCount implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STAFF,
                    (String) field.getParameter(), filtersByFieldName);

            Set<String> stfOids = new HashSet();
            String oidBeanPath = crdcData.m_crdcDataHelper.getBeanPath(Dataset.STAFF.getProcedureId(), "StaffOid");

            for (ExportFormatRow row : rows) {
                stfOids.add((String) row.getFieldValueByBeanPath(oidBeanPath));

            }

            return Integer.valueOf(stfOids.size());
        }

    }

    /**
     * The Class RetrieverStfExists.
     */
    public class RetrieverStfExists implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean value = false;

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STAFF,
                    (String) field.getParameter(), filtersByFieldName);

            if (!rows.isEmpty()) {
                value = true;
            }

            return Boolean.valueOf(value);
        }

    }

    /**
     * The Class RetrieverStfFte.
     */
    public class RetrieverStfFte implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            float value = 0.0f;

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STAFF,
                    (String) field.getParameter(), filtersByFieldName);

            String fteBeanPath = crdcData.m_crdcDataHelper.getBeanPath(Dataset.STAFF.getProcedureId(), "FTE");

            for (ExportFormatRow row : rows) {
                String stringValue = (String) row.getFieldValueByBeanPath(fteBeanPath);
                stringValue = stringValue.replace(',', '.');
                float curFte = Float.parseFloat(stringValue);

                value = value + curFte;
            }
            // Use BigDecimal for precise rounding
            BigDecimal bdValue = new BigDecimal(Float.toString(value));
            BigDecimal roundedValue = bdValue.setScale(2, RoundingMode.HALF_UP);

            // Convert back to float
            return roundedValue.floatValue();
        }

    }

    /**
     * The Class RetrieverSum.
     */
    public class RetrieverSum implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows =
                    crdcData.m_crdcDataHelper.getFilteredRows(Dataset.SCHOOL, null, filtersByFieldName);

            // Should be one row always (school dataset filtered by entity oid)
            if (!rows.isEmpty()) {
                ExportFormatRow row = rows.iterator().next();
                String fieldName = (String) field.getParameter();
                String procedureId = m_crdcDataHelper.getProcedureIdByRow(row);
                String beanPath = m_crdcDataHelper.getBeanPath(procedureId, fieldName);

                value = (String) row.getFieldValueByBeanPath(beanPath);
            }

            return value;
        }
    }

    /**
     * The Class RetrieverSumFte.
     */
    public class RetrieverSumFte implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = BigDecimal.ZERO;

            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STAFF,
                    (String) field.getParameter(), filtersByFieldName);
            if (!rows.isEmpty()) {
                String beanPath =
                        crdcData.m_crdcDataHelper.getBeanPath(rows.get(0).getDefinition().getProcedureId(), "FTE");
                double sum = 0;
                for (ExportFormatRow row : rows) {
                    String fieldValue = (String) row.getFieldValueByBeanPath(beanPath);
                    if (!StringUtils.isEmpty(fieldValue)) {
                        try {
                            sum += Double.parseDouble(fieldValue);
                        } catch (NumberFormatException e) {
                            // Skip on parse error
                        }
                    }
                }
                value = new BigDecimal(sum);
            }

            return value;
        }

    }

    /**
     * The Class RetrieverPriorYearStfCount.
     */
    public class RetrieverPriorYearStfCount implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CRDCSchData crdcData = (CRDCSchData) data;
            Map<Dataset, String> filtersByFieldName = new HashMap<Dataset, String>();
            filtersByFieldName.put(Dataset.SCHOOL, entity.getBean().getOid());
            List<ExportFormatRow> rows = crdcData.m_crdcDataHelper.getFilteredRows(Dataset.STAFF_PRIOR_YR,
                    (String) field.getParameter(), filtersByFieldName);

            Set<String> stfOids = new HashSet();
            String oidBeanPath =
                    crdcData.m_crdcDataHelper.getBeanPath(Dataset.STAFF_PRIOR_YR.getProcedureId(), "StaffOid");

            for (ExportFormatRow row : rows) {
                stfOids.add((String) row.getFieldValueByBeanPath(oidBeanPath));

            }

            return Integer.valueOf(stfOids.size());
        }

    }

    private static final String ALIAS_NCES_SCHOOL_ID = "all-skl-NCESSchoolID";

    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";

    private static final String INPUT_PARAM_RESULT_OID_MST = "mstResultOid";
    private static final String INPUT_PARAM_RESULT_OID_PRIOR_YEAR_STD_MST = "priorYrStfResultOid";
    private static final String INPUT_PARAM_RESULT_OID_SKL = "sklResultOid";
    private static final String INPUT_PARAM_RESULT_OID_STD = "stdResultOid";
    private static final String INPUT_PARAM_RESULT_OID_STD_MST = "stdMstResultOid";
    private static final String INPUT_PARAM_RESULT_OID_STF = "stfResultOid";

    protected CRDCDataHelper m_crdcDataHelper = null;
    protected String m_fieldNCESSchoolId;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        m_crdcDataHelper = new CRDCDataHelper(this, getResultsCriteria());
        m_crdcDataHelper.setReportDate((PlainDate) getParameter(INPUT_PARAM_REPORT_DATE));
        m_fieldNCESSchoolId = translateAliasToJavaName(ALIAS_NCES_SCHOOL_ID, true);

        if (getSetupErrors().isEmpty()) {
            X2Criteria schoolCriteria = new X2Criteria();

            if (isSchoolContext()) {
                schoolCriteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
            } else {
                schoolCriteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
                schoolCriteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
            }

            schoolCriteria.addNotEmpty(m_fieldNCESSchoolId, getBroker().getPersistenceKey());
            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);

            setQuery(schoolQuery);
            setEntityClass(SchoolCRDCEntity.class);

            Map<String, FieldRetriever> calcMap = new HashMap<String, FieldRetriever>();
            calcMap.put("SCHOOL_P1_STD_EXISTS", new RetrieverStdExists());
            calcMap.put("SCHOOL_P1_AP_SLFSELA", new RetrieverApSelfSelAll());
            calcMap.put("SCHOOL_P1_AP_TYPES", new RetrieverApTypes());
            calcMap.put("SCHOOL_P1_MST_COUNT", new RetrieverMstCount());
            calcMap.put("SCHOOL_P1_STD_COUNT", new RetrieverStdCount());
            calcMap.put("SCHOOL_P1_STF_COUNT", new RetrieverStfCount());
            calcMap.put("SCHOOL_P1_PRYRSTF_CT", new RetrieverPriorYearStfCount());
            calcMap.put("SCHOOL_P1_STF_EXISTS", new RetrieverStfExists());
            calcMap.put("SCHOOL_P1_STF_FTE", new RetrieverStfFte());
            calcMap.put("SCHOOL_P2_STD_COUNT", new RetrieverStdCount());
            calcMap.put("SCHOOL_P2_AP_COUNT", new RetrieverStdAPCount());
            calcMap.put("SCHOOL_P2_STD_EXISTS", new RetrieverStdExists());
            calcMap.put("SCHOOL_P2_STD_SUM", new RetrieverStdSum());
            calcMap.put("SCH_P2_STD_COUNT_K12", new RetrieverStdCountK12(""));
            calcMap.put("SCH_P2_Q19_COUNT_K12", new RetrieverStdCountK12("IDEA=N,Section504=N,"));
            calcMap.put("SCHOOL_P2_EXISTS", new RetrieverExists());
            calcMap.put("SCHOOL_P2_SUM", new RetrieverSum());
            calcMap.put("SCHOOL_P2_SUM_FTE", new RetrieverSumFte());
            calcMap.put("SCHOOL_P2_SKL_EXISTS", new RetrieverSklExists());
            calcMap.put("SCH_ID", new RetrieverNCESId());

            addCalcs(calcMap);
        }
    }

    /**
     * Gets the results criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getResultsCriteria() {
        Collection<String> resultsOids = new ArrayList<String>();

        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_SKL));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_STD));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_STF));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_MST));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_STD_MST));
        resultsOids.add((String) getParameter(INPUT_PARAM_RESULT_OID_PRIOR_YEAR_STD_MST));

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(X2BaseBean.COL_OID, resultsOids);

        return criteria;
    }
    
}
