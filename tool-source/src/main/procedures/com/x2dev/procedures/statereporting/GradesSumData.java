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

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DictionaryExtendable;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.ToolManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.reports.ReportCompiler;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerFactoryConfigurationError;
import net.sf.jasperreports3.engine.JRException;
import net.sf.json.JSONObject;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Export data module for SR-GRD-SUM reports.
 *
 * @author Follett Software Company
 */
public class GradesSumData extends StateReportData {
    /**
     * Encapsulate comparators of each field type.
     *
     * @author Follett Software Company
     */
    private static class ComparatorFactory {

        /**
         * The Class Comparator.
         */
        protected abstract static class Comparator {

            /**
             * The Enum Sign.
             */
            private enum Sign {
                EQUAL("="), GREATER(">"), LESS("<");

                String m_sign;

                /**
                 * Instantiates a new sign.
                 *
                 * @param sign String
                 */
                private Sign(String sign) {
                    m_sign = sign;
                }

                /**
                 * Gets the sign.
                 *
                 * @return String
                 */
                public String getSign() {
                    return m_sign;
                }
            }

            /**
             * Returns result of expression "dbValue 'sign' paramValue" where 'sign' is comparison
             * sign from enum Sign.
             * Result of expression is boolean.
             *
             * @param dbValue Object
             * @param sign String
             * @param paramValue Object
             * @return boolean
             */
            public boolean compare(Object dbValue, String sign, Object paramValue) {
                dbValue = getConvertedDBValue(dbValue);
                paramValue = getConvertedParamValue(paramValue);
                Integer[] expectedResults = signToComparatorResults(sign);

                boolean result = false;

                if (dbValue instanceof Comparable && paramValue instanceof Comparable) {
                    int compResult = ((Comparable) dbValue).compareTo((paramValue));
                    result = Arrays.asList(expectedResults).contains(Integer.valueOf(compResult));
                } else {
                    if (dbValue == null) {
                        if (paramValue == null) {
                            result = true;
                        }
                    } else {
                        throw new X2RuntimeException();
                    }
                }

                return result;
            }

            /**
             * Returns converted to comparable DBvalue.
             *
             * @param dbValue Object
             * @return Object
             */
            abstract public Object getConvertedDBValue(Object dbValue);

            /**
             * Returns converted to comparable paramValue.
             *
             * @param paramValue Object
             * @return Object
             */
            abstract public Object getConvertedParamValue(Object paramValue);

            /**
             * Returns array of results that should be to return to return "true" by comparator.
             *
             * @param sign String
             * @return Integer[]
             */
            private Integer[] signToComparatorResults(String sign) {
                Integer[] expectedResults = new Integer[3];

                if (sign.contains(Sign.LESS.getSign())) {
                    expectedResults[1] = Integer.valueOf(-1);
                }
                if (sign.contains(Sign.EQUAL.getSign())) {
                    expectedResults[1] = Integer.valueOf(0);
                }
                if (sign.contains(Sign.GREATER.getSign())) {
                    expectedResults[1] = Integer.valueOf(1);
                }

                return expectedResults;
            }
        }

        /**
         * Dates comparator implementation.
         *
         * @author Follett Software Company
         */
        private class DatesComparator extends Comparator {
            private Converter converter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER);

            /**
             * Instantiates a new dates comparator.
             */
            public DatesComparator() {
                // to avoid warning
            }

            /**
             * Gets the converted DB value.
             *
             * @param dbValue Object
             * @return Object
             * @see com.x2dev.procedures.statereporting.GradesSumData.ComparatorFactory.Comparator#
             *      getConvertedDBValue(java.lang.Object)
             */
            @Override
            public Object getConvertedDBValue(Object dbValue) {
                if (!(dbValue instanceof PlainDate)) {
                    dbValue = converter.stringToJava((String) dbValue);
                }
                return dbValue;
            }

            /**
             * Gets the converted param value.
             *
             * @param paramValue Object
             * @return Object
             * @see com.x2dev.procedures.statereporting.GradesSumData.ComparatorFactory.Comparator#
             *      getConvertedParamValue(java.lang.Object)
             */
            @Override
            public Object getConvertedParamValue(Object paramValue) {
                if (!(paramValue instanceof PlainDate)) {
                    paramValue = converter.stringToJava((String) paramValue);
                }
                return paramValue;
            }
        }

        /**
         * Logical comparator implementation.
         *
         * @author Follett Software Company
         */
        private class LogicalComparator extends Comparator {
            private Converter converter = ConverterFactory.getConverterForClass(Converter.BOOLEAN_CONVERTER);

            /**
             * Instantiates a new logical comparator.
             */
            public LogicalComparator() {
                // to avoid warning
            }

            /**
             * Gets the converted DB value.
             *
             * @param dbValue Object
             * @return Object
             * @see com.x2dev.procedures.statereporting.GradesSumData.ComparatorFactory.Comparator#
             *      getConvertedDBValue(java.lang.Object)
             */
            @Override
            public Object getConvertedDBValue(Object dbValue) {
                if (!(dbValue instanceof Boolean)) {
                    dbValue = converter.stringToJava((String) dbValue);
                }
                return dbValue;
            }

            /**
             * Gets the converted param value.
             *
             * @param paramValue Object
             * @return Object
             * @see com.x2dev.procedures.statereporting.GradesSumData.ComparatorFactory.Comparator#
             *      getConvertedParamValue(java.lang.Object)
             */
            @Override
            public Object getConvertedParamValue(Object paramValue) {
                if (!(paramValue instanceof Boolean)) {
                    paramValue = Boolean.valueOf(LogicalParamValue.TRUE.name().equalsIgnoreCase((String) paramValue));
                }
                return paramValue;
            }

        }

        /**
         * Number comparator implementation.
         *
         * @author Follett Software Company
         */
        private class NumberComparator extends Comparator {
            Converter converter = ConverterFactory.getConverterForClass(Converter.DOUBLE_CONVERTER);

            /**
             * Instantiates a new number comparator.
             */
            public NumberComparator() {
                // to avoid warning
            }

            /**
             * Gets the converted DB value.
             *
             * @param dbValue Object
             * @return Object
             * @see com.x2dev.procedures.statereporting.GradesSumData.ComparatorFactory.Comparator#
             *      getConvertedDBValue(java.lang.Object)
             */
            @Override
            public Object getConvertedDBValue(Object dbValue) {
                if (!(dbValue instanceof Number)) {
                    dbValue = converter.stringToJava((String) dbValue);
                }
                return dbValue;
            }

            /**
             * Gets the converted param value.
             *
             * @param paramValue Object
             * @return Object
             * @see com.x2dev.procedures.statereporting.GradesSumData.ComparatorFactory.Comparator#
             *      getConvertedParamValue(java.lang.Object)
             */
            @Override
            public Object getConvertedParamValue(Object paramValue) {
                if (!(paramValue instanceof Number)) {
                    paramValue = converter.stringToJava((String) paramValue);
                }
                return paramValue;
            }
        }

        /**
         * The Enum LogicalParamValue.
         */
        private enum LogicalParamValue {
            TRUE, FALSE
        }

        /**
         * Instantiates a new comparator factory.
         */
        public ComparatorFactory() {
            // to avoid warning
        }

        /**
         * Returns comparator based on passed formatType.
         *
         * @param code FormatTypeCode
         * @return Comparator
         */
        public Comparator getComparator(ExportFormatField.FormatTypeCode code) {
            Comparator comparator = null;

            switch (code) {
                case DATE:
                    comparator = new DatesComparator();
                    break;

                case NUMBER:
                    comparator = new NumberComparator();
                    break;

                case LOGICAL:
                    comparator = new LogicalComparator();
                    break;

                default:
                    break;
            }

            return comparator;
        }
    }

    /**
     * Entity returning data for export format field based on parameters of field and current bean.
     *
     * @author Follett Software Company
     */
    public static class GradesSumEntity extends StateReportEntity {
        public static final String FIELD_GRADE = "grade";
        public static final String FIELD_SCHOOL = "school";
        public static final String FIELD_STUDENT = "student";
        public static final String FIELD_ROW_NAME = "rowName";
        public static final String FIELD_TYPE = "type";
        public static final String FIELD_VALUE = "value";

        /**
         * Instantiates a new grades sum entity.
         */
        public GradesSumEntity() {
            // Empty for dynamic instantiation.
        }

        private X2BaseBean m_bean = null;
        private ExtendedDataDictionary m_extDictionary = null;
        private ExportFormatDefinition m_formatDefinition = null;
        private GradesSumData m_gsData = null;
        private Pattern m_relationPattern = Pattern.compile("\\|\\||\\&\\&");
        private Pattern m_signPattern = Pattern.compile("<=|>=|!=|=|>|<");

        /**
         * Returns the current export format definition for the current row of the entity.
         *
         * @return Export format definition
         */
        public ExportFormatDefinition getCurrentFormatDefinition() {
            return m_formatDefinition;
        }

        /**
         * Gets the current format definition id.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#
         *      getCurrentFormatDefinitionId()
         */
        @Override
        public String getCurrentFormatDefinitionId() {
            return getCurrentFormatDefinition().getProcedureId().replace(m_gsData.getProcedureId() + "-", EMPTY_STRING);
        }

        /**
         * Returns number of export format definitions fields.
         *
         * @return int
         */
        public int getFieldCount() {
            return m_formatDefinition.getFields().size();
        }

        /**
         * Overridden to return json string containing all needed data calculated based on
         * current bean and field parameters.
         *
         * @param index int
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValue(int)
         */
        @Override
        public String getFieldValue(int index) {
            FieldDefinition fieldDefinition = m_gsData.getFieldDefinition(index);
            JSONObject jsonObject = new JSONObject();

            SisStudent student = ((SisStudent) m_bean.getFieldValueByBeanPath(m_gsData.m_studentBeanPath));
            jsonObject.put(FIELD_SCHOOL, student.getSchool().getName());
            jsonObject.put(FIELD_STUDENT, student.getOid());
            jsonObject.put(FIELD_GRADE, student.getGradeLevel());
            jsonObject.put(FIELD_TYPE, m_extDictionary.getName());
            jsonObject.put(FIELD_ROW_NAME, fieldDefinition.getSifPath());
            jsonObject.put(FIELD_VALUE, calcValue(fieldDefinition));

            return jsonObject.toString();
        }

        /**
         * Intitialize.
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

            m_gsData = (GradesSumData) data;
            m_bean = bean;

            m_formatDefinition = determineFormatDefinition();

            if (m_formatDefinition == null) {
                setRowCount(0);
            }
        }

        /**
         * Returns calculated value ("0" or "1") based on current bean and field parameters from
         * passed field definition.
         *
         * @param fd FieldDefinition
         * @return String
         */
        private String calcValue(FieldDefinition fd) {
            boolean meetCriteria = false;

            String parameter = (String) fd.getParameter();

            if (!StringUtils.isEmpty(parameter)) {
                ArrayList<String> relationCriteria = parseCriteria(parameter);

                ArrayList<String> relationCollection = new ArrayList<String>();
                ArrayList<String> criteriaCollection = new ArrayList<String>();
                ArrayList<Boolean> meetsCollection = new ArrayList<Boolean>();

                for (String criteria : relationCriteria) {
                    Matcher matcher = m_relationPattern.matcher(criteria);
                    String relation = null;
                    if (matcher.find()) {
                        relation = matcher.group();
                    }

                    if (relation != null) {
                        criteria = criteria.substring(relation.length(), criteria.length());
                    }

                    relationCollection.add(relation);
                    criteriaCollection.add(criteria);

                    ExportFormatField.FormatTypeCode formatType =
                            m_gsData.getFormatType(m_formatDefinition.getProcedureId(), parameter);

                    meetsCollection.add(meetCriteria(formatType, criteria));
                }

                for (int i = 0; i < meetsCollection.size(); i++) {
                    Boolean curMeetCriteria = meetsCollection.get(i);
                    String curRelation = relationCollection.get(i);

                    if (StringUtils.isEmpty(curRelation)) {
                        meetCriteria = curMeetCriteria.booleanValue();
                    } else {
                        switch (curRelation) {
                            case "&&":
                                meetCriteria = meetCriteria && curMeetCriteria.booleanValue();
                                break;
                            case "||":
                                meetCriteria = meetCriteria || curMeetCriteria.booleanValue();
                                break;
                            default:
                                break;
                        }
                    }
                }
            }

            return meetCriteria ? BooleanAsStringConverter.TRUE : BooleanAsStringConverter.FALSE;
        }

        /**
         * Returns export format definitions based on ID from export format definition name.
         *
         * @return ExportFormatDefinition
         */
        private ExportFormatDefinition determineFormatDefinition() {
            ExportFormatDefinition formatDefinition = null;
            if (m_bean instanceof DictionaryExtendable) {
                m_extDictionary = (ExtendedDataDictionary) ((DictionaryExtendable) m_bean).getExtendedDataDictionary();
                for (ExportFormatDefinition efd : m_gsData.m_formatDefinitions) {
                    if ((m_extDictionary == null && efd.getExtendedDataDictionary() == null) ||
                            (m_extDictionary != null && m_extDictionary.equals(efd.getExtendedDataDictionary()))) {
                        formatDefinition = efd;
                        DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_extDictionary,
                                m_gsData.getBroker().getPersistenceKey());
                        m_gsData.setDataDictionary(dictionary);
                    }
                }
            } else {
                formatDefinition = m_gsData.m_formatDefinitions.iterator().next();
            }
            return formatDefinition;
        }

        /**
         * Returns true if bean meet passed criteria.
         *
         * @param formatType FormatTypeCode
         * @param criteria String
         * @return Boolean
         */
        private Boolean meetCriteria(ExportFormatField.FormatTypeCode formatType, String criteria) {
            boolean meetCriteria = false;

            String fieldPath = null;
            String sign = null;
            String paramValue = null;

            Matcher signMatcher = m_signPattern.matcher(criteria);
            if (signMatcher.find()) {
                sign = signMatcher.group();
            }

            if (!StringUtils.isEmpty(sign)) {
                fieldPath = criteria.substring(0, criteria.indexOf(sign));
                sign = criteria.substring(criteria.indexOf(sign), criteria.indexOf(sign) + sign.length());
                paramValue = criteria.substring(criteria.indexOf(sign) + sign.length(), criteria.length());
            } else {
                fieldPath = criteria;
            }

            fieldPath = m_gsData.getResolvedAliasBeanPath(fieldPath);
            Object dbValue = null;
            try {
                dbValue = WebUtils.getProperty(m_bean, fieldPath);
            } catch (X2BaseException e) {
                // remains null
            }

            if (StringUtils.isEmpty(sign)) {
                if (dbValue instanceof Boolean) {
                    meetCriteria = ((Boolean) dbValue).booleanValue();
                }
                /*
                 * If not boolean just check if value exists
                 */
                else {
                    meetCriteria = !StringUtils.isEmpty((String) dbValue);
                }
            } else {
                switch (formatType) {
                    case DATE:
                        meetCriteria = m_gsData.m_comparatorDates.compare(dbValue, sign, paramValue);
                        break;

                    case NUMBER:
                        meetCriteria = m_gsData.m_comparatorNumber.compare(dbValue, sign, paramValue);
                        break;

                    case LOGICAL:
                        meetCriteria = m_gsData.m_comparatorLogical.compare(dbValue, sign, paramValue);
                        break;

                    case NONE:
                        meetCriteria = (dbValue == paramValue || dbValue.equals(paramValue));
                        break;

                    default:
                        break;
                }
            }
            return Boolean.valueOf(meetCriteria);
        }

        /**
         * Parses the parameter and returns ordered list of criteria with AND/OR relation if exists.
         *
         * @param parameter String
         * @return ArrayList<String>
         */
        private ArrayList<String> parseCriteria(String parameter) {
            ArrayList<String> criteriaCollection = new ArrayList<String>();

            Matcher matcher = m_relationPattern.matcher(parameter);
            while (matcher.find()) {
                String relation = matcher.group();
                String beforeRelation = parameter.substring(0, parameter.indexOf(relation));
                if (!StringUtils.isEmpty(beforeRelation)) {
                    criteriaCollection.add(beforeRelation);
                }

                String afterRelation =
                        parameter.substring(parameter.indexOf(relation) + relation.length(), parameter.length());
                Matcher subMatcher = m_relationPattern.matcher(afterRelation);
                if (subMatcher.find()) {
                    String nextRelation = subMatcher.group();
                    criteriaCollection.add(relation + afterRelation.substring(0, afterRelation.indexOf(nextRelation)));
                    criteriaCollection.addAll(parseCriteria(
                            afterRelation.substring(afterRelation.indexOf(nextRelation), afterRelation.length())));
                } else {
                    criteriaCollection.add(relation + afterRelation);
                }
            }

            if (criteriaCollection.isEmpty()) {
                criteriaCollection.add(parameter);
            }

            return criteriaCollection;
        }
    }

    /**
     * Abstract class for grade summary reports. It is here as a static nested class.
     *
     * @author Follett Software Company
     */
    public static abstract class GradesSumReport extends ReportJavaSourceNet {
        private static final String FIELD_ROW_ORDER = "rowOrder";

        private static final int GRADE_COLUMN_WIDTH = 25;

        private static final String INPUT_PARAM_FORMAT_ID_CSV = "CSV_FORMAT_ID";
        private static final String INPUT_PARAM_FORMAT_ID_PDF = "PDF_FORMAT_ID";

        private static final String JR_DISTRICT_SUMMARY = "ZZZ_DISTRICTSUMMARY";
        private static final String JR_ELEMENT_REPORT_ELEMENT = "reportElement";
        private static final String JR_ELEMENT_TEXT_FIELD = "textField";
        private static final String JR_ELEMENT_TEXT_ELEMENT = "textElement";
        private static final String JR_ELEMENT_TEXT_FIELD_EXPRESSION = "textFieldExpression";
        private static final String JR_KEY_TYPE_ANCHOR = "Anchor";
        private static final String JR_KEY_TYPE_SHIFTED = "Shifted";
        private static final String JR_MARKED_ELEMENT_AREA_DETAIL = "detail";
        private static final String JR_MARKED_ELEMENT_AREA_HEADER = "header";
        private static final String JR_MARKED_ELEMENT_AREA_SUMMARY = "detailSummary";
        private static final String JR_REPORT_ATTRIBUTE_ALIGNMENT = "textAlignment";
        private static final String JR_REPORT_ATTRIBUTE_EVALUATION_GROUP = "evaluationGroup";
        private static final String JR_REPORT_ATTRIBUTE_EVALUATION_TIME = "evaluationTime";
        private static final String JR_REPORT_ATTRIBUTE_KEY = "key";
        private static final String JR_REPORT_ATTRIBUTE_LEFT = "x";
        private static final String JR_REPORT_ATTRIBUTE_WIDTH = "width";
        private static final String JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT = "Right";
        private static final String JR_REPORT_ATT_VALUE_GROUP = "Group";
        private static final String JR_REPORT_ATT_VALUE_ROW_NAME = "rowName";
        private static final String JR_SUMMARY_VAR_POSTFIX = "summary";

        private static final String NEW_COLUMN = "newColumn";

        private static final String PARAM_END_DATE = "endDate";
        private static final String PARAM_GRADES = "selectedGrades";
        private static final String PARAM_ORGANIZATION = "organization";
        private static final String PARAM_START_DATE = "startDate";
        private static final String PARAM_TITLE = "title";

        private static final String SR_GRD_SUM_PROCEDURE_ID = "SR-GRD-SUM";

        Comparator<ExportFormatField> m_fieldsComparator;

        private Document m_format;
        private Boolean m_hideEmpty;
        private Report m_report;
        private StateReportData m_reportData;
        private List<String> m_validGrades =
                Arrays.asList("K4", "K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");

        /**
         * Gather data.
         *
         * @return Object
         * @throws Exception exception
         * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
         */
        @Override
        protected Object gatherData() throws Exception {
            m_reportData.open();

            GradesSumEntity entity = null;

            Set<ExportFormatDefinition> summaryDefinitions = new HashSet<ExportFormatDefinition>();

            ReportDataGrid grid = new ReportDataGrid();

            while ((entity = (GradesSumEntity) m_reportData.next()) != null) {
                ExportFormatDefinition currentFormatDefinition = entity.getCurrentFormatDefinition();
                int fieldCount = currentFormatDefinition.getFields().size();
                for (int i = 0; i < fieldCount; i++) {
                    String fieldsValuesJsonString = entity.getFieldValue(i);
                    ObjectMapper mapper = new ObjectMapper();
                    Map<String, String> map =
                            mapper.readValue(fieldsValuesJsonString, new TypeReference<Map<String, String>>() {
                                // empty block
                            });

                    String school = map.get(GradesSumEntity.FIELD_SCHOOL);
                    String type = map.get(GradesSumEntity.FIELD_TYPE);
                    String rowName = map.get(GradesSumEntity.FIELD_ROW_NAME);
                    String student = map.get(GradesSumEntity.FIELD_STUDENT);
                    String grade = map.get(GradesSumEntity.FIELD_GRADE);
                    String value = map.get(GradesSumEntity.FIELD_VALUE);

                    if (!StringUtils.isEmpty(rowName)) {
                        grid.append();

                        grid.set(FIELD_ROW_ORDER, Integer.valueOf(i));
                        grid.set(GradesSumEntity.FIELD_SCHOOL, school);
                        grid.set(GradesSumEntity.FIELD_TYPE, type);
                        grid.set(GradesSumEntity.FIELD_ROW_NAME, rowName);
                        grid.set(GradesSumEntity.FIELD_STUDENT, student);
                        grid.set(GradesSumEntity.FIELD_GRADE, grade);
                        grid.set(GradesSumEntity.FIELD_VALUE, Integer.valueOf(value));

                        if (!StringUtils.isEmpty(value) && !"0".equals(value)) {
                            summaryDefinitions.add(currentFormatDefinition);
                        }
                    }
                }
            }

            // compare to avoid null pointer if m_hideEmpty is null
            if (!Boolean.TRUE.equals(m_hideEmpty)) {
                addEmptyReports(grid);

                if (grid.getRows().isEmpty()) {
                    grid.append();
                }
            }

            addSummaries(grid, summaryDefinitions);

            grid.sort(Arrays.asList(GradesSumEntity.FIELD_TYPE,
                    GradesSumEntity.FIELD_SCHOOL,
                    FIELD_ROW_ORDER,
                    GradesSumEntity.FIELD_ROW_NAME), true);

            grid.beforeTop();
            return grid;
        }

        /**
         * Return the query that will be used to specify the collection of items that are processed
         * by this report. The
         * class of this query must match the data table fo the export format definitions used for
         * the report.
         *
         * @return Bean query
         */
        protected abstract BeanQuery getDetailQuery();

        /**
         * The end date for the report interval.
         *
         * @return Plain date
         */
        protected abstract PlainDate getEndDate();

        /**
         * Overridden to compile and use adjusted format.
         *
         * @return Input stream
         */
        @Override
        protected InputStream getFormat() {
            byte[] compiledFormat = null;

            try {
                ReportCompiler reportCompiler = new ReportCompiler(getJob().getTempFolder(), getBroker());

                DOMImplementationLS domImplementationLS =
                        (DOMImplementationLS) m_format.getImplementation().getFeature("LS", "3.0");
                LSOutput lsOutput = domImplementationLS.createLSOutput();
                ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                lsOutput.setByteStream(outputStream);
                LSSerializer lsSerializer = domImplementationLS.createLSSerializer();
                lsSerializer.write(m_format, lsOutput);

                compiledFormat = reportCompiler.compileReport(outputStream.toByteArray(), m_report.getEngineVersion(),
                        m_report.getJarPluginPath());
            } catch (JRException | TransformerFactoryConfigurationError | IOException | X2BaseException
                    | dori.jasper.engine.JRException | net.sf.jasperreports.engine.JRException
                    | net.sf.jasperreports5.engine.JRException | net.sf.jasperreports6.engine.JRException e) {
                // nothing to do. Can not throw new X2BaseException(e), because super.getFormat
                // hasn't throws
            }

            return new ByteArrayInputStream(compiledFormat);
        }

        /**
         *
         * The format definitions used to specify the rows for this report.
         *
         * @return Collection
         */
        protected abstract Collection<ExportFormatDefinition> getFormatDefinitions();

        /**
         *
         * The grades that will be summarized in this report.
         *
         * @return Collection
         */
        protected abstract Collection<String> getGrades();

        /**
         * The schools that are included in this report.
         *
         * @return Collection
         */
        protected abstract Collection<SisSchool> getSchools();

        /**
         *
         * The start date for the report interval.
         *
         * @return Plain date
         */
        protected abstract PlainDate getStartDate();

        /**
         *
         * The title for the report.
         *
         * @return String
         */
        protected abstract String getTitle();

        /**
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
         */
        @Override
        protected void initialize() throws X2BaseException {
            m_hideEmpty = (Boolean) getParameter("hideEmpty");

            initReportsFormat();
            initStateReportData();

            m_fieldsComparator = new Comparator<ExportFormatField>() {
                @Override
                public int compare(ExportFormatField o1, ExportFormatField o2) {
                    return Integer.valueOf(o1.getPosition()).compareTo(Integer.valueOf(o2.getPosition()));
                }
            };

            m_report = ReportUtils.getReport(getFormatID(), getBroker());

            adjustFormat();

            addParameter(PARAM_GRADES, getGrades());
            addParameter(PARAM_ORGANIZATION, getOrganization());
            addParameter(PARAM_START_DATE, getStartDate());
            addParameter(PARAM_END_DATE, getEndDate());
            addParameter(PARAM_TITLE, getTitle());

            super.initialize();
        }

        /**
         * Fill grid with nulls for selected types, rows, schools.
         *
         * @param grid ReportDataGrid
         */
        private void addEmptyReports(ReportDataGrid grid) {
            for (ExportFormatDefinition efd : getFormatDefinitions()) {
                ExtendedDataDictionary m_dictionary = efd.getExtendedDataDictionary();

                ArrayList<ExportFormatField> fieldsCollection = new ArrayList<ExportFormatField>(efd.getFields());
                Collections.sort(fieldsCollection, m_fieldsComparator);

                Collection<String> schoolNames = new ArrayList<String>();

                for (SisSchool school : getSchools()) {
                    schoolNames.add(school.getName());
                }

                for (int i = 0; i < fieldsCollection.size(); i++) {
                    for (String schoolName : schoolNames) {
                        grid.append();
                        grid.set(FIELD_ROW_ORDER, Integer.valueOf(i));
                        grid.set(GradesSumEntity.FIELD_SCHOOL, schoolName);
                        grid.set(GradesSumEntity.FIELD_GRADE, "dummy");
                        grid.set(GradesSumEntity.FIELD_TYPE, m_dictionary.getName());
                        grid.set(GradesSumEntity.FIELD_ROW_NAME, fieldsCollection.get(i).getSifPath());
                        grid.set(GradesSumEntity.FIELD_VALUE, Integer.valueOf(0));
                    }
                }
            }
        }

        /**
         * Add DISTRICT SUMMARY as last school.
         *
         * @param grid ReportDataGrid
         * @param summaryDefinitions Set<ExportFormatDefinition>
         */
        private void addSummaries(ReportDataGrid grid, Set<ExportFormatDefinition> summaryDefinitions) {
            Collection<ExportFormatDefinition> definitions =
                    !Boolean.TRUE.equals(m_hideEmpty) ? getFormatDefinitions() : summaryDefinitions;
            for (ExportFormatDefinition efd : definitions) {
                ExtendedDataDictionary m_dictionary = efd.getExtendedDataDictionary();

                ArrayList<ExportFormatField> fieldsCollection = new ArrayList<ExportFormatField>(efd.getFields());
                Collections.sort(fieldsCollection, m_fieldsComparator);

                Collection<String> schoolNames = new ArrayList<String>();

                for (SisSchool school : getSchools()) {
                    schoolNames.add(school.getName());
                }
                schoolNames.add(JR_DISTRICT_SUMMARY);

                for (int i = 0; i < fieldsCollection.size(); i++) {
                    grid.append();
                    grid.set(FIELD_ROW_ORDER, Integer.valueOf(i));
                    grid.set(GradesSumEntity.FIELD_SCHOOL, JR_DISTRICT_SUMMARY);
                    grid.set(GradesSumEntity.FIELD_GRADE, "dummy");
                    grid.set(GradesSumEntity.FIELD_TYPE, m_dictionary.getName());
                    grid.set(GradesSumEntity.FIELD_ROW_NAME, fieldsCollection.get(i).getSifPath());
                    grid.set(GradesSumEntity.FIELD_VALUE, Integer.valueOf(0));
                }
            }
        }

        /**
         * Add selected grades dynamically.
         */
        private void adjustFormat() {
            String format = m_report.getFormat();

            try {
                DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
                dbf.setNamespaceAware(true);
                dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
                dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
                DocumentBuilder builder = dbf.newDocumentBuilder();
                m_format = builder.parse(new InputSource(new StringReader(format)));

                Element root = m_format.getDocumentElement();
                NodeList reportElements = root.getElementsByTagName(JR_ELEMENT_REPORT_ELEMENT);

                /*
                 * Find Anchors nodes.
                 */
                Map<String, Node> bindAnchorMap = initBindNodeMap(reportElements, JR_KEY_TYPE_ANCHOR);

                /*
                 * Find Shifted nodes.
                 */

                Map<String, Node> bindShiftedMap = initBindNodeMap(reportElements, JR_KEY_TYPE_SHIFTED);

                /*
                 * Insert grades.
                 */
                for (String grade : m_validGrades) {
                    if (!getGrades().contains(grade)) {
                        continue;
                    }

                    for (Entry<String, Node> entry : bindAnchorMap.entrySet()) {
                        String bind = entry.getKey();
                        Node anchor = entry.getValue();

                        String newColumnPrefix = NEW_COLUMN + bind;

                        Node newElement = anchor.cloneNode(true);
                        NodeList newElementChildren = newElement.getChildNodes();

                        for (int k = 0; k < newElementChildren.getLength(); k++) {
                            Node newElementChild = newElementChildren.item(k);
                            if (attributeKeyContains(newElementChild, JR_KEY_TYPE_ANCHOR)) {
                                newElementChild.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY)
                                        .setTextContent(newColumnPrefix + "0");
                            }
                        }

                        Node bindNode = anchor.getParentNode();
                        bindNode.insertBefore(newElement, bindShiftedMap.get(bind));

                        /*
                         * Get element that before new added element to determine correct position
                         * and key of the new element.
                         */
                        String reportElementContainerName = null;
                        String textContainerName = null;
                        switch (bind) {
                            case JR_MARKED_ELEMENT_AREA_HEADER:
                                reportElementContainerName = JR_ELEMENT_TEXT_FIELD;
                                textContainerName = JR_ELEMENT_TEXT_FIELD_EXPRESSION;
                                break;
                            case JR_MARKED_ELEMENT_AREA_DETAIL:
                                reportElementContainerName = JR_ELEMENT_TEXT_FIELD;
                                textContainerName = JR_ELEMENT_TEXT_FIELD_EXPRESSION;
                                break;
                            case JR_MARKED_ELEMENT_AREA_SUMMARY:
                                reportElementContainerName = JR_ELEMENT_TEXT_FIELD;
                                textContainerName = JR_ELEMENT_TEXT_FIELD_EXPRESSION;
                                break;

                            default:
                                break;
                        }
                        NodeList prevElementChildren =
                                getPreviousElementByName(newElement, reportElementContainerName).getChildNodes();
                        Node prevReportElement = getNodeFromNodeList(JR_ELEMENT_REPORT_ELEMENT, prevElementChildren);

                        int attPrevLeft = Integer.parseInt(prevReportElement.getAttributes()
                                .getNamedItem(JR_REPORT_ATTRIBUTE_LEFT).getTextContent());
                        int attPrevWidth = Integer.parseInt(prevReportElement.getAttributes()
                                .getNamedItem(JR_REPORT_ATTRIBUTE_WIDTH).getTextContent());
                        String attPrevKey = prevReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY)
                                .getTextContent();

                        Node newReportElement = getNodeFromNodeList(JR_ELEMENT_REPORT_ELEMENT, newElementChildren);
                        if (attPrevKey.startsWith(NEW_COLUMN)) {
                            int numOfColumn = Integer.parseInt(attPrevKey.replace(newColumnPrefix, EMPTY_STRING));
                            newReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY)
                                    .setTextContent(newColumnPrefix + String.valueOf(++numOfColumn));
                        }
                        newReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_LEFT)
                                .setTextContent(String.valueOf(attPrevLeft + attPrevWidth));
                        newReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_WIDTH)
                                .setTextContent(String.valueOf(GRADE_COLUMN_WIDTH));
                        Node newTextElement = getNodeFromNodeList(textContainerName, newElementChildren);

                        Node textElement = null;

                        switch (bind) {
                            case JR_MARKED_ELEMENT_AREA_HEADER:
                                newTextElement.setTextContent(wrapAsString(grade));
                                break;
                            case JR_MARKED_ELEMENT_AREA_DETAIL:

                                textElement = getNodeFromNodeList(JR_ELEMENT_TEXT_ELEMENT, newElementChildren);

                                ((Element) textElement).setAttribute(JR_REPORT_ATTRIBUTE_ALIGNMENT,
                                        JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT);

                                newTextElement.setTextContent(wrapAsVariable(grade));
                                ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_TIME,
                                        JR_REPORT_ATT_VALUE_GROUP);
                                ((Element) newElement).setAttribute(JR_REPORT_ATTRIBUTE_EVALUATION_GROUP,
                                        JR_REPORT_ATT_VALUE_ROW_NAME);
                                break;
                            case JR_MARKED_ELEMENT_AREA_SUMMARY:

                                textElement = getNodeFromNodeList(JR_ELEMENT_TEXT_ELEMENT, newElementChildren);

                                ((Element) textElement).setAttribute(JR_REPORT_ATTRIBUTE_ALIGNMENT,
                                        JR_REPORT_ATT_VALUE_ALIGNMENT_RIGHT);

                                newTextElement.setTextContent(wrapAsVariable(grade + JR_SUMMARY_VAR_POSTFIX));
                                break;

                            default:
                                break;
                        }

                        /*
                         * Shift headerShifted
                         */
                        Node shiftedReportElement = getNodeFromNodeList(JR_ELEMENT_REPORT_ELEMENT,
                                bindShiftedMap.get(bind).getChildNodes());
                        int attShiftedLeft = Integer.parseInt(shiftedReportElement.getAttributes()
                                .getNamedItem(JR_REPORT_ATTRIBUTE_LEFT).getTextContent());
                        shiftedReportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_LEFT)
                                .setTextContent(String.valueOf(attShiftedLeft + GRADE_COLUMN_WIDTH));
                    }
                }
            } catch (ParserConfigurationException | SAXException | IOException e) {
                e.printStackTrace();
            }
        }

        /**
         * Return true if the node has attribute key and the key contains passed keyType.
         *
         * @param node Node
         * @param keyType String
         * @return boolean
         */
        private boolean attributeKeyContains(Node node, String keyType) {
            boolean attributeKeyContains = false;
            if (node.getAttributes() != null && node.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY) != null) {
                attributeKeyContains =
                        node.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY).getTextContent().contains(keyType);
            }
            return attributeKeyContains;
        }

        /**
         * Return last node with passed node name from the node list.
         *
         * @param nodeName String
         * @param nodeList NodeList
         * @return Node
         */
        private Node getNodeFromNodeList(String nodeName, NodeList nodeList) {
            Node reportElement = null;
            for (int k = 0; k < nodeList.getLength(); k++) {
                if (nodeList.item(k).getNodeName().equals(nodeName)) {
                    reportElement = nodeList.item(k);
                }
            }

            return reportElement;
        }

        /**
         * Returns previous element that before passed node and with passed name.
         *
         * @param newNode Node
         * @param name String
         * @return Node
         */
        private Node getPreviousElementByName(Node newNode, String name) {
            Node currentNode = newNode.getPreviousSibling();

            while (!name.equals(currentNode.getNodeName())) {
                currentNode = currentNode.getPreviousSibling();
            }
            if (!name.equals(currentNode.getNodeName())) {
                currentNode = null;
            }
            return currentNode;
        }

        /**
         * Initialize map of nodes based on passed type and using passed list of elements.
         * We need determine concrete nodes that should be used in dynamically added format, e.g. to
         * determine
         * start position where we should add new nodes in the export format should be node
         * containing "Anchor" in
         * report element key, etc.
         *
         * @param reportElements NodeList
         * @param keyType String
         * @return Map<String, Node>
         */
        private Map<String, Node> initBindNodeMap(NodeList reportElements, String keyType) {
            Map<String, Node> bindNodeMap = new HashMap<String, Node>();
            for (int i = 0; i < reportElements.getLength(); i++) {
                Node reportElement = reportElements.item(i);
                if (attributeKeyContains(reportElement, keyType)) {
                    Node container = reportElement.getParentNode();
                    String bind = reportElement.getAttributes().getNamedItem(JR_REPORT_ATTRIBUTE_KEY).getTextContent()
                            .replace(keyType, EMPTY_STRING);

                    bindNodeMap.put(bind, container);
                }
            }
            return bindNodeMap;
        }

        /**
         * Initialize report formats.
         */
        private void initReportsFormat() {
            String formatPDF = (String) getParameter(INPUT_PARAM_FORMAT_ID_PDF);
            String formatCSV = (String) getParameter(INPUT_PARAM_FORMAT_ID_CSV);
            ToolJob job = this.getJob();
            switch (job.getInput().getFormat()) {
                case ToolInput.CSV_FORMAT:
                    this.setFormatId(formatCSV);
                    break;
                case ToolInput.HTML_FORMAT:
                    this.setFormatId(formatPDF);
                    break;
                case ToolInput.PDF_FORMAT:
                    this.setFormatId(formatPDF);
                    break;
                case ToolInput.XLS_FORMAT:
                    this.setFormatId(formatPDF);
                    break;
            }
        }

        /**
         * Initialize state report data.
         *
         * @throws X2BaseException exception
         */
        private void initStateReportData() throws X2BaseException {
            ArrayList<StateReportValidationError> m_initErrors = new ArrayList<StateReportValidationError>();
            Procedure procedure =
                    (Procedure) ToolManager.getToolForId(Tool.TYPE_PROCEDURE, SR_GRD_SUM_PROCEDURE_ID, getBroker());
            if (procedure == null) {
                throw new ToolRunException(
                        "Procedure with ID = " + SR_GRD_SUM_PROCEDURE_ID + " could not be found and is required");
            }
            /*
             * Since we are using the tool loader, we cannot compile the procedure separately from
             * the class compiled with the report
             * The new operator must be used to create the instance.
             */
            m_reportData = new GradesSumData();
            ToolSourceCode sourceCode = procedure.getSourceCode();
            if (sourceCode != null) {
                m_reportData.setInputDefinition(sourceCode.getInputDefinition());
            }
            m_reportData.setProcedureId(SR_GRD_SUM_PROCEDURE_ID);
            m_initErrors.addAll(m_reportData.loadDefinitions(SR_GRD_SUM_PROCEDURE_ID, getBroker()));

            getParameters().put(GradesSumData.PASSED_FORMAT_DEFINITIONS, getFormatDefinitions());
            getParameters().put(GradesSumData.PASSED_QUERY, getDetailQuery());

            // Initialize the report data object.
            m_reportData.setBroker(getBroker());
            m_reportData.setCurrentContext(getCurrentContext());
            m_reportData.setOrganization(getOrganization());
            m_reportData.setPrivilegeSet(getPrivilegeSet());
            m_reportData.setSchoolContext(isSchoolContext());
            m_reportData.setSchool(getSchool());
            m_reportData.setParameters(getParameters());
            m_reportData.setUser(getUser());
            m_reportData.initializeExport();
        }

        /**
         * Wrap passed text as text of jasper report format text field.
         *
         * @param text String
         * @return String
         */
        private String wrapAsString(String text) {
            return "\"" + text + "\"";
        }

        /**
         * Wrap passed name as variable of jasper report format.
         *
         * @param variableName String
         * @return String
         */
        private String wrapAsVariable(String variableName) {
            return "$V{" + variableName + "}";
        }
    }

    public static final String PASSED_QUERY = "detailQuery";
    public static final String PASSED_FORMAT_DEFINITIONS = "formatDefinitions";
    public static final String PASSED_STUDENT_BEAN_PATH = "studentBeanPath";

    ComparatorFactory.Comparator m_comparatorDates = null;
    ComparatorFactory.Comparator m_comparatorNumber = null;
    ComparatorFactory.Comparator m_comparatorLogical = null;

    Collection<ExportFormatDefinition> m_formatDefinitions = null;
    Map<String, Collection> m_formatFields = null;
    String m_studentBeanPath;

    /**
     * Returns fromatType code based on passed procedureId and calcParam. It's supposed that the
     * same
     * calcParams should be with the same formatType now.
     *
     * @param procedureId String
     * @param calcParam String
     * @return ExportFormatField.FormatTypeCode
     */
    protected ExportFormatField.FormatTypeCode getFormatType(String procedureId, String calcParam) {
        ExportFormatField.FormatTypeCode typeCode = null;

        int formatTypeOrdinal = 0;
        Collection<ExportFormatField> effCollection = m_formatFields.get(procedureId);

        for (ExportFormatField eff : effCollection) {
            if (eff.getCalcParam() != null && eff.getCalcParam().equals(calcParam)) {
                formatTypeOrdinal = eff.getFormatType();
                break;
            }
        }

        typeCode = ExportFormatField.FormatTypeCode.values()[formatTypeOrdinal];

        return typeCode;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        ComparatorFactory factory = new ComparatorFactory();
        m_comparatorDates = factory.getComparator(ExportFormatField.FormatTypeCode.DATE);
        m_comparatorNumber = factory.getComparator(ExportFormatField.FormatTypeCode.NUMBER);
        m_comparatorLogical = factory.getComparator(ExportFormatField.FormatTypeCode.LOGICAL);

        m_formatDefinitions = (Collection<ExportFormatDefinition>) getParameter(PASSED_FORMAT_DEFINITIONS);

        initFormatFields();

        m_studentBeanPath = (String) getParameter(PASSED_STUDENT_BEAN_PATH);

        setQuery((BeanQuery) getParameter(PASSED_QUERY));
        setEntityClass(GradesSumEntity.class);
    }

    /**
     * Initializes format fields.
     */
    private void initFormatFields() {
        Collection<String> efdProceduresId = new ArrayList<String>();
        for (ExportFormatDefinition formatDefinition : m_formatDefinitions) {
            efdProceduresId.add(formatDefinition.getProcedureId());
        }
        X2Criteria effCriteria = new X2Criteria();
        effCriteria.addIn(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER +
                ExportFormatDefinition.COL_PROCEDURE_ID, efdProceduresId);
        QueryByCriteria effQuery = new QueryByCriteria(ExportFormatField.class, effCriteria);
        m_formatFields = getBroker().getGroupedCollectionByQuery(effQuery, ExportFormatField.REL_DEFINITION +
                ModelProperty.PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID, 1);
    }

}
