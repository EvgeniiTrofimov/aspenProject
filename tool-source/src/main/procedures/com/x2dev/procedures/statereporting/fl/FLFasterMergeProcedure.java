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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.LocalResultHandler;
import com.follett.fsc.core.k12.tools.RemoteResultHandler;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.fl.FLFasterUtils.ToolInputProcessor;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.StudentTransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObjectRecord;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.zip.ZipException;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterMergeProcedure.
 */
public class FLFasterMergeProcedure extends ToolJavaSource {

    /**
     * The Class MergeToolJob.
     */
    public static class MergeToolJob extends ToolJob {
        private X2Broker m_broker;
        private Map<String, List<String>> m_actualRecords;

        /**
         * Instantiates a new merge tool job.
         *
         * @param tool Tool
         * @param userData UserDataContainer
         * @param resultHandler ResultHandler
         * @param locale Locale
         * @throws IOException Signals that an I/O exception has occurred.
         * @throws ClassNotFoundException exception
         * @throws InstantiationException exception
         * @throws IllegalAccessException exception
         * @throws X2BaseException exception
         */
        protected MergeToolJob(Tool tool, UserDataContainer userData, ResultHandler resultHandler, Locale locale)
                throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException,
                X2BaseException {
            super(tool, userData, resultHandler, locale);
        }

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         */
        public X2Broker getBroker() {
            return m_broker;
        }

        /**
         * Gets the actual rows.
         *
         * @return Map
         */
        public Map<String, List<String>> getActualRows() {
            return m_actualRecords;
        }

        /**
         * Sets the broker.
         *
         * @param broker void
         */
        public void setBroker(X2Broker broker) {
            m_broker = broker;
        }

        /**
         * Sets the actual records.
         *
         * @param actualRecords Map<String,List<String>>
         */
        public void setActualRecords(Map<String, List<String>> actualRecords) {
            m_actualRecords = actualRecords;
        }

        /**
         * Creates the.
         *
         * @param tool Tool
         * @param userData UserDataContainer
         * @param resultFolder File
         * @param downloadFileName String
         * @param remote boolean
         * @param locale Locale
         * @return MergeToolJob
         * @throws ClassNotFoundException exception
         * @throws InstantiationException exception
         * @throws IllegalAccessException exception
         * @throws IOException Signals that an I/O exception has occurred.
         * @throws X2BaseException exception
         */
        public static MergeToolJob create(Tool tool,
                                          UserDataContainer userData,
                                          File resultFolder,
                                          String downloadFileName,
                                          boolean remote,
                                          Locale locale)
                throws ClassNotFoundException, InstantiationException, IllegalAccessException, IOException,
                X2BaseException {
            MergeToolJob job = null;

            int format = userData.getToolInput().getFormat();

            ResultHandler resultHandler = null;
            if (remote) {
                resultHandler = new RemoteResultHandler(resultFolder, downloadFileName, format, tool);
            } else {
                resultHandler = new LocalResultHandler(resultFolder, downloadFileName, format, tool);
            }

            try {
                job = new MergeToolJob(tool, userData, resultHandler, locale);
            } catch (X2RuntimeException x2re) {
                throw x2re;
            } catch (ClassCastException cce) {
                /*
                 * Many procedures cannot be run directly or scheduled as jobs.
                 * Check to see if the tool job was not created because the procedure being run does
                 * not extend ToolJavaSource. If that is the case, throw the ClassCastException so
                 * that ToolRunAction can display an appropriate error message.
                 */
                if (cce.getMessage().contains(ToolJavaSource.class.getName())) {
                    throw cce;
                }
            }

            return job;
        }
    }

    /**
     * The Class RecordTypeMergeData.
     */
    public static abstract class RecordTypeMergeData extends ToolJavaSource {


        /**
         * The Class ValueAdjusterDate.
         */
        public static class ValueAdjusterDate extends ValueAdjuster {
            private SimpleDateFormat m_format = null;


            /**
             * Instantiates a new value adjuster date.
             *
             * @param format String
             */
            public ValueAdjusterDate(String format) {
                m_format = new SimpleDateFormat(format);
            }


            /**
             * Gets the adjusted value.
             *
             * @param value String
             * @return Object
             * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
             */
            @Override
            public Object getAdjustedValue(String value) {
                try {
                    return new PlainDate(m_format.parse(value));
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                return null;
            }

        }


        /**
         * The Class ValueAdjusterPgmDate.
         */
        public static class ValueAdjusterPgmDate extends ValueAdjuster {
            private SimpleDateFormat m_format = null;

            private static final SimpleDateFormat s_pgmDateFormat = new SimpleDateFormat("yyyy-MM-dd");


            /**
             * Instantiates a new value adjuster pgm date.
             *
             * @param dateFormat String
             */
            public ValueAdjusterPgmDate(String dateFormat) {
                m_format = new SimpleDateFormat(dateFormat);
            }


            /**
             * Gets the adjusted value.
             *
             * @param value String
             * @return Object
             * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
             */
            @Override
            public Object getAdjustedValue(String value) {
                try {
                    Date parsedDate = m_format.parse(value);
                    return s_pgmDateFormat.format(parsedDate);
                } catch (ParseException e) {
                    return null;
                }
            }
        }

        /**
         * The Class ValueAdjusterDateByAlias.
         */
        public static class ValueAdjusterDateByAlias extends ValueAdjuster {
            private ValueAdjuster m_customDateAdjuster = null;

            private static SimpleDateFormat s_toAliasDateFormat =
                    new SimpleDateFormat(DateAsStringConverter.STRING_DATE_FORMAT);

            /**
             * Instantiates a new value adjuster date by alias.
             *
             * @param customDateAdjuster ValueAdjuster
             */
            public ValueAdjusterDateByAlias(ValueAdjuster customDateAdjuster) {
                m_customDateAdjuster = customDateAdjuster;
            }

            /**
             * Gets the adjusted value.
             *
             * @param value String
             * @return Object
             * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
             */
            @Override
            public Object getAdjustedValue(String value) {
                Date adjustedValue = (Date) m_customDateAdjuster.getAdjustedValue(value);
                return s_toAliasDateFormat.format(adjustedValue);
            }
        }

        /**
         * The Class ValueAdjusterMulticode.
         */
        public static class ValueAdjusterMulticode extends ValueAdjuster {


            /**
             * Gets the adjusted value.
             *
             * @param value String
             * @return Object
             * @see com.x2dev.procedures.statereporting.fl.FLFasterMergeProcedure.ValueAdjuster#getAdjustedValue(java.lang.String)
             */
            @Override
            public Object getAdjustedValue(String value) {
                return value.replaceAll(".(?!$)", "$0, ");
            }

        }

        private Map<String, List<String>> m_actualRows = null;
        private Map<String, DataDictionary> m_asdDictionaries = new HashMap<>();
        private X2Broker m_broker = null;
        private String m_currentPlainRow = null;
        private List<String> m_currentResponseRecords = null;
        private Map<String, ExtendedDataDictionary> m_ddxById = new HashMap<>();
        private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();
        private DataDictionary m_dictionary = null;
        private FLFasterExportConfiguration m_exportConfig = null;
        private Map<String, Map<String, FieldInfo>> m_fieldInfosByPlainRow =
                new HashMap<String, Map<String, FieldInfo>>();
        private Map<String, DataDictionaryField> m_fieldsByAlias = null;
        private Map<String, Map<String, DataDictionaryField>> m_fieldsByBeanPath = null;
        private Map<String, String> m_fieldValues = new HashMap<>();
        private String m_importingPlainRow;
        private boolean m_isMatchChecked = false;
        private List<String> m_mergeableFieldNames = null;
        private LinkedList<String> m_messages = new LinkedList<>();
        private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;
        private X2BaseBean m_previouslyShownBean = null;
        private SisStudent m_student = null;
        private UserDataContainer m_userData = null;

        /**
         * Adds the message line.
         *
         * @param message String
         */
        protected final void addMessageLine(String message) {
            m_messages.add(message);
        }


        /**
         * Adds the message lines.
         *
         * @param messages String[]
         */
        protected final void addMessageLines(String[] messages) {
            for (String message : messages) {
                m_messages.add(message);
            }
        }


        /**
         * Before merge.
         *
         * @param fieldName String
         */
        protected void beforeMerge(String fieldName) {
            // override if needed
        }


        /**
         * Find field merge attributes by field name.
         *
         * @param fieldName String
         * @return FieldMergeAttributesInterface
         */
        protected FieldMergeAttributesInterface findFieldMergeAttributesByFieldName(String fieldName) {
            for (FieldMergeAttributesInterface attributes : getFieldMergeAttributes()) {
                if (attributes.getFieldName().equals(fieldName)) {
                    return attributes;
                }
            }
            return null;
        }


        /**
         * Find field merge attributes by field name.
         *
         * @param atributesArr FieldMergeAttributesInterface[]
         * @param fieldName String
         * @return FieldMergeAttributesInterface
         */
        protected static FieldMergeAttributesInterface findFieldMergeAttributesByFieldName(FieldMergeAttributesInterface[] atributesArr,
                                                                                           String fieldName) {
            for (FieldMergeAttributesInterface attributes : atributesArr) {
                if (attributes.getFieldName().equals(fieldName)) {
                    return attributes;
                }
            }
            return null;
        }


        /**
         * Gets the adjusted value.
         *
         * @param fieldName String
         * @return Object
         */
        protected final Object getAdjustedValue(String fieldName) {
            FieldMergeAttributesInterface mergeAttributes = findFieldMergeAttributesByFieldName(fieldName);
            if (mergeAttributes == null) {
                throw new RuntimeException(
                        "Please determine merge attributes to handle field " + fieldName);
            }
            ValueAdjuster adjuster = mergeAttributes.getValueAdjuster();
            String value = getImportingFieldInfo(fieldName).getValue();
            if (adjuster != null) {
                return adjuster.getAdjustedValue(value);
            }
            return value.trim();
        }


        /**
         * Gets the adjusted value.
         *
         * @param attributes FieldMergeAttributesInterface
         * @param fieldInfo FieldInfo
         * @return Object
         */
        protected static Object getAdjustedValue(FieldMergeAttributesInterface attributes, FieldInfo fieldInfo) {
            ValueAdjuster adjuster = attributes.getValueAdjuster();
            if (adjuster != null) {
                return adjuster.getAdjustedValue(fieldInfo.getValue());
            }
            return fieldInfo.getValue().trim();
        }


        /**
         * Gets the adjusted value.
         *
         * @param attributesArr FieldMergeAttributesInterface[]
         * @param fieldInfo FieldInfo
         * @return Object
         */
        protected static Object getAdjustedValue(FieldMergeAttributesInterface[] attributesArr, FieldInfo fieldInfo) {
            FieldMergeAttributesInterface mergeAttributes =
                    findFieldMergeAttributesByFieldName(attributesArr, fieldInfo.getFieldName());
            if (mergeAttributes == null) {
                throw new RuntimeException(
                        "Please determine merge attributes to handle field " + fieldInfo.getFieldName());
            }
            ValueAdjuster adjuster = mergeAttributes.getValueAdjuster();
            if (adjuster != null) {
                return adjuster.getAdjustedValue(fieldInfo.getValue());
            }
            return fieldInfo.getValue().trim();
        }


        /**
         * Gets the bean descriptor.
         *
         * @param fieldName String
         * @return String
         */
        protected String getBeanDescriptor(String fieldName) {
            return getClassMergeTo().getSimpleName();
        }


        /**
         * Gets the bean merge description.
         *
         * @param fieldName String
         * @return String
         */
        protected String getBeanMergeDescription(String fieldName) {
            StringBuilder message = new StringBuilder();
            X2BaseBean beanMergeTo = getBeanMergeTo(fieldName);
            message.append("\t\t");
            if (beanMergeTo == null) {
                return getBeanDescriptor(fieldName) + " cannot be created";
            }
            if (beanMergeTo.getOid() == null) {
                message.append("New " + getBeanDescriptor(fieldName) + " was created");
            } else {
                message.append(getBeanDescriptor(fieldName) + " was changed:");
            }
            return message.toString();
        }


        /**
         * Gets the bean merge to.
         *
         * @param fieldName String
         * @return X 2 base bean
         */
        protected abstract X2BaseBean getBeanMergeTo(String fieldName);


        /**
         * Gets the broker.
         *
         * @return X 2 broker
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
         */
        @Override
        protected final X2Broker getBroker() {
            return m_broker;
        }


        /**
         * Gets the class merge to.
         *
         * @return Class
         */
        protected abstract Class<?> getClassMergeTo();


        /**
         * Gets the current plain row.
         *
         * @return String
         */
        protected String getCurrentPlainRow() {
            if (m_currentPlainRow == null && !m_isMatchChecked) {
                List<String> recordTypeResponsePlainRows = getCurrentRecordTypeResponseRecords();

                if (recordTypeResponsePlainRows != null) {
                    if (getFieldNamesToIdentifyRow().isEmpty()) {
                        Iterator<String> recordsIterator = recordTypeResponsePlainRows.iterator();
                        if (recordsIterator.hasNext()) {
                            return recordsIterator.next();
                        }
                        return null;
                    }
                    for (String plainRow : recordTypeResponsePlainRows) {
                        Map<String, FieldInfo> fieldInfosForPlainRow = getFieldInfosForPlainRow(plainRow);
                        boolean isMatched = true;
                        for (String fieldName : getFieldNamesToIdentifyRow()) {

                            FieldInfo importingFieldInfo = getImportingFieldInfos().get(fieldName);
                            FieldInfo currentFieldInfo = fieldInfosForPlainRow.get(fieldName);

                            if (!importingFieldInfo.getValue().equals(currentFieldInfo.getValue())) {
                                isMatched = false;
                                break;
                            }
                        }
                        if (isMatched) {
                            m_currentPlainRow = plainRow;
                        }
                    }
                    m_isMatchChecked = true;
                }
            }

            return m_currentPlainRow;
        }


        /**
         * Gets the date format.
         *
         * @param dateFormatCode String
         * @return String
         */
        protected static String getDateFormat(String dateFormatCode) {
            String dateFormatToParse = null;
            switch (dateFormatCode) {
                case DATE_FORMAT_CODE_CM:
                    dateFormatToParse = DATE_FORMAT_CM;
                    break;
                case DATE_FORMAT_CODE_CY:
                    dateFormatToParse = DATE_FORMAT_CY;
                    break;
                case DATE_FORMAT_CODE_D8:
                    dateFormatToParse = DATE_FORMAT_D8;
                    break;
                case DATE_FORMAT_CODE_DB:
                    dateFormatToParse = DATE_FORMAT_DB;
                    break;

                default:
                    break;
            }
            return dateFormatToParse;
        }


        /**
         * Gets the dictionary.
         *
         * @return Data dictionary
         */
        protected final DataDictionary getDictionary() {
            if (m_dictionary == null) {
                m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            }
            return m_dictionary;
        }


        /**
         * Gets the dictionary by id.
         *
         * @param id String
         * @return Data dictionary
         */
        protected final DataDictionary getDictionaryById(String id) {
            if (id == null) {
                return getDictionary();
            }

            if (getClassMergeTo() == StudentAssessment.class) {
                return getAsmDataDictionary(id);
            }

            DataDictionary dictionary = m_dictionariesById.get(id);
            if (dictionary == null) {
                ExtendedDataDictionary ddx = getExtendedDataDictionaryById(id);
                dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
                m_dictionariesById.put(id, dictionary);
            }
            return dictionary;
        }


        /**
         * Gets the export config.
         *
         * @return FL faster export configuration
         */
        protected final FLFasterExportConfiguration getExportConfig() {
            return m_exportConfig;
        }


        /**
         * Gets the extended data dictionary by id.
         *
         * @param ddxId String
         * @return Extended data dictionary
         */
        protected ExtendedDataDictionary getExtendedDataDictionaryById(String ddxId) {
            ExtendedDataDictionary extendedDataDictionary = m_ddxById.get(ddxId);
            if (extendedDataDictionary == null) {
                X2Criteria ddxCriteria = new X2Criteria();

                ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);

                QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
                extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
                m_ddxById.put(ddxId, extendedDataDictionary);
            }

            return extendedDataDictionary;
        }


        /**
         * Gets the field by alias.
         *
         * @param alias String
         * @param dataDictionary DataDictionary
         * @return Data dictionary field
         */
        protected final DataDictionaryField getFieldByAlias(String alias, DataDictionary dataDictionary) {
            if (m_fieldsByAlias == null) {
                m_fieldsByAlias = new HashMap<>();
            }
            if (m_fieldsByAlias.get(alias) == null) {
                DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(alias);
                if (field != null) {
                    m_fieldsByAlias.put(alias, field);
                }
            }
            return m_fieldsByAlias.get(alias);
        }


        /**
         * Gets the field by bean path.
         *
         * @param clazz Class
         * @param beanPath String
         * @param dataDictionary DataDictionary
         * @return Data dictionary field
         */
        protected final DataDictionaryField getFieldByBeanPath(Class clazz,
                                                               String beanPath,
                                                               DataDictionary dataDictionary) {
            if (m_fieldsByBeanPath == null) {
                m_fieldsByBeanPath = new HashMap<>();
            }
            Map<String, DataDictionaryField> fields = m_fieldsByBeanPath.get(clazz.getName());
            if (fields == null) {
                fields = new HashMap<>();
                m_fieldsByBeanPath.put(clazz.getName(), fields);
            }

            if (fields.get(beanPath) == null) {
                DataDictionaryField field = dataDictionary.findDataDictionaryField(clazz.getName(), beanPath);
                if (field != null) {
                    fields.put(beanPath, field);
                }
            }
            return fields.get(beanPath);
        }


        /**
         * Gets the field info.
         *
         * @param fieldName String
         * @param plainRow String
         * @param beanPath String
         * @param ddxId String
         * @return Field info
         */
        protected final FieldInfo getFieldInfo(String fieldName, String plainRow, String beanPath, String ddxId) {
            DataDictionary dictionary = getDictionaryById(ddxId);
            FieldInfo fieldInfo = new FieldInfo(getExportConfig(), plainRow, fieldName, getClassMergeTo(),
                    beanPath, dictionary);
            return fieldInfo;
        }

        /**
         * Gets the field info.
         *
         * @param fieldName String
         * @param plainRow String
         * @return Field info
         */
        protected final FieldInfo getFieldInfo(String fieldName, String plainRow) {
            return getFieldInfosForPlainRow(plainRow).get(fieldName);
        }

        /**
         * Gets the field infos for plain row.
         *
         * @param plainRow String
         * @return Map
         */
        protected final Map<String, FieldInfo> getFieldInfosForPlainRow(String plainRow) {
            Map<String, FieldInfo> fieldInfos = m_fieldInfosByPlainRow.get(plainRow);
            if (fieldInfos == null) {
                fieldInfos = new HashMap<>();
                m_fieldInfosByPlainRow.put(plainRow, fieldInfos);

                for (String fieldName : getMergeableFieldNames()) {
                    FieldMerger merger = findFieldMergeAttributesByFieldName(fieldName).getMerger();
                    fieldInfos.put(fieldName,
                            getFieldInfo(fieldName, plainRow, merger.getBeanPath(), merger.getExtendedDictionaryId()));
                }
            }

            return fieldInfos;
        }


        /**
         * Gets the field merge attributes.
         *
         * @return Field merge attributes interface[]
         */
        protected abstract FieldMergeAttributesInterface[] getFieldMergeAttributes();


        /**
         * Gets the field merger.
         *
         * @param fieldName String
         * @return Field merger
         */
        protected final FieldMerger getFieldMerger(String fieldName) {
            FieldMergeAttributesInterface attributes = findFieldMergeAttributesByFieldName(fieldName);
            if (attributes != null) {
                return findFieldMergeAttributesByFieldName(fieldName).getMerger();
            }
            return null;
        }


        /**
         * Gets the field names to identify row.
         *
         * @return List
         */
        protected List<String> getFieldNamesToIdentifyRow() {
            return Collections.EMPTY_LIST;
        }


        /**
         * Gets the field value.
         *
         * @param bean X2BaseBean
         * @param field DataDictionaryField
         * @return Object
         * @throws X2BaseException exception
         */
        protected final Object getFieldValue(X2BaseBean bean, DataDictionaryField field) throws X2BaseException {
            Object value = null;
            if (bean != null && field != null) {
                if (field.getDataTable().getClassName().equals(bean.getClass().getName())) {
                    value = WebUtils.getProperty(bean, field.getJavaName());

                    if (value instanceof String) {
                        if (field.isString()) {
                            String format = WebUtils.generateFormat(field,
                                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()));
                            Converter baseConverter = ConverterFactory.getConverterForClass(
                                    field.getEffectiveJavaType(),
                                    LocalizationCache.getPrimarySystemLocale(getBroker().getPersistenceKey()),
                                    field.isString(), format);
                            if (baseConverter instanceof SystemStringConverter) {
                                SystemStringConverter converter = ((SystemStringConverter) baseConverter);
                                if (converter != null) {
                                    value = converter.parseSystemString((String) value);
                                }
                            }
                        }
                    }
                    if (value instanceof String && !StringUtils.isEmpty((String) value) && field.hasReferenceTable()) {
                        if (field.getLength() > 50) { // process as D field with multiple values
                            List<String> codes = Arrays.asList(((String) value).split(","));
                            StringBuilder converted = new StringBuilder();
                            for (String code : codes) {
                                code = code.trim();
                                String stateCode = lookupReferenceCodeByRefTbl(field.getReferenceTableOid(), code,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                if (!StringUtils.isEmpty(stateCode)) {
                                    if (converted.length() > 0) {
                                        converted.append(",");
                                    }
                                    converted.append(stateCode);
                                }
                            }
                            value = converted.toString();
                        } else {
                            value = lookupReferenceCodeByRefTbl(field.getReferenceTableOid(), (String) value,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        }
                    }
                } else {
                    throw new X2RuntimeException(new UnsupportedOperationException(
                            "Bean class is " + bean.getClass() + " and field class is "
                                    + field.getDataTable().getClass()));
                }
            }
            return value;
        }


        /**
         * Gets the importing field info.
         *
         * @param fieldName String
         * @return Field info
         */
        protected FieldInfo getImportingFieldInfo(String fieldName) {
            return getFieldInfosForPlainRow(getImportingPlainRow()).get(fieldName);
        }

        /**
         * Gets the importing field value.
         *
         * @param fieldName String
         * @return String
         */
        public String getImportingFieldValue(String fieldName) {
            String fieldValue = m_fieldValues.get(fieldName);
            if (fieldValue == null) {
                FieldInfo fieldInfo = getFieldInfo(fieldName, getImportingPlainRow(), null, null);
                fieldValue = fieldInfo.getValue();
                m_fieldValues.put(fieldName, fieldValue);
            }
            return fieldValue;
        }

        /**
         * Gets the importing plain row.
         *
         * @return String
         */
        protected final String getImportingPlainRow() {
            return m_importingPlainRow;
        }


        /**
         * Gets the java name by alias.
         *
         * @param alias String
         * @param dataDictionary DataDictionary
         * @return String
         */
        protected final String getJavaNameByAlias(String alias, DataDictionary dataDictionary) {
            DataDictionaryField field = getFieldByAlias(alias, dataDictionary);
            if (field != null) {
                return field.getJavaName();
            }
            return null;
        }


        /**
         * Gets the mergeable field names.
         *
         * @return List
         */
        protected final List<String> getMergeableFieldNames() {
            if (m_mergeableFieldNames == null) {
                m_mergeableFieldNames = new ArrayList<String>();
                String recordType = TransferObjectHelper.getRecordType(m_importingPlainRow);
                List<String> fieldNames = getExportConfig().getColumnNamesByRecordType(recordType);
                for (String fieldName : fieldNames) {
                    FieldMerger merger = getFieldMerger(fieldName);
                    if (merger != null) {
                        m_mergeableFieldNames.add(fieldName);
                    }
                }
            }
            return m_mergeableFieldNames;
        }


        /**
         * Gets the reference codes.
         *
         * @param referenceTableOid String
         * @return Map
         */
        protected final Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
            Map<String, ReferenceCode> codeMap = null;
            if (m_refTableMap == null) {
                m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
            }

            if (m_refTableMap.containsKey(referenceTableOid)) {
                codeMap = m_refTableMap.get(referenceTableOid);
            } else {
                codeMap = new HashMap<String, ReferenceCode>();
                ReferenceTable refTable =
                        (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, referenceTableOid);
                if (refTable != null) {
                    codeMap = refTable.getCodeMap(getBroker());
                }
                m_refTableMap.put(referenceTableOid, codeMap);
            }

            return codeMap;
        }


        /**
         * Gets the student.
         *
         * @return Sis student
         */
        protected final SisStudent getStudent() {
            return m_student;
        }


        /**
         * Gets the user data.
         *
         * @return User data container
         */
        protected final UserDataContainer getUserData() {
            return m_userData;
        }


        /**
         * Initialize.
         *
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
         */
        @Override
        protected void initialize() throws X2BaseException {
            super.initialize();
            m_exportConfig =
                    new FLFasterExportConfiguration(getCurrentContext(), getBroker());
            m_importingPlainRow = (String) getParameter(INPUT_PARAM_PLAIN_ROW);
            String studentOid = (String) getParameter(INPUT_PARAM_STUDENT_OID);
            m_student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);
        }


        /**
         * Initialize transients.
         *
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initializeTransients()
         */
        @Override
        protected void initializeTransients() {
            super.initializeTransients();
            ToolJob job = getJob();
            X2Broker broker = (X2Broker) callDeclaredMethod(job, "getBroker");
            Map<String, List<String>> actualRows = (Map<String, List<String>>) callDeclaredMethod(job, "getActualRows");
            setBroker(broker);
            setActualRows(actualRows);
        }

        /**
         * Lookup reference code by ref tbl.
         *
         * @param referenceTableOid String
         * @param value String
         * @param referenceMap int
         * @return String
         */
        protected final String lookupReferenceCodeByRefTbl(String referenceTableOid, String value, int referenceMap) {
            String returnValue = null;
            Map<String, ReferenceCode> refCodes = getReferenceCodes(referenceTableOid);
            ReferenceCode code = refCodes.get(value);
            if (code != null) {
                if (referenceMap == ExportFormatField.ReferenceMapTypeCode.STATE.ordinal()) {
                    returnValue = code.getStateCode();
                } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.FEDERAL.ordinal()) {
                    returnValue = code.getFederalCode();
                } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal()) {
                    returnValue = code.getLocalCode();
                } else if (referenceMap == ExportFormatField.ReferenceMapTypeCode.SYSTEM.ordinal()) {
                    returnValue = code.getSystemCode();
                }
            }

            return returnValue;
        }


        /**
         * Merge field.
         *
         * @param merger FieldMerger
         * @param bean X2BaseBean
         * @param fieldInfo FieldInfo
         * @return List
         */
        protected List<ValidationError> mergeField(FieldMerger merger,
                                                   X2BaseBean bean,
                                                   FieldInfo fieldInfo) {
            merger.merge();
            List<ValidationError> mergeValidationErrors = persistChanges(bean, fieldInfo);
            if (mergeValidationErrors.size() > 0) {
                for (ValidationError error : mergeValidationErrors) {
                    addMessageLine(error.toString());
                }
                getJob().setStatus(ToolJob.STATUS_ABORT);
            }
            return mergeValidationErrors;
        }


        /**
         * Merge fields.
         */
        protected final void mergeFields() {
            for (String fieldName : getMergeableFieldNames()) {
                FieldMerger merger = getFieldMerger(fieldName);
                try {
                    merger.initialize(fieldName, this);
                    if (merger.isMergeNeeded()) {
                        beforeMerge(fieldName);
                        X2BaseBean beanMergeTo = getBeanMergeTo(fieldName);
                        FieldInfo importingFieldInfo = getImportingFieldInfo(fieldName);
                        showMergeDescription(merger, beanMergeTo, importingFieldInfo);
                        mergeField(merger, beanMergeTo, importingFieldInfo);
                    }
                } finally {
                    merger.clearMergeData();
                }
            }
        }


        /**
         * Parses the date.
         *
         * @param date String
         * @param dateFormat String
         * @return PlainDate
         */
        protected static PlainDate parseDate(String date, String dateFormat) {
            SimpleDateFormat format = new SimpleDateFormat(dateFormat);
            format.setLenient(false);

            try {
                return new PlainDate(format.parse(date));
            } catch (ParseException e) {
                throw new X2RuntimeException();
            }
        }


        /**
         * Persist changes.
         *
         * @param bean X2BaseBean
         * @param fieldInfo FieldInfo
         * @return List
         */
        protected final List<ValidationError> persistChanges(X2BaseBean bean,
                                                             FieldInfo fieldInfo) {
            List<ValidationError> errors = new ArrayList<>();
            X2BaseBean beanToSave = null;
            if (fieldInfo.getBeanPath() != null && fieldInfo.getBeanPath().contains(".")) {
                String relatedBeanPath =
                        fieldInfo.getBeanPath().substring(0, fieldInfo.getBeanPath().lastIndexOf("."));
                beanToSave = (X2BaseBean) bean.getFieldValueByBeanPath(relatedBeanPath);
            } else {
                beanToSave = bean;
            }
            getBroker().saveBeanForced(beanToSave);
            return errors;
        }


        /**
         * Run.
         *
         * @throws Exception exception
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
         */
        @Override
        protected final void run() throws Exception {
            try {
                mergeFields();
            } finally {
                printMessages();
            }
        }


        /**
         * Save state.
         *
         * @param userData UserDataContainer
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
         */
        @Override
        protected final void saveState(UserDataContainer userData) throws X2BaseException {
            super.saveState(userData);

            m_userData = userData;
        }

        /**
         * Sets the actual rows.
         *
         * @param actualRecords Map<String,List<String>>
         */
        protected void setActualRows(Map<String, List<String>> actualRecords) {
            m_actualRows = actualRecords;
        }

        /**
         * Sets the broker.
         *
         * @param broker void
         */
        protected void setBroker(X2Broker broker) {
            m_broker = broker;
        }

        /**
         * Show merge description.
         *
         * @param merger FieldMerger
         * @param beanMergeTo X2BaseBean
         * @param importingFieldInfo FieldInfo
         */
        protected void showMergeDescription(FieldMerger merger, X2BaseBean beanMergeTo, FieldInfo importingFieldInfo) {
            if (!isBeanMergeDescriptionShown(beanMergeTo)) {
                addMessageLine(getBeanMergeDescription(importingFieldInfo.getFieldName()));
                m_previouslyShownBean = beanMergeTo;
            }
            if (!merger.getFieldsToMerge().isEmpty()) {
                addMessageLines(merger.getFieldMergeDescription());
            }
        }


        /**
         * Gets the asm data dictionary.
         *
         * @param asdId String
         * @return Data dictionary
         */
        private DataDictionary getAsmDataDictionary(String asdId) {
            if (asdId == null) {
                return getDictionary();
            }
            if (m_asdDictionaries.get(asdId) == null) {
                X2Criteria asdCriteria = new X2Criteria();
                asdCriteria.addEqualTo(AssessmentDefinition.COL_ID, asdId);
                QueryByCriteria asdQuery = new QueryByCriteria(AssessmentDefinition.class, asdCriteria);
                AssessmentDefinition asd = (AssessmentDefinition) getBroker().getBeanByQuery(asdQuery);
                if (asd == null) {
                    throw new X2RuntimeException();
                }
                DataDictionary dictionary = DataDictionary.getDistrictDictionary(asd, getBroker().getPersistenceKey());
                m_asdDictionaries.put(asdId, dictionary);
            }
            return m_asdDictionaries.get(asdId);
        }

        /**
         * Gets the current record type response records.
         *
         * @return List
         */
        private List<String> getCurrentRecordTypeResponseRecords() {
            if (m_currentResponseRecords == null) {
                String recordType = TransferObjectHelper.getRecordType(getImportingPlainRow());
                try {
                    m_currentResponseRecords = m_actualRows.get(recordType);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            return m_currentResponseRecords;
        }


        /**
         * Gets the importing field infos.
         *
         * @return Map
         */
        private Map<String, FieldInfo> getImportingFieldInfos() {
            return getFieldInfosForPlainRow(m_importingPlainRow);
        }


        /**
         * Checks if is bean merge description shown.
         *
         * @param bean X2BaseBean
         * @return true, if is bean merge description shown
         */
        private boolean isBeanMergeDescriptionShown(X2BaseBean bean) {
            return bean == null || bean.equals(m_previouslyShownBean);
        }


        /**
         * Prints the messages.
         *
         * @throws IOException Signals that an I/O exception has occurred.
         */
        private void printMessages() throws IOException {
            for (String message : m_messages) {
                PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
                stream.print(message + "\n");
            }
        }
    }


    /**
     * The Class FieldMerger.
     */
    public static class FieldMerger {
        private String m_beanPath = null;
        private String m_extendedDictionaryId = null;
        private String m_fieldName = null;
        private Map<String, Object> m_fieldsToMerge = new HashMap<>();
        private RecordTypeMergeData m_mergeData = null;


        /**
         * Instantiates a new field merger.
         */
        public FieldMerger() {}


        /**
         * Instantiates a new field merger.
         *
         * @param beanPath String
         */
        public FieldMerger(String beanPath) {
            m_beanPath = beanPath;
        }


        /**
         * Instantiates a new field merger.
         *
         * @param extendedDataDictionaryId String
         * @param beanPath String
         */
        public FieldMerger(String extendedDataDictionaryId, String beanPath) {
            m_extendedDictionaryId = extendedDataDictionaryId;
            m_beanPath = beanPath;
        }



        /**
         * Adds the field to merge.
         *
         * @param javaName String
         * @param value Object
         */
        public final void addFieldToMerge(String javaName, Object value) {
            if (m_fieldsToMerge == null) {
                m_fieldsToMerge = new HashMap<>();
            }
            m_fieldsToMerge.put(javaName, value);
        }

        /**
         * Clear merge data.
         */
        public final void clearMergeData() {
            m_fieldName = null;
            m_mergeData = null;
            m_fieldsToMerge = null;
        }


        /**
         * Gets the bean path.
         *
         * @return String
         */
        public String getBeanPath() {
            return m_beanPath;
        }


        /**
         * Gets the extended dictionary id.
         *
         * @return String
         */
        public String getExtendedDictionaryId() {
            return m_extendedDictionaryId;
        }


        /**
         * Gets the field merge description.
         *
         * @return String[]
         */
        public String[] getFieldMergeDescription() {
            X2BaseBean bean = m_mergeData.getBeanMergeTo(m_fieldName);

            List<String> messages = new ArrayList<String>();
            if (m_fieldsToMerge.size() > 0) {
                for (Entry<String, Object> entry : m_fieldsToMerge.entrySet()) {
                    StringBuilder message = new StringBuilder();
                    message.append("\t\t\t");
                    String beanPath = entry.getKey();
                    Object oldValue = bean.getFieldValueByBeanPath(beanPath);
                    Object newValue = entry.getValue();
                    X2BaseBean beanToMerge = null;
                    String beanPathToMerge = null;
                    if (beanPath.contains(".")) {
                        String relatedBeanPath =
                                beanPath.substring(0, beanPath.lastIndexOf("."));
                        beanToMerge = (X2BaseBean) bean.getFieldValueByBeanPath(relatedBeanPath);

                        String[] paths = beanPath.split("[.]");
                        beanPathToMerge = paths[paths.length - 1];
                    } else {
                        beanToMerge = bean;
                        beanPathToMerge = beanPath;
                    }

                    FieldInfo fieldInfo = m_mergeData.getImportingFieldInfo(m_fieldName);
                    DataDictionaryField field =
                            fieldInfo.getDictionary().findDataDictionaryField(beanToMerge.getClass().getName(),
                                    beanPathToMerge);
                    String fieldName = field.getUserLongName();
                    String alias = StringUtils.isEmpty(field.getAlias()) ? "" : " (" + field.getAlias() + ")";
                    message.append("Field '" + fieldName + "'" + alias + ": old value '" + oldValue + "', new value '"
                            + newValue + "'");
                    messages.add(message.toString());
                }
            }
            return messages.toArray(new String[0]);
        }


        /**
         * Gets the fields to merge.
         *
         * @return Map
         */
        public Map<String, Object> getFieldsToMerge() {
            return m_fieldsToMerge;
        }


        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Gets the merge data.
         *
         * @return Record type merge data
         */
        public RecordTypeMergeData getMergeData() {
            return m_mergeData;
        }


        /**
         * Initialize.
         *
         * @param fieldName String
         * @param mergeData RecordTypeMergeData
         */
        public void initialize(String fieldName, RecordTypeMergeData mergeData) {
            m_mergeData = mergeData;
            m_fieldName = fieldName;

            if (isMergeNeeded()) {
                initializeFieldsToMerge();
            }
        }



        /**
         * Checks if is merge needed.
         *
         * @return true, if is merge needed
         */
        public boolean isMergeNeeded() {
            FieldInfo importingFieldInfo = m_mergeData.getImportingFieldInfo(m_fieldName);
            FieldMergeAttributesInterface mergeAttributes =
                    m_mergeData.findFieldMergeAttributesByFieldName(m_fieldName);

            X2BaseBean bean = m_mergeData.getBeanMergeTo(m_fieldName);
            if (bean == null) {
                return false;
            }

            String currentPlainRow = m_mergeData.getCurrentPlainRow();
            FieldInfo currentFieldInfo = null;

            if (currentPlainRow != null) {
                currentFieldInfo = m_mergeData.getFieldInfo(m_fieldName, currentPlainRow);
            }

            if (currentFieldInfo != null) {
                String trimmedCurrentValue = currentFieldInfo.getValue().trim();
                String trimmedImportingValue = importingFieldInfo.getValue().trim();
                return (StringUtils.isEmpty(trimmedCurrentValue) || trimmedCurrentValue.matches(PATTERN_N)
                        || trimmedCurrentValue.matches(PATTERN_Z_FILLED)
                        || trimmedCurrentValue.matches(PATTERN_ZEROS_FILLED))
                        &&
                        (!StringUtils.isEmpty(trimmedImportingValue) && !trimmedImportingValue.matches(PATTERN_N)
                                && !trimmedImportingValue.matches(PATTERN_Z_FILLED)
                                && !trimmedImportingValue.matches(PATTERN_ZEROS_FILLED))
                        &&
                        !currentFieldInfo.getValue().equals(importingFieldInfo.getValue());
            }

            String beanPath = importingFieldInfo.getBeanPath();
            if (beanPath == null) {
                AppGlobals.getLog().log(Level.SEVERE, "null bean path," + importingFieldInfo.getFieldName());
            }
            Object currentValue = bean.getFieldValueByBeanPath(beanPath);
            Object importingValue = importingFieldInfo.getValue();

            String importingTrimmed = importingValue.toString().trim();
            if (!StringUtils.isEmpty(importingTrimmed)
                    && !importingTrimmed.matches(PATTERN_N)
                    && !importingTrimmed.matches(PATTERN_Z_FILLED)
                    && !importingTrimmed.matches(PATTERN_ZEROS_FILLED)) {
                Object importingAdjustedValue =
                        RecordTypeMergeData.getAdjustedValue(mergeAttributes, importingFieldInfo);
                if (importingAdjustedValue != null) {
                    return (currentValue == null || StringUtils.isEmpty(currentValue.toString())
                            || currentValue.toString().matches(PATTERN_N)
                            || currentValue.toString().matches(PATTERN_Z_FILLED)
                            || currentValue.toString().matches(PATTERN_ZEROS_FILLED))
                            && !importingAdjustedValue.equals(currentValue);
                }
            }

            return false;
        }


        /**
         * Merge.
         */
        public void merge() {
            X2BaseBean bean = m_mergeData.getBeanMergeTo(m_fieldName);
            if (bean != null) {
                for (Entry<String, Object> fieldToMerge : m_fieldsToMerge.entrySet()) {
                    bean.setFieldValueByBeanPath(fieldToMerge.getKey(), fieldToMerge.getValue());
                }
            }
        }

        /**
         * Initialize fields to merge.
         */
        public final void initializeFieldsToMerge() {
            m_fieldsToMerge = new HashMap<>();
            FieldInfo importingFieldInfo = getMergeData().getImportingFieldInfo(getFieldName());
            String beanPath = importingFieldInfo.getBeanPath();
            if (!StringUtils.isEmpty(beanPath)) {
                addFieldToMerge(beanPath,
                        getMergeData().getAdjustedValue(getFieldName()));
            }
        }
    }


    /**
     * The Interface FieldMergeAttributesInterface.
     */
    public interface FieldMergeAttributesInterface {


        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName();


        /**
         * Gets the merger.
         *
         * @return Field merger
         */
        public FieldMerger getMerger();


        /**
         * Gets the value adjuster.
         *
         * @return Value adjuster
         */
        public ValueAdjuster getValueAdjuster();
    }


    /**
     * The Class ValueAdjuster.
     */
    public static class ValueAdjuster {


        /**
         * Gets the adjusted value.
         *
         * @param value String
         * @return Object
         */
        public Object getAdjustedValue(String value) {
            return value;
        }
    }

    public static final String PATTERN_ZEROS_FILLED = "^0+$";
    public static final String PATTERN_Z_FILLED = "^Z+$";
    public static final String PATTERN_N = "^N$";

    private static final String DATE_FORMAT_CODE_CM = "CM";
    private static final String DATE_FORMAT_CODE_CY = "CY";
    private static final String DATE_FORMAT_CODE_D8 = "D8";
    private static final String DATE_FORMAT_CODE_DB = "DB";

    private static final String DATE_FORMAT_CM = "yyyyMM";
    private static final String DATE_FORMAT_CY = "yyyy";
    private static final String DATE_FORMAT_D8 = "yyyyMMdd";
    private static final String DATE_FORMAT_DB = "MMddyyyy";

    private static final String INPUT_PARAM_OMIT_HEADER_RECORD = "omitHeaderRecord";
    private static final String INPUT_PARAM_PERSIST_CHANGES = "persistChanges";
    private static final String INPUT_PARAM_PLAIN_ROW = "plainRow";
    private static final String INPUT_PARAM_RECORDS_TYPE = "recordsType";
    private static final String INPUT_PARAM_STUDENT_OID = "studentOid";
    private static final String INPUT_PARAM_STUDENT_TRANSFERS = "studentTransferOids";
    private static final String INPUT_PARAM_TRANSFER_TYPE = "transferType";

    private static final int JOB_STATUS_PROCEDURE_NOT_FOUND = 99999;

    private static final String PATTERN_MERGE_PROCEDURE_ID = "FL-FST-MERGE-#RECORD_TYPE#%";
    private static final String PATTERN_RECORD_TYPE = "#RECORD_TYPE#";

    private static Map<String, List<String>> s_currentPlainRows = null;
    private List<String> s_excludedRecords =
            Arrays.asList(TransferObjectRecord.RECORD_TYPE_00, TransferObjectRecord.RECORD_TYPE_99);

    private X2Broker m_broker = null;
    private TransferObjectHelper m_helper = null;
    private boolean m_persistChanges;
    private Collection<StudentTransferObject> m_studentTransfers;
    private UserDataContainer m_userData = null;

    /**
     * Call declared method.
     *
     * @param object Object
     * @param methodName String
     * @return Object
     */
    public static Object callDeclaredMethod(Object object, String methodName) {
        try {
            Method method = object.getClass().getDeclaredMethod(methodName);
            method.setAccessible(true);
            return method.invoke(object);
        } catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException e) {
            e.printStackTrace();
        }
        return null;
    
    }

    /**
     * Clear plain rows.
     */
    public void clearPlainRows() {
        s_currentPlainRows = null;
    }

    /**
     * Creates the merge tool job.
     *
     * @param procedure Procedure
     * @param parameters Map<String,Object>
     * @param userData UserDataContainer
     * @param broker X2Broker
     * @param locale Locale
     * @return MergeToolJob
     * @throws Exception exception
     */
    public static MergeToolJob createMergeToolJob(Procedure procedure,
                                                  Map<String, Object> parameters,
                                                  UserDataContainer userData,
                                                  X2Broker broker,
                                                  Locale locale)
            throws Exception {
        ToolInput exportInput =
                ToolInputProcessor.restoreToolInput(procedure, true, userData, broker, locale);

        for (Entry<String, Object> parameter : parameters.entrySet()) {
            exportInput.setParameterAsString(parameter.getKey(), parameter.getValue().toString());
        }

        userData.setToolInput(exportInput);

        File tempFolderRoot = AppGlobals.getRootTemporaryFolder();
        File tempFolder = FolderUtils.createUniqueFolder(tempFolderRoot);
        return MergeToolJob.create(procedure, userData, tempFolder, procedure.getName(), false, locale);
    }


    /**
     * Gets the rows by type.
     *
     * @param recordType String
     * @return List
     */
    public List<String> getRowsByType(String recordType) {
        return s_currentPlainRows.get(recordType);
    }


    /**
     * Initialize current plain rows.
     *
     * @param studentOid String
     * @param recordsType String
     * @param userData UserDataContainer
     * @param broker X2Broker
     * @param locale Locale
     */
    public void initializeCurrentPlainRows(String studentOid,
                                           String recordsType,
                                           UserDataContainer userData,
                                           X2Broker broker,
                                           Locale locale) {
        try {
            Map<String, String> parameters = new HashMap<>();
            parameters.put(INPUT_PARAM_RECORDS_TYPE, recordsType);
            parameters.put(INPUT_PARAM_TRANSFER_TYPE, TransferObject.TRANSFER_TYPE_RESPONSE);
            parameters.put(INPUT_PARAM_OMIT_HEADER_RECORD, Boolean.TRUE.toString());
            s_currentPlainRows = FLFasterUtils.getActualResponseRecords(studentOid, parameters, userData,
                    broker, locale);
            AppGlobals.getLog().log(Level.SEVERE, "current plain rows " + s_currentPlainRows);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_helper = new TransferObjectHelper(super.getBroker());
        Boolean persistChanges = (Boolean) getParameter(INPUT_PARAM_PERSIST_CHANGES);
        m_persistChanges = persistChanges == null ? false : persistChanges.booleanValue();
    }


    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        initializeStudentTransfers();
        FLFasterExportConfiguration exportConfig =
                new FLFasterExportConfiguration(getCurrentContext(), getBroker());
        mergeStudentTransfers(exportConfig);
    }


    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_userData = userData;
    }


    /**
     * Initialize student transfers.
     */
    private void initializeStudentTransfers() {
        m_studentTransfers = new ArrayList<>();
        String studentTransferOidsString = (String) getParameter(INPUT_PARAM_STUDENT_TRANSFERS);
        String[] studentTransferOids = null;
        if (studentTransferOidsString != null) {
            studentTransferOids = studentTransferOidsString.split(";");

            X2Criteria studentUdcCriteria = new X2Criteria();
            studentUdcCriteria.addIn(X2BaseBean.COL_OID, Arrays.asList(studentTransferOids));
            QueryByCriteria studentUdcQuery = new QueryByCriteria(UserDefinedTableC.class, studentUdcCriteria);
            Collection<UserDefinedTableC> studentUdcs = getBroker().getCollectionByQuery(studentUdcQuery);
            for (UserDefinedTableC udc : studentUdcs) {
                StudentTransferObject studentTransfer = new StudentTransferObject(m_helper, udc);
                m_studentTransfers.add(studentTransfer);
            }
        }
    }


    /**
     * Merge student transfers.
     *
     * @param exportConfig FLFasterExportConfiguration
     */
    private void mergeStudentTransfers(FLFasterExportConfiguration exportConfig) {
        LinkedList<String> outputList = new LinkedList<>();
        if (!m_persistChanges) {
            outputList.add(
                    "Merge procedure have been run with \"Persist changes\" flag set to false, so changes below weren't made:");
        }
        for (StudentTransferObject studentTransferObject : m_studentTransfers) {
            LinkedList<String> comments = new LinkedList<String>();
            comments.add("\nChanges for student " + studentTransferObject.getStudentName() + ":");
            List<TransferObjectRecord> records = studentTransferObject.getRecords();
            if (!getBroker().isInTransaction()) {
                getBroker().beginTransaction();
            }

            initializeCurrentPlainRows(studentTransferObject.getStudentOid(),
                    studentTransferObject.getRecordsType(), m_userData, getBroker(), getLocale());

            boolean mergeFailed = false;
            try {
                for (TransferObjectRecord record : records) {
                    String recordType = TransferObjectHelper.getRecordType(record.getPlainRow());
                    if (!s_excludedRecords.contains(recordType)) {
                        String mergeProcedureIdPrefix =
                                PATTERN_MERGE_PROCEDURE_ID.replace(PATTERN_RECORD_TYPE, recordType);
                        X2Criteria criteria = new X2Criteria();
                        criteria.addLike(Procedure.COL_ID, mergeProcedureIdPrefix);
                        QueryByCriteria query = new QueryByCriteria(Procedure.class, criteria);
                        query.addOrderBy(Procedure.COL_WEIGHT, true);
                        Collection<Procedure> procedures =
                                getBroker().getCollectionByQuery(query);
                        for (Procedure procedure : procedures) {
                            int jobStatus = runRecordTypeMergeProcedure(procedure, recordType,
                                    studentTransferObject.getStudentOid(),
                                    record.getPlainRow(),
                                    comments);
                            if (jobStatus != JOB_STATUS_PROCEDURE_NOT_FOUND && jobStatus != ToolJob.STATUS_SUCCESS) {
                                mergeFailed = true;
                                break;
                            }
                        }
                    }
                }
            } catch (Exception e) {
                mergeFailed = true;
                comments.add("\t\tError!!!");
                comments.add("\t\t" + e.getMessage());
                comments.add("\t\t" + e.getStackTrace().toString());
                e.printStackTrace();
            } finally {
                StringBuffer mergeResult = new StringBuffer();
                if (mergeFailed) {
                    outputList.add("Merge Failed!!!\n");
                }
                for (String comment : comments) {
                    mergeResult.append(comment + "\n");
                }
                if (mergeFailed || !m_persistChanges) {
                    getBroker().rollbackTransaction();
                } else {
                    getBroker().commitTransaction();

                    studentTransferObject.setStatus(StudentTransferObject.STATUS_MERGED);
                    studentTransferObject.setMergeResult(mergeResult.toString());
                    studentTransferObject.persist();
                    StudentTransferObject request = m_helper.getRequestToReply(studentTransferObject, exportConfig);
                    if (request != null) {
                        request.setStatus(StudentTransferObject.STATUS_MERGED);
                        request.setMergeResult(mergeResult.toString());
                        request.persist();
                    }
                }
                clearPlainRows();
            }
            outputList.addAll(comments);
        }
        for (String comment : outputList) {
            PrintStream stream = null;
            try {
                stream = new PrintStream(getResultHandler().getOutputStream());
            } catch (IOException e) {
                throw new X2RuntimeException();
            }
            stream.print(comment + "\n");
        }
    }


    /**
     * Run record type merge procedure.
     *
     * @param procedure Procedure
     * @param recordType String
     * @param studentOid String
     * @param plainRow String
     * @param comments LinkedList<String>
     * @return int
     * @throws Exception exception
     */
    private int runRecordTypeMergeProcedure(Procedure procedure,
                                            String recordType,
                                            String studentOid,
                                            String plainRow,
                                            LinkedList<String> comments)
            throws Exception {
        if (procedure != null) {
            Map<String, Object> parameters = new HashMap<>();
            parameters.put(INPUT_PARAM_STUDENT_OID, studentOid);
            parameters.put(INPUT_PARAM_PLAIN_ROW, plainRow);
            parameters.put(INPUT_PARAM_PERSIST_CHANGES, getParameter(INPUT_PARAM_PERSIST_CHANGES));
            MergeToolJob recordMergeJob = null;
            try {
                recordMergeJob =
                        createMergeToolJob(procedure, parameters, m_userData, getBroker(), getLocale());
            } catch (ZipException e) {
                System.out.println("Cannot find procedure with id " + procedure.getId());
                throw e;
            }
            boolean error = false;
            try {
                recordMergeJob.setBroker(getBroker());
                recordMergeJob.setActualRecords(s_currentPlainRows);
                recordMergeJob.run();
            } catch (Exception e) {
                error = true;
                throw e;
            } finally {
                if (recordMergeJob.getStatus() != ToolJob.STATUS_SUCCESS) {
                    error = true;
                }
                ResultHandler resultHandler = recordMergeJob.getResultHandler();

                try {
                    if (!error) {
                        comments.add("\n\tRecord type " + recordType + " '" + procedure.getName() + "' changes:");
                        List<String> rows = FLFasterUtils.getRecords(recordMergeJob.getResultHandler());

                        if (rows.size() == 0) {
                            comments.add("\t\tNo changes");
                        } else {
                            comments.addAll(rows);
                        }
                    } else {
                        comments.add("\t\tError!!!");
                    }
                } catch (IOException ioe) {
                    throw new X2BaseException(ioe);
                } finally {
                    resultHandler.close();
                }
            }
            return recordMergeJob.getStatus();
        }
        return JOB_STATUS_PROCEDURE_NOT_FOUND;
    }
}
