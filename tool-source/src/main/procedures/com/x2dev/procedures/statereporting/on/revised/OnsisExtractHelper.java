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
package com.x2dev.procedures.statereporting.on.revised;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.utils.io.file.CSVReader;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisRetrieverNestedExport;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisRetrieverNestedExportStreaming;
import com.x2dev.utils.FolderDoesNotExistException;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.UniqueFolderNotCreatedException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.ZipUtils;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.zip.ZipException;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The Class OnsisExtractHelper.
 */
public class OnsisExtractHelper {
    /**
     * The Enum CsvField.
     */
    enum CsvField {
        ADE("Ind Study Reg ADE"),
        //
        ASSIGNMENT_START_DATE("Assign start date"),
        //
        ASSIGNMENT_WITHDRAWAL_TYPE("Withdrawal Type"),
        //
        ATTENDANCE_TYPE("Attendance Type"),
        //
        CARE_TRTMNT_RES_TYPE("Care Treatm Resid"),
        //
        CLASS_CODE("Class Code"),
        //
        CLASS_START_DATE("Class Start Date"),
        //
        CLASS_END_DATE("Class End Date"),
        //
        COURSE_START_DATE("Course Start Date"),
        //
        COUNTRY_TYPE_BIRTH("Country Type Birth"),
        //
        COUNTRY_TYPE_EXIT("Country Type Exit"),
        //
        COMMUNITY_INVOLMENT_ACCU_HOURS("Comm Involv Acc Hrs"),
        //
        CREDIT_RECOVERY("Credit Recovery"),
        //
        CURRENT_RESIDENCE_COUNTRY("CurrentResCountry"),
        //
        CURRENT_RESIDENCE_PROVINCE("CurrentResProvince"),
        //
        EDUCATOR_FTE("FTE"),
        //
        EDUCATOR_LEAVE_TYPE("Leave type code"),
        //
        ELEMENTARY_SUBJECT_TYPE("El subject type"),
        //
        ENR_END_DATE("Enr End Date"),
        //
        ENR_START_DATE("Enr Start Date"),
        //
        STD_CLASS_LANGUAGE_TYPE("Std Cl Enr Lang Type"),
        //
        EXCEPTIONALITY_TYPE("Exceptionality Type"),
        //
        FRENCH_ADMISSION_APPROVAL_DATE("Fr Adm Aprvl Date"),
        //
        FTE("Regular FTE"),
        //
        GRADE_DESIGNATION_TYPE("Grade Designation"),
        //
        GRADE_TYPE("Grade Level"),
        //
        GRADE_TYPES("Grade types"),
        //
        HIGH_CREDIT_ADE("Ind Study HC ADE"),
        //
        HIGH_CREDIT_FTE("High Credit FTE"),
        //
        INCIDENT_ID("Incident Id"),
        //
        INDIGENOUS_SELF_ID("Indig Self Ident"),
        //
        INDIVIDUAL_EDUCATION_PLAN_FLAG("Ind Ed Plan Flag"),
        //
        INSTRUCTIONAL_TM_TYPE("Instr time type"),
        //
        IPRC_STUDENT_FLAG("IPRC Student Flag"),
        //
        IPRC_REVIEW_DATE("IPRC Review Date"),
        //
        LANGUAGE_TYPE("Language Type Code"),
        //
        LITERACY_STATUS_TYPE("Literacy Status"),
        //
        LOCAL_COURSE_CODE("Local Course Code"),
        //
        MATURE_STUDENT_FLAG("Mature Std Flag"),
        //
        MAIN_EXCEPTIONALITY_FLAG("Main Except Flag"),
        //
        MAIN_SCHOOL_FLAG("Main School Flag"),
        //
        MEN("MEN"),
        //
        MINISTRY_DEV_TYPE("Ministry Dev Type"),
        //
        MINISTRY_DFND_CRS("Min Def Course Code"),
        //
        NON_IDENTIFIED_STUDENT_FLAG("Non Id Student Flag"),
        //
        OEN("OEN"),
        //
        POSITION_TYPE_CODE("Position type code"),
        //
        POSTAL_AREA_TYPE("Postal Area Type"),
        //
        PROVINCE_STATE_TYPE_BIRTH("Prvnc St Type Birth"),
        //
        PROVINCE_STATE_TYPE_EXIT("Prvnc St Type Exit"),
        //
        REFERENCE_NUMBER("Reference Number"),
        //
        RESIDENCE_STATUS_TYPE("Residence Status"),
        //
        SAL_COMP_TYPE("SAL Comp Type"),
        //
        SAL_END_DATE("SAL End Date"),
        //
        SAL_START_DATE("SAL Start Date"),
        //
        SCHOOL_NUMBER("School Number"),
        //
        SCHOOL_STUDENT_NUMBER("Skl Std Number"),
        //
        SPECIAL_EDU_PLMNT_TYPE("Spec Edu Plmnt Type"),
        //
        SPECIAL_EDUCATION_FLAG("Special Education"),
        //
        STU_BRD_RES_STAT_TYPE("Std Brd R Status"),
        //
        STUDENT_MOBILITY("Std Mobility"),
        //
        STUDENT_MOBILITY_TYPE_EXIT("Std Mobility Exit"),
        //
        TEACHING_TYPE("Teaching type code"),
        //
        YOUR_REFERENCE_NUMBER("Your Ref Number");

        private String m_fieldName;

        /**
         * Instantiates a new csv field.
         *
         * @param fieldName String
         */
        private CsvField(String fieldName) {
            m_fieldName = fieldName;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return m_fieldName;
        }
    }

    public static final MultiLevelMap.ValueByKeyResolver CSV_RECORD_RESOLVER = new MultiLevelMap.ValueByKeyResolver() {

        /**
         * @see com.x2dev.procedures.statereporting.on.FilterableFactory.MultiLevelMap.ValueByKeyResolver#getValue(java.lang.String,
         *      java.lang.Object)
         */
        @Override
        public Object getValue(String key, Object entity) {
            OnsisCsvDataRecord record = (OnsisCsvDataRecord) entity;
            return record.getSingleFieldValue(key);
        }

    };
    public static final String D1 = "D1";
    public static final String D2 = "D2";
    public static final String D3 = "D3";
    public static final String DT = "DT";
    public static final String EXSMS_FORMAT_ID_PREFIX = "EXSMS-";
    public static final String FORMAT_ID_DEFAULT = "EXSMS-DEF-";
    public static final String H1 = "H1";
    public static final String H2 = "H2";

    public static final List<String> s_defaultFormatsRecordTypes = Arrays.<String>asList(H1, H2);

    private static final String FORMAT_ID_DEFAULT_H1 = FORMAT_ID_DEFAULT + H1;
    private static final String FORMAT_ID_DEFAULT_H2 = FORMAT_ID_DEFAULT + H2;

    private static final int INDEX_EXTRACT_TYPE = 1;
    private static final int INDEX_FORMAT_ID_PREFIX_END = 9;
    private static final int INDEX_FORMAT_ID_PREFIX_START = 0;
    private static final int INDEX_RECORD_TYPE = 0;
    private static final int MIN_COLUMNS_NUMBER = 2;
    private static final int RECORD_TYPE_LENGTH = 2;

    private final X2Broker m_broker;
    private Date m_creationDate = new Date();
    private SimpleDateFormat m_creationTimeFormat = new SimpleDateFormat("hh:mm");
    private Map<ExportFormatDefinition, List<ExportFormatField>> m_exportFormatFields = new HashMap<>();
    private Map<ExportFormatDefinition, Map<String, ExportFormatField>> m_exportFormatFieldById = new HashMap<>();
    private Map<String, ExportFormatDefinition> m_formats;
    private Map<String, OnsisExtractRecords> m_matchersByExtractType = new HashMap<>();
    private OnsisElementsHelper m_onsisElementsHelper;
    private SubmissionPeriod m_submissionPeriod;
    public Map<String, String> m_multiValuedDelimiterByFieldName = new HashMap<>();


    /*
     * For special-case Other Course Information Type field
     */
    public static final String EX_STUDENT_CLASS_ENROLMENT = "STUDENT CLASS ENROLMENT";


    /**
     * Instantiates a new onsis extract helper.
     *
     * @param broker X2Broker
     */
    public OnsisExtractHelper(X2Broker broker) {
        m_broker = broker;
        m_multiValuedDelimiterByFieldName.put(CsvField.SAL_COMP_TYPE.toString(), ",");
        m_multiValuedDelimiterByFieldName.put(CsvField.GRADE_TYPES.toString(), ",");
    }

    /**
     * The Class OnsisExtractHelperUtils.
     */
    public static class OnsisExtractHelperUtils {
        public static Comparator fieldsPositionComparator = new Comparator<ExportFormatField>() {
            @Override
            public int compare(ExportFormatField o1, ExportFormatField o2) {
                return Integer.valueOf(o1.getPosition()).compareTo(Integer.valueOf(o2.getPosition()));
            }
        };

        /**
         * Gets the formats by id.
         *
         * @param id String
         * @param formatsById Map<String,ExportFormatDefinition>
         * @return List
         */
        public static List<ExportFormatDefinition> getFormatsById(String id,
                                                                  Map<String, ExportFormatDefinition> formatsById) {
            List<ExportFormatDefinition> formats = new ArrayList<>();
            for (Entry<String, ExportFormatDefinition> formatById : formatsById.entrySet()) {
                if (formatById.getKey().startsWith(id)) {
                    formats.add(formatById.getValue());
                }
            }
            return formats;
        }
    }

    /**
     * The Class OnsisExtractRecordsMatcher.
     */
    public class OnsisExtractRecords {
        public static final String FIELD_VALUE_ACADEMIC_YEAR = "Academic Year";
        public static final String FIELD_VALUE_SUBMISSION_PERIOD = "Subm Per Type";

        private static final String EMPTY_STRING = "";
        private static final String MARKER_CREATION_TIME = "CREATION_TIME";
        private static final String MARKER_CREATION_DATE = "CREATION_DATE";
        private static final String MARKER_FILE_TYPE = "FILE_TYPE";
        private static final String MARKER_NO_CSV_SPECIFICATION = "NO_CSV_SPECIFICATION";
        private static final String MARKER_NO_FIELD = "NO_FIELD";
        private static final String MARKER_NO_RECORDS_IN_XML = "NO_RECORDS_IN_XML";
        private static final String MARKER_RECORD_TYPE = "RECORD_TYPE";
        private static final String MARKER_VERSION = "VERSION";

        private static final String PATTERN_DASH = "[-]";

        /**
         * The Class DataRecord.
         */
        public class OnsisCsvDataRecord {
            private final Map<String, List<String>> m_fullKeysList = new HashMap();
            private final String[] m_rowDT;
            private final String[] m_rowH1;
            private final String[] m_rowH2;

            /**
             * Instantiates a new data record.
             *
             * @param rowH1 String[]
             * @param rowH2 String[]
             * @param rowDT String[]
             */
            public OnsisCsvDataRecord(String[] rowH1, String[] rowH2, String[] rowDT) {
                m_rowH1 = rowH1;
                m_rowH2 = rowH2;
                m_rowDT = rowDT;
            }

            /**
             * Instantiates a new data record.
             *
             * @param rowH1 String[]
             * @param rowH2 String[]
             * @param rowDT String[]
             */
            public OnsisCsvDataRecord(List<String> rowH1, List<String> rowH2, List<String> rowDT) {
                m_rowH1 = rowH1.toArray(new String[rowH1.size()]);
                m_rowH2 = rowH2.toArray(new String[rowH2.size()]);
                m_rowDT = rowDT.toArray(new String[rowDT.size()]);
            }

            /**
             * Array to csv.
             *
             * @param row String[]
             * @return String
             */
            public String arrayToCsv(String[] row) {
                return String.join(",",
                        Arrays.stream(row).map(value -> "\"" + (value == null ? "" : value) + "\"")
                                .collect(Collectors.toList()));
            }

            /**
             * Gets the dt csv.
             *
             * @return String
             */
            public String getDtCsv() {
                return arrayToCsv(m_rowDT);
            }

            /**
             * Gets the field value.
             *
             * @param fieldName String
             * @return String
             */
            public String getSingleFieldValue(String fieldName) {
                String result = null;

                // /*
                // * XML export field OTHER_COURSE_INFORMATION_TYPE
                // * maps to two columns in the EXSMS-0004 CSV file.
                // * The first column is Credit Recovery Type: "1" or blank.
                // * The next column is Ministry Dev Type: "2" or blank.
                // *
                // * The two columns combine to form OtherCourseInfoType.
                // * If both values are present, the combined value is "1,2".
                // */
                // if (CsvField.OTHER_COURSE_INFORMATION_TYPE.toString().equals(fieldName)
                // && getExtractType().startsWith(EX_STUDENT_CLASS_ENROLMENT)) {
                // /*
                // * Lookup the two values
                // */
                // String creditRecoveryType = null; // "1" or blank
                // String ministryDevType = null; // "2" or blank
                //
                // int indexDT = getFieldIndexFromDT(fieldName);
                // if (indexDT > -1) {
                // creditRecoveryType = m_rowDT[indexDT];
                // ministryDevType = m_rowDT[indexDT + 1];
                // }
                //
                // /*
                // * Combine the two values
                // */
                // if (!StringUtils.isBlank(creditRecoveryType)) {
                // result = creditRecoveryType;
                // }
                // if (!StringUtils.isBlank(ministryDevType)) {
                // if (StringUtils.isBlank(result)) {
                // result = ministryDevType;
                // } else {
                // result = result + "," + ministryDevType;
                // }
                // }
                // if (StringUtils.isBlank(result)) {
                // result = "";
                // }
                // } else {
                int indexDT = getFieldIndexFromDT(fieldName);
                if (indexDT > -1 && indexDT < m_rowDT.length) {
                    result = m_rowDT[indexDT];
                }
                // }

                int indexH1 = getFieldIndexFromH1(fieldName);
                if (indexH1 > -1) {
                    result = m_rowH1[indexH1];
                }
                int indexH2 = getFieldIndexFromH2(fieldName);
                if (indexH2 > -1) {
                    result = m_rowH2[indexH2];
                }
                if (!StringUtils.isEmpty(result)) {
                    if (CsvField.OEN.toString().equals(fieldName) || CsvField.MEN.toString().equals(fieldName)) {
                        result = result.replaceAll(PATTERN_DASH, EMPTY_STRING);
                    }
                }
                if (result != null) {
                    return result;
                }
                throw new RuntimeException("Cannot find field " + fieldName + " for extract type " + getExtractType());
            }

            /**
             * Gets the field value.
             *
             * @param field CsvField
             * @return String
             */
            public String getSingleFieldValue(CsvField field) {
                return getSingleFieldValue(field.toString());
            }

            /**
             * Gets the delimited field values.
             *
             * @param key the key
             * @return the delimited field values
             */
            public List<String> getDelimitedFieldValues(String key) {
                String fieldValue = getSingleFieldValue(key);

                if (fieldValue == null
                        || !m_multiValuedDelimiterByFieldName.containsKey(key)
                        || !fieldValue.contains(m_multiValuedDelimiterByFieldName.get(key))) {
                    return Arrays.asList(fieldValue);
                }


                boolean trim = true;
                List<String> individualValues =
                        StringUtils.convertDelimitedStringToList(fieldValue,
                                m_multiValuedDelimiterByFieldName.get(key), trim);

                return individualValues;
            }

            /**
             * Gets the full keys.
             *
             * @param existingKeyFieldsList List<List<String>>
             * @param filterKeyFieldsFn the filter key fields fn
             * @return List
             */
            public List<String> getFullKeysList(List<List<String>> existingKeyFieldsList,
                                                Predicate<String> filterKeyFieldsFn) {
                String key = existingKeyFieldsList.toString() + filterKeyFieldsFn;
                List<String> fullKeysList = m_fullKeysList.get(key);
                if (fullKeysList == null) {
                    LinkedHashSet<String> fullKeys = new LinkedHashSet<>();

                    for (List<String> existingKeyFields : existingKeyFieldsList) {
                        fullKeys.addAll(getFullKeys(existingKeyFields, filterKeyFieldsFn));
                    }

                    fullKeysList = new ArrayList<String>(fullKeys);
                    m_fullKeysList.put(key, fullKeysList);
                }
                return fullKeysList;
            }

            /**
             * Gets the full keys.
             *
             * @param existingKeyFields the existing key fields
             * @param filterKeyFieldsFn the filter key fields fn
             * @return the full keys
             */
            public List<String> getFullKeys(List<String> existingKeyFields, Predicate<String> filterKeyFieldsFn) {
                LinkedHashSet<String> fullKeys = new LinkedHashSet<>();

                String h1KeyValue = getH1KeyValue(existingKeyFields);
                String h2KeyValue = getH2KeyValue(existingKeyFields);

                List<String> dtKeyValues = getDTKeyValues(existingKeyFields, filterKeyFieldsFn);

                for (String dtKeyValue : dtKeyValues) {
                    String fullKey = h1KeyValue + h2KeyValue + dtKeyValue;
                    fullKeys.add(fullKey.toUpperCase());
                }

                return new ArrayList<String>(fullKeys);
            }

            /**
             * Gets the h 1 csv.
             *
             * @return String
             */
            public String getH1Csv() {
                return arrayToCsv(m_rowH1);
            }

            /**
             * Gets the h 2 csv.
             *
             * @return String
             */
            public String getH2Csv() {
                return arrayToCsv(m_rowH2);
            }

            /**
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                StringBuilder output = new StringBuilder();
                if (m_rowH1 != null && m_rowH1.length > 0) {
                    output.append("H1: " + getH1Csv() + "  ");
                }
                if (m_rowH2 != null && m_rowH2.length > 0) {
                    output.append("H2: " + getH2Csv() + "  ");
                }
                if (m_rowDT != null && m_rowDT.length > 0) {
                    output.append("DT: " + getDtCsv() + "  ");
                }
                return output.toString();
            }

            /**
             * Write elements.
             *
             * @param xmlWriter XMLStreamWriter
             * @throws XMLStreamException exception
             */
            public void writeElements(XMLStreamWriter xmlWriter) throws XMLStreamException {
                writeElements(xmlWriter, m_formatH1, m_rowH1);
                writeElements(xmlWriter, m_formatH2, m_rowH2);
                writeElements(xmlWriter, m_formatsDT.iterator().next(), m_rowDT);
            }

            /**
             * Gets the DT key values.
             *
             * @param existingKeyFields List<String>
             * @param filterKeyFieldsFn the filter key fields fn
             * @return List
             */
            protected List<String> getDTKeyValues(List<String> existingKeyFields, Predicate<String> filterKeyFieldsFn) {
                LinkedHashSet<String> dtKeyValues = new LinkedHashSet<>();
                for (LinkedHashMap<String, Integer> keysPositionsDT : m_keysPositionsDTList) {
                    List<String> dtKeyValuesForThisKeyList =
                            getKeysByIndiced(m_rowDT, keysPositionsDT, existingKeyFields, filterKeyFieldsFn);
                    if (dtKeyValuesForThisKeyList == null) {
                        continue;
                    }

                    dtKeyValues.addAll(dtKeyValuesForThisKeyList);
                }
                return new ArrayList<String>(dtKeyValues);
            }

            /**
             * Gets the h1 key value.
             *
             * @param existingKeyFields List<String>
             * @return String
             */
            private String getH1KeyValue(List<String> existingKeyFields) {
                return getKeyByIndiced(m_rowH1, m_keysPositionsH1, existingKeyFields, null);
            }

            /**
             * Gets the h2 key value.
             *
             * @param existingKeyFields List<String>
             * @return String
             */
            private String getH2KeyValue(List<String> existingKeyFields) {
                return getKeyByIndiced(m_rowH2, m_keysPositionsH2, existingKeyFields, null);
            }

            /**
             * Build this row's single fullkey for this keyList.
             * Assumes only one possible fullkey can be built.
             *
             * @param row the row
             * @param indices the indices
             * @param existingKeyFields the existing key fields
             * @param filterKeyFieldsFn the filter key fields fn
             * @return the key by indiced
             */
            private String getKeyByIndiced(String[] row,
                                           LinkedHashMap<String, Integer> indices,
                                           List<String> existingKeyFields,
                                           Predicate<String> filterKeyFieldsFn) {
                List<String> fullkeys = getKeysByIndiced(row, indices, existingKeyFields, filterKeyFieldsFn);
                if (fullkeys == null || fullkeys.isEmpty()) {
                    return "";
                }
                return fullkeys.get(0);
            }

            /**
             * Gets the key by indiced.
             *
             * @param row String[]
             * @param indices List<Integer> Key fields on the DT record itself
             * @param existingKeyFields List<String> All key names in query incl. parent keys
             * @param filterKeyFieldsFn the filter key fields fn
             * @return String
             */
            private List<String> getKeysByIndiced(String[] row,
                                                  LinkedHashMap<String, Integer> indices,
                                                  List<String> existingKeyFields,
                                                  Predicate<String> filterKeyFieldsFn) {
                List<String> fullkeys = new ArrayList<>();

                List<StringBuffer> partialKeys = new ArrayList<>();
                for (Entry<String, Integer> index : indices.entrySet()) {
                    // if (!existingKeyFields.contains(index.getKey())) {
                    // return null;
                    // }

                    /*
                     * Skip entire key if empty field value
                     */
                    String fieldValue = getSingleFieldValue(index.getKey());
                    boolean keep = filterKeyFieldsFn == null
                            || !StringUtils.isBlank(fieldValue)
                            || filterKeyFieldsFn.test(index.getKey());
                    if (!keep) {
                        return null;
                    }

                    /*
                     * If this field is a list of multiple individual key values,
                     * parse and generate a key for each value
                     */
                    if (fieldValue != null
                            && m_multiValuedDelimiterByFieldName.containsKey(index.getKey())
                            && fieldValue.contains(m_multiValuedDelimiterByFieldName.get(index.getKey()))) {
                        List<String> individualValues = getDelimitedFieldValues(index.getKey());

                        List<StringBuffer> newPartialKeys = new ArrayList<>();

                        for (int i = 0; i < individualValues.size(); i++) {
                            String individualValue = individualValues.get(i);

                            /*
                             * Skip if empty field value
                             */
                            boolean keepIndividualValue = filterKeyFieldsFn == null
                                    || !StringUtils.isBlank(individualValue)
                                    || filterKeyFieldsFn.test(individualValue);
                            if (!keepIndividualValue) {
                                continue;
                            }

                            // clone the original partialKeys and append this individualValue
                            String individualValueKey = OnsisStateReportData.makeKey(index.getKey(), individualValue);
                            if (partialKeys.isEmpty()) {
                                // nothing to clone. Just add an item
                                newPartialKeys.add(new StringBuffer(individualValueKey));
                            } else {
                                for (StringBuffer oldPartialKey : partialKeys) {
                                    newPartialKeys.add(new StringBuffer(oldPartialKey.toString() + individualValueKey));
                                }
                            }
                        }

                        partialKeys = newPartialKeys;
                    } else {
                        // single-valued field. append to each partial key
                        String fieldValueKey = OnsisStateReportData.makeKey(index.getKey(), fieldValue);
                        if (partialKeys.isEmpty()) {
                            partialKeys.add(new StringBuffer(fieldValueKey));
                        } else {
                            appendToAll(fieldValueKey, partialKeys);
                        }
                    }
                }

                partialKeys.stream().forEach(buf -> fullkeys.add(buf.toString()));

                return fullkeys;
            }

            /**
             * Append to all.
             *
             * @param value the value
             * @param partialKeyList the partial key list
             */
            private void appendToAll(String value, List<StringBuffer> partialKeyList) {
                for (StringBuffer partialKey : partialKeyList) {
                    partialKey.append(value);
                }
            }

            /**
             * Adds the to all.
             *
             * @param value the value
             * @param partialValueSets the partial value sets
             */
            private void addToAll(String value, List<List<String>> partialValueSets) {
                for (List<String> partialValueSet : partialValueSets) {
                    partialValueSet.add(value);
                }
            }

            /**
             * Generate 1+ currentEntityValueSet from the CSV DT record
             * for the given currentEntityKeySet.
             *
             * @param currentEntityKeySet the current entity key set
             * @param fullKeyToDelete the full key to delete
             * @return the current entity value sets for key set that match full key
             */
            public List<List<String>> getCurrentEntityValueSetsForKeySet_thatMatchFullKey(List<String> currentEntityKeySet,
                                                                                          String fullKeyToDelete) {
                List<List<String>> partialValueSets = new ArrayList<>();

                for (String currentEntityKey : currentEntityKeySet) {
                    List<String> fieldValues = getDelimitedFieldValues(currentEntityKey);
                    if (fieldValues.size() == 0) {
                        throw new RuntimeException(
                                "fieldValues is empty for " + currentEntityKey + " of " + currentEntityKeySet);
                    }

                    if (fieldValues.size() == 1) {
                        // easy case. create or append to all
                        if (partialValueSets.isEmpty()) {
                            List<String> partialValueSet = new ArrayList<>();
                            partialValueSets.add(partialValueSet);
                        }
                        addToAll(fieldValues.get(0), partialValueSets);
                    } else {
                        List<List<String>> newPartialValueSets = new ArrayList<>();
                        for (String individualValue : fieldValues) {
                            // easy case. create or append to all
                            if (partialValueSets.isEmpty()) {
                                List<String> newPartialValueSet = new ArrayList<>();
                                newPartialValueSet.add(individualValue);
                                newPartialValueSets.add(newPartialValueSet);
                            } else {
                                // clone the original partialValueSets
                                // and append this individualValue
                                for (List<String> oldPartialValueSet : partialValueSets) {
                                    List<String> newPartialValueSet = new ArrayList<>(oldPartialValueSet);
                                    newPartialValueSet.add(individualValue);
                                    newPartialValueSets.add(newPartialValueSet);
                                }
                            }
                        }
                        partialValueSets = newPartialValueSets;
                    }
                }

                /*
                 * Filter partialValueSets to those that match fullKeyToDelete
                 */
                List<List<String>> currentValueKeySets = new ArrayList<>();

                for (List<String> partialValueSet : partialValueSets) {
                    String dtKey = null;
                    for (int i = 0; i < currentEntityKeySet.size(); i++) {
                        String key = currentEntityKeySet.get(i);
                        String value = partialValueSet.get(i);
                        String oneFieldKey = OnsisStateReportData.makeKey(key, value);
                        if (StringUtils.isBlank(oneFieldKey)) {
                            break;
                        }
                        dtKey = StringUtils.emptyIfNull(dtKey) + oneFieldKey;
                    }
                    if (dtKey == null) {
                        continue;
                    }
                    if (fullKeyToDelete.endsWith(dtKey)) {
                        currentValueKeySets.add(partialValueSet);
                    }
                }

                return currentValueKeySets;
            }

            /**
             * Write elements.
             *
             * @param xmlWriter XMLStreamWriter
             * @param efd ExportFormatDefinition
             * @param values String[]
             * @throws XMLStreamException exception
             */
            private void writeElements(XMLStreamWriter xmlWriter, ExportFormatDefinition efd, String[] values)
                    throws XMLStreamException {
                List<ExportFormatField> fields = getExportFormatFields(efd);
                int ind = 0;
                for (ExportFormatField field : fields) {
                    if (ind < values.length) {
                        if (!StringUtils.isEmpty(field.getSifPath())) {
                            String[] elements = field.getSifPath().split("\\/");
                            for (String element : elements) {
                                xmlWriter.writeStartElement(element);
                            }
                            xmlWriter.writeCharacters(values[ind]);
                            for (int i = 0; i < elements.length; ++i) {
                                xmlWriter.writeEndElement();
                            }
                        }
                    }
                    ++ind;
                }
            }
        }

        private List<OnsisCsvDataRecord> m_dataRecords;
        private List<OnsisCsvDataRecord> m_headerRecords;
        private final Map<String, Map<String, OnsisCsvDataRecord>> m_dataRecordsByExistingKeyFields;
        private final ExportFormatDefinition m_formatH1;
        private final ExportFormatDefinition m_formatH2;
        private final List<ExportFormatDefinition> m_formatsDT;
        private final List<List<String>> m_keys;
        private final List<LinkedHashMap<String, Integer>> m_keysPositionsDTList;
        private final LinkedHashMap<String, Integer> m_keysPositionsH1;
        private final LinkedHashMap<String, Integer> m_keysPositionsH2;
        private final Map<String, LinkedHashSet<List<String>>> m_parentKeys = new HashMap();
        private ArrayList<String[]> m_plainRows;
        private final List<Filterable<OnsisCsvDataRecord>> m_searchers;
        private Map<String, Integer> m_indexForFormatField = new HashMap<>();
        private ValueByKeyResolver m_valueResolver = new ValueByKeyResolver<OnsisCsvDataRecord>() {

            @Override
            public Object getValue(String key, OnsisCsvDataRecord entity) {
                return entity.getSingleFieldValue(key);
            }

        };

        /**
         * Instantiates a new onsis extract records matcher.
         *
         * @param formatsH1 List<ExportFormatDefinition>
         * @param formatsH2 List<ExportFormatDefinition>
         * @param formatsDT List<ExportFormatDefinition>
         * @param rows ArrayList<String[]>
         */
        public OnsisExtractRecords(List<ExportFormatDefinition> formatsH1,
                List<ExportFormatDefinition> formatsH2,
                List<ExportFormatDefinition> formatsDT, ArrayList<String[]> rows) {
            m_formatH1 = formatsH1.iterator().next();
            m_formatH2 = formatsH2.iterator().next();
            m_formatsDT = formatsDT;
            m_keysPositionsH1 = getKeyPositions(m_formatH1);
            m_keysPositionsH2 = getKeyPositions(m_formatH2);
            m_keysPositionsDTList = new ArrayList<>();
            for (ExportFormatDefinition format : formatsDT) {
                m_keysPositionsDTList.add(getKeyPositions(format));
            }
            m_plainRows = rows;
            m_keys = getKeys(m_formatH1, m_formatH2, m_formatsDT);
            m_headerRecords = getHeaderRecords(rows);
            m_dataRecords = getDataRecords(rows);

            m_dataRecordsByExistingKeyFields = new HashMap<>();

            m_searchers = new ArrayList<>();
            for (List<String> keys : m_keys) {
                m_searchers.add(FilterableFactory.create(m_dataRecords, keys, m_valueResolver));
            }
        }

        /**
         * Does record exist.
         *
         * @param fullKeyToFind String
         * @param existingKeyFieldsList List<List<String>>
         * @param filterKeyFieldsFn the filter key fields fn
         * @return true, if successful
         */
        public boolean doesRecordExist(String fullKeyToFind,
                                       List<List<String>> existingKeyFieldsList,
                                       Predicate<String> filterKeyFieldsFn) {
            boolean doesRecordExist = false;

            for (List<String> existingKeyFields : existingKeyFieldsList) {
                StringBuilder existingKeyFieldsKey = new StringBuilder();
                for (String existingKey : existingKeyFields) {
                    existingKeyFieldsKey.append(existingKey);
                }
                String keyFromKeyFields = existingKeyFieldsKey.toString();
                Map<String, OnsisCsvDataRecord> dataRecordsByKeyMap =
                        m_dataRecordsByExistingKeyFields.get(keyFromKeyFields);

                if (dataRecordsByKeyMap == null) {
                    dataRecordsByKeyMap = new HashMap<>();
                    m_dataRecordsByExistingKeyFields.put(keyFromKeyFields, dataRecordsByKeyMap);

                    for (OnsisCsvDataRecord record : m_dataRecords) {
                        for (String fullKey : record.getFullKeys(existingKeyFields, filterKeyFieldsFn)) {
                            fullKey = fullKey.toUpperCase();
                            dataRecordsByKeyMap.put(fullKey, record);
                        }
                    }
                }
                doesRecordExist |= dataRecordsByKeyMap.containsKey(fullKeyToFind);
                if (doesRecordExist) {
                    break;
                }
            }

            return doesRecordExist;
        }

        /**
         * Find record.
         *
         * @param keys ArrayList<String>
         * @param values ArrayList<String>
         * @return OnsisCsvDataRecord
         */
        public OnsisCsvDataRecord findRecord(List<String> keys, List<String> values) {
            for (Filterable<OnsisCsvDataRecord> searcher : m_searchers) {
                OnsisCsvDataRecord record = searcher.extractFirst(keys, values);
                if (record != null) {
                    return record;
                }
            }
            return null;
        }

        /**
         * Find record.
         *
         * @param keys ArrayList<String>
         * @param values ArrayList<String>
         * @return OnsisCsvDataRecord
         */
        public Collection<OnsisCsvDataRecord> findRecords(List<String> keys, List<String> values) {
            for (Filterable<OnsisCsvDataRecord> searcher : m_searchers) {
                Collection<OnsisCsvDataRecord> records = searcher.filter(keys, values).extract();
                if (records != null && !records.isEmpty()) {
                    return records;
                }
            }
            return null;
        }

        /**
         * Gets the keys.
         *
         * @return List
         */
        public List<List<String>> getAllKeyFields() {
            return m_keys;
        }

        /**
         * Gets the data records.
         *
         * @return List
         */
        public List<OnsisCsvDataRecord> getDataRecords() {
            return m_dataRecords;
        }

        /**
         * Gets the extract type.
         *
         * @return String
         */
        public String getExtractType() {
            return (m_formatsDT != null && m_formatsDT.get(0) != null)
                    ? m_formatsDT.get(0).getSifProfile().toUpperCase()
                    : OnsisExtractHelper.getFileType(m_plainRows.get(0)).toUpperCase();
        }

        /**
         * Gets the header records.
         *
         * @return List
         */
        public List<OnsisCsvDataRecord> getHeaderRecords() {
            return m_headerRecords;
        }

        /**
         * Gets the multi valued delimiter by field name.
         *
         * @param key the key
         * @return the multi valued delimiter by field name
         */
        public String getMultiValuedDelimiterByFieldName(String key) {
            return m_multiValuedDelimiterByFieldName.get(key);
        }

        /**
         * Gets the parent full key.
         *
         * @param data the data
         * @param parentKeys the parent keys
         * @return the parent full key
         */
        public String getParentFullKey(OnsisStateReportData data, List<String> parentKeys) {
            StringBuilder parentFullKey = new StringBuilder();
            for (String parentKey : parentKeys) {
                String parentValue = data.getFieldValueByFieldNameFromAncestor(parentKey);
                parentFullKey.append(OnsisStateReportData.makeKey(parentKey, parentValue));
            }
            return parentFullKey.toString();
        }

        /**
         * Gets the parent full keys.
         *
         * @param data the data
         * @return the parent full keys
         */
        public List<String> getParentFullKeys(OnsisStateReportData data) {
            LinkedHashSet<List<String>> parentKeysSet = getParentKeys(data);
            List<String> parentFullKeys = new ArrayList<String>(parentKeysSet.size());
            for (List<String> parentKeys : parentKeysSet) {
                StringBuilder parentFullKey = new StringBuilder();
                for (String parentKey : parentKeys) {
                    String parentValue = data.getFieldValueByFieldNameFromAncestor(parentKey);
                    parentFullKey.append(OnsisStateReportData.makeKey(parentKey, parentValue));
                }
                parentFullKeys.add(parentFullKey.toString().toUpperCase());
            }
            return parentFullKeys;
        }

        /**
         * Gets the parent keys.
         *
         * @param data the data
         * @return the parent keys
         */
        public LinkedHashSet<List<String>> getParentKeys(OnsisStateReportData data) {
            String mapKey = data.getClass().getName();
            LinkedHashSet<List<String>> parentAllKeys = m_parentKeys.get(mapKey);
            if (parentAllKeys == null) {
                List<List<String>> allKeys = getAllKeyFields();
                parentAllKeys = new LinkedHashSet();
                for (List<String> keyList : allKeys) {
                    List<String> parentKeys = new ArrayList<String>();
                    for (String key : keyList) {
                        boolean currentField = data.getCurrentFieldById(key) != null;
                        if (data.deepDoesFieldExist(key) && !currentField) {
                            parentKeys.add(key);
                        }
                    }
                    parentAllKeys.add(parentKeys);
                }
                m_parentKeys.put(mapKey, parentAllKeys);
            }
            return parentAllKeys;
        }

        /**
         * @param parentKey
         * @return
         */
        Map<List<String>, Map<String, List<OnsisCsvDataRecord>>> m_indexedRecordsMap = new HashMap();

        /**
         * Gets the records map.
         *
         * @param parentKey the parent key
         * @param filterKeyFieldsFn the filter key fields fn
         * @return the records map
         */
        public Map<String, List<OnsisCsvDataRecord>> getRecordsMap(List<String> parentKey,
                                                                   Predicate<String> filterKeyFieldsFn) {
            if (!m_indexedRecordsMap.containsKey(parentKey)) {
                Map<String, List<OnsisCsvDataRecord>> map = new HashMap();

                for (OnsisCsvDataRecord record : getDataRecords()) {
                    record.getFullKeys(parentKey, filterKeyFieldsFn).stream().distinct().forEach(fullKey -> {
                        String[] keys = fullKey.split("\\|+");
                        if (keys.length > parentKey.size()) {
                            StringBuilder key = new StringBuilder();
                            for (int i = 1; i < parentKey.size() + 1; ++i) {
                                key.append("|");
                                key.append(keys[i]);
                                key.append("|");
                            }
                            List<OnsisCsvDataRecord> records = map.get(key.toString());
                            if (records == null) {
                                records = new ArrayList();
                                map.put(key.toString(), records);
                            }
                            records.add(record);
                        }
                    });
                }

                m_indexedRecordsMap.put(parentKey, map);
            }
            return m_indexedRecordsMap.get(parentKey);
        }

        /**
         * Parses the xml.
         *
         * @param element Element
         * @return OnsisExtractRecords
         */
        public OnsisExtractRecords parseXml(Element element) {
            m_dataRecords = parseXml(
                    element,
                    m_formatsDT.get(0).getSifTopic(),
                    getExportFormatFields(m_formatH1),
                    getExportFormatFields(m_formatH2),
                    getExportFormatFields(m_formatsDT.get(0)));
            return this;
        }

        /**
         * Parses the xml.
         *
         * @param element Element
         * @param relatedNodePath String
         * @param h1ElementPaths List<String>
         * @param h2ElementPaths List<String>
         * @param dtElementPaths List<String>
         * @return List
         */
        public List<OnsisCsvDataRecord> parseXml(Element element,
                                                 String relatedNodePath,
                                                 List<ExportFormatField> h1ElementPaths,
                                                 List<ExportFormatField> h2ElementPaths,
                                                 List<ExportFormatField> dtElementPaths) {
            return OnsisResultsHelper.getNodesByPath(element, relatedNodePath)
                    .stream()
                    .map(node -> {
                        List<String> h1Values = h1ElementPaths.stream()
                                .map(field -> getCsvValueByElementPath(node, field))
                                .collect(Collectors.toList());
                        List<String> h2Values = h2ElementPaths.stream()
                                .map(field -> getCsvValueByElementPath(node, field))
                                .collect(Collectors.toList());
                        List<String> dtValues = dtElementPaths.stream()
                                .map(field -> getCsvValueByElementPath(node, field))
                                .collect(Collectors.toList());
                        return new OnsisCsvDataRecord(h1Values, h2Values, dtValues);
                    })
                    .collect(Collectors.toList());

        }

        /**
         * To csv.
         *
         * @return File
         */
        public File toCsv() {
            StringBuilder fileNameBuilder = new StringBuilder();
            fileNameBuilder.append(EXSMS_FORMAT_ID_PREFIX);
            fileNameBuilder.append(getExtractType().trim().replaceAll("/", "_"));
            String pathToNode = m_formatsDT.get(0).getSifTopic();
            if (MARKER_NO_CSV_SPECIFICATION.equals(pathToNode)) {
                fileNameBuilder.append("(");
                fileNameBuilder.append(MARKER_NO_CSV_SPECIFICATION);
                fileNameBuilder.append(")");
            } else if (m_dataRecords == null || m_dataRecords.isEmpty()) {
                fileNameBuilder.append("(");
                fileNameBuilder.append(MARKER_NO_RECORDS_IN_XML);
                fileNameBuilder.append(")");
            }
            fileNameBuilder.append(".csv");
            File csvFile = new File(fileNameBuilder.toString());
            try (FileWriter fw = new FileWriter(csvFile)) {
                Map<String, Map<String, List<OnsisCsvDataRecord>>> groupedRecords =
                        m_dataRecords.stream().collect(
                                Collectors.groupingBy(OnsisCsvDataRecord::getH1Csv,
                                        Collectors.groupingBy(OnsisCsvDataRecord::getH2Csv,
                                                Collectors.toList())));
                groupedRecords.entrySet().stream().forEach(h1entry -> {
                    String h1 = h1entry.getKey();
                    writeLineToFile(fw, h1);
                    h1entry.getValue().entrySet().stream().forEach(h2entry -> {
                        String h2 = h2entry.getKey();
                        writeLineToFile(fw, h2);
                        h2entry.getValue().stream().map(csvRecord -> csvRecord.getDtCsv())
                                .forEach(dtCsv -> writeLineToFile(fw, dtCsv));
                    });
                });
                fw.flush();
            } catch (IOException e) {
                e.printStackTrace();
            }

            return csvFile;
        }

        /**
         * Gets the csv value by element name.
         *
         * @param node Node
         * @param field ExportFormatField
         * @return String
         */
        private String getCsvValueByElementPath(Node node, ExportFormatField field) {
            String elementName = field.getSifPath();
            switch (field.getSifPath()) {
                case MARKER_RECORD_TYPE:
                    return field.getDefaultValue();
                case MARKER_FILE_TYPE:
                    return getExtractType();
                case MARKER_VERSION:
                    return "1";
                case MARKER_CREATION_TIME:
                    return OnsisConstants.DATE_FORMATTER_HH_COLON_MM.format(m_creationDate);
                case MARKER_CREATION_DATE:
                    return OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.format(m_creationDate);
                case MARKER_NO_FIELD:
                    return "";

            }

            return OnsisResultsHelper.getValueByElementPath(node, elementName);
        }

        /**
         * Gets the data records.
         *
         * @param plainRows List<String[]>
         * @return List
         */
        private List<OnsisCsvDataRecord> getDataRecords(List<String[]> plainRows) {
            String[] currentH1 = null;
            String[] currentH2 = null;
            List<OnsisCsvDataRecord> dataRecords = new ArrayList<>();
            for (String[] plainRow : plainRows) {
                String rowType = plainRow[INDEX_RECORD_TYPE].substring(INDEX_RECORD_TYPE, RECORD_TYPE_LENGTH);
                switch (rowType) {
                    case H1:
                        currentH1 = plainRow;
                        break;
                    case H2:
                        currentH2 = plainRow;
                        break;
                    case D1:
                    case D2:
                    case D3:
                    case DT:
                        dataRecords.add(new OnsisCsvDataRecord(currentH1, currentH2, plainRow));
                        break;

                    default:
                        break;
                }
            }
            return dataRecords;
        }

        /**
         * Gets the field index for format.
         *
         * @param fieldName String
         * @param format ExportFormatDefinition
         * @return int
         */
        private int getFieldIndexForFormat(String fieldName, ExportFormatDefinition format) {
            String formatFieldKey = format.getOid() + "|" + fieldName;
            Integer fieldIndex = m_indexForFormatField.get(formatFieldKey);
            if (fieldIndex != null) {
                return fieldIndex.intValue();
            }

            List<ExportFormatField> fields = getExportFormatFields(format);
            ExportFormatField fieldWithName = null;
            for (ExportFormatField field : fields) {
                if (field.getId().equals(fieldName)) {
                    fieldWithName = field;
                    break;
                }
            }
            if (fieldWithName != null) {
                fieldIndex = Integer.valueOf(fields.indexOf(fieldWithName));
            }
            if (fieldIndex == null) {
                fieldIndex = Integer.valueOf(-1);
            }
            m_indexForFormatField.put(formatFieldKey, fieldIndex);
            return fieldIndex.intValue();
        }

        /**
         * Gets the field index from DT.
         *
         * @param fieldName String
         * @return int
         */
        private int getFieldIndexFromDT(String fieldName) {
            return getFieldIndexForFormat(fieldName, m_formatsDT.iterator().next());
        }

        /**
         * Gets the field index from H 1.
         *
         * @param fieldName String
         * @return int
         */
        private int getFieldIndexFromH1(String fieldName) {
            return getFieldIndexForFormat(fieldName, m_formatH1);
        }

        /**
         * Gets the field index from H 2.
         *
         * @param fieldName String
         * @return int
         */
        private int getFieldIndexFromH2(String fieldName) {
            return getFieldIndexForFormat(fieldName, m_formatH2);
        }

        /**
         * Gets the key fields for formats.
         *
         * @param formats ExportFormatDefinition[]
         * @return List
         */
        private List<String> getKeyFieldsForFormats(ExportFormatDefinition... formats) {
            List<String> keyFieldNames = new ArrayList<String>();
            for (ExportFormatDefinition format : formats) {
                for (ExportFormatField field : getExportFormatFields(format)) {
                    if (field.getId().contains("?")) {
                        throw new RuntimeException("Field " + field.getId()
                                + " contains question mark that originally may be non-printable character, please avoid using it in field names");
                    }
                    if (field.getKeyInd()) {
                        keyFieldNames.add(field.getId());
                    }
                }
            }
            return keyFieldNames;
        }

        /**
         * Gets the header records.
         *
         * @param plainRows List<String[]>
         * @return List
         */
        private List<OnsisCsvDataRecord> getHeaderRecords(List<String[]> plainRows) {
            String[] currentH1 = null;
            List<OnsisCsvDataRecord> dataRecords = new ArrayList<>();
            for (String[] plainRow : plainRows) {
                String rowType = plainRow[INDEX_RECORD_TYPE].substring(INDEX_RECORD_TYPE, RECORD_TYPE_LENGTH);
                switch (rowType) {
                    case H1:
                        currentH1 = plainRow;
                        break;
                    case H2:
                        dataRecords.add(new OnsisCsvDataRecord(currentH1, plainRow, plainRow));
                        break;

                    default:
                        break;
                }
            }
            if (dataRecords.size() == 0 && currentH1 != null) {
                dataRecords.add(new OnsisCsvDataRecord(currentH1, null, null));
            }
            return dataRecords;
        }

        /**
         * Gets the key positions.
         *
         * @param format ExportFormatDefinition
         * @return List
         */
        private LinkedHashMap<String, Integer> getKeyPositions(ExportFormatDefinition format) {
            List<ExportFormatField> fields = getExportFormatFields(format);
            LinkedHashMap<String, Integer> indexes = new LinkedHashMap<>();
            for (int i = 0; i < fields.size(); i++) {
                if (fields.get(i).getKeyInd()) {
                    indexes.put(fields.get(i).getId(), Integer.valueOf(i));
                }
            }
            return indexes;
        }

        /**
         * Gets the keys.
         *
         * @param formatH1 ExportFormatDefinition
         * @param formatH2 ExportFormatDefinition
         * @param formatsDT List<ExportFormatDefinition>
         * @return List
         */
        private List<List<String>> getKeys(ExportFormatDefinition formatH1,
                                           ExportFormatDefinition formatH2,
                                           List<ExportFormatDefinition> formatsDT) {
            List<String> headersKeyFields = getKeyFieldsForFormats(formatH1, formatH2);
            List<List<String>> keyFieldsSetsDT = new ArrayList<>();
            for (ExportFormatDefinition formatDT : formatsDT) {
                List<String> keyFields = getKeyFieldsForFormats(formatDT);
                keyFieldsSetsDT.add(keyFields);
            }
            List<List<String>> fullKeyFieldsSets = new ArrayList<>();
            for (List<String> keyFieldsSetDT : keyFieldsSetsDT) {
                ArrayList<String> fullKeys = new ArrayList<>();
                fullKeys.addAll(headersKeyFields);
                fullKeys.addAll(keyFieldsSetDT);
                fullKeyFieldsSets.add(fullKeys);
            }
            return fullKeyFieldsSets;
        }

        /**
         * Write line to file.
         *
         * @param fw FileWriter
         * @param toWrite String
         */
        private void writeLineToFile(FileWriter fw, String toWrite) {
            try {
                fw.write(toWrite + "\n");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }


    /**
     * The Class OnsisElementsHelper.
     */
    static class OnsisElementsHelper {
        // TODO: revert this to the correct ID
        public static final String ONSIS_FORMAT_ID_PREFIX = "ONSI2-";

        private static final String PATH_DELIMITER = "/";

        private X2Broker m_broker;
        private Map<String, String> m_extractTypesByPaths = new HashMap<>();
        private Map<String, List<String>> m_pathsBySifTopic = new HashMap<>();
        private Filterable<ExportFormatDefinition> m_formatsHelper;
        private Filterable<ExportFormatField> m_fieldsHelper;
        private Map<String, Filter<ExportFormatField>> m_filtersByElementName = new HashMap<>();
        private Map<String, Filter<ExportFormatField>> m_filtersBySifTopic = new HashMap<>();

        /**
         * Instantiates a new onsis elements helper.
         *
         * @param broker X2Broker
         */
        private OnsisElementsHelper(X2Broker broker) {
            m_broker = broker;
        }

        /**
         * Gets the extract type by element path.
         *
         * @param elementPath String
         * @return String
         */
        public String getExtractTypeByElementPath(String elementPath) {
            String extractType = m_extractTypesByPaths.get(elementPath);
            if (extractType == null) {
                List<String> pathElements = Arrays.asList(elementPath.split(PATH_DELIMITER));

                if (pathElements.isEmpty()) {
                    throw new RuntimeException("Cannot resolve field, empty path");
                }
                String lastElement = pathElements.get(pathElements.size() - 1);
                Filter<ExportFormatField> filter = getEffFilterByPathElement(lastElement);
                List<ExportFormatField> fields = new ArrayList<>(getEffHelper().filter(filter).extract());
                if (fields.size() == 0) {
                    throw new RuntimeException("Cannot resolve field for element by path: " + elementPath);
                }
                if (fields.size() == 1) {
                    extractType = getExtractTypeByField(fields.get(0));
                    if (StringUtils.isEmpty(extractType)) {
                        throw new RuntimeException(
                                "Format of element by path " + elementPath + " doesn't contain extract type");
                    }
                    return extractType;
                }
                String formatPath = elementPath.substring(0, elementPath.lastIndexOf(PATH_DELIMITER));
                if (StringUtils.isEmpty(formatPath)) {
                    throw new RuntimeException("Cannot resolve field for element by path: " + elementPath);
                }
                Map<String, String> extractTypes = new HashMap<>();
                for (ExportFormatField field : fields) {
                    String sifTopic = getFieldSifTopic(field);
                    List<String> paths = m_pathsBySifTopic.get(sifTopic);
                    if (paths == null) {
                        paths = getFormatPaths(sifTopic);
                        m_pathsBySifTopic.put(sifTopic, paths);
                    }
                    for (String path : paths) {
                        if (path.endsWith(formatPath)) {
                            String extractTypeByField = getExtractTypeByField(field);
                            if (StringUtils.isEmpty(extractType)) {
                                throw new RuntimeException(
                                        "Format of element by path " + elementPath + " doesn't contain extract type");
                            }
                            extractTypes.put(extractTypeByField, formatPath + PATH_DELIMITER + field.getSifPath());
                        }
                    }
                }
                if (extractTypes.isEmpty()) {
                    throw new RuntimeException("Cannot resolve field for element by path: " + elementPath);
                }

                if (extractTypes.size() > 1) {
                    StringBuilder extractTypesDesc = new StringBuilder();
                    for (Entry<String, String> extractTypeEntry : extractTypes.entrySet()) {
                        extractTypesDesc.append(extractTypeEntry.getKey());
                        extractTypesDesc.append(" by path ");
                        extractTypesDesc.append(extractTypeEntry.getValue());
                        extractTypesDesc.append("\r\n");
                    }
                    throw new RuntimeException(
                            "Cannot resolve field for element by path: " + elementPath + " between several: "
                                    + extractTypesDesc.toString());
                }
            }

            return extractType;
        }

        /**
         * Gets the format path.
         *
         * @param sifTopic String
         * @return String
         */
        private List<String> getFormatPaths(String sifTopic) {
            if (isRoot(sifTopic)) {
                return Arrays.asList(sifTopic);
            }
            Filter<ExportFormatField> filter = getEffFilterBySifTopic(sifTopic);
            Collection<ExportFormatField> fields = getEffHelper().filter(filter).extract();

            List<String> fieldPaths = new ArrayList<>();
            for (ExportFormatField field : fields) {
                String nextSifTopic = getFieldSifTopic(field);
                for (String deeperPath : getFormatPaths(nextSifTopic)) {
                    fieldPaths.add(String.join(PATH_DELIMITER, deeperPath, field.getSifPath()));
                }
            }
            return fieldPaths;
        }

        /**
         * Gets the eff helper.
         *
         * @return Filterable
         */
        private Filterable<ExportFormatField> getEffHelper() {
            if (m_fieldsHelper == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addBeginsWith(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                        + ExportFormatDefinition.COL_PROCEDURE_ID, ONSIS_FORMAT_ID_PREFIX);
                criteria.addNotBeginsWith(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                        + ExportFormatDefinition.COL_PROCEDURE_ID, ONSIS_FORMAT_ID_PREFIX + "VAL");
                m_fieldsHelper = FilterableFactory.create(m_broker, ExportFormatField.class, criteria);
            }
            return m_fieldsHelper;
        }

        /**
         * Gets the eff filter by path element.
         *
         * @param pathElement String
         * @return Filter
         */
        private Filter<ExportFormatField> getEffFilterByPathElement(final String pathElement) {
            Filter<ExportFormatField> filter = m_filtersByElementName.get(pathElement);
            if (filter == null) {
                filter = new Filter<ExportFormatField>() {
                    @Override
                    public boolean isFiltered(ExportFormatField toFilter) {
                        return pathElement.equals(toFilter.getSifPath());
                    }

                };
                m_filtersByElementName.put(pathElement, filter);
            }
            return filter;
        }

        /**
         * Gets the eff filter by sif topic.
         *
         * @param sifTopic String
         * @return Filter
         */
        private Filter<ExportFormatField> getEffFilterBySifTopic(final String sifTopic) {
            Filter<ExportFormatField> filter = m_filtersBySifTopic.get(sifTopic);
            if (filter == null) {
                filter = new Filter<ExportFormatField>() {
                    @Override
                    public boolean isFiltered(ExportFormatField toFilter) {
                        return sifTopic.equals(extractTopic(toFilter))
                                && (OnsisRetrieverNestedExport.CALC_ID.equals(toFilter.getCalculationId())
                                        || OnsisRetrieverNestedExportStreaming.CALC_ID
                                                .equals(toFilter.getCalculationId()));
                    }

                };
                m_filtersByElementName.put(sifTopic, filter);
            }
            return filter;
        }

        /**
         * Gets the extract type by field.
         *
         * @param field ExportFormatField
         * @return String
         */
        private String getExtractTypeByField(ExportFormatField field) {
            if (OnsisRetrieverNestedExport.CALC_ID.equals(field.getCalculationId())
                    || OnsisRetrieverNestedExportStreaming.CALC_ID.equals(field.getCalculationId())) {
                String topic = extractTopic(field);
                ExportFormatDefinition format =
                        getFormatHelper().extractFirst(ExportFormatDefinition.COL_SIF_TOPIC, topic);
                if (format == null) {
                    throw new RuntimeException(
                            "Cannot find format for nested export by parameter: " + field.getCalcParam());
                }

                return format.getSifProfile();
            }

            String formatOid = field.getDefinitionOid();
            return getFormatHelper().extractFirst(X2BaseBean.COL_OID, formatOid).getSifProfile();
        }

        /**
         * Gets the field sif topic.
         *
         * @param field ExportFormatField
         * @return String
         */
        private String getFieldSifTopic(ExportFormatField field) {
            String definitionOid = field.getDefinitionOid();
            String sifTopic = getFormatHelper().extractFirst(X2BaseBean.COL_OID, definitionOid).getSifTopic();
            if (StringUtils.isEmpty(sifTopic)) {
                throw new RuntimeException("Sif topic is empty!");
            }
            return sifTopic;
        }

        /**
         * Gets the format helper.
         *
         * @return Filterable
         */
        private Filterable<ExportFormatDefinition> getFormatHelper() {
            if (m_formatsHelper == null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addBeginsWith(ExportFormatDefinition.COL_PROCEDURE_ID, ONSIS_FORMAT_ID_PREFIX);
                m_formatsHelper =
                        FilterableFactory.create(m_broker, ExportFormatDefinition.class, criteria);
            }
            return m_formatsHelper;
        }

        /**
         * Checks if is root.
         *
         * @param formatPath String
         * @return true, if is root
         */
        private static boolean isRoot(String formatPath) {
            return OnsisResultsHelper.s_submissionsTypes.contains(formatPath);
        }
    }


    /**
     * The Class SubmissionPeriod.
     */
    public class SubmissionPeriod {
        private String m_description;
        private String m_schoolYear;
        private int m_submissionYear;

        /**
         * Instantiates a new submission period.
         *
         * @param description String
         * @param year int
         */
        public SubmissionPeriod(String description, int year) {
            m_description = description;
            m_submissionYear = year;
            m_schoolYear = String.valueOf(year - 1) + "-" + String.valueOf(year);
        }

        /**
         * Gets the school year.
         *
         * @return String
         */
        public String getSchoolYear() {
            return m_schoolYear;
        }

        /**
         * Gets the period code.
         *
         * @return String
         */
        public String getPeriodCode() {
            return m_description;
        }

        /**
         * Gets the submission year.
         *
         * @return int
         */
        public int getSubmissionYear() {
            return m_submissionYear;
        }

    }

    /**
     * Find csv record.
     *
     * @param extractType String
     * @param keys ArrayList<String>
     * @param values ArrayList<String>
     * @return OnsisCsvDataRecord
     */
    public OnsisCsvDataRecord findCsvRecord(String extractType, ArrayList<String> keys, ArrayList<String> values) {
        OnsisExtractRecords records = getMatcherByExtractType(extractType.toUpperCase());
        return records.findRecord(keys, values);
    }

    /**
     * Gets the export format fields.
     *
     * @param format ExportFormatDefinition
     * @return List
     */
    public List<ExportFormatField> getExportFormatFields(ExportFormatDefinition format) {
        List<ExportFormatField> fields = m_exportFormatFields.get(format);
        if (fields == null) {
            fields = new ArrayList(format.getFields());
            Collections.sort(fields, OnsisExtractHelperUtils.fieldsPositionComparator);
            m_exportFormatFields.put(format, fields);
        }
        return fields;
    }

    /**
     * Gets the export format field by id.
     *
     * @param format the format
     * @param id the id
     * @return the export format field by id
     */
    public ExportFormatField getExportFormatFieldById(ExportFormatDefinition format, String id) {
        Map<String, ExportFormatField> map = m_exportFormatFieldById.get(format);
        if (map == null) {
            map = new HashMap();
            for (ExportFormatField field : getExportFormatFields(format)) {
                map.put(field.getId(), field);
            }
            m_exportFormatFieldById.put(format, map);
        }
        return map.get(id);
    }

    /**
     * Gets the extract types.
     *
     * @return Sets the
     */
    public Set<String> getExtractTypes() {
        return m_matchersByExtractType.keySet();
    }

    /**
     * Gets the first field value.
     *
     * @param fieldName String
     * @return String
     */
    public String getFirstFieldValue(String fieldName) {
        String fieldValue = null;
        for (OnsisExtractRecords matcher : m_matchersByExtractType.values()) {
            if (matcher.getDataRecords().isEmpty()) {
                continue;
            }
            fieldValue = matcher.getDataRecords().get(0).getSingleFieldValue(fieldName);
            if (!StringUtils.isEmpty(fieldValue)) {
                break;
            }
        }
        if (StringUtils.isEmpty(fieldValue)) {
            throw new RuntimeException(
                    fieldName + " is demanded but field \"" + fieldName
                            + "\" cannot be found in the EXSMS *.csv files.");
        }
        return fieldValue;
    }

    /**
     * Gets the first field value.
     *
     * @param fieldName String
     * @return String
     */
    public String getFirstHeaderValue(String fieldName) {
        String fieldValue = null;
        for (OnsisExtractRecords matcher : m_matchersByExtractType.values()) {
            if (matcher.getHeaderRecords().isEmpty()) {
                continue;
            }
            fieldValue = matcher.getHeaderRecords().get(0).getSingleFieldValue(fieldName);
            if (!StringUtils.isEmpty(fieldValue)) {
                break;
            }
        }
        if (StringUtils.isEmpty(fieldValue)) {
            throw new RuntimeException(
                    fieldName + " is demanded but field \"" + fieldName
                            + "\" cannot be found in the EXSMS *.csv files.");
        }
        return fieldValue;
    }

    public static final String KEY_TOPIC = "topic";
    public static final String KEY_PROCEDURE_ID = "procedureId";

    /**
     * Extract topic.
     *
     * @param exportFormatField the export format field
     * @return the string
     */
    public static String extractTopic(ExportFormatField exportFormatField) {
        String topic = exportFormatField.getCalcParam();

        Map<String, String> map = extractCalcParams(exportFormatField);
        if (map.containsKey(KEY_TOPIC)) {
            return map.get(KEY_TOPIC);
        }

        return topic;
    }

    /**
     * Extract calc params.
     *
     * @param exportFormatField the export format field
     * @return the map
     */
    public static Map<String, String> extractCalcParams(ExportFormatField exportFormatField) {
        return jsonToMap(exportFormatField.getCalcParam());
    }

    /**
     * Json to map.
     *
     * @param calcParam the calc param
     * @return the map
     */
    public static Map<String, String> jsonToMap(String calcParam) {
        if (StringUtils.isBlank(calcParam)) {
            return Collections.EMPTY_MAP;
        }

        int pos = calcParam.indexOf("{");
        if (pos < 0) {
            Map<String, String> map = new HashMap<>(1);
            map.put(KEY_TOPIC, calcParam);

            return map;
        }

        ObjectMapper mapper = new ObjectMapper();
        Map<String, String> map;
        try {
            map = mapper.readValue(calcParam, Map.class);
        } catch (IOException e) {
            throw new RuntimeException("Cannot parse parameter: " + calcParam, e);
        }

        return map;
    }

    /**
     * Gets the formats by extract type.
     *
     * @param extractType String
     * @param formats Collection<ExportFormatDefinition>
     * @return List
     */
    public static List<ExportFormatDefinition> getFormatsByExtractType(String extractType,
                                                                       Collection<ExportFormatDefinition> formats) {
        List<ExportFormatDefinition> formatsWithExtractType = new ArrayList<>();
        for (ExportFormatDefinition format : formats) {
            String formatExtractType = format.getSifProfile();
            if (formatExtractType != null) {
                String adjustedFormatExtractType = formatExtractType.trim().toUpperCase();
                String adjustedExtractTypeToCompare = extractType.trim().toUpperCase();
                if (adjustedFormatExtractType.equals(adjustedExtractTypeToCompare)) {
                    formatsWithExtractType.add(format);
                }
            }
        }
        return formatsWithExtractType;
    }

    /**
     * Gets the matcher by extract type.
     *
     * @param extractType String
     * @return Onsis extract records matcher
     */
    public OnsisExtractRecords getMatcherByExtractType(String extractType) {
        String extractTypeUpper = StringUtils.emptyIfNull(extractType).toUpperCase();
        OnsisExtractRecords matcher = m_matchersByExtractType.get(extractTypeUpper);
        if (matcher == null) {
            matcher = generateMatcherByExtractType(extractType, new ArrayList(Collections.EMPTY_LIST));
            m_matchersByExtractType.put(extractType, matcher);
        }
        return matcher;
    }

    /**
     * Gets the onsis elements helper.
     *
     * @return Onsis elements helper
     */
    public OnsisElementsHelper getOnsisElementsHelper() {
        if (m_onsisElementsHelper == null) {
            m_onsisElementsHelper = new OnsisElementsHelper(m_broker);
        }
        return m_onsisElementsHelper;
    }

    /**
     * Gets the submission period.
     *
     * @return Submission period
     */
    public SubmissionPeriod getSubmissionPeriod() {
        if (m_submissionPeriod == null) {
            int periodYear = getSubmissionPeriodYear();
            String periodDescription = getSubmissionPeriodDesc();
            m_submissionPeriod = new SubmissionPeriod(periodDescription, periodYear);
        }
        return m_submissionPeriod;
    }

    /**
     * Initialize matchers.
     *
     * @param zipFile File
     * @param userTempFolder File
     * @param bsids
     * @throws FolderDoesNotExistException exception
     * @throws UniqueFolderNotCreatedException exception
     * @throws ZipException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public void initializeMatchers(File zipFile, Charset charset, File userTempFolder, Set<String> bsids)
            throws FolderDoesNotExistException, UniqueFolderNotCreatedException, ZipException, IOException {
        if (zipFile == null) {
            throw new X2RuntimeException();
        }

        /*
         * Want to end up with one Matcher per format group.
         * Format Group is the <sifprofile> of each EXSMS export format.
         *
         * In the case of Other Course Info:
         * The file is type "STUDENT CLASS ENROLMENT"
         * but OCI needs to use different key fields (i.e. different EFDs)
         * than the OnsisStudentClassEnrolment topic.
         *
         * To handle this, EFD's are grouped into a Matcher
         * based on their <sifprofile>:
         * EXSMS EFD for Student Class Enrolment: sifprofile="STUDENT CLASS ENROLMENT"
         * EXSMS EFD for Other Course Info: sifprofile="STUDENT CLASS ENROLMENT:OCI"
         *
         * The left side of that matches the CSV type.
         * The right side means this format needs to be in a separate matcher.
         *
         * This allows there to be two matchers for the same CSV file
         * with different sets of export formats.
         */

        /*
         * Get a map of extractTypes (e.g. "STUDENT CLASS ENROLMENT:OCI")
         * for each file type ("STUDENT CLASS ENROLMENT")
         */
        Map<String, Collection<String>> extractTypesForFileType = new HashMap<>();
        for (ExportFormatDefinition format : getFormats().values()) {
            if (StringUtils.isBlank(format.getSifProfile())) {
                continue;
            }

            String extractType = format.getSifProfile().trim().toUpperCase();
            String fileType = extractType.contains(":")
                    ? fileType = extractType.substring(0, extractType.indexOf(":"))
                    : extractType;

            Collection<String> extractTypes = extractTypesForFileType.get(fileType);
            if (extractTypes == null) {
                extractTypes = new LinkedHashSet<>();
                extractTypesForFileType.put(fileType, extractTypes);
            }
            extractTypes.add(extractType);
        }

        /*
         * For each CSV fileType, generate a matcher for its extractType(s)
         */
        File workFolder = FolderUtils.createUniqueFolder(userTempFolder);
        ArrayList<File> unzippedFiles = readZipFile(zipFile, workFolder);

        for (File unzippedFile : unzippedFiles) {
            String fileName = unzippedFile.getName().toUpperCase();
            if (!fileName.startsWith(EXSMS_FORMAT_ID_PREFIX)) {
                continue;
            }

            String fileType = getFileType(unzippedFile);
            if (StringUtils.isBlank(fileType)) {
                System.out.println(
                        "EXSMS CSV file missing fileType [" + fileName + "]");
                continue;
            }

            Collection<String> extractTypes = extractTypesForFileType.get(fileType);
            if (extractTypes == null) {
                System.out.println(
                        "EXSMS CSV fileType found [" + fileType + "] with no matching Export Format Definition");
                continue;
            }

            for (String extractType : extractTypes) {
                OnsisExtractRecords matcherToAdd = getMatcher(unzippedFile, charset, extractType, bsids);
                if (matcherToAdd != null) {
                    m_matchersByExtractType.put(matcherToAdd.getExtractType(), matcherToAdd);
                }
            }
        }
        // This is created in user temp data and delete fails from report servers starting in 6.6 so
        // remove delete
        // FileUtils.deleteDirectory(workFolder);
    }

    /**
     * Gets the file type.
     *
     * @param unzippedFile the unzipped file
     * @return the file type
     */
    private static String getFileType(File unzippedFile) {
        try (FileReader fileReader = new FileReader(unzippedFile);
                BufferedReader bufReader = new BufferedReader(fileReader)) {
            String[] firstRow = parseCsvFirstRow(bufReader);
            return getFileType(firstRow);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Gets the extract type.
     *
     * @param firstRow String[]
     * @return String
     */
    private static String getFileType(String[] firstRow) {
        if (firstRow == null || firstRow.length < MIN_COLUMNS_NUMBER
                || !OnsisExtractHelper.H1.equals(firstRow[INDEX_RECORD_TYPE])) {
            throw new X2RuntimeException();
        }

        return firstRow[INDEX_EXTRACT_TYPE].trim().toUpperCase();
    }

    /**
     * Gets the matcher.
     *
     * public for unit tests
     *
     * @param extractFile File
     * @param extractType the extract type
     * @param bsids
     * @return Onsis extract records matcher
     */
    protected OnsisExtractRecords getMatcher(File extractFile, Charset charset, String extractType, Set<String> bsids) {
        try (BufferedReader bufReader =
                Files.newBufferedReader(Paths.get(extractFile.getPath()), charset)) {
            return getMatcherFromReader(bufReader, extractType, bsids);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Gets the matcher from reader.
     *
     * @param reader the reader
     * @param extractType the extract type
     * @param bsids
     * @return the matcher from reader
     */
    public OnsisExtractRecords getMatcherFromReader(Reader reader, String extractType, Set<String> bsids) {
        ArrayList<String[]> rows = parseCsvToRows(reader, bsids);
        if (extractType == null) {
            extractType = getFileType(rows.get(0));
        }

        return generateMatcherByExtractType(extractType, rows);
    }

    /**
     * Sets the matcher.
     *
     * @param extractType the extract type
     * @param matcher the matcher
     */
    public void setMatcher(String extractType, OnsisExtractRecords matcher) {
        m_matchersByExtractType.put(extractType, matcher);
    }

    /**
     * Generate matcher by extract type.
     *
     * @param extractType String
     * @param rows ArrayList<String[]>
     * @return OnsisExtractRecords
     */
    private OnsisExtractRecords generateMatcherByExtractType(String extractType, ArrayList<String[]> rows) {
        List<ExportFormatDefinition> defaultH1 =
                OnsisExtractHelperUtils.getFormatsById(FORMAT_ID_DEFAULT_H1, getFormats());
        List<ExportFormatDefinition> defaultH2 =
                OnsisExtractHelperUtils.getFormatsById(FORMAT_ID_DEFAULT_H2, getFormats());

        List<ExportFormatDefinition> formatsDT = getFormatsByExtractType(extractType, getFormats().values());
        if (formatsDT.isEmpty()) {
            throw new RuntimeException("Cannot find formats for file type: " + extractType
                    + ", please make sure the csv file contains correct file type.");
        }
        String formatsPrefix =
                formatsDT.iterator().next().getProcedureId().substring(INDEX_FORMAT_ID_PREFIX_START,
                        INDEX_FORMAT_ID_PREFIX_END);


        List<ExportFormatDefinition> formatsH1 = getFormatsByExtractType(formatsPrefix + H1, getFormats().values());
        List<ExportFormatDefinition> formatsH2 = getFormatsByExtractType(formatsPrefix + H2, getFormats().values());

        return new OnsisExtractRecords(
                formatsH1.isEmpty() ? defaultH1 : formatsH1,
                formatsH2.isEmpty() ? defaultH2 : formatsH2,
                formatsDT, rows);
    }

    /**
     * Gets the submission period code.
     *
     * @return String
     */
    private String getSubmissionPeriodDesc() {
        return getFirstHeaderValue(OnsisExtractRecords.FIELD_VALUE_SUBMISSION_PERIOD);
    }

    /**
     * Gets the submission period year.
     *
     * @return int
     */
    private int getSubmissionPeriodYear() {
        return Integer.valueOf(getFirstHeaderValue(OnsisExtractRecords.FIELD_VALUE_ACADEMIC_YEAR).substring(5))
                .intValue();
    }

    /**
     * Parses the csv to rows.
     *
     * @param reader Reader
     * @return ArrayList
     */
    private static ArrayList<String[]> parseCsvToRows(Reader reader, Set<String> bsids) {
        ArrayList<String[]> rows = new ArrayList<>();
        try {
            CSVReader csvReader = new CSVReader(reader);
            boolean includeRow = true;

            String[] columns = csvReader.parseLine();
            if (columns != null && columns.length > 0) {
                // Remove byte order mark
                int firstQuote = columns[0].indexOf("\"");
                if (firstQuote >= 0) {
                    columns[0] = columns[0].substring(firstQuote).replace("\"", "");
                }
            }
            while (columns != null && columns.length > 1) {
                if (columns.length > 1 && columns[0].equals("H2")) {
                    includeRow = bsids.contains(columns[1]);
                }
                if (includeRow) {
                    rows.add(columns);
                }
                columns = csvReader.parseLine();
            }
        } catch (FileNotFoundException e) {
            // do nothing
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return rows;
    }

    /**
     * Parses the csv first row.
     *
     * @param reader the reader
     * @return the string[]
     */
    private static String[] parseCsvFirstRow(Reader reader) {
        String[] firstRow = null;
        try {
            CSVReader csvReader = new CSVReader(reader);
            firstRow = csvReader.parseLine();
            if (firstRow != null && firstRow.length > 0) {
                int firstQuote = firstRow[0].indexOf("\"");
                // Remove byte order mark
                if (firstQuote >= 0) {
                    firstRow[0] = firstRow[0].substring(firstQuote).replace("\"", "");
                }
            }
        } catch (FileNotFoundException e) {
            // do nothing
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return firstRow;
    }

    /**
     * Query formats.
     *
     * @return Map
     */
    Map<String, ExportFormatDefinition> getFormats() {
        if (m_formats == null) {
            X2Criteria formatsCriteria = new X2Criteria();
            formatsCriteria.addBeginsWith(ExportFormatDefinition.COL_PROCEDURE_ID,
                    OnsisExtractHelper.EXSMS_FORMAT_ID_PREFIX);
            QueryByCriteria formatsQuery = new QueryByCriteria(ExportFormatDefinition.class, formatsCriteria);
            m_formats = m_broker.getMapByQuery(formatsQuery, ExportFormatDefinition.COL_PROCEDURE_ID, 50);
        }
        return m_formats;
    }

    /**
     * Get zip file from input and unzip it to disk.
     *
     * @param sourceFile File
     * @param targetDirectory File
     * @return list of files from zip
     * @throws ZipException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private static ArrayList<File> readZipFile(File sourceFile, File targetDirectory) throws ZipException, IOException {
        ArrayList<File> files = new ArrayList<File>();

        if (sourceFile != null && sourceFile.exists() && sourceFile.toString().toLowerCase().endsWith(".zip")) {
            ZipUtils.unzipFilesAndFoldersToDisk(sourceFile, targetDirectory, false);

            File[] dirs = targetDirectory.listFiles();
            ArrayList<File> listOfZips = new ArrayList<File>();

            for (File file : dirs) {
                if (file.isDirectory()) {
                    listOfZips.addAll(Arrays.asList(file.listFiles()));
                }
            }

            for (File file : listOfZips) {
                if (file.exists() && file.getName().endsWith(".zip")) {
                    ZipUtils.unzipFilesAndFoldersToDisk(file, targetDirectory, false);
                }
            }
            files.addAll(Arrays.asList(targetDirectory.listFiles()));
        }

        return files;
    }
}
