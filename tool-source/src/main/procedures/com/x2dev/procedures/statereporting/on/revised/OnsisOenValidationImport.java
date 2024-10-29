/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisOenValidationImport.MismatchError.OenMismatchField;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisException;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.Node;

/**
 * The Class OnsisErrorsImport.
 */
public class OnsisOenValidationImport extends ImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class MismatchError.
     */
    protected static class MismatchError {

        /**
         * The Enum OenMismatchField.
         */
        enum OenMismatchField {
            DATE_OF_BIRTH {
                @Override
                public String getAspenValue(OnsisStudent student) {
                    PlainDate dob = student.getDob();
                    if (dob != null) {
                        return m_dobFormatter.format(dob);
                    }
                    return null;
                }

                @Override
                public String getMessageName() {
                    return "Date of Birth";
                }
            },
            GENDER {
                @Override
                public String getAspenValue(OnsisStudent student) {
                    return student.getGenderType();
                }

                @Override
                public String getMessageName() {
                    return "Gender";
                }
            },
            LEGAL_FIRST_NAME {
                @Override
                public String getAspenValue(OnsisStudent student) {
                    return student.getLegalFirstName();
                }

                @Override
                public String getMessageName() {
                    return "Legal First Name";
                }
            },
            LEGAL_SURNAME {
                @Override
                public String getAspenValue(OnsisStudent student) {
                    return student.getLegalLastName();
                }

                @Override
                public String getMessageName() {
                    return "Legal Surname";
                }
            };

            private final static SimpleDateFormat m_dobFormatter = new SimpleDateFormat("yyyy/MM/dd");

            /**
             * Includes.
             *
             * @param elementName String
             * @return true, if successful
             */
            public static boolean includes(String elementName) {
                for (OenMismatchField field : values()) {
                    if (field.toString().equals(elementName)) {
                        return true;
                    }
                }
                return false;
            }

            /**
             * Gets the aspen value.
             *
             * @param student OnsisStudent
             * @return String
             */
            public abstract String getAspenValue(OnsisStudent student);

            /**
             * Gets the message name.
             *
             * @return String
             */
            public abstract String getMessageName();
        }

        /**
         * The Enum Status.
         */
        enum Status {
            MISMATCH, SKIPPED;
        }

        /**
         * Instantiates a new mismatch error.
         *
         * @param student OnsisStudent
         * @param oen String
         */
        public MismatchError(OnsisStudent student, String oen) {
            m_student = student;
            m_oen = oen;
        }

        public static final String DDX_ID = "ON-SIS-OEN-VAL";

        private static final String ALIAS_DDX_CREATION_DATE = "creation-date";
        private static final String ALIAS_DDX_DESCRIPTION = "description";
        private static final String ALIAS_DDX_DETAIL_DESCRIPTION = "detail-description";

        private Map<OenMismatchField, String> m_mismatchFields = new HashMap<>();
        private final String m_oen;
        private School m_school;
        private final OnsisStudent m_student;

        /**
         * Adds the mismatch field.
         *
         * @param field OenMismatchField
         * @param value String
         */
        public void addMismatchField(OenMismatchField field, String value) {
            m_mismatchFields.put(field, value);
        }

        /**
         * Gets the description.
         *
         * @return String
         */
        public String getDescription() {
            return StringUtils.isEmpty(getSchoolBsid()) ? "Validation Mismatch"
                    : "School " + getSchoolBsid() + ", Validation Mismatch";
        }

        /**
         * Generate text messages.
         *
         * @return String
         */
        public String getDetailDescription() {
            StringBuilder builder = new StringBuilder();
            builder.append("OEN: " + m_oen + "\n");
            if (!m_mismatchFields.entrySet().isEmpty()) {
                for (Entry<OenMismatchField, String> entry : m_mismatchFields.entrySet()) {
                    OenMismatchField mismatchField = entry.getKey();
                    String value = entry.getValue();
                    builder.append(mismatchField.getMessageName() + " Mismatch: OEN: " + value + ", Aspen: "
                            + (m_student == null ? "Student not found" : mismatchField.getAspenValue(m_student))
                            + "\n");
                }
            } else {
                builder.append("SKIPPED\n");
            }

            return builder.toString();
        }

        /**
         * Gets the school bsid.
         *
         * @return String
         */
        public String getSchoolBsid() {
            return m_school != null ? (String) m_school.getFieldValueByAlias(ALIAS_SKL_BSID) : null;
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public OnsisStudent getStudent() {
            return m_student;
        }

        /**
         * Save.
         *
         * @param organization Organization
         * @param broker X2Broker
         * @param ddxExtractor DictionaryExtractor
         * @param headerOid
         */
        public void save(Organization organization,
                         X2Broker broker,
                         DictionaryExtractor ddxExtractor,
                         String headerOid) {
            UserDefinedTableB bean = new UserDefinedTableB(broker.getPersistenceKey());
            DataDictionary dictionary = ddxExtractor.getDictionary(DDX_ID);
            bean.setExtendedDataDictionaryOid(dictionary.getExtendedDictionaryOid());
            bean.setFieldValueByAlias(ALIAS_DDX_CREATION_DATE, getCurrentDate(), dictionary);
            bean.setFieldValueByAlias(ALIAS_DDX_DESCRIPTION, getDescription(), dictionary);
            bean.setFieldValueByAlias(ALIAS_DDX_DETAIL_DESCRIPTION, getDetailDescription(), dictionary);
            bean.setStudentOid(m_student.getOid());
            bean.setSchoolOid(m_school != null ? m_school.getOid() : null);
            bean.setUserDefinedTableCOid(headerOid);

            bean.setOrganization1Oid(organization.getOid());
            broker.saveBean(bean);
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(School school) {
            m_school = school;
        }
    }

    private static final String ENCODING_WINDOWS_1252 = "windows-1252";
    private Integer m_counterMismatch = 0;
    private Integer m_counterSkipped = 0;
    private final static SimpleDateFormat s_timestampDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    private DictionaryExtractor m_dictionaryExtractor;
    private String m_headerOid;
    private List<MismatchError> m_mismatchErrors = new ArrayList<>();
    private Set<String> m_oensInvalid = new TreeSet<>();
    private Set<String> m_oensNotFound = new TreeSet<>();
    private Filterable<OnsisStudent> m_stdsFilterable;

    private static final String ALIAS_SKL_BSID = "all-skl-BSID";

    private static final String ELEMENT_NAME_OEN = "OEN";

    private static final String NODE_NAME_OEN_DETAILS = "OEN_DETAILS";

    /**
     * Gets the current date.
     *
     * @return String
     */
    public static String getCurrentDate() {
        return s_timestampDateFormat.format(new Date());
    }

    /**
     * Gets the dictionary extractor.
     *
     * @return Dictionary extractor
     */
    public DictionaryExtractor getDictionaryExtractor() {
        if (m_dictionaryExtractor == null) {
            m_dictionaryExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictionaryExtractor;
    }

    /**
     * Export results.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        //
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
        Charset windows1252 = Charset.forName(ENCODING_WINDOWS_1252);
        try (BufferedReader reader = Files.newBufferedReader(sourceFile.toPath(), windows1252)) {
            StringBuilder sourceFileStringBuilder = new StringBuilder();
            String line = null;
            while ((line = reader.readLine()) != null) {
                sourceFileStringBuilder.append(line);
            }

            boolean isException = false;
            String exceptionString = null;
            getBroker().beginTransaction();
            removePreviousRecords();
            try {
                List<Node> oenNodes =
                        OnsisResultsHelper.getNodesByName(sourceFileStringBuilder.toString(), ELEMENT_NAME_OEN);
                Filterable<Node> oensHelper = FilterableFactory.create(oenNodes,
                        Arrays.asList(ELEMENT_NAME_OEN), new ValueByKeyResolver<Node>() {
                            @Override
                            public Object getValue(String key, Node node) {
                                return OnsisResultsHelper.getValueByElementName(node, key);
                            }
                        });
                Set<String> oens = new HashSet(oensHelper.extract(ELEMENT_NAME_OEN));
                for (String oen : oens) {
                    Node oenNode =
                            oensHelper.extractFirst(ELEMENT_NAME_OEN, oen);
                    Optional<Node> oenDetailsNodeO =
                            OnsisResultsHelper.getOuterNode(oenNode, NODE_NAME_OEN_DETAILS);
                    if (oenDetailsNodeO.isPresent()) {
                        handleOenDetailsNode(oenDetailsNodeO.get());
                    } else {
                        handleInvalidOen(oenNode);
                    }
                }
            } catch (Exception e) {
                isException = true;
                exceptionString = LoggerUtils.convertThrowableToString(e);
            } finally {
                StringBuilder outputMessage = new StringBuilder();
                for (MismatchError mismatch : m_mismatchErrors) {
                    outputMessage.append(mismatch.getDetailDescription());
                    outputMessage.append("\n");
                }

                outputMessage.append("Invalid OENs: " + String.join(", ", m_oensInvalid) + "\n\n");
                outputMessage.append("Not found in Aspen OENs: " + String.join(", ", m_oensNotFound) + "\n\n");

                outputMessage.append(m_counterMismatch + " Mismatch records\n");
                outputMessage.append(m_counterSkipped + " Skipped records");

                String message = outputMessage.toString();
                if (isException) {
                    getBroker().rollbackTransaction();
                    message = exceptionString;

                    removePreviousExceptions();
                    OnsisException exception = new OnsisResultsHelper.OnsisException(getBroker(), getOrganization(),
                            exceptionString, getDictionaryExtractor());
                    exception.getBean().setUserDefinedTableCOid(m_headerOid);
                    getBroker().saveBean(exception.getBean());
                } else {
                    getBroker().commitTransaction();
                }
                try (InputStream in = new ByteArrayInputStream(message.getBytes(StandardCharsets.UTF_8));
                        OutputStream out = getResultHandler().getOutputStream()) {
                    StreamUtils.copyStream(in, out);
                }
            }

        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_headerOid = userData.getCurrentRecord(UserDefinedTableC.class).getOid();
    }

    /**
     * Find student by oen.
     *
     * @param oen String
     * @return OnsisStudent
     */
    private OnsisStudent findStudentByOen(String oen) {
        if (m_stdsFilterable == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addNotEmpty(OnStudent.FIELD_OEN.resolve(getDictionaryExtractor()),
                    getBroker().getPersistenceKey());

            m_stdsFilterable = FilterableFactory
                    .create(getBroker(), OnsisStudent.class, criteria, null);
        }
        OnsisStudent stdReturn = null;
        if (m_stdsFilterable != null) {
            stdReturn = m_stdsFilterable.extractFirst(new Filter() {
                @Override
                public boolean isFiltered(Object toFilter) {
                    OnsisStudent student = (OnsisStudent) toFilter;
                    return Arrays.asList(student.getOen(), student.getOenRaw()).contains(oen);
                }
            });
        }
        return stdReturn;
    }

    /**
     * Handle invalid oen.
     *
     * @param invalidOenNode Node
     */
    private void handleInvalidOen(Node invalidOenNode) {
        String invalidOen = OnsisResultsHelper.getValueByElementName(invalidOenNode, ELEMENT_NAME_OEN);
        m_oensInvalid.add(invalidOen);
        OnsisStudent student = findStudentByOen(invalidOen);
        if (student == null) {
            m_oensNotFound.add(invalidOen);
        }
    }

    /**
     * Handle oen details node.
     *
     * @param oenDetailsNode Node
     */
    private void handleOenDetailsNode(Node oenDetailsNode) {
        MismatchError record = null;
        String oen = OnsisResultsHelper.getValueByElementName(oenDetailsNode, ELEMENT_NAME_OEN);
        OnsisStudent student = findStudentByOen(oen);
        if (student == null) {
            m_oensNotFound.add(oen);
            return;
        }
        for (int i = 0; i < oenDetailsNode.getChildNodes().getLength(); i++) {
            Node mismatchNode = oenDetailsNode.getChildNodes().item(i);
            if (MismatchError.OenMismatchField.includes(mismatchNode.getNodeName())) {
                String mismatchNodeName = mismatchNode.getNodeName();
                OenMismatchField field = OenMismatchField.valueOf(mismatchNodeName);
                String value = OnsisResultsHelper.getValueByElementName(mismatchNode, mismatchNodeName);
                String aspenValue = field.getAspenValue(student);
                if (value != null && !value.equals(aspenValue)) {
                    if (record == null) {
                        record = new MismatchError(student, oen);
                        record.setSchool(getSchool());
                        m_mismatchErrors.add(record);
                    }
                    record.addMismatchField(field, value);
                    m_counterMismatch++;
                }
            }
        }
        if (record == null) {
            m_counterSkipped++;
        } else {
            record.save(getOrganization(), getBroker(), getDictionaryExtractor(), m_headerOid);
        }
    }

    private void removePreviousExceptions() {
        X2Criteria previousExceptionsCriteria = new X2Criteria();
        previousExceptionsCriteria.addEqualTo(UserDefinedTableB.COL_USER_DEFINED_TABLE_C_OID, m_headerOid);
        previousExceptionsCriteria.addEqualTo(UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID,
                getDictionaryExtractor()
                        .getDictionary(OnsisResultsHelper.OnsisException.DDX_ID).getExtendedDictionaryOid());
        getBroker().deleteByQuery(new QueryByCriteria(UserDefinedTableB.class, previousExceptionsCriteria));
    }

    private void removePreviousRecords() {
        X2Criteria headerRecordsCriteria = new X2Criteria();
        headerRecordsCriteria.addEqualTo(UserDefinedTableB.COL_USER_DEFINED_TABLE_C_OID, m_headerOid);

        getBroker().deleteByQuery(new QueryByCriteria(UserDefinedTableB.class, headerRecordsCriteria));
    }
}
