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

import static com.x2dev.procedures.statereporting.on.revised.OnsisConstants.ELEMENTS_PATH_DELIMITER;
import static com.x2dev.procedures.statereporting.on.revised.OnsisConstants.FORMAT_PREFIX_ONSIS;
import static com.x2dev.procedures.statereporting.on.revised.OnsisConstants.INPUT_PARAM_DEBUG_DETAIL;
import com.follett.fsc.core.framework.persistence.CollectionCriteriaHelper;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatField.ReferenceMapTypeCode;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ToolManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.procedures.JarPluginNotFoundException;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanRelationship.LoaderMode;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.PerformanceMonitor;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportValidationError;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.ToolUserDataContainer;
import com.x2dev.procedures.statereporting.on.ResultException;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.FilterablesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisScheduleTeacher;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.CsvField;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractHelperUtils;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.XMLStreamWriterHolder;
import com.x2dev.procedures.statereporting.on.revised.OnsisValidations.OnsisValidator;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections.comparators.ComparatorChain;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;

// TODO: implement a getRootReportData() method that will find and save the root report data

/**
 * The Class OnsisStateReportData.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisStateReportData extends StateReportData {
    /**
     * The Class DebugOutputter.
     */
    public static class DebugOutputter {

        /**
         * Instantiates a new debug outputter.
         */
        public DebugOutputter() {
            //
        }

        /**
         * Gets the transformer.
         *
         * @return Transformer
         */
        protected Transformer getTransformer() {
            Transformer transformer = null;

            try {
                TransformerFactory transformerFactory = TransformerFactory.newInstance();
                transformer = transformerFactory.newTransformer();

                transformer.setOutputProperty(OutputKeys.METHOD, "xml");
                transformer.setOutputProperty(OutputKeys.INDENT, "yes");
                transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
                transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
                transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
                transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            } catch (Exception e) {
                //
            }
            return transformer;
        }

        /**
         * Output string.
         *
         * @param node Node
         * @return String
         */
        public String outputString(Node node) {
            String resultStr = "";

            try {
                DOMSource source = new DOMSource(node);
                StreamResult result = new StreamResult(new StringWriter());
                Transformer transformer = getTransformer();
                transformer.transform(source, result);
                resultStr = result.getWriter().toString();
                // resultStr = resultStr.replaceFirst("\\?><", "\\?>\\n<");
                // resultStr = resultStr.replaceAll("/>\\n", " />\\n");
            } catch (Exception e) {
                //
            }
            return resultStr;
        }

        /**
         * Output string.
         *
         * @param doc Document
         * @param out OutputStream
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public void outputString(org.w3c.dom.Document doc, OutputStream out) throws IOException {
            try {
                Transformer transformer = getTransformer();
                String encoding = transformer.getOutputProperty(OutputKeys.ENCODING);

                transformer.transform(new DOMSource(doc),
                        new StreamResult(new OutputStreamWriter(out, encoding)));
            } catch (UnsupportedEncodingException | TransformerException e) {
                throw new RuntimeException(e);
            }
        }

        /**
         * Output string 1.
         *
         * @param source Node
         * @return String
         */
        public String outputString1(Node source) {
            String subscrXML = null;
            StringWriter stringWriter = new StringWriter();
            try {
                // Get the implementations

                DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();

                DOMImplementationLS impls = (DOMImplementationLS) registry.getDOMImplementation("LS");


                // Prepare the output
                LSOutput domOutput = impls.createLSOutput();
                domOutput.setEncoding(java.nio.charset.Charset.defaultCharset().name());
                domOutput.setCharacterStream(stringWriter);
                domOutput.setEncoding("UTF-8");

                // Prepare the serializer
                LSSerializer domWriter = impls.createLSSerializer();
                DOMConfiguration domConfig = domWriter.getDomConfig();
                domConfig.setParameter("format-pretty-print", true);
                domConfig.setParameter("element-content-whitespace", true);
                domWriter.setNewLine("\r\n");
                domConfig.setParameter("cdata-sections", Boolean.TRUE);

                // And finally, write
                domWriter.write(source, domOutput);
                subscrXML = domOutput.getCharacterStream().toString();

                // Just for curiosity....
                /*
                 * DOMStringList dsl=domConfig.getParameterNames();
                 * for(int i=0;i<dsl.getLength();i){
                 * System.out.println(dsl.item(i)" = ["domConfig.getParameter(dsl.item(i))"]");
                 * }
                 */
            } catch (Exception e) {
                e.printStackTrace();
            }
            return subscrXML;
        }
    }

    /**
     * The Class FieldsRepository.
     */
    public static class FieldsRepository {
        public static final String PATH_CONDUCT =
                "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_INCIDENT";
        public static final String PATH_STAFF =
                "SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT";
        public static final String PATH_STUDENT =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT";
        public static final String PATH_STUDENT_NON_ENROLMENT =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT";
        public static final String PATH_STUDENT_NON_ENROLMENT_DIPLOMA =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/DIPLOMA";
        public static final String PATH_STUDENT_NON_ENROLMENT_E_OPTION =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/E_OPTION_SHEET";
        public static final String PATH_STUDENT_NON_ENROLMENT_SHSM =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SHSM_PROGRAM";
        public static final String PATH_STUDENT_NON_ENROLMENT_SPCE =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_NON_ENROLMENT/SPCE";
        public static final String PATH_STUDENT_ENROLMENT_SALEP =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SALEP";
        public static final String PATH_STUDENT_ENROLMENT_SHSM_CERTIFICATION =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SHSM_PROGRAM/SHSM_CERTIFICATION";
        public static final String PATH_STUDENT_ENROLMENT_SLP =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/SECOND_LANGUAGE_PROGRAM";
        public static final String PATH_STUDENT_TRANSITION_DATE =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/TRANSITION_DATE";
        public static final String PATH_STUDENT_SUBJECT_STRAND =
                "SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/REPORT_CARD/TERM/SUBJECT_STRAND";
        public static final String PATH_PLAR_MATURE_REPORT =
                "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_REPORT";
        public static final String PLAR_MATURE_EQ_COURSES =
                "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COURSES";
        public static final String PLAR_MATURE_EQ_COUNT =
                "SCHOOL_SUBMISSION/SCHOOL/PLAR_MATURE_EQ_COUNT";

        private final String[] m_fields;
        Set<String> m_fieldsSet;

        /**
         * Instantiates a new fields repository.
         *
         * @param fields String
         */
        private FieldsRepository(String fields) {
            fields = fields.replaceAll("[\\t ]", "");
            m_fields = fields.split("\\r\\n|\\n|,");
            Set<String> parents = new HashSet();
            for (String field : m_fields) {
                String[] tokens = field.split("/");
                if (tokens.length > 1) {
                    StringBuilder parent = new StringBuilder();
                    for (int i = 0; i < tokens.length - 1; ++i) {
                        if (i > 0) {
                            parent.append("/");
                        }
                        parent.append(tokens[i]);
                    }
                    if (!parents.contains(parent.toString())) {
                        throw new IllegalStateException(
                                "The fields repository is invalid.  There is no parent for " + field);
                    }
                }
                parents.add(field);
            }
        }

        /**
         * Contains.
         *
         * @param lookupValue String
         * @return true, if successful
         */
        public boolean contains(String lookupValue) {
            if (m_fieldsSet == null) {
                m_fieldsSet = new HashSet(Arrays.asList(getFields()));
            }
            return m_fieldsSet.contains(lookupValue);
        }

        /**
         * Does field exist.
         *
         * @param field String
         * @return true, if successful
         */
        public boolean doesFieldExist(String field) {
            String patternRegexString = "^" + field.replaceAll("/", Matcher.quoteReplacement("\\/")) + "$";
            Pattern pattern = Pattern.compile(patternRegexString);
            for (String oneOfExistingFields : m_fields) {
                if (pattern.matcher(oneOfExistingFields).matches()) {
                    return true;
                }
            }
            return false;
        }

        /**
         * Gets the fields.
         *
         * @return String[]
         */
        public String[] getFields() {
            return m_fields;
        }

        /**
         * Of.
         *
         * @param fields String
         * @return FieldsRepository
         */
        public static FieldsRepository of(String fields) {
            return new FieldsRepository(fields);
        }
    }

    /**
     * The Class NoCsvException.
     */
    public static class NoCsvException extends RuntimeException {

        /**
         * Instantiates a new no export exception.
         *
         * @param message String
         */
        public NoCsvException(String message) {
            super(message);
        }
    }

    /**
     * The Class NoExportException.
     */
    public static class NoExportException extends RuntimeException {

        /**
         * Instantiates a new no export exception.
         *
         * @param message String
         */
        public NoExportException(String message) {
            super(message);
        }
    }


    /**
     * The Class OnsisExtractor.
     */
    public static class OnsisExtractor {
        private X2Broker m_broker;
        private DistrictSchoolYearContext m_context;
        private Map<String, Set<ResultException>> m_exceptions;
        private OnsisExtractHelper m_helper;
        private Set<String> m_missingTags;
        private Map m_parameters;
        private Organization m_organization;
        private OnsisStateReportData m_reportData = null;
        private SubmissionType m_submissionType;
        private String m_topic;
        private ToolUserDataContainer m_userData;
        private OnsisValidator m_validator;

        public static final String PARAMETER_BROKER = "Broker";
        public static final String PARAMETER_CONTEXT = "Context";
        public static final String PARAMETER_EXCEPTIONS = "Exceptions";
        public static final String PARAMETER_EXTRACT_HELPER = "Extract Helper";
        public static final String PARAMETER_INPUT_PARAMETERS = "Input Parameters";
        public static final String PARAMETER_ORGANIZATION = "Organization";
        public static final String PARAMETER_SCHOOL = "School";
        public static final String PARAMETER_SUBMISSION_TYPE = "Submission Type";
        public static final String PARAMETER_TOPIC = "Topic";
        public static final String PARAMETER_XML_WRITER = "Xml Writer";
        public static final String PARAMETER_ZONE_ID = "Zone Id";

        /**
         * Instantiates a new onsis extractor.
         *
         */
        public OnsisExtractor() {
            //
        }

        /**
         * Broker.
         *
         * @param broker X2Broker
         * @return OnsisExtractor
         */
        public OnsisExtractor setBroker(X2Broker broker) {
            m_broker = Objects.requireNonNull(broker);
            return this;
        }

        /**
         * Context.
         *
         * @param context DistrictSchoolYearContext
         * @return OnsisExtractor
         */
        public OnsisExtractor setContext(DistrictSchoolYearContext context) {
            m_context = context;
            return this;
        }

        /**
         * Exceptions.
         *
         * @param exceptions Map<String,Set<String>>
         * @return OnsisExtractor
         */
        public OnsisExtractor setExceptions(Map<String, Set<ResultException>> exceptions) {
            m_exceptions = exceptions;
            return this;
        }

        /**
         * Extract helper.
         *
         * @param helper OnsisExtractHelper
         * @return OnsisExtractor
         */
        public OnsisExtractor setExtractHelper(OnsisExtractHelper helper) {
            m_helper = helper;
            return this;
        }

        /**
         * Missing tags.
         *
         * @param missingTags Set<String>
         * @return OnsisExtractor
         */
        public OnsisExtractor setMissingTags(Set<String> missingTags) {
            m_missingTags = missingTags;
            return this;
        }

        /**
         * Parameters.
         *
         * @param parameters Map
         * @return OnsisExtractor
         */
        public OnsisExtractor setParameters(Map parameters) {
            m_parameters = parameters;
            return this;
        }

        /**
         * Organization.
         *
         * @param organization Organization
         * @return OnsisExtractor
         */
        public OnsisExtractor setOrganization(Organization organization) {
            m_organization = organization;
            return this;
        }

        /**
         * Submission type.
         *
         * @param submissionType SubmissionType
         * @return OnsisExtractor
         */
        public OnsisExtractor setSubmissionType(SubmissionType submissionType) {
            m_submissionType = submissionType;
            return this;
        }

        /**
         * Topic.
         *
         * @param topic String
         * @return OnsisExtractor
         */
        public OnsisExtractor setTopic(String topic) {
            m_topic = topic;
            return this;
        }

        /**
         * User data.
         *
         * @param data OnsisUserDataContainer
         * @return OnsisExtractor
         */
        public OnsisExtractor setUserData(ToolUserDataContainer data) {
            m_userData = data;
            return this;
        }

        /**
         * Validator.
         *
         * @param validations OnsisValidations
         * @return OnsisExtractor
         */
        public OnsisExtractor setValidator(OnsisValidator validations) {
            m_validator = validations;
            return this;
        }

        /**
         * Extract onsis.
         *
         * @param xmlHolder XMLStreamWriterHolder
         * @param parentElement Element
         * @throws JarPluginNotFoundException exception
         * @throws X2BaseException exception
         * @throws XMLStreamException exception
         */
        public void extractOnsis(XMLStreamWriterHolder xmlHolder, Element parentElement)
                throws JarPluginNotFoundException, X2BaseException, XMLStreamException {
            Collection<StateReportValidationError> errors = new ArrayList<>();
            try {
                m_broker.beginSession();

                ToolBean.DistrictManager.setOrganization(m_organization);
                DistrictManager.setDefaultCalendarIds(Arrays.asList(OnsisConstants.DEFAULT_ONSIS_CALENDAR_IDS));
                DistrictManager.setAnnualSpanFactory(new OnsisAnnualSpanFactory(m_broker));

                DistrictManager.setStudentScheduleSpanFactory(new OnsisStudentScheduleSpanFactory());

                ToolBean.registerClass(FteMonthly.class);
                ToolBean.registerClass(OnAddress.class);
                ToolBean.registerClass(OnConductAction.class);
                ToolBean.registerClass(OnEnrollment.class);
                ToolBean.registerClass(OnGradebookColumnDefinition.class);
                ToolBean.registerClass(OnGradebookScore.class);
                ToolBean.registerClass(OnGraduationStudentProgram.class);
                ToolBean.registerClass(OnGraduationStudentWaiver.class);
                ToolBean.registerClass(OnOrganization.class);
                ToolBean.registerClass(OnRubricAssessmentPerformance.class);
                ToolBean.registerClass(OnRubricCriterion.class);
                ToolBean.registerClass(OnSchedule.class);
                ToolBean.registerClass(OnScheduleBellPeriod.class);
                ToolBean.registerClass(OnsisScheduleTeacher.class);
                ToolBean.registerClass(OnSchool.class);
                ToolBean.registerClass(OnSchoolIncident.class);
                ToolBean.registerClass(OnSection.class);
                ToolBean.registerClass(OnStaff.class);
                ToolBean.registerClass(OnStaffPosition.class);
                ToolBean.registerClass(OnsisStudent.class);
                ToolBean.registerClass(OnStudentAttendance.class);
                ToolBean.registerClass(OnStudentSchool.class);
                ToolBean.registerClass(OnTranscript.class);
                ToolBean.registerClass(OnTranscriptRubric.class);
                ToolBean.registerClass(OnTranscriptColumnDefinition.class);

                if (m_reportData == null) {
                    m_reportData =
                            OnsisStateReportData.loadPluginStateReportData(m_topic, null, errors, m_broker, m_userData);
                    // catch initialize exceptions
                    m_reportData.getGlobalData()
                            .setBroker(m_broker)
                            .setExceptions(m_exceptions);
                    m_reportData.initializeExport();
                }

                ToolBean.setBroker(m_broker);
                ToolBean.setDictionaryExtractor(m_reportData.getGlobalData().getDictionaryExtractor());

                m_reportData.getGlobalData()
                        .setCurrentContext(m_context)
                        .setOrganization(m_organization)
                        .setParameters(m_parameters)
                        .setSubmissionType(m_submissionType)
                        .setExceptions(m_exceptions)
                        .setMissingTags(m_missingTags)
                        .setUserData(m_userData)
                        .setValidator(m_validator)
                        .setExtractHelper(m_helper);

                ToolBean.setPreference(PREFERENCE_GLOBAL_DATA, m_reportData.getGlobalData());
                ToolBean.setPreference(ToolBean.PREFERENCE_CURRENT_CONTEXT, m_context);

                if (m_broker instanceof ModelBroker) {
                    PrivilegeSet privilegeSet = ((ModelBroker) m_broker).getPrivilegeSet();
                    m_reportData.setPrivilegeSet(privilegeSet);
                }

                m_reportData.buildBeans();
                if (xmlHolder != null) {
                    m_reportData.renderReportDataToStream(xmlHolder, parentElement, RenderGranularity.FIELD);
                } else {
                    m_reportData.renderReportData(parentElement);
                }

            } catch (Exception ex) {
                String schoolOid = null;

                try {
                    String schoolOids =
                            (String) m_reportData.getGlobalData().getParameter("schoolOids");
                    schoolOid = Arrays.asList(schoolOids.split(",")).get(0);
                } catch (Exception e) {
                    // use null school oid if cannot be determined
                }

                OnsisStateReportData.storeException(m_exceptions, "extractOnsis", schoolOid, ex);
            } finally {
                m_broker.endSession();
                if (m_reportData != null) {
                    m_reportData.close();
                }
            }

            // System.out.println(m_reportData.getGlobalData().getFieldPerformanceMonitor().toString());
            // System.out.println(ToolBean.getCachedCounts());
        }

        /**
         * Write document.
         *
         * @param document Document
         * @param writer Writer
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public void writeDocument(org.w3c.dom.Document document, Writer writer) throws IOException {
            String resultToPrint =
                    // new XMLOutputter(
                    // org.jdom.output.Format.getPrettyFormat().setEncoding(OnsisXmlBatchFile.ENCODING_UTF_8))
                    // .outputString(document);
                    new DebugOutputter().outputString(document);
            writer.append(resultToPrint);
            writer.flush();
        }

        /**
         * Gets the report data.
         *
         * @return Onsis state report data
         */
        public OnsisStateReportData getReportData() {
            return m_reportData;
        }
    }

    /**
     * The Class OnsisHelper.
     */
    public static class OnsisHelper {
        public static final String KEY_SEP = "|";
        /*
         * Constants
         */
        public static final String PATH_CALC_SEPARATOR = "$";
        public static final String PATH_DDX_ID_SEPARATOR = "@";
        public static final String PATH_REGEX_START = "REG(";
        public static final String PATH_REGEX_END = ")REG";

        /*
         * propertyPath node separator (;)
         *
         * Using a char different from ModelProperty.PATH_SEPARATOR ('.')
         * allows treating a long beanPath as one path node (optimization).
         *
         * E.g. a propertyPath of "getFirstActiveEnrollment;student.person.dob"
         * has two propertyPath nodes. The second node ("student.person.dob") can be
         * evaluated in a single call to getFieldValueByBeanPath
         */
        public static final char PATH_NODE_SEP = ';';
        /*
         * A propertyPath can optionally be suffixed with "#STATE", "#FEDERAL" etc
         */
        public static final String PATH_REF_SEP = "#";

        /*
         * Members
         */
        private SimpleDateFormat m_onsisDateFormat = null;
        private HashMap<String, Method> m_getters = null;

        /**
         * String compare with null allowed.
         *
         * @param str1 String
         * @param str2 String
         * @return int
         */
        public static int compareTo(String str1, String str2) {
            String nonNullStr1 = StringUtils.emptyIfNull(str1);
            String nonNullStr2 = StringUtils.emptyIfNull(str2);
            return nonNullStr1.compareTo(nonNullStr2);
        }

        /**
         * Compare start dates.
         *
         * @param a PlainDate
         * @param b PlainDate
         * @return int
         */
        public static int compareStartDates(PlainDate a, PlainDate b) {
            boolean nullIsOld = true;
            return compareDates(a, b, nullIsOld);
        }

        /**
         * Compare end dates.
         *
         * @param a PlainDate
         * @param b PlainDate
         * @return int
         */
        public static int compareEndDates(PlainDate a, PlainDate b) {
            boolean nullIsOld = false;
            return compareDates(a, b, nullIsOld);
        }

        /**
         * Return negative if a < b, 0 if equal, and positive if a > b.
         *
         * @param a PlainDate
         * @param b PlainDate
         * @param nullIsOld Treat a null date as infinitely old
         * @return int
         */
        public static int compareDates(PlainDate a, PlainDate b, boolean nullIsOld) {
            if (a == null && b == null) {
                return 0;
            }

            if (a == null && b != null) {
                if (nullIsOld) {
                    return -1;
                }

                return 1;
            }

            if (a != null && b == null) {
                if (nullIsOld) {
                    return 1;
                }

                return -1;
            }

            return a.compareTo(b);
        }

        /**
         * Format validation errors.
         *
         * @param errors Collection<StateReportValidationError>
         * @return String
         */
        public String formatValidationErrors(Collection<StateReportValidationError> errors) {
            StringBuilder comment = new StringBuilder();
            String lastName = "";
            if (errors != null && errors.size() > 0) {
                for (StateReportValidationError err : errors) {
                    String thisName = err.getEntityName();
                    if (!lastName.equals(thisName)) {
                        comment.append(err.getEntityName());
                        comment.append("\n");
                        lastName = thisName;
                    }
                    comment.append("    ");
                    comment.append(err.getFieldName());
                    comment.append("   ");
                    comment.append(err.getErrorId());
                    comment.append("   ");
                    comment.append(err.getErrorMessage());
                    comment.append("\n");
                }

            }

            return comment.toString();
        }

        /**
         * Gets the property path from object.
         *
         * @param originalSourceObject Object
         * @param propertyPath String
         * @return Object[] {currentObject, previousObject};
         * @throws X2BaseException exception
         * @throws NoSuchMethodException exception
         * @throws SecurityException exception
         * @throws IllegalAccessException exception
         * @throws IllegalArgumentException exception
         * @throws InvocationTargetException exception
         */
        public Object[] getPropertyPathFromObject(Object originalSourceObject, String propertyPath)
                throws X2BaseException, NoSuchMethodException, SecurityException, IllegalAccessException,
                IllegalArgumentException, InvocationTargetException {
            return getPropertyPathFromObject(originalSourceObject, propertyPath, null);
        }

        /**
         * Gets the property path from object.
         *
         * @param originalSourceObject Object
         * @param propertyPath String
         * @param dictionary DataDictionary
         * @return Object[] {currentObject, previousObject};
         * @throws X2BaseException exception
         * @throws NoSuchMethodException exception
         * @throws SecurityException exception
         * @throws IllegalAccessException exception
         * @throws IllegalArgumentException exception
         * @throws InvocationTargetException exception
         */
        public Object[] getPropertyPathFromObject(Object originalSourceObject,
                                                  String propertyPath,
                                                  DataDictionary dictionary)
                throws X2BaseException, NoSuchMethodException, SecurityException, IllegalAccessException,
                IllegalArgumentException, InvocationTargetException {
            if (originalSourceObject == null) {
                return null;
            }
            if (StringUtils.isEmpty(propertyPath)) {
                return null;
            }

            String[] firstAndReamaining = splitFirstNode(propertyPath);
            String currentPathNode = firstAndReamaining[0];
            String remainingPath = firstAndReamaining[1];

            Object sourceObject = originalSourceObject;
            Object result = null;

            while (sourceObject != null && !StringUtils.isEmpty(currentPathNode)) {
                /*
                 * Get the property/getter/alias from currentObject
                 */
                if (sourceObject instanceof X2BaseBean) {
                    if (currentPathNode.startsWith("[")) {
                        // X2BaseBean: Retrieve value from alias
                        result = getResultFromAlias(sourceObject, currentPathNode, dictionary);
                    } else {
                        // X2BaseBean: Retrieve result from beanPath
                        result = ((X2BaseBean) sourceObject).getFieldValueByBeanPath(currentPathNode);
                    }
                } else {
                    // Not an X2BaseBean: Retrieve result from regex or getter
                    if (currentPathNode.startsWith(PATH_REGEX_START)) {
                        result = getResultFromRegex(sourceObject.toString(), currentPathNode);
                    } else {
                        result = getResultFromGetter(sourceObject, currentPathNode);
                    }
                }

                // If null result of current sourceObject, return null even if remainingPath exists.
                if (result == null) {
                    // if (!StringUtils.isEmpty(remainingPath)) {
                    // throw new RuntimeException("intermediate path node returned null.
                    // sourceObject ["
                    // + sourceObject
                    // + "] currentNode [" + currentPathNode
                    // + "] remainingPath [" + remainingPath + "]");
                    // }
                    return new Object[] {result, sourceObject};
                }

                // No more nodes: return retrieved object
                if (StringUtils.isEmpty(remainingPath)) {
                    return new Object[] {result, sourceObject};
                }

                // Continue to next node
                firstAndReamaining = splitFirstNode(remainingPath);
                currentPathNode = firstAndReamaining[0];
                remainingPath = firstAndReamaining[1];
                sourceObject = result;
            }


            // Invalid chain
            String message = "Invalid propertyPath [" + propertyPath + "] for Object [" + sourceObject + "]";
            throw new RuntimeException(message);
        }

        /**
         * Split a path into [firstNode, remainingPath].
         *
         * @param path String
         * @return String[firstNode, remainingPath]
         */
        private String[] splitFirstNode(String path) {
            String firstNode = path;
            String remainingPath = null;

            int secondNodeStart = -1;

            /*
             * nodes are separated by semicolon ";"
             * A special node type "replaceAll" might contain a semicolon so check for that first
             */
            boolean isReplaceAll = path.startsWith(PATH_REGEX_END);
            String terminator = isReplaceAll ? PATH_REGEX_END + ";" : "" + PATH_NODE_SEP;
            int terminatorX = path.indexOf(terminator);

            if (terminatorX >= 0) {
                secondNodeStart = terminatorX + terminator.length();
                if (secondNodeStart >= 0) {
                    firstNode = firstNode.substring(0, terminatorX);
                    remainingPath = path.substring(secondNodeStart).trim();
                }
            }

            firstNode = firstNode.trim();

            return new String[] {firstNode, remainingPath};
        }

        /**
         * Gets the result from alias.
         *
         * @param bean Object
         * @param alias String
         * @param dictionary DataDictionary
         * @return Object
         */
        private Object getResultFromAlias(Object bean, String alias, DataDictionary dictionary) {
            if (!(bean instanceof X2BaseBean)) {
                return null;
            }

            if (StringUtils.isEmpty(alias)) {
                return null;
            }

            if (!alias.startsWith("[") || !alias.endsWith("]")) {
                return null;
            }

            alias = alias.substring(1, alias.length() - 1);

            Object aliasResult = (dictionary == null)
                    ? ((X2BaseBean) bean).getFieldValueByAlias(alias)
                    : ((X2BaseBean) bean).getFieldValueByAlias(alias, dictionary);

            return aliasResult;
        }

        /**
         * Gets the result from getter.
         *
         * @param entityRowBean Object
         * @param propertyName String
         * @return Object
         * @throws NoSuchMethodException exception
         * @throws SecurityException exception
         * @throws IllegalAccessException exception
         * @throws IllegalArgumentException exception
         * @throws InvocationTargetException exception
         */
        public Object getResultFromGetter(Object entityRowBean, String propertyName)
                throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException,
                InvocationTargetException {
            Object getterResult = null;

            if (entityRowBean == null) {
                throw new RuntimeException("Null entityRowBean while retrieving property [" + propertyName + "]");
                // return null;
            }

            if (m_getters == null) {
                m_getters = new HashMap<String, Method>();
            }

            Class clazz = entityRowBean.getClass();
            String getterName = propertyName;

            Method method = null;
            String key = clazz.getName() + "." + getterName;
            if (m_getters.containsKey(key)) {
                method = m_getters.get(key);
            } else {
                long startTime = System.currentTimeMillis();
                method = clazz.getMethod(getterName);
                OnsisConstants.accumulateFrom("clazz.getMethod", startTime);
                m_getters.put(key, method);
            }

            if (method != null) {
                long startTime = System.currentTimeMillis();
                getterResult = method.invoke(entityRowBean);
                OnsisConstants.accumulateFrom(key, startTime);
            }

            return getterResult;
        }

        /**
         * Gets the result from regex.
         *
         * @param sourceObject String
         * @param currentPathNode String
         * @return Object
         */
        public Object getResultFromRegex(String sourceObject, String currentPathNode) {
            if (sourceObject == null) {
                return null;
            }

            if (StringUtils.isBlank(currentPathNode)) {
                return null;
            }

            currentPathNode = currentPathNode.trim();

            if (currentPathNode.endsWith(";")) {
                currentPathNode = currentPathNode.substring(0, currentPathNode.length() - 1);
            }

            if (!currentPathNode.startsWith(PATH_REGEX_START) || !currentPathNode.endsWith(PATH_REGEX_END)) {
                return null;
            }

            // remove ends
            currentPathNode = currentPathNode.substring(PATH_REGEX_START.length(),
                    currentPathNode.length() - PATH_REGEX_END.length());

            /*
             * parse currentPathNode into String regex, String replacement.
             * Should be
             */
            String[] regexAndReplace = parseRegex(currentPathNode);

            if (regexAndReplace.length != 2) {
                return null;
            }

            String regex = regexAndReplace[0];
            String replacement = regexAndReplace[1];
            String result = sourceObject.replaceAll(regex, replacement);

            return result;
        }

        /**
         * Parse currentPathNode into String regex, String replacement.
         *
         * Input: delim + regexString + delim + replacementString + delim
         *
         * delim can be any char that isn't used in regexString or replacementString
         *
         * Example where delim='/'
         * /[aA]/B/
         *
         * @param currentPathNode String
         * @return String[regexStr, replacementStr]
         */
        protected String[] parseRegex(String currentPathNode) {
            if (StringUtils.isBlank(currentPathNode)) {
                return new String[] {"", ""};
            }
            char delim = currentPathNode.charAt(0);

            ArrayList<String> regexAndReplacement = StringUtils.convertDelimitedStringToList(currentPathNode, delim);

            String regex = (regexAndReplacement.size() < 1) ? "" : regexAndReplacement.get(0);
            String replacement = (regexAndReplacement.size() < 2) ? "" : regexAndReplacement.get(1);

            return new String[] {regex, replacement};
        }

        /**
         * Generate OnsisXml... class name from TAG_NAME
         *
         * @param tagName String
         * @return String
         */
        public String tagNameToClassName(String tagName) {
            StringBuffer b = new StringBuffer();
            Matcher m = Pattern.compile("_(.)").matcher(tagName);
            while (m.find()) {
                m.appendReplacement(b, m.group(1).toUpperCase());
            }

            m.appendTail(b);

            String className = "OnsisXml" + StringUtils.capitalize(b.toString());

            return className;
        }

        // public OnsisStateReportData loadOnsisStateReportData(String topic,
        // String sifProfile,
        // Collection<StateReportValidationError> errors,
        // X2Broker broker) {
        // ExportFormatDefinition formatDef = getExportFormatForTopicAndProfile(topic, sifProfile,
        // broker);
        //
        // if (formatDef == null) {
        // throw new NoExportException(
        // "Unable to get export format for topic [" + topic + "] profile [" + sifProfile + "]");
        // }
        //
        // return (OnsisStateReportData) loadStateReportData(formatDef, errors, broker);
        // }
    }

    /**
     * The Class OnsisStateReportEntity.
     */
    public static class OnsisStateReportEntity extends StateReportEntity {
        protected boolean m_isRowCanceled = false;
        protected Set<String> m_tempGeneratedRowKeys = new HashSet<>();

        /**
         * Gets the csv date.
         *
         * @param csvRecord OnsisCsvDataRecord
         * @param fieldName CsvField
         * @return Plain date
         */
        public PlainDate getCsvDate(OnsisCsvDataRecord csvRecord, OnsisExtractHelper.CsvField fieldName) {
            if (csvRecord == null) {
                return null;
            }

            String csvValue = csvRecord.getSingleFieldValue(fieldName);
            if (StringUtils.isBlank(csvValue)) {
                return null;
            }

            Date date = null;
            try {
                /*
                 * Reality check on CSV date
                 * because sometimes a csv has been manually edited using a spreadsheet
                 * and the dates are saved in the wrong format.
                 *
                 * Should be: "yyyy/MM/dd"
                 * Received: 9/6/2016
                 */
                if (!(csvValue.startsWith("19") || csvValue.startsWith("20"))
                        || csvValue.indexOf('/') != 4) {
                    throw new RuntimeException("Invalid CSV date format: [" + csvValue
                            + "]. Expected format: " + OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.toPattern());
                }
                date = OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.parse(csvValue);
            } catch (ParseException e) {
                throw new X2RuntimeException(e);
            }

            return (date == null)
                    ? null
                    : new PlainDate(date);
        }

        /**
         * Gets the debug info.
         *
         * @return String
         */
        public String getDebugInfo() {
            String debugInfo = "";
            if (getReportData() != null && getReportData().getParentEntity() != null) {
                try {
                    debugInfo = getReportData().getParentEntity().getDebugInfo() + "\n";
                } catch (Exception e) {
                    //
                }
            }

            String myDebugInfo = null;
            try {
                myDebugInfo = getMyDebugInfo();
            } catch (Exception e) {
                myDebugInfo = this.getClass().getSimpleName();
            }

            return debugInfo + myDebugInfo;
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         */
        public DictionaryExtractor getDictionaryExtractor() {
            return getGlobalData().getDictionaryExtractor();
        }

        /**
         * Gets the current element name.
         *
         * @return String
         */
        public String getElementName() {
            String sifTopic = getReportData().getSifTopic(getReportData().getProcedureId());
            String[] formatPath = getReportData().getFormatPath(sifTopic).split("/");
            return formatPath[formatPath.length - 1];
        }

        /**
         * Gets the global data.
         *
         * @return Global data
         */
        public GlobalData getGlobalData() {
            return getReportData().getGlobalData();
        }

        /**
         * Get value from entity bean.
         *
         * @param beanPath String
         * @return String
         */
        public String getFieldValueByBeanPath(String beanPath) {
            ToolBean entityObject = getBean();

            return (String) entityObject.getFieldValueByColumnName(beanPath);
        }

        /**
         * Gets the my debug info.
         *
         * @return String
         */
        public String getMyDebugInfo() {
            String debugInfo = StringUtils.emptyIfNull(getElementName());
            String entityIds = getEntityName();
            if (!StringUtils.isBlank(entityIds)) {
                debugInfo += " [" + entityIds + "]";
            }
            return debugInfo;
        }

        /**
         * Gets the report data.
         *
         * @return Onsis state report data
         */
        public OnsisStateReportData getReportData() {
            return (OnsisStateReportData) getData();
        }

        /**
         * Render the current entity row as an XML entity.
         *
         * @param xmlHolder XMLStreamWriterHolder
         * @param emptyParentElement Element
         * @throws Exception exception
         */
        public void renderEntityRow(XMLStreamWriterHolder xmlHolder, Element emptyParentElement) throws Exception {
            /*
             * ReportData classes (topics) may not implement this method if:
             * - they have a Action retriever (due to sorting Deletes before Adds requirement)
             * - they override isRowCanceled
             * - they override afterRenderRowFields
             *
             * Implementing this streamable version of renderEntityRow must do this:
             * - Write start tag to stream
             * - Render all contained field definitions to the stream
             * - Write end tag to stream
             */
            if (getReportData().hasActionTag()) {
                throw new RuntimeException(
                        "Streaming output not supported for topics containing an <ACTION> tag due to sortElements requirement");
            }

            if (isCancelable()) {
                throw new RuntimeException("Streaming output not supported for topics that implement isCancelable");
            }

            setCanceled(false);
            getTempRowKeys().clear();
            List<FieldDefinition> fieldDefinitions = getReportData().getFilteredFieldDefinitions();

            Element entityElement = getReportData().createDefaultElement(emptyParentElement.getOwnerDocument());
            emptyParentElement.appendChild(entityElement);

            /*
             * Stream start of entity
             */
            xmlHolder.writeRawNextLine("");
            xmlHolder.xmlWriter.writeStartElement(entityElement.getTagName());

            try {
                /*
                 * Stream all defined fields
                 */
                for (FieldDefinition fieldDefinition : fieldDefinitions) {

                    getReportData().getGlobalData().pushFieldDefinition(fieldDefinition);

                    try {
                        /*
                         * We need to stream this topic.
                         * If the underlying field supports streaming,
                         * call its streaming render method.
                         *
                         * Otherwise, render it as a DOM Element then stream it
                         */
                        renderField(xmlHolder, fieldDefinition, entityElement);

                    } catch (Exception e) {
                        throw e;
                    } finally {
                        getReportData().getGlobalData().popFieldDefinition();
                    }
                }
            } finally {
                /*
                 * Stream end of entity
                 */
                xmlHolder.writeRawNextLine("");
                xmlHolder.xmlWriter.writeEndElement();
            }
        }

        /**
         * Render the current entity row as an XML entity.
         *
         * @param parentElement Element
         * @return Element
         * @throws Exception exception
         */
        public Element renderEntityRow(Element parentElement) throws Exception {
            /*
             * An entity row can cancel itself after it has generated partial content.
             */
            setCanceled(false);

            /*
             * Every possible exported row key for this entity row
             * is written to tempRowKeys by OnsisRetrieverAction.getActionValue.
             *
             * Note that if a topic substitutes its own retriever for Action,
             * that retriever must generate
             *
             * Then at the end of this method, if the entity row isn't canceled,
             * tempRowKeys are added to generatedRowKeys.
             *
             * generatedRowKeys is queried by generateDeletes at the end of the topic export
             * when determining which deletes to generate.
             */
            getTempRowKeys().clear();

            List<FieldDefinition> fieldDefinitions = getReportData().getFilteredFieldDefinitions();

            Element entityElement = getReportData().createDefaultElement(parentElement.getOwnerDocument());

            /*
             * Add the element to the DOM tree
             * before rendering individual fields
             * so that rendering and isRowCanceled
             * can read anything in the DOM
             * they might need for calculation.
             */
            parentElement.appendChild(entityElement);
            boolean performCancel = true;

            try {

                /*
                 * Before render children
                 */
                beforeRenderRowFields(entityElement);

                /*
                 * Loop through all defined fields
                 */
                try {
                    for (FieldDefinition fieldDefinition : fieldDefinitions) {

                        getReportData().getGlobalData().pushFieldDefinition(fieldDefinition);

                        try {
                            renderField(null, fieldDefinition, entityElement);
                        } catch (Exception e) {
                            throw e;
                        } finally {
                            getReportData().getGlobalData().popFieldDefinition();
                        }
                    }

                    afterRenderRowFields(entityElement);
                } catch (Exception e) {
                    throw e;
                }

                performCancel =
                        isCancelable() && !isSkipCancel(entityElement) && isRowCanceled(entityElement, parentElement);
                // } catch (Throwable t) {
            } finally {
                /*
                 * If canceled, remove the element from the DOM tree.
                 */
                if (performCancel) {
                    // Canceled. Remove from DOM tree. Don't return the entityElement
                    parentElement.removeChild(entityElement);
                    entityElement = null;
                }
            }

            if (!performCancel) {
                // Not canceled. Write tempRowKeys to generated list
                for (String rowKey : getTempRowKeys()) {
                    getReportData().getGlobalData().addGeneratedRowKey(rowKey,
                            getReportData().getProcedureId());
                }
            }

            getTempRowKeys().clear();

            return entityElement;
        }

        /**
         * After render row fields.
         *
         * @param entityElement Element
         */
        protected void afterRenderRowFields(Element entityElement) {
            //
        }

        /**
         * Before render row fields.
         *
         * @param entityElement Element
         */
        protected void beforeRenderRowFields(Element entityElement) {
            //
        }

        /**
         * Deep get field value by element name.
         *
         * @param elementName String
         * @return String
         */
        protected String deepGetFieldValueByElementName(String elementName) {
            String value = getFieldValueBySifPath(elementName);
            if (value != null) {
                return value;
            }
            return getReportData().getFieldValueByElementNameFromAncestor(elementName);
        }

        /**
         * Deep get field value by field name.
         *
         * @param fieldName String
         * @return String
         */
        protected String deepGetFieldValueByFieldName(String fieldName) {
            String value = getFieldValue(fieldName);
            if (value != null) {
                return value;
            }
            return getReportData().getFieldValueByFieldNameFromAncestor(fieldName);
        }

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         */
        protected X2Broker getBroker() {
            return getReportData().getBroker();
        }

        /**
         * Gets the temp row keys.
         *
         * @return Sets the
         */
        protected Set<String> getTempRowKeys() {
            return m_tempGeneratedRowKeys;
        }

        /**
         * Checks if is cancelable.
         *
         * @return true, if is cancelable
         */
        protected boolean isCancelable() {
            return false;
        }

        /**
         * Checks if is row canceled.
         *
         * @param entityElement Element
         * @param parentElement Element
         * @return true, if is row canceled
         */
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            return m_isRowCanceled;
        }

        /**
         * Render field.
         *
         * @param xmlHolder XMLStreamWriterHolder
         * @param fieldDefinition FieldDefinition
         * @param parentElement Element
         * @throws X2BaseException exception
         * @throws XMLStreamException exception
         * @throws IOException Signals that an I/O exception has occurred.
         */
        protected void renderField(XMLStreamWriterHolder xmlHolder,
                                   FieldDefinition fieldDefinition,
                                   Element parentElement)
                throws X2BaseException, XMLStreamException, IOException {
            /*
             * A streaming caller will provide xmlHolder.
             * A non-streaming (DOM-based) caller will call with xmlHolder==null.
             *
             * If caller is streaming, this method must render to the stream:
             * - A normal String field should be written inside a tag
             * - An XmlFieldRetriever renders an Element which must then be streamed, then deleted
             * - An XmlFieldRetrieverStream writes to the stream. Nothing more required here.
             *
             * If caller is not streaming, this method must render to parentElement
             */


            String fieldId = fieldDefinition.getFieldId();
            if (fieldId.contains("?")) {
                throw new RuntimeException("Field " + fieldId
                        + " contains question mark that originally may be non-printable character, please avoid using it in field names");
            }
            // Each field will return either an XML element
            // or a String value which then must be wrapped in an XML element
            if (fieldDefinition.getRetriever() instanceof XmlFieldRetrieverStreaming) {
                /*
                 * render directly to xmlWriter stream
                 */
                ((XmlFieldRetrieverStreaming) fieldDefinition.getRetriever()).renderField(
                        xmlHolder,
                        getReportData(), this,
                        fieldDefinition,
                        parentElement);
            } else if (fieldDefinition.getRetriever() instanceof XmlFieldRetriever) {
                /*
                 * render directly to parentElement DOM
                 * (including sorting if necessary)
                 */
                if (xmlHolder != null) {
                    removeChildElements(parentElement);
                }

                ((XmlFieldRetriever) fieldDefinition.getRetriever()).renderField(
                        getReportData(), this,
                        fieldDefinition,
                        parentElement);

                /*
                 * Serialize to XML Writer
                 */
                if (xmlHolder != null) {
                    List<Element> children = getChildElements("*", parentElement);
                    for (Element childElement : children) {
                        xmlHolder.writeElementNextLine(childElement);
                    }

                    /*
                     * Empty parentElement
                     */
                    removeChildElements(parentElement);
                }
            } else {
                /*
                 * wrap a string value for appropriate XML destination (stream or DOM)
                 */

                // Traditional string value: Wrap into an XML element
                String fieldValue = "";
                try {
                    fieldValue = getFieldValue(fieldDefinition.getFieldId());
                } catch (Exception ex) {
                    String msg = this.getDebugInfo() + "\n" + fieldDefinition.getFieldId();
                    RuntimeException debugEx = new RuntimeException(msg, ex);

                    getReportData().storeException(fieldDefinition, this, debugEx);
                }
                /*
                 * Skip an empty tag if minLength == 0 or minLangth and MaxLength are
                 * different
                 */
                if (StringUtils.isEmpty(fieldValue)) {
                    if (fieldDefinition.getMinLength() == 0
                            || fieldDefinition.getMaxLength() != fieldDefinition.getMinLength()) {
                        return;
                    }
                }

                String sifPath = fieldDefinition.getSifPath();

                // Because it isn't an XmlRetriever, the field must provide an XML
                // element name to wrap the content
                if (StringUtils.isBlank(sifPath)) {
                    String message = "Missing sifPath for field " + fieldDefinition.getFieldId();
                    throw new RuntimeException(message);
                }

                /*
                 * Don't export a NO_FIELD field to the XML.
                 * (but do evaluate it and return it in getFieldValue above).
                 * This is to support matching an Entity fieldname with a CSV Extract fieldname.
                 */
                if (MARKER_NO_FIELD.equals(sifPath)) {
                    return;
                }

                // Write fieldValue to DOM Element or XML writer
                String fieldElementName = sifPath.trim();
                if (xmlHolder == null) {
                    OnsisStateReportData.appendTextElement(fieldElementName, fieldValue, parentElement);
                } else {
                    xmlHolder.xmlWriter.writeCharacters("\n");
                    OnsisStateReportData.writeTag(xmlHolder.xmlWriter, fieldElementName, fieldValue);
                }
            }

            return;
        }

        /**
         * Set this to skip rendering the current entity row.
         *
         * @param isCanceled void
         */
        protected void setCanceled(boolean isCanceled) {
            m_isRowCanceled = isCanceled;
        }

        /**
         * Returns the value of a field based on the state name of the field.
         *
         * @param elementName String
         * @return the field value
         */
        private String getFieldValueBySifPath(String elementName) {
            String value = null;
            for (int pos = 0; pos < getReportData().getFieldCount(); pos++) {
                if (getReportData().getFieldDefinition(pos).getSifPath().equals(elementName)) {
                    value = getFieldValue(pos);
                    break;
                }
            }
            return value;
        }

        /**
         * Checks if is skip cancel.
         *
         * @param entityElement Element
         * @return true, if is skip cancel
         */
        private boolean isSkipCancel(Element entityElement) {
            Element skipCancel =
                    OnsisStateReportData.getChildElement("SKIP_CANCEL", entityElement);
            if (skipCancel != null) {
                entityElement.removeChild(skipCancel);
                return true;
            }
            return false;
        }

    }

    /**
     * The Enum RenderGranularity.
     */
    public enum RenderGranularity {
        FIELD, ENTITY
    }

    /**
     * The Enum SubmissionSchoolType.
     */
    public enum SubmissionSchoolType {
        CONTINUING_EDUCATION(OnsisConstants.SUBMISSION_SCHOOL_TYPE_CONTINUING_EDUCATION) {
            private List<String> m_creditConedProgramTypes;

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.SubmissionSchoolType#getMasterScheduleInclusionCriteria(com.x2dev.procedures.statereporting.on.OnsisStateReportData)
             */
            @Override
            public X2Criteria getMasterScheduleInclusionCriteria(GlobalData globalData) {
                X2Criteria criteria = super.getMasterScheduleInclusionCriteria(globalData);
                X2Criteria additionalCriteria = additionalSectionCriteria(globalData);
                if (additionalCriteria != null) {
                    criteria.addAndCriteria(additionalCriteria);
                }
                addNonCreditCriteria(globalData, criteria);
                return criteria;
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.SubmissionSchoolType#isSectionIncluded(com.x2dev.sis.model.beans.MasterSchedule)
             */
            @Override
            public Boolean isSectionIncluded(GlobalData globalData, OnSection section) {
                return creditConedProgTypes(globalData).contains(section.getConedProgType());
            }

            /**
             * Additional section criteria.
             *
             * @param reportData OnsisStateReportData
             * @return X2Criteria
             */
            private X2Criteria additionalSectionCriteria(GlobalData globalData) {
                X2Criteria additionalCriteria = null;
                String conedProgTypePath =
                        OnSection.FIELD_CONED_PROG_TYPE.resolve(globalData.getDictionaryExtractor());
                if (StringUtils.isEmpty(conedProgTypePath)) {
                    additionalCriteria = new X2Criteria();
                    additionalCriteria.addIn(conedProgTypePath, creditConedProgTypes(globalData));
                }
                addNonCreditCriteria(globalData, additionalCriteria);
                return additionalCriteria;
            }

            private X2Criteria addNonCreditCriteria(GlobalData globalData,
                                                    X2Criteria baseCriteria) {
                DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
                X2Criteria criteria = new X2Criteria();
                String conedProgTypePath = OnSection.FIELD_CONED_PROG_TYPE.resolve(dictionaryExtractor);
                if (StringUtils.isEmpty(conedProgTypePath)) {
                    List<String> nonCreditCodes = globalData.getDictionaryExtractor()
                            .getRefCodesWithStateValue(OnSection.FIELD_CONED_PROG_TYPE.getField(dictionaryExtractor),
                                    Arrays.asList(OnSection.CONED_LITERACY, OnSection.CONED_INDIGENOUS_LANGUAGE,
                                            OnSection.CONED_DEVELOPMENTALLY_DISABLED, OnSection.CONED_IILE))
                            .stream().map(ReferenceCode::getCode).collect(Collectors.toList());
                    criteria.addIn(OnSection.FIELD_CONED_PROG_TYPE.resolve(dictionaryExtractor), nonCreditCodes);
                    baseCriteria.addOrCriteria(criteria);
                }
                return baseCriteria;
            }


            private List<String> creditConedProgTypes(GlobalData globalData) {
                if (m_creditConedProgramTypes == null) {
                    List<String> codes = Collections.EMPTY_LIST;
                    DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
                    String conedProgTypePath = OnSection.FIELD_CONED_PROG_TYPE.resolve(dictionaryExtractor);
                    if (!StringUtils.isEmpty(conedProgTypePath)) {
                        codes = globalData.getDictionaryExtractor()
                                .getRefCodesWithStateValue(
                                        OnSection.FIELD_CONED_PROG_TYPE.getField(dictionaryExtractor),
                                        Arrays.asList(OnSection.CONED_CREDIT_NIGHT, OnSection.CONED_CREDIT_DAY,
                                                OnSection.CONED_SELF_STUDY))
                                .stream().map(ReferenceCode::getCode).collect(Collectors.toList());
                    }
                    m_creditConedProgramTypes = codes;
                }
                return m_creditConedProgramTypes;
            }
        },
        //
        CTCC_SECONDARY(OnsisConstants.SUBMISSION_SCHOOL_TYPE_CTCC_SECONDARY),
        //
        DEFAULT(Collections.EMPTY_LIST),
        //
        ECPP(OnsisConstants.SUBMISSION_SCHOOL_TYPE_ECPP),
        //
        PUBLIC_ELEMENTARY(OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_ELEMENTARY) {

            @Override
            public X2Criteria getCourseRequestCriteria(GlobalData globalData) {
                DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
                X2Criteria inclusionCriteria = new X2Criteria();

                X2Criteria courseCriteria = new X2Criteria();
                courseCriteria.addEqualTo(OnCourseRequest.FIELD_COURSE_CODE_TYPE.resolve(dictionaryExtractor),
                        OnSection.COURSE_CODE_TYPE_HOMEROOM);

                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addIn(
                        OnCourseRequest.FIELD_ELEMENTARY_SUBJECT_TYPE.resolve(dictionaryExtractor),
                        dictionaryExtractor
                                .getRefCodesWithStateValue(
                                        OnCourseRequest.FIELD_ELEMENTARY_SUBJECT_TYPE.getField(dictionaryExtractor))
                                .stream().map(code -> code.getCode()).collect(Collectors.toList()));
                courseCriteria.addOrCriteria(orCriteria);

                inclusionCriteria.addOrCriteria(courseCriteria);

                inclusionCriteria.addOrCriteria(getCourseRequestPlaceholderCriteria(globalData));
                return inclusionCriteria;
            }

            /**
             * Gets the master schedule inclusion criteria.
             *
             * @param globalData GlobalData
             * @return X 2 criteria
             * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.SubmissionSchoolType#getMasterScheduleInclusionCriteria(com.x2dev.procedures.statereporting.on.OnsisStateReportData)
             */
            @Override
            public X2Criteria getMasterScheduleInclusionCriteria(GlobalData globalData) {
                DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
                X2Criteria inclusionCriteria = new X2Criteria();

                X2Criteria courseCriteria = new X2Criteria();
                courseCriteria.addEqualTo(OnSection.FIELD_COURSE_CODE_TYPE.resolve(dictionaryExtractor),
                        OnSection.COURSE_CODE_TYPE_HOMEROOM);
                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addIn(OnSection.FIELD_ELEMENTARY_SUBJECT_TYPE.resolve(null),
                        dictionaryExtractor
                                .getRefCodesWithStateValue(
                                        OnSection.FIELD_ELEMENTARY_SUBJECT_TYPE.getField(dictionaryExtractor))
                                .stream().map(code -> code.getCode()).collect(Collectors.toList()));
                courseCriteria.addOrCriteria(orCriteria);

                inclusionCriteria.addOrCriteria(courseCriteria);

                inclusionCriteria.addOrCriteria(getSectionPlaceholderCriteria(globalData));
                return inclusionCriteria;
            }

            /**
             *
             *
             * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.SubmissionSchoolType#getReportCardGradeLevelLimiter()
             */
            @Override
            public Collection<String> getReportCardGradeLevelLimiter() {
                return IntStream.rangeClosed(1, 8).mapToObj(i -> Integer.toString(i)).collect(Collectors.toSet());
            }

            /**
             * Gets the school classes.
             *
             * @param sections Collection<OnsisSection>
             * @return Collection
             */
            @Override
            public Collection<OnSection> getSchoolClasses(Collection<OnSection> sections) {
                return sections.stream()
                        .filter(section -> OnSection.COURSE_CODE_TYPE_HOMEROOM.equals(section.getCourseCodeType()))
                        .collect(Collectors.toList());
            }

            @Override
            public boolean isClassAssignment(OnSection section) {
                return OnSection.COURSE_CODE_TYPE_HOMEROOM.equals(section.getCourseCodeType());
            }
        },
        //
        PUBLIC_SECONDARY(OnsisConstants.SUBMISSION_SCHOOL_TYPE_PUBLIC_SECONDARY),
        //
        SUMMER_SCHOOL(OnsisConstants.SUBMISSION_SCHOOL_TYPE_SUMMER_SCHOOL) {
            private List<String> m_creditConedProgramTypes;

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.SubmissionSchoolType#getMasterScheduleInclusionCriteria(com.x2dev.procedures.statereporting.on.OnsisStateReportData)
             */
            @Override
            public X2Criteria getMasterScheduleInclusionCriteria(GlobalData globalData) {
                return null;
            }

            /**
             * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.SubmissionSchoolType#isSectionIncluded(com.x2dev.sis.model.beans.MasterSchedule)
             */
            @Override
            public Boolean isSectionIncluded(GlobalData globalData, OnSection section) {
                return creditConedProgTypes(globalData).contains(section.getConedProgType());
            }

            private List<String> creditConedProgTypes(GlobalData globalData) {
                if (m_creditConedProgramTypes == null) {
                    List<String> codes = Collections.EMPTY_LIST;
                    DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
                    String conedProgTypePath = OnSection.FIELD_CONED_PROG_TYPE.resolve(dictionaryExtractor);
                    if (!StringUtils.isEmpty(conedProgTypePath)) {
                        codes = globalData.getDictionaryExtractor()
                                .getRefCodesWithStateValue(
                                        OnSection.FIELD_CONED_PROG_TYPE.getField(dictionaryExtractor),
                                        Arrays.asList(OnSection.CONED_SUMMER_CREDIT,
                                                OnSection.CONED_SELF_STUDY))
                                .stream().map(ReferenceCode::getCode).collect(Collectors.toList());
                    }
                    m_creditConedProgramTypes = codes;
                }
                return m_creditConedProgramTypes;
            }

        };

        public static final String DEFAULT_SCHOOL_LEVEL_CODE_ELEMENTARY = "01";
        public static final String DEFAULT_SCHOOL_LEVEL_CODE_SECONDARY = "02";

        /**
         * Gets the course placeholder criteria.
         *
         * @param globalData GlobalData
         * @return X 2 criteria
         */
        public static X2Criteria getCourseRequestPlaceholderCriteria(GlobalData globalData) {
            DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(OnCourseRequest.FIELD_COURSE_CODE_TYPE.resolve(dictionaryExtractor),
                    OnSection.COURSE_CODE_TYPE_PLACEHOLDER);
            criteria.addNotEmpty(OnCourseRequest.FIELD_CRS_NUMBER.resolve(dictionaryExtractor),
                    globalData.getBroker().getPersistenceKey());
            return criteria;
        }

        /**
         * Gets the course placeholder criteria.
         *
         * @param globalData GlobalData
         * @return X 2 criteria
         */
        public static X2Criteria getSectionPlaceholderCriteria(GlobalData globalData) {
            DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(OnSection.FIELD_COURSE_CODE_TYPE.resolve(dictionaryExtractor),
                    OnSection.COURSE_CODE_TYPE_PLACEHOLDER);
            criteria.addNotEmpty(ToolSection.FIELD_CRS_NUMBER.resolve(dictionaryExtractor),
                    globalData.getBroker().getPersistenceKey());
            return criteria;
        }

        /**
         * Gets the submission school type.
         *
         * @param submissionTypeCode String
         * @return Submission school type
         */
        public static SubmissionSchoolType getSubmissionSchoolType(String submissionTypeCode) {
            for (SubmissionSchoolType submissionSchoolType : values()) {
                if (submissionSchoolType.isSubmissionType(submissionTypeCode)) {
                    return submissionSchoolType;
                }
            }
            return null;
        }

        private List<String> m_submissionTypeCodes = new ArrayList<>();

        /**
         * Instantiates a new submission school type.
         *
         * @param submissionTypeCodes List<String>
         */
        SubmissionSchoolType(List<String> submissionTypeCodes) {
            m_submissionTypeCodes.addAll(submissionTypeCodes);
        }

        /**
         * Gets the course request criteria.
         *
         * @param globalData GlobalData
         * @return X 2 criteria
         */
        public X2Criteria getCourseRequestCriteria(GlobalData globalData) {
            DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
            X2Criteria inclusionCriteria = new X2Criteria();

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(OnCourseRequest.FIELD_COURSE_CODE_TYPE.resolve(dictionaryExtractor),
                    dictionaryExtractor
                            .getRefCodesWithStateValue(
                                    OnCourseRequest.FIELD_COURSE_CODE_TYPE.getField(dictionaryExtractor),
                                    OnSection.COURSE_CODE_TYPES_MDC_DCC_LDC)
                            .stream()
                            .map(code -> code.getCode()).collect(Collectors.toList()));
            criteria.addNotEmpty(OnCourseRequest.FIELD_MINISTRY_COURSE_CODE.resolve(dictionaryExtractor),
                    globalData.getBroker().getPersistenceKey());

            inclusionCriteria.addOrCriteria(criteria);
            inclusionCriteria.addOrCriteria(getCourseRequestPlaceholderCriteria(globalData));
            return inclusionCriteria;
        }

        /**
         * Gets the master schedule inclusion criteria.
         *
         * @param globalData GlobalData
         * @return X 2 criteria
         */
        public X2Criteria getMasterScheduleInclusionCriteria(GlobalData globalData) {
            DictionaryExtractor dictionaryExtractor = globalData.getDictionaryExtractor();
            X2Criteria inclusionCriteria = new X2Criteria();

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(OnSection.FIELD_COURSE_CODE_TYPE.resolve(dictionaryExtractor),
                    dictionaryExtractor
                            .getRefCodesWithStateValue(
                                    OnSection.FIELD_COURSE_CODE_TYPE.getField(dictionaryExtractor),
                                    OnSection.COURSE_CODE_TYPES_MDC_DCC_LDC)
                            .stream()
                            .map(code -> code.getCode()).collect(Collectors.toList()));
            criteria.addNotEmpty(OnSection.FIELD_MINISTRY_COURSE_CODE.resolve(dictionaryExtractor),
                    globalData.getBroker().getPersistenceKey());

            inclusionCriteria.addOrCriteria(criteria);
            inclusionCriteria.addOrCriteria(getSectionPlaceholderCriteria(globalData));
            return inclusionCriteria;
        }

        /**
         * Gets the report card grade level limiter.
         *
         * @return Collection
         */
        public Collection<String> getReportCardGradeLevelLimiter() {
            Set<String> gradeLevelLimiter = null; // new LinkedHashSet<>();

            return gradeLevelLimiter;
        }

        /**
         * Gets the school classes.
         *
         * @param sections Collection<OnsisSection>
         * @return Collection
         */
        public Collection<OnSection> getSchoolClasses(Collection<OnSection> sections) {
            return sections;
        }

        /**
         * Gets the School Level Code to use when selecting schools for this submission type.
         * E.g. for OCTELEM the School Level Code is "Secondary".
         *
         * @return String
         */
        public String getSchoolLevelCode() {
            /*
             * TODO:
             * Add a UDF to Submission Type definition so the customer can configure.
             * Then this method should get the configured School Level Code from the Submission
             * Type.
             * If it's empty, use the default value.
             */
            return getDefaultSchoolLevelCode(this);
        }

        /**
         * Checks if is class assignment.
         *
         * @param section OnsisSection
         * @return true, if is class assignment
         */
        public boolean isClassAssignment(OnSection section) {
            return true;
        }

        /**
         * Checks if is section included.
         *
         * @param globalData the global data
         * @param section MasterSchedule
         * @return Boolean
         */
        public Boolean isSectionIncluded(GlobalData globalData, OnSection section) {
            /*
             * Default excludes all coned program types for SUMMER_SCHOOL and PUBLIC_SECONDARY
             */
            return StringUtils.isEmpty(section.getConedProgType());
        }

        /**
         * Checks if is submission type.
         *
         * @param submissionTypeCode String
         * @return true, if is submission type
         */
        public boolean isSubmissionType(String submissionTypeCode) {
            return m_submissionTypeCodes.contains(submissionTypeCode);
        }

        /**
         * Gets the default school level code.
         *
         * @param submissionSchoolType SubmissionSchoolType
         * @return String
         */
        private String getDefaultSchoolLevelCode(SubmissionSchoolType submissionSchoolType) {
            if (PUBLIC_SECONDARY.equals(submissionSchoolType)) {
                return DEFAULT_SCHOOL_LEVEL_CODE_SECONDARY;
            } else if (PUBLIC_ELEMENTARY.equals(submissionSchoolType)) {
                return DEFAULT_SCHOOL_LEVEL_CODE_ELEMENTARY;
            }
            return null;
        }


    }

    /**
     * The Enum ValidationErrorType.
     */
    enum ValidationErrorType {
        APPLIED_ERROR("Applied Error"),
        //
        CRITICAL_ERROR("Critical Error"),
        //
        WARNING("Warning"),
        //
        INVALID_DATA("Invalid Data");

        private final String m_description;

        /**
         * Instantiates a new validation error type.
         *
         * @param desc String
         */
        ValidationErrorType(String desc) {
            m_description = desc;
        }

        /**
         * Gets the description.
         *
         * @return String
         */
        public String getDescription() {
            return m_description;
        }
    }

    /**
     * The Class ValidationException.
     */
    abstract static class ValidationException extends RuntimeException {

        /**
         * Gets the error type.
         *
         * @return Validation error type
         */
        public abstract ValidationErrorType getErrorType();

        /**
         * Gets the group name.
         *
         * @return String
         */
        public abstract String getGroupName();

        /**
         * Gets the message code.
         *
         * @return String
         */
        public abstract String getMessageCode();

        /**
         * Gets the message en.
         *
         * @return String
         */
        public abstract String getMessageEn();

        /**
         * Gets the message fr.
         *
         * @return String
         */
        public abstract String getMessageFr();
    }

    public static final String EXTRACT_TYPE_ENR_ROOT = "STUDENT SCHOOL ENROLMENT";
    public static final String EXTRACT_TYPE_ENR_SSC = "STUDENT CLASS ENROLMENT";
    public static final String EXTRACT_TYPE_ASSIGNED_SUBJECT = "SCHOOL EDUCATOR ASSIGN SUBJECT";
    public static final String EXTRACT_TYPE_SCHOOL_CLASS = "CLASS LIST";
    public static final String EXTRACT_TYPE_SCHOOL_INCIDENT = "SCHOOL INCIDENT";

    public static final String INPUT_PARAM_DEBUG_STAFF_OID = "debugStaffOid";
    public static final String INPUT_PARAM_DEBUG_STUDENT_OID = "debugStudentOid";
    public static final String INPUT_PARAM_INCLUDE_G_DAYS = "includeGDays";
    public static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    public static final String INPUT_PARAM_XML_FIELDS = "xmlFields";

    public static final String PATH_TO_PROJECT = "com.x2dev.procedures.statereporting.on.revised.";

    public static final String PREFERENCE_GLOBAL_DATA = "globalData";

    protected static final String MARKER_NO_FIELD = "NO_FIELD";
    // TODO - fix version validaton
    public static final List<String> EXTERNAL_HELPER_TOOL_IDS = Arrays.asList("ONSIS-FTE-CLASS", "UTIL-FTE-HELPER");
    private static final String PROCEDURE_ID_BASE = "ONSI2-BASE";

    /**
     * Append text element.
     *
     * @param name Can use getElementName(getProcedureId())
     * @param text String
     * @param parentElement Element
     * @return Element
     */
    public static Element appendTextElement(String name, String text, Element parentElement) {
        Element textElement = createTextElement(name, text, parentElement.getOwnerDocument());
        parentElement.appendChild(textElement);
        return textElement;
    }

    /**
     * Creates the text element.
     *
     * @param name String
     * @param text String
     * @param document Document
     * @return Element
     */
    public static Element createTextElement(String name, String text, org.w3c.dom.Document document) {
        Element textElement = document.createElement(name);
        if (text != null) {
            textElement.setTextContent(text);
        }
        return textElement;
    }


    /**
     * Fixup class code.
     *
     * @param globalData GlobalData
     * @param classCode String
     * @param broker X2Broker
     * @return String
     */
    public static String fixupClassCode(GlobalData globalData, String classCode, X2Broker broker) {
        /*
         * S-56948 2020-12-01
         * ISSUE: Class Code in Aspen does not match data in csv (Submitted data to OnSIS)
         *
         * Goal: Generate/compute on fly to match Aspen data to data in csv
         *
         * June 2020 Secondary (JUNSEC1) for Maplewood converted customers, currently DSB ONE
         * (B28002) and Kenora (B29050)
         *
         * Affected Tags:
         * SCHOOL_SUBMISSION/SCHOOL/CLASS/CLASS_CODE
         * SCHOOL_SUBMISSION/SCHOOL/SCHOOL_EDUCATOR_ASSIGNMENT/CLASS_ASSIGNMENT/CLASS_CODE
         * SCHOOL_SUBMISSION/SCHOOL/STUDENT/STUDENT_SCHOOL_ENROLMENT/STUDENT_CLASS_ENROLMENT/
         * CLASS_CODE
         *
         * 1. Create process toward end of XML process
         * 2. Manipulate CLASS_CODE on the fly to match what is in csv. This is the value we must
         * publish in XML for OnSIS
         *
         * Rules:
         * 1. Strip hyphen from class code, unless followed by "GROUPED" I.E. *-GROUPED
         * 2. Convert Aspen value to Uppercase
         *
         * Examples
         * KHICNC-b from Aspen should be KHICNCB
         * KGWANCA-GROUPED from Aspen will remain KGWANCA-GROUPED
         *
         * All internal linkages need to be maintained within XML
         * ACTION Tags should now be UPDATE vs ADD and DELETE
         */
        String fixedClassCode = classCode;
        if (classCode == null) {
            return null;
        }

        Organization org = OrganizationManager.getRootOrganization(broker);
        String boardId = org.getId();
        boolean isSpecialDistrict = "B67172".equals(boardId);

        SubmissionType submissionType = globalData.getSubmissionType();
        String submissionPeriodCode = submissionType.getSubmissionPeriodCode();
        boolean isJuneSec2020 = submissionPeriodCode.startsWith("JUNSEC")
                && globalData.getCurrentYear() == 2021;

        if (isSpecialDistrict && isJuneSec2020) {
            int hyphenIndex = classCode.lastIndexOf('-');
            if (hyphenIndex >= 0) {
                if (!classCode.substring(hyphenIndex).startsWith("-GROUPED")) {
                    fixedClassCode = classCode.substring(0, hyphenIndex);
                    if (classCode.length() > hyphenIndex) {
                        fixedClassCode += classCode.substring(hyphenIndex + 1);
                    }
                }
            }
        }

        // 2021-03-16 S-58266 Remove all spaces from CLASS_CODE
        fixedClassCode = fixedClassCode.replaceAll("\\s", "");

        return fixedClassCode.toUpperCase();
    }

    /**
     * Gets the child element.
     *
     * @param childName String
     * @param parentElement Element
     * @return Element
     */
    public static Element getChildElement(String childName, Element parentElement) {
        if (parentElement == null) {
            return null;
        }
        List<Element> children = getChildElements(childName, parentElement);
        if (children.isEmpty()) {
            return null;
        }
        return children.get(0);
    }

    /**
     * Gets the child elements.
     *
     * @param childName String
     * @param parentElement Element
     * @return List
     */
    public static List<Element> getChildElements(String childName, Element parentElement) {
        boolean isAllTags = StringUtils.isBlank(childName) || "*".equals(childName);
        List<Element> matchingElements = new ArrayList<>();
        if (parentElement != null) {
            NodeList childNodes = parentElement.getChildNodes();

            for (int i = 0; i < childNodes.getLength(); i++) {
                Node childNode = childNodes.item(i);
                if (!(childNode instanceof Element)) {
                    continue;
                }

                Element childElement = (Element) childNode;
                if (isAllTags || childName.equals(childElement.getNodeName())) {
                    matchingElements.add(childElement);
                }
            }
        }

        return matchingElements;
    }

    /**
     * Gets the child text.
     *
     * @param childName String
     * @param parentElement Element
     * @return String
     */
    public static String getChildText(String childName, Element parentElement) {
        if (parentElement == null) {
            return null;
        }
        Element elem = getChildElement(childName, parentElement);
        if (elem == null) {
            return null;
        }

        return elem.getTextContent().trim();
    }

    /**
     * Gets the date from xml.
     *
     * @param dateTagName String
     * @param parentElement Element
     * @return Plain date
     */
    public static PlainDate getDateFromXml(String dateTagName, Element parentElement) {
        String formattedDate = getChildText(dateTagName, parentElement);
        return OnsisConstants.parseDate(formattedDate, OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES);
    }

    /**
     * Return all <elementName> who in turn have a <grandChildName> equal to "grandChildValue".
     *
     * E.g. get all <CLASS_ASSIGNMENT> with <ACTION>UPDATE</ACTION>
     *
     * @param elementName Use * to wildcard
     * @param grandParentElement Element
     * @param grandChildName String
     * @param grandChildValue String
     * @return List
     */
    public static List<Element> getElementsWithChildValue(String elementName,
                                                          Element grandParentElement,
                                                          String grandChildName,
                                                          String grandChildValue) {

        List<Element> matchingElements = new ArrayList<>();
        if (grandParentElement != null) {
            boolean isAllTags = StringUtils.isBlank(elementName) || "*".equals(elementName);

            NodeList parents = (isAllTags)
                    ? grandParentElement.getChildNodes()
                    : grandParentElement.getElementsByTagName(elementName);

            for (int i = 0; i < parents.getLength(); i++) {
                Node node = parents.item(i);
                if (!(node instanceof Element)) {
                    continue;
                }

                Element parentElement = (Element) node;

                if (!isAllTags) {
                    if (!elementName.equals(parentElement.getTagName())) {
                        continue;
                    }
                }

                NodeList grandchildren = parentElement.getElementsByTagName(grandChildName);
                for (int gcx = 0; gcx < grandchildren.getLength(); gcx++) {
                    Node grandchildNode = grandchildren.item(gcx);
                    Element grandchild = (Element) grandchildNode;

                    String textContent = grandchild.getTextContent();
                    if (StringUtils.isEqual(textContent, grandChildValue)) {
                        matchingElements.add(parentElement);
                        break;
                    }
                }
            }
        }

        return matchingElements;
    }

    /**
     * Returns the ExportFormatDefinition associated with the passed SIF topic and profile.
     *
     * @param topic String
     * @param broker X2Broker
     * @return ExportFormatDefinition
     */
    public static ExportFormatDefinition getExportFormatForTopic(String topic, X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatDefinition.COL_SIF_TOPIC, topic);
        criteria.addBeginsWith(ExportFormatDefinition.COL_PROCEDURE_ID, OnsisConstants.FORMAT_PREFIX_ONSIS);

        QueryByCriteria query = new QueryByCriteria(ExportFormatDefinition.class, criteria);

        return (ExportFormatDefinition) broker.getBeanByQuery(query);
    }

    /**
     * Gets the external source by id.
     *
     * @param procedureId String
     * @param broker X2Broker
     * @return Object
     */
    private static Object getExternalSourceById(String procedureId, X2Broker broker) {
        String className = null;
        try {
            className = getProcedureClassNameById(procedureId, broker);
            if (!StringUtils.isEmpty(className)) {
                Class clazz = OnsisStateReportData.class.getClassLoader()
                        .loadClass(PATH_TO_PROJECT + className);
                return clazz.getDeclaredConstructor().newInstance();
            }
        } catch (Exception e) {
            AppGlobals.getLog().log(Level.WARNING, "getExternalSourceById unable to instantiate Procedure "
                    + "with ID [" + procedureId + "]. className: [" + PATH_TO_PROJECT + className + "] \n"
                    + LoggerUtils.convertThrowableToString(e));
        } finally {
            if (StringUtils.isEmpty(className)) {
                AppGlobals.getLog().log(Level.WARNING, "getExternalSourceById unable to locate ClassName "
                        + "for Procedure with ID [" + procedureId + "]");
            }
        }
        return null;
    }

    /**
     * Gets the procedure class name by id.
     *
     * @param procedureId String
     * @param broker X2Broker
     * @return String
     */
    private static String getProcedureClassNameById(String procedureId, X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Procedure.COL_ID, procedureId);
        QueryByCriteria query = new QueryByCriteria(Procedure.class, criteria);
        Procedure procedure = broker.getBeanByQuery(query);
        String sourceCode = procedure.getSourceCode().getSourceCode();

        Pattern pattern = Pattern.compile("public class (\\S*)");
        Matcher matcher = pattern.matcher(sourceCode);
        if (matcher.find()) {
            return matcher.group(1);
        }
        AppGlobals.getLog().log(Level.WARNING, "getProcedureClassNameById unable to locate Procedure "
                + "with ID [" + procedureId + "] and sourceCode containing \"public class \"");
        return null;
    }

    /**
     * Load plugin state report data.
     *
     * @param topic String
     * @param customProcedureId String
     * @param errors Collection<StateReportValidationError>
     * @param broker X2Broker
     * @param m_userData OnsisUserDataContainer
     * @return OnsisStateReportData
     * @throws JarPluginNotFoundException exception
     * @throws ToolRunException exception
     */
    public static OnsisStateReportData loadPluginStateReportData(String topic,
                                                                 String customProcedureId,
                                                                 Collection<StateReportValidationError> errors,
                                                                 X2Broker broker,
                                                                 ToolUserDataContainer m_userData)
            throws JarPluginNotFoundException, ToolRunException {
        ExportFormatDefinition formatDef = getExportFormatForTopic(topic, broker);
        if (formatDef == null) {
            NoExportException e = new NoExportException(
                    "Unable to locate export format for topic [" + topic + "]");
            AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
            throw e;
        }
        return (OnsisStateReportData) loadStateReportData(formatDef, customProcedureId, errors, broker, m_userData);
    }

    /**
     * Instantiates the StateReportData class for the passed export format definition.
     *
     * @param formatDef ExportFormatDefinition
     * @param customProcedureId String
     * @param errors A list to be referenced within the newly instantiated SifStateReportData
     *        that
     *        will hold any validation errors
     * @param broker X2Broker
     * @param userData OnsisUserDataContainer
     * @return StateReportData
     * @throws JarPluginNotFoundException exception
     * @throws ToolRunException exception
     * @throws NoExportException exception
     */
    public static StateReportData loadStateReportData(ExportFormatDefinition formatDef,
                                                      String customProcedureId,
                                                      Collection<StateReportValidationError> errors,
                                                      X2Broker broker,
                                                      ToolUserDataContainer userData)
            throws JarPluginNotFoundException, ToolRunException, NoExportException {
        String procedureProcedureId =
                StringUtils.isEmpty(customProcedureId) ? formatDef.getProcedureId() : customProcedureId;
        String formatProcedureId = formatDef.getProcedureId();
        StateReportData reportData = (StateReportData) getExternalSourceById(procedureProcedureId, broker);
        if (reportData == null || !(reportData instanceof OnsisStateReportData)) {
            NoExportException e = new NoExportException(
                    "Unable to locate procedure with ID [" + formatProcedureId + "] for topic "
                            + formatDef.getSifTopic());
            AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
            throw e;
        }
        reportData.setProcedureId(procedureProcedureId);
        errors.addAll(reportData.loadDefinitions(formatProcedureId, broker));

        Organization organization = OrganizationManager.getRootOrganization(broker);

        reportData.setBroker(broker);
        reportData.setOrganization(organization);
        reportData.setSchoolContext(false);
        reportData.setParameters(new HashMap<String, Object>());
        reportData.setEfdOid(formatDef.getOid());
        reportData.setCurrentContext(organization.getCurrentContext());
        if (userData != null) {
            reportData.setUser(userData.getUser());
        }
        ((OnsisStateReportData) reportData).m_formatDefinition = formatDef;

        return reportData;
    }

    /**
     * Make key.
     *
     * @param keyField String
     * @param keyValue String
     * @return String
     */
    public static String makeKey(String keyField, String keyValue) {
        return "|" + keyField.toUpperCase() + "==" + keyValue + "|";
    }

    /**
     * Removes the child elements.
     *
     * @param parentElement Element
     */
    public static void removeChildElements(Element parentElement) {
        List<Element> childElements = getChildElements("*", parentElement);
        for (Element childElement : childElements) {
            parentElement.removeChild(childElement);
        }
    }

    /**
     * Store exception.
     *
     * @param exceptions Map<String,Set<String>>
     * @param groupDescription String
     * @param exception Exception
     */
    public static void storeException(Map<String, Set<ResultException>> exceptions,
                                      String groupDescription,
                                      String schoolOid,
                                      Exception exception) {
        Set<ResultException> groupExceptions = exceptions.get(groupDescription);
        if (groupExceptions == null) {
            groupExceptions = new HashSet<>();
            exceptions.put(groupDescription, groupExceptions);
        }
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        if (exception.getMessage() != null) {
            pw.println(exception.getMessage());
        }
        exception.printStackTrace(pw);
        groupExceptions.add(new ResultException(schoolOid, sw.toString()));
    }

    /**
     * Write tag.
     *
     * @param xmlWriter XMLStreamWriter
     * @param fieldElementName String
     * @param fieldValue String
     * @throws XMLStreamException exception
     */
    public static void writeTag(XMLStreamWriter xmlWriter, String fieldElementName, String fieldValue)
            throws XMLStreamException {
        xmlWriter.writeStartElement(fieldElementName);
        xmlWriter.writeCharacters(fieldValue);
        xmlWriter.writeEndElement();
    }


    /**
     * The Class GlobalData.
     */
    public class GlobalData implements OnSchoolDateRangeProvider {
        // Hiding fields from StateReportData to implement global data
        @SuppressWarnings("hiding")
        private X2Broker m_broker = null;
        @SuppressWarnings("hiding")
        private DistrictSchoolYearContext m_currentContext;
        @SuppressWarnings("hiding")
        private Organization m_organization;
        @SuppressWarnings("hiding")
        private Map<String, Object> m_parameters;

        private List<OnSchool> m_currentSchool = Collections.EMPTY_LIST;
        private DateAsStringConverter m_dateConverter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
        private PlainDate m_dateDecember31;
        private PlainDate m_dateFebruary1;
        private PlainDate m_dateJanuary31;
        private PlainDate m_dateNovember1PrevYear;
        private Range<Date> m_dateRange;
        private Boolean m_debugDetail;
        private DictionaryExtractor m_dictExtractor;
        private Map<String, Set<ResultException>> m_exceptions;
        private OnsisExtractHelper m_extractHelper;
        private PerformanceMonitor m_fieldPerformanceMonitor = new PerformanceMonitor();
        private FieldsRepository m_fieldsRepository;
        private FilterablesHelper m_filterablesHelper;
        private Map<String, ExportFormatDefinition> m_formatsById = new HashMap<>();
        private Map<String, List<ExportFormatField>> m_formatFields;
        private Map<String, Set<String>> m_generatedRowKeys = new HashMap<>();
        private GradesHelper m_gradesHelper;
        private boolean m_includeEmptyOen = false;
        private Predicate<OnSection> m_isSectionIncluded = null;
        private PlainDate m_july1Date;
        private Set<String> m_missingTags;
        private String m_preloadSectionSchoolOids;
        private ReportDataCache m_reportDataCache = new ReportDataCache();
        private String m_rootComment;
        private List<List<OnSchool>> m_schools = null;
        private SubmissionType m_submissionType;
        private PlainDate m_summerStartDate;
        private ToolUserDataContainer m_userData;
        private OnsisValidator m_validator;

        /**
         * Adds the missing tag.
         *
         * @param missingTag String
         */
        public void addMissingTag(String missingTag) {
            if (m_missingTags == null) {
                m_missingTags = new HashSet<String>();
            }
            m_missingTags.add(missingTag);
        }

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         */
        @Override
        public X2Broker getBroker() {
            return m_broker;
        }

        /**
         * Gets the context.
         *
         * @return District school year context
         * @see com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnCTXProvider#getContext()
         */
        @Override
        public DistrictSchoolYearContext getContext() {
            return getCurrentContext();
        }

        /**
         * Gets the context by year.
         *
         * @param schoolYear int
         * @return District school year context
         */
        public ToolDistrictContext getContextByYear(int schoolYear) {
            return getFilterablesHelper().getFilterable(ToolDistrictContext.class)
                    .extractFirst(ToolDistrictContext.FIELD_SCHOOL_YEAR.resolve(null), schoolYear);
        }

        /**
         * Gets the current context.
         *
         * @return DistrictSchoolYearContext
         */
        public DistrictSchoolYearContext getCurrentContext() {
            return m_currentContext;
        }

        /**
         * Gets the current school.
         *
         * @return School
         */
        public List<OnSchool> getCurrentSchool() {
            return m_currentSchool;
        }

        /**
         * Gets the current school bsids.
         *
         * @return List
         */
        public List<String> getCurrentSchoolBsids() {
            return getCurrentSchool().stream().map(OnSchool::getBsid).collect(Collectors.toList());
        }

        /**
         * Gets the current school.
         *
         * @return School
         */
        public OnSchool getCurrentSchoolFirst() {
            return getCurrentSchool() != null && !getCurrentSchool().isEmpty()
                    ? getCurrentSchool().get(0)
                    : null;
        }

        /**
         * Gets the current school oids.
         *
         * @return List
         */
        public List<String> getCurrentSchoolOids() {
            return getCurrentSchool().stream().map(OnSchool::getOid).collect(Collectors.toList());
        }

        /**
         * Gets the current school oids csv string.
         *
         * @return String
         */
        public String getCurrentSchoolOidsCsvString() {
            return getCurrentSchool().stream().map(OnSchool::getOid).collect(Collectors.joining(","));
        }


        /**
         * Gets the current year.
         *
         * @return int
         */
        public int getCurrentYear() {
            OnsisExtractHelper extractHelper = getExtractHelper();
            if (extractHelper == null) {
                return getCurrentContext().getSchoolYear();
            }
            return extractHelper.getSubmissionPeriod().getSubmissionYear();
        }

        /**
         * Gets the December 31 date fo the current school year.
         *
         * @return PlainDate
         */
        public PlainDate getDateDecember31() {
            if (m_dateDecember31 == null) {
                Calendar asOfDateCalendar = Calendar.getInstance();
                int year = getCurrentYear() - 1;
                asOfDateCalendar.set(Calendar.YEAR, year);
                asOfDateCalendar.set(Calendar.MONTH, Calendar.DECEMBER);
                asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 31);
                m_dateDecember31 = new PlainDate(asOfDateCalendar.getTime());
            }
            return m_dateDecember31;
        }

        /**
         * Gets the December 31 date fo the current school year.
         *
         * @return PlainDate
         */
        public PlainDate getDateFebruary1() {
            if (m_dateFebruary1 == null) {
                Calendar asOfDateCalendar = Calendar.getInstance();
                int year = getCurrentYear();
                asOfDateCalendar.set(Calendar.YEAR, year);
                asOfDateCalendar.set(Calendar.MONTH, Calendar.FEBRUARY);
                asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 1);
                m_dateFebruary1 = new PlainDate(asOfDateCalendar.getTime());
            }
            return m_dateFebruary1;
        }

        /**
         * Gets the December 31 date fo the current school year.
         *
         * @return PlainDate
         */
        public PlainDate getDateJanuary31() {
            if (m_dateJanuary31 == null) {
                Calendar asOfDateCalendar = Calendar.getInstance();
                int year = getCurrentYear();
                asOfDateCalendar.set(Calendar.YEAR, year);
                asOfDateCalendar.set(Calendar.MONTH, Calendar.JANUARY);
                asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 31);
                m_dateJanuary31 = new PlainDate(asOfDateCalendar.getTime());
            }
            return m_dateJanuary31;
        }

        /**
         * Gets the November 1 date of the previous school year.
         *
         * @return PlainDate
         */
        public PlainDate getDateNovember1PrevYear() {
            if (m_dateNovember1PrevYear == null) {
                Calendar asOfDateCalendar = Calendar.getInstance();
                asOfDateCalendar.setTime(getGlobalData().getStartDate());
                asOfDateCalendar.add(Calendar.YEAR, -1);
                asOfDateCalendar.set(Calendar.MONTH, Calendar.NOVEMBER);
                asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 1);
                m_dateNovember1PrevYear = new PlainDate(asOfDateCalendar.getTime());
            }
            return m_dateNovember1PrevYear;
        }

        /**
         * Gets the date range.
         *
         * @return Range
         */
        public Range<Date> getDateRange() {
            if (m_dateRange == null) {
                m_dateRange = Range.of(getStartDate(), getEndDate());
            }
            return m_dateRange;
        }

        /**
         * Gets the debug detail.
         *
         * @return boolean
         */
        public boolean getDebugDetail() {
            if (m_debugDetail == null) {
                m_debugDetail = getParameter(INPUT_PARAM_DEBUG_DETAIL) != null
                        && getParameter(INPUT_PARAM_DEBUG_DETAIL) instanceof Boolean
                        && ((Boolean) getParameter(INPUT_PARAM_DEBUG_DETAIL));
            }
            return m_debugDetail.booleanValue() ? true : false;
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         */
        @Override
        public DictionaryExtractor getDictionaryExtractor() {
            if (m_dictExtractor == null) {
                m_dictExtractor = new DictionaryExtractor(getBroker());
            }
            return m_dictExtractor;
        }

        /**
         * Return the submission period End Date,
         * else the Report Date:
         * --- INPUT_PARAM_REPORT_DATE
         * --- else today.
         *
         * @return Plain date
         */
        @Override
        public PlainDate getEndDate() {
            SubmissionType submissionType = getSubmissionType();
            return submissionType.getPeriodEndDate();
        }

        /**
         * Gets the extract helper.
         *
         * @return Onsis extract helper
         */
        public OnsisExtractHelper getExtractHelper() {
            return m_extractHelper;
        }

        /**
         * Gets the fields repository.
         *
         * @return Fields repository
         */
        public FieldsRepository getFieldsRepository() {
            if (m_fieldsRepository == null) {
                String inputXmlFields = (String) getParameter(INPUT_PARAM_XML_FIELDS);
                if (inputXmlFields == null) {
                    SubmissionType submissionType = getSubmissionType();

                    inputXmlFields = submissionType == null ? "" : submissionType.getFields();
                }
                if (inputXmlFields == null) {
                    throw new RuntimeException("Cannot determine list of xml fields to show");
                }
                m_fieldsRepository = FieldsRepository.of(inputXmlFields);
            }
            return m_fieldsRepository;
        }

        /**
         * Gets the report date.
         *
         * @return Plain date
         */
        public PerformanceMonitor getFieldPerformanceMonitor() {
            return m_fieldPerformanceMonitor;
        }

        /**
         * Gets the filterables helper.
         *
         * @return Filterables helper
         */
        public FilterablesHelper getFilterablesHelper() {
            if (m_filterablesHelper == null) {
                m_filterablesHelper = new FilterablesHelper(getBroker(), getDictionaryExtractor());
            }
            return m_filterablesHelper;
        }

        /**
         * Adds the generated row key.
         *
         * @param key String
         * @param procedureId String
         */
        public void addGeneratedRowKey(String key, String procedureId) {
            String extractType = getExtractType(procedureId);
            Set<String> rowKeys = m_generatedRowKeys.get(extractType);
            if (rowKeys == null) {
                rowKeys = new HashSet<>();
                m_generatedRowKeys.put(extractType, rowKeys);
            }
            rowKeys.add(key.toUpperCase());
        }

        /**
         * Gets the generated rows keys.
         *
         * @param procedureId String
         * @return List
         */
        public Set<String> getGeneratedRowsKeys(String procedureId) {
            String extractType = getExtractType(procedureId);
            Set<String> keys = m_generatedRowKeys.get(extractType);
            return keys == null ? Collections.EMPTY_SET : keys;
        }

        /**
         * Gets the grades helper.
         *
         * @return Grades helper
         */
        public GradesHelper getGradesHelper() {
            if (m_gradesHelper == null) {
                m_gradesHelper = new GradesHelper(this);
            }
            return m_gradesHelper;
        }

        /**
         * Gets the include empty oen.
         *
         * @return boolean
         */
        public boolean getIncludeEmptyOen() {
            return m_includeEmptyOen;
        }

        /**
         * Gets the July 1 date.
         *
         * @return Plain date
         */
        public PlainDate getJuly1Date() {
            if (m_july1Date == null) {
                Calendar asOfDateCalendar = Calendar.getInstance();
                int year = getCurrentYear();
                asOfDateCalendar.set(Calendar.YEAR, year);
                asOfDateCalendar.set(Calendar.MONTH, Calendar.JULY);
                asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 1);
                m_july1Date = new PlainDate(asOfDateCalendar.getTime());
            }
            return m_july1Date;
        }

        /**
         * Returns the organization set in the report or export parameters.
         *
         * @return Organization
         */
        public Organization getOrganization() {
            return m_organization;
        }

        /**
         * Gets the organization tool bean.
         *
         * @return Tool organization
         */
        public OnOrganization getOrganizationToolBean() {
            return (OnOrganization) ToolBean.DistrictManager.getOrganizationToolBean(getBroker());
        }

        /**
         * Returns a parameter as specified in the input from the user for this report run.
         *
         * @param key String
         * @return Object - value for passed key
         */
        public Object getParameter(String key) {
            return m_parameters.get(key);
        }

        /**
         * Gets the previous context.
         *
         * @return Tool district context
         */
        public ToolDistrictContext getPreviousContext() {
            int previousYear = getCurrentYear() - 1;
            return getContextByYear(previousYear);
        }

        /**
         * Gets the report data cache.
         *
         * @return Report data cache
         */
        public ReportDataCache getReportDataCache() {
            return m_reportDataCache;
        }

        /**
         * Gets the root procedure comment.
         *
         * @return String
         */
        public String getRootProcedureComment() {
            if (m_rootComment == null) {
                Tool rootProcedureTool = ToolManager.getToolForId(Tool.TYPE_PROCEDURE, PROCEDURE_ID_BASE, getBroker());
                m_rootComment = rootProcedureTool.getComment();
                if (m_rootComment == null) {
                    m_rootComment = EMPTY_STRING;
                }
            }
            return m_rootComment;
        }

        /**
         * Gets the school by bsid.
         *
         * @param issuedByBSID String
         * @return List
         */
        public List<OnSchool> getSchoolByBsid(String issuedByBSID) {
            List<OnSchool> values = Collections.EMPTY_LIST;
            if (!StringUtils.isEmpty(issuedByBSID)) {
                values = getSchoolsFilterable().getGroup(OnSchool.FIELD_BSID, issuedByBSID);
            }
            return values;
        }

        /**
         * Gets the school.
         *
         * @return Onsis school
         * @see com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnSchoolProvider#getSchool()
         */
        @Override
        public OnSchool getSchool() {
            return getCurrentSchool() != null && !getCurrentSchool().isEmpty() ? getCurrentSchool().get(0) : null;
        }

        /**
         * Gets all schools
         * The schools are grouped into lists with common bsid value.
         * The list is sorted by name of the first element in the list
         *
         * @return Collection
         */
        public List<List<OnSchool>> getSchools() {
            if (m_schools == null) {
                // make sure all schools are cached
                getSchoolsFilterable();
                String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);
                m_schools = new ArrayList();
                Map<String, List<OnSchool>> bsidMap = new HashMap();

                Arrays.asList(schoolOids.split(",")).stream()
                        .map(oid -> ToolBean.getCachedToolBean(OnSchool.class, oid))
                        .sorted(Comparator.comparing(ToolSchool::getName)).forEach(bean -> {
                            String bsid = bean.getBsid();
                            if (bsidMap.containsKey(bsid)) {
                                bsidMap.get(bsid).add(bean);
                            } else {
                                List<OnSchool> schools = new ArrayList();
                                schools.add(bean);
                                m_schools.add(schools);
                                bsidMap.put(bsid, schools);
                            }
                        });
            }
            return m_schools;
        }

        /**
         * Gets the schools filterable.
         *
         * @return Filterable
         */
        public Filterable<OnSchool> getSchoolsFilterable() {
            return (Filterable<OnSchool>) DistrictManager.getSchoolsFilterable();
        }

        /**
         * Gets the school type.
         *
         * @return Submission school type
         */
        public SubmissionSchoolType getSchoolType() {
            SubmissionType submissionType = getSubmissionType();
            SubmissionSchoolType submissionSchoolType =
                    SubmissionSchoolType.getSubmissionSchoolType(submissionType.getSubmissionPeriodCode());
            if (submissionSchoolType == null) {
                throw new RuntimeException(
                        "Unable to determine SubmissionSchoolType for SubmissionPeriodCode: "
                                + submissionType.getSubmissionPeriodCode());
            }

            return submissionSchoolType;
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        @Override
        public PlainDate getStartDate() {
            SubmissionType submissionType = getSubmissionType();
            return submissionType.getPeriodStartDate();
        }

        /**
         * Gets the submission type.
         *
         * @return Submission type
         */
        public SubmissionType getSubmissionType() {
            return m_submissionType;
        }


        /**
         * Gets the summer start date.
         *
         * @return Plain date
         */
        public PlainDate getSummerStartDate() {
            if (m_summerStartDate == null) {
                Calendar asOfDateCalendar = Calendar.getInstance();
                int year = getCurrentYear();
                asOfDateCalendar.set(Calendar.YEAR, year);
                asOfDateCalendar.set(Calendar.MONTH, Calendar.JUNE);
                asOfDateCalendar.set(Calendar.DAY_OF_MONTH, 25);
                m_summerStartDate = new PlainDate(asOfDateCalendar.getTime());
            }
            return m_summerStartDate;
        }

        /**
         * Gets the user data.
         *
         * @return User data container
         */
        public ToolUserDataContainer getUserData() {
            return m_userData;
        }

        /**
         * Gets the validations.
         *
         * @return Onsis validations
         */
        public OnsisValidator getValidator() {
            // TODO: Support validator
            throw new IllegalStateException("validator not yet supported");
            // return m_validator;
        }

        public Predicate<OnSection> isSectionIncluded() {
            if (m_isSectionIncluded == null) {
                GlobalData globalData = this;
                m_isSectionIncluded = new Predicate<OnSection>() {

                    @Override
                    public boolean test(OnSection section) {
                        return globalData.getSchoolType().isSectionIncluded(globalData, section);
                    }
                };
            }
            return m_isSectionIncluded;
        }

        /**
         * Load sections.
         */
        public void loadSections() {
            if (ToolBean.getCachedToolBeans(OnSection.class).isEmpty()) {
                ToolBean.addAndCriteria(m_broker, getBeanClassTool(), getSectionCriteria());
            }

        }

        /**
         * Parses the date.
         *
         * @param dateStringObject Object
         * @return PlainDate
         */
        public PlainDate parseDate(Object dateStringObject) {
            if (dateStringObject == null) {
                return null;
            }
            return (PlainDate) m_dateConverter.parseSystemString(dateStringObject.toString());
        }

        /**
         * Pop field definition.
         */
        public void popFieldDefinition() {
            if (getDebugDetail()) {
                m_fieldPerformanceMonitor.popFieldDefinition();
            }
        }

        /**
         * Preload the data needed to process the current submission and school clearing any data
         * from previous school.
         */
        public void preload() {
            if (!getCurrentSchoolOidsCsvString().equals(m_preloadSectionSchoolOids)) {
                if (!StringUtils.isEmpty(m_preloadSectionSchoolOids)) {
                    pushDataDefinition("Clear Data");

                    ToolBean.clearAllCachedToolBeans(CommunityInvolvementAssessment.class);
                    ToolBean.clearAllCachedToolBeans(FteMonthly.class);
                    ToolBean.clearAllCachedToolBeans(OnAddress.class);
                    ToolBean.clearAllCachedToolBeans(OnConductAction.class);
                    ToolBean.clearAllCachedToolBeans(OnEnrollment.class);
                    ToolBean.clearAllCachedToolBeans(OnGraduationStudentProgram.class);
                    ToolBean.clearAllCachedToolBeans(OnGraduationStudentWaiver.class);
                    ToolBean.clearAllCachedToolBeans(OnRubricAssessmentPerformance.class);
                    ToolBean.clearAllCachedToolBeans(OnsisScheduleTeacher.class);
                    ToolBean.clearAllCachedToolBeans(OnSchoolIncident.class);
                    ToolBean.clearAllCachedToolBeans(OnSection.class);
                    ToolBean.clearAllCachedToolBeans(OnStaff.class);
                    ToolBean.clearAllCachedToolBeans(OnStaffPosition.class);
                    ToolBean.clearAllCachedToolBeans(OnsisStudent.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentAttendance.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentOyap.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentSalep.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentSalepDetail.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentSchool.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentELLProgram.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentSLPProgram.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentSLPProgramFrench.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentSped.class);
                    ToolBean.clearAllCachedToolBeans(OnStudentSpedDetail.class);
                    ToolBean.clearAllCachedToolBeans(OnTranscript.class);
                    ToolBean.clearAllCachedToolBeans(OnTranscriptRubric.class);
                    ToolBean.clearAllCachedToolBeans(OssltAssessment.class);
                    ToolBean.clearAllCachedToolBeans(ShsmAssessment.class);
                    ToolBean.getCachedToolBeans(OnGraduationRequirement.class).stream()
                            .forEach(OnGraduationRequirement::clearAssessmentOids);
                    ToolBean.clearAllCachedToolBeans(ToolConductIncident.class);
                    ToolBean.clearAllCachedToolBeans(ToolConductOffense.class);
                    ToolBean.clearAllCachedToolBeans(ToolPersonAddress.class);
                    ToolBean.clearAllCachedToolBeans(ToolRubricAssessment.class);
                    ToolBean.clearAllCachedToolBeans(ToolRubricDefinition.class);
                    ToolBean.clearAllCachedToolBeans(ToolScheduleClass.class);
                    ToolBean.clearAllCachedToolBeans(ToolStudentPeriodAttendance.class);
                    ToolBean.clearAllCachedToolBeans(ToolStudentSchedule.class);
                    ToolBean.clearAllCachedToolBeans(ToolStudentScheduleChange.class);

                    ToolBean.resetCriteria(getBroker(), CommunityInvolvementAssessment.class);
                    ToolBean.resetCriteria(getBroker(), FteMonthly.class);
                    ToolBean.resetCriteria(getBroker(), OnRubricAssessmentPerformance.class);
                    ToolBean.resetCriteria(getBroker(), OnsisScheduleTeacher.class);
                    ToolBean.resetCriteria(getBroker(), OnStudentAttendance.class);
                    ToolBean.resetCriteria(getBroker(), OnStudentOyap.class);
                    ToolBean.resetCriteria(getBroker(), OnStudentSalep.class);
                    ToolBean.resetCriteria(getBroker(), OnStudentELLProgram.class);
                    ToolBean.resetCriteria(getBroker(), OnStudentSLPProgram.class);
                    ToolBean.resetCriteria(getBroker(), OnStudentSLPProgramFrench.class);
                    ToolBean.resetCriteria(getBroker(), OnStudentSped.class);
                    ToolBean.resetCriteria(getBroker(), OnTranscript.class);
                    ToolBean.resetCriteria(getBroker(), OnTranscriptRubric.class);
                    ToolBean.resetCriteria(getBroker(), OssltAssessment.class);
                    ToolBean.resetCriteria(getBroker(), ToolConductIncident.class);
                    ToolBean.resetCriteria(getBroker(), ToolStudentSchedule.class);
                    ToolBean.resetCriteria(getBroker(), ToolStudentScheduleChange.class);

                    popFieldDefinition();
                }

                m_preloadSectionSchoolOids = getCurrentSchoolOidsCsvString();

                if (getFieldsRepository().contains(FieldsRepository.PATH_CONDUCT)) {
                    pushDataDefinition("preload: " + OnSchoolIncident.class.getName());
                    X2Criteria sklIncCriteria = new X2Criteria();

                    sklIncCriteria.addIn(OnSchoolIncident.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                            getCurrentSchoolOids());
                    FilterableFactory.create(getBroker(), getDictionaryExtractor(), OnSchoolIncident.class,
                            sklIncCriteria,
                            Arrays.asList(ToolBean.FIELD_OID));
                    ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolConductIncident.PARENT_UDE);
                    popFieldDefinition();
                } else {
                    pushDataDefinition("preloadStudents(): ");
                    boolean isStudentPreload = preloadStudents();
                    popFieldDefinition();

                    pushDataDefinition("preload: " + OnSection.class.getName());
                    X2Criteria criteria = getSectionCriteria();
                    FilterableFactory.create(getBroker(), getDictionaryExtractor(), OnSection.class, criteria,
                            Arrays.asList(ToolSection.FIELD_CRS_NUMBER, ToolBean.FIELD_OID));
                    popFieldDefinition();

                    // load master terms
                    pushDataDefinition("preload: " + ToolMasterTerm.class.getName());
                    ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolMasterTerm.PARENT_SECTION);
                    popFieldDefinition();

                    // preload ScheduleClass
                    pushDataDefinition("filterable: " + ToolScheduleClass.class.getName());
                    Set<String> scheduleClassOids =
                            ToolBean.getCachedToolBeans(OnSection.class).stream()
                                    .map(section -> section.getSectionClassOid()).collect(Collectors.toSet());
                    if (!scheduleClassOids.isEmpty()) {
                        X2Criteria classCriteria = new X2Criteria();
                        CollectionCriteriaHelper helper = null;
                        if (scheduleClassOids.size() > ToolBean.MAX_SAFE_PARAMETERS) {
                            helper = new CollectionCriteriaHelper(scheduleClassOids, getGlobalData().getBroker());
                            helper.applyToCriteria(SisBeanPaths.SCHEDULE_CLASS.oid().getPath(), classCriteria);
                        } else {
                            classCriteria.addIn(SisBeanPaths.SCHEDULE_CLASS.oid().getPath(), scheduleClassOids);
                        }
                        FilterableFactory.create(getBroker(), getDictionaryExtractor(), ToolScheduleClass.class,
                                classCriteria, null);
                    }
                    popFieldDefinition();

                    ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class, getStudentScheduleCriteria());
                    ToolBean.addAndCriteria(getBroker(), ToolStudentScheduleChange.class,
                            getStudentScheduleChangeCriteria());

                    if (isStudentPreload) {
                        pushDataDefinition("preload: " + ToolStudentSchedule.class.getName());
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                                ToolStudentSchedule.PARENT_STUDENT);
                        ToolBean.reload(getBroker(), getDictionaryExtractor(), null,
                                ToolStudentSchedule.PARENT_SECTION);
                        popFieldDefinition();

                        pushDataDefinition("preload: " + ToolStudentScheduleChange.class.getName());
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                                ToolStudentScheduleChange.PARENT_STUDENT);
                        ToolBean.reload(getBroker(), getDictionaryExtractor(), null,
                                ToolStudentScheduleChange.PARENT_SECTION);
                        popFieldDefinition();

                    } else {
                        pushDataDefinition("preload: " + ToolStudentSchedule.class.getName());
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                                ToolStudentSchedule.PARENT_SECTION);
                        popFieldDefinition();

                        pushDataDefinition("preload: " + ToolStudentScheduleChange.class.getName());
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                                ToolStudentScheduleChange.PARENT_SECTION);
                        popFieldDefinition();
                    }

                    // Do we need to load teacher information
                    if (getFieldsRepository().contains(FieldsRepository.PATH_STAFF)) {
                        pushDataDefinition("filterable: " + OnStaffPosition.class.getName());
                        FilterableFactory.create(getBroker(), getDictionaryExtractor(), OnStaffPosition.class,
                                getStaffPositionCriteria(), null);
                        popFieldDefinition();

                        pushDataDefinition("preload: " + ToolScheduleTeacher.class.getName());
                        String debugStaffOid = (String) getParameter(INPUT_PARAM_DEBUG_STAFF_OID);
                        if (!StringUtils.isBlank(debugStaffOid)) {
                            X2Criteria debugStaffCriteria = new X2Criteria();
                            debugStaffCriteria.addEqualTo(OnsisScheduleTeacher.FIELD_STAFF_OID.resolve(null),
                                    debugStaffOid);
                            ToolBean.addAndCriteria(getBroker(), OnsisScheduleTeacher.class, debugStaffCriteria);
                        }
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                                ToolScheduleTeacher.PARENT_SECTION);
                        popFieldDefinition();

                        // Load staff with staff positions
                        Set<String> staffOids = ToolBean.getCachedToolBeans(OnStaffPosition.class).stream()
                                .map(sfp -> sfp.getStaffOid()).collect(Collectors.toSet());
                        staffOids.removeAll(ToolBean.getCachedToolBeanOids(OnStaff.class));

                        if (!staffOids.isEmpty()) {
                            pushDataDefinition("filterable: " + OnStaff.class.getName());
                            criteria = new X2Criteria();
                            CollectionCriteriaHelper helper = null;
                            try {
                                helper = new CollectionCriteriaHelper(staffOids, getBroker());
                                helper.applyToCriteria(ToolBean.FIELD_OID.resolve(getDictionaryExtractor()), criteria);

                                FilterableFactory.create(getBroker(), getDictionaryExtractor(), OnStaff.class, criteria,
                                        Arrays.asList(OnStaff.FIELD_MEN, ToolBean.FIELD_OID));
                            } finally {
                                if (helper != null) {
                                    helper.cleanup();
                                }
                            }
                            popFieldDefinition();
                        }

                        pushDataDefinition("reload: " + ToolScheduleTeacher.class.getName() + "-SECTION");
                        ToolBean.reload(getBroker(), getDictionaryExtractor(), null,
                                ToolScheduleTeacher.PARENT_SECTION);
                        popFieldDefinition();

                        pushDataDefinition("reload: " + ToolScheduleTeacher.class.getName() + "-STAFF");
                        ToolBean.reload(getBroker(), getDictionaryExtractor(), null, ToolScheduleTeacher.PARENT_STAFF);
                        popFieldDefinition();

                        pushDataDefinition("reload: " + ToolStaffPosition.class.getName() + "-STAFF");
                        ToolBean.reload(getBroker(), getDictionaryExtractor(), null, ToolStaffPosition.PARENT_STAFF);
                        popFieldDefinition();
                    }
                    pushDataDefinition("reload: " + ToolSection.class.getName() + "-SCHEDULE_CLASS");
                    ToolBean.reload(getBroker(), getDictionaryExtractor(), null, ToolSection.PARENT_SCHEDULE_CLASS);
                    popFieldDefinition();

                    if (!isStudentPreload) {
                        pushDataDefinition("filterable: " + OnsisStudent.class.getName());
                        Set<String> studentOids = ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream()
                                .map(ssc -> ssc.getStudentOid()).collect(Collectors.toSet());
                        Set<String> changeOids = ToolBean.getCachedToolBeans(ToolStudentScheduleChange.class).stream()
                                .map(scc -> scc.getStudentOid()).collect(Collectors.toSet());
                        studentOids.addAll(changeOids);
                        studentOids.removeAll(ToolBean.getCachedToolBeanOids(OnsisStudent.class));

                        if (!studentOids.isEmpty()) {
                            criteria = new X2Criteria();
                            CollectionCriteriaHelper helper = null;
                            try {
                                helper = new CollectionCriteriaHelper(studentOids, getBroker());
                                helper.applyToCriteria(ToolBean.FIELD_OID.resolve(getDictionaryExtractor()), criteria);

                                FilterableFactory.create(getBroker(), getDictionaryExtractor(), OnsisStudent.class,
                                        criteria,
                                        Arrays.asList(OnsisStudent.FIELD_OEN, ToolBean.FIELD_OID));
                            } finally {
                                if (helper != null) {
                                    helper.cleanup();
                                }
                            }
                        }
                        popFieldDefinition();
                    }

                    /*
                     * The necessary student schedule and student schedule change records are
                     * already
                     * loaded. Process with reload to insure that the empty collections are properly
                     * initialized for all additional students that have been loaded.
                     */
                    pushDataDefinition("reload: " + ToolStudentSchedule.class.getName() + "-STUDENT");
                    ToolBean.reload(getBroker(), getDictionaryExtractor(), null,
                            ToolStudentSchedule.PARENT_STUDENT.setLoaderMode(LoaderMode.CLEAR));
                    popFieldDefinition();

                    pushDataDefinition("reload: " + ToolStudentScheduleChange.class.getName() + "-STUDENT");
                    ToolBean.reload(getBroker(), getDictionaryExtractor(), null,
                            ToolStudentScheduleChange.PARENT_STUDENT.setLoaderMode(LoaderMode.CLEAR));
                    popFieldDefinition();

                    /*
                     * Preload the other classes that are needed
                     */
                    if (isStudentPreload) {
                        pushDataDefinition("preload: " + FteMonthly.class.getName());
                        X2Criteria fteMontlyCriteria = new X2Criteria();
                        fteMontlyCriteria.addEqualTo(FteMonthly.FIELD_SCHOOL_YEAR.resolve(getDictionaryExtractor()),
                                getCurrentContext().getContextId());
                        ToolBean.addAndCriteria(getBroker(), FteMonthly.class, fteMontlyCriteria);
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null, FteMonthly.PARENT_STUDENT);
                        popFieldDefinition();

                        pushDataDefinition("preload: " + ToolTranscript.class.getName());
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolTranscript.PARENT_STUDENT);
                        popFieldDefinition();

                        pushDataDefinition("preload: " + ToolStudentPeriodAttendance.class.getName());
                        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                                ToolStudentPeriodAttendance.PARENT_SECTION);
                        popFieldDefinition();
                    }
                }
            }
        }

        /**
         * Push data load.
         *
         * @param dataLoad String
         */
        public void pushDataDefinition(String dataLoad) {
            if (getDebugDetail()) {
                m_fieldPerformanceMonitor.pushFieldDefinition(dataLoad);
            }
        }

        /**
         * Push field definition.
         *
         * @param field FieldDefinition
         */
        public void pushFieldDefinition(FieldDefinition field) {
            if (getDebugDetail()) {
                m_fieldPerformanceMonitor.pushFieldDefinition(field.getSifPath());
            }
        }

        /**
         * Sets the broker.
         *
         * @param broker X2Broker
         * @return GlobalData
         */
        public GlobalData setBroker(X2Broker broker) {
            m_broker = broker;
            return this;
        }

        /**
         * Sets the current context.
         *
         * @param currentContext DistrictSchoolYearContext
         * @return GlobalData
         */
        public GlobalData setCurrentContext(DistrictSchoolYearContext currentContext) {
            m_currentContext = currentContext;
            return this;
        }

        /**
         * Sets the current school.
         *
         * @param schools void
         */
        public void setCurrentSchool(List<OnSchool> schools) {
            m_currentSchool = schools;
        }

        /**
         * Sets the exceptions.
         *
         * @param exceptions Map<String,Set<String>>
         * @return GlobalData
         */
        public GlobalData setExceptions(Map<String, Set<ResultException>> exceptions) {
            m_exceptions = Objects.requireNonNull(exceptions);
            return this;
        }

        /**
         * Sets the extract helper.
         *
         * @param extractHelper void
         * @return GlobalData
         */
        public GlobalData setExtractHelper(OnsisExtractHelper extractHelper) {
            m_extractHelper = extractHelper;
            return this;
        }

        /**
         * Sets the include empty oen.
         *
         * @param value void
         */
        public void setIncludeEmptyOen(boolean value) {
            m_includeEmptyOen = value;
        }

        /**
         * Sets the missing tags.
         *
         * @param missingTags void
         * @return GlobalData
         */
        public GlobalData setMissingTags(Set<String> missingTags) {
            m_missingTags = missingTags;
            return this;
        }

        /**
         * Sets the district object for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         * @param organization void
         * @return GlobalData
         */
        public GlobalData setOrganization(Organization organization) {
            m_organization = organization;
            return this;
        }

        /**
         * Sets the user input parameters for this data retrieval process.
         * This should be set by the calling process (report or export).
         *
         * @param parameters Map<String,Object>
         * @return GlobalData
         */
        public GlobalData setParameters(Map<String, Object> parameters) {
            m_parameters = parameters;
            return this;
        }


        /**
         * Sets the submission type.
         *
         * @param submissionType void
         * @return GlobalData
         */
        public GlobalData setSubmissionType(SubmissionType submissionType) {
            m_submissionType = submissionType;
            return this;
        }

        /**
         * Sets the user data.
         *
         * @param userData void
         * @return GlobalData
         */
        public GlobalData setUserData(ToolUserDataContainer userData) {
            m_userData = userData;
            return this;
        }

        /**
         * Sets the submission type.
         *
         * @param validator void
         * @return GlobalData
         */
        public GlobalData setValidator(OnsisValidator validator) {
            m_validator = validator;
            return this;
        }

        /**
         * Store exception.
         *
         * @param groupDescription String
         * @param exception Exception
         */
        public void storeException(String groupDescription, Exception exception) {
            if (m_exceptions == null) {
                m_exceptions = new HashMap<>();
            }
            Set<ResultException> groupExceptions = m_exceptions.get(groupDescription);
            if (groupExceptions == null) {
                groupExceptions = new HashSet<>();
                m_exceptions.put(groupDescription, groupExceptions);
            }
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            if (exception.getMessage() != null) {
                pw.println(exception.getMessage());
            }
            exception.printStackTrace(pw);
            groupExceptions.add(new ResultException(getSchool() == null ? null : getSchool().getOid(), sw.toString()));
        }

        /**
         * Adds the csv students.
         *
         * @param students Set<OnsisStudent>
         */
        private void addCsvStudents(Set<OnsisStudent> students) {
            Set<String> existingOens = null;
            Set<String> oensToAdd = new HashSet();
            String thisBsid = getCurrentSchool().get(0).getBsid();
            DataDictionaryField oenField = OnsisStudent.FIELD_OEN.getField(getDictionaryExtractor());
            if (!StringUtils.isEmpty(thisBsid) && oenField != null) {
                OnsisExtractRecords matcher =
                        getExtractHelper().getMatcherByExtractType(OnsisStateReportData.EXTRACT_TYPE_ENR_ROOT);
                if (matcher != null) {
                    for (OnsisCsvDataRecord record : matcher.getDataRecords()) {
                        String bsid = record.getSingleFieldValue(CsvField.SCHOOL_NUMBER);
                        if (thisBsid.equals(bsid)) {
                            String oen = record.getSingleFieldValue(CsvField.OEN);
                            if (existingOens == null) {
                                existingOens = students.stream().map(OnsisStudent::getOen).collect(Collectors.toSet());
                            }
                            if (!existingOens.contains(oen) && !existingOens.contains(addDashes(oen))) {
                                oensToAdd.add(oen);
                                oensToAdd.add(addDashes(oen));
                            }
                            // process records 1000 at a time to avoid max parameter errors
                            if (oensToAdd.size() > 1000) {
                                X2Criteria criteria = new X2Criteria();
                                criteria.addIn(oenField.getJavaName(), oensToAdd);
                                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                                    criteria.addEqualTo(ToolBean.FIELD_OID.resolve(null), getDebugStudentOid());
                                }

                                Filterable<OnsisStudent> loadedStudents =
                                        FilterableFactory.create(getBroker(), getDictionaryExtractor(),
                                                OnsisStudent.class, criteria, null);
                                students.addAll(loadedStudents.extract());
                                oensToAdd.stream().filter(str -> str.indexOf("-") < 0).forEach(str -> {
                                    if (!loadedStudents.any(oenField.getJavaName(), str)
                                            && !loadedStudents.any(oenField.getJavaName(), addDashes(str))) {
                                        // This student has existing ENR CSV record but no database
                                        // record.
                                        String groupDescription = "SCHOOL_SUBMISSION/SCHOOL/STUDENT";
                                        Exception exception = new IllegalStateException("Student with OEN = " + str
                                                + " for school " + getCurrentSchool().get(0).getName()
                                                + " is found in the CSV file but does not exist in Aspen.\n"
                                                + "Oens: " + oensToAdd + "\n"
                                                + "Criteria: " + criteria + "\n"
                                                + "Students: " + loadedStudents.extract() + "\n");
                                        storeException(groupDescription, exception);
                                    }
                                });
                                // Don't process these OEN's again
                                existingOens.addAll(oensToAdd);
                                oensToAdd.clear();
                            }
                        }
                    }
                }
            }
            if (!oensToAdd.isEmpty()) {
                // process remaining records
                X2Criteria criteria = new X2Criteria();
                criteria.addIn(oenField.getJavaName(), oensToAdd);
                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    criteria.addEqualTo(ToolBean.FIELD_OID.resolve(null), getDebugStudentOid());
                }

                Filterable<OnsisStudent> loadedStudents =
                        FilterableFactory.create(getBroker(), getDictionaryExtractor(),
                                OnsisStudent.class, criteria, null);
                students.addAll(loadedStudents.extract());
                oensToAdd.stream().filter(str -> str.indexOf("-") < 0).forEach(str -> {
                    if (!loadedStudents.any(oenField.getJavaName(), str)
                            && !loadedStudents.any(oenField.getJavaName(), addDashes(str))) {
                        // This student has existing ENR CSV record but no database
                        // record.
                        String groupDescription = "SCHOOL_SUBMISSION/SCHOOL/STUDENT";
                        Exception exception = new IllegalStateException("Student with OEN = " + str
                                + " for school " + getCurrentSchool().get(0).getName()
                                + " is found in the CSV file but does not exist in Aspen.\n"
                                + "Oens: " + oensToAdd + "\n"
                                + "Criteria: " + criteria + "\n"
                                + "Students: " + loadedStudents.extract() + "\n");
                        storeException(groupDescription, exception);
                    }
                });
            }
        }

        /**
         * Adds the dashes.
         *
         * @param oen String
         * @return String
         */
        private String addDashes(String oen) {
            StringBuilder output = new StringBuilder();
            int i = 0;
            for (String chr : oen.split("")) {
                output.append(chr);
                if (++i % 3 == 0 && i < oen.length()) {
                    output.append("-");
                }
            }
            return output.toString();
        }


        /**
         * Adds the FTE students.
         *
         * @param students Set<SisStudent>
         */
        private void addFteStudents(Set<String> students) {
            if (getSubmissionType() != null) {
                X2Criteria criteria = ToolBean.getCriteria(getBroker(), FteMonthly.class).copy();
                criteria.addIn(FteMonthly.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()), getCurrentSchoolOids());
                criteria.addEqualTo(FteMonthly.FIELD_SCHOOL_YEAR.resolve(getDictionaryExtractor()),
                        getCurrentContext().getContextId());
                criteria.addEqualTo(FteMonthly.FIELD_MONTH.resolve(getDictionaryExtractor()),
                        getSubmissionType().getSubmissionPeriodCode());
                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    criteria.addEqualTo(FteMonthly.FIELD_STUDENT_OID.resolve(null), getDebugStudentOid());
                }
                Filterable<FteMonthly> fteMonthlyRecords =
                        FilterableFactory.create(getBroker(), getDictionaryExtractor(),
                                FteMonthly.class, criteria, null);

                Set<String> stdOidsToLoad =
                        fteMonthlyRecords.extract().stream().map(FteMonthly::getStudentOid).collect(Collectors.toSet());
                // remove students already loaded - this includes all students previously loaded
                students.addAll(stdOidsToLoad);
                stdOidsToLoad.removeAll(ToolBean.getCachedToolBeanOids(OnsisStudent.class));
                if (!stdOidsToLoad.isEmpty()) {
                    ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), OnsisStudent.class, stdOidsToLoad);
                }
            }
        }

        /**
         * Adds the non enrolled students.
         *
         * @param students Set<String>
         */
        private void addNonEnrolledStudents(Set<String> students) {

            List<SubQuery> queries = new ArrayList();
            // Items 3-11
            if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_NON_ENROLMENT_SPCE)
                    || getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_NON_ENROLMENT_SHSM)
                    || getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_NON_ENROLMENT_DIPLOMA)) {

                X2Criteria criteria = new X2Criteria();
                String issuedDateField =
                        OnGraduationStudentProgram.FIELD_ISSUED_DATE.resolve(getDictionaryExtractor());
                criteria.addGreaterOrEqualThan(issuedDateField, getStartDate());
                criteria.addLessOrEqualThan(issuedDateField, getEndDate());
                String diplomaTypePath =
                        OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE.resolve(getDictionaryExtractor());
                criteria.addIn(diplomaTypePath,
                        getDictionaryExtractor()
                                .getRefCodesWithStateValue(
                                        OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE
                                                .getField(getDictionaryExtractor()))
                                .stream().map(code -> code.getCode()).collect(Collectors.toList()));

                X2Criteria bsidCriteria = new X2Criteria();
                bsidCriteria.addIn(
                        OnGraduationStudentProgram.FIELD_BSID_DIPLOMA_EARNED.resolve(getDictionaryExtractor()),
                        getCurrentSchoolBsids());

                X2Criteria schoolCriteria = new X2Criteria();
                schoolCriteria.addEmpty(
                        OnGraduationStudentProgram.FIELD_BSID_DIPLOMA_EARNED.resolve(getDictionaryExtractor()),
                        getBroker().getPersistenceKey());
                schoolCriteria.addIn(OnGraduationStudentProgram.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                        getCurrentSchoolOids());
                schoolCriteria.addOrCriteria(bsidCriteria);
                criteria.addAndCriteria(schoolCriteria);

                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    criteria.addEqualTo(OnGraduationStudentProgram.FIELD_STUDENT_OID.resolve(null),
                            getDebugStudentOid());
                }

                queries.add(getStudentSubQuery(OnGraduationStudentProgram.getX2BaseClass(),
                        criteria,
                        SisBeanPaths.GRADUATION_STUDENT_PROGRAM.student().getPath()));
            }

            if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_NON_ENROLMENT_E_OPTION)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addIn(OnCourseRequest.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                        getCurrentSchoolOids());

                criteria.addEqualTo(OnCourseRequest.FIELD_SCHOOL_YEAR.resolve(getDictionaryExtractor()),
                        getCurrentYear() + 1);

                criteria.addAndCriteria(getSchoolType().getCourseRequestCriteria(this));
                criteria.addIn(OnCourseRequest.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                        getCurrentSchoolOids());

                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    criteria.addEqualTo(OnCourseRequest.FIELD_STUDENT_OID.resolve(null), getDebugStudentOid());
                }

                queries.add(getStudentSubQuery(OnCourseRequest.getX2BaseClass(),
                        criteria,
                        SisBeanPaths.GRADUATION_STUDENT_PROGRAM.student().getPath()));
            }

            if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_NON_ENROLMENT_SHSM)) {
                X2Criteria criteria = ToolBean.getCriteria(getBroker(), ShsmAssessment.class).copy();
                criteria.addIn(OnStudentAssessment.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                        getCurrentSchoolOids());
                criteria.addBetween(OnStudentAssessment.FIELD_DATE.resolve(getDictionaryExtractor()), getStartDate(),
                        getEndDate());

                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    criteria.addEqualTo(OnStudentAssessment.FIELD_STUDENT_OID.resolve(null), getDebugStudentOid());
                }

                queries.add(getStudentSubQuery(OnStudentAssessment.getX2BaseClass(), criteria,
                        SisBeanPaths.STUDENT_ASSESSMENT.student().getPath()));
            }

            if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_NON_ENROLMENT)) {
                X2Criteria criteria = ToolBean.getCriteria(getBroker(), OssltAssessment.class).copy();
                criteria.addIn(OnStudentAssessment.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                        getCurrentSchoolOids());
                criteria.addBetween(OnStudentAssessment.FIELD_DATE.resolve(getDictionaryExtractor()), getStartDate(),
                        getEndDate());

                queries.add(getStudentSubQuery(OnStudentAssessment.getX2BaseClass(), criteria,
                        SisBeanPaths.STUDENT_ASSESSMENT.student().getPath()));

                criteria = ToolBean.getCriteria(getBroker(), CommunityInvolvementAssessment.class).copy();
                criteria.addIn(OnStudentAssessment.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                        getCurrentSchoolOids());
                criteria.addBetween(OnStudentAssessment.FIELD_DATE.resolve(getDictionaryExtractor()), getStartDate(),
                        getEndDate());

                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    criteria.addEqualTo(OnStudentAssessment.FIELD_STUDENT_OID.resolve(null), getDebugStudentOid());
                }

                queries.add(getStudentSubQuery(OnStudentAssessment.getX2BaseClass(), criteria,
                        SisBeanPaths.STUDENT_ASSESSMENT.student().getPath()));



                X2Criteria scholarshipCriteria = new X2Criteria();
                scholarshipCriteria
                        .addEqualTo(OnGraduationStudentProgram.FIELD_PROGRAM_NAME.resolve(getDictionaryExtractor()),
                                OnGraduationStudentProgram.GPR_NAME_OS);
                scholarshipCriteria.addBetween(
                        OnGraduationStudentProgram.FIELD_ISSUED_DATE.resolve(getDictionaryExtractor()),
                        getStartDate(), getEndDate());
                scholarshipCriteria.addIn(
                        OnGraduationStudentProgram.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                        getCurrentSchoolOids());

                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    scholarshipCriteria.addEqualTo(OnGraduationStudentProgram.FIELD_STUDENT_OID.resolve(null),
                            getDebugStudentOid());
                }

                queries.add(getStudentSubQuery(OnGraduationStudentProgram.getX2BaseClass(), scholarshipCriteria,
                        SisBeanPaths.GRADUATION_STUDENT_PROGRAM.student().getPath()));
            }

            if (!queries.isEmpty()) {
                Set<String> stdOidsToLoad = new HashSet();
                for (SubQuery subQuery : queries) {
                    List<String> stdOids = (List<String>) getBroker().getSubQueryCollectionByQuery(subQuery);
                    if (getDebugDetail()) {
                        String stdList =
                                stdOids.stream()
                                        .map(oid -> ToolBean.getBeanByOid(getBroker(), OnsisStudent.class, oid))
                                        .map(OnsisStudent::getNameView).collect(Collectors.toList()).toString();
                        log(subQuery.toString() + " includes " + stdList);
                    }
                    stdOidsToLoad.addAll(stdOids);
                }

                students.addAll(stdOidsToLoad);
                stdOidsToLoad.removeAll(ToolBean.getCachedToolBeanOids(OnsisStudent.class));
                if (!stdOidsToLoad.isEmpty()) {
                    ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), OnsisStudent.class, stdOidsToLoad);
                }
            }
        }

        /**
         * Gets the formats by id map.
         *
         * @return Map
         */
        private Map<String, ExportFormatDefinition> getFormatsByIdMap() {
            return m_formatsById;
        }

        /**
         * Gets the format fields.
         *
         * @param formatOid String
         * @return List
         */
        private List<ExportFormatField> getFormatFields(String formatOid) {
            if (m_formatFields == null) {
                X2Criteria formatFieldsCriteria = new X2Criteria();
                formatFieldsCriteria.addBeginsWith(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                        + ExportFormatDefinition.COL_PROCEDURE_ID, FORMAT_PREFIX_ONSIS);
                QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, formatFieldsCriteria);
                m_formatFields = getBroker().getGroupedCollectionByQuery(query,
                        ExportFormatField.COL_DEFINITION_OID, 20);

                // throw an exception if duplicated XML Paths are found
                for (Entry<String, List<ExportFormatField>> definition : m_formatFields.entrySet()) {
                    Set<String> paths = new HashSet();
                    for (ExportFormatField field : definition.getValue()) {
                        if (paths.contains(field.getSifPath())) {
                            throw new IllegalStateException("Duplicate XML Path " + field.getSifPath() + " in "
                                    + field.getDefinition().getProcedureId());
                        }
                        if (!StringUtils.isEmpty(field.getSifPath()) && !field.getSifPath().equals("NO_FIELD")) {
                            paths.add(field.getSifPath());
                        }
                    }
                }
            }
            return m_formatFields.get(formatOid);
        }

        /**
         * Gets the master schedule criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getSectionCriteria() {
            X2Criteria sectionCriteria = ToolBean.getCriteria(getBroker(), OnSection.class).copy();

            // From active Schedule for the selected year.
            sectionCriteria.addEqualTo(SisBeanPaths.SCHEDULE_MASTER.schedule().activeSchoolScheduleContexts()
                    .districtContextOid().getPath(), getCurrentContext().getOid());

            // Require section term to start before report date.
            sectionCriteria.addLessOrEqualThan(
                    SisBeanPaths.SCHEDULE_MASTER.scheduleTerm().scheduleTermDates().startDate().getPath(),
                    getEndDate());

            // Require section term to end after submission period start date.
            sectionCriteria.addGreaterOrEqualThan(
                    SisBeanPaths.SCHEDULE_MASTER.scheduleTerm().scheduleTermDates().endDate().getPath(),
                    getStartDate());

            // Limit to selected schools
            sectionCriteria.addIn(SisBeanPaths.SCHEDULE_MASTER.schedule().schoolOid().getPath(),
                    getCurrentSchoolOids());

            X2Criteria inclusionCriteria = getSchoolType().getMasterScheduleInclusionCriteria(this);
            if (inclusionCriteria != null) {
                sectionCriteria.addAndCriteria(inclusionCriteria);
            }
            return sectionCriteria;
        }

        /**
         * Gets the staff position criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getStaffPositionCriteria() {
            /*
             * End date is either null or on/after the report date
             */
            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addIsNull(ToolStaffPosition.FIELD_END_DATE.resolve(null));

            X2Criteria endDateCriteria2 = new X2Criteria();
            PlainDate endDate = getGlobalData().getStartDate();
            if (getGlobalData().getSubmissionType().isElementarySubmission()) {
                endDate = getGlobalData().getDateNovember1PrevYear();
            }
            endDateCriteria2.addGreaterOrEqualThan(ToolStaffPosition.FIELD_END_DATE.resolve(null), endDate);

            endDateCriteria.addOrCriteria(endDateCriteria2);

            /*
             * Start date is either null or on/before the report date
             */
            X2Criteria startDateCriteria = new X2Criteria();
            X2Criteria startDateCriteria2 = new X2Criteria();

            SubmissionType submissionType = getGlobalData().getSubmissionType();
            PlainDate periodEndDate = submissionType.getPeriodEndDate();

            startDateCriteria2.addLessOrEqualThan(ToolStaffPosition.FIELD_START_DATE.resolve(null), periodEndDate);
            startDateCriteria.addEmpty(ToolStaffPosition.FIELD_START_DATE.resolve(null),
                    getBroker().getPersistenceKey());
            startDateCriteria.addOrCriteria(startDateCriteria2);

            /*
             * The full criteria includes dates and the subquery
             */
            X2Criteria initialCriteria = new X2Criteria();

            initialCriteria.addIn(ToolStaffPosition.FIELD_SCHOOL_OID.resolve(null),
                    getGlobalData().getCurrentSchoolOids());
            initialCriteria.addAndCriteria(endDateCriteria);
            initialCriteria.addAndCriteria(startDateCriteria);

            initialCriteria.addNotEmpty(OnStaffPosition.FIELD_MEN.resolve(null), getBroker().getPersistenceKey());
            initialCriteria.addNotEmpty(ToolStaffPosition.FIELD_JOB_CODE.resolve(null),
                    getBroker().getPersistenceKey());

            String debugStaffOid = (String) getParameter(INPUT_PARAM_DEBUG_STAFF_OID);
            if (!StringUtils.isBlank(debugStaffOid)) {
                initialCriteria.addEqualTo(ToolStaffPosition.FIELD_STAFF_OID.resolve(null), debugStaffOid);
            }
            return initialCriteria;
        }

        /**
         * Gets the student sub query.
         *
         * @param clazz Class
         * @param criteria X2Criteria
         * @param pathToStudent String
         * @return Sub query
         */
        private SubQuery getStudentSubQuery(Class clazz, X2Criteria criteria, String pathToStudent) {
            if (!getIncludeEmptyOen()) {
                String oenJavaName = OnsisStudent.FIELD_OEN.resolve(getDictionaryExtractor());
                String pathToOen =
                        pathToStudent == null ? oenJavaName
                                : (pathToStudent + ModelProperty.PATH_DELIMITER + oenJavaName);
                criteria.addNotEmpty(pathToOen, getBroker().getPersistenceKey());
            }
            String pathToOid = pathToStudent == null ? X2BaseBean.COL_OID : pathToStudent;
            return new SubQuery(clazz, pathToOid, criteria);
        }

        /**
         * Gets the student schedule change criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getStudentScheduleChangeCriteria() {
            X2Criteria scheduleCriteria = new X2Criteria();
            // From active Schedule for the selected year.
            scheduleCriteria.addEqualTo(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().activeSchoolScheduleContexts()
                    .districtContextOid().getPath(), getCurrentContext().getOid());

            // From current schools.
            scheduleCriteria.addIn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.schedule().schoolOid().getPath(),
                    getCurrentSchoolOids());

            String pathPrefix = SisBeanPaths.STUDENT_SCHEDULE_CHANGE.masterSchedule().getPath()
                    + ModelProperty.PATH_DELIMITER;

            scheduleCriteria.addNotEqualTo(
                    pathPrefix + ModelProperty.PATH_DELIMITER
                            + OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS.resolve(getDictionaryExtractor()),
                    BooleanAsStringConverter.TRUE);

            scheduleCriteria.addNotEqualTo(pathPrefix + ModelProperty.PATH_DELIMITER
                    + OnSection.FIELD_CRS_EXCLUDE.resolve(getDictionaryExtractor()),
                    BooleanAsStringConverter.TRUE);


            if (SubmissionSchoolType.PUBLIC_SECONDARY.equals(getSchoolType())
                    || SubmissionSchoolType.ECPP.equals(getSchoolType())) {
                String fieldCourseCodeType =
                        pathPrefix + OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictionaryExtractor());
                String fieldMinistryCode =
                        pathPrefix + OnSection.FIELD_MINISTRY_COURSE_CODE.resolve(getDictionaryExtractor());
                scheduleCriteria.addIn(fieldCourseCodeType, getDictionaryExtractor().getRefCodesWithStateValue(
                        OnSection.FIELD_COURSE_CODE_TYPE.getField(getDictionaryExtractor()),
                        OnSection.COURSE_CODE_TYPES_MDC_DCC_LDC)
                        .stream()
                        .map(code -> code.getCode()).collect(Collectors.toList()));
                scheduleCriteria.addNotEmpty(fieldMinistryCode, getBroker().getPersistenceKey());
            } else if (SubmissionSchoolType.PUBLIC_ELEMENTARY.equals(getSchoolType())) {
                X2Criteria sectionSelectionCriteria = new X2Criteria();
                sectionSelectionCriteria.addEqualTo(
                        pathPrefix + OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictionaryExtractor()),
                        OnSection.COURSE_CODE_TYPE_HOMEROOM);

                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addIn(
                        pathPrefix + OnSection.FIELD_ELEMENTARY_SUBJECT_TYPE.resolve(getDictionaryExtractor()),
                        getDictionaryExtractor().getRefCodesWithStateValue(
                                OnSection.FIELD_ELEMENTARY_SUBJECT_TYPE.getField(getDictionaryExtractor()), null)
                                .stream()
                                .map(code -> code.getCode()).collect(Collectors.toList()));
                sectionSelectionCriteria.addOrCriteria(orCriteria);

                scheduleCriteria.addAndCriteria(sectionSelectionCriteria);
            } else {
                scheduleCriteria.addNotNull(SisBeanPaths.STUDENT_SCHEDULE_CHANGE.oid().getPath());
            }

            if (!StringUtils.isEmpty(getDebugStudentOid())) {
                scheduleCriteria.addEqualTo(ToolStudentScheduleChange.FIELD_STUDENT_OID.resolve(null),
                        getDebugStudentOid());
            }

            return scheduleCriteria;
        }

        /**
         * Gets the student schedule criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getStudentScheduleCriteria() {
            X2Criteria scheduleCriteria = new X2Criteria();
            // From active Schedule for the selected year.
            scheduleCriteria.addEqualTo(SisBeanPaths.STUDENT_SCHEDULE.schedule().activeSchoolScheduleContexts()
                    .districtContextOid().getPath(), getCurrentContext().getOid());

            // From current schools.
            scheduleCriteria.addIn(SisBeanPaths.STUDENT_SCHEDULE.schedule().schoolOid().getPath(),
                    getCurrentSchoolOids());

            String pathPrefix = SisBeanPaths.STUDENT_SCHEDULE.section().getPath()
                    + ModelProperty.PATH_DELIMITER;

            scheduleCriteria.addNotEqualTo(
                    pathPrefix + ModelProperty.PATH_DELIMITER
                            + OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS.resolve(getDictionaryExtractor()),
                    BooleanAsStringConverter.TRUE);

            scheduleCriteria.addNotEqualTo(pathPrefix + ModelProperty.PATH_DELIMITER
                    + OnSection.FIELD_CRS_EXCLUDE.resolve(getDictionaryExtractor()),
                    BooleanAsStringConverter.TRUE);

            if (SubmissionSchoolType.PUBLIC_SECONDARY.equals(getSchoolType())
                    || SubmissionSchoolType.ECPP.equals(getSchoolType())) {
                String fieldCourseCodeType =
                        pathPrefix + OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictionaryExtractor());
                String fieldMinistryCode =
                        pathPrefix + OnSection.FIELD_MINISTRY_COURSE_CODE.resolve(getDictionaryExtractor());
                scheduleCriteria.addIn(fieldCourseCodeType, getDictionaryExtractor().getRefCodesWithStateValue(
                        OnSection.FIELD_COURSE_CODE_TYPE.getField(getDictionaryExtractor()),
                        OnSection.COURSE_CODE_TYPES_MDC_DCC_LDC)
                        .stream()
                        .map(code -> code.getCode()).collect(Collectors.toList()));
                scheduleCriteria.addNotEmpty(fieldMinistryCode, getBroker().getPersistenceKey());
            } else if (SubmissionSchoolType.PUBLIC_ELEMENTARY.equals(getSchoolType())) {
                X2Criteria sectionSelectionCriteria = new X2Criteria();
                sectionSelectionCriteria.addEqualTo(
                        pathPrefix + OnSection.FIELD_COURSE_CODE_TYPE.resolve(getDictionaryExtractor()),
                        OnSection.COURSE_CODE_TYPE_HOMEROOM);

                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addIn(
                        pathPrefix + OnSection.FIELD_ELEMENTARY_SUBJECT_TYPE.resolve(getDictionaryExtractor()),
                        getDictionaryExtractor().getRefCodesWithStateValue(
                                OnSection.FIELD_ELEMENTARY_SUBJECT_TYPE.getField(getDictionaryExtractor()), null)
                                .stream()
                                .map(code -> code.getCode()).collect(Collectors.toList()));
                sectionSelectionCriteria.addOrCriteria(orCriteria);

                scheduleCriteria.addAndCriteria(sectionSelectionCriteria);
            } else {
                scheduleCriteria.addNotNull(SisBeanPaths.STUDENT_SCHEDULE.oid().getPath());
            }

            if (!StringUtils.isEmpty(getDebugStudentOid())) {
                scheduleCriteria.addEqualTo(ToolStudentSchedule.FIELD_STUDENT_OID.resolve(null), getDebugStudentOid());
            }

            return scheduleCriteria;
        }

        /**
         * Preload students.
         *
         * @return true, if successful
         */
        private boolean preloadStudents() {
            boolean returnValue = false;
            if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT)) {
                pushDataDefinition("studentSpansCriteria");
                ToolBean.setPreference(ToolBean.PREFERENCE_ENR_IGNORE_FOR_SPANS, new Predicate<OnEnrollment>() {
                    @Override
                    public boolean test(OnEnrollment enr) {
                        return enr.isFteRecord();
                    }
                });

                ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE,
                        getContextByYear(getCurrentContext().getSchoolYear() - 1).getStartDate());
                ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, getEndDate());

                // need to make sure the field is resolved before adding as exclude criteria
                OnsisStudent.FIELD_EXCLUDE_FROM_REPORTING.resolve(getDictionaryExtractor());

                List<String> currentSchoolOids = getCurrentSchoolOids();

                X2Criteria studentLimitingCriteria = new X2Criteria();
                studentLimitingCriteria.addNotEmpty(OnsisStudent.FIELD_OEN.resolve(getDictionaryExtractor()),
                        getBroker().getPersistenceKey());

                EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                        .setSchoolOids(currentSchoolOids)
                        .setExcludeStudent(OnsisStudent.FIELD_EXCLUDE_FROM_REPORTING)
                        .setStudentLimitingCriteria(studentLimitingCriteria);
                if (!StringUtils.isEmpty(getDebugStudentOid())) {
                    spanCriteria.setLimitingStudentOids(Arrays.asList(getDebugStudentOid()));
                }
                X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
                popFieldDefinition();

                // load students using filterable
                pushDataDefinition("filterable: " + OnsisStudent.class.getName());
                FilterableFactory.create(getBroker(), getDictionaryExtractor(), OnsisStudent.class, candidateCriteria,
                        null);
                popFieldDefinition();

                pushDataDefinition("preload: " + ToolEnrollment.class.getName());
                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                        ToolStudent.CHILD_STUDENT_ENROLLMENTS);
                popFieldDefinition();

                pushDataDefinition("preload: " + ToolStudentSchool.class.getName());
                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                        ToolStudent.CHILD_STUDENT_SCHOOLS);
                popFieldDefinition();

                // Create list of students enrolled in the submission period
                Set<OnsisStudent> allStudents =
                        ToolBean.getCachedToolBeans(OnsisStudent.class).stream().filter(new Predicate<OnsisStudent>() {

                            @Override
                            public boolean test(OnsisStudent student) {
                                List<AnnualSpan> spans = student.getEnrollmentSpans(getBroker(), false, false);
                                boolean included = spans.stream().filter(span -> {
                                    boolean isPresent;
                                    try {
                                        isPresent = currentSchoolOids.contains(span.getSchool().getOid())
                                                && getDateRange().isOverlap(
                                                        Range.of(span.getSpanStartDate(),
                                                                span.getLastActiveInSessionDate()));
                                    } catch (Exception ex) {
                                        String message =
                                                "ProcessingContext: \nstudent: " + student +
                                                        "\nspan: " + span +
                                                        "\nspan.getSchoolCalendar: " + span.getSchoolCalendar() +
                                                        "\n";
                                        throw new RuntimeException(message, ex);
                                    }
                                    return isPresent;
                                }).findAny().isPresent();
                                return included;
                            }
                        }).collect(Collectors.toSet());

                if (getExtractHelper() != null) {
                    pushDataDefinition("addCsvStudents");
                    addCsvStudents(allStudents);
                    popFieldDefinition();
                }
                Set<String> allStudentsList =
                        allStudents.stream().map(OnsisStudent::getOid).collect(Collectors.toSet());
                pushDataDefinition("addFteStudents");
                addFteStudents(allStudentsList);
                popFieldDefinition();

                pushDataDefinition("addNonEnrolledStudents");
                addNonEnrolledStudents(allStudentsList);
                popFieldDefinition();


                // Filter to include only students contained in allStudents set
                ToolBean.filterCachedToolBeans(OnsisStudent.class,
                        student -> allStudentsList.contains(student.getOid()));
                returnValue = true;

                pushDataDefinition("preload: " + ToolEnrollment.class.getName() + "-AUGMENT");
                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                        ToolStudent.CHILD_STUDENT_ENROLLMENTS.setLoaderMode(LoaderMode.AUGMENT));
                popFieldDefinition();

                pushDataDefinition("preload: " + ToolStudentSchool.class.getName() + "-AUGMENT");
                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                        ToolStudent.CHILD_STUDENT_SCHOOLS.setLoaderMode(LoaderMode.AUGMENT));
                popFieldDefinition();

                // Load additional children
                // *************************
                // ToolPersonAddress
                // *************************
                pushDataDefinition("preload: " + ToolPersonAddress.class.getName());
                X2Criteria personAddressCriteria = new X2Criteria();
                personAddressCriteria.addEqualTo(ToolPersonAddress.FIELD_ADDRESS_TYPE.resolve(null),
                        ToolPersonAddress.ADDRESS_TYPE_PHYSICAL);
                personAddressCriteria.addLessOrEqualThan(ToolPersonAddress.FIELD_START_DATE.resolve(null),
                        getSubmissionType().getCountDate());
                X2Criteria dateCriteria = new X2Criteria();
                dateCriteria.addEmpty(ToolPersonAddress.FIELD_END_DATE.resolve(null), getBroker().getPersistenceKey());
                X2Criteria orDateCriteria = new X2Criteria();
                orDateCriteria.addGreaterOrEqualThan(ToolPersonAddress.FIELD_END_DATE.resolve(null),
                        getSubmissionType().getCountDate());
                dateCriteria.addOrCriteria(orDateCriteria);
                personAddressCriteria.addAndCriteria(dateCriteria);

                ToolBean.addAndCriteria(getBroker(), ToolPersonAddress.class, personAddressCriteria);

                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolPersonAddress.FIELD_START_DATE, ToolPersonAddress.FIELD_END_DATE),
                        ToolStudent.CHILD_PERSON_ADDRESSES);
                popFieldDefinition();

                // *************************
                // OnsisAddress
                // *************************
                pushDataDefinition("loadByOid: " + OnAddress.class.getName());
                Set<String> addressOidsToLoad = Stream.concat(
                        ToolBean.getCachedToolBeans(ToolPersonAddress.class).stream()
                                .map(ToolPersonAddress::getAddressOid),
                        ToolBean.getCachedToolBeans(OnsisStudent.class).stream()
                                .map(ToolStudent::getPhysicalAddressOid))
                        .collect(Collectors.toSet());
                ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), OnAddress.class, addressOidsToLoad);
                popFieldDefinition();

                // *************************
                // OnsisStudentAttendance - year to date
                // *************************
                pushDataDefinition("preload: " + OnStudentAttendance.class.getName());
                X2Criteria attCriteria = new X2Criteria();
                attCriteria.addIn(ToolStudentAttendance.FIELD_SCHOOL_OID.resolve(null), getCurrentSchoolOids());
                attCriteria.addBetween(ToolStudentAttendance.FIELD_DATE.resolve(null),
                        getCurrentContext().getStartDate(), getEndDate());

                ToolBean.addAndCriteria(getBroker(), OnStudentAttendance.class, attCriteria);

                ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolStudent.CHILD_STUDENT_ATTENDANCE);
                popFieldDefinition();

                // *************************
                // OnsisStudentExtendedDay
                // *************************
                pushDataDefinition("preload: " + OnStudentExtendedDay.class.getName());
                ToolBean.addAndCriteria(getBroker(), OnStudentExtendedDay.class, ToolStudentProgramParticipation
                        .getDateRangeCriteria(getBroker(), getStartDate(), getEndDate()));
                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                        OnsisStudent.CHILD_EXTENDED_DAY_PROGRAMS);
                popFieldDefinition();

                // *************************
                // OnsisStudentOyap
                // *************************
                pushDataDefinition("preload: " + OnStudentOyap.class.getName());
                ToolBean.addAndCriteria(getBroker(), OnStudentOyap.class, ToolStudentProgramParticipation
                        .getDateRangeCriteria(getBroker(), getEndDate(), getEndDate()));
                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                        OnsisStudent.CHILD_OYAP_PROGRAMS);
                popFieldDefinition();

                // *************************
                // OnsisStudentSalep
                // *************************
                if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_ENROLMENT_SALEP)) {
                    pushDataDefinition("preload: " + OnStudentSalepDetail.class.getName());
                    ToolBean.addAndCriteria(getBroker(), OnStudentSalep.class, ToolStudentProgramParticipation
                            .getDateRangeCriteria(getBroker(), getStartDate(), getEndDate()));
                    ToolBean.preload(getBroker(), getDictionaryExtractor(),
                            Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                            OnsisStudent.CHILD_SALEP_PROGRAMS);
                    ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                            OnStudentSalep.CHILD_PROGRAM_DETAILS);
                    popFieldDefinition();
                }

                // *************************
                // OnsisStudentSped
                // *************************
                pushDataDefinition("preload: " + OnStudentSped.class.getName());
                ToolBean.addAndCriteria(getBroker(), OnStudentSped.class, ToolStudentProgramParticipation
                        .getDateRangeCriteria(getBroker(), getStartDate(), getEndDate()));
                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                        OnsisStudent.CHILD_SPED_PROGRAMS);
                ToolBean.preload(getBroker(), getDictionaryExtractor(), null, OnStudentSped.CHILD_PROGRAM_DETAILS);
                popFieldDefinition();

                // *************************
                // OnsisGraduationStudentProgram
                // *************************
                pushDataDefinition("preload: " + OnGraduationStudentProgram.class.getName());
                ToolBean.preload(getBroker(), getDictionaryExtractor(), Arrays.asList(ToolBean.FIELD_OID),
                        OnsisStudent.CHILD_GRADUATION_STUDENT_PROGRAMS);
                popFieldDefinition();

                // *************************
                // OnsisGraduationStudentWaiver
                // *************************
                pushDataDefinition("preload: " + OnGraduationStudentWaiver.class.getName());
                ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                        OnGraduationStudentProgram.CHILD_WAIVERS);
                popFieldDefinition();

                // *************************
                // OssltAssessment
                // *************************
                // load most recent on or before report end date
                // sort descending on date
                pushDataDefinition("preload: " + OssltAssessment.class.getName());
                ToolBean.clearAllCachedToolBeans(OssltAssessment.class);
                OssltAssessment.PRELOAD_SET.clear();
                X2Criteria criteria = new X2Criteria();
                criteria.addLessOrEqualThan(OnStudentAssessment.FIELD_DATE.resolve(null), getEndDate());

                ToolBean.addAndCriteria(getBroker(), OssltAssessment.class, criteria);

                ToolBean.preload(getBroker(), getDictionaryExtractor(),
                        Arrays.asList(new ToolBean.ToolBeanColumn(SisBeanPaths.STUDENT_ASSESSMENT.date(), false)),
                        OnsisStudent.CHILD_OSSLT_ASSESSMENTS);
                popFieldDefinition();

                // *************************
                // CommunityInvolvementAssessment
                // *************************
                pushDataDefinition("preload: " + CommunityInvolvementAssessment.class.getName());
                X2Criteria asmCriteria = new X2Criteria();
                asmCriteria.addLessOrEqualThan(OnStudentAssessment.FIELD_DATE.resolve(null), getEndDate());

                ToolBean.addAndCriteria(getBroker(), CommunityInvolvementAssessment.class, asmCriteria);

                ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                        OnsisStudent.CHILD_COMMUNITY_INVOLVEMENT_ASSESSMENTS);
                popFieldDefinition();

                if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_TRANSITION_DATE)) {
                    // *************************
                    // OnsisStudentECPPProgram
                    // *************************
                    pushDataDefinition("preload: " + OnStudentECPPProgram.class.getName());
                    X2Criteria ecppCriteria = ToolStudentProgramParticipation
                            .getDateRangeCriteria(getBroker(), getStartDate(), getEndDate());
                    ecppCriteria.addIn(
                            OnStudentProgramParticipation.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                            getCurrentSchoolOids());
                    ToolBean.addAndCriteria(getBroker(), OnStudentECPPProgram.class, ecppCriteria);
                    ToolBean.preload(getBroker(), getDictionaryExtractor(),
                            Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                            OnsisStudent.CHILD_ECPP_PROGRAMS);
                    popFieldDefinition();
                }

                if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_ENROLMENT_SLP)) {
                    // *************************
                    // OnsisStudentELLProgram
                    // *************************
                    pushDataDefinition("preload: " + OnStudentELLProgram.class.getName());
                    ToolBean.addAndCriteria(getBroker(), OnStudentELLProgram.class, ToolStudentProgramParticipation
                            .getDateRangeCriteria(getBroker(), getCurrentContext().getStartDate(),
                                    getCurrentContext().getEndDate()));
                    ToolBean.preload(getBroker(), getDictionaryExtractor(),
                            Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                            OnsisStudent.CHILD_ELL_PROGRAMS);
                    popFieldDefinition();

                    // *************************
                    // OnsisStudentSLPProgram
                    // *************************
                    pushDataDefinition("preload: " + OnStudentSLPProgram.class.getName());
                    ToolBean.addAndCriteria(getBroker(), OnStudentSLPProgram.class, ToolStudentProgramParticipation
                            .getDateRangeCriteria(getBroker(), getCurrentContext().getStartDate(),
                                    getCurrentContext().getEndDate()));
                    ToolBean.preload(getBroker(), getDictionaryExtractor(),
                            Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                            OnsisStudent.CHILD_SLP_PROGRAMS);
                    popFieldDefinition();

                    // *************************
                    // OnsisStudentSLPProgramFrench
                    // *************************
                    pushDataDefinition("preload: " + OnStudentSLPProgramFrench.class.getName());
                    ToolBean.addAndCriteria(getBroker(), OnStudentSLPProgramFrench.class,
                            ToolStudentProgramParticipation
                                    .getDateRangeCriteria(getBroker(), getCurrentContext().getStartDate(),
                                            getCurrentContext().getEndDate()));
                    ToolBean.preload(getBroker(), getDictionaryExtractor(),
                            Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE),
                            OnsisStudent.CHILD_SLP_FR_PROGRAMS);
                    popFieldDefinition();
                }
                if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_ENROLMENT_SHSM_CERTIFICATION)) {
                    pushDataDefinition("preload: " + ShsmAssessment.class.getName());
                    ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                            OnsisStudent.CHILD_SHSM_ASSESSMENTS);
                    popFieldDefinition();
                }
                if (getFieldsRepository().contains(FieldsRepository.PATH_STUDENT_SUBJECT_STRAND)) {
                    // First load transcript rubric
                    pushDataDefinition("preload: " + ToolTranscriptRubric.class.getName());
                    X2Criteria trrCriteria = new X2Criteria();
                    trrCriteria.addIn(ToolTranscriptRubric.FIELD_SCHOOL_OID.resolve(null), getCurrentSchoolOids());

                    ToolBean.addAndCriteria(getBroker(), OnTranscriptRubric.class, trrCriteria);
                    ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                            OnsisStudent.CHILD_TRANSCRIPT_RUBRICS);
                    popFieldDefinition();

                    // then load RubricAssessment
                    pushDataDefinition("loadByOid: " + ToolRubricAssessment.class.getName());
                    List<String> rbaOids = ToolBean.getCachedToolBeans(OnTranscriptRubric.class).stream()
                            .map(trr -> trr.getRubricAssessmentOid())
                            .collect(Collectors.toList());
                    ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), ToolRubricAssessment.class, rbaOids);
                    popFieldDefinition();

                    // then load RubricAssessmentPerformance
                    pushDataDefinition("preload: " + ToolRubricAssessmentPerformance.class.getName());
                    X2Criteria rapCriteria = new X2Criteria();
                    rapCriteria.addIn(ToolRubricAssessmentPerformance.FIELD_SCHOOL_OID.resolve(null),
                            getCurrentSchoolOids());

                    ToolBean.addAndCriteria(getBroker(), OnRubricAssessmentPerformance.class, rapCriteria);
                    ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                            ToolRubricAssessment.CHILD_RUBRIC_ASSESSMENT_PERFORMANCES);
                    popFieldDefinition();

                    pushDataDefinition("loadByOid: " + ToolRubricDefinition.class.getName());
                    Set<String> rbdOids = ToolBean.getCachedToolBeans(OnRubricAssessmentPerformance.class).stream()
                            .map(rap -> rap.getRubricDefinitionOid())
                            .collect(Collectors.toSet());
                    ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), ToolRubricDefinition.class, rbdOids);
                    popFieldDefinition();

                    // then load RubricCriterion
                    pushDataDefinition("preload: " + ToolRubricCriterion.class.getName());
                    ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                            ToolRubricDefinition.CHILD_RUBRIC_CRITERIA);
                    popFieldDefinition();

                    // populate child transcript rubrics for rubric assessment
                    pushDataDefinition("preload: " + ToolTranscriptRubric.class.getName());
                    ToolBean.reload(getBroker(), getDictionaryExtractor(), null,
                            ToolRubricAssessment.CHILD_TRANSCRIPT_RUBRICS);
                    popFieldDefinition();

                }
            }
            return returnValue;
        }

        /**
         * Sets the format.
         *
         * @param procedureId String
         * @param format ExportFormatDefinition
         */
        private void setFormat(String procedureId, ExportFormatDefinition format) {
            m_formatsById.put(procedureId, format);
        }
    }

    /**
     * Determine if <ACTION> is ADD or UPDATE
     * by checking if record key was present in Onsis CSV extract file.
     *
     */
    public final class OnsisRetrieverAction extends XmlFieldRetriever {
        public static final String ACTION_ADD = "ADD";
        public static final String ACTION_UPDATE = "UPDATE";
        public static final String ACTION_DELETE = "DELETE";

        public static final String CALC_ID = "ACTION";

        public static final String ELEMENT_NAME_ACTION = "ACTION";

        public static final String FIELD_ACTION = "Action";

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
            return getActionValue((OnsisStateReportData) data, (OnsisStateReportEntity) entity);
        }

        /**
         * Lookup this entity's identifier in the publishOptions map for this topic.
         * If not found, return ADD.
         * If found, return CHANGE.
         *
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @param parentElement Element
         * @see com.follett.fsc.aspensif.framework.OnsisFieldRetriever#renderField(com.follett.fsc.aspensif.onsis.elements.OnsisXmlElement)
         */
        @Override
        public void renderField(OnsisStateReportData reportData,
                                OnsisStateReportEntity entity,
                                FieldDefinition field,
                                Element parentElement) {
            String action;
            if (ACTION_ADD.equals(entity.getReportData().getParentEntity().getFieldValue(FIELD_ACTION))) {
                action = ACTION_ADD;
            } else {
                action = getActionValue(reportData, entity);
            }
            appendTextElement(ELEMENT_NAME_ACTION, action, parentElement);
        }

        /**
         * Gets the action value.
         *
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @return String
         */
        protected String getActionValue(OnsisStateReportData reportData, OnsisStateReportEntity entity) {
            OnsisExtractHelper extractHelper = reportData.getGlobalData().getExtractHelper();
            String extractType = reportData.getExtractType(reportData.getProcedureId());
            OnsisExtractRecords matcher = extractHelper.getMatcherByExtractType(extractType);

            // generate the fullKeys for this exported entity row
            Collection<String> generatedEntityKeys = reportData.generateKeys(entity, matcher);

            // store fullKey in the regular or temporary generated key list
            for (String fullKey : generatedEntityKeys) {
                entity.getTempRowKeys().add(fullKey);
            }

            // look up fullKey in the CSV list
            boolean recordExists = false;
            for (String generatedEntityKey : generatedEntityKeys) {
                if (matcher.doesRecordExist(generatedEntityKey,
                        reportData.getExistingKeys(getProcedureId()),
                        getMatchOnEmptyKeyValueFn())) {
                    recordExists = true;
                    break;
                }
            }
            String result = recordExists ? ACTION_UPDATE : ACTION_ADD;
            // if (reportData.getDebugDetail()) {
            // result += "-" + fullKeys.toString();
            // }
            return result;
        }
    }

    /**
     * The Class OnsisRetrieverClearPending.
     */
    public class OnsisRetrieverClearPending extends XmlFieldRetriever {
        public static final String CALC_ID = "CLEAR_PENDING";
        private static final String ELEMENT_CLEAR_PENDING_AREA = "CLEAR_PENDING_AREA";
        private static final String RESULT_N = "N";
        private static final String RESULT_Y = "Y";

        /**
         * Render field.
         *
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @param parentElement Element
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.XmlFieldRetriever#renderField(com.x2dev.procedures.statereporting.on.OnsisStateReportData,
         *      com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, org.w3c.dom.Element)
         */
        @Override
        public void renderField(OnsisStateReportData reportData,
                                OnsisStateReportEntity entity,
                                FieldDefinition field,
                                Element parentElement)
                throws X2BaseException {
            if (!ELEMENT_CLEAR_PENDING_AREA.equals(field.getSifPath())) {
                return;
            }

            List<String> clearFields = reportData.getGlobalData().getSubmissionType().getClearFields();
            if (!clearFields.contains(ELEMENT_CLEAR_PENDING_AREA)) {
                appendTextElement(ELEMENT_CLEAR_PENDING_AREA, RESULT_N, parentElement);
            }

            for (String clearField : clearFields) {
                appendTextElement(clearField, RESULT_Y, parentElement);
            }
        }
    }

    /**
     * The Class RetrieveEntityProperty.
     */
    public class OnsisRetrieverEntityProperty implements FieldRetriever {
        public static final String CALC_ID = "ENTITY-PROP";

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

            OnsisStateReportData reportData = (OnsisStateReportData) data;
            OnsisStateReportEntity pluginEntity = (OnsisStateReportEntity) entity;

            // The propertyPath relative to the entity object
            String fieldParameter = (String) field.getParameter();

            Object result = null;
            try {
                // If no propertyPath defined, use the source field
                if (StringUtils.isEmpty(fieldParameter)) {
                    String beanPath = field.getBeanPath();
                    if (StringUtils.isEmpty(beanPath)) {
                        // Invalid chain
                        String message =
                                "Invalid beanPath (possibly due to missing alias) for export ["
                                        + reportData.getClass().getSimpleName() + "] field "
                                        + field.getFieldId() + " calcID [" + field.getCalcId()
                                        + "] calc-param [" + field.getParameter() + "]";
                        throw new RuntimeException(message);
                    }

                    result = pluginEntity.getFieldValueByBeanPath(beanPath);
                } else {
                    result = getResultForPropertyPath(pluginEntity, fieldParameter);
                }
            } catch (ValidationException validationException) {
                throw validationException;
            } catch (Exception ex) {
                if (ex instanceof InvocationTargetException) {
                    if (ex.getCause() instanceof ValidationException) {
                        throw (ValidationException) ((InvocationTargetException) ex).getCause();
                    }
                }
                // Invalid chain
                String message =
                        "Invalid configuration for field " + field.getFieldId() + " calcID [" + field.getCalcId()
                                + "] calc-param [" + field.getParameter() + "]";
                throw new RuntimeException(message, ex);
            }
            if (result instanceof String) {
                if (field.getConverter() == null) {
                    String procedureId = data.getProcedureId();
                    String fieldId = field.getFieldId();
                    ExportFormatField formatField = getFormatType(procedureId, fieldId);
                    if (ExportFormatField.FormatTypeCode.DATE.ordinal() == formatField.getFormatType()) {
                        return reportData.getGlobalData().parseDate(result);
                    }
                    if (ExportFormatField.FormatTypeCode.LOGICAL.ordinal() == formatField.getFormatType()) {
                        return Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(result));
                    }
                    if (ExportFormatField.FormatTypeCode.NUMBER.ordinal() == formatField.getFormatType()) {
                        Double resultDouble = Double.parseDouble((String) result);
                        return resultDouble;
                    }
                }
            }

            return result;
        }

        /**
         * Gets the format type.
         *
         * @param procedureId String
         * @param fieldId String
         * @return Export format field
         */
        private ExportFormatField getFormatType(String procedureId, String fieldId) {
            List<ExportFormatField> fields =
                    getGlobalData().getFormatFields(getFormat(procedureId).getOid());
            for (ExportFormatField eff : fields) {
                if (eff.getId().equals(fieldId)) {
                    return eff;
                }
            }
            return null;
        }
    }

    /**
     * The Class OnsisRetrieverFromCsv.
     */
    public class OnsisRetrieverFromCsv implements FieldRetriever {
        public static final String CALC_ID = "FROM-CSV";

        private static final String CALC_PARAM_ACADEMIC_YEAR = "ACADEMIC_YEAR";
        private static final String CALC_PARAM_PERIOD_TYPE = "PERIOD_TYPE";

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
            OnsisStateReportData reportData = (OnsisStateReportData) data;
            switch ((String) field.getParameter()) {
                case CALC_PARAM_ACADEMIC_YEAR:
                    return reportData.getGlobalData().getExtractHelper().getSubmissionPeriod().getSchoolYear();

                case CALC_PARAM_PERIOD_TYPE:
                    return reportData.getGlobalData().getExtractHelper().getSubmissionPeriod().getPeriodCode();

                default:
                    return null;
            }
        }
    }

    /**
     * The Class RetrieveNestedExport.
     */
    public class OnsisRetrieverNestedExport extends XmlFieldRetriever {
        public static final String CALC_ID = "ONSIS-CHILD-EXPORT";

        /**
         * Render field.
         *
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @param parentElement Element
         * @throws X2BaseException exception
         */
        @Override
        public void renderField(OnsisStateReportData reportData,
                                OnsisStateReportEntity entity,
                                FieldDefinition field,
                                Element parentElement)
                throws X2BaseException {
            try {
                OnsisStateReportData childReportData =
                        reportData.getGlobalData().getReportDataCache().getChildReportData(reportData, entity, field);

                childReportData.renderReportData(parentElement);

            } catch (Exception ex) {
                String msg = entity.getDebugInfo();
                RuntimeException debugEx = new RuntimeException(msg, ex);

                reportData.storeException(field, entity, debugEx);
            }
        }
    }

    /**
     * The Class RetrieveNestedExport.
     */
    public class OnsisRetrieverNestedExportStreaming extends XmlFieldRetrieverStreaming {
        public static final String CALC_ID = "ONSIS-EXPORT-STREAM";

        /**
         * Render field.
         *
         * @param xmlHolder XMLStreamWriterHolder
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @param parentElement Element
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.XmlFieldRetriever#renderField(com.x2dev.procedures.statereporting.on.OnsisStateReportData,
         *      com.x2dev.procedures.statereporting.on.OnsisStateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public void renderField(XMLStreamWriterHolder xmlHolder,
                                OnsisStateReportData reportData,
                                OnsisStateReportEntity entity,
                                FieldDefinition field,
                                Element parentElement)
                throws X2BaseException {

            try {
                OnsisStateReportData childReportData =
                        reportData.getGlobalData().getReportDataCache().getChildReportData(reportData, entity, field);
                // if (getDebugDetail()) {
                // childReportData.writeBeans(onsisWriter.xmlWriter, field.getSifPath());
                // }
                Map<String, String> params = OnsisExtractHelper.jsonToMap((String) field.getParameter());
                RenderGranularity granularity = RenderGranularity.FIELD;
                if (params.containsKey(RenderGranularity.class.getSimpleName())) {
                    String granularityStr = params.get(RenderGranularity.class.getSimpleName());
                    if (RenderGranularity.ENTITY.name().equals(granularityStr)) {
                        granularity = RenderGranularity.ENTITY;
                    }
                }

                childReportData.renderReportDataToStream(xmlHolder, parentElement, granularity);
            } catch (Exception ex) {
                String msg = entity.getDebugInfo();
                RuntimeException debugEx = new RuntimeException(msg, ex);

                reportData.storeException(field, entity, debugEx);
            }
        }
    }

    /**
     * The Class OnsisRetrieverFromCsv.
     */
    public class OnsisRetrieverRedirect implements FieldRetriever {
        public static final String CALC_ID = "REDIRECT";

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
            String fieldNameToRedirect = (String) field.getParameter();
            if (StringUtils.isEmpty(fieldNameToRedirect)) {
                throw new RuntimeException(
                        "Please, provide field name to redirect as a calculation parameter for field "
                                + field.getFieldId());
            }
            return entity.getFieldValue(fieldNameToRedirect);
        }
    }

    /**
     * The Class OnsisXmlBatchFile.
     */
    public class OnsisXmlBatchFile extends OnsisXmlElement {
        public static final String ENCODING_UTF_8 = "UTF-8";
        public static final String ENCODING_WINDOWS_1252 = "windows-1252";

        public static final String ONSIS_ELEM_BATCH_FILE = "ONSIS_BATCH_FILE";
        public static final String ONSIS_ELEM_BATCH_FILE_ID = "BATCH_FILE_ID";
        public static final String ONSIS_ELEM_BATCH_TYPE = "BATCH_TYPE";
        public static final String ONSIS_ELEM_DATA = "DATA";
        public static final String ONSIS_ELEM_DATE = "DATE";
        public static final String ONSIS_ELEM_FILE_ID = "FILE_ID";
        public static final String ONSIS_ELEM_HEADER = "HEADER";
        public static final String ONSIS_ELEM_OEN_BATCH_FILE = "OEN_BATCH_FILE";

        public static final String ONSIS_ELEM_SMS = "SMS";
        public static final String ONSIS_ELEM_TIME = "TIME";
        public static final String ONSIS_ELEM_VERSION = "VERSION";
        protected OnsisXmlElement m_data = null;
        protected OnsisXmlElement m_header = null;
        protected OnsisXmlElement m_schoolSubmission = null;

        /**
         * Instantiates a new onsis xml batch file.
         */
        public OnsisXmlBatchFile() {
            super(ONSIS_ELEM_BATCH_FILE);
        }

        /**
         * Gets the data.
         *
         * @return Onsis xml element
         */
        public OnsisXmlElement getData() {
            return m_data;
        }

        /**
         * Gets the header.
         *
         * @return Onsis xml element
         */
        public OnsisXmlElement getHeader() {
            return m_header;
        }

        /**
         * Gets the school submission.
         *
         * @return Onsis xml element
         */
        public OnsisXmlElement getSchoolSubmission() {
            return m_schoolSubmission;
        }
    }

    // /**
    // * Initialize this instance for use in publishing SIF objects from a bean or all.
    // *
    // * In theory the input combination of (primaryBean + publishParameter) should be enough to
    // * identify a single SIF object.
    // * This may not be implemented and unit tested across all topics however.
    // *
    // * @param primaryBean
    // * Primary bean defining scope of publishing. Null to publish all.
    // * @param publishParameter (optional)
    // * Entity limiter to limit to one entity row within the scope of
    // * primary bean e.g. one enrollment span for a Student.
    // *
    // * @throws X2BaseException exception
    // */
    // public void initializeDataSourceFromBeanOrAll(X2BaseBean primaryBean, String
    // publishParameter)
    // throws X2BaseException {
    // close();
    //
    // setMbean(primaryBean);
    // m_publishParameter = publishParameter;
    // }

    /**
     * The Class OnsisXmlElement.
     */
    public class OnsisXmlElement {
        protected Map<String, String> m_attributes;
        protected String m_tagName;
        protected String m_text;

        /**
         * Instantiates a new onsis xml element.
         *
         * @param tagName String
         */
        public OnsisXmlElement(String tagName) {
            m_tagName = tagName;
        }

        /**
         * Instantiates a new onsis xml element.
         *
         * @param tagName String
         * @param text String
         */
        public OnsisXmlElement(String tagName, String text) {
            m_tagName = tagName;
            m_text = text;
        }

        /**
         * Adds the attribute.
         *
         * @param name String
         * @param value String
         */
        public void addAttribute(String name, String value) {
            if (m_attributes == null) {
                m_attributes = new LinkedHashMap<>();
            }

            m_attributes.put(name, value);
        }

        /**
         * Render.
         *
         * @param writer XMLStreamWriter
         * @throws XMLStreamException exception
         */
        public void render(XMLStreamWriter writer) throws XMLStreamException {
            writer.writeStartElement(m_tagName);

            if (m_text != null) {
                writer.writeCharacters(m_text);
            }

            writer.writeEndElement();
        }
    }

    /**
     * The Class ReportDataCache.
     */
    public class ReportDataCache {
        public static final String KEY_TOPIC = "topic";
        public static final String KEY_PROCEDURE_ID = "procedureId";


        private Map<String, OnsisStateReportData> m_childReportDataCache = null;

        /**
         * Gets the child report data.
         *
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @return Onsis state report data
         * @throws JarPluginNotFoundException exception
         * @throws X2BaseException exception
         */
        public OnsisStateReportData getChildReportData(OnsisStateReportData reportData,
                                                       OnsisStateReportEntity entity,
                                                       FieldDefinition field)
                throws JarPluginNotFoundException, X2BaseException {
            Map<String, String> params = OnsisExtractHelper.jsonToMap((String) field.getParameter());
            String formatTopic = params.get(KEY_TOPIC);
            String customProcedureId = null;
            if (params.containsKey(KEY_PROCEDURE_ID)) {
                customProcedureId = params.get(KEY_PROCEDURE_ID);
            }

            OnsisStateReportData childReportData = getCachedReportData(formatTopic, customProcedureId, reportData);
            if (childReportData != null) {
                childReportData.initializeDataSourceFromParent(reportData, entity);

                if (childReportData.hasActionRetriever()) {
                    String extractType = childReportData.getExtractType(childReportData.getProcedureId());
                    OnsisExtractRecords matcher =
                            childReportData.getGlobalData().getExtractHelper().getMatcherByExtractType(extractType);
                    if (matcher == null) {
                        // there is no related csv extract in imported zip file
                        throw new NoCsvException(
                                "There is no CSV file for \"" + extractType + "\", the element will be skipped.");
                    }
                }
                childReportData.buildBeans();
                return childReportData;
            }
            return null;
        }

        /**
         * Gets the cached report data.
         *
         * @param onsisTopic String
         * @param customProcedureId String
         * @param parentReportData OnsisStateReportData
         * @return Onsis state report data
         * @throws JarPluginNotFoundException exception
         * @throws ToolRunException exception
         */
        private OnsisStateReportData getCachedReportData(String onsisTopic,
                                                         String customProcedureId,
                                                         OnsisStateReportData parentReportData)
                throws JarPluginNotFoundException, ToolRunException {
            OnsisStateReportData reportData = getChildReportDataCache().get(onsisTopic);

            if (reportData == null) {
                Collection<StateReportValidationError> setupErrors = new ArrayList<>();

                ExportFormatDefinition formatDef = getExportFormatForTopic(onsisTopic, m_broker);
                if (formatDef == null) {
                    NoExportException e = new NoExportException(
                            "Unable to locate export format for topic [" + onsisTopic + "]");
                    AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
                    throw e;
                }
                reportData = (OnsisStateReportData) loadStateReportData(formatDef, customProcedureId, setupErrors,
                        m_broker, getGlobalData().getUserData());

                Tool tool = ToolManager.getToolForId(Tool.TYPE_PROCEDURE, formatDef.getProcedureId(), getBroker());
                String currentComment = tool.getComment();
                String rootComment = getGlobalData().getRootProcedureComment();

                if (!rootComment.equals(currentComment)) {
                    throw new RuntimeException("Root commit number " + rootComment + ", wrong commit number "
                            + currentComment + " found for procedurre " + reportData.getProcedureId());
                }
                if (reportData != null) {
                    if (setupErrors.size() > 0) {
                        throw new RuntimeException(
                                "Export format validation errors: "
                                        + getOnsisHelper().formatValidationErrors(setupErrors));
                    }

                    // TODO: Add additional global sets here
                    reportData.setBroker(getBroker());
                    reportData.setCurrentContext(getCurrentContext());
                    reportData.setOrganization(getOrganization());
                    reportData.setGlobalData(getGlobalData());
                    Map<String, Object> parameters = new HashMap<String, Object>();
                    reportData.setParameters(parameters);

                    try {
                        reportData.initializeExport();
                    } catch (X2BaseException ex) {
                        throw new RuntimeException(ex);
                    } finally {
                        reportData.close();
                    }

                    if (getBroker() instanceof ModelBroker) {
                        PrivilegeSet privilegeSet = ((ModelBroker) getBroker()).getPrivilegeSet();
                        reportData.setPrivilegeSet(privilegeSet);
                    }

                    getChildReportDataCache().put(onsisTopic, reportData);
                }
            }

            return reportData;
        }

        /**
         * Gets the child report data cache.
         *
         * @return the m_childReportDataCache
         */
        private Map<String, OnsisStateReportData> getChildReportDataCache() {
            if (m_childReportDataCache == null) {
                m_childReportDataCache = new HashMap<>();
            }

            return m_childReportDataCache;
        }


    }

    /**
     * Render a foreign export definition as a child of this field.
     *
     * Example from export-format-ONSIS_STD.xml:
     *
     * <calculationId>ONSIS-CHILD-EXPORT</calculationId>
     * <calc-param>StudentSchoolEnrolment</calc-param>
     * <sifPath></sifPath>
     */
    public abstract class XmlFieldRetriever
            implements com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever {

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
            throw new RuntimeException("getFieldValue not supported in XmlFieldRetriever. Use renderField instead");
        }

        /**
         * Render field.
         *
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @param parentElement Element
         * @throws X2BaseException exception
         */
        public abstract void renderField(OnsisStateReportData reportData,
                                         OnsisStateReportEntity entity,
                                         FieldDefinition field,
                                         Element parentElement)
                throws X2BaseException;
    }

    /**
     * The Class XmlFieldRetrieverStreaming.
     */
    public abstract class XmlFieldRetrieverStreaming extends XmlFieldRetriever {

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
            throw new RuntimeException("getFieldValue not supported in XmlFieldRetriever. Use renderField instead");
        }

        /**
         * Render field.
         *
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @param parentElement Element
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.XmlFieldRetriever#renderField(com.x2dev.procedures.statereporting.on.OnsisStateReportData,
         *      com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, org.w3c.dom.Element)
         */
        @Override
        public void renderField(OnsisStateReportData reportData,
                                OnsisStateReportEntity entity,
                                FieldDefinition field,
                                Element parentElement)
                throws X2BaseException {
            throw new RuntimeException("Non-streaming renderField not supported in XmlFieldRetrieverStreaming.");
        }

        /**
         * Render field.
         *
         * @param xmlHolder XMLStreamWriterHolder
         * @param reportData OnsisStateReportData
         * @param entity OnsisStateReportEntity
         * @param field FieldDefinition
         * @param parentElement Element
         * @throws X2BaseException exception
         */
        public abstract void renderField(XMLStreamWriterHolder xmlHolder,
                                         OnsisStateReportData reportData,
                                         OnsisStateReportEntity entity,
                                         FieldDefinition field,
                                         Element parentElement)
                throws X2BaseException;
    }

    protected Predicate<String> m_matchOnEmptyKeyValueFn = null;
    protected Element m_parentElement = null; // parent XML Element of a child export

    private Collection<? extends ToolBean> m_beanCollection;
    private String m_elementName;
    private List<ExportFormatField> m_currentFields;
    private Map<String, ExportFormatField> m_currentFieldsById;
    private Map<String, List<List<String>>> m_existingKeys = new HashMap<>();
    private List<FieldDefinition> m_filteredFieldDefinitions = null;
    private ExportFormatDefinition m_formatDefinition;
    private GlobalData m_globalData;
    private Boolean m_hasActionTag = null;
    private Boolean m_hasActionRetriever = null;
    private OnsisHelper m_helper = null;
    private List<String> m_logMessages = null;
    private OnsisStateReportEntity m_parentEntity;
    private OnsisStateReportData m_parentReportData;

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     */
    public void buildBeans() throws X2BaseException {
        // Is Root?
        if (getParentReportData() == null) {
            setBeans(Arrays.asList(getGlobalData().getOrganizationToolBean()));
        } else {
            setBeans(Arrays.asList(getParentEntity().getBean()));
        }
    }

    /**
     * Clear messages.
     */
    public void clearLogMessages() {
        m_logMessages.clear();
    }

    /**
     * Creates the default element.
     *
     * @param document Document
     * @return Element
     */
    public Element createDefaultElement(org.w3c.dom.Document document) {
        return createTextElement(getElementName(getProcedureId()), null, document);
    }

    /**
     * Deep does field exist.
     *
     * @param fieldName String
     * @return true, if successful
     */
    public boolean deepDoesFieldExist(String fieldName) {
        ExportFormatField field =
                getGlobalData().getExtractHelper().getExportFormatFieldById(getFormatDefinition(), fieldName);
        if (field != null) {
            return true;
        }
        if (getParentReportData() != null) {
            return getParentReportData().deepDoesFieldExist(fieldName);
        }
        return false;
    }

    /**
     * Generate keys.
     *
     * @param entity OnsisStateReportEntity
     * @param matcher OnsisExtractRecordsMatcher
     * @return List
     */
    public Collection<String> generateKeys(OnsisStateReportEntity entity,
                                           OnsisExtractRecords matcher) {
        Collection<String> fullKeys = new LinkedHashSet<String>();

        for (List<String> keyFields : getAllKeyFieldsForTopic(matcher)) {
            StringBuilder fullKey = new StringBuilder();

            // Look for each EXSMS keyField in XML EFD
            for (String keyField : keyFields) {
                String keyValue = null;

                // String multiValuedDelimiter
                // =matcher.getMultiValuedDelimiterByFieldName(keyField);
                // if (multiValuedDelimiter !=null) {
                //
                // }

                if (deepDoesFieldExist(keyField)) {
                    keyValue = entity.deepGetFieldValueByFieldName(keyField);
                }

                // if (!deepDoesFieldExist(keyField)) {
                // fullKey = null;
                // break;
                // }

                if (StringUtils.isBlank(keyValue) && !getMatchOnEmptyKeyValue(keyField)) {
                    fullKey = null;
                    break;
                }

                keyValue = StringUtils.emptyIfNull(keyValue);
                fullKey.append(makeKey(keyField, keyValue));
            }

            if (fullKey != null) {
                fullKeys.add(fullKey.toString().toUpperCase());
            }
        }
        return fullKeys;
    }

    /**
     * Gets the all key fields for topic.
     *
     * @param matcher OnsisExtractRecords
     * @return List
     */
    public List<List<String>> getAllKeyFieldsForTopic(OnsisExtractRecords matcher) {
        return matcher.getAllKeyFields();
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#getBroker()
     */
    @Override
    public X2Broker getBroker() {
        return getGlobalData().getBroker();
    }

    /**
     * Returns a map of retrievers for this object's calculated fields.
     *
     * @return Map<String, FieldRetriever>
     */
    public Map<String, FieldRetriever> getCalcs() {
        HashMap<String, FieldRetriever> calcs = new HashMap<>(10);

        calcs.put(OnsisRetrieverEntityProperty.CALC_ID, new OnsisRetrieverEntityProperty());
        calcs.put(OnsisRetrieverNestedExport.CALC_ID, new OnsisRetrieverNestedExport());
        calcs.put(OnsisRetrieverNestedExportStreaming.CALC_ID, new OnsisRetrieverNestedExportStreaming());
        calcs.put(OnsisRetrieverRedirect.CALC_ID, new OnsisRetrieverRedirect());
        calcs.put(OnsisRetrieverClearPending.CALC_ID, new OnsisRetrieverClearPending());

        return calcs;
    }

    /**
     * Gets the current context.
     *
     * @return District school year context
     */
    @Override
    public DistrictSchoolYearContext getCurrentContext() {
        return getGlobalData().getCurrentContext();
    }

    /**
     * Gets the debug student oid.
     *
     * @return String
     */
    public String getDebugStudentOid() {
        Object debugStudentOid = getParameter(INPUT_PARAM_DEBUG_STUDENT_OID);
        if (debugStudentOid != null && debugStudentOid instanceof String
                && !StringUtils.isEmpty((String) debugStudentOid)) {
            return (String) debugStudentOid;
        }
        return null;
    }

    /**
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#getDictionaryExtractor()
     */
    @Override
    public DictionaryExtractor getDictionaryExtractor() {
        return getGlobalData().getDictionaryExtractor();
    }

    /**
     * Gets the element name.
     *
     * @param currentProcId String
     * @return String
     */
    final public String getElementName(String currentProcId) {
        if (m_elementName == null) {
            OnsisStateReportData parentData = getParentReportData();
            if (parentData == null) {
                m_elementName = getSifTopic(getProcedureId());
            } else {
                String parentProcId = parentData.getProcedureId();
                List<ExportFormatField> parentFields =
                        getGlobalData().getFormatFields(getFormat(parentProcId).getOid());
                for (ExportFormatField parentField : parentFields) {
                    String parentCalcParam = parentField.getCalcParam();
                    if (OnsisRetrieverNestedExport.CALC_ID.equals(parentField.getCalculationId())
                            || OnsisRetrieverNestedExportStreaming.CALC_ID.equals(parentField.getCalculationId())) {
                        parentCalcParam = OnsisExtractHelper.extractTopic(parentField);
                    }
                    if (getSifTopic(currentProcId).equals(parentCalcParam)) {
                        m_elementName = parentField.getSifPath();
                    }
                }
            }
        }
        if (m_elementName == null) {
            m_elementName = getSifTopic(getProcedureId());
        }
        if (m_elementName == null) {
            throw new RuntimeException();
        }
        return m_elementName;
    }

    /**
     * Gets the existing keys.
     *
     * @param procedureId String
     * @return List
     */
    public List<List<String>> getExistingKeys(String procedureId) {
        List<List<String>> existingKeys = m_existingKeys.get(procedureId);
        if (existingKeys != null) {
            return existingKeys;
        }

        if (existingKeys == null && procedureId.equals(getProcedureId())) {
            existingKeys = new ArrayList<>();
            List<List<String>> keyFieldsLists = getAllKeyFieldsForTopic(getMatcher());
            for (List<String> keyFields : keyFieldsLists) {
                List<String> keys = new ArrayList<>();
                for (String keyField : keyFields) {
                    if (deepDoesFieldExist(keyField)) {
                        keys.add(keyField);
                    }
                }
                if (!existingKeys.contains(keys)) {
                    existingKeys.add(keys);
                }
            }
            m_existingKeys.put(procedureId, existingKeys);
            return getExistingKeys(procedureId);
        }
        return existingKeys;
    }

    /**
     * Return the expected H1 value for this topic's CSV extract file
     * e.g. the H1 header row in the class list CSV extract file looks like
     * "H1","CLASS LIST","1","YYYY-YYYY","submission period type","yyyy/mm/dd","hh:mm"
     * so return "CLASS LIST".
     *
     * This is used as part of the name stored in PublishOptions.
     *
     * @param procedureId String
     * @return String
     */
    public final String getExtractType(String procedureId) {
        ExportFormatDefinition efd = getFormat(procedureId);
        String profile = efd == null ? null : efd.getSifProfile();
        if (profile != null) {
            return profile.toUpperCase().trim();
        } else if (hasActionRetriever()) {
            throw new RuntimeException("Format with " + OnsisRetrieverAction.CALC_ID
                    + " field must have sif profile filled with extract type");
        }
        return null;
    }

    /**
     * Gets the field value from ancestor.
     *
     * @param fieldName String
     * @return String
     */
    public String getFieldValueByFieldNameFromAncestor(String fieldName) {
        OnsisStateReportEntity parent = getParentEntity();
        if (parent != null) {
            return parent.deepGetFieldValueByFieldName(fieldName);
        }
        return null;
    }

    /**
     * Gets the format definition.
     *
     * @return Export format definition
     */
    @Override
    public ExportFormatDefinition getFormatDefinition() {
        if (m_formatDefinition == null) {
            return super.getFormatDefinition();
        }
        return m_formatDefinition;
    }

    /**
     * Gets the global data.
     *
     * @return Global data
     */
    public GlobalData getGlobalData() {
        if (m_globalData == null) {
            m_globalData = new GlobalData();
        }
        return m_globalData;
    }

    /**
     * Gets the log messages.
     *
     * @return List
     */
    public List<String> getLogMessages() {
        return m_logMessages;
    }

    /**
     * Gets the matcher.
     *
     * @return Onsis extract records matcher
     */
    public OnsisExtractRecords getMatcher() {
        String extractType = getExtractType(getProcedureId());
        return getGlobalData().getExtractHelper().getMatcherByExtractType(extractType);
    }

    /**
     * Should a blank exported value be used when building a fullKey to compare with CSV.
     *
     * @param keyField String
     * @return boolean
     */
    public boolean getMatchOnEmptyKeyValue(String keyField) {
        return true;
    }

    /**
     * Gets the match on empty key value fn.
     *
     * @return Predicate
     */
    public Predicate<String> getMatchOnEmptyKeyValueFn() {
        if (m_matchOnEmptyKeyValueFn == null) {
            m_matchOnEmptyKeyValueFn =
                    new Predicate<String>() {
                        @Override
                        public boolean test(String keyField) {
                            return getMatchOnEmptyKeyValue(keyField);
                        }
                    };
        }

        return m_matchOnEmptyKeyValueFn;
    }

    /**
     * Returns the Onsis helper instance.
     *
     * @return OnsisHelper
     */
    public OnsisHelper getOnsisHelper() {
        if (m_helper == null) {
            m_helper = new OnsisHelper();
        }

        return m_helper;
    }

    /**
     * Gets the organization.
     *
     * @return Organization
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#getOrganization()
     */
    @Override
    public Organization getOrganization() {
        return getGlobalData().getOrganization();
    }

    /**
     * Gets the parameter.
     *
     * @param key String
     * @return Object
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#getParameter(java.lang.String)
     */
    @Override
    public Object getParameter(String key) {
        return getGlobalData().getParameter(key);
    }

    /**
     * Gets the parent element.
     *
     * @return Element
     */
    public Element getParentElement() {
        return m_parentElement;
    }

    /**
     * Gets the parent entity.
     *
     * @return the m_parentEntity
     */
    public OnsisStateReportEntity getParentEntity() {
        return m_parentEntity;
    }

    /**
     * Gets the parent report data.
     *
     * @return the m_parentReportData
     */
    public OnsisStateReportData getParentReportData() {
        return m_parentReportData;
    }

    /**
     * Gets the result for property path.
     *
     * @param sourceObject Object
     * @param beanPath String
     * @return Object
     * @throws X2BaseException exception
     * @throws NoSuchMethodException exception
     * @throws SecurityException exception
     * @throws IllegalAccessException exception
     * @throws IllegalArgumentException exception
     * @throws InvocationTargetException exception
     */
    public Object getResultForPropertyPath(Object sourceObject,
                                           String beanPath)
            throws X2BaseException, NoSuchMethodException, SecurityException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException {
        /*
         * Start searching for "@$#" after the end of bean path
         * because it's possible that a regex in the bean path has those characters.
         *
         * For example, we DON'T want to find the "$" in the regex below:
         * getter;REG(/$/$/)REG
         */
        int indexOfLastSemi = beanPath.lastIndexOf(OnsisHelper.PATH_NODE_SEP);
        int indexOfLastRegex = beanPath.lastIndexOf(OnsisHelper.PATH_REGEX_END);
        int searchStart = Math.max(0, indexOfLastSemi);
        searchStart = Math.max(searchStart, indexOfLastRegex);


        String ddxId = null;
        if (beanPath.indexOf(OnsisHelper.PATH_DDX_ID_SEPARATOR, searchStart) >= 0) {
            ddxId = beanPath.substring(beanPath.lastIndexOf(OnsisHelper.PATH_DDX_ID_SEPARATOR) + 1);
            beanPath = beanPath.substring(0, beanPath.lastIndexOf(OnsisHelper.PATH_DDX_ID_SEPARATOR));
        }

        String calculation = null;
        if (beanPath.indexOf(OnsisHelper.PATH_CALC_SEPARATOR, indexOfLastSemi) >= 0) {
            calculation = beanPath.substring(beanPath.lastIndexOf(OnsisHelper.PATH_CALC_SEPARATOR) + 1);
            beanPath = beanPath.substring(0, beanPath.lastIndexOf(OnsisHelper.PATH_CALC_SEPARATOR));
        }

        /*
         * propertyPath (in calcParam) may optionally end with #STATE or #FEDERAL etc
         */
        ReferenceMapTypeCode refType = null;
        if (beanPath.indexOf(OnsisHelper.PATH_REF_SEP, indexOfLastSemi) >= 0) {
            String refTypeName = beanPath.substring(beanPath.lastIndexOf(OnsisHelper.PATH_REF_SEP) + 1);
            refType = ReferenceMapTypeCode.valueOf(refTypeName);
            beanPath = beanPath.substring(0, beanPath.lastIndexOf(OnsisHelper.PATH_REF_SEP));
        }

        Object[] resultAndLastObject = null;
        DataDictionary dictionary = getGlobalData().getDictionaryExtractor().getDictionary(ddxId);

        resultAndLastObject = getOnsisHelper().getPropertyPathFromObject(sourceObject, beanPath, dictionary);

        /*
         * Get the result for the propertyPath
         */

        Object result = resultAndLastObject[0];
        Object lastObject = resultAndLastObject[1];

        /*
         * Apply the ref code lookup
         */
        if (result != null && calculation != null) {
            // TODO: DO NOT USE calculator classes
            throw new IllegalStateException("DO NOT USE calculation " + calculation + " for " + result);
            // result = getCalculationValue(result, calculation);
        } else if (result != null && refType != null) {
            // Get last path node e.g. "[some_alias]"
            ArrayList<String> nodes =
                    StringUtils.convertDelimitedStringToList(beanPath, OnsisHelper.PATH_NODE_SEP);
            String lastPathNode = nodes.get(nodes.size() - 1);

            if (lastPathNode.startsWith("[")) {
                /*
                 * Strip the [] brackets and lookup the alias.
                 * If the alias lookup fails, return the non-translated value
                 */
                String alias = lastPathNode.substring(1, lastPathNode.length() - 1);
                boolean required = true;
                DataDictionaryField field =
                        getGlobalData().getDictionaryExtractor().getFieldByAlias(alias, ddxId, required);
                String referenceTableOid = field.getReferenceTableOid();
                if (StringUtils.isEmpty(referenceTableOid)) {
                    throw new RuntimeException("Reference table must exist for field by alias " + alias);
                }
                result = getGlobalData().getDictionaryExtractor().lookupReferenceCodeByRefTbl(
                        field.getReferenceTableOid(), (String) result,
                        refType.ordinal());
            } else {
                // beanPath lookup. Use class from the second-to-last node of the path
                // e.g. for original propertyPath of "getFirstSection.sectionCode:STATE"
                result = getGlobalData().getDictionaryExtractor().lookupReferenceCodeByBeanPath(lastObject.getClass(),
                        lastPathNode, (String) result,
                        refType.ordinal());
            }
        }

        return result;
    }

    /**
     * Gets the sif topic.
     *
     * @param procedureId String
     * @return String
     */
    public String getSifTopic(String procedureId) {
        return getFormat(procedureId).getSifTopic();
    }

    /**
     * Checks for action tag.
     *
     * @return true, if successful
     */
    public boolean hasActionTag() {
        if (m_hasActionTag == null) {
            m_hasActionTag = Boolean.FALSE;
            List<FieldDefinition> filteredFieldDefns = getFilteredFieldDefinitions();
            for (FieldDefinition fieldDefinition : filteredFieldDefns) {
                if (OnsisRetrieverAction.ELEMENT_NAME_ACTION.equals(fieldDefinition.getSifPath())) {
                    m_hasActionTag = Boolean.TRUE;
                    break;
                }
            }
        }
        return m_hasActionTag.booleanValue();
    }

    /**
     * Checks for action retriever.
     *
     * @return true, if successful
     */
    public boolean hasActionRetriever() {
        if (m_hasActionRetriever == null) {
            m_hasActionRetriever = Boolean.FALSE;
            if (getCalcs().get(OnsisRetrieverAction.CALC_ID) != null) {
                m_hasActionRetriever = Boolean.TRUE;
            }
        }
        return m_hasActionRetriever.booleanValue();
    }

    /**
     * One-time initialization for extending the export definition.
     * Called by StateReportData.initializeExport.
     *
     * @throws X2BaseException exception
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        // topic-specific method for loading one-time member data e.g. ref table codes
        initializeFields();

        initializeEntityClass();

        // Add any retrievers or validators.
        addCalcs(getCalcs());
    }

    /**
     * This export is being called by a parent field from another export format.
     *
     * @param parentData OnsisStateReportData
     * @param parentEntity OnsisStateReportEntity
     * @throws X2BaseException exception
     */
    public void initializeDataSourceFromParent(OnsisStateReportData parentData,
                                               OnsisStateReportEntity parentEntity)
            throws X2BaseException {
        close();

        m_parentReportData = parentData;
        m_parentEntity = parentEntity;
        setOrganization(parentData.getOrganization());
    }

    /**
     * Log severe.
     *
     * @param message String
     */
    public void log(String message) {
        if (getGlobalData().getDebugDetail()) {
            if (m_logMessages == null) {
                m_logMessages = new ArrayList<>();
            }
            m_logMessages.add(message);
        }
    }

    /**
     * Open the query iterator based on the query provided
     * by the implementing class.<br>
     * <code>close()</code> must be called after an open.
     * Otherwise, resources will be left open.
     *
     * @return boolean - true if the open was successful, false otherwise.
     */
    @Override
    public boolean open() {
        m_iterator = m_beanCollection == null ? null : m_beanCollection.iterator();

        return (m_iterator != null);
    }

    /**
     * Sets the beans.
     *
     * @param beans void
     */
    public void setBeans(Collection<? extends ToolBean> beans) {
        m_beanCollection = (beans == null)
                ? null
                : new ArrayList(beans);
    }

    /**
     * Sets the global data.
     *
     * @param globalData void
     */
    public void setGlobalData(GlobalData globalData) {
        m_globalData = globalData;
    }

    /**
     * Store exception.
     *
     * @param fieldDefinition FieldDefinition
     * @param exception RuntimeException
     */
    public void storeException(FieldDefinition fieldDefinition, RuntimeException exception) {
        String fieldPath = getElementPathByFieldOid(fieldDefinition.getEffOid());
        getGlobalData().storeException(fieldPath, exception);
    }

    /**
     * Store exception.
     *
     * @param fieldDefinition FieldDefinition
     * @param entity OnsisStateReportEntity
     * @param exception RuntimeException
     */
    public void storeException(FieldDefinition fieldDefinition,
                               OnsisStateReportEntity entity,
                               RuntimeException exception) {
        String entityNameStr = entity == null ? "Null Entity" : entity.getEntityName();
        String fieldPath = getElementPathByFieldOid(fieldDefinition.getEffOid());

        getGlobalData().storeException(entityNameStr + "-" + fieldPath, exception);
    }

    /**
     * Adds the if not blank.
     *
     * @param record OnsisCsvDataRecord
     * @param csvField CsvField
     * @param elementName String
     * @param parentElement Element
     */
    protected void addIfNotBlank(OnsisCsvDataRecord record,
                                 OnsisExtractHelper.CsvField csvField,
                                 String elementName,
                                 Element parentElement) {
        String csvValue = record.getSingleFieldValue(csvField);
        addIfNotBlank(elementName, csvValue, parentElement);
    }

    /**
     * Adds the if not blank.
     *
     * @param name String
     * @param value String
     * @param parentElement Element
     */
    protected void addIfNotBlank(String name, String value, Element parentElement) {
        if (StringUtils.isBlank(value)) {
            return;
        }

        /*
         * Build path to element
         */
        String fullPath =
                OnsisResultsHelper.getFullPathOfNode(parentElement, OnsisSchoolSubmission.ONSIS_TOPIC)
                        + ELEMENTS_PATH_DELIMITER + name;
        if (!getGlobalData().getFieldsRepository().doesFieldExist(fullPath)) {
            return;
        }

        appendTextElement(name, value, parentElement);
    }

    /**
     * Dump messages.
     *
     * @param parentElement Element
     */
    protected void dumpMessages(Element parentElement) {
        if (getLogMessages() == null) {
            return;
        }

        for (String message : getLogMessages()) {
            appendTextElement("DEBUG", message, parentElement);
        }
        clearLogMessages();
    }

    /**
     * Dump messages.
     *
     * @param xmlHolder XMLStreamWriterHolder
     * @throws XMLStreamException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    protected void dumpMessages(XMLStreamWriterHolder xmlHolder) throws XMLStreamException, IOException {
        if (getLogMessages() == null) {
            return;
        }

        if (xmlHolder != null) {
            for (String message : getLogMessages()) {
                xmlHolder.writeRawNextLine("<DEBUG>" + message + "</DEBUG>");
            }
        }
        clearLogMessages();
    }

    /**
     * For a given partial entityKeySet and corresponding entityValueKeySet,.
     *
     * @param record OnsisCsvDataRecord
     * @param currentEntityKeySet List<String>
     * @param currentEntityValueSet List<String>
     * @param parentElement Element
     * @return Element
     */
    protected Element generateAndAppendDelete(OnsisCsvDataRecord record,
                                              List<String> currentEntityKeySet,
                                              List<String> currentEntityValueSet,
                                              Element parentElement) {
        if (currentEntityKeySet.size() != currentEntityValueSet.size()) {
            throw new RuntimeException("keySet and valueSet sizes differ");
        }

        Element thisElement = createDefaultElement(parentElement.getOwnerDocument());
        parentElement.appendChild(thisElement);
        appendTextElement(OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_DELETE, thisElement);

        for (int i = 0; i < currentEntityKeySet.size(); i++) {
            String currentEntityKey = currentEntityKeySet.get(i);

            String tagToWrite = null;
            for (ExportFormatField field : getCurrentFields()) {
                if (currentEntityKey.equals(field.getId())) {
                    tagToWrite = field.getSifPath();
                    break;
                }
            }

            if (tagToWrite == null || MARKER_NO_FIELD.equals(tagToWrite)) {
                continue;
            }

            String currentEntityValue = currentEntityValueSet.get(i);
            appendTextElement(tagToWrite, currentEntityValue, thisElement);
        }

        return thisElement;
    }

    /**
     * Generate the DELETEs from one CsvRecord.
     *
     * @param fullKeyToDelete String
     * @param record OnsisCsvDataRecord
     * @param parentElement Element
     * @param currentEntityKeySets List<List<String>>
     */
    protected void generateAndAppendDeletes_AllKeySets(String fullKeyToDelete,
                                                       OnsisCsvDataRecord record,
                                                       Element parentElement,
                                                       List<List<String>> currentEntityKeySets) {

        /*
         * A CSV DT can have multiple EXSMS EFD defined for it,
         * each with a different set of keyIndicator fields
         * e.g. ["OEN", "SAL Comp Type"], ["OEN", "SAL xxx"]
         *
         * currentEntityKeySets is the list of keySets for the DT record.
         *
         * Strategy:
         *
         * For each currentEntityKeySet,
         * - Generate a list of unique valueSets corresponding to those keys.
         * - Generate a Delete for each valueSet
         */
        if (currentEntityKeySets.isEmpty()) {
            generateAndAppendDelete(
                    record,
                    Collections.EMPTY_LIST,
                    Collections.EMPTY_LIST,
                    parentElement);
            return;
        }

        for (Iterator<List<String>> currentEntityKeySetsIter =
                currentEntityKeySets.iterator(); currentEntityKeySetsIter.hasNext();) {
            List<String> currentEntityKeySet = currentEntityKeySetsIter.next();

            List<List<String>> currentEntityValueSetsForKeySet =
                    record.getCurrentEntityValueSetsForKeySet_thatMatchFullKey(currentEntityKeySet, fullKeyToDelete);

            for (List<String> currentEntityValueSet : currentEntityValueSetsForKeySet) {
                generateAndAppendDelete(
                        record,
                        currentEntityKeySet,
                        currentEntityValueSet,
                        parentElement);
            }
        }
    }

    /**
     * Generate Deletes for any remaining extract file keys.
     *
     * @param parentElement Element
     */
    protected void generateDeletes(Element parentElement) {
        /*
         * Presence of an ACTION retriever means we need to generate DELETEs
         * Note some topic have an Action tag but don't use the Action retriever.
         * They would have to override renderReportDataPost
         * and generate their own deletes.
         */
        if (!hasActionRetriever()) {
            return;
        }
        log("generateDeletes: " + getProcedureId());

        /*
         * Generate DELETEs performs a diff of CSV keys VS exported keys
         * The comparison scope is defined by the parent's key(s)
         *
         * Construct parentKeys = names of allKeys that aren't in m_currentFields
         * Construct parentFullKeys = parent fullKey(s)
         */
        String extractType = getExtractType(getProcedureId());
        OnsisExtractRecords matcher = getGlobalData().getExtractHelper().getMatcherByExtractType(extractType);

        Set<OnsisCsvDataRecord> recordsOfParent = new LinkedHashSet();
        for (List<String> parentKey : matcher.getParentKeys(this)) {
            Map<String, List<OnsisCsvDataRecord>> recordsMap =
                    matcher.getRecordsMap(parentKey, getMatchOnEmptyKeyValueFn());
            String parentFullKey = matcher.getParentFullKey(this, parentKey);
            List<OnsisCsvDataRecord> parentRecords = recordsMap.get(parentFullKey);
            if (parentRecords != null) {
                recordsOfParent.addAll(recordsMap.get(parentFullKey));
            }
        }
        log("generateDeletes[recordsOfParent]: " + recordsOfParent);

        /*
         * Get the exported fullKeys of this topic
         */
        final String procedureId = getProcedureId();
        Set<String> generatedKeys = getGlobalData().getGeneratedRowsKeys(procedureId);
        log("generateDeletes[generatedKeys]: " + generatedKeys);

        /*
         * Perform the diff:
         *
         * Compare CSV fullKeys with exported fullKeys (for CSV's starting with parentFullKey)
         *
         */
        List<List<String>> existingKeyNamesList = null;
        Map<String, OnsisCsvDataRecord> notGeneratedSoDelete = new HashMap<>();
        for (OnsisCsvDataRecord recordOfParent : recordsOfParent) {
            if (existingKeyNamesList == null) {
                existingKeyNamesList = getExistingKeys(getProcedureId());
            }

            // Generates fullKey for each EXSMS format
            log("generateDeletes[existingKeyNamesList]: " + existingKeyNamesList);
            List<String> fullKeys = recordOfParent.getFullKeysList(existingKeyNamesList, getMatchOnEmptyKeyValueFn());
            log("generateDeletes[fullKeys]: " + fullKeys);
            for (String fullKey : fullKeys) {
                if (!generatedKeys.contains(fullKey)) {
                    notGeneratedSoDelete.put(fullKey, recordOfParent);
                }
            }
        }
        if (notGeneratedSoDelete.isEmpty()) {
            return;
        }

        /*
         * In order to generate a DELETE, we need any key(s) for that entity
         * that are in addition to it's parent's key(s)
         *
         * Get keysets for the current entity format
         * (excluding parent key names).
         * Entity keyset defines the key alongside the <ACTION>
         */
        List<List<String>> allKeys = getAllKeyFieldsForTopic(matcher);
        List<List<String>> currentEntityKeySets = new ArrayList<>();
        for (List<String> keySet : allKeys) {
            List<String> currentEntityKeySet = new ArrayList<>();
            for (String key : keySet) {
                for (ExportFormatField field : m_currentFields) {
                    if (field.getId().equals(key)) {
                        currentEntityKeySet.add(key);
                    }
                }
            }
            if (!currentEntityKeySet.isEmpty()
                    && !currentEntityKeySets.contains(currentEntityKeySet)) {
                currentEntityKeySets.add(currentEntityKeySet);
            }
        }

        /*
         * Generate the DELETEs:
         *
         * for each notGenerated fullKey:
         * 1. Generate a <tag> for the entity e.g. <REPORT_CARD>
         * 2. <ACTION>DELETE</ACTION>
         * 3. <tag>keyValue</tag> for each keyName in entity keyset(s)
         *
         */
        for (String fullKeyToDelete : notGeneratedSoDelete.keySet()) {
            OnsisCsvDataRecord record = notGeneratedSoDelete.get(fullKeyToDelete);
            generateAndAppendDeletes_AllKeySets(fullKeyToDelete, record, parentElement, currentEntityKeySets);
        }
    }

    /**
     * List of format definitions fields.
     *
     * @return List<ExportFormatField>
     */
    protected List<ExportFormatField> getCurrentFields() {
        return m_currentFields;
    }

    /**
     * Gets the current field by id.
     *
     * @param id the id
     * @return the current field by id
     */
    protected ExportFormatField getCurrentFieldById(String id) {
        if (m_currentFieldsById == null) {
            m_currentFieldsById = new HashMap();
            for (ExportFormatField field : getCurrentFields()) {
                m_currentFieldsById.put(field.getId(), field);
            }
        }
        return m_currentFieldsById.get(id);
    }

    /**
     * Gets the filtered field definitions.
     *
     * @return List
     */
    protected List<FieldDefinition> getFilteredFieldDefinitions() {
        if (m_filteredFieldDefinitions != null) {
            return m_filteredFieldDefinitions;
        }

        if ("ONSIS-OENSTD".equals(getProcedureId()) || "ONSI2-OENSTD".equals(getProcedureId())) {
            m_filteredFieldDefinitions = getFieldDefinitions();
        } else {
            Map<String, FieldDefinition> definitionsByPath = new HashMap<String, FieldDefinition>();
            for (FieldDefinition fd : getFieldDefinitions()) {
                String fieldPath = getElementPathByFieldOid(fd.getEffOid());
                definitionsByPath.put(fieldPath, fd);
            }

            List<FieldDefinition> fieldDefinitions = new ArrayList<>();

            String procedureId = getProcedureId();
            String sifTopic = getSifTopic(procedureId);
            String formatPath = getFormatPath(sifTopic);
            int currentFormatNesting = getElementNesting(formatPath);
            List<String> formatRepositoryFields = new ArrayList<>();
            for (String repositoryField : getGlobalData().getFieldsRepository().getFields()) {
                boolean isOfCurrentFormat = repositoryField.startsWith(formatPath);
                int repositoryFieldNesting = getElementNesting(repositoryField);
                boolean isOneMoreLevelNesting = repositoryFieldNesting - currentFormatNesting == 1;
                if ((isOfCurrentFormat && isOneMoreLevelNesting)
                        || definitionsByPath.keySet().contains(repositoryField)) {
                    formatRepositoryFields.add(repositoryField);
                }
            }

            for (String formatRepositoryField : formatRepositoryFields) {
                FieldDefinition fieldDefinition = definitionsByPath.get(formatRepositoryField);
                if (fieldDefinition != null) {
                    fieldDefinitions.add(fieldDefinition);
                } else {
                    getGlobalData().addMissingTag(formatRepositoryField);
                }
            }
            m_filteredFieldDefinitions = fieldDefinitions;
        }

        return m_filteredFieldDefinitions;
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    protected void initializeEntityClass() {
        // TODO Auto-generated method stub

    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    protected void initializeFields() {
        if (getFormatDefinition() != null) {
            m_currentFields =
                    new ArrayList<>(getFormatDefinition().getFields());
            Collections.sort(m_currentFields, OnsisExtractHelperUtils.fieldsPositionComparator);
        }
    }

    /**
     * Render report data.
     *
     * @param parentElement Element
     */
    protected void renderReportData(Element parentElement) {
        OnsisStateReportEntity entity = null;
        this.m_parentElement = parentElement;

        try {
            open();
            while ((entity = (OnsisStateReportEntity) this.next()) != null) {
                ThreadUtils.checkInterrupt();

                StateReportValidationError err = entity.filterEntity();
                if (err == null) {
                    entity.preProcess();

                    try {
                        entity.renderEntityRow(parentElement);
                    } catch (Exception e) {
                        String msg = entity.getDebugInfo();
                        Exception debugEx = new RuntimeException(msg, e);
                        getGlobalData().storeException(entity.getEntityName(), debugEx);
                    }

                    entity.postProcess();
                }
            }
            renderReportDataPost(parentElement);

        } catch (Exception e) {
            String msg = "";
            OnSchool school = getGlobalData().getCurrentSchoolFirst();
            if (school != null) {
                msg += "School [" + school.getName() + "]";
            }
            throw new RuntimeException(msg, e);
        } finally {
            close();
        }
    }

    /**
     * Render report data post.
     *
     * @param parentElement Element
     * @see com.follett.fsc.aspensif.framework.XmlStateReportData#renderReportDataPost(javax.xml.stream.XMLStreamWriter)
     */
    protected void renderReportDataPost(Element parentElement) {
        generateDeletes(parentElement);

        if (hasActionTag()) {
            sortChildren(parentElement);
        }

        dumpMessages(parentElement);

        /*
         * Reset generated keys for next export of this type
         */
        Set<String> generatedKeys = getGlobalData().getGeneratedRowsKeys(getProcedureId());
        if (generatedKeys != null) {
            generatedKeys.clear();
        }
    }

    /**
     * Render report data post.
     *
     * @param xmlHolder XMLStreamWriterHolder
     * @param parentElement Element
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws XMLStreamException exception
     */
    protected void renderReportDataPost(XMLStreamWriterHolder xmlHolder, Element parentElement)
            throws IOException, XMLStreamException {
        removeChildElements(parentElement);

        generateDeletes(parentElement);

        /*
         * Serialize to XML Writer
         */
        if (xmlHolder != null) {
            List<Element> children = getChildElements("*", parentElement);
            for (Element childElement : children) {
                xmlHolder.writeElementNextLine(childElement);
            }

            /*
             * Empty parentElement
             */
            removeChildElements(parentElement);
        }

        dumpMessages(xmlHolder);

        /*
         * Reset generated keys for next export of this type
         */
        Set<String> generatedKeys = getGlobalData().getGeneratedRowsKeys(getProcedureId());
        if (generatedKeys != null) {
            generatedKeys.clear();
        }
    }

    /**
     * Default behavior renders to DOM then streams out.
     *
     * @param xmlHolder XMLStreamWriterHolder
     * @param parentElement Element
     * @param granularity RenderGranularity
     */
    protected void renderReportDataToStream(XMLStreamWriterHolder xmlHolder,
                                            Element parentElement,
                                            RenderGranularity granularity) {
        OnsisStateReportEntity entity = null;
        this.m_parentElement = parentElement;

        try {
            open();
            while ((entity = (OnsisStateReportEntity) this.next()) != null) {
                ThreadUtils.checkInterrupt();

                StateReportValidationError err = entity.filterEntity();
                if (err == null) {
                    entity.preProcess();

                    try {
                        if (RenderGranularity.FIELD.equals(granularity)) {
                            entity.renderEntityRow(xmlHolder, parentElement);
                        } else {
                            Element childElement = entity.renderEntityRow(parentElement);
                            if (childElement != null) {
                                xmlHolder.writeElementNextLine(childElement);
                                parentElement.removeChild(childElement);
                                childElement = null;
                            }
                        }
                    } catch (Exception e) {
                        String msg = entity.getDebugInfo();
                        Exception debugEx = new RuntimeException(msg, e);
                        getGlobalData().storeException(entity.getEntityName(), debugEx);
                    }

                    entity.postProcess();
                }
            }
            renderReportDataPost(xmlHolder, parentElement);
        } catch (Exception e) {
            String msg = "";
            OnSchool school = getGlobalData().getCurrentSchoolFirst();
            if (school != null) {
                msg += "School [" + school.getName() + "]";
            }
            throw new RuntimeException(msg, e);
        } finally {
            close();
        }
    }

    /**
     * Gets the bean by column value.
     *
     * @param <T> the generic type
     * @param beans Collection<T>
     * @param columnName String
     * @param value Object
     * @return t
     */
    private <T extends X2BaseBean> T getBeanByColumnValue(Collection<T> beans, String columnName, Object value) {
        for (T bean : beans) {
            if (value.equals(bean.getFieldValueByBeanPath(columnName))) {
                return bean;
            }
        }
        return null;
    }

    /**
     * Gets the element nesting.
     *
     * @param path String
     * @return int
     */
    private int getElementNesting(String path) {
        return path.split(ELEMENTS_PATH_DELIMITER).length;
    }

    /**
     * Gets the element path by field oid.
     *
     * @param effOid String
     * @return String
     */
    private String getElementPathByFieldOid(String effOid) {
        List<ExportFormatField> fields =
                getGlobalData().getFormatFields(getFormat(getProcedureId()).getOid());
        String fieldSifPath = getBeanByColumnValue(fields, X2BaseBean.COL_OID, effOid).getSifPath();
        String sifTopic = getSifTopic(getProcedureId());
        String parentPath = getFormatPath(sifTopic);
        return String.join(ELEMENTS_PATH_DELIMITER, Arrays.asList(parentPath, fieldSifPath));
    }

    /**
     * Gets the field value from ancestor.
     *
     * @param elementName String
     * @return String
     */
    private String getFieldValueByElementNameFromAncestor(String elementName) {
        OnsisStateReportEntity parent = getParentEntity();
        if (parent != null) {
            return parent.deepGetFieldValueByElementName(elementName);
        }
        return null;
    }

    /**
     * Gets the format.
     *
     * @param procedureId String
     * @return Export format definition
     */
    private ExportFormatDefinition getFormat(String procedureId) {
        ExportFormatDefinition format = getGlobalData().getFormatsByIdMap().get(procedureId);
        if (format != null) {
            return format;
        }
        if (format == null && procedureId.equals(getProcedureId())) {
            getGlobalData().setFormat(procedureId, getFormatDefinition());
            return getFormat(procedureId);
        }
        return format;
    }

    /**
     * Gets the format path.
     *
     * @param calcParam String
     * @return String
     */
    private String getFormatPath(String calcParam) {
        if (OnsisResultsHelper.s_submissionsTypes.contains(calcParam)) {
            return calcParam;
        }

        OnsisStateReportData parentReportData = getParentReportData();
        if (parentReportData != null) {
            String parentProcedureId = getParentReportData().getProcedureId();
            List<ExportFormatField> formatFields =
                    getGlobalData().getFormatFields(getFormat(parentProcedureId).getOid());
            ExportFormatField transitionField = null;
            for (ExportFormatField formatField : formatFields) {
                String calcId = formatField.getCalculationId();
                if (OnsisRetrieverNestedExport.CALC_ID.equals(calcId)
                        || OnsisRetrieverNestedExportStreaming.CALC_ID.equals(calcId)) {
                    String topic = OnsisExtractHelper.extractTopic(formatField);
                    if (topic.equals(calcParam)) {
                        transitionField = formatField;
                    }
                }
            }

            String sifPath = transitionField.getSifPath();
            return getParentReportData().getFormatPath(getSifTopic(parentProcedureId))
                    + ELEMENTS_PATH_DELIMITER + sifPath;
        }

        return calcParam;
    }


    /**
     * Sort children.
     *
     * @param element Element
     */
    private void sortChildren(Element element) {
        List<Element> childElementsList = getChildElements("*", element);

        boolean hasChildrenWithAction = false;
        for (Element childElement : childElementsList) {
            hasChildrenWithAction |= getChildElement(OnsisRetrieverAction.ELEMENT_NAME_ACTION, childElement) != null;
            if (hasChildrenWithAction) {
                break;
            }

            /*
             * Recursively process children before processing myself
             */
            // sortElements(childElement);
        }

        /*
         * Create and sort an independent list of my child elements
         */
        if (hasChildrenWithAction) {
            List<String> childsNamesForPrimaryOrder = new ArrayList<>();
            for (Element childElement : childElementsList) {
                String currentChildName = childElement.getNodeName();
                if (!childsNamesForPrimaryOrder.contains(currentChildName)) {
                    childsNamesForPrimaryOrder.add(currentChildName);
                }
            }

            ComparatorChain comparatorsChain = new ComparatorChain();
            comparatorsChain.addComparator(new Comparator<Element>() {
                @Override
                public int compare(Element o1, Element o2) {
                    return Integer.valueOf(childsNamesForPrimaryOrder.indexOf(o1.getNodeName()))
                            .compareTo(Integer.valueOf(childsNamesForPrimaryOrder.indexOf(o2.getNodeName())));
                }
            });

            comparatorsChain.addComparator(new Comparator<Element>() {
                @Override
                public int compare(Element o1, Element o2) {
                    Element actionElement1 = getChildElement(OnsisRetrieverAction.ELEMENT_NAME_ACTION, o1);
                    Element actionElement2 = getChildElement(OnsisRetrieverAction.ELEMENT_NAME_ACTION, o2);
                    if (actionElement1 == null || actionElement2 == null) {
                        return 0;
                    }

                    String action1 = actionElement1.getTextContent().trim();
                    String action2 = actionElement2.getTextContent().trim();

                    if (action1.equals(OnsisRetrieverAction.ACTION_DELETE)
                            && action2.equals(OnsisRetrieverAction.ACTION_DELETE)) {
                        return 0;
                    }
                    if (action1.equals(OnsisRetrieverAction.ACTION_DELETE)) {
                        return -1;
                    }
                    if (action2.equals(OnsisRetrieverAction.ACTION_DELETE)) {
                        return 1;
                    }
                    if (action1.equals(OnsisRetrieverAction.ACTION_UPDATE)
                            && action2.equals(OnsisRetrieverAction.ACTION_UPDATE)) {
                        return 0;
                    }
                    if (action1.equals(OnsisRetrieverAction.ACTION_UPDATE)) {
                        return -1;
                    }
                    if (action2.equals(OnsisRetrieverAction.ACTION_UPDATE)) {
                        return 1;
                    }
                    return 0;
                }
            });

            List<Element> sortedElementsList = new ArrayList<>();
            for (Element childElement : childElementsList) {
                try {
                    sortedElementsList.add(childElement);
                } catch (Throwable t) {
                    System.out.println(LoggerUtils.convertThrowableToString(t));
                }
            }
            Collections.sort(sortedElementsList, comparatorsChain);

            /*
             * Remove all children from the element
             */
            try {
                NodeList allChildNodes = element.getChildNodes();
                while (allChildNodes.getLength() > 0) {
                    element.removeChild(allChildNodes.item(0));
                    allChildNodes = element.getChildNodes();
                }
            } catch (Throwable t) {
                System.out.println(LoggerUtils.convertThrowableToString(t));
            }

            /*
             * Re-add the sorted child elements
             */
            try {
                for (Element child : sortedElementsList) {
                    element.appendChild(child);
                }
            } catch (Throwable t) {
                System.out.println(LoggerUtils.convertThrowableToString(t));
            }
        }
    }


}
