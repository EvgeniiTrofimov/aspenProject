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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.PredefinedResolver;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.GlobalData;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.SubmissionSchoolType;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;

/**
 * The Class OnsisHelpersContainer.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisHelpersContainer {
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
     * The Class OnsisAnnualSpan.
     */
    public static class OnsisAnnualSpan extends OnAnnualSpan {
        private static final Pattern PATTERN_GRADE_8_OR_LOWER = Pattern.compile("^(0[0-8])|[0-8]|PK|K$");
        private static final String GRADE_LEVEL_09 = "09";
        private OnsisAnnualSpan m_bestPrimarySpan;
        private Range<Date> m_dateRange;

        /**
         * Instantiates a new onsis annual span.
         *
         * @param enrollments List<? extends ToolEnrollment>
         * @param spanEnrollments List<? extends ToolEnrollment>
         * @param spanContext ToolDistrictContext
         */
        protected OnsisAnnualSpan(List<? extends ToolEnrollment> enrollments,
                List<? extends ToolEnrollment> spanEnrollments, ToolDistrictContext spanContext) {
            super(enrollments, spanEnrollments, spanContext);
        }

        /**
         * Instantiates a new annual span.
         *
         * @param studentSchool ToolStudentSchool
         * @param broker X2Broker
         */
        protected OnsisAnnualSpan(ToolStudentSchool studentSchool, X2Broker broker) {
            super(studentSchool, broker);
        }

        /**
         * Gets the board resident status.
         *
         * @param globalData GlobalData
         * @return String
         */
        public String getBoardResidentStatus(GlobalData globalData) {
            String status = super.getBoardResidentStatus(globalData.getDictionaryExtractor());
            if (StringUtils.isEmpty(status) && isSecondary()) {
                AnnualSpan primarySpan = getBestPrimarySpanFor(globalData.getBroker(), null);
                if (primarySpan == null) {
                    throw new RuntimeException(
                            "Unable to locate best primary span for concurrent span " + this);
                }
                OnSchool primarySchool = (OnSchool) primarySpan.getSchool();
                String bsid = primarySchool.getBsid();

                List<OnsisStudentScheduleSpan> studentScheduleSpans =
                        getStudent().getStudentScheduleSpans(globalData.getBroker(), globalData, null);
                List<StudentScheduleSpan> eLearningSpans = studentScheduleSpans.stream()
                        .filter(scheduleSpan -> {
                            OnSection section = (OnSection) scheduleSpan.getSection();
                            if (section != null) {
                                return section.isELearning();
                            }
                            return false;
                        }).collect(Collectors.toList());

                if ((StringUtils.isBlank(bsid) && eLearningSpans.isEmpty())
                        || globalData.getSubmissionType().isContinuingEducationSubmission()
                        || globalData.getSubmissionType().isSummerSubmission()
                        || globalData.getSubmissionType().isElementarySubmission()) {
                    OnEnrollment enrollment = (OnEnrollment) primarySpan.getRecentEnrollmentES();
                    if (enrollment != null) {
                        status = globalData.getDictionaryExtractor().getStateValue(enrollment,
                                OnEnrollment.FIELD_BRD_RES_STAT_TYPE);
                    }
                } else {
                    status = OnsisConstants.VALUE_STU_BRD_RES_STAT_SHARED;
                    if (eLearningSpans.size() == studentScheduleSpans.size()) {
                        status = OnsisConstants.VALUE_STU_BRD_RES_STAT_E_LEARNING;
                    }
                }
            }


            return status;
        }

        /**
         * Gets the canadian residence status.
         *
         * @param globalData GlobalData
         * @return String
         */
        public String getCanadianResidenceStatus(GlobalData globalData) {
            return getCanadianResidenceStatus(globalData.getDictionaryExtractor());
        }

        /**
         * Gets the grade type.
         *
         * @param globalData GlobalData
         * @return String
         */
        public String getGradeType(GlobalData globalData) {
            String gradeLevel = super.getGradeType(globalData.getBroker(), globalData.getDictionaryExtractor(),
                    globalData.getGradesHelper());

            /*
             * S-58329 Grade 8 students with secondary school association
             * at high school flagged as 'Reach Ahead'
             * should be reported with GRADE_TYPE = 9
             */
            if (isSecondary() && getReachAheadFlag(globalData)) {
                gradeLevel = globalData.getDictionaryExtractor().lookupStateValue(ToolStudent.FIELD_GRADE_LEVEL,
                        GRADE_LEVEL_09);
            }

            return gradeLevel;
        }

        /**
         * Gets the language prev school.
         *
         * @param globalData GlobalData
         * @return String
         */
        public String getLanguagePrevSchool(GlobalData globalData) {
            return getLanguagePrevSchool(globalData.getDictionaryExtractor());
        }

        /**
         * Gets the mobility type entry.
         *
         * @param globalData GlobalData
         * @return String
         */
        public String getMobilityTypeEntry(GlobalData globalData) {
            return getMobilityTypeEntry(globalData.getDictionaryExtractor());
        }

        /**
         * Gets the mobility type exit.
         *
         * @param globalData GlobalData
         * @return String
         */
        public String getMobilityTypeExit(GlobalData globalData) {
            String result = null;
            OnEnrollment recentEnrollment = (OnEnrollment) getRecentEnrollmentEW();
            if (recentEnrollment != null) {
                if (StudentEnrollment.WITHDRAWAL.equals(recentEnrollment.getEnrollmentType())) {
                    result = globalData.getDictionaryExtractor().getStateValue(recentEnrollment,
                            ToolEnrollment.FIELD_ENROLLMENT_CODE);
                }
            }
            return getMobilityTypeExit(globalData.getDictionaryExtractor());
        }

        /**
         * Gets the mobility type from primary.
         *
         * @param globalData GlobalData
         * @param isEntry boolean
         * @return String
         */
        public String getMobilityTypeFromPrimary(GlobalData globalData, boolean isEntry) {
            return getMobilityTypeFromPrimary(globalData.getDictionaryExtractor(), isEntry);
        }

        /**
         * Gets the reach ahead flag.
         *
         * @param globalData GlobalData
         * @return Boolean
         */
        public Boolean getReachAheadFlag(GlobalData globalData) {
            Boolean result = Boolean.FALSE;
            SubmissionSchoolType submissionSchoolType = globalData.getSchoolType();
            if (submissionSchoolType == null
                    || !StringUtils.isEqual(SubmissionSchoolType.DEFAULT_SCHOOL_LEVEL_CODE_SECONDARY,
                            submissionSchoolType.getSchoolLevelCode())) {
                return Boolean.FALSE;
            }

            OnEnrollment enrollment = null;
            if (!isSecondary()) {
                return Boolean.FALSE;
            }

            OnsisAnnualSpan primarySpan = (OnsisAnnualSpan) getBestPrimarySpanFor(globalData.getBroker(), null);
            if (primarySpan != null) {
                enrollment = (OnEnrollment) primarySpan.getRecentEnrollment();
            }

            if (enrollment != null) {
                int yog = enrollment.getYog();
                if (yog != 0) {
                    String gradeLevel = globalData.getGradesHelper().getGradeLevel(yog);
                    result = PATTERN_GRADE_8_OR_LOWER.matcher(gradeLevel).matches();
                }
            }
            return result;
        }

        /**
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan#getRecentEnrollmentEW()
         */
        @Override
        public ToolEnrollment getRecentEnrollmentEW() {
            // make sure to include any trailing withdrawal
            PlainDate saveDate = getQueryAsOfDate();
            setQueryAsOfDate(null);
            ToolEnrollment enr = super.getRecentEnrollmentEW();
            setQueryAsOfDate(saveDate);
            return enr;
        }

        /**
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan#getStudent()
         */
        @Override
        public OnsisStudent getStudent() {
            return (OnsisStudent) super.getStudent();
        }

    }

    /**
     * A factory for creating OnsisAnnualSpan objects.
     */
    public static class OnsisAnnualSpanFactory extends OnAnnualSpanFactory {

        /**
         *
         */
        public OnsisAnnualSpanFactory(X2Broker broker) {
            super(broker);
        }

        /**
         * Instantiate annual span.
         *
         * @param broker X2Broker
         * @param ssk ToolStudentSchool
         * @return AnnualSpan
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpanFactory#instantiateAnnualSpan(com.follett.fsc.core.k12.business.X2Broker,
         *      com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool)
         */
        @Override
        public AnnualSpan instantiateAnnualSpan(X2Broker broker, ToolStudentSchool ssk) {
            OnsisAnnualSpan span = new OnsisAnnualSpan(ssk, broker);
            initialize(span);
            return span;
        }

        /**
         * Instantiate annual span.
         *
         * @param enrollmentsUpToYear List<? extends ToolEnrollment>
         * @param enrollmentsInThisYear List<? extends ToolEnrollment>
         * @param spanContext ToolDistrictContext
         * @return AnnualSpan
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpanFactory#instantiateAnnualSpan(java.util.List,
         *      java.util.List,
         *      com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext)
         */
        @Override
        public AnnualSpan instantiateAnnualSpan(List<? extends ToolEnrollment> enrollmentsUpToYear,
                                                List<? extends ToolEnrollment> enrollmentsInThisYear,
                                                ToolDistrictContext spanContext) {
            OnsisAnnualSpan span = new OnsisAnnualSpan(enrollmentsUpToYear, enrollmentsInThisYear, spanContext);
            initialize(span);
            return span;
        }
    }

    /**
     * Always assume user has written to the XMLStreamWriter.
     */
    public static class XMLStreamWriterHolder {
        public XMLStreamWriter xmlWriter;
        private Writer rawWriter;
        DebugOutputter debugOutputter = null;

        /**
         * Instantiates a new XML stream writer holder.
         *
         * @param xmlWriter XMLStreamWriter
         * @param writer Writer
         */
        public XMLStreamWriterHolder(XMLStreamWriter xmlWriter,
                Writer writer) {
            this.xmlWriter = xmlWriter;
            this.rawWriter = writer;
        }

        /**
         * Write raw next line.
         *
         * @param rawXml String
         * @throws XMLStreamException exception
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public void writeRawNextLine(String rawXml) throws XMLStreamException, IOException {
            xmlWriter.writeCharacters("\n");
            xmlWriter.flush();
            rawWriter.append(rawXml);
            rawWriter.flush();
        }

        /**
         * Write element next line.
         *
         * @param element Element
         * @throws IOException Signals that an I/O exception has occurred.
         * @throws XMLStreamException exception
         */
        public void writeElementNextLine(Element element) throws IOException, XMLStreamException {
            String xmlStr = getDebugOutputter().outputString(element);
            writeRawNextLine(xmlStr.trim());
        }

        /**
         * Gets the debug outputter.
         *
         * @return Debug outputter
         */
        private DebugOutputter getDebugOutputter() {
            if (debugOutputter == null) {
                debugOutputter = new DebugOutputter();
            }
            return debugOutputter;
        }
    }


    /**
     * Greater than.
     *
     * @param bigDecimal BigDecimal
     * @param zero BigDecimal
     *
     * @return true, if successful
     */
    public static boolean greaterThan(java.math.BigDecimal bigDecimal, java.math.BigDecimal zero) {
        bigDecimal = bigDecimal == null ? BigDecimal.ZERO : bigDecimal;
        zero = zero == null ? BigDecimal.ZERO : zero;

        return bigDecimal.compareTo(zero) > 0;
    }

    /**
     * Max date.
     *
     * @param dates PlainDate[]
     * @return PlainDate
     */
    public static PlainDate maxDate(PlainDate... dates) {
        PlainDate maxDate = null;
        for (PlainDate date : dates) {
            if (date != null && (maxDate == null || date.after(maxDate))) {
                maxDate = date;
            }
        }
        return maxDate;
    }

    /**
     * Creates the Filterable.
     *
     * @param <T> the generic type
     * @param beans List<T>
     * @return Filterable
     */
    public static <T extends X2BaseBean> Filterable createFilterable(Collection<T> beans) {
        return FilterableFactory.create(beans, Arrays.asList(X2BaseBean.COL_OID),
                PredefinedResolver.X2BASE_BEAN);
    }

    /**
     * Parses the multi checkbox value.
     *
     * @param value String
     * @return List
     */
    private static List<String> parseMultiCheckboxValue(String value) {
        if (StringUtils.isEmpty(value)) {
            return Collections.EMPTY_LIST;
        }
        String[] valuesArray = value.split(",");
        ArrayList<String> valuesList = new ArrayList<>();
        for (String valueFromArray : valuesArray) {
            valuesList.add(valueFromArray.trim());
        }
        return valuesList;
    }

}
