/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * This class imports exam results for the UK education system.
 *
 * @author Follett Software Company
 */

public class ATFExportCompleteV09 extends ToolJavaSource {
    public static final String REPORT_ATF = "ATF";

    private List<String> m_messages = new LinkedList();
    private DfEExportManager m_dfEExportManager = null;
    private Document XMLDocument = null;

    private ArrayList<String> m_studentOids = new ArrayList<String>();
    private HashMap<String, Collection<StudentEnrollment>> m_studentEnrollments =
            new HashMap<String, Collection<StudentEnrollment>>();
    private Collection m_students;

    private SisSchool m_school = null;
    private String m_sourceLEA = null;
    private String m_sourceEstab = null;
    private String m_destLEA = null;
    private String m_destEstab = null;

    private HashMap<String, Boolean> m_activeSections = new HashMap<String, Boolean>();

    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";

    public static final List VALID_ENROLLMENT_TYPES = new ArrayList(Arrays.asList(new String[] {"E", "W"}));

    /**
     * Export to XMLDocument.
     */
    protected void exportData() {
        m_dfEExportManager = new DfEExportManager(getBroker(), getLocale());

        logMessage("ATF Export Starting...");

        if (validateSchool()) {
            if (m_students.size() > 0) {
                XMLDocument = createXMLDocument();
            } else {
                logMessage("Error: No students selected.");
            }
        }

        logMessage("ATF Export Completed.");
    }

    /**
     * Load parameters.
     */
    protected void loadParameters() {
        // Student Criteria
        Criteria criteria = new Criteria();
        if (isSchoolContext()) {
            criteria.addEqualTo(Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID,
                    getSchool().getOid());
        }
        String queryBy = (String) getParameter(QUERY_BY_PARAM);

        addUserCriteria(criteria, queryBy, null, Student.class, X2BaseBean.COL_OID);

        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, criteria);
        Collection<String> selectedStudentOids = getBroker().getSubQueryCollectionByQuery(subQuery);

        m_studentOids.addAll(selectedStudentOids);

        if (m_studentOids.size() == 0) {
            logMessage("Error: No students selected.");
        } else {
            loadStudents();

            loadStudentsEnrollments();
        }

        m_school = (SisSchool) getSchool();

        m_destLEA = (String) getParameter(DfEManager.PARAM_DESTINATION_LEA);
        m_destEstab = (String) getParameter(DfEManager.PARAM_DESTINATION_ESTAB);

        Boolean code = (Boolean) getParameter(DfEManager.PARAM_CORE);
        m_activeSections.put(DfEManager.PARAM_CORE, code);
        Boolean basicDetails = (Boolean) getParameter(DfEManager.PARAM_BASIC_DETAILS);
        m_activeSections.put(DfEManager.PARAM_BASIC_DETAILS, basicDetails);
        Boolean sENHistory = (Boolean) getParameter(DfEManager.PARAM_SEN_HISTORY);
        m_activeSections.put(DfEManager.PARAM_SEN_HISTORY, sENHistory);
        Boolean fSMHistory = (Boolean) getParameter(DfEManager.PARAM_FSM_HISTORY);
        m_activeSections.put(DfEManager.PARAM_FSM_HISTORY, fSMHistory);
        Boolean lookedAfter = (Boolean) getParameter(DfEManager.PARAM_LOOKED_AFTER);
        m_activeSections.put(DfEManager.PARAM_LOOKED_AFTER, lookedAfter);
        Boolean admissions = (Boolean) getParameter(DfEManager.PARAM_ADMISSIONS);
        m_activeSections.put(DfEManager.PARAM_ADMISSIONS, admissions);
        Boolean address = (Boolean) getParameter(DfEManager.PARAM_ADDRESS_PHONE_EMAIL);
        m_activeSections.put(DfEManager.PARAM_ADDRESS_PHONE_EMAIL, address);
        Boolean contacts = (Boolean) getParameter(DfEManager.PARAM_CONTACTS);
        m_activeSections.put(DfEManager.PARAM_CONTACTS, contacts);
        Boolean schoolHistory = (Boolean) getParameter(DfEManager.PARAM_SCHOOL_HISTORY);
        m_activeSections.put(DfEManager.PARAM_SCHOOL_HISTORY, schoolHistory);
    }

    /**
     * Load Students.
     */
    protected void loadStudents() {
        if (m_studentOids.size() > 0) {
            Criteria studentCriteria = new Criteria();
            studentCriteria.addIn(X2BaseBean.COL_OID, m_studentOids);

            QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

            m_students = getBroker().getCollectionByQuery(studentQuery);
        } else {
            m_students = new ArrayList<SisStudent>();
        }
    }

    /**
     * Load Students Enrollments.
     */
    protected void loadStudentsEnrollments() {
        if (m_studentOids.size() > 0) {
            Criteria studentEnrollmentCriteria = new Criteria();
            studentEnrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, m_studentOids);
            studentEnrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, VALID_ENROLLMENT_TYPES);

            QueryByCriteria studentEnrollmentQuery =
                    new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);
            studentEnrollmentQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
            studentEnrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
            studentEnrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_TYPE);

            m_studentEnrollments = (HashMap<String, Collection<StudentEnrollment>>) getBroker()
                    .getGroupedCollectionByQuery(studentEnrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 64);
        } else {
            m_studentEnrollments = new HashMap<String, Collection<StudentEnrollment>>();
        }
    }

    /**
     * Returns the value of the passed resource key. If the key is not found, the key itself is
     * returned.
     *
     * @param key String
     * @param resources MessageResources
     * @return String
     */
    protected String getMessage(String key, MessageResources resources) {
        String message = resources.getMessage(getLocale(), key);
        if (message == null) {
            message = key;
        }

        return message;
    }

    /**
     * Returns the entire list of log messages.
     *
     * @return List<String>
     */
    protected List<String> getMessages() {
        return m_messages;
    }

    /**
     * Logs a message that will be written to the results file. Each message is written on its own
     * line.
     *
     * @param key a resource key, if a corresponding message cannot be found then the key itself
     *        will be logged
     */
    protected void logMessage(String key) {
        m_messages.add(key);
    }

    /**
     * Release resources.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();
        return;
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        loadParameters();

        exportData();

        if (XMLDocument != null) {
            exportResults();
        } else {
            displayMessages();
        }
    }

    /**
     * Create DfE ATF XML Document from Student.
     *
     * @return Document
     */
    private Document createXMLDocument() {
        Document xmlDocument = new Document();

        Element rootElement = new Element(DfEHeader.ELEMENT_ATFILE);

        Element headerElement = getCreateHeaderElement(m_school);
        rootElement.addContent(headerElement);

        Element ATFPupilDataElement = new Element(DfEPupil.ELEMENT_ATF_PUPIL_DATA);
        Iterator it = m_students.iterator();
        while (it.hasNext()) {
            SisStudent student = (SisStudent) it.next();

            String studentOid = student.getOid();
            Collection<StudentEnrollment> enrollments = new ArrayList<StudentEnrollment>();
            if (m_studentEnrollments.containsKey(studentOid)) {
                enrollments = m_studentEnrollments.get(studentOid);
            }

            Element pupilElement = getCreateATFPupilDataElement(student, enrollments);

            ATFPupilDataElement.addContent(pupilElement);
        }
        rootElement.addContent(ATFPupilDataElement);

        xmlDocument.setRootElement(rootElement);

        return xmlDocument;
    }

    /**
     * Print out the logs to the user.
     *
     * @throws X2BaseException exception
     */
    private void displayMessages() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);

        MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        buffer.append(resources.getMessage(getLocale(), "message.procedure.results.messages"));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.procedure.results.messagesRule"));
        buffer.append('\n');
        buffer.append('\n');

        if (m_messages.isEmpty()) {
            buffer.append(resources.getMessage(getLocale(), "message.procedure.results.noMessages"));
        } else {
            Iterator messages = m_messages.iterator();
            while (messages.hasNext()) {
                String key = (String) messages.next();

                String message = getMessage(key, resources);

                buffer.append(message);
                buffer.append('\n');
            }
        }

        try {
            byte[] messageBytes = buffer.toString().getBytes();
            ByteArrayInputStream inputStream = new ByteArrayInputStream(messageBytes);
            try {
                OutputStream outputStream = getResultHandler().getOutputStream();
                StreamUtils.copyStream(inputStream, outputStream);
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Exports the XML.
     */
    private void exportResults() {
        try {
            XMLOutputter xMLOutputter = new XMLOutputter();
            Format format = Format.getPrettyFormat();
            xMLOutputter.setFormat(format);

            OutputStream outputStream = getResultHandler().getOutputStream();
            xMLOutputter.output(XMLDocument, outputStream);
        } catch (FileNotFoundException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        } catch (IOException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.getMessage(), e);
        }
    }

    /**
     * Create DfE ATF PupilData Element.
     *
     * @param student SisStudent
     * @param enrollments Collection<StudentEnrollment>
     * @return Element
     */
    private Element getCreateATFPupilDataElement(SisStudent student, Collection<StudentEnrollment> enrollments) {
        DfEPupil dfEPupil = m_dfEExportManager.getDfEPupilByStudent(student, enrollments);

        Element pupilElement = m_dfEExportManager.getPupilDataElement(REPORT_ATF, dfEPupil, m_activeSections);

        return pupilElement;
    }

    /**
     * Create DfE ATF Header Element from Student.
     *
     * @param school SisSchool
     * @return Element
     */
    private Element getCreateHeaderElement(SisSchool school) {
        Schedule schedule = school.getActiveSchedule();
        DistrictSchoolYearContext context = schedule.getDistrictContext();
        String sourceSchoolAcademicYear = Integer.toString(context.getSchoolYear());

        DfEHeader dfEHeader = m_dfEExportManager.getDfEHeader(REPORT_ATF, m_sourceLEA, m_sourceEstab, school.getName(),
                sourceSchoolAcademicYear, m_destLEA, m_destEstab);

        Element headerElement = m_dfEExportManager.getHeaderElement(dfEHeader);

        return headerElement;
    }

    /**
     * Validate the school selected.
     *
     * @return boolean
     */
    private boolean validateSchool() {
        if (m_school == null) {
            logMessage("Error: The selected school doesn't exist.");

            return false;
        }
        m_sourceEstab = m_dfEExportManager.getEstabBySchool(m_school);

        Schedule schedule = m_school.getActiveSchedule();
        if (schedule == null) {
            logMessage("Error: The selected school " + m_school.getName() + " doesn't have an active schedule.");

            return false;
        }

        m_sourceLEA = m_dfEExportManager.getLEABySchool(m_school);
        if (m_sourceLEA == null) {
            logMessage("Error: The selected school's LEA field has not been setup in the Data Dictionary.");

            return false;
        }

        return true;
    }


    /**
     * The Class DfEAddress.
     */
    class DfEAddress {
        public static final String ELEMENT_ADDRESS = "Address";
        public static final String ELEMENT_BS7666_ADDRESS = "BS7666Address";
        public static final String ELEMENT_PAON = "PAON";
        public static final String ELEMENT_SAON = "SAON";
        public static final String ELEMENT_STREET = "Street";
        public static final String ELEMENT_LOCALITY = "Locality";
        public static final String ELEMENT_TOWN = "Town";
        public static final String ELEMENT_ADMINISTRATIVE_AREA = "AdministrativeArea";
        public static final String ELEMENT_POST_TOWN = "PostTown";
        public static final String ELEMENT_UNIQUE_PROPERTY_REFERENCE_NUMBER = "UniquePropertyReferenceNumber";
        public static final String ELEMENT_ADDRESS_LINES = "AddressLines";
        public static final String ELEMENT_ADDRESS_LINE_1 = "AddressLine1";
        public static final String ELEMENT_ADDRESS_LINE_2 = "AddressLine2";
        public static final String ELEMENT_ADDRESS_LINE_3 = "AddressLine3";
        public static final String ELEMENT_ADDRESS_LINE_4 = "AddressLine4";
        public static final String ELEMENT_ADDRESS_LINE_5 = "AddressLine5";
        public static final String ELEMENT_COUNTY = "County";
        public static final String ELEMENT_POST_CODE = "PostCode";
        public static final String ELEMENT_ZIP = "Zip";
        public static final String ELEMENT_COUNTRY = "Country";
        public static final String ELEMENT_EASTING = "Easting";
        public static final String ELEMENT_NORTHING = "Northing";

        private boolean bS7666Address = false;
        private String pAON = null;
        private String sAON = null;
        private String street = null;
        private String locality = null;
        private String town = null;
        private String administrativeArea = null;
        private String postTown = null;
        private String uniquePropertyReferenceNumber = null;
        private String addressLine1 = null;
        private String addressLine2 = null;
        private String addressLine3 = null;
        private String addressLine4 = null;
        private String addressLine5 = null;
        private String county = null;
        private String postCode = null;
        private String zip = null;
        private String country = null;
        private String easting = null;
        private String northing = null;

        /**
         * Constructor for DfE (UK Department for Education) Address Object
         *
         * Convert DfE Address XML Element to DfEAddress Object
         * Used in CTF/ATF Import.
         *
         * @param addressElement Element
         */
        public DfEAddress(Element addressElement) {
            Element bS7666AddressElement = addressElement.getChild(ELEMENT_BS7666_ADDRESS);
            if (bS7666AddressElement != null) {
                bS7666Address = true;
                setSAON(bS7666AddressElement.getChild(ELEMENT_SAON));
                setPAON(bS7666AddressElement.getChild(ELEMENT_PAON));
                setStreet(bS7666AddressElement.getChild(ELEMENT_STREET));
                setLocality(bS7666AddressElement.getChild(ELEMENT_LOCALITY));
                setTown(bS7666AddressElement.getChild(ELEMENT_TOWN));
                setAdministrativeArea(bS7666AddressElement.getChild(ELEMENT_ADMINISTRATIVE_AREA));
                setPostTown(bS7666AddressElement.getChild(ELEMENT_POST_TOWN));
                setUniquePropertyReferenceNumber(
                        bS7666AddressElement.getChild(ELEMENT_UNIQUE_PROPERTY_REFERENCE_NUMBER));
            } else {
                bS7666Address = false;
                Element addressLinesElement = addressElement.getChild(ELEMENT_ADDRESS_LINES);
                if (addressLinesElement != null) {
                    setAddressLine1(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_1));
                    setAddressLine2(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_2));
                    setAddressLine3(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_3));
                    setAddressLine4(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_4));
                    setAddressLine5(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_5));
                }
            }

            setCounty(addressElement.getChild(ELEMENT_COUNTY));
            setPostCode(addressElement.getChild(ELEMENT_POST_CODE));
            setCountry(addressElement.getChild(ELEMENT_COUNTRY));
            setZip(addressElement.getChild(ELEMENT_ZIP));
            setEasting(addressElement.getChild(ELEMENT_EASTING));
            setNorthing(addressElement.getChild(ELEMENT_NORTHING));
        }

        /**
         * Constructor for DfE (UK Department for Education) Address Object.
         */
        public DfEAddress() {}

        /**
         * Gets the hasBS7666Address flag.
         *
         * @return boolean
         */
        public boolean hasBS7666Address() {
            return bS7666Address;
        }

        /**
         * Sets the hasBS7666Address flag.
         *
         * @param hasBS7666Address void
         */
        public void setBS7666Address(boolean hasBS7666Address) {
            this.bS7666Address = hasBS7666Address;
        }

        /**
         * Gets the pAON.
         *
         * @return String
         */
        public String getPAON() {
            return pAON;
        }

        /**
         * Sets the pAON.
         *
         * @param pAON void
         */
        public void setPAON(String pAON) {
            this.pAON = pAON;
        }

        /**
         * Sets the pAON from a DfE XML Element.
         *
         * @param pAONElement void
         */
        public void setPAON(Element pAONElement) {
            if (pAONElement != null) {
                this.pAON = pAONElement.getTextTrim();
            }
        }

        /**
         * Gets the sAON.
         *
         * @return String
         */
        public String getSAON() {
            return sAON;
        }

        /**
         * Sets the sAON.
         *
         * @param sAON void
         */
        public void setSAON(String sAON) {
            this.sAON = sAON;
        }

        /**
         * Sets the sAON from a DfE XML Element.
         *
         * @param sAONElement void
         */
        public void setSAON(Element sAONElement) {
            if (sAONElement != null) {
                this.sAON = sAONElement.getTextTrim();
            }
        }

        /**
         * Gets the street.
         *
         * @return String
         */
        public String getStreet() {
            return street;
        }

        /**
         * Sets the street.
         *
         * @param street void
         */
        public void setStreet(String street) {
            this.street = street;
        }

        /**
         * Sets the street from a DfE XML Element.
         *
         * @param streetElement void
         */
        public void setStreet(Element streetElement) {
            if (streetElement != null) {
                this.street = streetElement.getTextTrim();
            }
        }

        /**
         * Gets the locality.
         *
         * @return String
         */
        public String getLocality() {
            return locality;
        }

        /**
         * Sets the locality.
         *
         * @param locality void
         */
        public void setLocality(String locality) {
            this.locality = locality;
        }

        /**
         * Sets the locality from a DfE XML Element.
         *
         * @param localityElement void
         */
        public void setLocality(Element localityElement) {
            if (localityElement != null) {
                this.locality = localityElement.getTextTrim();
            }
        }

        /**
         * Gets the town.
         *
         * @return String
         */
        public String getTown() {
            return town;
        }

        /**
         * Sets the town.
         *
         * @param town void
         */
        public void setTown(String town) {
            this.town = town;
        }

        /**
         * Sets the town from a DfE XML Element.
         *
         * @param townElement void
         */
        public void setTown(Element townElement) {
            if (townElement != null) {
                this.town = townElement.getTextTrim();
            }
        }

        /**
         * Gets the addressLine1.
         *
         * @return String
         */
        public String getAddressLine1() {
            return addressLine1;
        }

        /**
         * Sets the addressLine1.
         *
         * @param addressLine1 void
         */
        public void setAddressLine1(String addressLine1) {
            this.addressLine1 = addressLine1;
        }

        /**
         * Sets the addressLine1 from a DfE XML Element.
         *
         * @param addressLine1Element void
         */
        public void setAddressLine1(Element addressLine1Element) {
            if (addressLine1Element != null) {
                this.addressLine1 = addressLine1Element.getTextTrim();
            }
        }

        /**
         * Gets the addressLine2.
         *
         * @return String
         */
        public String getAddressLine2() {
            return addressLine2;
        }

        /**
         * Sets the addressLine2.
         *
         * @param addressLine2 void
         */
        public void setAddressLine2(String addressLine2) {
            this.addressLine2 = addressLine2;
        }

        /**
         * Sets the addressLine2 from a DfE XML Element.
         *
         * @param addressLine2Element void
         */
        public void setAddressLine2(Element addressLine2Element) {
            if (addressLine2Element != null) {
                this.addressLine2 = addressLine2Element.getTextTrim();
            }
        }

        /**
         * Gets the addressLine3.
         *
         * @return String
         */
        public String getAddressLine3() {
            return addressLine3;
        }

        /**
         * Sets the addressLine3.
         *
         * @param addressLine3 void
         */
        public void setAddressLine3(String addressLine3) {
            this.addressLine3 = addressLine3;
        }

        /**
         * Sets the addressLine3 from a DfE XML Element.
         *
         * @param addressLine3Element void
         */
        public void setAddressLine3(Element addressLine3Element) {
            if (addressLine3Element != null) {
                this.addressLine3 = addressLine3Element.getTextTrim();
            }
        }

        /**
         * Gets the addressLine4.
         *
         * @return String
         */
        public String getAddressLine4() {
            return addressLine4;
        }

        /**
         * Sets the addressLine4.
         *
         * @param addressLine4 void
         */
        public void setAddressLine4(String addressLine4) {
            this.addressLine4 = addressLine4;
        }

        /**
         * Sets the addressLine4 from a DfE XML Element.
         *
         * @param addressLine4Element void
         */
        public void setAddressLine4(Element addressLine4Element) {
            if (addressLine4Element != null) {
                this.addressLine4 = addressLine4Element.getTextTrim();
            }
        }

        /**
         * Gets the addressLine5.
         *
         * @return String
         */
        public String getAddressLine5() {
            return addressLine5;
        }

        /**
         * Sets the addressLine5.
         *
         * @param addressLine5 void
         */
        public void setAddressLine5(String addressLine5) {
            this.addressLine5 = addressLine5;
        }

        /**
         * Sets the addressLine5 from a DfE XML Element.
         *
         * @param addressLine5Element void
         */
        public void setAddressLine5(Element addressLine5Element) {
            if (addressLine5Element != null) {
                this.addressLine5 = addressLine5Element.getTextTrim();
            }
        }

        /**
         * Gets the county.
         *
         * @return String
         */
        public String getCounty() {
            return county;
        }

        /**
         * Sets the county.
         *
         * @param county void
         */
        public void setCounty(String county) {
            this.county = county;
        }

        /**
         * Sets the county from a DfE XML Element.
         *
         * @param countyElement void
         */
        public void setCounty(Element countyElement) {
            if (countyElement != null) {
                this.county = countyElement.getTextTrim();
            }
        }

        /**
         * Gets the postCode.
         *
         * @return String
         */
        public String getPostCode() {
            return postCode;
        }

        /**
         * Sets the postCode.
         *
         * @param postCode void
         */
        public void setPostCode(String postCode) {
            this.postCode = postCode;
        }

        /**
         * Sets the postCode from a DfE XML Element.
         *
         * @param postCodeElement void
         */
        public void setPostCode(Element postCodeElement) {
            if (postCodeElement != null) {
                this.postCode = postCodeElement.getTextTrim();
            }
        }

        /**
         * Gets the country.
         *
         * @return String
         */
        public String getCountry() {
            return country;
        }

        /**
         * Sets the country.
         *
         * @param country void
         */
        public void setCountry(String country) {
            this.country = country;
        }

        /**
         * Sets the country from a DfE XML Element.
         *
         * @param countryElement void
         */
        public void setCountry(Element countryElement) {
            if (countryElement != null) {
                this.country = countryElement.getTextTrim();
            }
        }

        /**
         * Gets the administrativeArea.
         *
         * @return String
         */
        public String getAdministrativeArea() {
            return administrativeArea;
        }

        /**
         * Sets the administrativeArea.
         *
         * @param administrativeArea void
         */
        public void setAdministrativeArea(String administrativeArea) {
            this.administrativeArea = administrativeArea;
        }

        /**
         * Sets the administrativeArea from a DfE XML Element.
         *
         * @param administrativeAreaElement void
         */
        public void setAdministrativeArea(Element administrativeAreaElement) {
            if (administrativeAreaElement != null) {
                this.administrativeArea = administrativeAreaElement.getTextTrim();
            }
        }

        /**
         * Gets the postTown.
         *
         * @return String
         */
        public String getPostTown() {
            return postTown;
        }

        /**
         * Sets the postTown.
         *
         * @param postTown void
         */
        public void setPostTown(String postTown) {
            this.postTown = postTown;
        }

        /**
         * Sets the postTown from a DfE XML Element.
         *
         * @param postTownElement void
         */
        public void setPostTown(Element postTownElement) {
            if (postTownElement != null) {
                this.postTown = postTownElement.getTextTrim();
            }
        }

        /**
         * Gets the uniquePropertyReferenceNumber.
         *
         * @return String
         */
        public String getUniquePropertyReferenceNumber() {
            return uniquePropertyReferenceNumber;
        }

        /**
         * Sets the uniquePropertyReferenceNumber.
         *
         * @param uniquePropertyReferenceNumber void
         */
        public void setUniquePropertyReferenceNumber(
                                                     String uniquePropertyReferenceNumber) {
            this.uniquePropertyReferenceNumber = uniquePropertyReferenceNumber;
        }

        /**
         * Sets the uniquePropertyReferenceNumber from a DfE XML Element.
         *
         * @param uniquePropertyReferenceNumberElement void
         */
        public void setUniquePropertyReferenceNumber(Element uniquePropertyReferenceNumberElement) {
            if (uniquePropertyReferenceNumberElement != null) {
                this.uniquePropertyReferenceNumber = uniquePropertyReferenceNumberElement.getTextTrim();
            }
        }

        /**
         * Gets the zip.
         *
         * @return String
         */
        public String getZip() {
            return zip;
        }

        /**
         * Sets the zip.
         *
         * @param zip void
         */
        public void setZip(String zip) {
            this.zip = zip;
        }

        /**
         * Sets the zip from a DfE XML Element.
         *
         * @param zipElement void
         */
        public void setZip(Element zipElement) {
            if (zipElement != null) {
                this.zip = zipElement.getTextTrim();
            }
        }

        /**
         * Gets the easting.
         *
         * @return String
         */
        public String getEasting() {
            return easting;
        }

        /**
         * Sets the easting.
         *
         * @param easting void
         */
        public void setEasting(String easting) {
            this.easting = easting;
        }

        /**
         * Sets the easting from a DfE XML Element.
         *
         * @param eastingElement void
         */
        public void setEasting(Element eastingElement) {
            if (eastingElement != null) {
                this.easting = eastingElement.getTextTrim();
            }
        }

        /**
         * Gets the northing.
         *
         * @return String
         */
        public String getNorthing() {
            return northing;
        }

        /**
         * Sets the northing.
         *
         * @param northing void
         */
        public void setNorthing(String northing) {
            this.northing = northing;
        }

        /**
         * Sets the northing from a DfE XML Element.
         *
         * @param northingElement void
         */
        public void setNorthing(Element northingElement) {
            if (northingElement != null) {
                this.northing = northingElement.getTextTrim();
            }
        }
    }

    /**
     * The Class DfEAttendance.
     */
    class DfEAttendance {
        public static final String ELEMENT_YEAR_DATA = "YearData";
        public static final String ELEMENT_YEAR = "Year";
        public static final String ELEMENT_LEA = "LEA";
        public static final String ELEMENT_ESTAB = "Estab";
        public static final String ELEMENT_SCHOOL_NAME = "SchoolName";
        public static final String ELEMENT_SESSIONS_POSSIBLE = "SessionsPossible";
        public static final String ELEMENT_SESSIONS_AUTHORISED = "SessionsAuthorised";
        public static final String ELEMENT_SESSIONS_ATTENDED = "SessionsAttended";
        public static final String ELEMENT_SESSIONS_UNAUTHORISED = "SessionsUnauthorised";
        public static final String ELEMENT_ATTEND_SESSIONS = "AttendSessions";
        public static final String ELEMENT_ATTEND_START_DATE = "AttendanceStartDate";
        public static final String ELEMENT_ATTEND_MARKS = "AttendanceMarks";

        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

        private String year = null;
        private String lEA = null;
        private String estab = null;
        private String schoolName = null;
        private int sessionsPossible = 0;
        private int sessionsAttended = 0;
        private int sessionsAuthorised = 0;
        private int sessionsUnauthorised = 0;
        private PlainDate attendanceStartDate = null;
        private String attendanceMarks = null;

        /**
         * Constructor for DfE Attendance Object.
         *
         * @param studentAttendanceElement Element
         */
        public DfEAttendance(Element studentAttendanceElement) {
            setYear(studentAttendanceElement.getChild(ELEMENT_YEAR));
            setLEA(studentAttendanceElement.getChild(ELEMENT_LEA));
            setEstab(studentAttendanceElement.getChild(ELEMENT_ESTAB));
            setSchoolName(studentAttendanceElement.getChild(ELEMENT_SCHOOL_NAME));
            setSessionsPossible(studentAttendanceElement.getChild(ELEMENT_SESSIONS_POSSIBLE));
            setSessionsAuthorised(studentAttendanceElement.getChild(ELEMENT_SESSIONS_AUTHORISED));
            setSessionsAttended(studentAttendanceElement.getChild(ELEMENT_SESSIONS_ATTENDED));
            setSessionsUnauthorised(studentAttendanceElement.getChild(ELEMENT_SESSIONS_UNAUTHORISED));

            Element attendSessionsElement = studentAttendanceElement.getChild(ELEMENT_ATTEND_SESSIONS);
            if (attendSessionsElement != null) {
                setAttendanceStartDate(attendSessionsElement.getChild(ELEMENT_ATTEND_START_DATE));
                setAttendanceMarks(attendSessionsElement.getChild(ELEMENT_ATTEND_MARKS));
            }
        }

        /**
         * Constructor for DfE (UK Department for Education) Attendance Object.
         *
         * @param year String
         * @param lEA String
         * @param estab String
         * @param schoolName String
         * @param sessionsPossible int
         * @param essionsAuthorised int
         * @param sessionsAttended int
         * @param sessionsUnauthorised int
         * @param attendanceStartDateStr String
         * @param attendanceMarks String
         */
        public DfEAttendance(String year, String lEA, String estab, String schoolName, int sessionsPossible,
                int essionsAuthorised,
                int sessionsAttended, int sessionsUnauthorised, String attendanceStartDateStr, String attendanceMarks) {
            setYear(year);
            setLEA(lEA);
            setEstab(estab);
            setSchoolName(schoolName);
            setSessionsPossible(sessionsPossible);
            setSessionsAuthorised(essionsAuthorised);
            setSessionsAttended(sessionsAttended);
            setSessionsUnauthorised(sessionsUnauthorised);
            setAttendanceStartDate(attendanceStartDateStr);
            setAttendanceMarks(attendanceMarks);
        }

        /**
         * Gets the year.
         *
         * @return PlainDate
         */
        public String getYear() {
            return year;
        }

        /**
         * Sets the year.
         *
         * @param year void
         */
        public void setYear(String year) {
            this.year = year;
        }

        /**
         * Sets the year from a DfE XML Element.
         *
         * @param yearElement void
         */
        public void setYear(Element yearElement) {
            if (yearElement != null) {
                this.year = yearElement.getTextTrim();
            }
        }

        /**
         * Gets the lEA.
         *
         * @return PlainDate
         */
        public String getLEA() {
            return lEA;
        }

        /**
         * Sets the lEA.
         *
         * @param lEA void
         */
        public void setLEA(String lEA) {
            this.lEA = lEA;
        }

        /**
         * Sets the lEA from a DfE XML Element.
         *
         * @param lEA void
         */
        public void setLEA(Element lEA) {
            if (lEA != null) {
                this.lEA = lEA.getText().trim();
            }
        }

        /**
         * Gets the estab.
         *
         * @return PlainDate
         */
        public String getEstab() {
            return estab;
        }

        /**
         * Sets the estab.
         *
         * @param estab void
         */
        public void setEstab(String estab) {
            this.estab = estab;
        }

        /**
         * Sets the estab from a DfE XML Element.
         *
         * @param estabElement void
         */
        public void setEstab(Element estabElement) {
            if (estabElement != null) {
                this.estab = estabElement.getTextTrim();
            }
        }

        /**
         * Gets the schoolName.
         *
         * @return the schoolName
         */
        public String getSchoolName() {
            return schoolName;
        }

        /**
         * Sets the schoolName.
         *
         * @param schoolName void
         */
        public void setSchoolName(String schoolName) {
            this.schoolName = schoolName;
        }

        /**
         * Sets the schoolName from a DfE XML Element.
         *
         * @param schoolNameElement void
         */
        public void setSchoolName(Element schoolNameElement) {
            if (schoolNameElement != null) {
                this.schoolName = schoolNameElement.getTextTrim();
            }
        }

        /**
         * Gets the sessionsPossible.
         *
         * @return the sessionsPossible
         */
        public int getSessionsPossible() {
            return sessionsPossible;
        }

        /**
         * Sets the sessionsPossible.
         *
         * @param sessionsPossible void
         */
        public void setSessionsPossible(int sessionsPossible) {
            this.sessionsPossible = sessionsPossible;
        }

        /**
         * Sets the sessionsPossible from a DfE XML Element.
         *
         * @param sessionsPossibleElement void
         */
        public void setSessionsPossible(Element sessionsPossibleElement) {
            if (sessionsPossibleElement != null) {
                String sessionsPossible1 = sessionsPossibleElement.getTextTrim();
                if (!StringUtils.isEmpty(sessionsPossible1) && StringUtils.isNumeric(sessionsPossible1)) {
                    this.sessionsPossible = Integer.parseInt(sessionsPossible1);
                }
            }
        }

        /**
         * Gets the sessionsAttended.
         *
         * @return the sessionsAttended
         */
        public int getSessionsAttended() {
            return sessionsAttended;
        }

        /**
         * Sets the sessionsAttended.
         *
         * @param sessionsAttended void
         */
        public void setSessionsAttended(int sessionsAttended) {
            this.sessionsAttended = sessionsAttended;
        }

        /**
         * Sets the sessionsAttended from a DfE XML Element.
         *
         * @param sessionsAttendedElement void
         */
        public void setSessionsAttended(Element sessionsAttendedElement) {
            if (sessionsAttendedElement != null) {
                String sessionsAttended1 = sessionsAttendedElement.getTextTrim();
                if (!StringUtils.isEmpty(sessionsAttended1) && StringUtils.isNumeric(sessionsAttended1)) {
                    this.sessionsAttended = Integer.parseInt(sessionsAttended1);
                }
            }
        }

        /**
         * Gets the sessionsAuthorised.
         *
         * @return the sessionsAuthorised
         */
        public int getSessionsAuthorised() {
            return sessionsAuthorised;
        }

        /**
         * Sets the sessionsAuthorised.
         *
         * @param sessionsAuthorised void
         */
        public void setSessionsAuthorised(int sessionsAuthorised) {
            this.sessionsAuthorised = sessionsAuthorised;
        }

        /**
         * Sets the sessionsAuthorised from a DfE XML Element.
         *
         * @param sessionsAuthorisedElement void
         */
        public void setSessionsAuthorised(Element sessionsAuthorisedElement) {
            if (sessionsAuthorisedElement != null) {
                String sessionsAuthorised1 = sessionsAuthorisedElement.getTextTrim();
                if (!StringUtils.isEmpty(sessionsAuthorised1) && StringUtils.isNumeric(sessionsAuthorised1)) {
                    this.sessionsAuthorised = Integer.parseInt(sessionsAuthorised1);
                }
            }
        }

        /**
         * Gets the sessionsUnauthorised.
         *
         * @return the sessionsUnauthorised
         */
        public int getSessionsUnauthorised() {
            return sessionsUnauthorised;
        }

        /**
         * Sets the sessionsUnauthorised.
         *
         * @param sessionsUnauthorised void
         */
        public void setSessionsUnauthorised(int sessionsUnauthorised) {
            this.sessionsUnauthorised = sessionsUnauthorised;
        }

        /**
         * Sets the sessionsUnauthorised from a DfE XML Element.
         *
         * @param sessionsUnauthorisedElement void
         */
        public void setSessionsUnauthorised(Element sessionsUnauthorisedElement) {
            if (sessionsUnauthorisedElement != null) {
                String sessionsUnauthorised1 = sessionsUnauthorisedElement.getTextTrim();
                if (!StringUtils.isEmpty(sessionsUnauthorised1) && StringUtils.isNumeric(sessionsUnauthorised1)) {
                    this.sessionsUnauthorised = Integer.parseInt(sessionsUnauthorised1);
                }
            }
        }

        /**
         * Gets the attendanceStartDate.
         *
         * @return the attendanceStartDate
         */
        public PlainDate getAttendanceStartDate() {
            return attendanceStartDate;
        }

        /**
         * Sets the attendanceStartDate.
         *
         * @param attendanceStartDate void
         */
        public void setAttendanceStartDate(PlainDate attendanceStartDate) {
            this.attendanceStartDate = attendanceStartDate;
        }

        /**
         * Sets the attendanceStartDate from a String Date.
         *
         * @param attendanceStartDate void
         */
        public void setAttendanceStartDate(String attendanceStartDate) {
            if (attendanceStartDate != null) {
                String attendanceStartDateStr = attendanceStartDate.trim();
                if (!StringUtils.isEmpty(attendanceStartDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(attendanceStartDateStr);
                        this.attendanceStartDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.attendanceStartDate = null;
                    }
                }
            }
        }

        /**
         * Sets the attendanceStartDate from a DfE XML Element.
         *
         * @param attendanceStartDateElement void
         */
        public void setAttendanceStartDate(Element attendanceStartDateElement) {
            if (attendanceStartDateElement != null) {
                String entryDateStr = attendanceStartDateElement.getTextTrim();
                if (!StringUtils.isEmpty(entryDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(entryDateStr);
                        this.attendanceStartDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.attendanceStartDate = null;
                    }
                }
            }
        }

        /**
         * Gets the attendanceMarks.
         *
         * @return the attendanceMarks
         */
        public String getAttendanceMarks() {
            return attendanceMarks;
        }

        /**
         * Sets the attendanceMarks.
         *
         * @param attendanceMarks void
         */
        public void setAttendanceMarks(String attendanceMarks) {
            this.attendanceMarks = attendanceMarks;
        }

        /**
         * Sets the attendanceMarks from a DfE XML Element.
         *
         * @param attendanceMarksElement void
         */
        public void setAttendanceMarks(Element attendanceMarksElement) {
            if (attendanceMarksElement != null) {
                this.attendanceMarks = attendanceMarksElement.getTextTrim();
            }
        }

    }


    /**
     * The Class DfEContact.
     */
    class DfEContact {
        public static final String ELEMENT_ORDER = "Order";
        public static final String ELEMENT_TITLE = "Title";
        public static final String ELEMENT_FORENAME = "Forename";
        public static final String ELEMENT_SURNAME = "Surname";
        public static final String ELEMENT_GENDER = "Gender";
        public static final String ELEMENT_MIDDLE_NAMES = "MiddleNames";
        public static final String ELEMENT_RELATIONSHIP = "Relationship";
        public static final String ELEMENT_RESPONSIBILITY = "Responsibility";
        public static final String ELEMENT_ADDRESS = "Address";
        public static final String ELEMENT_ADDRESS_AS_PUPIL = "AddressAsPupil";
        public static final String ELEMENT_PHONES = "Phones";
        public static final String ELEMENT_PHONE = "Phone";
        public static final String ELEMENT_TELEPHONE_TYPE = "TelephoneType";
        public static final String ELEMENT_PHONE_NUMBER = "PhoneNo";
        public static final String ELEMENT_EMAIL = "Email";

        private int order = 0;
        private String title = null;
        private String surname = null;
        private String forename = null;
        private String middleNames = null;
        private String gender = null;
        private String relationshipCode = null;
        private Boolean isResponsible = null;
        private Boolean hasAddressAsPupil = null;
        private DfEAddress dfEAddress = null;
        private ArrayList<DfETelephone> telephones = new ArrayList();
        private String email = null;

        /**
         * Constructor for DfE (UK Department for Education) Contact Object.
         *
         * @param contactElement Element
         */
        public DfEContact(Element contactElement) {
            setOrder(contactElement.getChild(ELEMENT_ORDER));
            setTitle(contactElement.getChild(ELEMENT_TITLE));
            setSurname(contactElement.getChild(ELEMENT_SURNAME));
            setForename(contactElement.getChild(ELEMENT_FORENAME));
            setGender(contactElement.getChild(ELEMENT_GENDER));
            setMiddleNames(contactElement.getChild(ELEMENT_MIDDLE_NAMES));
            setRelationship(contactElement.getChild(ELEMENT_RELATIONSHIP));
            setResponsible(contactElement.getChild(ELEMENT_RESPONSIBILITY));

            Element addressElement = contactElement.getChild(ELEMENT_ADDRESS);
            if (addressElement != null) {
                Element addressAsPupilElement = addressElement.getChild(ELEMENT_ADDRESS_AS_PUPIL);
                if (addressAsPupilElement != null) {
                    setAddressAsPupil(addressElement.getChild(ELEMENT_ADDRESS_AS_PUPIL));
                } else {
                    this.dfEAddress = new DfEAddress(addressElement);
                }
            }

            Element phonesElement = contactElement.getChild(ELEMENT_PHONES);
            if (phonesElement != null) {
                List<Element> phonesList = phonesElement.getChildren(ELEMENT_PHONE);
                for (int k = 0; k < phonesList.size(); k++) {
                    Element telephoneElement = phonesList.get(k);
                    DfETelephone dfETelephone = new DfETelephone(telephoneElement);
                    telephones.add(dfETelephone);
                }
            }

            setEmail(contactElement.getChild(ELEMENT_EMAIL));
        }

        /**
         * Constructor for DfE (UK Department for Education) Contact Object.
         */
        public DfEContact() {}

        /**
         * Gets the order.
         *
         * @return int
         */
        public int getOrder() {
            return order;
        }

        /**
         * Sets the order.
         *
         * @param order void
         */
        public void setOrder(int order) {
            this.order = order;
        }

        /**
         * Sets the order from a DfE XML Element.
         *
         * @param orderElement void
         */
        public void setOrder(Element orderElement) {
            if (orderElement != null) {
                int orderInt = 0;
                String orderStr = orderElement.getTextTrim();
                if (StringUtils.isInteger(orderStr)) {
                    orderInt = Integer.parseInt(orderStr);
                    this.order = orderInt;
                }
            }
        }

        /**
         * Gets the title.
         *
         * @return String
         */
        public String getTitle() {
            return title;
        }

        /**
         * Sets the title.
         *
         * @param title void
         */
        public void setTitle(String title) {
            this.title = title;
        }

        /**
         * Sets the title from a DfE XML Element.
         *
         * @param titleElement void
         */
        public void setTitle(Element titleElement) {
            if (titleElement != null) {
                this.title = titleElement.getTextTrim();
            }
        }

        /**
         * Gets the surname.
         *
         * @return String
         */
        public String getSurname() {
            return surname;
        }

        /**
         * Sets the surname.
         *
         * @param surname void
         */
        public void setSurname(String surname) {
            this.surname = surname;
        }

        /**
         * Sets the surname from a DfE XML Element.
         *
         * @param surnameElement void
         */
        public void setSurname(Element surnameElement) {
            if (surnameElement != null) {
                this.surname = surnameElement.getTextTrim();
            }
        }

        /**
         * Gets the forename.
         *
         * @return String
         */
        public String getForename() {
            return forename;
        }

        /**
         * Sets the forename.
         *
         * @param forename void
         */
        public void setForename(String forename) {
            this.forename = forename;
        }

        /**
         * Sets the forename from a DfE XML Element.
         *
         * @param forenameElement void
         */
        public void setForename(Element forenameElement) {
            if (forenameElement != null) {
                this.forename = forenameElement.getTextTrim();
            }
        }

        /**
         * Gets the gender.
         *
         * @return String
         */
        public String getGender() {
            return gender;
        }

        /**
         * Sets the gender.
         *
         * @param gender void
         */
        public void setGender(String gender) {
            this.gender = gender;
        }

        /**
         * Sets the gender from a DfE XML Element.
         *
         * @param genderElement void
         */
        public void setGender(Element genderElement) {
            if (genderElement != null) {
                this.gender = genderElement.getTextTrim();
            }
        }

        /**
         * Gets the middleNames.
         *
         * @return String
         */
        public String getMiddleNames() {
            return middleNames;
        }

        /**
         * Sets the middleNames.
         *
         * @param middleNames void
         */
        public void setMiddleNames(String middleNames) {
            this.middleNames = middleNames;
        }

        /**
         * Sets the middleNames from a DfE XML Element.
         *
         * @param middleNamesElement void
         */
        public void setMiddleNames(Element middleNamesElement) {
            if (middleNamesElement != null) {
                this.middleNames = middleNamesElement.getTextTrim();
            }
        }

        /**
         * Gets the relationshipCode.
         *
         * @return String
         */
        public String getRelationshipCode() {
            return relationshipCode;
        }

        /**
         * Sets the relationshipCode.
         *
         * @param relationshipCode void
         */
        public void setRelationshipCode(String relationshipCode) {
            this.relationshipCode = relationshipCode;
        }

        /**
         * Sets the relationshipCode from a DfE XML Element.
         *
         * @param relationshipCodeElement void
         */
        public void setRelationship(Element relationshipCodeElement) {
            if (relationshipCodeElement != null) {
                this.relationshipCode = relationshipCodeElement.getTextTrim();
            }
        }

        /**
         * Gets the Responsible flag.
         *
         * @return Boolean
         */
        public Boolean getResponsible() {
            return isResponsible;
        }

        /**
         * Sets the responsible flag.
         *
         * @param responsible void
         */
        public void setResponsible(Boolean responsible) {
            this.isResponsible = responsible;
        }

        /**
         * Sets the responsible from a DfE XML Element.
         *
         * @param responsibleElement void
         */
        public void setResponsible(Element responsibleElement) {
            if (responsibleElement != null) {
                String resp = responsibleElement.getTextTrim().toLowerCase();
                this.isResponsible = Boolean.valueOf(DfEManager.STRING_TRUE.equals(resp));
            }
        }

        /**
         * Gets the isAddressAsPupil flag.
         *
         * @return Boolean
         */
        public Boolean getAddressAsPupil() {
            return hasAddressAsPupil;
        }

        /**
         * Sets the addressAsPupil flag.
         *
         * @param addressAsPupil void
         */
        public void setAddressAsPupil(Boolean addressAsPupil) {
            this.hasAddressAsPupil = addressAsPupil;
        }

        /**
         * Sets the addressAsPupil from a DfE XML Element.
         *
         * @param addressAsPupilElement void
         */
        public void setAddressAsPupil(Element addressAsPupilElement) {
            if (addressAsPupilElement != null) {
                String sameAddress = addressAsPupilElement.getTextTrim().toLowerCase();
                this.hasAddressAsPupil = Boolean.valueOf(DfEManager.STRING_TRUE.equals(sameAddress));
            }
        }

        /**
         * Gets the DfEAddress for Contact.
         *
         * @return DfEAddress
         */
        public DfEAddress getDfEAddress() {
            return dfEAddress;
        }

        /**
         * Sets the dfEAddress for Contact.
         *
         * @param dfEAddress void
         */
        public void setDfEAddress(DfEAddress dfEAddress) {
            this.dfEAddress = dfEAddress;
        }

        /**
         * Gets the ArrayList of DfETelephone for Contact.
         *
         * @return ArrayList<DfETelephone>
         */
        public ArrayList<DfETelephone> getTelephones() {
            return telephones;
        }

        /**
         * Adds a DfETelephone to the Pupil's DfETelephone Collection.
         *
         * @param dfETelephone DfETelephone
         */
        public void addTelephone(DfETelephone dfETelephone) {
            telephones.add(dfETelephone);
        }

        /**
         * Gets the email.
         *
         * @return String
         */
        public String getEmail() {
            return email;
        }

        /**
         * Sets the email.
         *
         * @param email void
         */
        public void setEmail(String email) {
            this.email = email;
        }

        /**
         * Sets the email from a DfE XML Element.
         *
         * @param emailElement void
         */
        public void setEmail(Element emailElement) {
            if (emailElement != null) {
                this.email = emailElement.getTextTrim();
            }
        }

    }


    /**
     * The Class DfEFSMInstance.
     */
    class DfEFSMInstance {
        public static final String PROGRAM_CODE_FSM = "FSM";
        public static final String ELEMENT_FSM_HISTORY = "FSMhistory";
        public static final String ELEMENT_FSM_REVIEW_DATE = "FSMreviewDate";
        public static final String ELEMENT_FSM_INSTANCE = "FSMinstance";
        public static final String ELEMENT_FSM_START_DATE = "FSMstartDate";
        public static final String ELEMENT_FSM_END_DATE = "FSMendDate";
        public static final String ELEMENT_FSM_UK_COUNTRY = "UKcountry";

        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

        private PlainDate fSMStartDate = null;
        private PlainDate fSMEndDate = null;
        private String uKCountry = null;

        /**
         * Constructor for DfE (UK Department for Education) FSM Instance Object.
         *
         * @param fSMInstanceElement Element
         */
        public DfEFSMInstance(Element fSMInstanceElement) {
            setFSMStartDate(fSMInstanceElement.getChild(ELEMENT_FSM_START_DATE));
            setFSMEndDate(fSMInstanceElement.getChild(ELEMENT_FSM_END_DATE));
            setUKCountry(fSMInstanceElement.getChild(ELEMENT_FSM_UK_COUNTRY));
        }

        /**
         * Constructor for DfE (UK Department for Education) SEN Need Object.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param uKCountry String
         */
        public DfEFSMInstance(PlainDate startDate, PlainDate endDate, String uKCountry) {
            setFSMStartDate(startDate);
            setFSMEndDate(endDate);
            setUKCountry(uKCountry);
        }

        /**
         * Gets the fSMStartDate.
         *
         * @return PlainDate
         */
        public PlainDate getFSMStartDate() {
            return fSMStartDate;
        }

        /**
         * Sets the fSMStartDate.
         *
         * @param fSMStartDate void
         */
        public void setFSMStartDate(PlainDate fSMStartDate) {
            this.fSMStartDate = fSMStartDate;
        }

        /**
         * Sets the fSMStartDate from a DfE XML Element.
         *
         * @param fSMStartDateElement void
         */
        public void setFSMStartDate(Element fSMStartDateElement) {
            if (fSMStartDateElement != null) {
                String fSMStartDateStr = fSMStartDateElement.getTextTrim();
                if (!StringUtils.isEmpty(fSMStartDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(fSMStartDateStr);
                        this.fSMStartDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.fSMStartDate = null;
                    }
                }
            }
        }

        /**
         * Gets the fSMEndDate.
         *
         * @return PlainDate
         */
        public PlainDate getFSMEndDate() {
            return fSMEndDate;
        }

        /**
         * Sets the fSMEndDate.
         *
         * @param fSMEndDate void
         */
        public void setFSMEndDate(PlainDate fSMEndDate) {
            this.fSMEndDate = fSMEndDate;
        }

        /**
         * Sets the fSMEndDate from a DfE XML Element.
         *
         * @param fSMEndDateElement void
         */
        public void setFSMEndDate(Element fSMEndDateElement) {

            if (fSMEndDateElement != null) {
                String fSMEndDateStr = fSMEndDateElement.getTextTrim();
                if (!StringUtils.isEmpty(fSMEndDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(fSMEndDateStr);
                        this.fSMEndDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.fSMEndDate = null;
                    }
                }
            }
        }

        /**
         * Gets the uKCountry.
         *
         * @return String
         */
        public String getUKCountry() {
            return uKCountry;
        }

        /**
         * Sets the uKCountry.
         *
         * @param uKCountry void
         */
        public void setUKCountry(String uKCountry) {
            this.uKCountry = uKCountry;
        }

        /**
         * Sets the uKCountry from a DfE XML Element.
         *
         * @param uKCountryElement void
         */
        public void setUKCountry(Element uKCountryElement) {
            if (uKCountryElement != null) {
                this.uKCountry = uKCountryElement.getTextTrim();
            }
        }

    }


    /**
     * The Class DfEHeader.
     */
    class DfEHeader {
        public static final String ELEMENT_CTFILE = "CTfile";
        public static final String ELEMENT_ATFILE = "ATfile";
        public static final String ELEMENT_HEADER = "Header";
        public static final String ELEMENT_DOCUMENT_NAME = "DocumentName";
        public static final String ELEMENT_CTF_VERSION = "CTFversion";
        public static final String ELEMENT_ATF_VERSION = "ATFversion";
        public static final String ELEMENT_DATE_TIME = "DateTime";
        public static final String ELEMENT_DOCUMENT_QUALIFIER = "DocumentQualifier";
        public static final String ELEMENT_DATA_QUALIFIER = "DataQualifier";
        public static final String ELEMENT_DATA_DESCRIPTOR = "DataDescriptor";
        public static final String ELEMENT_SUPPLIER_ID = "SupplierID";
        public static final String ELEMENT_SOURCE_SCHOOL = "SourceSchool";
        public static final String ELEMENT_LEA = "LEA";
        public static final String ELEMENT_ESTABISHMENT = "Estab";
        public static final String ELEMENT_SCHOOL_NAME = "SchoolName";
        public static final String ELEMENT_ACADEMIC_YEAR = "AcademicYear";
        public static final String ELEMENT_DESTINATION_SCHOOL = "DestSchool";
        public static final String ELEMENT_SUPP_ID = "SuppID";

        public static final String HEADER_TYPE_CTF = "CTF";
        public static final String CTF_DOCUMENT_NAME = "Common Transfer File";
        public static final String CTF_VERSION = "12.0";
        public static final String HEADER_TYPE_ATF = "ATF";
        public static final String ATF_DOCUMENT_NAME = "Admissions Data Transfer File";
        public static final String ATF_VERSION = "9.1";
        public static final String DEFAULT_DOCUMENT_QUALIFIER = "full";
        public static final String DEFAULT_SUPPLIER_ID = "FSC";

        private String headerType = null;
        private String documentName = null;
        private String version = null;
        private String dateTime = null;
        private String documentQualifier = null;
        private String dataQualifier = null;
        private String dataDescriptor = null;
        private String supplierID = null;
        private String sourceSchoolLEA = null;
        private String sourceSchoolEstab = null;
        private String sourceSchoolName = null;
        private String sourceSchoolAcademicYear = null;
        private String destSchoolLEA = null;
        private String destSchoolEstab = null;
        private String suppID = null;

        /**
         * Constructor for DfE (UK Department for Education) Header Object.
         */
        public DfEHeader() {}

        /**
         * Constructor for DfE (UK Department for Education) Header Object.
         *
         * @param HeaderElement Element
         */
        public DfEHeader(Element HeaderElement) {
            setDocumentName(HeaderElement.getChild(ELEMENT_DOCUMENT_NAME));
            if (CTF_DOCUMENT_NAME.equals(documentName)) {
                setHeaderType(HEADER_TYPE_CTF);
                setVersion(HeaderElement.getChild(ELEMENT_CTF_VERSION));
            } else {
                setHeaderType(HEADER_TYPE_ATF);
                setVersion(HeaderElement.getChild(ELEMENT_ATF_VERSION));
            }
            setDateTime(HeaderElement.getChild(ELEMENT_DATE_TIME));
            setDocumentQualifier(HeaderElement.getChild(ELEMENT_DOCUMENT_QUALIFIER));
            setDataQualifier(HeaderElement.getChild(ELEMENT_DATA_QUALIFIER));
            setDataDescriptor(HeaderElement.getChild(ELEMENT_DATA_DESCRIPTOR));
            setSupplierID(HeaderElement.getChild(ELEMENT_SUPPLIER_ID));

            Element sourceSchoolElement = HeaderElement.getChild(ELEMENT_SOURCE_SCHOOL);
            if (sourceSchoolElement != null) {
                setSourceSchoolLEA(sourceSchoolElement.getChild(ELEMENT_LEA));
                setSourceSchoolEstab(sourceSchoolElement.getChild(ELEMENT_ESTABISHMENT));
                setSourceSchoolName(sourceSchoolElement.getChild(ELEMENT_SCHOOL_NAME));
                setSourceSchoolAcademicYear(sourceSchoolElement.getChild(ELEMENT_ACADEMIC_YEAR));
            }

            Element destSchoolElement = HeaderElement.getChild(ELEMENT_DESTINATION_SCHOOL);
            if (destSchoolElement != null) {
                setDestSchoolLEA(destSchoolElement.getChild(ELEMENT_LEA));
                setDestSchoolEstab(destSchoolElement.getChild(ELEMENT_ESTABISHMENT));
            }

            setSuppID(HeaderElement.getChild(ELEMENT_SUPP_ID));
        }

        /**
         * Gets the documentName.
         *
         * @return String
         */
        public String getDocumentName() {
            return documentName;
        }

        /**
         * Sets the documentName.
         *
         * @param documentName void
         */
        public void setDocumentName(String documentName) {
            this.documentName = documentName;
        }

        /**
         * Sets the documentName from a DfE XML Element.
         *
         * @param documentNameElement void
         */
        public void setDocumentName(Element documentNameElement) {
            if (documentNameElement != null) {
                this.documentName = documentNameElement.getTextTrim();
            }
        }

        /**
         * Gets the headerType.
         *
         * @return String
         */
        public String getHeaderType() {
            return headerType;
        }

        /**
         * Sets the headerType.
         *
         * @param headerType void
         */
        public void setHeaderType(String headerType) {
            this.headerType = headerType;
        }

        /**
         * Sets the headerType from a DfE XML Element.
         *
         * @param headerTypeElement void
         */
        public void setHeaderType(Element headerTypeElement) {
            if (headerTypeElement != null) {
                this.headerType = headerTypeElement.getTextTrim();
            }
        }

        /**
         * Gets the version.
         *
         * @return String
         */
        public String getVersion() {
            return version;
        }

        /**
         * Sets the Version.
         *
         * @param version void
         */
        public void setVersion(String version) {
            this.version = version;
        }

        /**
         * Sets the Version from a DfE XML Element.
         *
         * @param versionElement void
         */
        public void setVersion(Element versionElement) {
            if (versionElement != null) {
                this.version = versionElement.getTextTrim();
            }
        }

        /**
         * Gets the dateTime.
         *
         * @return String
         */
        public String getDateTime() {
            return dateTime;
        }

        /**
         * Sets the dateTime.
         *
         * @param dateTime void
         */
        public void setDateTime(String dateTime) {
            this.dateTime = dateTime;
        }

        /**
         * Sets the dateTime from a DfE XML Element.
         *
         * @param dateTimeElement void
         */
        public void setDateTime(Element dateTimeElement) {
            if (dateTimeElement != null) {
                this.dateTime = dateTimeElement.getTextTrim();
            }
        }

        /**
         * Gets the documentQualifier.
         *
         * @return String
         */
        public String getDocumentQualifier() {
            return documentQualifier;
        }

        /**
         * Sets the documentQualifier.
         *
         * @param documentQualifier void
         */
        public void setDocumentQualifier(String documentQualifier) {
            this.documentQualifier = documentQualifier;
        }

        /**
         * Sets the documentQualifier from a DfE XML Element.
         *
         * @param documentQualifierElement void
         */
        public void setDocumentQualifier(Element documentQualifierElement) {
            if (documentQualifierElement != null) {
                this.documentQualifier = documentQualifierElement.getTextTrim();
            }
        }

        /**
         * Gets the dataQualifier.
         *
         * @return String
         */
        public String getDataQualifier() {
            return dataQualifier;
        }

        /**
         * Sets the dataQualifier.
         *
         * @param dataQualifier void
         */
        public void setDataQualifier(String dataQualifier) {
            this.dataQualifier = dataQualifier;
        }

        /**
         * Sets the dataQualifier from a DfE XML Element.
         *
         * @param dataQualifierElement void
         */
        public void setDataQualifier(Element dataQualifierElement) {
            if (dataQualifierElement != null) {
                this.dataQualifier = dataQualifierElement.getTextTrim();
            }
        }

        /**
         * Gets the dataDescriptor.
         *
         * @return String
         */
        public String getDataDescriptor() {
            return dataDescriptor;
        }

        /**
         * Sets the dataDescriptor.
         *
         * @param dataDescriptor void
         */
        public void setDataDescriptor(String dataDescriptor) {
            this.dataDescriptor = dataDescriptor;
        }

        /**
         * Sets the dataDescriptor from a DfE XML Element.
         *
         * @param dataDescriptorElement void
         */
        public void setDataDescriptor(Element dataDescriptorElement) {
            if (dataDescriptorElement != null) {
                this.dataDescriptor = dataDescriptorElement.getTextTrim();
            }
        }

        /**
         * Gets the supplierID.
         *
         * @return String
         */
        public String getSupplierID() {
            return supplierID;
        }

        /**
         * Sets the supplierID.
         *
         * @param supplierID void
         */
        public void setSupplierID(String supplierID) {
            this.supplierID = supplierID;
        }

        /**
         * Sets the supplierID from a DfE XML Element.
         *
         * @param supplierIDElement void
         */
        public void setSupplierID(Element supplierIDElement) {
            if (supplierIDElement != null) {
                this.supplierID = supplierIDElement.getTextTrim();
            }
        }

        /**
         * Gets the sourceSchoolLEA.
         *
         * @return String
         */
        public String getSourceSchoolLEA() {
            return sourceSchoolLEA;
        }

        /**
         * Sets the sourceSchoolLEA.
         *
         * @param sourceSchoolLEA void
         */
        public void setSourceSchoolLEA(String sourceSchoolLEA) {
            this.sourceSchoolLEA = sourceSchoolLEA;
        }

        /**
         * Sets the sourceSchoolLEA from a DfE XML Element.
         *
         * @param sourceSchoolLEAElement void
         */
        public void setSourceSchoolLEA(Element sourceSchoolLEAElement) {
            if (sourceSchoolLEAElement != null) {
                this.sourceSchoolLEA = sourceSchoolLEAElement.getTextTrim();
            }
        }

        /**
         * Gets the sourceSchoolEstab.
         *
         * @return String
         */
        public String getSourceSchoolEstab() {
            return sourceSchoolEstab;
        }

        /**
         * Sets the sourceSchoolEstab.
         *
         * @param sourceSchoolEstab void
         */
        public void setSourceSchoolEstab(String sourceSchoolEstab) {
            this.sourceSchoolEstab = sourceSchoolEstab;
        }

        /**
         * Sets the sourceSchoolEstab from a DfE XML Element.
         *
         * @param sourceSchoolEstabElement void
         */
        public void setSourceSchoolEstab(Element sourceSchoolEstabElement) {
            if (sourceSchoolEstabElement != null) {
                this.sourceSchoolEstab = sourceSchoolEstabElement.getTextTrim();
            }
        }

        /**
         * Gets the sourceSchoolName.
         *
         * @return String
         */
        public String getSourceSchoolName() {
            return sourceSchoolName;
        }

        /**
         * Sets the sourceSchoolName.
         *
         * @param sourceSchoolName void
         */
        public void setSourceSchoolName(String sourceSchoolName) {
            this.sourceSchoolName = sourceSchoolName;
        }

        /**
         * Sets the sourceSchoolName from a DfE XML Element.
         *
         * @param sourceSchoolNameElement void
         */
        public void setSourceSchoolName(Element sourceSchoolNameElement) {
            if (sourceSchoolNameElement != null) {
                this.sourceSchoolName = sourceSchoolNameElement.getTextTrim();
            }
        }

        /**
         * Gets the sourceSchoolAcademicYear.
         *
         * @return String
         */
        public String getSourceSchoolAcademicYear() {
            return sourceSchoolAcademicYear;
        }

        /**
         * Sets the sourceSchoolAcademicYear.
         *
         * @param sourceSchoolAcademicYear void
         */
        public void setSourceSchoolAcademicYear(String sourceSchoolAcademicYear) {
            this.sourceSchoolAcademicYear = sourceSchoolAcademicYear;
        }

        /**
         * Sets the sourceSchoolAcademicYear from a DfE XML Element.
         *
         * @param sourceSchoolAcademicYearElement void
         */
        public void setSourceSchoolAcademicYear(Element sourceSchoolAcademicYearElement) {
            if (sourceSchoolAcademicYearElement != null) {
                this.sourceSchoolAcademicYear = sourceSchoolAcademicYearElement.getTextTrim();
            }
        }

        /**
         * Gets the destSchoolLEA.
         *
         * @return String
         */
        public String getDestSchoolLEA() {
            return destSchoolLEA;
        }

        /**
         * Sets the destSchoolLEA.
         *
         * @param destSchoolLEA void
         */
        public void setDestSchoolLEA(String destSchoolLEA) {
            this.destSchoolLEA = destSchoolLEA;
        }

        /**
         * Sets the destSchoolLEA from a DfE XML Element.
         *
         * @param destSchoolLEAElement void
         */
        public void setDestSchoolLEA(Element destSchoolLEAElement) {
            if (destSchoolLEAElement != null) {
                this.destSchoolLEA = destSchoolLEAElement.getTextTrim();
            }
        }

        /**
         * Gets the destSchoolEstab.
         *
         * @return String
         */
        public String getDestSchoolEstab() {
            return destSchoolEstab;
        }

        /**
         * Sets the destSchoolEstab.
         *
         * @param destSchoolEstab void
         */
        public void setDestSchoolEstab(String destSchoolEstab) {
            this.destSchoolEstab = destSchoolEstab;
        }

        /**
         * Sets the destSchoolEstab from a DfE XML Element.
         *
         * @param destSchoolEstabElement void
         */
        public void setDestSchoolEstab(Element destSchoolEstabElement) {
            if (destSchoolEstabElement != null) {
                this.destSchoolEstab = destSchoolEstabElement.getTextTrim();
            }
        }

        /**
         * Gets the suppID.
         *
         * @return String
         */
        public String getSuppID() {
            return suppID;
        }

        /**
         * Sets the suppID.
         *
         * @param suppID void
         */
        public void setSuppID(String suppID) {
            this.suppID = suppID;
        }

        /**
         * Sets the suppID from a DfE XML Element.
         *
         * @param suppIDElement void
         */
        public void setSuppID(Element suppIDElement) {
            if (suppIDElement != null) {
                this.suppID = suppIDElement.getTextTrim();
            }
        }

    }

    /**
     * The Class DfELanguage.
     */
    class DfELanguage {
        public static final String ELEMENT_LANGUAGES = "Languages";
        public static final String ELEMENT_TYPE = "Type";
        public static final String ELEMENT_LANGUAGE_TYPE = "LanguageType";
        public static final String ELEMENT_LANGUAGE_CODE = "Language";

        public static final String DEFAULT_LANGUAGE_TYPE = "F"; // First

        private String languageType = null;
        private String languageCode = null;

        /**
         * Constructor for DfE (UK Department for Education) Language Object.
         *
         * @param languageElement Element
         */
        public DfELanguage(Element languageElement) {
            setLanguageType(languageElement.getChild(ELEMENT_LANGUAGE_TYPE));
            setLanguageCode(languageElement.getChild(ELEMENT_LANGUAGE_CODE));
        }

        /**
         * Constructor for DfE (UK Department for Education) Language Object.
         *
         * @param languageType String
         * @param languageCode String
         */
        public DfELanguage(String languageType, String languageCode) {
            setLanguageType(languageType);
            setLanguageCode(languageCode);
        }

        /**
         * Gets the languageType.
         *
         * @return String
         */
        public String getLanguageType() {
            return languageType;
        }

        /**
         * Sets the languageType.
         *
         * @param languageType void
         */
        public void setLanguageType(String languageType) {
            this.languageType = languageType;
        }

        /**
         * Sets the languageType from a DfE XML Element.
         *
         * @param languageTypeElement void
         */
        public void setLanguageType(Element languageTypeElement) {
            if (languageTypeElement != null) {
                this.languageType = languageTypeElement.getTextTrim();
            }
        }

        /**
         * Gets the languageCode.
         *
         * @return String
         */
        public String getLanguageCode() {
            return languageCode;
        }

        /**
         * Sets the languageCode.
         *
         * @param languageCode void
         */
        public void setLanguageCode(String languageCode) {
            this.languageCode = languageCode;
        }

        /**
         * Sets the languageCode from a DfE XML Element.
         *
         * @param languageCodeElement void
         */
        public void setLanguageCode(Element languageCodeElement) {
            if (languageCodeElement != null) {
                this.languageCode = languageCodeElement.getTextTrim();
            }
        }

    }

    /**
     * The Class DfENAWDetail.
     */
    public class DfENAWDetail {
        public static final String ELEMENT_NAW_DETAILS = "NAWdetails";
        public static final String ELEMENT_SPEAK_WELSH = "SpeakWelsh";
        public static final String ELEMENT_HOME_WELSH = "HomeWelsh";
        public static final String ELEMENT_NATIONAL_IDENTITY = "NationalIdentity";
        public static final String ELEMENT_WELSH_SOURCE = "WelshSource";
        public static final String ELEMENT_LANGAUGE_SOURCE = "LanguageSource";
        public static final String ELEMENT_EAL_ACQUISITION = "EALAcquisition";
        public static final String ELEMENT_SEN_CURR_TEACHING_METHODS = "SENCurriculumandTeachingMethods";
        public static final String ELEMENT_SEN_GROUPINGS_SUPPORT = "SENGroupingandSupport";
        public static final String ELEMENT_SEN_SPECIALISED_RESOURCES = "SENSpecialisedResources";
        public static final String ELEMENT_SEN_ADVICE_ASSESSMENT = "SENAdviceandAssessment";

        private String speakWelsh = null;
        private String homeWelsh = null;
        private String nationalIdentity = null;
        private String welshSource = null;
        private String languageSource = null;
        private String eALAcquisition = null;
        private String sENCurrTeachingMethods = null;
        private String sENGroupingAndSupport = null;
        private String sENSpecialisedResources = null;
        private String sENAdviceAndAssessment = null;

        /**
         * Constructor for DfE NAWDetail Object.
         *
         * @param nAWDetailElement Element
         */
        public DfENAWDetail(Element nAWDetailElement) {
            setSpeakWelsh(nAWDetailElement.getChild(ELEMENT_SPEAK_WELSH));
            setHomeWelsh(nAWDetailElement.getChild(ELEMENT_HOME_WELSH));
            setNationalIdentity(nAWDetailElement.getChild(ELEMENT_NATIONAL_IDENTITY));
            setWelshSource(nAWDetailElement.getChild(ELEMENT_WELSH_SOURCE));
            setLanguageSource(nAWDetailElement.getChild(ELEMENT_LANGAUGE_SOURCE));
            setEALAcquisition(nAWDetailElement.getChild(ELEMENT_EAL_ACQUISITION));
            setSENCurrTeachingMethods(nAWDetailElement.getChild(ELEMENT_SEN_CURR_TEACHING_METHODS));
            setSENGroupingAndSupport(nAWDetailElement.getChild(ELEMENT_SEN_GROUPINGS_SUPPORT));
            setSENSpecialisedResources(nAWDetailElement.getChild(ELEMENT_SEN_SPECIALISED_RESOURCES));
            setSENAdviceAndAssessment(nAWDetailElement.getChild(ELEMENT_SEN_ADVICE_ASSESSMENT));
        }

        /**
         * Constructor for DfE NAWDetail Object.
         *
         * @param speakWelsh String
         * @param homeWelsh String
         * @param nationalIdentity String
         * @param welshSource String
         * @param languageSource String
         * @param eALAcquisition String
         * @param sENCurrTeachingMethods String
         * @param sENGroupingandSupport String
         * @param sENSpecialisedResources String
         * @param sENAdviceandAssessment String
         */
        public DfENAWDetail(String speakWelsh, String homeWelsh, String nationalIdentity, String welshSource,
                String languageSource, String eALAcquisition, String sENCurrTeachingMethods,
                String sENGroupingandSupport, String sENSpecialisedResources, String sENAdviceandAssessment) {
            setSpeakWelsh(speakWelsh);
            setHomeWelsh(homeWelsh);
            setNationalIdentity(nationalIdentity);
            setWelshSource(welshSource);
            setLanguageSource(languageSource);
            setEALAcquisition(eALAcquisition);
            setSENCurrTeachingMethods(sENCurrTeachingMethods);
            setSENGroupingAndSupport(sENGroupingandSupport);
            setSENSpecialisedResources(sENSpecialisedResources);
            setSENAdviceAndAssessment(sENAdviceandAssessment);
        }


        /**
         * Gets the speakWelsh.
         *
         * @return String
         */
        public String getSpeakWelsh() {
            return speakWelsh;
        }

        /**
         * Sets the speakWelsh.
         *
         * @param speakWelsh void
         */
        public void setSpeakWelsh(String speakWelsh) {
            this.speakWelsh = speakWelsh;
        }

        /**
         * Sets the speakWelsh from a DfE XML Element.
         *
         * @param speakWelshElement void
         */
        public void setSpeakWelsh(Element speakWelshElement) {
            if (speakWelshElement != null) {
                this.speakWelsh = speakWelshElement.getTextTrim();
            }
        }

        /**
         * Gets the homeWelsh.
         *
         * @return String
         */
        public String getHomeWelsh() {
            return homeWelsh;
        }

        /**
         * Sets the homeWelsh.
         *
         * @param homeWelsh void
         */
        public void setHomeWelsh(String homeWelsh) {
            this.homeWelsh = homeWelsh;
        }

        /**
         * Sets the homeWelsh from a DfE XML Element.
         *
         * @param homeWelshElement void
         */
        public void setHomeWelsh(Element homeWelshElement) {
            if (homeWelshElement != null) {
                this.homeWelsh = homeWelshElement.getTextTrim();
            }
        }

        /**
         * Gets the nationalIdentity.
         *
         * @return String
         */
        public String getNationalIdentity() {
            return nationalIdentity;
        }

        /**
         * Sets the nationalIdentity.
         *
         * @param nationalIdentity void
         */
        public void setNationalIdentity(String nationalIdentity) {
            this.nationalIdentity = nationalIdentity;
        }

        /**
         * Sets the nationalIdentity from a DfE XML Element.
         *
         * @param nationalIdentityElement void
         */
        public void setNationalIdentity(Element nationalIdentityElement) {
            if (nationalIdentityElement != null) {
                this.nationalIdentity = nationalIdentityElement.getTextTrim();
            }
        }

        /**
         * Gets the welshSource.
         *
         * @return String
         */
        public String getWelshSource() {
            return welshSource;
        }

        /**
         * Sets the welshSource.
         *
         * @param welshSource void
         */
        public void setWelshSource(String welshSource) {
            this.welshSource = welshSource;
        }

        /**
         * Sets the welshSource from a DfE XML Element.
         *
         * @param welshSourceElement void
         */
        public void setWelshSource(Element welshSourceElement) {
            if (welshSourceElement != null) {
                this.welshSource = welshSourceElement.getTextTrim();
            }
        }

        /**
         * Gets the languageSource.
         *
         * @return String
         */
        public String getLanguageSource() {
            return languageSource;
        }

        /**
         * Sets the languageSource.
         *
         * @param languageSource void
         */
        public void setLanguageSource(String languageSource) {
            this.languageSource = languageSource;
        }

        /**
         * Sets the languageSource from a DfE XML Element.
         *
         * @param languageSourceElement void
         */
        public void setLanguageSource(Element languageSourceElement) {
            if (languageSourceElement != null) {
                this.languageSource = languageSourceElement.getTextTrim();
            }
        }

        /**
         * Gets the eALAcquisition.
         *
         * @return String
         */
        public String getEALAcquisition() {
            return eALAcquisition;
        }

        /**
         * Sets the eALAcquisition.
         *
         * @param eALAcquisition void
         */
        public void setEALAcquisition(String eALAcquisition) {
            this.eALAcquisition = eALAcquisition;
        }

        /**
         * Sets the eALAcquisition from a DfE XML Element.
         *
         * @param eALAcquisitionElement void
         */
        public void setEALAcquisition(Element eALAcquisitionElement) {
            if (eALAcquisitionElement != null) {
                this.eALAcquisition = eALAcquisitionElement.getTextTrim();
            }
        }

        /**
         * Gets the sENCurrTeachingMethods.
         *
         * @return String
         */
        public String getSENCurrTeachingMethods() {
            return sENCurrTeachingMethods;
        }

        /**
         * Sets the sENCurrTeachingMethods.
         *
         * @param sENCurrTeachingMethods void
         */
        public void setSENCurrTeachingMethods(String sENCurrTeachingMethods) {
            this.sENCurrTeachingMethods = sENCurrTeachingMethods;
        }

        /**
         * Sets the sENCurrTeachingMethods from a DfE XML Element.
         *
         * @param sENCurrTeachingMethodsElement void
         */
        public void setSENCurrTeachingMethods(Element sENCurrTeachingMethodsElement) {
            if (sENCurrTeachingMethodsElement != null) {
                this.sENCurrTeachingMethods = sENCurrTeachingMethodsElement.getTextTrim();
            }
        }

        /**
         * Gets the sENGroupingAndSupport.
         *
         * @return String
         */
        public String getSENGroupingAndSupport() {
            return sENGroupingAndSupport;
        }

        /**
         * Sets the sENGroupingAndSupport.
         *
         * @param sENGroupingAndSupport void
         */
        public void setSENGroupingAndSupport(String sENGroupingAndSupport) {
            this.sENGroupingAndSupport = sENGroupingAndSupport;
        }

        /**
         * Sets the sENGroupingAndSupport from a DfE XML Element.
         *
         * @param sENGroupingAndSupportElement void
         */
        public void setSENGroupingAndSupport(Element sENGroupingAndSupportElement) {
            if (sENGroupingAndSupportElement != null) {
                this.sENGroupingAndSupport = sENGroupingAndSupportElement.getTextTrim();
            }
        }

        /**
         * Gets the sENSpecialisedResources.
         *
         * @return String
         */
        public String getSENSpecialisedResources() {
            return sENSpecialisedResources;
        }

        /**
         * Sets the sENSpecialisedResources.
         *
         * @param sENSpecialisedResources void
         */
        public void setSENSpecialisedResources(String sENSpecialisedResources) {
            this.sENSpecialisedResources = sENSpecialisedResources;
        }

        /**
         * Sets the sENSpecialisedResources from a DfE XML Element.
         *
         * @param sENSpecialisedResourcesElement void
         */
        public void setSENSpecialisedResources(Element sENSpecialisedResourcesElement) {
            if (sENSpecialisedResourcesElement != null) {
                this.sENSpecialisedResources = sENSpecialisedResourcesElement.getTextTrim();
            }
        }

        /**
         * Gets the sENAdviceAndAssessment.
         *
         * @return String
         */
        public String getSENAdviceAndAssessment() {
            return sENAdviceAndAssessment;
        }

        /**
         * Sets the sENAdviceAndAssessment.
         *
         * @param sENAdviceAndAssessment void
         */
        public void setSENAdviceAndAssessment(String sENAdviceAndAssessment) {
            this.sENAdviceAndAssessment = sENAdviceAndAssessment;
        }

        /**
         * Sets the sENAdviceAndAssessment from a DfE XML Element.
         *
         * @param sENAdviceAndAssessmentElement void
         */
        public void setSENAdviceAndAssessment(Element sENAdviceAndAssessmentElement) {
            if (sENAdviceAndAssessmentElement != null) {
                this.sENAdviceAndAssessment = sENAdviceAndAssessmentElement.getTextTrim();
            }
        }

    }


    /**
     * The Class DfEPupil.
     */
    class DfEPupil {
        public static final String ELEMENT_CTF_PUPIL_DATA = "CTFpupilData";
        public static final String ELEMENT_ATF_PUPIL_DATA = "ATFpupilData";
        public static final String ELEMENT_PUPIL = "Pupil";
        public static final String ELEMENT_APPLICATION_REFERENCE = "ApplicationReference";
        public static final String ELEMENT_UNIQUE_PUPIL_NUMBER = "UPN";
        public static final String ELEMENT_UNIQUE_LEARNER_NUMBER = "UniqueLearnerNumber";
        public static final String ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER = "UCI";
        public static final String ELEMENT_SURNAME = "Surname";
        public static final String ELEMENT_FORENAME = "Forename";
        public static final String ELEMENT_DATE_OF_BIRTH = "DOB";
        public static final String ELEMENT_GENDER = "Gender";
        public static final String ELEMENT_BASIC_DETAILS = "BasicDetails";
        public static final String ELEMENT_FORMER_UNIQUE_PUPIL_NUMBER = "FormerUPN";
        public static final String ELEMENT_PREFERRED_SURNAME = "PreferredSurname";
        public static final String ELEMENT_PREFERRED_FORENAME = "PreferredForename";
        public static final String ELEMENT_FORMER_SURNAME = "FormerSurname";
        public static final String ELEMENT_FORMER_FORENAME = "FormerForename";
        public static final String ELEMENT_MIDDLE_NAMES = "MiddleNames";
        public static final String ELEMENT_NC_YEAR_ACTUAL = "NCyearActual";
        public static final String ELEMENT_ETHINICITY = "Ethnicity";
        public static final String ELEMENT_ETHINICITY_SOURCE = "EthnicitySource";
        public static final String ELEMENT_ENROLL_STATUS = "EnrolStatus";
        public static final String ELEMENT_NAW_DETAILS = "NAWdetails";
        public static final String ELEMENT_LOOK_AFTER = "LookedAfter";
        public static final String ELEMENT_IN_CARE = "InCare";
        public static final String ELEMENT_CARE_AUTHORITY = "CareAuthority";
        public static final String ELEMENT_LANGUAGES = "Languages";
        public static final String ELEMENT_TYPE = "Type";
        public static final String ELEMENT_MEDICAL_FLAG = "MedicalFlag";
        public static final String ELEMENT_DISABILITIES = "Disabilities";
        public static final String ELEMENT_DISABILITY = "Disability";
        public static final String ELEMENT_FSM_HISTORY = "FSMhistory";
        public static final String ELEMENT_FSM_REVIEW_DATE = "FSMreviewDate";
        public static final String ELEMENT_FSM_INSTANCE = "FSMinstance";
        public static final String ELEMENT_FSM_START_DATE = "FSMstartDate";
        public static final String ELEMENT_FSM_END_DATE = "FSMendDate";
        public static final String ELEMENT_FSM_UK_COUNTRY = "UKcountry";
        public static final String ELEMENT_SEN_HISTORY = "SENhistory";
        public static final String ELEMENT_SEN = "SEN";
        public static final String ELEMENT_SEN_START_DATE = "StartDate";
        public static final String ELEMENT_SEN_PROVISION = "SENprovision";
        public static final String ELEMENT_SEN_NEEDS = "SENneeds";
        public static final String ELEMENT_SEN_NEED = "SENneed";
        public static final String ELEMENT_ADMISSIONS = "Admissions";
        public static final String ELEMENT_ACCEPT = "Accept";
        public static final String ELEMENT_ADDRESS = "Address";
        public static final String ELEMENT_PHONES = "Phones";
        public static final String ELEMENT_PHONE = "Phone";
        public static final String ELEMENT_EMAIL = "Email";
        public static final String ELEMENT_CONTACTS = "Contacts";
        public static final String ELEMENT_CONTACT = "Contact";
        public static final String ELEMENT_ATTENDANCE = "Attendance";
        public static final String ELEMENT_YEAR_DATA = "YearData";
        public static final String ELEMENT_STAGE_ASSESSMENTS = "StageAssessments";
        public static final String ELEMENT_KEY_STAGE = "KeyStage";
        public static final String ELEMENT_STAGE = "Stage";
        public static final String ELEMENT_STAGE_ASSESSMENT = "StageAssessment";
        public static final String ELEMENT_SCHOOL_HISTORY = "SchoolHistory";
        public static final String ELEMENT_SCHOOL = "School";

        public static final String DEFAULT_ETHNICITY_SOURCE = "C";
        public static final String DEFAULT_ENROLL_STATUS = "C";
        public final Boolean DEFAULT_MEDICAL_FLAG = Boolean.valueOf(false);

        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

        // Pupil variables
        private String applicationReference = null;
        private String uniquePupilNumber = null;
        private String uniqueLearnerNumber = null;
        private String uniqueCandidateIdentifier = null;
        private String surname = null;
        private String forename = null;
        private PlainDate birthDate = null;
        private String gender = null;
        private String formerUniquePupilNumber = null;
        private String preferredSurname = null;
        private String preferredForename = null;
        private String formerSurname = null;
        private String formerForename = null;
        private String middleNames = null;
        private String nCYearActual = null;
        private String ethnicity = null;
        private String ethnicitySource = null;
        private ArrayList<DfELanguage> languages = new ArrayList();
        private Boolean medicalFlag = null;
        private ArrayList<String> disabilities = new ArrayList();
        private String enrollStatus = null;
        private PlainDate fSMReviewDate = null;
        private Boolean inCare = null;
        private String careAuthority = null;
        private PlainDate sENStartDate = null;
        private String sENProvision = null;
        private ArrayList<DfEFSMInstance> fSMInstances = new ArrayList();
        private ArrayList<DfENAWDetail> nAWDetails = new ArrayList();
        private ArrayList<DfESENNeed> sENNeeds = new ArrayList();
        private ArrayList<DfEAttendance> studentAttendances = new ArrayList();
        private ArrayList<DfESchoolHistory> schoolHistories = new ArrayList();
        private ArrayList<DfEStageAssessment> stageAssessments = new ArrayList();
        private DfEAddress dfEAddress = null;
        private ArrayList<DfEContact> contacts = new ArrayList();
        private ArrayList<DfETelephone> telephones = new ArrayList();
        private String email = null;
        private String attendanceXML = null;
        private String assessmentsXML = null;
        private String schoolHistoryXML = null;

        /**
         * Constructor for DfE (UK Department for Education) Pupil Object
         *
         * Convert DfE Pupil XML Element to DfEPupil Object
         * Used in CTF/ATF Import.
         *
         * @param pupilElement Element
         */
        public DfEPupil(Element pupilElement) {
            setApplicationReference(pupilElement.getChild(ELEMENT_APPLICATION_REFERENCE));
            setUniquePupilNumber(pupilElement.getChild(ELEMENT_UNIQUE_PUPIL_NUMBER));
            setUniqueLearnerNumber(pupilElement.getChild(ELEMENT_UNIQUE_LEARNER_NUMBER));
            setUniqueCandidateIdentifier(pupilElement.getChild(ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER));
            setForename(pupilElement.getChild(ELEMENT_FORENAME));
            setSurname(pupilElement.getChild(ELEMENT_SURNAME));
            setBirthDate(pupilElement.getChild(ELEMENT_DATE_OF_BIRTH));
            setGender(pupilElement.getChild(ELEMENT_GENDER));

            Element basicDetailsElement = pupilElement.getChild(ELEMENT_BASIC_DETAILS);
            if (basicDetailsElement != null) {
                setFormerUniquePupilNumber(basicDetailsElement.getChild(ELEMENT_FORMER_UNIQUE_PUPIL_NUMBER));
                setPreferredSurname(basicDetailsElement.getChild(ELEMENT_PREFERRED_SURNAME));
                setPreferredForename(basicDetailsElement.getChild(ELEMENT_PREFERRED_FORENAME));
                setFormerSurname(basicDetailsElement.getChild(ELEMENT_FORMER_SURNAME));
                setFormerForename(basicDetailsElement.getChild(ELEMENT_FORMER_FORENAME));
                setMiddleNames(basicDetailsElement.getChild(ELEMENT_MIDDLE_NAMES));
                setNCyearActual(basicDetailsElement.getChild(ELEMENT_NC_YEAR_ACTUAL));
                setEthnicity(basicDetailsElement.getChild(ELEMENT_ETHINICITY));
                setEthnicitySource(basicDetailsElement.getChild(ELEMENT_ETHINICITY_SOURCE));

                Element languagesElement = basicDetailsElement.getChild(ELEMENT_LANGUAGES);
                if (languagesElement != null) {
                    List<Element> languageElementList = languagesElement.getChildren(ELEMENT_TYPE);
                    for (int j = 0; j < languageElementList.size(); j++) {
                        Element languageElement = languageElementList.get(j);
                        DfELanguage dfELanguage = new DfELanguage(languageElement);
                        languages.add(dfELanguage);
                    }
                }

                setEnrollStatus(basicDetailsElement.getChild(ELEMENT_ENROLL_STATUS));
                setMedicalFlag(basicDetailsElement.getChild(ELEMENT_MEDICAL_FLAG));

                Element disabiliesElement = basicDetailsElement.getChild(ELEMENT_DISABILITIES);
                if (disabiliesElement != null) {
                    List<Element> disabilityElementList = disabiliesElement.getChildren(ELEMENT_DISABILITY);
                    for (int l = 0; l < disabilityElementList.size(); l++) {
                        Element disabilityElement = disabilityElementList.get(l);
                        if (disabilityElement != null) {
                            String disability = disabilityElement.getTextTrim();
                            disabilities.add(disability);
                        }
                    }
                }
            }

            // Look After
            Element lookAfterElement = pupilElement.getChild(ELEMENT_LOOK_AFTER);
            if (lookAfterElement != null) {
                setInCare(lookAfterElement.getChild(ELEMENT_IN_CARE));
                setCareAuthority(lookAfterElement.getChild(ELEMENT_CARE_AUTHORITY));
            }

            // Phone
            Element phonesElement = pupilElement.getChild(ELEMENT_PHONES);
            if (phonesElement != null) {
                List<Element> phonesList = phonesElement.getChildren(ELEMENT_PHONE);
                for (int k = 0; k < phonesList.size(); k++) {
                    Element telephoneElement = phonesList.get(k);
                    DfETelephone dfETelephone = new DfETelephone(telephoneElement);
                    telephones.add(dfETelephone);
                }
            }
            setEmail(pupilElement.getChild(ELEMENT_EMAIL));

            // FSM
            Element fSMHistoryElement = pupilElement.getChild(ELEMENT_FSM_HISTORY);
            if (fSMHistoryElement != null) {
                Element fsmReviewDateElement = fSMHistoryElement.getChild(ELEMENT_FSM_REVIEW_DATE);
                setFSMReviewDate(fsmReviewDateElement);

                List<Element> fsmInstancesElementList = fSMHistoryElement.getChildren(ELEMENT_FSM_INSTANCE);
                if (fsmInstancesElementList.size() > 0) {
                    for (int m = 0; m < fsmInstancesElementList.size(); m++) {
                        Element fsmInstanceElement = fsmInstancesElementList.get(m);
                        DfEFSMInstance dfEFSMInstance = new DfEFSMInstance(fsmInstanceElement);
                        fSMInstances.add(dfEFSMInstance);
                    }
                }
            }

            // NAWDetail
            List<Element> nAWDetailsElementList = pupilElement.getChildren(ELEMENT_NAW_DETAILS);
            if (nAWDetailsElementList.size() > 0) {
                for (int n = 0; n < nAWDetailsElementList.size(); n++) {
                    Element nAWDetailsElement = nAWDetailsElementList.get(n);
                    DfENAWDetail dfENAWDetail = new DfENAWDetail(nAWDetailsElement);
                    nAWDetails.add(dfENAWDetail);
                }
            }

            // SEN
            Element sENHistoryElement = pupilElement.getChild(ELEMENT_SEN_HISTORY);
            if (sENHistoryElement != null) {
                Element sENElement = sENHistoryElement.getChild(ELEMENT_SEN);
                Element sENStartDateElement = sENElement.getChild(ELEMENT_SEN_START_DATE);
                setSENStartDate(sENStartDateElement);
                Element sENProvisionElement = sENElement.getChild(ELEMENT_SEN_PROVISION);
                setSENProvision(sENProvisionElement);

                Element sENNeedsElement = sENHistoryElement.getChild(ELEMENT_SEN_NEEDS);
                if (sENNeedsElement != null) {
                    List<Element> sENNeedsElementList = sENNeedsElement.getChildren(ELEMENT_SEN_NEED);
                    for (int k = 0; k < sENNeedsElementList.size(); k++) {
                        Element sENNeedElement = sENNeedsElementList.get(k);
                        DfESENNeed dfESENNeed = new DfESENNeed(sENNeedElement);
                        sENNeeds.add(dfESENNeed);
                    }
                }
            }

            // Address
            Element addressElement = pupilElement.getChild(ELEMENT_ADDRESS);
            if (addressElement != null) {
                setDfEAddress(new DfEAddress(addressElement));
            }

            // Contact
            Element contactsElement = pupilElement.getChild(ELEMENT_CONTACTS);
            if (contactsElement != null) {
                List<Element> contactsElementList = contactsElement.getChildren(ELEMENT_CONTACT);
                for (int l = 0; l < contactsElementList.size(); l++) {
                    Element contactElement = contactsElementList.get(l);
                    DfEContact dfEContact = new DfEContact(contactElement);
                    contacts.add(dfEContact);
                }
            }

            // Save all XML as text in the database.
            XMLOutputter xMLOutputter = new XMLOutputter();

            Element attendanceElement = pupilElement.getChild(ELEMENT_ATTENDANCE);
            if (attendanceElement != null) {
                // TODO Remove later
                String attendanceStr = xMLOutputter.outputString(attendanceElement);
                setAttendanceXML(attendanceStr);

                List<Element> yearDataList = attendanceElement.getChildren(ELEMENT_YEAR_DATA);
                for (int r = 0; r < yearDataList.size(); r++) {
                    Element yearDataElement = yearDataList.get(r);
                    DfEAttendance dfEAttendance = new DfEAttendance(yearDataElement);
                    studentAttendances.add(dfEAttendance);
                }
            }

            // Student Assessment
            Element assessmentsElement = pupilElement.getChild(ELEMENT_STAGE_ASSESSMENTS);
            if (assessmentsElement != null) {
                // TODO Remove later
                String assessmentsStr = xMLOutputter.outputString(assessmentsElement);
                setAssessmentsXML(assessmentsStr);

                List<Element> keyStageList = assessmentsElement.getChildren(ELEMENT_KEY_STAGE);
                for (int p = 0; p < keyStageList.size(); p++) {
                    Element keyStageElement = keyStageList.get(p);

                    Element stageElement = keyStageElement.getChild(ELEMENT_STAGE);
                    String stage = stageElement.getTextTrim();

                    List<Element> stageAssessmentList = keyStageElement.getChildren(ELEMENT_STAGE_ASSESSMENT);
                    for (int q = 0; q < stageAssessmentList.size(); q++) {
                        Element stageAssessmentElement = stageAssessmentList.get(q);

                        DfEStageAssessment dfEStageAssessment = new DfEStageAssessment(stage, stageAssessmentElement);
                        stageAssessments.add(dfEStageAssessment);
                    }
                }
            }

            // School History
            Element schoolHistoryElement = pupilElement.getChild(ELEMENT_SCHOOL_HISTORY);
            if (schoolHistoryElement != null) {
                // TODO Remove later
                String schoolHistoryStr = xMLOutputter.outputString(schoolHistoryElement);
                setSchoolHistoryXML(schoolHistoryStr);

                List<Element> schoolList = schoolHistoryElement.getChildren(ELEMENT_SCHOOL);
                for (int m = 0; m < schoolList.size(); m++) {
                    Element schoolElement = schoolList.get(m);
                    DfESchoolHistory dfESchoolHistory = new DfESchoolHistory(schoolElement);
                    schoolHistories.add(dfESchoolHistory);
                }
            }

        }


        /**
         * Constructor for DfE (UK Department for Education) Pupil Object.
         */
        public DfEPupil() {}


        /**
         * Gets the uniquePupilNumber.
         *
         * @return String
         */
        public String getUniquePupilNumber() {
            return uniquePupilNumber;
        }

        /**
         * Sets the uniquePupilNumber.
         *
         * @param uniquePupilNumber void
         */
        public void setUniquePupilNumber(String uniquePupilNumber) {
            this.uniquePupilNumber = uniquePupilNumber;
        }

        /**
         * Sets the uniquePupilNumber from a DfE XML Element.
         *
         * @param uniquePupilNumberElement void
         */
        public void setUniquePupilNumber(Element uniquePupilNumberElement) {
            if (uniquePupilNumberElement != null) {
                this.uniquePupilNumber = uniquePupilNumberElement.getTextTrim();
            }
        }

        /**
         * Gets the applicationReference.
         *
         * @return String
         */
        public String getApplicationReference() {
            return applicationReference;
        }

        /**
         * Sets the applicationReference.
         *
         * @param applicationReference void
         */
        public void setApplicationReference(String applicationReference) {
            this.applicationReference = applicationReference;
        }

        /**
         * Sets the uniquePupilNumber from a DfE XML Element.
         *
         * @param applicationReferenceElement void
         */
        public void setApplicationReference(Element applicationReferenceElement) {
            if (applicationReferenceElement != null) {
                this.applicationReference = applicationReferenceElement.getTextTrim();
            }
        }

        /**
         * Gets the uniqueLearnerNumber.
         *
         * @return String
         */
        public String getUniqueLearnerNumber() {
            return uniqueLearnerNumber;
        }

        /**
         * Sets the uniqueLearnerNumber.
         *
         * @param uniqueLearnerNumber void
         */
        public void setUniqueLearnerNumber(String uniqueLearnerNumber) {
            this.uniqueLearnerNumber = uniqueLearnerNumber;
        }

        /**
         * Sets the uniqueLearnerNumber from a DfE XML Element.
         *
         * @param uniqueLearnerNumberElement void
         */
        public void setUniqueLearnerNumber(Element uniqueLearnerNumberElement) {
            if (uniqueLearnerNumberElement != null) {
                this.uniqueLearnerNumber = uniqueLearnerNumberElement.getTextTrim();
            }
        }

        /**
         * Gets the uniqueCandidateNumber.
         *
         * @return String
         */
        public String getUniqueCandidateIdentifier() {
            return uniqueCandidateIdentifier;
        }

        /**
         * Sets the uniqueCandidateNumber.
         *
         * @param uniqueCandidateIdentifier void
         */
        public void setUniqueCandidateIdentifier(String uniqueCandidateIdentifier) {
            this.uniqueCandidateIdentifier = uniqueCandidateIdentifier;
        }

        /**
         * Sets the uniqueCandidateIdentifier from a DfE XML Element.
         *
         * @param uniqueCandidateIdentifierElement void
         */
        public void setUniqueCandidateIdentifier(Element uniqueCandidateIdentifierElement) {
            if (uniqueCandidateIdentifierElement != null) {
                this.uniqueCandidateIdentifier = uniqueCandidateIdentifierElement.getTextTrim();
            }
        }

        /**
         * Gets the surname.
         *
         * @return String
         */
        public String getSurname() {
            return surname;
        }

        /**
         * Sets the surname.
         *
         * @param surname void
         */
        public void setSurname(String surname) {
            this.surname = surname;
        }

        /**
         * Sets the surname from a DfE XML Element.
         *
         * @param surnameElement void
         */
        public void setSurname(Element surnameElement) {
            if (surnameElement != null) {
                this.surname = surnameElement.getTextTrim();
            }
        }

        /**
         * Gets the forename.
         *
         * @return String
         */
        public String getForename() {
            return forename;
        }

        /**
         * Sets the forename.
         *
         * @param forename void
         */
        public void setForename(String forename) {
            this.forename = forename;
        }

        /**
         * Sets the forename from a DfE XML Element.
         *
         * @param forenameElement void
         */
        public void setForename(Element forenameElement) {
            if (forenameElement != null) {
                this.forename = forenameElement.getTextTrim();
            }
        }

        /**
         * Gets the birthDate.
         *
         * @return PlainDate
         */
        public PlainDate getBirthDate() {
            return birthDate;
        }

        /**
         * Sets the birthDate.
         *
         * @param birthDate void
         */
        public void setBirthDate(PlainDate birthDate) {
            this.birthDate = birthDate;
        }

        /**
         * Sets the birthDate from a DfE XML Element.
         *
         * @param birthDateElement void
         */
        public void setBirthDate(Element birthDateElement) {
            if (birthDateElement != null) {
                String birthDateStr = birthDateElement.getTextTrim();
                if (!StringUtils.isEmpty(birthDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(birthDateStr);
                        this.birthDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.birthDate = null;
                    }
                }
            }
        }

        /**
         * Gets the gender.
         *
         * @return String
         */
        public String getGender() {
            return gender;
        }

        /**
         * Sets the gender.
         *
         * @param gender void
         */
        public void setGender(String gender) {
            this.gender = gender;
        }

        /**
         * Sets the gender from a DfE XML Element.
         *
         * @param genderElement void
         */
        public void setGender(Element genderElement) {
            if (genderElement != null) {
                this.gender = genderElement.getTextTrim();
            }
        }

        /**
         * Gets the formerUniquePupilNumber.
         *
         * @return String
         */
        public String getFormerUniquePupilNumber() {
            return formerUniquePupilNumber;
        }

        /**
         * Sets the formerUniquePupilNumber.
         *
         * @param formerUniquePupilNumber void
         */
        public void setFormerUniquePupilNumber(String formerUniquePupilNumber) {
            this.formerUniquePupilNumber = formerUniquePupilNumber;
        }

        /**
         * Sets the formerUniquePupilNumber from a DfE XML Element.
         *
         * @param formerUniquePupilNumberElement void
         */
        public void setFormerUniquePupilNumber(Element formerUniquePupilNumberElement) {
            if (formerUniquePupilNumberElement != null) {
                this.formerUniquePupilNumber = formerUniquePupilNumberElement.getTextTrim();
            }
        }

        /**
         * Gets the preferredSurname.
         *
         * @return String
         */
        public String getPreferredSurname() {
            return preferredSurname;
        }

        /**
         * Sets the preferredSurname.
         *
         * @param preferredSurname void
         */
        public void setPreferredSurname(String preferredSurname) {
            this.preferredSurname = preferredSurname;
        }

        /**
         * Sets the preferredSurname from a DfE XML Element.
         *
         * @param preferredSurnameElement void
         */
        public void setPreferredSurname(Element preferredSurnameElement) {
            if (preferredSurnameElement != null) {
                this.preferredSurname = preferredSurnameElement.getTextTrim();
            }
        }

        /**
         * Gets the formerSurname.
         *
         * @return String
         */
        public String getFormerSurname() {
            return formerSurname;
        }

        /**
         * Sets the formerSurname.
         *
         * @param formerSurname void
         */
        public void setFormerSurname(String formerSurname) {
            this.formerSurname = formerSurname;
        }

        /**
         * Sets the formerSurname from a DfE XML Element.
         *
         * @param formerSurnameElement void
         */
        public void setFormerSurname(Element formerSurnameElement) {
            if (formerSurnameElement != null) {
                this.formerSurname = formerSurnameElement.getTextTrim();
            }
        }

        /**
         * Gets the formerForename.
         *
         * @return String
         */
        public String getFormerForename() {
            return formerForename;
        }

        /**
         * Sets the formerForename.
         *
         * @param formerForename void
         */
        public void setFormerForename(String formerForename) {
            this.formerForename = formerForename;
        }

        /**
         * Sets the formerForename from a DfE XML Element.
         *
         * @param formerForenameElement void
         */
        public void setFormerForename(Element formerForenameElement) {
            if (formerForenameElement != null) {
                this.formerForename = formerForenameElement.getTextTrim();
            }
        }

        /**
         * Gets the preferredForename.
         *
         * @return String
         */
        public String getPreferredForename() {
            return preferredForename;
        }

        /**
         * Sets the preferredForename.
         *
         * @param preferredForename void
         */
        public void setPreferredForename(String preferredForename) {
            this.preferredForename = preferredForename;
        }

        /**
         * Sets the preferredForename from a DfE XML Element.
         *
         * @param preferredForenameElement void
         */
        public void setPreferredForename(Element preferredForenameElement) {
            if (preferredForenameElement != null) {
                this.preferredForename = preferredForenameElement.getTextTrim();
            }
        }

        /**
         * Gets the middleNames.
         *
         * @return String
         */
        public String getMiddleNames() {
            return middleNames;
        }

        /**
         * Sets the middleNames.
         *
         * @param middleNames void
         */
        public void setMiddleNames(String middleNames) {
            this.middleNames = middleNames;
        }

        /**
         * Sets the middleNames from a DfE XML Element.
         *
         * @param middleNamesElement void
         */
        public void setMiddleNames(Element middleNamesElement) {
            if (middleNamesElement != null) {
                this.middleNames = middleNamesElement.getTextTrim();
            }
        }

        /**
         * Gets the NCYearActual.
         *
         * @return String
         */
        public String getNCYearActual() {
            return nCYearActual;
        }

        /**
         * Sets the NCYearActual.
         *
         * @param nCYearActual void
         */
        public void setNCYearActual(String nCYearActual) {
            this.nCYearActual = nCYearActual;
        }

        /**
         * Sets the NCYearActual from a DfE XML Element.
         *
         * @param nCYearActualElement void
         */
        public void setNCyearActual(Element nCYearActualElement) {
            if (nCYearActualElement != null) {
                this.nCYearActual = nCYearActualElement.getTextTrim();
            }
        }

        /**
         * Gets the ethnicity.
         *
         * @return String
         */
        public String getEthnicity() {
            return ethnicity;
        }

        /**
         * Sets the ethnicity.
         *
         * @param ethnicity void
         */
        public void setEthnicity(String ethnicity) {
            this.ethnicity = ethnicity;
        }

        /**
         * Sets the ethnicity from a DfE XML Element.
         *
         * @param ethnicityElement void
         */
        public void setEthnicity(Element ethnicityElement) {
            if (ethnicityElement != null) {
                this.ethnicity = ethnicityElement.getTextTrim();
            }
        }

        /**
         * Gets the ethnicitySource.
         *
         * @return String
         */
        public String getEthnicitySource() {
            return ethnicitySource;
        }

        /**
         * Sets the ethnicitySource.
         *
         * @param ethnicitySource void
         */
        public void setEthnicitySource(String ethnicitySource) {
            this.ethnicitySource = ethnicitySource;
        }

        /**
         * Sets the ethnicitySource from a DfE XML Element.
         *
         * @param ethnicitySourceElement void
         */
        public void setEthnicitySource(Element ethnicitySourceElement) {
            if (ethnicitySourceElement != null) {
                this.ethnicitySource = ethnicitySourceElement.getTextTrim();
            }
        }

        /**
         * Gets the first languageCode in the language List.
         *
         * @return String
         */
        public String getFirstLanguageCode() {
            String languageCode = null;
            if (languages != null && languages.size() > 0) {
                DfELanguage langauge = languages.get(0);
                languageCode = langauge.getLanguageCode();
            }
            return languageCode;
        }

        /**
         * Add to the languages collection.
         *
         * @param dfELangage DfELanguage
         */
        public void addDfELanguage(DfELanguage dfELangage) {
            languages.add(dfELangage);
        }

        /**
         * Gets the fSMReviewDate.
         *
         * @return PlainDate
         */
        public PlainDate getFSMReviewDate() {
            return fSMReviewDate;
        }

        /**
         * Sets the fSMReviewDate.
         *
         * @param fSMReviewDate void
         */
        public void setFSMReviewDate(PlainDate fSMReviewDate) {
            this.fSMReviewDate = fSMReviewDate;
        }

        /**
         * Sets the fSMReviewDate from a DfE XML Element.
         *
         * @param fSMReviewDateElement void
         */
        public void setFSMReviewDate(Element fSMReviewDateElement) {
            if (fSMReviewDateElement != null) {
                String reviewDateStr = fSMReviewDateElement.getTextTrim();
                if (!StringUtils.isEmpty(reviewDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(reviewDateStr);
                        this.fSMReviewDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.fSMReviewDate = null;
                    }
                }
            }
        }

        /**
         * Sets the fSMReviewDate from a String Date.
         *
         * @param fSMReviewDateStr void
         */
        public void setFSMReviewDate(String fSMReviewDateStr) {
            if (fSMReviewDateStr != null) {
                fSMReviewDateStr = fSMReviewDateStr.trim();
                if (!StringUtils.isEmpty(fSMReviewDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(fSMReviewDateStr);
                        this.fSMReviewDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.fSMReviewDate = null;
                    }
                }
            }
        }

        /**
         * Gets the enrollStatus.
         *
         * @return String
         */
        public String getEnrollStatus() {
            return enrollStatus;
        }

        /**
         * Sets the enrollStatus.
         *
         * @param enrollStatus void
         */
        public void setEnrollStatus(String enrollStatus) {
            this.enrollStatus = enrollStatus;
        }

        /**
         * Sets the enrollStatus from a DfE XML Element.
         *
         * @param enrollStatusElement void
         */
        public void setEnrollStatus(Element enrollStatusElement) {
            if (enrollStatusElement != null) {
                this.enrollStatus = enrollStatusElement.getTextTrim();
            }
        }

        /**
         * Gets the email.
         *
         * @return String
         */
        public String getEmail() {
            return email;
        }

        /**
         * Sets the email.
         *
         * @param email void
         */
        public void setEmail(String email) {
            this.email = email;
        }

        /**
         * Sets the email from a DfE XML Element.
         *
         * @param emailElement void
         */
        public void setEmail(Element emailElement) {
            if (emailElement != null) {
                this.email = emailElement.getTextTrim();
            }
        }

        /**
         * Gets the languages list.
         *
         * @return ArrayList<DfELanguage>
         */
        public ArrayList<DfELanguage> getLanguages() {
            return languages;
        }

        /**
         * Gets the SENNeed list.
         *
         * @return ArrayList<SENNeed>
         */
        public ArrayList<DfESENNeed> getSENNeeds() {
            return sENNeeds;
        }

        /**
         * Adds a DfESENNeed to the Pupil's DfESENNeed Collection.
         *
         * @param dfESENNeed DfESENNeed
         */
        public void addSENNeed(DfESENNeed dfESENNeed) {
            sENNeeds.add(dfESENNeed);
        }

        /**
         * Gets the DfEFSMInstance list.
         *
         * @return ArrayList<DfEFSMInstance>
         */
        public ArrayList<DfEFSMInstance> getFSMInstances() {
            return fSMInstances;
        }

        /**
         * Adds a DfEFSMInstance to the Pupil's DfEFSMInstances Collection.
         *
         * @param dfEFSMInstance DfEFSMInstance
         */
        public void addFSMInstance(DfEFSMInstance dfEFSMInstance) {
            fSMInstances.add(dfEFSMInstance);
        }

        /**
         * Gets the nAWDetails list.
         *
         * @return ArrayList<DfENAWDetail>
         */
        public ArrayList<DfENAWDetail> getNAWDetails() {
            return nAWDetails;
        }

        /**
         * Adds a DfENAWDetail to the Pupil's nAWDetails Collection.
         *
         * @param dfENAWDetail DfENAWDetail
         */
        public void addNAWDetail(DfENAWDetail dfENAWDetail) {
            nAWDetails.add(dfENAWDetail);
        }

        /**
         * Gets the Pupil's DfEAddress.
         *
         * @return DfEAddress
         */
        public DfEAddress getDfEAddress() {
            return dfEAddress;
        }

        /**
         * Sets the dfEAddress.
         *
         * @param dfEAddress void
         */
        public void setDfEAddress(DfEAddress dfEAddress) {
            this.dfEAddress = dfEAddress;
        }

        /**
         * Gets the Pupil's Contacts list.
         *
         * @return ArrayList<DfEContact>
         */
        public ArrayList<DfEContact> getContacts() {
            return contacts;
        }

        /**
         * Adds a DfEContact to the Pupil's Contact Collection.
         *
         * @param dfEContact DfEContact
         */
        public void addContact(DfEContact dfEContact) {
            contacts.add(dfEContact);
        }

        /**
         * Gets the DfEAttendance list.
         *
         * @return ArrayList<DfEAttendance>
         */
        public ArrayList<DfEAttendance> getAttendances() {
            return studentAttendances;
        }

        /**
         * Adds all DfEAttendance's to the Pupil's studentAttendances Collection.
         *
         * @param attendance DfEAttendance
         */
        public void addAttendance(DfEAttendance attendance) {
            studentAttendances.add(attendance);
        }

        /**
         * Adds all DfEAttendance's to the Pupil's studentAttendances Collection.
         *
         * @param OldAttendances ArrayList<DfEAttendance>
         */
        public void addAllAttendances(ArrayList<DfEAttendance> OldAttendances) {
            studentAttendances.addAll(OldAttendances);
        }

        /**
         * Gets the DfESchoolHistory list.
         *
         * @return ArrayList<DfESchoolHistory>
         */
        public ArrayList<DfESchoolHistory> getSchoolHistories() {
            return schoolHistories;
        }

        /**
         * Adds a DfESchoolHistory to the Pupil's schoolHistories Collection.
         *
         * @param dfESchoolHistory DfESchoolHistory
         */
        public void addSchoolHistory(DfESchoolHistory dfESchoolHistory) {
            schoolHistories.add(dfESchoolHistory);
        }

        /**
         * Adds all DfESchoolHistory's to the Pupil's schoolHistories Collection.
         *
         * @param OldSchoolHistories ArrayList<DfESchoolHistory>
         */
        public void addAllSchoolHistory(ArrayList<DfESchoolHistory> OldSchoolHistories) {
            schoolHistories.addAll(OldSchoolHistories);
        }

        /**
         * Gets the DfEStageAssessment list.
         *
         * @return ArrayList<DfEStageAssessment>
         */
        public ArrayList<DfEStageAssessment> getStageAssessments() {
            return stageAssessments;
        }

        /**
         * Adds a DfEStageAssessment to the Pupil's stageAssessments Collection.
         *
         * @param dfEStageAssessment DfEStageAssessment
         */
        public void addStageAssessment(DfEStageAssessment dfEStageAssessment) {
            stageAssessments.add(dfEStageAssessment);
        }

        /**
         * Adds all DfEStageAssessment's to the Pupil's stageAssessments Collection.
         *
         * @param OldStageAssessments ArrayList<DfEStageAssessment>
         */
        public void addAllStageAssessment(ArrayList<DfEStageAssessment> OldStageAssessments) {
            stageAssessments.addAll(OldStageAssessments);
        }

        /**
         * Gets the sENStartDate.
         *
         * @return PlainDate
         */
        public PlainDate getSENStartDate() {
            return sENStartDate;
        }

        /**
         * Sets the sENStartDate.
         *
         * @param sENStartDate void
         */
        public void setSENStartDate(PlainDate sENStartDate) {
            this.sENStartDate = sENStartDate;
        }

        /**
         * Sets the SENStartDate from a DfE XML Element.
         *
         * @param sENStartDateElement void
         */
        public void setSENStartDate(Element sENStartDateElement) {
            if (sENStartDateElement != null) {
                String sENStartDateStr = sENStartDateElement.getTextTrim();
                if (!StringUtils.isEmpty(sENStartDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(sENStartDateStr);
                        this.sENStartDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.sENStartDate = null;
                    }
                }
            }
        }

        /**
         * Gets the ArrayList of DfETelephone for Pupil.
         *
         * @return ArrayList<DfETelephone>
         */
        public ArrayList<DfETelephone> getTelephones() {
            return telephones;
        }

        /**
         * Adds a DfETelephone to the Pupil's DfETelephone Collection.
         *
         * @param dfETelephone DfETelephone
         */
        public void addTelephone(DfETelephone dfETelephone) {
            telephones.add(dfETelephone);
        }

        /**
         * Gets the ArrayList of Disabilities for Pupil.
         *
         * @return ArrayList<String>
         */
        public ArrayList<String> getDisabilities() {
            return disabilities;
        }

        /**
         * Adds a disability to the Pupil's disability Collection.
         *
         * @param disability String
         */
        public void addDisability(String disability) {
            disabilities.add(disability);
        }

        /**
         * Gets the sENProvision.
         *
         * @return String
         */
        public String getSENProvision() {
            return sENProvision;
        }

        /**
         * Sets the sENProvision.
         *
         * @param sENProvision void
         */
        public void setSENProvision(String sENProvision) {
            this.sENProvision = sENProvision;
        }

        /**
         * Sets the sENProvision from a DfE XML Element.
         *
         * @param sENProvisionElement void
         */
        public void setSENProvision(Element sENProvisionElement) {
            if (sENProvisionElement != null) {
                this.sENProvision = sENProvisionElement.getTextTrim();
            }
        }

        /**
         * Gets the attendanceXML.
         *
         * @return String
         */
        public String getAttendanceXML() {
            return attendanceXML;
        }

        /**
         * Sets the attendanceXML.
         *
         * @param attendanceXML void
         */
        public void setAttendanceXML(String attendanceXML) {
            this.attendanceXML = attendanceXML;
        }

        /**
         * Gets the assessmentsXML.
         *
         * @return String
         */
        public String getAssessmentsXML() {
            return assessmentsXML;
        }

        /**
         * Sets the assessmentsXML.
         *
         * @param assessmentsXML void
         */
        public void setAssessmentsXML(String assessmentsXML) {
            this.assessmentsXML = assessmentsXML;
        }

        /**
         * Gets the schoolHistoryXML.
         *
         * @return String
         */
        public String getSchoolHistoryXML() {
            return schoolHistoryXML;
        }

        /**
         * Sets the schoolHistoryXML.
         *
         * @param schoolHistoryXML void
         */
        public void setSchoolHistoryXML(String schoolHistoryXML) {
            this.schoolHistoryXML = schoolHistoryXML;
        }

        /**
         * Gets the medicalFlag.
         *
         * @return String
         */
        public Boolean getMedicalFlag() {
            return medicalFlag;
        }

        /**
         * Sets the medicalFlag.
         *
         * @param medicalFlag void
         */
        public void setMedicalFlag(Boolean medicalFlag) {
            this.medicalFlag = medicalFlag;
        }

        /**
         * Sets the medicalFlag from a DfE XML Element.
         *
         * @param medicalFlagElement void
         */
        public void setMedicalFlag(Element medicalFlagElement) {
            if (medicalFlagElement != null) {
                String resp = medicalFlagElement.getTextTrim().toLowerCase();
                this.medicalFlag = Boolean.valueOf(DfEManager.STRING_TRUE.equals(resp));
            }
        }

        /**
         * Gets the inCare.
         *
         * @return String
         */
        public Boolean getInCare() {
            return inCare;
        }

        /**
         * Sets the inCare.
         *
         * @param inCare void
         */
        public void setInCare(Boolean inCare) {
            this.inCare = inCare;
        }

        /**
         * Sets the inCare from a DfE XML Element.
         *
         * @param inCareElement void
         */
        public void setInCare(Element inCareElement) {
            if (inCareElement != null) {
                String resp = inCareElement.getTextTrim().toLowerCase();
                this.inCare = Boolean.valueOf(DfEManager.STRING_TRUE.equals(resp));
            }
        }

        /**
         * Gets the careAuthority.
         *
         * @return String
         */
        public String getCareAuthority() {
            return careAuthority;
        }

        /**
         * Sets the careAuthority.
         *
         * @param careAuthority void
         */
        public void setCareAuthority(String careAuthority) {
            this.careAuthority = careAuthority;
        }

        /**
         * Sets the careAuthority from a DfE XML Element.
         *
         * @param careAuthorityElement void
         */
        public void setCareAuthority(Element careAuthorityElement) {
            if (careAuthorityElement != null) {
                this.careAuthority = careAuthorityElement.getTextTrim();
            }
        }

    }


    /**
     * The Class DfESchoolHistory.
     */
    class DfESchoolHistory {
        public static final String ELEMENT_SCHOOL_HISTORY = "SchoolHistory";
        public static final String ELEMENT_SCHOOL = "School";
        public static final String ELEMENT_LEA = "LEA";
        public static final String ELEMENT_ESTAB = "Estab";
        public static final String ELEMENT_SCHOOL_NAME = "SchoolName";
        public static final String ELEMENT_ENTRY_DATE = "EntryDate";
        public static final String ELEMENT_LEAVING_DATE = "LeavingDate";
        public static final String ELEMENT_LEAVING_REASON = "LeavingReason";
        public static final String ELEMENT_LAST_SCHOOL = "LastSchool";

        public static final String TRUE = "true";

        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

        private String lEA = null;
        private String estab = null;
        private String schoolName = null;
        private PlainDate entryDate = null;
        private PlainDate leavingDate = null;
        private String leavingReason = null;
        private Boolean lastSchool = null;


        /**
         * Constructor for DfE School History Object.
         *
         * @param schoolHistoryElement Element
         */
        public DfESchoolHistory(Element schoolHistoryElement) {
            setLEA(schoolHistoryElement.getChild(ELEMENT_LEA));
            setEstab(schoolHistoryElement.getChild(ELEMENT_ESTAB));
            setSchoolName(schoolHistoryElement.getChild(ELEMENT_SCHOOL_NAME));
            setEntryDate(schoolHistoryElement.getChild(ELEMENT_ENTRY_DATE));
            setLeavingDate(schoolHistoryElement.getChild(ELEMENT_LEAVING_DATE));
            setLeavingReason(schoolHistoryElement.getChild(ELEMENT_LEAVING_REASON));
            setLastSchool(schoolHistoryElement.getChild(ELEMENT_LAST_SCHOOL));
        }

        /**
         * Constructor for DfE School History Object.
         *
         * @param lEA String
         * @param estab String
         * @param schoolName String
         * @param entryDate PlainDate
         * @param leavingDate PlainDate
         * @param leavingReason String
         * @param lastSchool Boolean
         */
        public DfESchoolHistory(String lEA, String estab, String schoolName, PlainDate entryDate, PlainDate leavingDate,
                String leavingReason, Boolean lastSchool) {
            setLEA(lEA);
            setEstab(estab);
            setSchoolName(schoolName);
            setEntryDate(entryDate);
            setLeavingDate(leavingDate);
            setLeavingReason(leavingReason);
            setLastSchool(lastSchool);
        }


        /**
         * Gets the lEA.
         *
         * @return PlainDate
         */
        public String getLEA() {
            return lEA;
        }

        /**
         * Sets the lEA.
         *
         * @param lEA void
         */
        public void setLEA(String lEA) {
            this.lEA = lEA;
        }

        /**
         * Sets the lEA from a DfE XML Element.
         *
         * @param lEAElement void
         */
        public void setLEA(Element lEAElement) {
            if (lEAElement != null) {
                this.lEA = lEAElement.getTextTrim();
            }
        }

        /**
         * Gets the estab.
         *
         * @return PlainDate
         */
        public String getEstab() {
            return estab;
        }

        /**
         * Sets the estab.
         *
         * @param estab void
         */
        public void setEstab(String estab) {
            this.estab = estab;
        }

        /**
         * Sets the estab from a DfE XML Element.
         *
         * @param estabElement void
         */
        public void setEstab(Element estabElement) {
            if (estabElement != null) {
                this.estab = estabElement.getTextTrim();
            }
        }

        /**
         * Gets the schoolName.
         *
         * @return PlainDate
         */
        public String getSchoolName() {
            return schoolName;
        }

        /**
         * Sets the schoolName.
         *
         * @param schoolName void
         */
        public void setSchoolName(String schoolName) {
            this.schoolName = schoolName;
        }

        /**
         * Sets the schoolName from a DfE XML Element.
         *
         * @param schoolNameElement void
         */
        public void setSchoolName(Element schoolNameElement) {
            if (schoolNameElement != null) {
                this.schoolName = schoolNameElement.getTextTrim();
            }
        }

        /**
         * Gets the entryDate.
         *
         * @return PlainDate
         */
        public PlainDate getEntryDate() {
            return entryDate;
        }

        /**
         * Sets the entryDate.
         *
         * @param entryDate void
         */
        public void setEntryDate(PlainDate entryDate) {
            this.entryDate = entryDate;
        }

        /**
         * Sets the entryDate from a DfE XML Element.
         *
         * @param entryDateElement void
         */
        public void setEntryDate(Element entryDateElement) {
            if (entryDateElement != null) {
                String entryDateStr = entryDateElement.getTextTrim();
                if (!StringUtils.isEmpty(entryDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(entryDateStr);
                        this.entryDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.entryDate = null;
                    }
                }
            }
        }

        /**
         * Gets the leavingDate.
         *
         * @return PlainDate
         */
        public PlainDate getLeavingDate() {
            return leavingDate;
        }

        /**
         * Sets the leavingDate.
         *
         * @param leavingDate void
         */
        public void setLeavingDate(PlainDate leavingDate) {
            this.leavingDate = leavingDate;
        }

        /**
         * Sets the entryDate from a DfE XML Element.
         *
         * @param leavingDateElement void
         */
        public void setLeavingDate(Element leavingDateElement) {
            if (leavingDateElement != null) {
                String leavingDateStr = leavingDateElement.getTextTrim();
                if (!StringUtils.isEmpty(leavingDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(leavingDateStr);
                        this.leavingDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.leavingDate = null;
                    }
                }
            }
        }

        /**
         * Gets the leavingReason.
         *
         * @return PlainDate
         */
        public String getLeavingReason() {
            return leavingReason;
        }

        /**
         * Sets the leavingReason.
         *
         * @param leavingReason void
         */
        public void setLeavingReason(String leavingReason) {
            this.leavingReason = leavingReason;
        }

        /**
         * Sets the leavingReason from a DfE XML Element.
         *
         * @param leavingReasonElement void
         */
        public void setLeavingReason(Element leavingReasonElement) {
            if (leavingReasonElement != null) {
                this.leavingReason = leavingReasonElement.getTextTrim();
            }
        }

        /**
         * Gets the lastSchool.
         *
         * @return String
         */
        public Boolean getLastSchool() {
            return lastSchool;
        }

        /**
         * Sets the lastSchool.
         *
         * @param lastSchool void
         */
        public void setLastSchool(Boolean lastSchool) {
            this.lastSchool = lastSchool;
        }

        /**
         * Sets the lastSchool from a DfE XML Element.
         *
         * @param lastSchoolElement void
         */
        public void setLastSchool(Element lastSchoolElement) {
            if (lastSchoolElement != null) {
                String resp = lastSchoolElement.getTextTrim().toLowerCase();
                this.lastSchool = Boolean.valueOf(TRUE.equals(resp));
            }
        }

    }


    /**
     * The Class DfESENNeed.
     */
    class DfESENNeed {
        public static final String PROGRAM_CODE_SEN = "SEN";
        public static final String ELEMENT_SEN = "SEN";
        public static final String ELEMENT_SEN_START_DATE = "StartDate";
        public static final String ELEMENT_SEN_PROVISION = "SENprovision";
        public static final String ELEMENT_SEN_NEEDS = "SENneeds";
        public static final String ELEMENT_SEN_NEED = "SENneed";
        public static final String ELEMENT_SEN_TYPE = "SENtype";
        public static final String ELEMENT_SEN_TYPE_RANK = "SENtypeRank";

        private String type = null;
        private String typeRank = null;

        /**
         * Constructor for DfE (UK Department for Education) SEN Need Object.
         *
         * @param sENNeedElement Element
         */
        public DfESENNeed(Element sENNeedElement) {
            setType(sENNeedElement.getChild(ELEMENT_SEN_TYPE));
            setTypeRank(sENNeedElement.getChild(ELEMENT_SEN_TYPE_RANK));
        }

        /**
         * Constructor for DfE (UK Department for Education) SEN Need Object.
         *
         * @param type String
         * @param typeRank String
         */
        public DfESENNeed(String type, String typeRank) {
            setType(type);
            setTypeRank(typeRank);
        }

        /**
         * Gets the type.
         *
         * @return String
         */
        public String getType() {
            return type;
        }

        /**
         * Sets the type.
         *
         * @param type void
         */
        public void setType(String type) {
            this.type = type;
        }

        /**
         * Sets the type from a DfE XML Element.
         *
         * @param typeElement void
         */
        public void setType(Element typeElement) {
            if (typeElement != null) {
                this.type = typeElement.getTextTrim();
            }
        }

        /**
         * Gets the typeRank.
         *
         * @return String
         */
        public String getTypeRank() {
            return typeRank;
        }

        /**
         * Sets the typeRank.
         *
         * @param typeRank void
         */
        public void setTypeRank(String typeRank) {
            this.typeRank = typeRank;
        }

        /**
         * Sets the typeRank from a DfE XML Element.
         *
         * @param typeRankElement void
         */
        public void setTypeRank(Element typeRankElement) {
            if (typeRankElement != null) {
                this.typeRank = typeRankElement.getTextTrim();
            }
        }

    }

    /**
     * The Class DfEStageAssessment.
     */
    class DfEStageAssessment {
        public static final String ELEMENT_STAGE_ASSESSMENTS = "StageAssessments";
        public static final String ELEMENT_KEY_STAGE = "KeyStage";
        public static final String ELEMENT_STAGE = "Stage";
        public static final String ELEMENT_STAGE_ASSESSMENT = "StageAssessment";
        public static final String ELEMENT_LOCALE = "Locale";
        public static final String ELEMENT_YEAR = "Year";
        public static final String ELEMENT_SUBJECT = "Subject";
        public static final String ELEMENT_METHOD = "Method";
        public static final String ELEMENT_COMPONENT = "Component";
        public static final String ELEMENT_RESULT_STATUS = "ResultStatus";
        public static final String ELEMENT_RESULT_QUALIFIER = "ResultQualifier";
        public static final String ELEMENT_RESULT = "Result";
        public static final String ELEMENT_RESULT_DATE = "ResultDate";

        public static final String TRUE = "true";

        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

        private String stage = null;
        private String locale = null;
        private String year = null;
        private String subject = null;
        private String method = null;
        private String component = null;
        private String resultStatus = null;
        private String resultQualifier = null;
        private String result = null;
        private PlainDate resultDate = null;

        /**
         * Constructor for DfE School History Object.
         *
         * @param stage String
         * @param stageAssessmentElement Element
         */
        public DfEStageAssessment(String stage, Element stageAssessmentElement) {
            setStage(stage);
            setLocale(stageAssessmentElement.getChild(ELEMENT_LOCALE));
            setYear(stageAssessmentElement.getChild(ELEMENT_YEAR));
            setResultDate(stageAssessmentElement.getChild(ELEMENT_RESULT_DATE));
            setSubject(stageAssessmentElement.getChild(ELEMENT_SUBJECT));
            setMethod(stageAssessmentElement.getChild(ELEMENT_METHOD));
            setComponent(stageAssessmentElement.getChild(ELEMENT_COMPONENT));
            setResultStatus(stageAssessmentElement.getChild(ELEMENT_RESULT_STATUS));
            setResultQualifier(stageAssessmentElement.getChild(ELEMENT_RESULT_QUALIFIER));
            setResult(stageAssessmentElement.getChild(ELEMENT_RESULT));
            setResultDate(stageAssessmentElement.getChild(ELEMENT_RESULT_DATE));
        }

        /**
         * Constructor for DfE School History Object.
         *
         * @param stage String
         * @param locale String
         * @param year String
         * @param subject String
         * @param method String
         * @param component String
         * @param resultStatus String
         * @param resultQualifier String
         * @param result String
         * @param resultDateStr String
         */
        public DfEStageAssessment(String stage, String locale, String year, String subject, String method,
                String component, String resultStatus, String resultQualifier, String result, String resultDateStr) {
            setStage(stage);
            setLocale(locale);
            setYear(year);
            setSubject(subject);
            setMethod(method);
            setComponent(component);
            setResultStatus(resultStatus);
            setResultQualifier(resultQualifier);
            setResult(result);
            setResultDate(resultDateStr);
        }


        /**
         * Gets the stage.
         *
         * @return PlainDate
         */
        public String getStage() {
            return stage;
        }

        /**
         * Sets the stage.
         *
         * @param stage void
         */
        public void setStage(String stage) {
            this.stage = stage;
        }

        /**
         * Sets the stage from a DfE XML Element.
         *
         * @param stageElement void
         */
        public void setStage(Element stageElement) {
            if (stageElement != null) {
                this.stage = stageElement.getTextTrim();
            }
        }

        /**
         * Gets the locale.
         *
         * @return PlainDate
         */
        public String getLocale() {
            return locale;
        }

        /**
         * Sets the locale.
         *
         * @param locale void
         */
        public void setLocale(String locale) {
            this.locale = locale;
        }

        /**
         * Sets the locale from a DfE XML Element.
         *
         * @param localeElement void
         */
        public void setLocale(Element localeElement) {
            if (localeElement != null) {
                this.locale = localeElement.getTextTrim();
            }
        }

        /**
         * Gets the year.
         *
         * @return PlainDate
         */
        public String getYear() {
            return year;
        }

        /**
         * Sets the year.
         *
         * @param year void
         */
        public void setYear(String year) {
            this.year = year;
        }

        /**
         * Sets the year from a DfE XML Element.
         *
         * @param yearElement void
         */
        public void setYear(Element yearElement) {
            if (yearElement != null) {
                this.year = yearElement.getTextTrim();
            }
        }

        /**
         * Gets the subject.
         *
         * @return PlainDate
         */
        public String getSubject() {
            return subject;
        }

        /**
         * Sets the subject.
         *
         * @param subject void
         */
        public void setSubject(String subject) {
            this.subject = subject;
        }

        /**
         * Sets the subject from a DfE XML Element.
         *
         * @param subjectElement void
         */
        public void setSubject(Element subjectElement) {
            if (subjectElement != null) {
                this.subject = subjectElement.getTextTrim();
            }
        }



        /**
         * Gets the method.
         *
         * @return String
         */
        public String getMethod() {
            return method;
        }

        /**
         * Sets the method.
         *
         * @param method void
         */
        public void setMethod(String method) {
            this.method = method;
        }

        /**
         * Sets the component from a DfE XML Element.
         *
         * @param methodElement void
         */
        public void setMethod(Element methodElement) {
            if (methodElement != null) {
                this.method = methodElement.getTextTrim();
            }
        }

        /**
         * Gets the component.
         *
         * @return String
         */
        public String getComponent() {
            return component;
        }

        /**
         * Sets the component.
         *
         * @param component void
         */
        public void setComponent(String component) {
            this.component = component;
        }

        /**
         * Sets the component from a DfE XML Element.
         *
         * @param componentElement void
         */
        public void setComponent(Element componentElement) {
            if (componentElement != null) {
                this.component = componentElement.getTextTrim();
            }
        }

        /**
         * Gets the resultStatus.
         *
         * @return String
         */
        public String getResultStatus() {
            return resultStatus;
        }

        /**
         * Sets the resultStatus.
         *
         * @param resultStatus void
         */
        public void setResultStatus(String resultStatus) {
            this.resultStatus = resultStatus;
        }

        /**
         * Sets the resultStatus from a DfE XML Element.
         *
         * @param resultStatusElement void
         */
        public void setResultStatus(Element resultStatusElement) {
            if (resultStatusElement != null) {
                this.resultStatus = resultStatusElement.getTextTrim();
            }
        }

        /**
         * Gets the uniqueCandidateNumber.
         *
         * @return String
         */
        public String getResultQualifier() {
            return resultQualifier;
        }

        /**
         * Sets the uniqueCandidateNumber.
         *
         * @param resultQualifier void
         */
        public void setResultQualifier(String resultQualifier) {
            this.resultQualifier = resultQualifier;
        }

        /**
         * Sets the resultQualifier from a DfE XML Element.
         *
         * @param resultQualifierElement void
         */
        public void setResultQualifier(Element resultQualifierElement) {
            if (resultQualifierElement != null) {
                this.resultQualifier = resultQualifierElement.getTextTrim();
            }
        }

        /**
         * Gets the result.
         *
         * @return String
         */
        public String getResult() {
            return result;
        }

        /**
         * Sets the result.
         *
         * @param result void
         */
        public void setResult(String result) {
            this.result = result;
        }

        /**
         * Sets the result from a DfE XML Element.
         *
         * @param resultElement void
         */
        public void setResult(Element resultElement) {
            if (resultElement != null) {
                this.result = resultElement.getTextTrim();
            }
        }

        /**
         * Gets the resultDate.
         *
         * @return PlainDate
         */
        public PlainDate getResultDate() {
            return resultDate;
        }

        /**
         * Sets the resultDate.
         *
         * @param resultDate void
         */
        public void setResultDate(PlainDate resultDate) {
            this.resultDate = resultDate;
        }

        /**
         * Sets the resultDate from a String Date.
         *
         * @param resultDate void
         */
        public void setResultDate(String resultDate) {
            if (resultDate != null) {
                String reviewDateStr = resultDate.trim();
                if (!StringUtils.isEmpty(reviewDateStr)) {
                    Date date = null;
                    try {
                        date = m_dateFormat.parse(reviewDateStr);
                        this.resultDate = new PlainDate(date);
                    } catch (ParseException e) {
                        this.resultDate = null;
                    }
                }
            }
        }

        /**
         * Sets the resultDate from a DfE XML Element.
         *
         * @param resultDateElement void
         */
        public void setResultDate(Element resultDateElement) {
            if (resultDateElement != null) {
                String resultDateStr = resultDateElement.getTextTrim();
                Date date = null;
                try {
                    date = m_dateFormat.parse(resultDateStr);
                    this.resultDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.resultDate = null;
                }
            }
        }

    }

    /**
     * The Class DfETelephone.
     */
    class DfETelephone {
        public static final String ELEMENT_PHONES = "Phones";
        public static final String ELEMENT_PHONE = "Phone";
        public static final String ELEMENT_TELEPHONE_TYPE = "TelephoneType";
        public static final String ELEMENT_PHONE_NUMBER = "PhoneNo";

        public static final String DEFAULT_TELEPHONE_TYPE = "H";

        private String telephoneType = null;
        private String telephoneNumber = null;

        /**
         * Constructor for DfE (UK Department for Education) telephone Object.
         *
         * @param telephoneElement Element
         */
        public DfETelephone(Element telephoneElement) {
            setTelephoneType(telephoneElement.getChild(ELEMENT_TELEPHONE_TYPE));
            setTelephoneNumber(telephoneElement.getChild(ELEMENT_PHONE_NUMBER));
        }

        /**
         * Constructor for DfE (UK Department for Education) telephone Object.
         *
         * @param telephoneType String
         * @param telephoneNumber String
         */
        public DfETelephone(String telephoneType, String telephoneNumber) {
            setTelephoneType(telephoneType);
            setTelephoneNumber(telephoneNumber);
        }

        /**
         * Gets the telephoneType.
         *
         * @return String
         */
        public String getTelephoneType() {
            return telephoneType;
        }

        /**
         * Sets the telephoneType.
         *
         * @param telephoneType void
         */
        public void setTelephoneType(String telephoneType) {
            this.telephoneType = telephoneType;
        }

        /**
         * Sets the telephoneType from a DfE XML Element.
         *
         * @param telephoneTypeElement void
         */
        public void setTelephoneType(Element telephoneTypeElement) {
            if (telephoneTypeElement != null) {
                this.telephoneType = telephoneTypeElement.getTextTrim();
            }
        }

        /**
         * Gets the telephoneNumber.
         *
         * @return String
         */
        public String getTelephoneNumber() {
            return telephoneNumber;
        }

        /**
         * Sets the telephoneNumber.
         *
         * @param telephoneNumber void
         */
        public void setTelephoneNumber(String telephoneNumber) {
            this.telephoneNumber = telephoneNumber;
        }

        /**
         * Sets the telephoneNumber from a DfE XML Element.
         *
         * @param telephoneNumberElement void
         */
        public void setTelephoneNumber(Element telephoneNumberElement) {
            if (telephoneNumberElement != null) {
                this.telephoneNumber = telephoneNumberElement.getTextTrim();
            }
        }

    }


    /**
     * The Class DfEManager.
     */
    class DfEManager {
        public static final String DEFAULT_ORGANIZATION = "*dst";
        public static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
        public static final String REPORT_TYPE_ATF = "ATF";
        public static final String REPORT_TYPE_CTF = "CTF";

        public static final String PARAM_STUDENT_OIDS = "student-oids";
        public static final String PARAM_DESTINATION_LEA = "dest-lea";
        public static final String PARAM_DESTINATION_ESTAB = "dest-estab";

        public static final String PARAM_CORE = "core";
        public static final String PARAM_BASIC_DETAILS = "basic-details";
        public static final String PARAM_LOOKED_AFTER = "looked-after";
        public static final String PARAM_SEN_HISTORY = "sen-history";
        public static final String PARAM_ADMISSIONS = "admissions";
        public static final String PARAM_ADDRESS_PHONE_EMAIL = "address-phone-email";
        public static final String PARAM_CONTACTS = "contacts";
        public static final String PARAM_ATTENDANCE = "attendance";
        public static final String PARAM_ATTENDANCE_SESSIONS = "attendance-sessions";
        public static final String PARAM_ASSESSMENTS = "assessments";
        public static final String PARAM_SCHOOL_HISTORY = "school-history";
        public static final String PARAM_NAW_DETAILS = "naw-details";
        public static final String PARAM_FSM_HISTORY = "fsm-history";

        public static final String PARAM_UPDATE_EXISTING = "update-existing";

        public static final String STRING_TRUE = "true";
        public static final String STRING_FALSE = "false";
        public static final String STRING_EMPTY = "";
        public static final String DB_TRUE = "1";
        public static final String DB_FALSE = "0";
        public final Boolean BOOLEAN_TRUE = Boolean.valueOf(true);
        public final Boolean BOOLEAN_FALSE = Boolean.valueOf(false);
        public static final int DB_FIELD_A_MAX_LENGTH = 10;
        public static final int DB_FIELD_B_MAX_LENGTH = 25;
        public static final int DB_FIELD_C_MAX_LENGTH = 50;
        public static final int STRING_10_MAX_LENGTH = 10;
        public static final int STRING_20_MAX_LENGTH = 20;
        public static final int STRING_32_MAX_LENGTH = 32;
        public static final int STRING_40_MAX_LENGTH = 40;
        public static final int STRING_50_MAX_LENGTH = 50;
        public static final int STRING_100_MAX_LENGTH = 100;

        public static final String DFE_DATA_TIME_DELIMITER = "T";
        public static final String SCHOOL_ID_CTF_SCHOOL = "CTF School";
        public static final String ASSESSMENT_DEFINITION_SAT = "SAT";

        public static final String ALIAS_NAME_LEA = "DFE LEA ID";
        public static final String ALIAS_NAME_ESTAB = "DFE ESTABLISHMENT ID";

        public static final String ALIAS_NAME_APPLICATION_REFERENCE = "DFE APPLICATION REF";
        public static final String ALIAS_NAME_UPN = "DFE UPN";
        public static final String ALIAS_NAME_FORMER_UPN = "DFE FORMER UPN";
        public static final String ALIAS_NAME_PREFERRED_SURNAME = "DFE PREFERRED SURNAME";
        public static final String ALIAS_NAME_PREFERRED_FORENAME = "DFE PREFERRED FORENAME";
        public static final String ALIAS_NAME_FORMER_SURNAME = "DFE FORMER SURNAME";
        public static final String ALIAS_NAME_FORMER_FORENAME = "DFE FORMER FORENAME";
        public static final String ALIAS_NAME_ETHNICITY = "DFE ETHNICITY";
        public static final String ALIAS_NAME_ETHNICITY_SOURCE = "DFE ETHNICITY SOURCE";
        public static final String ALIAS_NAME_MEDICAL_FLAG = "DFE MEDICAL FLAG";
        public static final String ALIAS_NAME_ORDER = "DFE ORDER";
        public static final String ALIAS_NAME_RESPONSIBLE = "DFE RESPONSIBLE";
        public static final String ALIAS_NAME_ULN = "DFE ULN";
        public static final String ALIAS_NAME_UCI = "DFE UCI";
        public static final String ALIAS_NAME_IN_CARE = "DFE IN CARE";
        public static final String ALIAS_NAME_CARE_AUTHORITY = "DFE CARE AUTHORITY";
        public static final String ALIAS_NAME_TELEPHONE_TYPE_1 = "DFE TELEPHONE TYPE 1";
        public static final String ALIAS_NAME_TELEPHONE_TYPE_2 = "DFE TELEPHONE TYPE 2";
        public static final String ALIAS_NAME_TELEPHONE_TYPE_3 = "DFE TELEPHONE TYPE 3";
        public static final String ALIAS_NAME_FSM_REVIEW_DATE = "DFE FSM REVIEW DATE";
        public static final String ALIAS_NAME_FSM_UK_COUNTRY = "DFE FSM UK COUNTRY";
        public static final String ALIAS_NAME_SEN_PUPIL_PROVISION = "DFE PUPIL SEN PROVISION";
        public static final String ALIAS_NAME_SEN_PROVISION = "DFE SEN PROVISION";
        public static final String ALIAS_NAME_SEN_TYPE = "DFE SEN TYPE";
        public static final String ALIAS_NAME_SEN_RANK = "DFE SEN RANK";
        public static final String ALIAS_NAME_SPEAK_WELSH = "DFE SPEAK WELSH";
        public static final String ALIAS_NAME_HOME_WELSH = "DFE HOME WELSH";
        public static final String ALIAS_NAME_NATIONAL_IDENTITY = "DFE NATIONAL IDENTITY";
        public static final String ALIAS_NAME_WELSH_SOURCE = "DFE WELSH SOURCE";
        public static final String ALIAS_NAME_EAL_ACQUISITION = "DFE EAL ACQUISITION";
        public static final String ALIAS_NAME_LANGUAGE_SOURCE = "DFE LANGUAGE SOURCE";
        public static final String ALIAS_NAME_SEN_CURR_TEACH_METHOD = "DFE SEN CURR TEACH METHOD";
        public static final String ALIAS_NAME_SEN_GROUPING_SUPPORT = "DFE SEN GROUPING SUPPORT";
        public static final String ALIAS_NAME_SEN_SPEC_RESOURCES = "DFE SEN SPEC RESOURCES";
        public static final String ALIAS_NAME_SEN_ADVICE_ASSESSMENT = "DFE SEN ADVICE ASSESSMENT";

        public static final String ALIAS_NAME_SAON = "DFE ADDRESS SAON";
        public static final String ALIAS_NAME_LOCALITY = "DFE ADDRESS LOCALITY";
        public static final String ALIAS_NAME_ADMIN_AREA = "DFE ADDRESS ADMIN AREA";
        public static final String ALIAS_NAME_POST_TOWN = "DFE ADDRESS POST TOWN";
        public static final String ALIAS_NAME_UNIQUE_PROP_REF_NUM = "DFE UNIQUE PROP REF NUM";
        public static final String ALIAS_NAME_EASTING = "DFE ADDRESS EASTING";
        public static final String ALIAS_NAME_NORTHING = "DFE ADDRESS NORTHING";
        public static final String ALIAS_NAME_ADDRESS_LINE_4 = "DFE ADDRESS LINE 4";
        public static final String ALIAS_NAME_ADDRESS_LINE_5 = "DFE ADDRESS LINE 5";

        public static final String ALIAS_NAME_ATTENDANCE_XML = "DFE ATTENDANCE XML";
        public static final String ALIAS_NAME_ASSESSMENTS_XML = "DFE ASSESSMENTS XML";
        public static final String ALIAS_NAME_SCHOOL_HISTORY_XML = "DFE SCHOOL HISTORY XML";

        public static final String ALIAS_NAME_ATTEND_YEAR = "DFE ATTEND YEAR";
        public static final String ALIAS_NAME_ATTEND_LEA = "DFE ATTEND LEA";
        public static final String ALIAS_NAME_ATTEND_ESTAB = "DFE ATTEND ESTAB";
        public static final String ALIAS_NAME_ATTEND_SCHOOL_NAME = "DFE ATTEND SCHOOL NAME";
        public static final String ALIAS_NAME_ATTEND_SESS_POSSIBLE = "DFE ATTEND SESS POSSIBLE";
        public static final String ALIAS_NAME_ATTEND_SESS_AUTHORIZED = "DFE ATTEND SESS AUTHORIZED";
        public static final String ALIAS_NAME_ATTEND_SESS_ATTENDED = "DFE ATTEND SESS ATTENDED";
        public static final String ALIAS_NAME_ATTEND_SESS_UNAUTHORIZED = "DFE ATTEND SESS UNAUTHORIZED";
        public static final String ALIAS_NAME_ATTEND_START_DATE = "DFE ATTEND START DATE";
        public static final String ALIAS_NAME_ATTEND_MARKS = "DFE ATTEND MARKS";

        public static final String ALIAS_NAME_STAGE = "DFE ASSESS STAGE";
        public static final String ALIAS_NAME_LOCALE = "DFE ASSESS LOCALE";
        public static final String ALIAS_NAME_YEAR_TAKEN = "DFE ASSESS YEAR TAKEN";
        public static final String ALIAS_NAME_SUBJECT = "DFE ASSESS SUBJECT";
        public static final String ALIAS_NAME_METHOD = "DFE ASSESS METHOD";
        public static final String ALIAS_NAME_COMPONENT = "DFE ASSESS COMPONENT";
        public static final String ALIAS_NAME_RESULT_STATUS = "DFE ASSESS RESULT STATUS";
        public static final String ALIAS_NAME_RESULT_QUALIFIER = "DFE ASSESS RESULT QUALIFIER";
        public static final String ALIAS_NAME_RESULT = "DFE ASSESS RESULT";
        public static final String ALIAS_NAME_RESULT_DATE = "DFE ASSESS RESULT DATE";

        public static final String ALIAS_NAME_OUT_OF_LEA_ID = "DFE OUT OF LEA ID";
        public static final String ALIAS_NAME_OUT_OF_LEA_ESTAB = "DFE OUT OF LEA ESTAB";
        public static final String ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME = "DFE OUT OF LEA ESTAB NAME";

        /**
         * A local copy of the data dictionary for use by various lookup utilities.
         */
        private DataDictionary m_dictionary;

        /**
         * A local copy of the X2 Broker.
         */
        protected X2Broker m_broker = null;

        /**
         * A local copy of the locale.
         */
        protected Locale m_locale = null;

        /**
         * Default SAT Assessment Definition.
         */
        protected AssessmentDefinition m_sATAssessmentDefinition = null;

        /**
         * Default CTF School.
         */
        protected School m_cTFSchool = null;

        /**
         * Date and Time formats.
         */
        public SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        public SimpleDateFormat m_timeFormat = new SimpleDateFormat("hh:mm:ss");

        /**
         * A map of maps of reference code.
         * The outer map is indexed by the reference table OID. It contains maps of reference codes.
         * The inner map is indexed by the reference code. It contains the RefrenceCode bean for the
         * code.
         */
        private Map<String, Map<String, ReferenceCode>> m_refTableMap = null;

        /**
         * Constructor for DfEManager.
         *
         * @param broker X2Broker
         * @param locale Locale
         */
        public DfEManager(X2Broker broker, Locale locale) {
            m_broker = broker;
            m_locale = locale;

            m_cTFSchool = getCTFSchool();
            m_sATAssessmentDefinition = getSATAssessmentDefinition();
        }

        /**
         * Convert a Java File to byte Array.
         *
         * @param file File
         * @return byte[]
         * @throws IOException Signals that an I/O exception has occurred.
         */
        public byte[] getBytesFromFile(File file) throws IOException {
            // Get the size of the file
            long length = file.length();

            // You cannot create an array using a long type.
            // It needs to be an int type.
            // Before converting to an int type, check
            // to ensure that file is not larger than Integer.MAX_VALUE.
            if (length > Integer.MAX_VALUE) {
                // File is too large
            }

            // Create the byte array to hold the data
            byte[] bytes = new byte[(int) length];

            InputStream is = new FileInputStream(file);
            try {
                // Read in the bytes
                int offset = 0;
                int numRead = 0;
                while (offset < bytes.length && (numRead = is.read(bytes, offset, bytes.length - offset)) >= 0) {
                    offset += numRead;
                }

                // Ensure all the bytes have been read in
                if (offset < bytes.length) {
                    throw new IOException("Could not completely read file " + file.getName());
                }
            } finally {
                // Close the input stream and return bytes
                is.close();
            }

            return bytes;
        }

        /**
         * Get the default CTF School
         *
         * This is used for imported out-of-LEA SchoolHistory and Student Attendance.
         *
         * @return School
         */
        public School getCTFSchool() {
            School cTFSchool = null;

            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(School.COL_SCHOOL_ID, SCHOOL_ID_CTF_SCHOOL);
            BeanQuery schoolQuery = new BeanQuery(School.class, schoolCriteria);
            Collection<School> schools = m_broker.getCollectionByQuery(schoolQuery);

            if (schools.size() > 0) {
                for (School school : schools) {
                    cTFSchool = school;
                    break;
                }
            } else {
                // If not found create it
                cTFSchool = X2BaseBean.newInstance(School.class, m_broker.getPersistenceKey());
                cTFSchool.setOrganization1Oid(DEFAULT_ORGANIZATION);
                cTFSchool.setName(SCHOOL_ID_CTF_SCHOOL);
                cTFSchool.setSchoolId(SCHOOL_ID_CTF_SCHOOL);
                cTFSchool.setInactiveIndicator(true);

                m_broker.saveBeanForced(cTFSchool);
            }

            return cTFSchool;
        }

        /**
         * Returns a local instance of a district data dictionary.
         *
         * @return DataDictionary.
         */
        public DataDictionary getDataDictionary() {
            if (m_dictionary == null) {
                m_dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            }
            return m_dictionary;
        }

        /**
         * Lookup and return a DataDictionaryField based on a root bean and bean path.
         * This allows multi-hop paths in the bean path.
         *
         *
         * @param beanClass Class
         * @param path String
         * @return DataDictionaryField
         */
        public DataDictionaryField getDataDictionaryField(Class beanClass, String path) {
            ModelProperty prop = new ModelProperty(beanClass, path, m_broker.getPersistenceKey());
            DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryField(prop.getFieldId());
            return dictionaryField;
        }

        /**
         * Lookup and return a DataDictionaryField based on a root bean and bean path.
         * This allows multi-hop paths in the bean path.
         *
         *
         * @param bean X2BaseBean
         * @param path String
         * @return DataDictionaryField
         */
        public DataDictionaryField getDataDictionaryField(X2BaseBean bean, String path) {
            ModelProperty prop = new ModelProperty(bean.getClass(), path, m_broker.getPersistenceKey());
            DataDictionaryField dictionaryField = getDataDictionary().findDataDictionaryField(prop.getFieldId());
            return dictionaryField;
        }

        /**
         * Get a UK Establishment by UK School .
         *
         * @param school School
         * @return String
         */
        public String getEstabBySchool(School school) {
            String estab = null;

            if (school != null) {
                estab = (String) school.getFieldValueByAlias(ALIAS_NAME_ESTAB, getDataDictionary());
            }

            return estab;
        }

        /**
         * Get a UK LEA by UK School .
         *
         * @param school School
         * @return String
         */
        public String getLEABySchool(School school) {
            String lEA = null;

            Organization organization = school.getOrganization1();
            if (organization != null) {
                lEA = (String) organization.getFieldValueByAlias(ALIAS_NAME_LEA, getDataDictionary());
            }

            return lEA;
        }

        /**
         * Get the District's Organization by UK LEA .
         *
         * @param lEA String
         * @return Organization
         */
        public Organization getOrganizationByLea(String lEA) {
            Organization existingOrganization = null;

            String lEAAliasName = translateAliasToJavaName(ALIAS_NAME_LEA, true);

            // If not found error DFE LEA ID not properly defined.
            if (lEAAliasName == null) {
                String message = "The server's Data Dictionary was not configurated with the proper field Aliases!";
                AppGlobals.getLog().log(Level.SEVERE, message);
            } else {
                X2Criteria organizationCriteria = new X2Criteria();
                organizationCriteria.addEqualTo(lEAAliasName, lEA);

                BeanQuery organizationQuery = new BeanQuery(Organization.class, organizationCriteria);
                Collection<Organization> organizations = m_broker.getCollectionByQuery(organizationQuery);

                if (organizations.size() > 0) {
                    for (Organization organization : organizations) {
                        existingOrganization = organization;
                        break;
                    }
                }
            }

            return existingOrganization;
        }

        /**
         * Lookup a map of reference codes for a reference table oid.
         * Cache the results for later use.
         *
         * @param referenceTableOid String
         * @return Map<String, ReferenceCode>
         */
        public Map<String, ReferenceCode> getReferenceCodes(String referenceTableOid) {
            Map<String, ReferenceCode> codeMap = null;
            if (m_refTableMap == null) {
                m_refTableMap = new HashMap<String, Map<String, ReferenceCode>>();
            }

            if (m_refTableMap.containsKey(referenceTableOid)) {
                codeMap = m_refTableMap.get(referenceTableOid);
            } else {
                codeMap = new HashMap<String, ReferenceCode>();
                ReferenceTable refTable =
                        (ReferenceTable) m_broker.getBeanByOid(ReferenceTable.class, referenceTableOid);
                if (refTable != null) {
                    Collection<ReferenceCode> codes = refTable.getReferenceCodes(m_broker);
                    for (ReferenceCode code : codes) {
                        codeMap.put(code.getCode(), code);
                    }
                }
                m_refTableMap.put(referenceTableOid, codeMap);
            }
            return codeMap;
        }

        /**
         * Get a UK SAT Assessment Definition .
         *
         * @return AssessmentDefinition
         */
        public AssessmentDefinition getSATAssessmentDefinition() {
            AssessmentDefinition assessmentDefinition = null;

            X2Criteria assessmentDefinitionCriteria = new X2Criteria();
            assessmentDefinitionCriteria.addEqualTo(AssessmentDefinition.COL_ID, ASSESSMENT_DEFINITION_SAT);

            BeanQuery assessmentDefinitionQuery =
                    new BeanQuery(AssessmentDefinition.class, assessmentDefinitionCriteria);
            assessmentDefinition = (AssessmentDefinition) m_broker.getBeanByQuery(assessmentDefinitionQuery);

            // If it doesn't exist then produce an error message.
            if (assessmentDefinition == null || assessmentDefinition.getAssessmentColumnDefinitions().size() == 0) {
                String message = "ERROR CTF Import: SAT Assessment Definition is not configured.";
                AppGlobals.getLog().log(Level.WARNING, message);
            }

            return assessmentDefinition;
        }

        /**
         * Get a UK School by UK Estab .
         *
         * @param estab String
         * @return School
         */
        public School getSchoolByEstab(String estab) {
            School existingSchool = null;

            String estabAliasName = translateAliasToJavaName(ALIAS_NAME_ESTAB, true);

            // If not found error DFE Estab ID not properly defined.
            if (estabAliasName == null) {
                String message = "The server's Data Dictionary was not configurated with the proper field Aliases!";
                AppGlobals.getLog().log(Level.SEVERE, message);
            } else {
                X2Criteria schoolCriteria = new X2Criteria();
                schoolCriteria.addEqualTo(estabAliasName, estab);

                BeanQuery schoolQuery = new BeanQuery(School.class, schoolCriteria);
                Collection<School> schools = m_broker.getCollectionByQuery(schoolQuery);

                if (schools.size() > 0) {
                    for (School school : schools) {
                        existingSchool = school;
                        break;
                    }
                }
            }

            return existingSchool;
        }

        /**
         * Get the Student by their UPN (Unique Pupil Number)
         *
         * A student existence is checked by searching for their UPN.
         *
         * @param dfEPupil DfEPupil
         * @return Student
         */
        public Student getStudentByUPN(DfEPupil dfEPupil) {
            Student existingStudent = null;

            String studentUPNAliasName = translateAliasToJavaName(ALIAS_NAME_UPN, true);

            // If not found error DFE UPN not properly defined in Data Dictionary.
            if (studentUPNAliasName == null) {
                String message = "The server's Data Dictionary was not configurated with the proper field Aliases!";
                AppGlobals.getLog().log(Level.SEVERE, message);
            } else {
                // Check Student UPN [DFE UPN] and Former UPN [DFE FORMER UPN]
                X2Criteria studentCriteria = new X2Criteria();
                studentCriteria.addEqualTo(studentUPNAliasName, dfEPupil.getUniquePupilNumber());

                BeanQuery studentQuery = new BeanQuery(Student.class, studentCriteria);
                Collection<Student> students = m_broker.getCollectionByQuery(studentQuery);

                if (!students.isEmpty()) {
                    for (Student student : students) {
                        existingStudent = student;
                        break;
                    }
                }
            }

            return existingStudent;
        }

        /**
         * Check is a Section is Active .
         *
         * @param activeSections HashMap<String,Boolean>
         * @param sectionName String
         * @return boolean
         */
        public boolean isSectionActive(HashMap<String, Boolean> activeSections, String sectionName) {
            boolean isActive = false;
            if (activeSections.get(sectionName) != null && activeSections.get(sectionName).booleanValue()) {
                isActive = true;
            }

            return isActive;
        }

        /**
         * Returns the state lookup code for field value.
         * Look up based on bean path.
         *
         * @param beanClass - the class of the bean to find the reference table.
         * @param beanPath - the bean path of the bean to find the reference table.
         * @param value - the value to lookup and translate in the lookup table.
         * @param referenceMap - the reference map type
         *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
         *
         * @return String - state code for input value.
         */
        public String lookupReferenceCodeByBeanPath(Class beanClass, String beanPath, String value, int referenceMap) {
            String stateValue = null;
            DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
            if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
                stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
            }
            return stateValue;
        }

        /**
         * Returns the lookup code value for field value.
         * Look up based on the reference table.
         *
         * @param referenceTableOid - the reference table OID of the reference table.
         * @param value - the value to lookup and translate in the lookup table.
         * @param referenceMap - the reference map type
         *        (ExportFormatField.ReferenceMapTypeCode.*.ordinal()) of the lookup.
         *
         * @return String - reference code lookup value for input value.
         */
        public String lookupReferenceCodeByRefTbl(String referenceTableOid, String value, int referenceMap) {
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
         * Returns the lookup code value for Local Code.
         * Look up based on the reference table.
         *
         * @param beanClass - the class of the bean to find the reference table.
         * @param beanPath - the bean path of the bean to find the reference table.
         * @param lookupLocalCode - the value to lookup and translate in the lookup table.
         *
         * @return String - reference code lookup value for local code.
         */
        public String lookupRefCodeByLocalCode(Class beanClass, String beanPath, String lookupLocalCode) {
            String refCode = null;
            Map<String, ReferenceCode> m_referenceCodeMap = new HashMap<String, ReferenceCode>();

            DataDictionaryField dataDictionaryField = getDataDictionaryField(beanClass, beanPath);
            ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
            m_referenceCodeMap = referenceTable.getCodeMap(m_broker);
            for (ReferenceCode referenceCode : m_referenceCodeMap.values()) {
                String localCode = referenceCode.getLocalCode();
                if (localCode != null && localCode.equals(lookupLocalCode)) {
                    refCode = referenceCode.getCode();
                    return refCode; // break
                }
            }

            return refCode;
        }

        /**
         * Returns the lookup code value for Local Code Alias.
         * Look up based on the reference table.
         *
         * @param beanClass - the class of the bean to find the reference table.
         * @param beanPath - the bean path of the bean to find the reference table.
         * @param lookupLocalCode - the value to lookup and translate in the lookup table.
         *
         * @return String - reference code lookup value for local code.
         */
        public String lookupRefCodeByLocalCodeAlias(Class beanClass, String beanPath, String lookupLocalCode) {
            String refCode = null;
            Map<String, ReferenceCode> m_referenceCodeMap = new HashMap<String, ReferenceCode>();
            DataDictionaryField dataDictionaryField = getDataDictionaryField(beanClass, beanPath);
            ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
            m_referenceCodeMap = referenceTable.getCodeMap(m_broker);
            for (ReferenceCode referenceCode : m_referenceCodeMap.values()) {
                String localCode = referenceCode.getLocalCode();
                if (localCode != null && localCode.equals(lookupLocalCode)) {
                    refCode = referenceCode.getCode();
                    return refCode; // break
                }
            }

            return refCode;
        }

        /**
         * Returns the lookup code value for State Code.
         * Look up based on the reference table.
         *
         * @param beanClass - the class of the bean to find the reference table.
         * @param beanPath - the bean path of the bean to find the reference table.
         * @param lookupStateCode - the value to lookup and translate in the lookup table.
         *
         * @return String - reference code lookup value for state code.
         */
        public String lookupRefCodeByStateCode(Class beanClass, String beanPath, String lookupStateCode) {
            String refCode = null;
            Map<String, ReferenceCode> m_referenceCodeMap = new HashMap<String, ReferenceCode>();

            DataDictionaryField dataDictionaryField = getDataDictionaryField(beanClass, beanPath);
            ReferenceTable referenceTable = dataDictionaryField.getReferenceTable();
            m_referenceCodeMap = referenceTable.getCodeMap(m_broker);
            for (ReferenceCode referenceCode : m_referenceCodeMap.values()) {
                String stateCode = referenceCode.getStateCode();
                if (stateCode != null && stateCode.equals(lookupStateCode)) {
                    refCode = referenceCode.getCode();
                    return refCode; // break
                }
            }

            return refCode;
        }

        /**
         * Returns the state lookup code for field value.
         * Look up based on bean path.
         *
         * @param beanClass - the class of the bean to find the reference table.
         * @param beanPath - the bean path of the bean to find the reference table.
         * @param refCode - the refCode to lookup and translate in the lookup table.
         *
         * @return String - state code for input refCode.
         */
        public String lookupStateValueByRefCode(Class beanClass, String beanPath, String refCode) {
            String stateValue = lookupReferenceCodeByBeanPath(beanClass, beanPath, refCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return stateValue;
        }

        /**
         * Set the data dictionary to be used on this export.
         * If not set, a district data dictionary will be used.
         *
         * @param dictionary void
         */
        public void setDataDictionary(DataDictionary dictionary) {
            m_dictionary = dictionary;
        }

        /**
         * Translates an alias into a Java bean path name. An initialization error will be logged
         * if the alias does not exist.
         *
         * @param alias String
         * @param required boolean
         * @return String
         */
        public String translateAliasToJavaName(String alias, boolean required) {
            String javaName = null;

            DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                javaName = field.getJavaName();
            } else if (required) {
                String message = LocalizationCache.getMessages(m_broker.getPersistenceKey(), m_locale)
                        .getMessage(ERROR_ALIAS_LOOKUP);
                AppGlobals.getLog().log(Level.SEVERE, message);
            }

            return javaName;
        }

    }


    /**
     * The Class DfEExportManager.
     */
    class DfEExportManager extends DfEManager {
        private static final String ASPEN_PROGRAM_CODE_SE = "Special Education";
        private static final String ASPEN_PROGRAM_CODE_FRL = "Free/Reduced Lunch";
        private static final String ASPEN_PROGRAM_CODE_NAW = "NAW";

        public static final char LABEL_PREFIX_CHAR = '$';

        public static final String ATTENDANCE_MARK_NOT_IN_SESSION = "#";
        public static final String ATTENDANCE_MARK_PRESENT_AM_SESSION = "/";
        public static final String ATTENDANCE_MARK_PRESENT_PM_SESSION = "\\";

        /**
         * A local copy XML File Element builder.
         */
        private SAXBuilder m_builder = null;

        /**
         * List of the start and end dates of each school year
         */
        protected ArrayList<ArrayList<PlainDate>> m_schoolYearRanges;

        /**
         * Set of authorized attendance codes
         */
        public final Set<String> VAL_AUTHORISED_CODE_SET =
                new HashSet<String>(Arrays.asList(new String[] {
                        "I", // authorized illness
                        "M", // authorized medical/dental appoint
                        "R", // authorized religious observance
                        "S", // authorized study leave
                        "T", // authorized traveler absence
                        "H", // authorized agreed family holiday
                        "F", // authorized agreed extended family holiday
                        "E", // authorized excluded no alt. provision
                        "C" // authorized other circumstances
                }));

        /**
         * Set of unauthorized attendance codes
         */
        public final Set<String> VAL_UNAUTHORISED_CODE_SET =
                new HashSet<String>(Arrays.asList(new String[] {
                        "G", // unauthorized family holiday
                        "U", // unauthorized arrived after registers closed
                        "O", // unauthorized other
                        "N" // unauthorized reason not provided
                }));


        /**
         * Constructor for DfEManager.
         *
         * @param broker X2Broker
         * @param locale Locale
         */
        public DfEExportManager(X2Broker broker, Locale locale) {
            super(broker, locale);
            m_builder = new SAXBuilder();
        }

        /**
         * Get a Address Element from DfEAddress.
         *
         * @param dfEAddress DfEAddress
         * @return Element
         */
        public Element getAddressElement(DfEAddress dfEAddress) {
            Element addressElement = new Element(DfEAddress.ELEMENT_ADDRESS);

            if (dfEAddress.hasBS7666Address()) {
                Element bS7666AddressElement = new Element(DfEAddress.ELEMENT_BS7666_ADDRESS);

                if (dfEAddress.getSAON() != null) {
                    Element sAONElement = new Element(DfEAddress.ELEMENT_SAON);
                    sAONElement.addContent(dfEAddress.getSAON());
                    bS7666AddressElement.addContent(sAONElement);
                }
                if (dfEAddress.getPAON() != null) {
                    Element pAONElement = new Element(DfEAddress.ELEMENT_PAON);
                    pAONElement.addContent(dfEAddress.getPAON());
                    bS7666AddressElement.addContent(pAONElement);
                }
                if (dfEAddress.getStreet() != null) {
                    Element streetElement = new Element(DfEAddress.ELEMENT_STREET);
                    streetElement.addContent(dfEAddress.getStreet());
                    bS7666AddressElement.addContent(streetElement);
                }
                if (dfEAddress.getLocality() != null) {
                    Element localityElement = new Element(DfEAddress.ELEMENT_LOCALITY);
                    localityElement.addContent(dfEAddress.getLocality());
                    bS7666AddressElement.addContent(localityElement);
                }
                if (dfEAddress.getTown() != null) {
                    Element townElement = new Element(DfEAddress.ELEMENT_TOWN);
                    townElement.addContent(dfEAddress.getTown());
                    bS7666AddressElement.addContent(townElement);
                }
                if (dfEAddress.getAdministrativeArea() != null) {
                    Element administrativeAreaElement = new Element(DfEAddress.ELEMENT_ADMINISTRATIVE_AREA);
                    administrativeAreaElement.addContent(dfEAddress.getAdministrativeArea());
                    bS7666AddressElement.addContent(administrativeAreaElement);
                }
                if (dfEAddress.getPostTown() != null) {
                    Element postTownElement = new Element(DfEAddress.ELEMENT_POST_TOWN);
                    postTownElement.addContent(dfEAddress.getPostTown());
                    bS7666AddressElement.addContent(postTownElement);
                }
                if (dfEAddress.getZip() != null) {
                    Element postTownElement = new Element(DfEAddress.ELEMENT_ZIP);
                    postTownElement.addContent(dfEAddress.getPostTown());
                    bS7666AddressElement.addContent(postTownElement);
                }
                if (dfEAddress.getUniquePropertyReferenceNumber() != null) {
                    Element uniquePropertyReferenceNumberElement =
                            new Element(DfEAddress.ELEMENT_UNIQUE_PROPERTY_REFERENCE_NUMBER);
                    uniquePropertyReferenceNumberElement.addContent(dfEAddress.getUniquePropertyReferenceNumber());
                    bS7666AddressElement.addContent(uniquePropertyReferenceNumberElement);
                }

                addressElement.addContent(bS7666AddressElement);
            } else {
                Element addressLineElement = new Element(DfEAddress.ELEMENT_ADDRESS_LINES);

                if (dfEAddress.getAddressLine1() != null) {
                    Element addressLine1Element = new Element(DfEAddress.ELEMENT_ADDRESS_LINE_1);
                    addressLine1Element.addContent(dfEAddress.getAddressLine1());
                    addressLineElement.addContent(addressLine1Element);
                }
                if (dfEAddress.getAddressLine2() != null) {
                    Element addressLine2Element = new Element(DfEAddress.ELEMENT_ADDRESS_LINE_2);
                    addressLine2Element.addContent(dfEAddress.getAddressLine2());
                    addressLineElement.addContent(addressLine2Element);
                }
                if (dfEAddress.getAddressLine3() != null) {
                    Element addressLine3Element = new Element(DfEAddress.ELEMENT_ADDRESS_LINE_3);
                    addressLine3Element.addContent(dfEAddress.getAddressLine3());
                    addressLineElement.addContent(addressLine3Element);
                }
                if (dfEAddress.getAddressLine4() != null) {
                    Element addressLine4Element = new Element(DfEAddress.ELEMENT_ADDRESS_LINE_4);
                    addressLine4Element.addContent(dfEAddress.getAddressLine4());
                    addressLineElement.addContent(addressLine4Element);
                }
                if (dfEAddress.getAddressLine5() != null) {
                    Element addressLine5Element = new Element(DfEAddress.ELEMENT_ADDRESS_LINE_5);
                    addressLine5Element.addContent(dfEAddress.getAddressLine5());
                    addressLineElement.addContent(addressLine5Element);
                }

                addressElement.addContent(addressLineElement);
            }

            if (dfEAddress.getCounty() != null) {
                Element countyElement = new Element(DfEAddress.ELEMENT_COUNTY);
                countyElement.addContent(dfEAddress.getCounty());
                addressElement.addContent(countyElement);
            }
            if (dfEAddress.getPostCode() != null) {
                Element postCodeElement = new Element(DfEAddress.ELEMENT_POST_CODE);
                postCodeElement.addContent(dfEAddress.getPostCode());
                addressElement.addContent(postCodeElement);
            }
            if (dfEAddress.getZip() != null) {
                Element zipElement = new Element(DfEAddress.ELEMENT_ZIP);
                zipElement.addContent(dfEAddress.getZip());
                addressElement.addContent(zipElement);
            }
            if (dfEAddress.getCountry() != null) {
                Element countryElement = new Element(DfEAddress.ELEMENT_COUNTRY);
                countryElement.addContent(dfEAddress.getCountry());
                addressElement.addContent(countryElement);
            }
            if (dfEAddress.getEasting() != null) {
                Element eastingElement = new Element(DfEAddress.ELEMENT_EASTING);
                eastingElement.addContent(dfEAddress.getEasting());
                addressElement.addContent(eastingElement);
            }
            if (dfEAddress.getNorthing() != null) {
                Element northingElement = new Element(DfEAddress.ELEMENT_NORTHING);
                northingElement.addContent(dfEAddress.getNorthing());
                addressElement.addContent(northingElement);
            }

            return addressElement;
        }

        /**
         * Get a Admissions Element from a DfEPupil.
         *
         * @return Element
         */
        public Element getAdmissionsElement() {
            // Setting default to "true" because in Aspen a student has already been admitted as a
            // result of being Enrolled.
            Element admissionsElement = new Element(DfEPupil.ELEMENT_ADMISSIONS);
            Element acceptElement = new Element(DfEPupil.ELEMENT_ACCEPT);
            acceptElement.addContent(STRING_TRUE);
            admissionsElement.addContent(acceptElement);

            return admissionsElement;
        }

        /**
         * Get all Attendances for a Pupil.
         *
         * @param student Student
         * @param attendanceHistories Collection<StudentSchool>
         * @param attendances Collection<StudentAttendance>
         * @param attendancesByDate Map<PlainDate,StudentAttendance>
         * @param attendanceXML String
         * @return ArrayList<DfEAttendance>
         */
        public ArrayList<DfEAttendance> getAttendance(Student student,
                                                      Collection<StudentSchool> attendanceHistories,
                                                      Collection<StudentAttendance> attendances,
                                                      Map<PlainDate, StudentAttendance> attendancesByDate,
                                                      String attendanceXML) {
            ArrayList<DfEAttendance> dfeAttendances = new ArrayList<DfEAttendance>();
            DfEAttendance dfeAttendance = null;

            // TODO Remove later
            // All previously imported CTF attendances and add to attendances list
            /*
             * if (!StringUtils.isEmpty(attendanceXML))
             * {
             * ArrayList<DfEAttendance> oldAttendances = parseAttendanceXML(attendanceXML);
             *
             * if (oldAttendances != null && oldAttendances.size() > 0)
             * {
             * dfeAttendances.addAll(oldAttendances);
             * }
             * }
             */

            // Retrieve Student Attendance data that was imported from a student's older schools
            // before entry into the Aspen system
            for (StudentSchool studentSchool : attendanceHistories) {
                String schoolyear = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_YEAR);
                String lEA = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_LEA);
                String estab = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_ESTAB);
                String schoolName = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SCHOOL_NAME);
                String sessionsPossibleStr =
                        (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_POSSIBLE);
                int sessionsPossible = 0;
                if (!StringUtils.isEmpty(sessionsPossibleStr) && StringUtils.isNumeric(sessionsPossibleStr)) {
                    sessionsPossible = Integer.parseInt(sessionsPossibleStr);
                }
                String sessionsAuthorisedStr =
                        (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_AUTHORIZED);
                int sessionsAuthorised = 0;
                if (!StringUtils.isEmpty(sessionsAuthorisedStr) && StringUtils.isNumeric(sessionsAuthorisedStr)) {
                    sessionsAuthorised = Integer.parseInt(sessionsAuthorisedStr);
                }
                String sessionsAttendedStr =
                        (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_ATTENDED);
                int sessionsAttended = 0;
                if (!StringUtils.isEmpty(sessionsAttendedStr) && StringUtils.isNumeric(sessionsAttendedStr)) {
                    sessionsAttended = Integer.parseInt(sessionsAttendedStr);
                }
                String sessionsUnauthorisedStr =
                        (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_UNAUTHORIZED);
                int sessionsUnauthorised = 0;
                if (!StringUtils.isEmpty(sessionsUnauthorisedStr) && StringUtils.isNumeric(sessionsUnauthorisedStr)) {
                    sessionsUnauthorised = Integer.parseInt(sessionsUnauthorisedStr);
                }
                String attendanceStartDateStr =
                        (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_START_DATE);
                String attendanceMarks = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_MARKS);

                dfeAttendance =
                        new DfEAttendance(schoolyear, lEA, estab, schoolName, sessionsPossible, sessionsAuthorised,
                                sessionsAttended, sessionsUnauthorised, attendanceStartDateStr, attendanceMarks);
                dfeAttendances.add(dfeAttendance);
            }


            // Retrieve Student Attendance data that has been added since the student has entered
            // the system
            Calendar calendar = Calendar.getInstance();
            AttendanceStatistics attendanceStatistics;

            char presentAMSessionChar = ATTENDANCE_MARK_PRESENT_AM_SESSION.charAt(0);
            char presentPMSessionChar = ATTENDANCE_MARK_PRESENT_PM_SESSION.charAt(0);

            Map<String, Object> parametersMap = new HashMap<String, Object>();
            parametersMap.put("queryBy1", "oid");
            parametersMap.put("queryString1", student.getOid());

            // Prepare data source.
            attendanceStatistics = new AttendanceStatistics();
            attendanceStatistics.setBroker(m_broker);
            attendanceStatistics.setOrganization(student.getSchool().getParentOrganization());
            attendanceStatistics.setSchoolContext(false);
            attendanceStatistics.setParameters(parametersMap);
            try {
                attendanceStatistics.initializeExport();
            } catch (X2BaseException e) {
                e.printStackTrace();
            }

            if (attendances != null) {
                for (ArrayList<PlainDate> dates : m_schoolYearRanges) {
                    PlainDate startDate = dates.get(0);
                    PlainDate endDate = dates.get(1);

                    String attendanceMarks = STRING_EMPTY;
                    PlainDate attendanceStartDate = null;
                    boolean findAttendanceStartDate = true;
                    int sessionsPossible = 0;
                    int sessionsAttended = 0;
                    int sessionsAuthorised = 0;
                    int sessionsUnauthorised = 0;
                    SisSchool school = null;

                    if (startDate != null) {
                        calendar.setTime(startDate);

                        List<StudentEnrollmentSpan> spans =
                                attendanceStatistics.m_helper.getStudentEnrollmentSpans(student, false);

                        for (StudentEnrollmentSpan span : spans) {
                            PlainDate firstActiveDate = null;
                            if (span.getFirstActiveEnrollment() != null) {
                                firstActiveDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                            }

                            if (firstActiveDate == null) {
                                firstActiveDate = startDate;
                            } else if (firstActiveDate.before(startDate)) {
                                firstActiveDate = startDate;
                            }

                            PlainDate lastActiveDate = null;
                            if (span.getFirstInactiveEnrollment() != null) {
                                lastActiveDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
                            }

                            if (lastActiveDate == null) {
                                lastActiveDate = endDate;
                            } else if (lastActiveDate.after(endDate)) {
                                lastActiveDate = endDate;
                            }

                            if (findAttendanceStartDate) {
                                attendanceStartDate = firstActiveDate;

                                if (attendanceStartDate == null) {
                                    attendanceStartDate = startDate;
                                }

                                findAttendanceStartDate = false;
                            }

                            if (firstActiveDate != null) {
                                attendanceMarks +=
                                        getAttendanceMarks(student, firstActiveDate, lastActiveDate, attendancesByDate);

                                Collection<PlainDate> inSessionDates =
                                        CalendarManager.getInSessionDates(firstActiveDate,
                                                lastActiveDate, (SisStudent) student, m_broker);

                                for (PlainDate date : inSessionDates) {
                                    if (!date.before(firstActiveDate) && !date.after(lastActiveDate)) {
                                        sessionsPossible += 2;
                                    }
                                }
                            } else {
                                attendanceMarks +=
                                        getAttendanceMarks(student, startDate, lastActiveDate, attendancesByDate);
                            }

                            for (StudentAttendance attendance : attendances) {
                                PlainDate attendanceDate = attendance.getDate();
                                if (school == null) {
                                    school = attendance.getSchool();
                                }

                                if ((attendanceDate.after(startDate) || attendanceDate.equals(startDate)) &&
                                        (attendanceDate.before(endDate) || attendanceDate.equals(endDate))) {
                                    /*
                                     * ------------------------------------------
                                     * AM
                                     */
                                    String otherCodeAM = attendance.getOtherCode();
                                    if (VAL_AUTHORISED_CODE_SET.contains(otherCodeAM)) {
                                        sessionsAuthorised++;
                                    } else if (VAL_UNAUTHORISED_CODE_SET.contains(otherCodeAM)) {
                                        sessionsUnauthorised++;
                                    }

                                    /*
                                     * -------------------------------------------
                                     * PM
                                     */
                                    String otherCodePM = attendance.getOtherCode02();
                                    if (VAL_AUTHORISED_CODE_SET.contains(otherCodePM)) {
                                        sessionsAuthorised++;
                                    } else if (VAL_UNAUTHORISED_CODE_SET.contains(otherCodePM)) {
                                        sessionsUnauthorised++;
                                    }
                                }
                            }
                        }
                    }

                    if (!StringUtils.isEmpty(attendanceMarks)) {
                        for (int i = 0; i < attendanceMarks.length(); i++) {
                            if (attendanceMarks.charAt(i) == presentAMSessionChar
                                    || attendanceMarks.charAt(i) == presentPMSessionChar) {
                                sessionsAttended++;
                            }
                        }
                    }

                    if (school == null) {
                        school = (SisSchool) student.getSchool();
                    }
                    Organization organizaion = school.getOrganization1();
                    String schoolyear = Integer.toString(calendar.get(Calendar.YEAR));
                    String lEA = (String) organizaion.getFieldValueByAlias(ALIAS_NAME_LEA, getDataDictionary());
                    String estab = (String) school.getFieldValueByAlias(ALIAS_NAME_ESTAB, getDataDictionary());
                    String schoolName = school.getName();
                    String attendanceStartDateStr = null;
                    if (attendanceStartDate != null) {
                        attendanceStartDateStr = m_dateFormat.format(attendanceStartDate);
                    }

                    dfeAttendance =
                            new DfEAttendance(schoolyear, lEA, estab, schoolName, sessionsPossible, sessionsAuthorised,
                                    sessionsAttended, sessionsUnauthorised, attendanceStartDateStr, attendanceMarks);
                    dfeAttendances.add(dfeAttendance);
                }
            }

            return dfeAttendances;
        }

        /**
         * Get a Attendance Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @param includeSessions boolean
         * @return Element
         */
        public Element getAttendanceElement(DfEPupil dfEPupil, boolean includeSessions) {
            Element studentAttendanceElement = null;
            Element attendSessionsElement = null;

            if (dfEPupil.getAttendances() != null && dfEPupil.getAttendances().size() > 0) {
                studentAttendanceElement = new Element(DfEPupil.ELEMENT_ATTENDANCE);

                ArrayList<DfEAttendance> attendances = dfEPupil.getAttendances();

                // Sort by Year, LEA, Estab and School Name
                TreeMap<String, DfEAttendance> attendancesMap = new TreeMap<String, DfEAttendance>();
                for (DfEAttendance dfEAttendance : attendances) {
                    String key = dfEAttendance.getYear() + ":" + dfEAttendance.getLEA() + ":" + dfEAttendance.getEstab()
                            + ":" + dfEAttendance.getSchoolName();
                    attendancesMap.put(key, dfEAttendance);
                }

                for (DfEAttendance dfEAttendance : attendancesMap.values()) {
                    Element yearDataElement = new Element(DfEAttendance.ELEMENT_YEAR_DATA);

                    if (dfEAttendance.getSessionsPossible() > 0) {
                        if (dfEAttendance.getYear() != null) {
                            Element yearElement = new Element(DfEAttendance.ELEMENT_YEAR);
                            yearElement.addContent(dfEAttendance.getYear());
                            yearDataElement.addContent(yearElement);
                        }
                        if (dfEAttendance.getLEA() != null) {
                            Element leaElement = new Element(DfEAttendance.ELEMENT_LEA);
                            leaElement.addContent(dfEAttendance.getLEA());
                            yearDataElement.addContent(leaElement);
                        }
                        if (dfEAttendance.getEstab() != null) {
                            Element estabElement = new Element(DfEAttendance.ELEMENT_ESTAB);
                            estabElement.addContent(dfEAttendance.getEstab());
                            yearDataElement.addContent(estabElement);
                        }
                        if (dfEAttendance.getSchoolName() != null) {
                            Element schoolNameElement = new Element(DfEAttendance.ELEMENT_SCHOOL_NAME);
                            schoolNameElement.addContent(dfEAttendance.getSchoolName());
                            yearDataElement.addContent(schoolNameElement);
                        }
                        if (dfEAttendance.getSessionsPossible() >= 0) {
                            Element sessionsPossibleElement = new Element(DfEAttendance.ELEMENT_SESSIONS_POSSIBLE);
                            sessionsPossibleElement.addContent(String.valueOf(dfEAttendance.getSessionsPossible()));
                            yearDataElement.addContent(sessionsPossibleElement);
                        }
                        if (dfEAttendance.getSessionsAuthorised() >= 0) {
                            Element sessionsAuthorisedElement = new Element(DfEAttendance.ELEMENT_SESSIONS_AUTHORISED);
                            sessionsAuthorisedElement.addContent(String.valueOf(dfEAttendance.getSessionsAuthorised()));
                            yearDataElement.addContent(sessionsAuthorisedElement);
                        }
                        if (dfEAttendance.getSessionsAttended() >= 0) {
                            Element sessionsAttendedElement = new Element(DfEAttendance.ELEMENT_SESSIONS_ATTENDED);
                            sessionsAttendedElement.addContent(String.valueOf(dfEAttendance.getSessionsAttended()));
                            yearDataElement.addContent(sessionsAttendedElement);
                        }
                        if (dfEAttendance.getSessionsUnauthorised() >= 0) {
                            Element sessionsUnauthorisedElement =
                                    new Element(DfEAttendance.ELEMENT_SESSIONS_UNAUTHORISED);
                            sessionsUnauthorisedElement
                                    .addContent(String.valueOf(dfEAttendance.getSessionsUnauthorised()));
                            yearDataElement.addContent(sessionsUnauthorisedElement);
                        }

                        if (includeSessions) {
                            attendSessionsElement = new Element(DfEAttendance.ELEMENT_ATTEND_SESSIONS);

                            if (dfEAttendance.getAttendanceStartDate() != null) {
                                Element attendanceStartDateElement =
                                        new Element(DfEAttendance.ELEMENT_ATTEND_START_DATE);
                                attendanceStartDateElement
                                        .addContent(m_dateFormat.format(dfEAttendance.getAttendanceStartDate()));
                                attendSessionsElement.addContent(attendanceStartDateElement);
                            }
                            if (dfEAttendance.getAttendanceMarks() != null) {
                                Element attendanceMarksElement = new Element(DfEAttendance.ELEMENT_ATTEND_MARKS);
                                attendanceMarksElement.addContent(dfEAttendance.getAttendanceMarks());
                                attendSessionsElement.addContent(attendanceMarksElement);
                            }

                            if (dfEAttendance.getAttendanceStartDate() != null
                                    || dfEAttendance.getAttendanceMarks() != null) {
                                yearDataElement.addContent(attendSessionsElement);
                            }
                        }

                        studentAttendanceElement.addContent(yearDataElement);
                    }
                }
            }

            return studentAttendanceElement;
        }

        /**
         * Gets the attendance marks for a student between a two date parameters.
         *
         * @param student Student
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param attendancesByDate Map<PlainDate,StudentAttendance>
         * @return attendanceMarks
         */
        public String getAttendanceMarks(Student student,
                                         PlainDate startDate,
                                         PlainDate endDate,
                                         Map<PlainDate, StudentAttendance> attendancesByDate) {
            Criteria criteria = new Criteria();
            criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, startDate);
            criteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, endDate);
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER
                    + SchoolCalendar.COL_SCHOOL_OID, student.getSchoolOid());

            QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
            query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

            QueryIterator iterator = m_broker.getIteratorByQuery(query);

            StringBuffer attendanceMarks = new StringBuffer(STRING_EMPTY);
            while (iterator.hasNext()) {
                SchoolCalendarDate scd = (SchoolCalendarDate) iterator.next();
                PlainDate date = scd.getDate();
                boolean inSession = scd.getInSessionIndicator();

                if (!inSession) {
                    attendanceMarks.append(ATTENDANCE_MARK_NOT_IN_SESSION + ATTENDANCE_MARK_NOT_IN_SESSION);
                } else if (attendancesByDate != null && attendancesByDate.containsKey(date) && inSession) {
                    StudentAttendance attendance = attendancesByDate.get(date);

                    if (!StringUtils.isEmpty(attendance.getOtherCode())) {
                        attendanceMarks.append(attendance.getOtherCode());
                    } else {
                        attendanceMarks.append(ATTENDANCE_MARK_PRESENT_AM_SESSION);
                    }

                    if (!StringUtils.isEmpty(attendance.getOtherCode02())) {
                        attendanceMarks.append(attendance.getOtherCode02());
                    } else {
                        attendanceMarks.append(ATTENDANCE_MARK_PRESENT_PM_SESSION);
                    }
                } else {
                    attendanceMarks.append(ATTENDANCE_MARK_PRESENT_AM_SESSION + ATTENDANCE_MARK_PRESENT_PM_SESSION);
                }
            }
            iterator.close();

            return attendanceMarks.toString();
        }

        /**
         * Get a Pupil BasicDetails Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getBasicDetailsElement(DfEPupil dfEPupil) {
            // Basic Details Element Start
            Element basicDetailsElement = new Element(DfEPupil.ELEMENT_BASIC_DETAILS);
            if (dfEPupil.getFormerUniquePupilNumber() != null) {
                Element formerUniquePupilNumberElement = new Element(DfEPupil.ELEMENT_FORMER_UNIQUE_PUPIL_NUMBER);
                formerUniquePupilNumberElement.addContent(dfEPupil.getFormerUniquePupilNumber());
                basicDetailsElement.addContent(formerUniquePupilNumberElement);
            }
            if (dfEPupil.getPreferredSurname() != null) {
                Element preferredSurnameElement = new Element(DfEPupil.ELEMENT_PREFERRED_SURNAME);
                preferredSurnameElement.addContent(dfEPupil.getPreferredSurname());
                basicDetailsElement.addContent(preferredSurnameElement);
            }
            if (dfEPupil.getPreferredForename() != null) {
                Element preferredForenameElement = new Element(DfEPupil.ELEMENT_PREFERRED_FORENAME);
                preferredForenameElement.addContent(dfEPupil.getPreferredForename());
                basicDetailsElement.addContent(preferredForenameElement);
            }
            if (dfEPupil.getFormerSurname() != null) {
                Element formerSurnameElement = new Element(DfEPupil.ELEMENT_FORMER_SURNAME);
                formerSurnameElement.addContent(dfEPupil.getFormerSurname());
                basicDetailsElement.addContent(formerSurnameElement);
            }
            if (dfEPupil.getFormerForename() != null) {
                Element formerForenameElement = new Element(DfEPupil.ELEMENT_FORMER_FORENAME);
                formerForenameElement.addContent(dfEPupil.getFormerForename());
                basicDetailsElement.addContent(formerForenameElement);
            }
            if (dfEPupil.getMiddleNames() != null) {
                Element middleNamesElement = new Element(DfEPupil.ELEMENT_MIDDLE_NAMES);
                middleNamesElement.addContent(dfEPupil.getMiddleNames());
                basicDetailsElement.addContent(middleNamesElement);
            }
            if (dfEPupil.getNCYearActual() != null) {
                Element nCyearActualElement = new Element(DfEPupil.ELEMENT_NC_YEAR_ACTUAL);
                nCyearActualElement.addContent(dfEPupil.getNCYearActual());
                basicDetailsElement.addContent(nCyearActualElement);
            }
            if (dfEPupil.getEthnicity() != null) {
                Element ethnicityElement = new Element(DfEPupil.ELEMENT_ETHINICITY);
                ethnicityElement.addContent(dfEPupil.getEthnicity());
                basicDetailsElement.addContent(ethnicityElement);
            }
            if (dfEPupil.getEthnicitySource() != null) {
                Element ethnicitySourceElement = new Element(DfEPupil.ELEMENT_ETHINICITY_SOURCE);
                ethnicitySourceElement.addContent(dfEPupil.getEthnicitySource());
                basicDetailsElement.addContent(ethnicitySourceElement);
            }

            // Languages
            if (dfEPupil.getLanguages() != null && dfEPupil.getLanguages().size() > 0) {
                Element languagesElement = getLanguagesElement(dfEPupil.getLanguages());
                basicDetailsElement.addContent(languagesElement);
            }

            if (dfEPupil.getMedicalFlag() != null) {
                Element medicalFlagElement = new Element(DfEPupil.ELEMENT_MEDICAL_FLAG);
                Boolean medicalFlag = dfEPupil.getMedicalFlag();
                if (medicalFlag.booleanValue()) {
                    medicalFlagElement.addContent(STRING_TRUE);
                } else {
                    medicalFlagElement.addContent(STRING_FALSE);
                }
                basicDetailsElement.addContent(medicalFlagElement);
            }

            // Disabilities
            if (dfEPupil.getDisabilities() != null && dfEPupil.getDisabilities().size() > 0) {
                Element disabilitiesElement = getDisabilitiesElement(dfEPupil.getDisabilities());
                basicDetailsElement.addContent(disabilitiesElement);
            }

            if (dfEPupil.getEnrollStatus() != null) {
                Element enrollStatusElement = new Element(DfEPupil.ELEMENT_ENROLL_STATUS);
                enrollStatusElement.addContent(dfEPupil.getEnrollStatus());
                basicDetailsElement.addContent(enrollStatusElement);
            }

            return basicDetailsElement;
        }

        /**
         * Get a Contacts Element from Collection<DfEContact> .
         *
         * @param contactList Collection<DfEContact>
         * @return Element
         */
        public Element getContacts(Collection<DfEContact> contactList) {
            Element contactsElement = new Element(DfEPupil.ELEMENT_CONTACTS);

            // Make sure the contacts are in order
            TreeMap<String, DfEContact> contactsMap = new TreeMap<String, DfEContact>();
            for (DfEContact dfEContact : contactList) {
                Integer order = Integer.valueOf(dfEContact.getOrder());
                String key = String.format("%03d", order) + dfEContact.getSurname() + dfEContact.getForename()
                        + dfEContact.getRelationshipCode();
                contactsMap.put(key, dfEContact);
            }
            Collection<DfEContact> contacts = contactsMap.values();

            for (DfEContact dfEContact : contacts) {
                Element contactElement = new Element(DfEPupil.ELEMENT_CONTACT);

                if (dfEContact.getOrder() != 0) {
                    Element orderElement = new Element(DfEContact.ELEMENT_ORDER);
                    orderElement.addContent(Integer.toString(dfEContact.getOrder()));
                    contactElement.addContent(orderElement);
                }
                if (dfEContact.getTitle() != null) {
                    Element titleElement = new Element(DfEContact.ELEMENT_TITLE);
                    titleElement.addContent(dfEContact.getTitle());
                    contactElement.addContent(titleElement);
                }
                if (dfEContact.getSurname() != null) {
                    Element surnameElement = new Element(DfEContact.ELEMENT_SURNAME);
                    surnameElement.addContent(dfEContact.getSurname());
                    contactElement.addContent(surnameElement);
                }
                if (dfEContact.getForename() != null) {
                    Element forenameElement = new Element(DfEContact.ELEMENT_FORENAME);
                    forenameElement.addContent(dfEContact.getForename());
                    contactElement.addContent(forenameElement);
                }
                if (dfEContact.getGender() != null) {
                    Element genderElement = new Element(DfEContact.ELEMENT_GENDER);
                    genderElement.addContent(dfEContact.getGender());
                    contactElement.addContent(genderElement);
                }
                if (dfEContact.getRelationshipCode() != null) {
                    Element relationshipCodeElement = new Element(DfEContact.ELEMENT_RELATIONSHIP);
                    relationshipCodeElement.addContent(dfEContact.getRelationshipCode());
                    contactElement.addContent(relationshipCodeElement);

                    Element responsibilityElement = new Element(DfEContact.ELEMENT_RESPONSIBILITY);
                    if (dfEContact.getResponsible() != null && dfEContact.getResponsible().booleanValue() == true) {
                        responsibilityElement.addContent(STRING_TRUE);
                    } else {
                        responsibilityElement.addContent(STRING_FALSE);
                    }
                    contactElement.addContent(responsibilityElement);
                }

                // Contact Address
                if (dfEContact.getAddressAsPupil() != null && dfEContact.getAddressAsPupil().booleanValue() == true) {
                    Element addressElement = new Element(DfEContact.ELEMENT_ADDRESS);

                    Element addressAsPupilElement = new Element(DfEContact.ELEMENT_ADDRESS_AS_PUPIL);
                    addressAsPupilElement.addContent(STRING_TRUE);
                    addressElement.addContent(addressAsPupilElement);

                    contactElement.addContent(addressElement);
                } else {
                    if (dfEContact.getDfEAddress() != null) {
                        Element addressElement = getAddressElement(dfEContact.getDfEAddress());
                        contactElement.addContent(addressElement);
                    }
                }

                // Telephone
                if (dfEContact.getTelephones() != null && dfEContact.getTelephones().size() > 0) {
                    Element phonesElement = getPhonesElement(dfEContact.getTelephones());
                    contactElement.addContent(phonesElement);
                }

                if (!StringUtils.isEmpty(dfEContact.getEmail())) {
                    Element emailElement = new Element(DfEContact.ELEMENT_EMAIL);
                    emailElement.addContent(dfEContact.getEmail());
                    contactElement.addContent(emailElement);
                }


                contactsElement.addContent(contactElement);
            }

            return contactsElement;
        }

        /**
         * Get the DfEHeader.
         *
         * @param headerType String
         * @param sourceLEA String
         * @param sourceEstab String
         * @param schoolName String
         * @param sourceSchoolAcademicYear String
         * @param destLEA String
         * @param destEstab String
         * @return DfECTFHeader
         */
        public DfEHeader getDfEHeader(String headerType,
                                      String sourceLEA,
                                      String sourceEstab,
                                      String schoolName,
                                      String sourceSchoolAcademicYear,
                                      String destLEA,
                                      String destEstab) {
            DfEHeader dfEHeader = new DfEHeader();
            dfEHeader.setHeaderType(headerType);
            if (REPORT_TYPE_CTF.equals(headerType)) {
                dfEHeader.setDocumentName(DfEHeader.CTF_DOCUMENT_NAME);
                dfEHeader.setVersion(DfEHeader.CTF_VERSION);
            } else if (REPORT_TYPE_ATF.equals(headerType)) {
                dfEHeader.setDocumentName(DfEHeader.ATF_DOCUMENT_NAME);
                dfEHeader.setVersion(DfEHeader.ATF_VERSION);
            }
            Date now = new Date();
            String dateTime = m_dateFormat.format(now) + DFE_DATA_TIME_DELIMITER + m_timeFormat.format(now);
            dfEHeader.setDateTime(dateTime);
            dfEHeader.setDocumentQualifier(DfEHeader.DEFAULT_DOCUMENT_QUALIFIER);
            dfEHeader.setSupplierID(DfEHeader.DEFAULT_SUPPLIER_ID);
            dfEHeader.setSourceSchoolLEA(sourceLEA);
            dfEHeader.setSourceSchoolEstab(sourceEstab);
            dfEHeader.setSourceSchoolName(schoolName);
            dfEHeader.setSourceSchoolAcademicYear(sourceSchoolAcademicYear);
            dfEHeader.setDestSchoolLEA(destLEA);
            dfEHeader.setDestSchoolEstab(destEstab);

            return dfEHeader;
        }

        /**
         * Get DfEPupil object from the Student information from the DB
         *
         * Used in ATF Export.
         *
         * @param student SisStudent
         * @param enrollments Collection<StudentEnrollment>
         * @return DfEPupil
         */
        public DfEPupil getDfEPupilByStudent(SisStudent student, Collection<StudentEnrollment> enrollments) {
            // Provide empty collections
            Collection<StudentAssessment> studentAssessments = new ArrayList<StudentAssessment>();
            Collection<StudentSchool> studentAttendanceHistories = new ArrayList<StudentSchool>();
            Collection<StudentAttendance> studentAttendances = new ArrayList<StudentAttendance>();
            Map<PlainDate, StudentAttendance> attendancesByDate = new HashMap<PlainDate, StudentAttendance>();

            return getDfEPupilByStudent(student, enrollments, studentAssessments, studentAttendanceHistories,
                    studentAttendances, attendancesByDate);
        }

        /**
         * Get DfEPupil object from the Student information from the DB
         *
         * Used in CTF Export.
         *
         * @param student SisStudent
         * @param enrollments Collection<StudentEnrollment>
         * @param assessments Collection<StudentAssessment>
         * @param attendanceHistories Collection<StudentSchool>
         * @param attendances Collection<StudentAttendance>
         * @param attendancesByDate Map<PlainDate,StudentAttendance>
         * @return DfEPupil
         */
        public DfEPupil getDfEPupilByStudent(SisStudent student,
                                             Collection<StudentEnrollment> enrollments,
                                             Collection<StudentAssessment> assessments,
                                             Collection<StudentSchool> attendanceHistories,
                                             Collection<StudentAttendance> attendances,
                                             Map<PlainDate, StudentAttendance> attendancesByDate) {
            // Get Student
            Person studentPerson = student.getPerson();

            DfEPupil dfEPupil = new DfEPupil();
            String applicationReference =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_APPLICATION_REFERENCE, getDataDictionary());
            if (!StringUtils.isEmpty(applicationReference)) {
                dfEPupil.setApplicationReference(applicationReference);
            }
            String uniquePupilNumber = (String) student.getFieldValueByAlias(ALIAS_NAME_UPN, getDataDictionary());
            if (!StringUtils.isEmpty(uniquePupilNumber)) {
                dfEPupil.setUniquePupilNumber(uniquePupilNumber);
            }
            String uniqueLearningNumber = (String) student.getFieldValueByAlias(ALIAS_NAME_ULN, getDataDictionary());
            if (!StringUtils.isEmpty(uniqueLearningNumber)) {
                dfEPupil.setUniqueLearnerNumber(uniqueLearningNumber);
            }
            String uniqueCandidateIdentifier =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_UCI, getDataDictionary());
            if (!StringUtils.isEmpty(uniqueCandidateIdentifier)) {
                dfEPupil.setUniqueCandidateIdentifier(uniqueCandidateIdentifier);
            }
            if (!StringUtils.isEmpty(studentPerson.getLastName())) {
                dfEPupil.setSurname(studentPerson.getLastName());
            }
            if (!StringUtils.isEmpty(studentPerson.getFirstName())) {
                dfEPupil.setForename(studentPerson.getFirstName());
            }
            if (studentPerson.getDob() != null) {
                dfEPupil.setBirthDate(studentPerson.getDob());
            }
            if (!StringUtils.isEmpty(studentPerson.getGenderCode())) {
                dfEPupil.setGender(studentPerson.getGenderCode());
            }
            String formerUniquePupilNumber =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_FORMER_UPN, getDataDictionary());
            if (!StringUtils.isEmpty(formerUniquePupilNumber)) {
                dfEPupil.setFormerUniquePupilNumber(formerUniquePupilNumber);
            }
            String preferredSurname =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_PREFERRED_SURNAME, getDataDictionary());
            if (!StringUtils.isEmpty(preferredSurname)) {
                dfEPupil.setPreferredSurname(preferredSurname);
            }
            String preferredForename =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_PREFERRED_FORENAME, getDataDictionary());
            if (!StringUtils.isEmpty(preferredForename)) {
                dfEPupil.setPreferredForename(preferredForename);
            }
            String formerSurname =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_FORMER_SURNAME, getDataDictionary());
            if (!StringUtils.isEmpty(formerSurname)) {
                dfEPupil.setFormerSurname(formerSurname);
            }
            String formerForename =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_FORMER_FORENAME, getDataDictionary());
            if (!StringUtils.isEmpty(formerForename)) {
                dfEPupil.setFormerForename(formerForename);
            }
            if (!StringUtils.isEmpty(studentPerson.getMiddleName())) {
                dfEPupil.setMiddleNames(studentPerson.getMiddleName());
            }
            dfEPupil.setNCYearActual(student.getGradeLevel());

            // No translation of DfE Ethnicity Code
            String ethnicity = (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_ETHNICITY, getDataDictionary());
            if (!StringUtils.isEmpty(ethnicity)) {
                dfEPupil.setEthnicity(ethnicity);
            }
            String ethnicitySource =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_ETHNICITY_SOURCE, getDataDictionary());
            if (!StringUtils.isEmpty(ethnicitySource)) {
                String ethnicitySourceFieldName = translateAliasToJavaName(ALIAS_NAME_ETHNICITY_SOURCE, true);
                if (ethnicitySourceFieldName != null) {
                    String ethnicitySourceStateCode =
                            lookupStateValueByRefCode(Person.class, ethnicitySourceFieldName, ethnicitySource);
                    dfEPupil.setEthnicitySource(ethnicitySourceStateCode);
                }
            }

            // Setting Medical Flag
            String medicalFlagStr =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_MEDICAL_FLAG, getDataDictionary());
            if (DB_TRUE.equals(medicalFlagStr)) {
                dfEPupil.setMedicalFlag(BOOLEAN_TRUE);
            } else {
                dfEPupil.setMedicalFlag(BOOLEAN_FALSE);
            }

            // Set Language
            // Translate Aspen Language Code to DfE Language Code.
            String languageCode = lookupStateValueByRefCode(Student.class, Student.COL_HOME_LANGUAGE_CODE,
                    student.getHomeLanguageCode());
            DfELanguage dfELanguage = new DfELanguage(DfELanguage.DEFAULT_LANGUAGE_TYPE, languageCode);
            dfEPupil.addDfELanguage(dfELanguage);

            // Set FSM Review Date
            String fSMReviewDateStr =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_FSM_REVIEW_DATE, getDataDictionary());
            if (!StringUtils.isEmpty(fSMReviewDateStr)) {
                dfEPupil.setFSMReviewDate(fSMReviewDateStr);
            }

            // Set Disabilities
            Collection<IepDisability> studentDisabilities = student.getIepDisability();
            if (studentDisabilities.size() > 0) {
                for (IepDisability disability : studentDisabilities) {
                    if (!StringUtils.isEmpty(disability.getDisabilityCode())) {
                        // Translate Aspen Language Code to DfE Language Code.
                        String disabilityCode = lookupStateValueByRefCode(IepDisability.class,
                                IepDisability.COL_DISABILITY_CODE, disability.getDisabilityCode());
                        dfEPupil.addDisability(disabilityCode);
                    }
                }
            }

            // Translate Aspen Language Code to DfE Language Code.
            String enrollStatus = lookupStateValueByRefCode(Student.class, Student.COL_ENROLLMENT_STATUS,
                    student.getEnrollmentStatus());
            dfEPupil.setEnrollStatus(enrollStatus);

            // Looked After
            String inCare = (String) student.getFieldValueByAlias(ALIAS_NAME_IN_CARE, getDataDictionary());
            if (!StringUtils.isEmpty(inCare)) {
                if (DB_TRUE.equalsIgnoreCase(inCare) || STRING_TRUE.equalsIgnoreCase(inCare)) {
                    dfEPupil.setInCare(BOOLEAN_TRUE);
                } else {
                    dfEPupil.setInCare(BOOLEAN_FALSE);
                }
                // No Translation of DfE Care Authority Code
                String careAuthority =
                        (String) student.getFieldValueByAlias(ALIAS_NAME_CARE_AUTHORITY, getDataDictionary());
                if (!StringUtils.isEmpty(careAuthority)) {
                    dfEPupil.setCareAuthority(careAuthority);
                }
            }

            Collection<StudentProgramParticipation> studentPrograms = student.getProgramParticipation(m_broker);
            if (studentPrograms.size() > 0) {
                // FSM
                for (StudentProgramParticipation studentProgramParticipation : studentPrograms) {
                    String programCode = studentProgramParticipation.getProgramCode();
                    if (ASPEN_PROGRAM_CODE_FRL.equals(programCode)) {
                        PlainDate startDate = studentProgramParticipation.getStartDate();
                        PlainDate endDate = studentProgramParticipation.getEndDate();
                        // Translation DfE FSM UK Country Code
                        String fSMUKCountry = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_FSM_UK_COUNTRY, getDataDictionary());
                        String uKCountryFieldName = translateAliasToJavaName(ALIAS_NAME_FSM_UK_COUNTRY, true);
                        String fSMUKCountryStateCode = null;
                        if (uKCountryFieldName != null) {
                            fSMUKCountryStateCode = lookupStateValueByRefCode(StudentProgramParticipation.class,
                                    uKCountryFieldName, fSMUKCountry);
                        }

                        DfEFSMInstance dfEFSMInstance = new DfEFSMInstance(startDate, endDate, fSMUKCountryStateCode);

                        dfEPupil.addFSMInstance(dfEFSMInstance);
                    }
                }

                // NAW
                for (StudentProgramParticipation studentProgramParticipation : studentPrograms) {
                    String programCode = studentProgramParticipation.getProgramCode();
                    if (ASPEN_PROGRAM_CODE_NAW.equals(programCode)) {
                        String speakWelsh = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_SPEAK_WELSH, getDataDictionary());
                        String homeWelsh = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_HOME_WELSH, getDataDictionary());
                        // Translation DfE National Identity Code
                        String nationalIdentity = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_NATIONAL_IDENTITY, getDataDictionary());
                        String nationalIdentityFieldName = translateAliasToJavaName(ALIAS_NAME_NATIONAL_IDENTITY, true);
                        String nationalIdentityStateCode = null;
                        if (nationalIdentityFieldName != null) {
                            nationalIdentityStateCode = lookupStateValueByRefCode(StudentProgramParticipation.class,
                                    nationalIdentityFieldName, nationalIdentity);
                        }
                        String welshSource = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_WELSH_SOURCE, getDataDictionary());
                        String languageSource = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_EAL_ACQUISITION, getDataDictionary());
                        String eALAcquisition = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_LANGUAGE_SOURCE, getDataDictionary());
                        String sENCurrTeachingMethods = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_SEN_CURR_TEACH_METHOD, getDataDictionary());
                        String sENGroupingAndSupport = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_SEN_GROUPING_SUPPORT, getDataDictionary());
                        String sENSpecialisedResources = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_SEN_SPEC_RESOURCES, getDataDictionary());
                        String sENAdviceAndAssessment = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_SEN_ADVICE_ASSESSMENT, getDataDictionary());

                        DfENAWDetail dfENAWDetail = new DfENAWDetail(speakWelsh, homeWelsh, nationalIdentityStateCode,
                                welshSource, languageSource, eALAcquisition, sENCurrTeachingMethods,
                                sENGroupingAndSupport, sENSpecialisedResources, sENAdviceAndAssessment);
                        dfEPupil.addNAWDetail(dfENAWDetail);
                    }
                }

                // SEN
                PlainDate sENStartDate = null;
                String sENProvision = null;
                for (StudentProgramParticipation studentProgramParticipation : studentPrograms) {
                    String programCode = studentProgramParticipation.getProgramCode();
                    if (ASPEN_PROGRAM_CODE_SE.equals(programCode)) {
                        // No translation of DfE SEN Provision code, save as is.
                        String type = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_SEN_TYPE,
                                getDataDictionary());
                        String rank = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_SEN_RANK,
                                getDataDictionary());
                        // Not translation of DfE SEN Type code, save as is.
                        sENProvision = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_SEN_PROVISION, getDataDictionary());
                        DfESENNeed sENNeed = new DfESENNeed(type, rank);
                        dfEPupil.addSENNeed(sENNeed);

                        sENStartDate = studentProgramParticipation.getStartDate();
                    }
                }
                dfEPupil.setSENStartDate(sENStartDate);
                dfEPupil.setSENProvision(sENProvision);

            }

            // Set Address
            Address studentAddress = studentPerson.getPhysicalAddress();
            String studentAddressOid = null;
            DfEAddress dfEAddress = null;
            boolean hasBS7666Address = false;
            if (studentAddress != null && studentAddress.getOid() != null) {
                studentAddressOid = studentAddress.getOid();

                dfEAddress = new DfEAddress();
                if (!StringUtils.isEmpty(studentAddress.getStreetName())) {
                    hasBS7666Address = true;

                    int streetNumber = studentAddress.getStreetNumber();
                    if (streetNumber != 0) {
                        dfEAddress.setPAON(Integer.toString(streetNumber));
                    } else {
                        dfEAddress.setPAON(studentAddress.getStreetLetter());
                    }
                    String sAON = (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_SAON, getDataDictionary());
                    dfEAddress.setSAON(sAON);
                    dfEAddress.setStreet(studentAddress.getStreetName());
                    if (!StringUtils.isEmpty(studentAddress.getCity())) {
                        dfEAddress.setTown(studentAddress.getCity());
                    }
                    String locality =
                            (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_LOCALITY, getDataDictionary());
                    dfEAddress.setLocality(locality);
                    String adminArea =
                            (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_ADMIN_AREA, getDataDictionary());
                    dfEAddress.setAdministrativeArea(adminArea);
                    String postTown =
                            (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_POST_TOWN, getDataDictionary());
                    dfEAddress.setPostTown(postTown);
                    String uniquePropertyReferenceNumber = (String) studentAddress
                            .getFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM, getDataDictionary());
                    dfEAddress.setUniquePropertyReferenceNumber(uniquePropertyReferenceNumber);
                } else {
                    if (!StringUtils.isEmpty(studentAddress.getAddressLine01())) {
                        dfEAddress.setAddressLine1(studentAddress.getAddressLine01());
                    }
                    if (!StringUtils.isEmpty(studentAddress.getAddressLine02())) {
                        dfEAddress.setAddressLine2(studentAddress.getAddressLine02());
                    }
                    if (!StringUtils.isEmpty(studentAddress.getAddressLine03())) {
                        dfEAddress.setAddressLine3(studentAddress.getAddressLine03());
                    }
                    String addressLine4 = (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4,
                            getDataDictionary());
                    if (!StringUtils.isEmpty(addressLine4)) {
                        dfEAddress.setAddressLine4(addressLine4);
                    }
                    String addressLine5 = (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5,
                            getDataDictionary());
                    if (!StringUtils.isEmpty(addressLine5)) {
                        dfEAddress.setAddressLine5(addressLine5);
                    }
                }
                dfEAddress.setBS7666Address(hasBS7666Address);

                if (!StringUtils.isEmpty(studentAddress.getCounty())) {
                    dfEAddress.setCounty(studentAddress.getCounty());
                }
                if (!StringUtils.isEmpty(studentAddress.getPostalCode())) {
                    dfEAddress.setPostCode(studentAddress.getPostalCode());
                }
                if (!StringUtils.isEmpty(studentAddress.getCountry())) {
                    dfEAddress.setCountry(studentAddress.getCountry());
                }
                String easting = (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_EASTING, getDataDictionary());
                if (!StringUtils.isEmpty(easting)) {
                    dfEAddress.setEasting(easting);
                }
                String northing =
                        (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_NORTHING, getDataDictionary());
                if (!StringUtils.isEmpty(northing)) {
                    dfEAddress.setNorthing(northing);
                }

                dfEPupil.setDfEAddress(dfEAddress);
            }


            // Add Telephones and Email
            if (!StringUtils.isEmpty(studentPerson.getPhone01())) {
                String telephoneType1 =
                        (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1, getDataDictionary());
                DfETelephone dfETelephone = new DfETelephone(telephoneType1, studentPerson.getPhone01());
                dfEPupil.addTelephone(dfETelephone);
            }
            if (!StringUtils.isEmpty(studentPerson.getPhone02())) {
                String telephoneType2 =
                        (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2, getDataDictionary());
                DfETelephone dfETelephone = new DfETelephone(telephoneType2, studentPerson.getPhone02());
                dfEPupil.addTelephone(dfETelephone);
            }
            if (!StringUtils.isEmpty(studentPerson.getPhone03())) {
                String telephoneType3 =
                        (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3, getDataDictionary());
                DfETelephone dfETelephone = new DfETelephone(telephoneType3, studentPerson.getPhone03());
                dfEPupil.addTelephone(dfETelephone);
            }

            if (!StringUtils.isEmpty(studentPerson.getEmail01())) {
                dfEPupil.setEmail(studentPerson.getEmail01());
            }

            Collection<StudentContact> studentContacts = student.getContacts();
            for (StudentContact studentContact : studentContacts) {
                Contact contact = studentContact.getContact();
                Person contactPerson = contact.getPerson();

                DfEContact dfEContact = new DfEContact();

                String order = (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_ORDER, getDataDictionary());
                if (StringUtils.isInteger(order)) {
                    dfEContact.setOrder(Integer.parseInt(order));
                } else {
                    dfEContact.setOrder(0);
                }
                if (!StringUtils.isEmpty(contactPerson.getNameTitleCode())) {
                    dfEContact.setTitle(contactPerson.getNameTitleCode());
                }
                if (!StringUtils.isEmpty(contactPerson.getLastName())) {
                    dfEContact.setSurname(contactPerson.getLastName());
                }
                if (!StringUtils.isEmpty(contactPerson.getFirstName())) {
                    dfEContact.setForename(contactPerson.getFirstName());
                }
                if (!StringUtils.isEmpty(contactPerson.getMiddleName())) {
                    dfEContact.setMiddleNames(contactPerson.getMiddleName());
                }
                if (!StringUtils.isEmpty(contactPerson.getGenderCode())) {
                    // Translate Aspen Gender Code to DfE Gender Code.
                    String genderCode = lookupStateValueByRefCode(Person.class, Person.COL_GENDER_CODE,
                            contactPerson.getGenderCode());
                    dfEContact.setGender(genderCode);
                }
                String responsibleStr =
                        (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_RESPONSIBLE, getDataDictionary());
                if (DB_TRUE.equals(responsibleStr)) {
                    dfEContact.setResponsible(BOOLEAN_TRUE);
                } else {
                    dfEContact.setResponsible(BOOLEAN_FALSE);
                }

                if (!StringUtils.isEmpty(studentContact.getRelationshipCode())) {
                    // Translate Aspen Gender Code to DfE Gender Code.
                    String relationshipCode = lookupStateValueByRefCode(StudentContact.class,
                            StudentContact.COL_RELATIONSHIP_CODE, studentContact.getRelationshipCode());
                    dfEContact.setRelationshipCode(relationshipCode);
                }

                // Add Telephones and Email
                if (!StringUtils.isEmpty(contactPerson.getPhone01())) {
                    String telephoneType1 = (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1,
                            getDataDictionary());
                    DfETelephone dfETelephone = new DfETelephone(telephoneType1, contactPerson.getPhone01());
                    dfEContact.addTelephone(dfETelephone);
                }
                if (!StringUtils.isEmpty(contactPerson.getPhone02())) {
                    String telephoneType2 = (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2,
                            getDataDictionary());
                    DfETelephone dfETelephone = new DfETelephone(telephoneType2, contactPerson.getPhone02());
                    dfEContact.addTelephone(dfETelephone);
                }
                if (!StringUtils.isEmpty(contactPerson.getPhone03())) {
                    String telephoneType3 = (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3,
                            getDataDictionary());
                    DfETelephone dfETelephone = new DfETelephone(telephoneType3, contactPerson.getPhone03());
                    dfEContact.addTelephone(dfETelephone);
                }
                if (!StringUtils.isEmpty(contactPerson.getEmail01())) {
                    dfEContact.setEmail(contactPerson.getEmail01());
                }

                // Set Address
                Address contactAddress = contactPerson.getPhysicalAddress();
                if (contactAddress != null) {
                    String contactAddressOid = contactAddress.getOid();

                    if (contactAddressOid.equals(studentAddressOid)) {
                        dfEContact.setAddressAsPupil(BOOLEAN_TRUE);
                    } else {
                        dfEAddress = new DfEAddress();
                        hasBS7666Address = false;
                        if (!StringUtils.isEmpty(contactAddress.getStreetName())) {
                            hasBS7666Address = true;

                            int streetNumber = contactAddress.getStreetNumber();
                            if (streetNumber != 0) {
                                dfEAddress.setPAON(Integer.toString(streetNumber));
                            } else {
                                dfEAddress.setPAON(contactAddress.getStreetLetter());
                            }
                            String sAON =
                                    (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_SAON, getDataDictionary());
                            dfEAddress.setSAON(sAON);
                            dfEAddress.setStreet(contactAddress.getStreetName());
                            if (!StringUtils.isEmpty(contactAddress.getCity())) {
                                dfEAddress.setTown(contactAddress.getCity());
                            }
                            String locality = (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_LOCALITY,
                                    getDataDictionary());
                            dfEAddress.setLocality(locality);
                            String adminArea = (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_ADMIN_AREA,
                                    getDataDictionary());
                            dfEAddress.setAdministrativeArea(adminArea);
                            String postTown = (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_POST_TOWN,
                                    getDataDictionary());
                            dfEAddress.setPostTown(postTown);
                            String uniquePropertyReferenceNumber = (String) contactAddress
                                    .getFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM, getDataDictionary());
                            dfEAddress.setUniquePropertyReferenceNumber(uniquePropertyReferenceNumber);
                        } else {
                            if (!StringUtils.isEmpty(contactAddress.getAddressLine01())) {
                                dfEAddress.setAddressLine1(contactAddress.getAddressLine01());
                            }
                            if (!StringUtils.isEmpty(contactAddress.getAddressLine02())) {
                                dfEAddress.setAddressLine2(contactAddress.getAddressLine02());
                            }
                            if (!StringUtils.isEmpty(contactAddress.getAddressLine03())) {
                                dfEAddress.setAddressLine3(contactAddress.getAddressLine03());
                            }
                            String addressLine4 = (String) contactAddress
                                    .getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4, getDataDictionary());
                            if (!StringUtils.isEmpty(addressLine4)) {
                                dfEAddress.setAddressLine4(addressLine4);
                            }
                            String addressLine5 = (String) contactAddress
                                    .getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5, getDataDictionary());
                            if (!StringUtils.isEmpty(addressLine5)) {
                                dfEAddress.setAddressLine5(addressLine5);
                            }
                        }
                        dfEAddress.setBS7666Address(hasBS7666Address);

                        if (!StringUtils.isEmpty(contactAddress.getCounty())) {
                            dfEAddress.setCounty(contactAddress.getCounty());
                        }
                        if (!StringUtils.isEmpty(contactAddress.getPostalCode())) {
                            dfEAddress.setPostCode(contactAddress.getPostalCode());
                        }
                        if (!StringUtils.isEmpty(contactAddress.getCountry())) {
                            dfEAddress.setCountry(contactAddress.getCountry());
                        }
                        String easting =
                                (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_EASTING, getDataDictionary());
                        if (!StringUtils.isEmpty(easting)) {
                            dfEAddress.setEasting(easting);
                        }
                        String northing =
                                (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_NORTHING, getDataDictionary());
                        if (!StringUtils.isEmpty(northing)) {
                            dfEAddress.setNorthing(northing);
                        }

                        dfEContact.setDfEAddress(dfEAddress);
                    }
                }

                dfEPupil.addContact(dfEContact);

            }

            // Don't return for ATF
            // Get Student Attendance Summary
            // TODO Remove later
            String attendanceXML =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_ATTENDANCE_XML, getDataDictionary());
            if (!StringUtils.isEmpty(attendanceXML)) {
                dfEPupil.setAttendanceXML(attendanceXML);
            }

            // Add all old and recently created Student Attendance
            ArrayList<DfEAttendance> studentAttendances =
                    getAttendance(student, attendanceHistories, attendances, attendancesByDate, attendanceXML);
            dfEPupil.addAllAttendances(studentAttendances);

            // Don't return for ATF
            // Get Student Assessment Summary
            // TODO Remove later
            String assessmentsXML =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_ASSESSMENTS_XML, getDataDictionary());
            if (!StringUtils.isEmpty(assessmentsXML)) {
                dfEPupil.setAssessmentsXML(assessmentsXML);
            }

            // Add all old and recently created Student Assessments
            ArrayList<DfEStageAssessment> studentAssessments = getStageAssessment(student, assessments, assessmentsXML);
            dfEPupil.addAllStageAssessment(studentAssessments);

            // Get School History Summary
            // Set Previously imported CTF Student School History
            // TODO Remove later
            String schoolHistoryXML =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_SCHOOL_HISTORY_XML, getDataDictionary());
            if (!StringUtils.isEmpty(schoolHistoryXML)) {
                dfEPupil.setSchoolHistoryXML(schoolHistoryXML);
            }

            // Add all old and recently created Student School histories
            ArrayList<DfESchoolHistory> studentSchoolHistories =
                    getSchoolHistory(student, enrollments, schoolHistoryXML);
            dfEPupil.addAllSchoolHistory(studentSchoolHistories);

            return dfEPupil;
        }

        /**
         * Get a Pupil Disabilities Element from Collection<String>.
         *
         * @param disabilitiesList Collection<String>
         * @return Element
         */
        public Element getDisabilitiesElement(Collection<String> disabilitiesList) {
            Element disabilitiesElement = new Element(DfEPupil.ELEMENT_DISABILITIES);

            for (String disability : disabilitiesList) {
                Element disabilityElement = new Element(DfEPupil.ELEMENT_DISABILITY);
                disabilityElement.addContent(disability);
                disabilitiesElement.addContent(disabilityElement);
            }

            return disabilitiesElement;
        }

        /**
         * Get XML Element from an XML String.
         *
         * @param xmlString String
         * @return Element
         */
        public Element getElementFromXMLString(String xmlString) {
            Element newElement = null;

            if (xmlString != null && xmlString.length() > 0) {
                byte[] elementBytes = xmlString.getBytes();
                try {
                    Document XMLElement = m_builder.build(new ByteArrayInputStream(elementBytes));
                    Element rootElement = XMLElement.getRootElement();
                    newElement = (Element) rootElement.clone();
                    newElement.detach();
                } catch (JDOMException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            return newElement;
        }

        /**
         * Get a FSMHistory Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getFSMHistoryElement(DfEPupil dfEPupil) {
            Element fSMHistoryElement = null;

            if (dfEPupil.getFSMInstances() != null && dfEPupil.getFSMInstances().size() > 0) {
                fSMHistoryElement = new Element(DfEPupil.ELEMENT_FSM_HISTORY);

                if (dfEPupil.getFSMReviewDate() != null) {
                    Element fSMReviewDateElement = new Element(DfEPupil.ELEMENT_FSM_REVIEW_DATE);
                    fSMReviewDateElement.addContent(m_dateFormat.format(dfEPupil.getFSMReviewDate()));
                    fSMHistoryElement.addContent(fSMReviewDateElement);
                }

                Collection<DfEFSMInstance> fSMInstances = dfEPupil.getFSMInstances();
                for (DfEFSMInstance dfEFSMInstance : fSMInstances) {
                    Element fSMInstanceElement = new Element(DfEFSMInstance.ELEMENT_FSM_INSTANCE);

                    if (dfEFSMInstance.getFSMStartDate() != null) {
                        Element fSMStartDateElement = new Element(DfEFSMInstance.ELEMENT_FSM_START_DATE);
                        fSMStartDateElement.addContent(m_dateFormat.format(dfEFSMInstance.getFSMStartDate()));
                        fSMInstanceElement.addContent(fSMStartDateElement);
                    }
                    if (dfEFSMInstance.getFSMEndDate() != null) {
                        Element fSMEndDateElement = new Element(DfEFSMInstance.ELEMENT_FSM_END_DATE);
                        fSMEndDateElement.addContent(m_dateFormat.format(dfEFSMInstance.getFSMEndDate()));
                        fSMInstanceElement.addContent(fSMEndDateElement);
                    }
                    if (dfEFSMInstance.getUKCountry() != null) {
                        Element fSMUKCountryElement = new Element(DfEFSMInstance.ELEMENT_FSM_UK_COUNTRY);
                        fSMUKCountryElement.addContent(dfEFSMInstance.getUKCountry());
                        fSMInstanceElement.addContent(fSMUKCountryElement);
                    }

                    fSMHistoryElement.addContent(fSMInstanceElement);
                }

            }

            return fSMHistoryElement;
        }

        /**
         * Get Header Element from DfEHeader
         *
         * Note: Not include SuppInfo.
         *
         * @param dfEHeader DfEHeader
         * @return Element
         */
        public Element getHeaderElement(DfEHeader dfEHeader) {
            Element headerElement = new Element(DfEHeader.ELEMENT_HEADER);

            Element documentNameElement = new Element(DfEHeader.ELEMENT_DOCUMENT_NAME);
            documentNameElement.addContent(dfEHeader.getDocumentName());
            headerElement.addContent(documentNameElement);
            Element versionElement = null;
            if (REPORT_TYPE_CTF.equals(dfEHeader.getHeaderType())) {
                versionElement = new Element(DfEHeader.ELEMENT_CTF_VERSION);
            } else if (REPORT_TYPE_ATF.equals(dfEHeader.getHeaderType())) {
                versionElement = new Element(DfEHeader.ELEMENT_ATF_VERSION);
            }

            if (versionElement != null) {
                versionElement.addContent(dfEHeader.getVersion());
                headerElement.addContent(versionElement);
            }

            Element dateTimeElement = new Element(DfEHeader.ELEMENT_DATE_TIME);
            dateTimeElement.addContent(dfEHeader.getDateTime());
            headerElement.addContent(dateTimeElement);
            if (dfEHeader.getDocumentQualifier() != null) {
                Element documentQualifierElement = new Element(DfEHeader.ELEMENT_DOCUMENT_QUALIFIER);
                documentQualifierElement.addContent(dfEHeader.getDocumentQualifier());
                headerElement.addContent(documentQualifierElement);
            }
            if (dfEHeader.getDataQualifier() != null) {
                Element dateQualifierElement = new Element(DfEHeader.ELEMENT_DATA_QUALIFIER);
                dateQualifierElement.addContent(dfEHeader.getDataQualifier());
                headerElement.addContent(dateQualifierElement);
            }
            if (dfEHeader.getDataDescriptor() != null) {
                Element dateDescriptorElement = new Element(DfEHeader.ELEMENT_DATA_DESCRIPTOR);
                dateDescriptorElement.addContent(dfEHeader.getDataDescriptor());
                headerElement.addContent(dateDescriptorElement);
            }
            if (dfEHeader.getSupplierID() != null) {
                Element supplierIDElement = new Element(DfEHeader.ELEMENT_SUPPLIER_ID);
                supplierIDElement.addContent(dfEHeader.getSupplierID());
                headerElement.addContent(supplierIDElement);
            }

            // Element SourceSchool
            Element sourceSchoolElement = new Element(DfEHeader.ELEMENT_SOURCE_SCHOOL);
            Element sourceSchoolLEAElement = new Element(DfEHeader.ELEMENT_LEA);
            sourceSchoolLEAElement.addContent(dfEHeader.getSourceSchoolLEA());
            sourceSchoolElement.addContent(sourceSchoolLEAElement);
            Element sourceSchoolEstabElement = new Element(DfEHeader.ELEMENT_ESTABISHMENT);
            sourceSchoolEstabElement.addContent(dfEHeader.getSourceSchoolEstab());
            sourceSchoolElement.addContent(sourceSchoolEstabElement);
            Element sourceSchoolNameElement = new Element(DfEHeader.ELEMENT_SCHOOL_NAME);
            sourceSchoolNameElement.addContent(dfEHeader.getSourceSchoolName());
            sourceSchoolElement.addContent(sourceSchoolNameElement);
            Element sourceSchoolAcademicYearElement = new Element(DfEHeader.ELEMENT_ACADEMIC_YEAR);
            sourceSchoolAcademicYearElement.addContent(dfEHeader.getSourceSchoolAcademicYear());
            sourceSchoolElement.addContent(sourceSchoolAcademicYearElement);
            headerElement.addContent(sourceSchoolElement);

            // Element DestSchool
            Element destSchoolElement = new Element(DfEHeader.ELEMENT_DESTINATION_SCHOOL);
            Element destSchoolLEAElement = new Element(DfEHeader.ELEMENT_LEA);
            destSchoolLEAElement.addContent(dfEHeader.getDestSchoolLEA());
            destSchoolElement.addContent(destSchoolLEAElement);
            Element destSchoolEstabElement = new Element(DfEHeader.ELEMENT_ESTABISHMENT);
            destSchoolEstabElement.addContent(dfEHeader.getDestSchoolEstab());
            destSchoolElement.addContent(destSchoolEstabElement);
            headerElement.addContent(destSchoolElement);

            return headerElement;
        }


        /**
         * Get a Languages Element from Collection<DfELanguage>.
         *
         * @param languagesList Collection<DfELanguage>
         * @return Element
         */
        public Element getLanguagesElement(Collection<DfELanguage> languagesList) {
            Element languagesElement = new Element(DfELanguage.ELEMENT_LANGUAGES);

            for (DfELanguage dfELanguage : languagesList) {
                Element typeElement = new Element(DfELanguage.ELEMENT_TYPE);

                if (dfELanguage.getLanguageType() != null) {
                    Element languageTypeElement = new Element(DfELanguage.ELEMENT_LANGUAGE_TYPE);
                    languageTypeElement.addContent(dfELanguage.getLanguageType());
                    typeElement.addContent(languageTypeElement);
                }
                if (dfELanguage.getLanguageCode() != null) {
                    Element languageCodeElement = new Element(DfELanguage.ELEMENT_LANGUAGE_CODE);
                    languageCodeElement.addContent(dfELanguage.getLanguageCode());
                    typeElement.addContent(languageCodeElement);
                }

                languagesElement.addContent(typeElement);
            }

            return languagesElement;
        }

        /**
         * Get a LookAfter Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getLookAfterElement(DfEPupil dfEPupil) {
            Element lookedAfterElement = new Element(DfEPupil.ELEMENT_LOOK_AFTER);
            if (dfEPupil.getInCare() != null) {
                Element inCareElement = new Element(DfEPupil.ELEMENT_IN_CARE);
                Boolean inCare = dfEPupil.getInCare();
                if (inCare.booleanValue()) {
                    inCareElement.addContent(STRING_TRUE);
                } else {
                    inCareElement.addContent(STRING_FALSE);
                }

                lookedAfterElement.addContent(inCareElement);
            }
            if (dfEPupil.getCareAuthority() != null) {
                Element careAuthorityElement = new Element(DfEPupil.ELEMENT_CARE_AUTHORITY);
                careAuthorityElement.addContent(dfEPupil.getCareAuthority());
                lookedAfterElement.addContent(careAuthorityElement);
            }

            return lookedAfterElement;
        }

        /**
         * Get a NAWDetails Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getNAWDetailsElement(DfEPupil dfEPupil) {
            Element nAWDetailsElement = null;

            if (dfEPupil.getNAWDetails() != null && dfEPupil.getNAWDetails().size() > 0) {
                nAWDetailsElement = new Element(DfEPupil.ELEMENT_NAW_DETAILS);

                ArrayList<DfENAWDetail> nAWDetails = dfEPupil.getNAWDetails();

                // Only interested in the first one.
                DfENAWDetail nAWDetail = nAWDetails.get(0);

                if (nAWDetail.getSpeakWelsh() != null) {
                    Element speakWelshElement = new Element(DfENAWDetail.ELEMENT_SPEAK_WELSH);
                    speakWelshElement.addContent(nAWDetail.getSpeakWelsh());
                    nAWDetailsElement.addContent(speakWelshElement);
                }
                if (nAWDetail.getHomeWelsh() != null) {
                    Element homeWelshElement = new Element(DfENAWDetail.ELEMENT_HOME_WELSH);
                    homeWelshElement.addContent(nAWDetail.getHomeWelsh());
                    nAWDetailsElement.addContent(homeWelshElement);
                }
                if (nAWDetail.getNationalIdentity() != null) {
                    Element nationalIdentityElement = new Element(DfENAWDetail.ELEMENT_NATIONAL_IDENTITY);
                    nationalIdentityElement.addContent(nAWDetail.getNationalIdentity());
                    nAWDetailsElement.addContent(nationalIdentityElement);
                }
                if (nAWDetail.getWelshSource() != null) {
                    Element welshSourceElement = new Element(DfENAWDetail.ELEMENT_WELSH_SOURCE);
                    welshSourceElement.addContent(nAWDetail.getWelshSource());
                    nAWDetailsElement.addContent(welshSourceElement);
                }
                if (nAWDetail.getEALAcquisition() != null) {
                    Element eALAcquisitionElement = new Element(DfENAWDetail.ELEMENT_EAL_ACQUISITION);
                    eALAcquisitionElement.addContent(nAWDetail.getEALAcquisition());
                    nAWDetailsElement.addContent(eALAcquisitionElement);
                }
                if (nAWDetail.getLanguageSource() != null) {
                    Element languageSourceElement = new Element(DfENAWDetail.ELEMENT_LANGAUGE_SOURCE);
                    languageSourceElement.addContent(nAWDetail.getLanguageSource());
                    nAWDetailsElement.addContent(languageSourceElement);
                }
                if (nAWDetail.getSENCurrTeachingMethods() != null) {
                    Element sENCurrTeachingMethodsElement = new Element(DfENAWDetail.ELEMENT_SEN_CURR_TEACHING_METHODS);
                    sENCurrTeachingMethodsElement.addContent(nAWDetail.getSENCurrTeachingMethods());
                    nAWDetailsElement.addContent(sENCurrTeachingMethodsElement);
                }
                if (nAWDetail.getSENGroupingAndSupport() != null) {
                    Element sENGroupingAndSupportElement = new Element(DfENAWDetail.ELEMENT_SEN_GROUPINGS_SUPPORT);
                    sENGroupingAndSupportElement.addContent(nAWDetail.getSENGroupingAndSupport());
                    nAWDetailsElement.addContent(sENGroupingAndSupportElement);
                }
                if (nAWDetail.getSENSpecialisedResources() != null) {
                    Element sENSpecialisedResourcesElement =
                            new Element(DfENAWDetail.ELEMENT_SEN_SPECIALISED_RESOURCES);
                    sENSpecialisedResourcesElement.addContent(nAWDetail.getSENSpecialisedResources());
                    nAWDetailsElement.addContent(sENSpecialisedResourcesElement);
                }
                if (nAWDetail.getSENAdviceAndAssessment() != null) {
                    Element sENAdviceAndAssessmentElement = new Element(DfENAWDetail.ELEMENT_SEN_ADVICE_ASSESSMENT);
                    sENAdviceAndAssessmentElement.addContent(nAWDetail.getSENAdviceAndAssessment());
                    nAWDetailsElement.addContent(sENAdviceAndAssessmentElement);
                }
            }

            return nAWDetailsElement;
        }

        /**
         * Get a DfETelephone Elements from Collection<DfETelephone>.
         *
         * @param telephoneList Collection<DfETelephone>
         * @return Element
         */
        public Element getPhonesElement(Collection<DfETelephone> telephoneList) {
            Element phonesElement = new Element(DfETelephone.ELEMENT_PHONES);

            Collection<DfETelephone> telephones = telephoneList;
            for (DfETelephone dfEtelephone : telephones) {
                Element phoneElement = new Element(DfETelephone.ELEMENT_PHONE);

                // Optional, Hide default
                if (dfEtelephone.getTelephoneType() != null) {
                    Element telephoneTypeElement = new Element(DfETelephone.ELEMENT_TELEPHONE_TYPE);
                    telephoneTypeElement.addContent(dfEtelephone.getTelephoneType());
                    phoneElement.addContent(telephoneTypeElement);
                }

                Element telephoneNumberElement = new Element(DfETelephone.ELEMENT_PHONE_NUMBER);
                telephoneNumberElement.addContent(dfEtelephone.getTelephoneNumber());
                phoneElement.addContent(telephoneNumberElement);

                phonesElement.addContent(phoneElement);
            }

            return phonesElement;
        }

        /**
         * Get a DfE Pupil Data from DfEPupil
         *
         * Note: Not including SuppInfo Elements.
         *
         * @param reportType String
         * @param dfEPupil DfEPupil
         * @param activeSections
         * @return Element
         */
        public Element getPupilDataElement(String reportType,
                                           DfEPupil dfEPupil,
                                           HashMap<String, Boolean> activeSections) {
            Element pupilElement = new Element(DfEPupil.ELEMENT_PUPIL);

            // Pupil Core Fields
            if (REPORT_TYPE_ATF.equals(reportType)) {
                if (!StringUtils.isEmpty(dfEPupil.getApplicationReference())) {
                    Element applicationReferenceElement = new Element(DfEPupil.ELEMENT_APPLICATION_REFERENCE);
                    applicationReferenceElement.addContent(dfEPupil.getApplicationReference());
                    pupilElement.addContent(applicationReferenceElement);
                }
            }
            if (!StringUtils.isEmpty(dfEPupil.getUniquePupilNumber())) {
                Element uniquePupilNumberElement = new Element(DfEPupil.ELEMENT_UNIQUE_PUPIL_NUMBER);
                uniquePupilNumberElement.addContent(dfEPupil.getUniquePupilNumber());
                pupilElement.addContent(uniquePupilNumberElement);
            }
            if (!StringUtils.isEmpty(dfEPupil.getUniqueLearnerNumber())) {
                Element uniqueLearnerNumberElement = new Element(DfEPupil.ELEMENT_UNIQUE_LEARNER_NUMBER);
                uniqueLearnerNumberElement.addContent(dfEPupil.getUniqueLearnerNumber());
                pupilElement.addContent(uniqueLearnerNumberElement);
            }
            if (REPORT_TYPE_CTF.equals(reportType)) {
                if (!StringUtils.isEmpty(dfEPupil.getUniqueCandidateIdentifier())) {
                    Element uniqueCandidateIdentifierElement =
                            new Element(DfEPupil.ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER);
                    uniqueCandidateIdentifierElement.addContent(dfEPupil.getUniqueCandidateIdentifier());
                    pupilElement.addContent(uniqueCandidateIdentifierElement);
                }
            }
            if (!StringUtils.isEmpty(dfEPupil.getSurname())) {
                Element surnameElement = new Element(DfEPupil.ELEMENT_SURNAME);
                surnameElement.addContent(dfEPupil.getSurname());
                pupilElement.addContent(surnameElement);
            }
            if (!StringUtils.isEmpty(dfEPupil.getForename())) {
                Element forenameElement = new Element(DfEPupil.ELEMENT_FORENAME);
                forenameElement.addContent(dfEPupil.getForename());
                pupilElement.addContent(forenameElement);
            }
            if (dfEPupil.getBirthDate() != null) {
                Element dateOfBirthElement = new Element(DfEPupil.ELEMENT_DATE_OF_BIRTH);
                dateOfBirthElement.addContent(m_dateFormat.format(dfEPupil.getBirthDate()));
                pupilElement.addContent(dateOfBirthElement);
            }
            if (!StringUtils.isEmpty(dfEPupil.getGender())) {
                Element genderElement = new Element(DfEPupil.ELEMENT_GENDER);
                genderElement.addContent(dfEPupil.getGender());
                pupilElement.addContent(genderElement);
            }

            // Pupil Basic Details
            if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
                Element basicDetailsElement = getBasicDetailsElement(dfEPupil);
                pupilElement.addContent(basicDetailsElement);
            }

            // FSMHistory fields
            if (isSectionActive(activeSections, PARAM_FSM_HISTORY)) {
                if (dfEPupil.getFSMInstances() != null && dfEPupil.getFSMInstances().size() > 0) {
                    Element fSMHistoryElement = getFSMHistoryElement(dfEPupil);

                    pupilElement.addContent(fSMHistoryElement);
                }
            }

            // NAW Details Fields
            if (isSectionActive(activeSections, PARAM_NAW_DETAILS)) {
                if (REPORT_TYPE_CTF.equals(reportType)) {
                    if (dfEPupil.getNAWDetails() != null && dfEPupil.getNAWDetails().size() > 0) {
                        Element nAWDetailsElement = getNAWDetailsElement(dfEPupil);

                        pupilElement.addContent(nAWDetailsElement);
                    }
                }
            }

            // Looked After Fields
            if (isSectionActive(activeSections, PARAM_LOOKED_AFTER)) {
                if (dfEPupil.getInCare() != null) {
                    Element lookedAfterElement = getLookAfterElement(dfEPupil);
                    pupilElement.addContent(lookedAfterElement);
                }
            }

            // SEN History Fields
            if (isSectionActive(activeSections, PARAM_SEN_HISTORY)) {
                if (dfEPupil.getSENNeeds() != null && dfEPupil.getSENNeeds().size() > 0) {
                    Element sENHistoryElement = getSENHistoryElement(dfEPupil);

                    pupilElement.addContent(sENHistoryElement);
                }
            }

            // Admissions Fields
            if (isSectionActive(activeSections, PARAM_ADMISSIONS)) {
                if (REPORT_TYPE_ATF.equals(reportType)) {
                    Element admissionsElement = getAdmissionsElement();

                    pupilElement.addContent(admissionsElement);
                }
            }

            // Address Fields
            if (isSectionActive(activeSections, PARAM_ADDRESS_PHONE_EMAIL)) {
                if (dfEPupil.getDfEAddress() != null) {
                    Element addressElement = getAddressElement(dfEPupil.getDfEAddress());

                    pupilElement.addContent(addressElement);
                }

                // Telephone Fields
                if (dfEPupil.getTelephones() != null && dfEPupil.getTelephones().size() > 0) {
                    Element phonesElement = getPhonesElement(dfEPupil.getTelephones());

                    pupilElement.addContent(phonesElement);
                }

                if (!StringUtils.isEmpty(dfEPupil.getEmail())) {
                    Element emailElement = new Element(DfEPupil.ELEMENT_EMAIL);
                    emailElement.addContent(dfEPupil.getEmail());
                    pupilElement.addContent(emailElement);
                }
            }

            // Contacts Fields
            if (isSectionActive(activeSections, PARAM_CONTACTS)) {
                if (dfEPupil.getContacts() != null && dfEPupil.getContacts().size() > 0) {
                    Element contactsElement = getContacts(dfEPupil.getContacts());
                    pupilElement.addContent(contactsElement);
                }
            }

            if (REPORT_TYPE_CTF.equals(reportType)) {
                // Pupil Attendance Fields
                if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
                    if (dfEPupil.getAttendances() != null && dfEPupil.getAttendances().size() > 0) {
                        Element attendanceElement = getAttendanceElement(dfEPupil, true);
                        pupilElement.addContent(attendanceElement);
                    }
                }

                // Pupil Assessments Fields
                if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
                    if (dfEPupil.getStageAssessments() != null && dfEPupil.getStageAssessments().size() > 0) {
                        Element stateAssessmentsElement = getStageAssessmentsElement(dfEPupil);
                        pupilElement.addContent(stateAssessmentsElement);
                    }
                }
            }

            // Pupil School History Fields
            if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
                if (dfEPupil.getSchoolHistories() != null && dfEPupil.getSchoolHistories().size() > 0) {
                    Element schoolHistoryElement = getSchoolHistoryElement(dfEPupil);
                    pupilElement.addContent(schoolHistoryElement);
                }
            }

            return pupilElement;
        }

        /**
         * Get all Student School History
         * Use any Imported CTF/ATF SchoolHistories as well as any Aspen school's from
         * StudentEnrollment records.
         *
         * @param student Student
         * @param enrollments Collection<StudentEnrollment>
         * @param schoolHistoryXML String
         * @return ArrayList<DfESchoolHistory>
         */
        public ArrayList<DfESchoolHistory> getSchoolHistory(Student student,
                                                            Collection<StudentEnrollment> enrollments,
                                                            String schoolHistoryXML) {
            ArrayList<DfESchoolHistory> schoolHistories = new ArrayList();

            // TODO Remove later
            // All previously imported CTF school history and add to school history list
            /*
             * if (!StringUtils.isEmpty(schoolHistoryXML))
             * {
             * ArrayList<DfESchoolHistory> oldStudentHistories =
             * parseSchoolHistoryXML(schoolHistoryXML);
             *
             * if (oldStudentHistories != null && oldStudentHistories.size() > 0)
             * {
             * schoolHistories.addAll(oldStudentHistories);
             * }
             * }
             */

            String cTFSchoolId = m_cTFSchool.getOid();

            // Create two maps containing enrollment and withdrawals from the student's
            // StudentEnrollments records
            HashMap<String, StudentEnrollment> schoolEnroll = new HashMap();
            HashMap<String, StudentEnrollment> schoolWithdrawl = new HashMap();
            StudentEnrollment prevStudentEnrollment = null;
            if (enrollments != null && enrollments.size() > 0) {
                // SchoolId is a unique identifier can be a Aspen schoolOid or a UK DfE
                // Establishment Number
                String schoolId = null;
                String prevSchoolId = null;
                String currEnrollmentType = null;
                String prevEnrollmentType = null;
                int k = 0; // over all
                int l = 0; // change of school
                for (StudentEnrollment studentEnrollment : enrollments) {
                    k++;
                    l++;

                    // If the Enrollment record is referencing the CTF School, get the Estab Number
                    // from the UDF
                    schoolId = studentEnrollment.getSchoolOid();
                    if (cTFSchoolId.equals(schoolId)) {
                        schoolId = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                getDataDictionary());
                    }

                    currEnrollmentType = studentEnrollment.getEnrollmentType();
                    if (!schoolId.equals(prevSchoolId)) {
                        l = 0;
                    }
                    if (l == 0
                            && StudentEnrollment.ENTRY.equalsIgnoreCase(currEnrollmentType)) {
                        schoolEnroll.put(schoolId, studentEnrollment);
                    }
                    if (k > 1
                            && !schoolId.equals(prevSchoolId)
                            && StudentEnrollment.WITHDRAWAL.equalsIgnoreCase(prevEnrollmentType)) {
                        schoolWithdrawl.put(prevSchoolId, prevStudentEnrollment);
                    }

                    prevSchoolId = schoolId;
                    prevEnrollmentType = currEnrollmentType;
                    prevStudentEnrollment = studentEnrollment;
                }
                if (l == 0
                        && StudentEnrollment.ENTRY.equalsIgnoreCase(prevEnrollmentType)) {
                    schoolEnroll.put(prevSchoolId, prevStudentEnrollment);
                }
                if (!schoolId.equals(prevSchoolId)
                        && StudentEnrollment.WITHDRAWAL.equalsIgnoreCase(prevEnrollmentType)) {
                    schoolWithdrawl.put(prevSchoolId, prevStudentEnrollment);
                }

                // Add to DfESchoolHistory objects from the enrollment maps.
                Collection<StudentEnrollment> studentEnrollments = schoolEnroll.values();
                for (StudentEnrollment studentEnrollment : studentEnrollments) {
                    // If the Enrollment record is referencing the CTF School, get the Estab Number
                    // from the Enrollment record UDF
                    String currSchoolId = studentEnrollment.getSchoolOid();
                    if (cTFSchoolId.equals(currSchoolId)) {
                        currSchoolId = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                getDataDictionary());
                    }

                    School school = studentEnrollment.getSchool();
                    if (school != null) {
                        String lEA = null;
                        String estab = null;
                        String schoolName = null;

                        // If the Enrollment record is referencing the CTF School, get the school
                        // information from the Enrollment record UDF's
                        if (cTFSchoolId.equals(studentEnrollment.getSchoolOid())) {
                            lEA = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID,
                                    getDataDictionary());
                            estab = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                    getDataDictionary());
                            schoolName = (String) studentEnrollment
                                    .getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME, getDataDictionary());
                        } else {
                            Organization organization = school.getOrganization1();
                            lEA = (String) organization.getFieldValueByAlias(ALIAS_NAME_LEA, getDataDictionary());
                            estab = (String) school.getFieldValueByAlias(ALIAS_NAME_ESTAB, getDataDictionary());
                            schoolName = school.getName();
                        }

                        Boolean lastSchool = BOOLEAN_FALSE;
                        PlainDate entryDate = studentEnrollment.getEnrollmentDate();
                        PlainDate leavingDate = null;
                        String leavingReason = null;

                        if (schoolWithdrawl.containsKey(currSchoolId)) {
                            StudentEnrollment studentWithdrawl = schoolWithdrawl.get(currSchoolId);
                            leavingDate = studentWithdrawl.getEnrollmentDate();
                            leavingReason = studentWithdrawl.getEnrollmentCode();
                        }
                        if (student.getSchoolOid().equals(currSchoolId)) {
                            lastSchool = BOOLEAN_TRUE;
                        }

                        DfESchoolHistory schoolHistory = new DfESchoolHistory(lEA, estab, schoolName, entryDate,
                                leavingDate, leavingReason, lastSchool);
                        schoolHistories.add(schoolHistory);
                    }
                }

            }

            return schoolHistories;
        }

        /**
         * Get a SchoolHistory Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getSchoolHistoryElement(DfEPupil dfEPupil) {
            Element schoolHistoryElement = null;

            if (dfEPupil.getSchoolHistories() != null && dfEPupil.getSchoolHistories().size() > 0) {
                schoolHistoryElement = new Element(DfEPupil.ELEMENT_SCHOOL_HISTORY);

                ArrayList<DfESchoolHistory> schoolHistories = dfEPupil.getSchoolHistories();

                // Sort by Entry Date
                TreeMap<String, DfESchoolHistory> schoolHistoriesMap = new TreeMap<String, DfESchoolHistory>();
                for (DfESchoolHistory dfESchoolHistory : schoolHistories) {
                    String entryDateStr = STRING_EMPTY;
                    if (dfESchoolHistory.getEntryDate() != null) {
                        entryDateStr = m_dateFormat.format(dfESchoolHistory.getEntryDate());
                    }
                    String key = entryDateStr + ":" + dfESchoolHistory.getSchoolName();
                    schoolHistoriesMap.put(key, dfESchoolHistory);
                }

                for (DfESchoolHistory dfESchoolHistory : schoolHistoriesMap.values()) {
                    Element schoolElement = new Element(DfESchoolHistory.ELEMENT_SCHOOL);

                    if (dfESchoolHistory.getLEA() != null) {
                        Element lEAElement = new Element(DfESchoolHistory.ELEMENT_LEA);
                        lEAElement.addContent(dfESchoolHistory.getLEA());
                        schoolElement.addContent(lEAElement);
                    }
                    if (dfESchoolHistory.getEstab() != null) {
                        Element estabElement = new Element(DfESchoolHistory.ELEMENT_ESTAB);
                        estabElement.addContent(dfESchoolHistory.getEstab());
                        schoolElement.addContent(estabElement);
                    }
                    if (dfESchoolHistory.getSchoolName() != null) {
                        Element schoolNameElement = new Element(DfESchoolHistory.ELEMENT_SCHOOL_NAME);
                        schoolNameElement.addContent(dfESchoolHistory.getSchoolName());
                        schoolElement.addContent(schoolNameElement);
                    }
                    if (dfESchoolHistory.getEntryDate() != null) {
                        Element entryDateElement = new Element(DfESchoolHistory.ELEMENT_ENTRY_DATE);
                        entryDateElement.addContent(m_dateFormat.format(dfESchoolHistory.getEntryDate()));
                        schoolElement.addContent(entryDateElement);
                    }
                    if (dfESchoolHistory.getLeavingDate() != null) {
                        Element leavingDateElement = new Element(DfESchoolHistory.ELEMENT_LEAVING_DATE);
                        leavingDateElement.addContent(m_dateFormat.format(dfESchoolHistory.getLeavingDate()));
                        schoolElement.addContent(leavingDateElement);
                    }
                    if (dfESchoolHistory.getLeavingReason() != null) {
                        Element leavingReasonElement = new Element(DfESchoolHistory.ELEMENT_LEAVING_REASON);
                        leavingReasonElement.addContent(dfESchoolHistory.getLeavingReason());
                        schoolElement.addContent(leavingReasonElement);
                    }
                    if (dfESchoolHistory.getLastSchool() != null) {
                        Element lastSchoolElement = new Element(DfESchoolHistory.ELEMENT_LAST_SCHOOL);
                        Boolean lastSchool = dfESchoolHistory.getLastSchool();
                        if (lastSchool.booleanValue()) {
                            lastSchoolElement.addContent(STRING_TRUE);
                            schoolElement.addContent(lastSchoolElement);
                        }
                    }

                    schoolHistoryElement.addContent(schoolElement);
                }

            }

            return schoolHistoryElement;
        }

        /**
         * Get a SENHistory Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getSENHistoryElement(DfEPupil dfEPupil) {
            Element sENHistoryElement = new Element(DfEPupil.ELEMENT_SEN_HISTORY);

            Element sENElement = new Element(DfEPupil.ELEMENT_SEN);
            if (dfEPupil.getSENStartDate() != null) {
                Element sENStartDateElement = new Element(DfEPupil.ELEMENT_SEN_START_DATE);
                sENStartDateElement.addContent(m_dateFormat.format(dfEPupil.getSENStartDate()));
                sENElement.addContent(sENStartDateElement);
            }
            if (dfEPupil.getSENProvision() != null) {
                Element sENProvisionElement = new Element(DfEPupil.ELEMENT_SEN_PROVISION);
                sENProvisionElement.addContent(dfEPupil.getSENProvision());
                sENElement.addContent(sENProvisionElement);
            }
            sENHistoryElement.addContent(sENElement);

            if (dfEPupil.getSENNeeds() != null && dfEPupil.getSENNeeds().size() > 0) {
                Element sENNeedsElement = new Element(DfESENNeed.ELEMENT_SEN_NEEDS);

                Collection<DfESENNeed> sENNeeds = dfEPupil.getSENNeeds();
                for (DfESENNeed dfESENNeed : sENNeeds) {
                    Element sENNeedElement = new Element(DfESENNeed.ELEMENT_SEN_NEED);

                    Element typeElement = new Element(DfESENNeed.ELEMENT_SEN_TYPE);
                    typeElement.addContent(dfESENNeed.getType());
                    sENNeedElement.addContent(typeElement);

                    Element typeRankElement = new Element(DfESENNeed.ELEMENT_SEN_TYPE_RANK);
                    typeRankElement.addContent(dfESENNeed.getTypeRank());
                    sENNeedElement.addContent(typeRankElement);

                    sENNeedsElement.addContent(sENNeedElement);
                }

                sENHistoryElement.addContent(sENNeedsElement);
            }

            return sENHistoryElement;
        }

        /**
         * Get all Student Stage Assessment
         * Use any Imported CTF/ATF SchoolHistories as well as any Aspen school's from
         * StudentAssessment records.
         *
         * @param student Student
         * @param assessments Collection<StudentAssessment>
         * @param stageAssessmentXML String
         * @return ArrayList<DfEStageAssessment>
         */
        public ArrayList<DfEStageAssessment> getStageAssessment(Student student,
                                                                Collection<StudentAssessment> assessments,
                                                                String stageAssessmentXML) {
            ArrayList<DfEStageAssessment> stageAssessments = new ArrayList();

            // TODO Remove later
            // All previously imported CTF school assessments and add to school history list
            /*
             * if (!StringUtils.isEmpty(stageAssessmentXML))
             * {
             * ArrayList<DfEStageAssessment> oldStudentHistories =
             * parseStageAssessmentXML(stageAssessmentXML);
             *
             * if (oldStudentHistories != null && oldStudentHistories.size() > 0)
             * {
             * stageAssessments.addAll(oldStudentHistories);
             * }
             * }
             */

            // Add to DfEStageAssessment objects from StudentAssessment.
            DataDictionary extendedDataDictionary = null;
            if (assessments != null && assessments.size() > 0) {
                for (StudentAssessment studentAssessment : assessments) {
                    if (extendedDataDictionary == null) {
                        extendedDataDictionary = DataDictionary.getDistrictDictionary(
                                studentAssessment.getExtendedDataDictionary(), m_broker.getPersistenceKey());
                    }

                    String stage =
                            (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_STAGE, extendedDataDictionary);
                    String locale =
                            (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_LOCALE, extendedDataDictionary);
                    String yearTakenStr = (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_YEAR_TAKEN,
                            extendedDataDictionary);
                    Integer yearTaken = Integer.valueOf(0);
                    if (StringUtils.isNumeric(yearTakenStr)) {
                        yearTaken = Integer.valueOf(yearTakenStr);
                    }
                    String subject =
                            (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_SUBJECT, extendedDataDictionary);
                    String method =
                            (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_METHOD, extendedDataDictionary);
                    String component = (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_COMPONENT,
                            extendedDataDictionary);
                    String resultStatus = (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_RESULT_STATUS,
                            extendedDataDictionary);
                    String resultQualifier = (String) studentAssessment
                            .getFieldValueByAlias(ALIAS_NAME_RESULT_QUALIFIER, extendedDataDictionary);
                    String result =
                            (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_RESULT, extendedDataDictionary);
                    String resultDateStr = (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_RESULT_DATE,
                            extendedDataDictionary);

                    DfEStageAssessment stageAssessment = new DfEStageAssessment(stage, locale, yearTaken.toString(),
                            subject, method, component, resultStatus, resultQualifier, result, resultDateStr);
                    stageAssessments.add(stageAssessment);
                }
            }

            return stageAssessments;
        }


        /**
         * Get a StageAssessment Element from DfEStageAssessment.
         *
         * @param stageAssessment DfEStageAssessment
         * @return Element
         */
        public Element getStageAssessmentElement(DfEStageAssessment stageAssessment) {
            Element stageAssessmentElement = new Element(DfEStageAssessment.ELEMENT_STAGE_ASSESSMENT);

            if (stageAssessment.getLocale() != null) {
                Element localeElement = new Element(DfEStageAssessment.ELEMENT_LOCALE);
                localeElement.addContent(stageAssessment.getLocale());
                stageAssessmentElement.addContent(localeElement);
            }
            if (stageAssessment.getYear() != null) {
                Element yearElement = new Element(DfEStageAssessment.ELEMENT_YEAR);
                yearElement.addContent(stageAssessment.getYear());
                stageAssessmentElement.addContent(yearElement);
            }
            if (stageAssessment.getSubject() != null) {
                Element subjectElement = new Element(DfEStageAssessment.ELEMENT_SUBJECT);
                subjectElement.addContent(stageAssessment.getSubject());
                stageAssessmentElement.addContent(subjectElement);
            }
            if (stageAssessment.getMethod() != null) {
                Element methodElement = new Element(DfEStageAssessment.ELEMENT_METHOD);
                methodElement.addContent(stageAssessment.getMethod());
                stageAssessmentElement.addContent(methodElement);
            }
            if (stageAssessment.getComponent() != null) {
                Element componentElement = new Element(DfEStageAssessment.ELEMENT_COMPONENT);
                componentElement.addContent(stageAssessment.getComponent());
                stageAssessmentElement.addContent(componentElement);
            }
            if (stageAssessment.getResultStatus() != null) {
                Element resultStatusElement = new Element(DfEStageAssessment.ELEMENT_RESULT_STATUS);
                resultStatusElement.addContent(stageAssessment.getResultStatus());
                stageAssessmentElement.addContent(resultStatusElement);
            }
            if (stageAssessment.getResultQualifier() != null) {
                Element resultQualifierElement = new Element(DfEStageAssessment.ELEMENT_RESULT_QUALIFIER);
                resultQualifierElement.addContent(stageAssessment.getResultQualifier());
                stageAssessmentElement.addContent(resultQualifierElement);
            }
            if (stageAssessment.getResult() != null) {
                Element resultElement = new Element(DfEStageAssessment.ELEMENT_RESULT);
                resultElement.addContent(stageAssessment.getResult());
                stageAssessmentElement.addContent(resultElement);
            }
            if (stageAssessment.getResultDate() != null) {
                Element resultDateElement = new Element(DfEStageAssessment.ELEMENT_RESULT_DATE);
                resultDateElement.addContent(m_dateFormat.format(stageAssessment.getResultDate()));
                stageAssessmentElement.addContent(resultDateElement);
            }

            return stageAssessmentElement;
        }

        /**
         * Get a StageAssessments Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getStageAssessmentsElement(DfEPupil dfEPupil) {
            Element stageAssessmentsElement = null;

            if (dfEPupil.getStageAssessments() != null && dfEPupil.getStageAssessments().size() > 0) {
                stageAssessmentsElement = new Element(DfEPupil.ELEMENT_STAGE_ASSESSMENTS);

                ArrayList<DfEStageAssessment> stageAssessments = dfEPupil.getStageAssessments();

                String currentStage = null;
                String prevStage = null;
                DfEStageAssessment prevStageAssessment = null;

                Element keyStageElement = null;
                Element stageAssessmentElement = null;

                int i = 0;
                for (DfEStageAssessment stageAssessment : stageAssessments) {
                    i++;

                    currentStage = stageAssessment.getStage();

                    if (i == 1) {
                        // Add first KeyStage
                        keyStageElement = new Element(DfEStageAssessment.ELEMENT_KEY_STAGE);

                        Element stageElement = new Element(DfEStageAssessment.ELEMENT_STAGE);
                        if (StringUtils.isEmpty(currentStage)) {
                            stageElement.addContent(STRING_EMPTY);
                        } else {
                            stageElement.addContent(currentStage);
                        }
                        keyStageElement.addContent(stageElement);
                    } else {
                        // Add previous StageAssessment
                        stageAssessmentElement = getStageAssessmentElement(prevStageAssessment);

                        keyStageElement.addContent(stageAssessmentElement);

                        // If Stage has changes close the previous and start the next
                        if (!currentStage.equals(prevStage)) {
                            // Close out previous KeyStage
                            stageAssessmentsElement.addContent(keyStageElement);

                            // Start next KeyStage
                            keyStageElement = new Element(DfEStageAssessment.ELEMENT_KEY_STAGE);

                            Element stageElement = new Element(DfEStageAssessment.ELEMENT_STAGE);
                            // currentStage = stageAssessment.getStage();
                            if (StringUtils.isEmpty(currentStage)) {
                                stageElement.addContent(STRING_EMPTY);
                            } else {
                                stageElement.addContent(currentStage);
                            }
                            keyStageElement.addContent(stageElement);
                        }

                        // End if i > 0
                    }

                    prevStageAssessment = stageAssessment;
                    prevStage = prevStageAssessment.getStage();

                    // End DfEStageAssessments loop
                }

                // Add Last StageAssessment
                stageAssessmentElement = getStageAssessmentElement(prevStageAssessment);

                keyStageElement.addContent(stageAssessmentElement);

                // Close out last KeyStage
                stageAssessmentsElement.addContent(keyStageElement);
            }

            return stageAssessmentsElement;
        }

        /**
         * Loads a List of Lists of School Year date ranges.
         */
        public void loadSchoolYears() {
            BeanQuery query = new BeanQuery(DistrictSchoolYearContext.class);
            query.addOrderByDescending(DistrictSchoolYearContext.COL_START_DATE);

            Iterator iterator = m_broker.getIteratorByQuery(query);

            m_schoolYearRanges = new ArrayList<ArrayList<PlainDate>>();
            while (iterator.hasNext()) {
                DistrictSchoolYearContext dst = (DistrictSchoolYearContext) iterator.next();

                ArrayList<PlainDate> listOfDates = new ArrayList<PlainDate>();
                listOfDates.add(dst.getStartDate());
                listOfDates.add(dst.getEndDate());
                m_schoolYearRanges.add(listOfDates);
            }
        }

        /**
         * Parse the imported CTF Attendance in an ArrayList for DfEAttendance.
         *
         * @param AttendancesXML String
         * @return ArrayList<DfEAttendance>
         */
        public ArrayList<DfEAttendance> parseAttendanceXML(String AttendancesXML) {
            ArrayList<DfEAttendance> Attendances = new ArrayList();
            Element AttendancesElement = getElementFromXMLString(AttendancesXML);

            if (AttendancesElement != null) {
                List<Element> yearDataElementList = AttendancesElement.getChildren(DfEAttendance.ELEMENT_YEAR_DATA);

                for (int i = 0; i < yearDataElementList.size(); i++) {
                    Element yearDataElement = yearDataElementList.get(i);

                    DfEAttendance Attendance = new DfEAttendance(yearDataElement);

                    Attendances.add(Attendance);
                }
            }

            return Attendances;
        }

        /**
         * Parse the imported CTF/ATF School History in an ArrayList for DfESchoolHistory.
         *
         * @param schoolHistoryXML String
         * @return ArrayList<DfESchoolHistory>
         */
        public ArrayList<DfESchoolHistory> parseSchoolHistoryXML(String schoolHistoryXML) {
            ArrayList<DfESchoolHistory> schoolHistories = new ArrayList();
            Element schoolHistoryElement = getElementFromXMLString(schoolHistoryXML);

            if (schoolHistoryElement != null) {
                List<Element> schoolHistoryElementList =
                        schoolHistoryElement.getChildren(DfESchoolHistory.ELEMENT_SCHOOL);

                for (int i = 0; i < schoolHistoryElementList.size(); i++) {
                    Element schoolElement = schoolHistoryElementList.get(i);
                    DfESchoolHistory schoolHistory = new DfESchoolHistory(schoolElement);
                    // Only the current school will be the last school
                    schoolHistory.setLastSchool(BOOLEAN_FALSE);
                    schoolHistories.add(schoolHistory);
                }
            }

            return schoolHistories;
        }

        /**
         * Parse the imported CTF Stage Assessment in an ArrayList for DfEStageAssessment.
         *
         * @param stageAssessmentsXML String
         * @return ArrayList<DfEStageAssessment>
         */
        public ArrayList<DfEStageAssessment> parseStageAssessmentXML(String stageAssessmentsXML) {
            ArrayList<DfEStageAssessment> stageAssessments = new ArrayList();
            Element stageAssessmentsElement = getElementFromXMLString(stageAssessmentsXML);

            if (stageAssessmentsElement != null) {
                List<Element> keyStageElementList =
                        stageAssessmentsElement.getChildren(DfEStageAssessment.ELEMENT_KEY_STAGE);

                for (int i = 0; i < keyStageElementList.size(); i++) {
                    Element keyStageElement = keyStageElementList.get(i);
                    Element stageElement = keyStageElement.getChild(DfEStageAssessment.ELEMENT_STAGE);
                    String stage = stageElement.getText().trim();

                    List<Element> stageAssessmentElementList =
                            keyStageElement.getChildren(DfEStageAssessment.ELEMENT_STAGE_ASSESSMENT);

                    for (int j = 0; j < stageAssessmentElementList.size(); j++) {
                        Element stageAssessmentElement = stageAssessmentElementList.get(j);

                        DfEStageAssessment stageAssessment = new DfEStageAssessment(stage, stageAssessmentElement);
                        stageAssessment.setStage(stage);

                        stageAssessments.add(stageAssessment);
                    }

                }
            }

            return stageAssessments;
        }

        /**
         * State Report Data class for gathering data, using StudentHistoryHelper, calculating
         * enrollment history.
         * This export should report a row for each student/school combination.
         * It should report positive attendance, and report positive codes for each day in
         * membership.
         * Days not in membership should be reported as empty.
         *
         * @author X2 Development Corporation
         */
        class AttendanceStatistics extends StateReportData {
            /*
             * Constants: Parameters, Constants
             */
            protected static final String DEFAULT_CALENDAR_NAME = "Standard";
            protected static final String PARAM_END_DATE = "endDate";
            protected static final String PARAM_START_DATE = "startDate";

            /*
             * Instance variables.
             */
            protected PlainDate m_endDate;
            protected EnrollmentManager m_enrollmentManager;
            protected StudentHistoryHelper m_helper;
            protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars =
                    new HashMap<String, Map<String, Set<PlainDate>>>();
            protected PlainDate m_startDate;
            protected SisSchool m_schoolLocal;

            /**
             * Initialize the export.
             * Set up the student history helper.
             */
            @Override
            public void initialize() {
                loadSchoolYears();

                int numOfSchoolYears = m_schoolYearRanges.size() - 1;

                PlainDate firstDateAllYears = m_schoolYearRanges.get(0).get(0);
                PlainDate lastDateAllYears = m_schoolYearRanges.get(numOfSchoolYears).get(1);

                m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
                m_helper = new StudentHistoryHelper(this);
                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, firstDateAllYears);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, lastDateAllYears);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.FALSE);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_INACTIVE, Boolean.TRUE);
                m_helper.setSelectionProperty("PROPERTY_LOAD_ALL_ATTENDANCE", Boolean.TRUE);
                if (m_schoolLocal != null) {
                    Criteria studentCriteria = m_helper.getStudentCriteria();
                    studentCriteria.addEqualTo(Student.COL_SCHOOL_OID, m_schoolLocal.getOid());
                }
                setQuery(m_helper.getStudentQuery(false));
            }

            /**
             * Set the initial school from the report save state.
             *
             * @param school void
             */
            public void setSchool(SisSchool school) {
                m_schoolLocal = school;
            }

            /**
             * Returns a list of all in-session PlainDate for the specified school and calendarId.
             *
             * @param school SisSchool
             * @param calendar String
             * @return Set<PlainDate>
             */
            protected Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
                if (!m_schoolsToCalendars.containsKey(school.getOid())) {
                    Map<String, Set<PlainDate>> calendarData =
                            m_enrollmentManager.getCalendarLookup(school, m_startDate, m_endDate);
                    m_schoolsToCalendars.put(school.getOid(), calendarData);
                }

                Set<PlainDate> set = m_schoolsToCalendars.get(school.getOid()).get(calendar);
                if (set == null) {
                    set = m_schoolsToCalendars.get(school.getOid()).get(DEFAULT_CALENDAR_NAME);
                    if (set == null) {
                        Set<Entry<String, Set<PlainDate>>> entrySet =
                                m_schoolsToCalendars.get(school.getOid()).entrySet();
                        if (entrySet.size() == 1) {
                            set = entrySet.iterator().next().getValue();
                        }
                    }
                }
                return set;
            }
        }

    }


}
