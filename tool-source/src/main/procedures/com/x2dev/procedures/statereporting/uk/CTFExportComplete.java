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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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

public class CTFExportComplete extends ToolJavaSource {
    private static final String PARAM_STUDENT_OIDS = "student-oids";
    private static final String PARAM_DESTINATION_LEA = "dest-lea";
    private static final String PARAM_DESTINATION_ESTAB = "dest-estab";

    private List<String> m_messages = new LinkedList();
    private DfEManager m_dfEManager = null;
    private Document XMLDocument = null;

    private ArrayList<String> m_studentOids = new ArrayList<String>();
    private Collection m_students;

    private SisSchool m_school = null;
    private String m_sourceLEA = null;
    private String m_sourceEstab = null;
    private String m_destLEA = null;
    private String m_destEstab = null;

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        logMessage("CTF Export Starting...");

        loadParameters();

        exportData();

        logMessage("CTF Export Completed.");

        if (XMLDocument != null) {
            exportResults();
        } else {
            displayMessages();
        }
    }

    /**
     * Load parameters.
     */
    protected void loadParameters() {
        String studentOidList = (String) getParameter(PARAM_STUDENT_OIDS);

        if (StringUtils.isEmpty(studentOidList)) {
            logMessage("Error: No students selected.");
        } else {
            m_studentOids = StringUtils.convertDelimitedStringToList(studentOidList, ',');
            loadStudents();

            m_school = (SisSchool) getSchool();
            if (m_school == null) {
                logMessage("Error: m_school is empty.");
            }

            m_destLEA = (String) getParameter(PARAM_DESTINATION_LEA);
            logMessage("m_destLEA: " + m_destLEA);
            m_destEstab = (String) getParameter(PARAM_DESTINATION_ESTAB);
            logMessage("m_destEstab: " + m_destEstab);
        }
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
     * Export to XMLDocument.
     */
    protected void exportData() {
        m_dfEManager = new DfEManager(getBroker());

        if (validateSchool()) {
            if (m_students.size() > 0) {
                XMLDocument = createXMLDocument();
            } else {
                logMessage("Error: No students selected.");
            }
        }
    }

    /**
     * Validate the school selected.
     *
     * @return true, if successful
     */
    private boolean validateSchool() {
        if (m_school == null) {
            logMessage("Error: The selected school doesn't exist.");

            return false;
        }
        m_sourceEstab = m_dfEManager.getEstabBySchool(m_school);

        Schedule schedule = m_school.getActiveSchedule();
        if (schedule == null) {
            logMessage("Error: The selected school " + m_school.getName() + " doesn't have an active schedule.");

            return false;
        }

        m_sourceLEA = m_dfEManager.getLEABySchool(m_school);
        if (m_sourceLEA == null) {
            logMessage("Error: The selected school's LEA field has not been setup in the Data Dictionary.");

            return false;
        }

        return true;
    }

    /**
     * Creates the XML document.
     *
     * @return Document
     */
    /*
     * Create DfE CTF XML Document from Student
     *
     * @param student
     *
     * @return Document
     */
    private Document createXMLDocument() {
        Document xmlDocument = new Document();

        Element rootElement = new Element(DfEManager.ELEMENT_CTFILE);

        Element headerElement = getCreateHeaderElement(m_school);
        rootElement.addContent(headerElement);

        Element cTFPupilDataElement = new Element(DfEManager.ELEMENT_CTF_PUPIL_DATA);
        Iterator it = m_students.iterator();
        while (it.hasNext()) {
            SisStudent student = (SisStudent) it.next();

            Element pupilElement = getCreateCTFPupilDataElement(student);

            cTFPupilDataElement.addContent(pupilElement);
        }
        rootElement.addContent(cTFPupilDataElement);

        xmlDocument.setRootElement(rootElement);

        return xmlDocument;
    }

    /**
     * Gets the creates the header element.
     *
     * @param school SisSchool
     * @return Element
     */
    /*
     * Create DfE CTF Header Element from Student
     *
     * @param student
     *
     * @return Element
     */
    private Element getCreateHeaderElement(SisSchool school) {
        Schedule schedule = school.getActiveSchedule();
        DistrictSchoolYearContext context = schedule.getDistrictContext();
        String sourceSchoolAcademicYear = Integer.toString(context.getSchoolYear());

        DfECTFHeader dfECTFHeader = m_dfEManager.getDfECTFHeaderByStudent(m_sourceLEA, m_sourceEstab, school.getName(),
                sourceSchoolAcademicYear, m_destLEA, m_destEstab);

        Element headerElement = m_dfEManager.getHeaderElement(dfECTFHeader);

        return headerElement;
    }

    /**
     * Gets the creates the CTF pupil data element.
     *
     * @param student SisStudent
     * @return Element
     */
    /*
     * Create DfE CTF PupilData Element
     *
     * @param student
     *
     * @return Element
     */
    private Element getCreateCTFPupilDataElement(SisStudent student) {
        DfEPupil dfEPupil = m_dfEManager.getDfEPupilByStudent(student);

        Element pupilElement = m_dfEManager.getCTFPupilDataElement(dfEPupil);

        return pupilElement;
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
     * Returns the entire list of log messages.
     *
     * @return List<String
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
     * Print out the logs to the user.
     *
     * @throws X2BaseException exception
     */
    private void displayMessages() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);

        MessageResources resources = LocalizationCache.getMessages(getUser().getPersistenceKey());

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
         * Constructor for DfE (UK Department for Education) Address Object.
         *
         * @param addressElement Element
         */
        public DfEAddress(Element addressElement) {
            Element bS7666AddressElement = addressElement.getChild(ELEMENT_BS7666_ADDRESS);
            if (bS7666AddressElement != null) {
                bS7666Address = true;
                setPAON(bS7666AddressElement.getChild(ELEMENT_PAON));
                setSAON(bS7666AddressElement.getChild(ELEMENT_SAON));
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
         * @param pAON void
         */
        public void setPAON(Element pAON) {
            if (pAON != null) {
                this.pAON = pAON.getText().trim();
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
         * @param sAON void
         */
        public void setSAON(Element sAON) {
            if (sAON != null) {
                this.sAON = sAON.getText().trim();
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
         * @param street void
         */
        public void setStreet(Element street) {
            if (street != null) {
                this.street = street.getText().trim();
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
         * @param locality void
         */
        public void setLocality(Element locality) {
            if (locality != null) {
                this.locality = locality.getText().trim();
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
         * @param town void
         */
        public void setTown(Element town) {
            if (town != null) {
                this.town = town.getText().trim();
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
         * @param addressLine1 void
         */
        public void setAddressLine1(Element addressLine1) {
            if (addressLine1 != null) {
                this.addressLine1 = addressLine1.getText().trim();
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
         * @param addressLine2 void
         */
        public void setAddressLine2(Element addressLine2) {
            if (addressLine2 != null) {
                this.addressLine2 = addressLine2.getText().trim();
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
         * @param addressLine3 void
         */
        public void setAddressLine3(Element addressLine3) {
            if (addressLine3 != null) {
                this.addressLine3 = addressLine3.getText().trim();
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
         * @param addressLine4 void
         */
        public void setAddressLine4(Element addressLine4) {
            if (addressLine4 != null) {
                this.addressLine4 = addressLine4.getText().trim();
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
         * @param addressLine5 void
         */
        public void setAddressLine5(Element addressLine5) {
            if (addressLine5 != null) {
                this.addressLine5 = addressLine5.getText().trim();
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
         * @param county void
         */
        public void setCounty(Element county) {
            if (county != null) {
                this.county = county.getText().trim();
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
         * @param postCode void
         */
        public void setPostCode(Element postCode) {
            if (postCode != null) {
                this.postCode = postCode.getText().trim();
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
         * @param country void
         */
        public void setCountry(Element country) {
            if (country != null) {
                this.country = country.getText().trim();
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
         * @param administrativeArea void
         */
        public void setAdministrativeArea(Element administrativeArea) {
            if (administrativeArea != null) {
                this.administrativeArea = administrativeArea.getText().trim();
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
         * @param postTown void
         */
        public void setPostTown(Element postTown) {
            if (postTown != null) {
                this.postTown = postTown.getText().trim();
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
         * @param uniquePropertyReferenceNumber void
         */
        public void setUniquePropertyReferenceNumber(Element uniquePropertyReferenceNumber) {
            if (uniquePropertyReferenceNumber != null) {
                this.uniquePropertyReferenceNumber = uniquePropertyReferenceNumber.getText().trim();
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
         * @param zip void
         */
        public void setZip(Element zip) {
            if (zip != null) {
                this.zip = zip.getText().trim();
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
         * @param easting void
         */
        public void setEasting(Element easting) {
            if (easting != null) {
                this.easting = easting.getText().trim();
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
         * @param northing void
         */
        public void setNorthing(Element northing) {
            if (northing != null) {
                this.northing = northing.getText().trim();
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
        private Boolean responsible = null;
        private Boolean addressAsPupil = null;
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
         * @param order void
         */
        public void setOrder(Element order) {
            if (order != null) {
                String orderStr = order.getText().trim();
                if (StringUtils.isInteger(orderStr)) {
                    int orderInt = Integer.parseInt(orderStr);
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
         * @param title void
         */
        public void setTitle(Element title) {
            if (title != null) {
                this.title = title.getText().trim();
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
         * @param surname void
         */
        public void setSurname(Element surname) {
            if (surname != null) {
                this.surname = surname.getText().trim();
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
         * @param forename void
         */
        public void setForename(Element forename) {
            if (forename != null) {
                this.forename = forename.getText().trim();
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
         * @param gender void
         */
        public void setGender(Element gender) {
            if (gender != null) {
                this.gender = gender.getText().trim();
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
         * @param middleNames void
         */
        public void setMiddleNames(Element middleNames) {
            if (middleNames != null) {
                this.middleNames = middleNames.getText().trim();
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
         * @param relationshipCode void
         */
        public void setRelationship(Element relationshipCode) {
            if (relationshipCode != null) {
                this.relationshipCode = relationshipCode.getText().trim();
            }
        }

        /**
         * Gets the Responsible flag.
         *
         * @return Boolean
         */
        public Boolean getResponsible() {
            return responsible;
        }

        /**
         * Sets the responsible flag.
         *
         * @param responsible void
         */
        public void setResponsible(Boolean responsible) {
            this.responsible = responsible;
        }

        /**
         * Sets the responsible from a DfE XML Element.
         *
         * @param responsible void
         */
        public void setResponsible(Element responsible) {
            if (responsible != null) {
                String resp = responsible.getText().trim().toLowerCase();
                this.responsible = Boolean.valueOf(DfEManager.TRUE.equals(resp));
            }
        }

        /**
         * Gets the isAddressAsPupil flag.
         *
         * @return Boolean
         */
        public Boolean getAddressAsPupil() {
            return addressAsPupil;
        }

        /**
         * Sets the addressAsPupil flag.
         *
         * @param addressAsPupil void
         */
        public void setAddressAsPupil(Boolean addressAsPupil) {
            this.addressAsPupil = addressAsPupil;
        }

        /**
         * Sets the addressAsPupil from a DfE XML Element.
         *
         * @param addressAsPupil void
         */
        public void setAddressAsPupil(Element addressAsPupil) {
            if (addressAsPupil != null) {
                String sameAddress = addressAsPupil.getText().trim().toLowerCase();
                this.addressAsPupil = Boolean.valueOf(DfEManager.TRUE.equals(sameAddress));
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
         * @param email void
         */
        public void setEmail(Element email) {
            if (email != null) {
                this.email = email.getText().trim();
            }
        }

    }

    /**
     * The Class DfECTFHeader.
     */
    class DfECTFHeader {
        public static final String ELEMENT_HEADER = "Header";
        public static final String ELEMENT_DOCUMENT_NAME = "DocumentName";
        public static final String ELEMENT_CTF_VERSION = "CTFversion";
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

        public static final String CTF_DOCUMENT_NAME = "Common Transfer File";
        public static final String CTF_VERSION = "11.0";
        public static final String DEFAULT_DOCUMENT_QUALIFIER = "full";
        // Note: Setting the Supplier to FSC
        public static final String DEFAULT_SUPPLIER_ID = "FSC";

        private String documentName = null;
        private String cTFVersion = null;
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
        public DfECTFHeader() {}

        /**
         * Constructor for DfE (UK Department for Education) Header Object.
         *
         * @param HeaderElement Element
         */
        public DfECTFHeader(Element HeaderElement) {
            setDocumentName(HeaderElement.getChild(ELEMENT_DOCUMENT_NAME));
            setCTFVersion(HeaderElement.getChild(ELEMENT_CTF_VERSION));
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
                this.documentName = documentNameElement.getText().trim();
            }
        }

        /**
         * Gets the cTFversion.
         *
         * @return String
         */
        public String getCTFVersion() {
            return cTFVersion;
        }

        /**
         * Sets the cTFVersion.
         *
         * @param cTFVersion void
         */
        public void setCTFVersion(String cTFVersion) {
            this.cTFVersion = cTFVersion;
        }

        /**
         * Sets the cTFVersion from a DfE XML Element.
         *
         * @param cTFVersionElement void
         */
        public void setCTFVersion(Element cTFVersionElement) {
            if (cTFVersionElement != null) {
                this.cTFVersion = cTFVersionElement.getText().trim();
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
                this.dateTime = dateTimeElement.getText().trim();
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
                this.documentQualifier = documentQualifierElement.getText().trim();
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
                this.dataQualifier = dataQualifierElement.getText().trim();
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
                this.dataDescriptor = dataDescriptorElement.getText().trim();
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
                this.supplierID = supplierIDElement.getText().trim();
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
                this.sourceSchoolLEA = sourceSchoolLEAElement.getText().trim();
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
                this.sourceSchoolEstab = sourceSchoolEstabElement.getText().trim();
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
                this.sourceSchoolName = sourceSchoolNameElement.getText().trim();
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
                this.sourceSchoolAcademicYear = sourceSchoolAcademicYearElement.getText().trim();
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
                this.destSchoolLEA = destSchoolLEAElement.getText().trim();
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
                this.destSchoolEstab = destSchoolEstabElement.getText().trim();
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
                this.suppID = suppIDElement.getText().trim();
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
         * @param languageType void
         */
        public void setLanguageType(Element languageType) {
            if (languageType != null) {
                this.languageType = languageType.getText().trim();
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
         * @param languageCode void
         */
        public void setLanguageCode(Element languageCode) {
            if (languageCode != null) {
                this.languageCode = languageCode.getText().trim();
            }
        }

    }

    /**
     * The Class DfEPupil.
     */
    class DfEPupil {
        // Pupil Sub Element Names
        public static final String ELEMENT_PUPIL = "Pupil";
        public static final String ELEMENT_UNIQUE_PUPIL_NUMBER = "UPN";
        public static final String ELEMENT_UNIQUE_LEARNER_NUMBER = "UniqueLearnerNumber";
        public static final String ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER = "ICI";
        public static final String ELEMENT_SURNAME = "Surname";
        public static final String ELEMENT_FORENAME = "Forename";
        public static final String ELEMENT_DATE_OF_BIRTH = "DOB";
        public static final String ELEMENT_GENDER = "Gender";
        public static final String ELEMENT_BASIC_DETAILS = "BasicDetails";
        public static final String ELEMENT_FORMER_UNIQUE_PUPIL_NUMBER = "FormerUPN";
        public static final String ELEMENT_PREFERRED_SURNAME = "PreferredSurname";
        public static final String ELEMENT_FORMER_SURNAME = "FormerSurname";
        public static final String ELEMENT_PREFERRED_FORENAME = "PreferredForename";
        public static final String ELEMENT_MIDDLE_NAMES = "MiddleNames";
        public static final String ELEMENT_NC_YEAR_ACTUAL = "NCyearActual";
        public static final String ELEMENT_ETHINICITY = "Ethnicity";
        public static final String ELEMENT_ETHINICITY_SOURCE = "EthnicitySource";
        public static final String ELEMENT_ENROLL_STATUS = "EnrolStatus";
        public static final String ELEMENT_FSM_REVIEW_DATE = "FSMreviewDate";
        public static final String ELEMENT_FSM_START_DATE = "FSMstartDate";
        public static final String ELEMENT_FSM_END_DATE = "FSMendDate";
        public static final String ELEMENT_NAW_DETAILS = "NAWdetails";
        public static final String ELEMENT_SPEAK_WELSH = "SpeakWelsh";
        public static final String ELEMENT_HOME_WELSH = "HomeWelsh";
        public static final String ELEMENT_NATIONAL_IDENTITY = "NationalIdentity";
        public static final String ELEMENT_WELSH_SOURCE = "WelshSource";
        public static final String ELEMENT_EAL_ACQUISITION = "EALAcquisition";
        public static final String ELEMENT_LOOK_AFTER = "LookAfter";
        public static final String ELEMENT_IN_CARE = "InCare";
        public static final String ELEMENT_CARE_AUTHORITY = "CareAuthority";
        public static final String ELEMENT_LANGUAGES = "Languages";
        public static final String ELEMENT_TYPE = "Type";
        public static final String ELEMENT_MEDICAL_FLAG = "MedicalFlag";
        public static final String ELEMENT_DISABILITIES = "Disabilities";
        public static final String ELEMENT_DISABILITY = "Disability";
        public static final String ELEMENT_SEN_HISTORY = "SENhistory";
        public static final String ELEMENT_SEN = "SEN";
        public static final String ELEMENT_SEN_START_DATE = "StartDate";
        public static final String ELEMENT_SEN_PROVISION = "SENprovision";
        public static final String ELEMENT_SEN_NEEDS = "SENneeds";
        public static final String ELEMENT_SEN_NEED = "SENneed";
        public static final String ELEMENT_ADDRESS = "Address";
        public static final String ELEMENT_PHONES = "Phones";
        public static final String ELEMENT_PHONE = "Phone";
        public static final String ELEMENT_EMAIL = "Email";
        public static final String ELEMENT_CONTACTS = "Contacts";
        public static final String ELEMENT_CONTACT = "Contact";
        public static final String ELEMENT_ATTENDANCE = "Attendance";
        public static final String ELEMENT_STAGE_ASSESSMENTS = "StageAssessments";
        public static final String ELEMENT_SCHOOL_HISTORY = "SchoolHistory";

        public static final String DEFAULT_ETHNICITY_SOURCE = "C";
        public static final String DEFAULT_ENROLL_STATUS = "C";
        public final Boolean DEFAULT_MEDICAL_FLAG = Boolean.valueOf(false);

        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

        // Pupil variables
        private String uniquePupilNumber = null;
        private String uniqueLearnerNumber = null;
        private String uniqueCandidateIdentifier = null;
        private String surname = null;
        private String forename = null;
        private PlainDate birthDate = null;
        private String gender = null;
        private String formerUniquePupilNumber = null;
        private String preferredSurname = null;
        private String formerSurname = null;
        private String preferredForename = null;
        private String middleNames = null;
        private String nCYearActual = null;
        private String ethnicity = null;
        private String ethnicitySource = null;
        private ArrayList<DfELanguage> languages = new ArrayList();
        private Boolean medicalFlag = null;
        private ArrayList<String> disabilities = new ArrayList();
        private String enrollStatus = null;
        private PlainDate fSMReviewDate = null;
        private PlainDate fSMStartDate = null;
        private PlainDate fSMEndDate = null;
        private String speakWelsh = null;
        private String homeWelsh = null;
        private String nationalIdentity = null;
        private String welshSource = null;
        private String eALAcquisition = null;
        private String inCare = null;
        private String careAuthority = null;
        private PlainDate sENStartDate = null;
        private String sENProvision = null;
        private ArrayList<DfESENNeed> sENNeeds = new ArrayList();
        private DfEAddress dfEAddress = null;
        private ArrayList<DfEContact> contacts = new ArrayList();
        private ArrayList<DfETelephone> telephones = new ArrayList();
        private String email = null;
        private String sENHistoryXML = null;
        private String attendanceXML = null;
        private String assessmentsXML = null;
        private String schoolHistoryXML = null;

        /**
         * Constructor for DfE (UK Department for Education) Pupil Object.
         *
         * @param pupilElement Element
         */
        public DfEPupil(Element pupilElement) {
            setUniquePupilNumber(pupilElement.getChild(ELEMENT_UNIQUE_PUPIL_NUMBER));
            setUniqueLearnerNumber(pupilElement.getChild(ELEMENT_UNIQUE_LEARNER_NUMBER));
            setUniqueCandidateIdentifier(pupilElement.getChild(ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER));
            setForename(pupilElement.getChild(ELEMENT_FORENAME));
            setSurname(pupilElement.getChild(ELEMENT_SURNAME));
            setBirthDate(pupilElement.getChild(ELEMENT_DATE_OF_BIRTH));
            setGender(pupilElement.getChild(ELEMENT_GENDER));

            Element basicDetailsElement = pupilElement.getChild(ELEMENT_BASIC_DETAILS);
            setFormerUniquePupilNumber(basicDetailsElement.getChild(ELEMENT_FORMER_UNIQUE_PUPIL_NUMBER));
            setPreferredSurname(basicDetailsElement.getChild(ELEMENT_PREFERRED_SURNAME));
            setFormerSurname(basicDetailsElement.getChild(ELEMENT_FORMER_SURNAME));
            setPreferredForename(basicDetailsElement.getChild(ELEMENT_PREFERRED_FORENAME));
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

            // Using CTF Ver 11.
            setFSMReviewDate(basicDetailsElement.getChild(ELEMENT_FSM_REVIEW_DATE));
            setFSMStartDate(basicDetailsElement.getChild(ELEMENT_FSM_START_DATE));
            setFSMEndDate(basicDetailsElement.getChild(ELEMENT_FSM_END_DATE));

            setEnrollStatus(basicDetailsElement.getChild(ELEMENT_ENROLL_STATUS));
            setMedicalFlag(basicDetailsElement.getChild(ELEMENT_MEDICAL_FLAG));

            Element disabiliesElement = basicDetailsElement.getChild(ELEMENT_DISABILITIES);
            if (disabiliesElement != null) {
                List<Element> disabilityElementList = disabiliesElement.getChildren(ELEMENT_DISABILITY);
                for (int l = 0; l < disabilityElementList.size(); l++) {
                    Element disabilityElement = disabilityElementList.get(l);
                    if (disabilityElement != null) {
                        String disability = disabilityElement.getText().trim();
                        disabilities.add(disability);
                    }
                }
            }

            Element nAWDetailsElement = pupilElement.getChild(ELEMENT_NAW_DETAILS);
            if (nAWDetailsElement != null) {
                setSpeakWelsh(nAWDetailsElement.getChild(ELEMENT_SPEAK_WELSH));
                setHomeWelsh(nAWDetailsElement.getChild(ELEMENT_HOME_WELSH));
                setNationalIdentity(nAWDetailsElement.getChild(ELEMENT_NATIONAL_IDENTITY));
                setWelshSource(nAWDetailsElement.getChild(ELEMENT_WELSH_SOURCE));
                setEALAcquisition(nAWDetailsElement.getChild(ELEMENT_EAL_ACQUISITION));
            }

            Element lookAfterElement = pupilElement.getChild(ELEMENT_LOOK_AFTER);
            if (lookAfterElement != null) {
                setInCare(lookAfterElement.getChild(ELEMENT_IN_CARE));
                setCareAuthority(lookAfterElement.getChild(ELEMENT_CARE_AUTHORITY));
            }

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

            Element addressElement = pupilElement.getChild(ELEMENT_ADDRESS);
            if (addressElement != null) {
                setDfEAddress(new DfEAddress(addressElement));
            }

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
            sENHistoryElement = pupilElement.getChild(ELEMENT_SEN_HISTORY);
            String sENHistoryStr = null;
            if (sENHistoryElement != null) {
                sENHistoryStr = xMLOutputter.outputString(sENHistoryElement);
                setSENHistoryXML(sENHistoryStr);
            }
            Element attendanceElement = pupilElement.getChild(ELEMENT_ATTENDANCE);
            String attendanceStr = null;
            if (attendanceElement != null) {
                attendanceStr = xMLOutputter.outputString(attendanceElement);
                setAttendanceXML(attendanceStr);
            }
            Element assessmentsElement = pupilElement.getChild(ELEMENT_STAGE_ASSESSMENTS);
            String assessmentsStr = null;
            if (assessmentsElement != null) {
                assessmentsStr = xMLOutputter.outputString(assessmentsElement);
                setAssessmentsXML(assessmentsStr);
            }
            Element schoolHistoryElement = pupilElement.getChild(ELEMENT_SCHOOL_HISTORY);
            String schoolHistoryStr = null;
            if (schoolHistoryElement != null) {
                schoolHistoryStr = xMLOutputter.outputString(schoolHistoryElement);
                setSchoolHistoryXML(schoolHistoryStr);
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
         * @param uniquePupilNumber void
         */
        public void setUniquePupilNumber(Element uniquePupilNumber) {
            if (uniquePupilNumber != null) {
                this.uniquePupilNumber = uniquePupilNumber.getText().trim();
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
         * @param uniqueLearnerNumber void
         */
        public void setUniqueLearnerNumber(Element uniqueLearnerNumber) {
            if (uniqueLearnerNumber != null) {
                this.uniqueLearnerNumber = uniqueLearnerNumber.getText().trim();
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
         * @param uniqueCandidateIdentifier void
         */
        public void setUniqueCandidateIdentifier(Element uniqueCandidateIdentifier) {
            if (uniqueCandidateIdentifier != null) {
                this.uniqueCandidateIdentifier = uniqueCandidateIdentifier.getText().trim();
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
         * @param surname void
         */
        public void setSurname(Element surname) {
            if (surname != null) {
                this.surname = surname.getText().trim();
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
         * @param forename void
         */
        public void setForename(Element forename) {
            if (forename != null) {
                this.forename = forename.getText().trim();
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
                Date date = null;
                try {
                    String birthDate1 = birthDateElement.getText().trim();
                    date = m_dateFormat.parse(birthDate1);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                this.birthDate = new PlainDate(date);
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
         * @param gender void
         */
        public void setGender(Element gender) {
            if (gender != null) {
                this.gender = gender.getText().trim();
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
         * @param formerUniquePupilNumber void
         */
        public void setFormerUniquePupilNumber(Element formerUniquePupilNumber) {
            if (formerUniquePupilNumber != null) {
                this.formerUniquePupilNumber = formerUniquePupilNumber.getText().trim();
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
         * @param preferredSurname void
         */
        public void setPreferredSurname(Element preferredSurname) {
            if (preferredSurname != null) {
                this.preferredSurname = preferredSurname.getText().trim();
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
         * @param formerSurname void
         */
        public void setFormerSurname(Element formerSurname) {
            if (formerSurname != null) {
                this.formerSurname = formerSurname.getText().trim();
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
         * @param preferredForename void
         */
        public void setPreferredForename(Element preferredForename) {
            if (preferredForename != null) {
                this.preferredForename = preferredForename.getText().trim();
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
         * Sets the middle names.
         *
         * @param middleNames void
         */
        public void setMiddleNames(Element middleNames) {
            if (middleNames != null) {
                this.middleNames = middleNames.getText().trim();
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
         * @param nCYearActual void
         */
        public void setNCyearActual(Element nCYearActual) {
            if (nCYearActual != null) {
                this.nCYearActual = nCYearActual.getText().trim();
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
         * @param ethnicity void
         */
        public void setEthnicity(Element ethnicity) {
            if (ethnicity != null) {
                this.ethnicity = ethnicity.getText().trim();
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
         * @param ethnicitySource void
         */
        public void setEthnicitySource(Element ethnicitySource) {
            if (ethnicitySource != null) {
                this.ethnicitySource = ethnicitySource.getText().trim();
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
         * @param fSMReviewDate void
         */
        public void setFSMReviewDate(Element fSMReviewDate) {

            if (fSMReviewDate != null) {
                String reviewDate = fSMReviewDate.getText().trim();
                Date date = null;
                try {
                    date = m_dateFormat.parse(reviewDate);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                this.fSMReviewDate = new PlainDate(date);
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
         * @param enrollStatus void
         */
        public void setEnrollStatus(Element enrollStatus) {
            if (enrollStatus != null) {
                this.enrollStatus = enrollStatus.getText().trim();
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
         * @param email void
         */
        public void setEmail(Element email) {
            if (email != null) {
                this.email = email.getText().trim();
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
                String sENStartDate1 = sENStartDateElement.getText().trim();
                Date date = null;
                try {
                    date = m_dateFormat.parse(sENStartDate1);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                this.sENStartDate = new PlainDate(date);
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
                this.sENProvision = sENProvisionElement.getText().trim();
            }
        }

        /**
         * Gets the sENHistoryXML.
         *
         * @return String
         */
        public String getSENHistoryXML() {
            return sENHistoryXML;
        }

        /**
         * Sets the sENHistoryXML.
         *
         * @param sENHistoryXML void
         */
        public void setSENHistoryXML(String sENHistoryXML) {
            this.sENHistoryXML = sENHistoryXML;
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
                String resp = medicalFlagElement.getText().trim().toLowerCase();
                this.medicalFlag = Boolean.valueOf(DfEManager.TRUE.equals(resp));
            }
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
                String fSMStartDate1 = fSMStartDateElement.getText().trim();
                Date date = null;
                try {
                    date = m_dateFormat.parse(fSMStartDate1);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                this.fSMStartDate = new PlainDate(date);
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
                String fSMEndDate1 = fSMEndDateElement.getText().trim();
                Date date = null;
                try {
                    date = m_dateFormat.parse(fSMEndDate1);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                this.fSMEndDate = new PlainDate(date);
            }
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
                this.speakWelsh = speakWelshElement.getText().trim();
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
                this.homeWelsh = homeWelshElement.getText().trim();
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
                this.nationalIdentity = nationalIdentityElement.getText().trim();
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
                this.welshSource = welshSourceElement.getText().trim();
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
                this.eALAcquisition = eALAcquisitionElement.getText().trim();
            }
        }

        /**
         * Gets the inCare.
         *
         * @return String
         */
        public String getInCare() {
            return inCare;
        }

        /**
         * Sets the inCare.
         *
         * @param inCare void
         */
        public void setInCare(String inCare) {
            this.inCare = inCare;
        }

        /**
         * Sets the inCare from a DfE XML Element.
         *
         * @param inCareElement void
         */
        public void setInCare(Element inCareElement) {
            if (inCareElement != null) {
                this.inCare = inCareElement.getText().trim();
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
                this.careAuthority = careAuthorityElement.getText().trim();
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
         * @param type void
         */
        public void setType(Element type) {
            if (type != null) {
                this.type = type.getText().trim();
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
         * @param typeRank void
         */
        public void setTypeRank(Element typeRank) {
            if (typeRank != null) {
                this.typeRank = typeRank.getText().trim();
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
                this.telephoneType = telephoneTypeElement.getText().trim();
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
                this.telephoneNumber = telephoneNumberElement.getText().trim();
            }
        }

    }


    /**
     * The Class DfEManager.
     */
    class DfEManager {
        private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
        private static final String DEFAULT_ORGANIZATION = "*dst";
        private static final String DEFAULT_CONTACT_TYPE = "Student";
        private static final String DFE_PROGRAM_CODE_FRL = "FRL";
        private static final String DFE_PROGRAM_CODE_SE = "SE";
        private static final String ASPEN_PROGRAM_CODE_SE = "Special Education";
        private static final String ASPEN_PROGRAM_CODE_FRL = "Free/Reduced Lunch";

        public static final String TRUE = "true";
        public static final String FALSE = "false";

        public static final String ALIAS_NAME_LEA = "DFE LEA ID";
        public static final String ALIAS_NAME_ESTAB = "DFE ESTABLISHMENT ID";
        public static final String ALIAS_NAME_UPN = "DFE UPN";
        public static final String ALIAS_NAME_FORMER_UPN = "DFE FORMER UPN";
        public static final String ALIAS_NAME_PREFERRED_SURNAME = "DFE PREFERRED SURNAME";
        public static final String ALIAS_NAME_PREFERRED_FORENAME = "DFE PREFERRED FORENAME";
        public static final String ALIAS_NAME_FORMER_SURNAME = "DFE FORMER SURNAME";
        public static final String ALIAS_NAME_ETHNICITY = "DFE ETHNICITY";
        public static final String ALIAS_NAME_ULN = "DFE ULN";
        public static final String ALIAS_NAME_SEN_PUPIL_PROVISION = "DFE PUPIL SEN PROVISION";
        public static final String ALIAS_NAME_SEN_PROVISION = "DFE SEN PROVISION";
        public static final String ALIAS_NAME_SEN_TYPE = "DFE SEN TYPE";
        public static final String ALIAS_NAME_SEN_RANK = "DFE SEN RANK";

        public static final String ELEMENT_CTFILE = "CTfile";
        public static final String ELEMENT_HEADER = "Header";
        public static final String ELEMENT_CTF_PUPIL_DATA = "CTFpupilData";
        public static final String ELEMENT_PUPIL = "Pupil";

        protected static final char LABEL_PREFIX_CHAR = '$';

        /**
         * A local copy of the data dictionary for use by various lookup utilities.
         */
        private DataDictionary m_dictionary;

        /**
         * A local copy of the X2 Broker.
         */
        private X2Broker m_broker = null;

        /**
         * A local copy XML File Element builder.
         */
        private SAXBuilder m_builder = null;

        /**
         * Date and Time formats.
         */
        private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        private SimpleDateFormat m_timeFormat = new SimpleDateFormat("hh:mm:ss");

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
         */
        public DfEManager(X2Broker broker) {
            m_broker = broker;
            m_builder = new SAXBuilder();
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
                is.close();
            }

            return bytes;
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
         * Get a UK LEA by UK School .
         *
         * @param school School
         * @return lEA
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
         * Get a UK Establishment by UK School .
         *
         * @param school School
         * @return estab
         */
        public String getEstabBySchool(School school) {
            String estab = null;

            if (school != null) {
                estab = (String) school.getFieldValueByAlias(ALIAS_NAME_ESTAB, getDataDictionary());
            }

            return estab;
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
         * Using the following algorithm:
         * 1. Find by UPN
         * 2. Else find by Former UPN
         * 3. Else find by the combination of the students FirstName, LastName, DateOfBirth and
         * Gender
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

                X2Criteria studentCriteria2 = new X2Criteria();
                studentCriteria2.addEqualTo(studentUPNAliasName, dfEPupil.getFormerUniquePupilNumber());

                studentCriteria.addOrCriteria(studentCriteria2);

                BeanQuery studentQuery = new BeanQuery(Student.class, studentCriteria);
                Collection<Student> students = m_broker.getCollectionByQuery(studentQuery);

                if (students.size() > 0) {
                    for (Student student : students) {
                        existingStudent = student;
                        break;
                    }
                } else {
                    // Check Student Person's First Name, Last Name DOB and Gender Combination
                    studentCriteria = new X2Criteria();
                    studentCriteria.addEqualTo(Student.REL_PERSON + "." + Person.COL_FIRST_NAME,
                            dfEPupil.getForename());
                    studentCriteria.addEqualTo(Student.REL_PERSON + "." + Person.COL_LAST_NAME, dfEPupil.getSurname());
                    studentCriteria.addEqualTo(Student.REL_PERSON + "." + Person.COL_DOB, dfEPupil.getBirthDate());
                    studentCriteria.addEqualTo(Student.REL_PERSON + "." + Person.COL_GENDER_CODE, dfEPupil.getGender());

                    studentQuery = new BeanQuery(Student.class, studentCriteria);
                    students = m_broker.getCollectionByQuery(studentQuery);

                    if (students.size() > 0) {
                        for (Student student : students) {
                            existingStudent = student;
                            break;
                        }
                    }
                }
            }

            return existingStudent;
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
         * Set the data dictionary to be used on this export.
         * If not set, a district data dictionary will be used.
         *
         * @param dictionary void
         */
        public void setDataDictionary(DataDictionary dictionary) {
            m_dictionary = dictionary;
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
                String message = LocalizationCache.getMessages(getDataDictionary().getPersistenceKey())
                        .getMessage(ERROR_ALIAS_LOOKUP);
                AppGlobals.getLog().log(Level.SEVERE, message);
            }

            return javaName;
        }

        /**
         * Save a DfE Pupil to Aspen Person, Address, Student, Race, Contact & StudentContact Tables
         * .
         *
         * @param dfEPupil DfEPupil
         * @param schoolOid String
         */
        public void saveDfEPupil(DfEPupil dfEPupil, String schoolOid) {
            // Save StudentAddress
            DfEAddress dfEAddress = dfEPupil.getDfEAddress();
            Address studentAddress = null;
            String studentAddressOid = null;
            String studentAddressLine1 = null;
            if (dfEAddress != null) {
                studentAddress = saveAddress(dfEAddress);
                studentAddressOid = studentAddress.getOid();
                studentAddressLine1 = studentAddress.getAddressLine01();
            }

            // Save StudentPerson & StudentPersonRace
            Person studentPerson = saveStudentPerson(dfEPupil, studentAddressOid);
            String studentPeriodOid = studentPerson.getOid();

            // Save Student
            Student student = saveStudent(dfEPupil, studentPeriodOid, studentAddressLine1, schoolOid);
            String studentOid = student.getOid();

            // Save StudentProgramParticiation
            saveStudentProgramParticiation(dfEPupil, studentOid);

            // Save StudentDisability
            saveStudentDisability(dfEPupil, studentOid);

            // Save All Contacts, ContactPerson's and ContactAddress's
            ArrayList<DfEContact> contacts = dfEPupil.getContacts();
            for (int j = 0; j < contacts.size(); j++) {
                DfEContact dfEContact = contacts.get(j);

                // Save ContactAddress
                String contactAddressOid = null;
                String addressView = null;
                Boolean addressAsPupil = dfEContact.getAddressAsPupil();
                boolean livesWithStudent = false;
                if (addressAsPupil != null && addressAsPupil.booleanValue() == true) {
                    livesWithStudent = true;
                    contactAddressOid = studentAddressOid;
                    addressView = studentAddress.getAddressLine01();
                } else {
                    DfEAddress contactDfEAddress = dfEContact.getDfEAddress();

                    Address contactAddress = null;
                    if (contactDfEAddress != null) {
                        contactAddress = saveAddress(contactDfEAddress);
                        contactAddressOid = contactAddress.getOid();

                        addressView = contactAddress.getAddressLine01();
                    }
                }

                // Save ContactPerson
                Person contactPerson = saveContactPerson(dfEContact, contactAddressOid);
                String contactPersonOid = contactPerson.getOid();

                // Save Contact
                Contact contact = saveContact(dfEContact, contactPersonOid, addressView);
                String contactOid = contact.getOid();

                // Save StudentContact
                saveStudentContact(dfEContact, contactOid, studentOid, livesWithStudent);
            }
        }

        /**
         * Save a DfE Address to Aspen Address Table.
         *
         * @param dfEAddress DfEAddress
         * @return Address
         */
        public Address saveAddress(DfEAddress dfEAddress) {
            Address address = X2BaseBean.newInstance(Address.class, m_broker.getPersistenceKey());
            address.setOrganization1Oid(DEFAULT_ORGANIZATION);
            if (dfEAddress.hasBS7666Address()) {
                String streetNumber = dfEAddress.getPAON();
                if (StringUtils.isNumeric(streetNumber)) {
                    int streetNum = Integer.parseInt(streetNumber);
                    address.setStreetNumber(streetNum);
                } else {
                    address.setStreetLetter(streetNumber);
                }

                address.setStreetName(dfEAddress.getStreet());
                String streetAddrees = dfEAddress.getPAON() + " " + dfEAddress.getStreet();
                if (dfEAddress.getSAON() != null) {
                    streetAddrees = streetAddrees + " " + dfEAddress.getSAON();
                }
                address.setCity(dfEAddress.getTown());

                address.setAddressLine01(streetAddrees.trim());
                address.setAddressLine02(dfEAddress.getLocality());
            } else {
                address.setAddressLine01(dfEAddress.getAddressLine1());
                address.setAddressLine02(dfEAddress.getAddressLine2());
                address.setAddressLine03(dfEAddress.getAddressLine3());
            }
            address.setPostalCode(dfEAddress.getPostCode());
            address.setCounty(dfEAddress.getCounty());
            address.setCountry(dfEAddress.getCountry());
            m_broker.saveBeanForced(address);

            return address;
        }

        /**
         * Save a DfE Pupil to Aspen Person & Race Table.
         *
         * @param dfEPupil DfEPupil
         * @param studentAddressOid String
         * @return Person
         */
        public Person saveStudentPerson(DfEPupil dfEPupil, String studentAddressOid) {
            // Save StudentPerson
            Person studentPerson = X2BaseBean.newInstance(Person.class, m_broker.getPersistenceKey());
            studentPerson.setOrganization1Oid(DEFAULT_ORGANIZATION);
            studentPerson.setLastName(dfEPupil.getSurname());
            studentPerson.setFirstName(dfEPupil.getForename());
            studentPerson.setDob(dfEPupil.getBirthDate());
            // Translating DfE Gender code.
            String genderCode = lookupRefCodeByStateCode(Person.class, Person.COL_GENDER_CODE, dfEPupil.getGender());
            studentPerson.setGenderCode(genderCode);
            studentPerson.setMiddleName(dfEPupil.getMiddleNames());
            studentPerson.setPhysicalAddressOid(studentAddressOid);
            studentPerson.setMailingAddressOid(studentAddressOid);
            studentPerson.setStudentIndicator(true);
            studentPerson.setFieldValueByAlias(ALIAS_NAME_PREFERRED_SURNAME, dfEPupil.getPreferredSurname(),
                    getDataDictionary());
            studentPerson.setFieldValueByAlias(ALIAS_NAME_FORMER_SURNAME, dfEPupil.getFormerSurname(),
                    getDataDictionary());
            // Not translating DfE Ethnicity code, save as is.
            studentPerson.setFieldValueByAlias(ALIAS_NAME_ETHNICITY, dfEPupil.getEthnicity(), getDataDictionary());
            studentPerson.setEmail01(dfEPupil.getEmail());

            ArrayList<DfETelephone> telephones = dfEPupil.getTelephones();
            for (int l = 0; l < 3; l++) {
                if (l >= telephones.size()) {
                    break;
                }

                DfETelephone telephone = telephones.get(l);
                if (l == 0) {
                    studentPerson.setPhone01(telephone.getTelephoneNumber());
                } else if (l == 1) {
                    studentPerson.setPhone02(telephone.getTelephoneNumber());
                } else if (l == 2) {
                    studentPerson.setPhone03(telephone.getTelephoneNumber());
                }
            }
            m_broker.saveBeanForced(studentPerson);

            return studentPerson;
        }

        /**
         * Save a DfE Pupil to Aspen Student Table.
         *
         * @param dfEPupil DfEPupil
         * @param studentPersonOid String
         * @param addressView String
         * @param schoolOid String
         * @return Student
         */
        public Student saveStudent(DfEPupil dfEPupil, String studentPersonOid, String addressView, String schoolOid) {
            Student student = X2BaseBean.newInstance(Student.class, m_broker.getPersistenceKey());
            student.setPersonOid(studentPersonOid);
            student.setOrganization1Oid(DEFAULT_ORGANIZATION);
            student.setSchoolOid(schoolOid);
            student.setFieldValueByAlias(ALIAS_NAME_UPN, dfEPupil.getUniquePupilNumber(), getDataDictionary());
            student.setFieldValueByAlias(ALIAS_NAME_FORMER_UPN, dfEPupil.getFormerUniquePupilNumber(),
                    getDataDictionary());
            student.setFieldValueByAlias(ALIAS_NAME_ULN, dfEPupil.getUniqueLearnerNumber(), getDataDictionary());
            student.setFieldValueByAlias(ALIAS_NAME_SEN_PUPIL_PROVISION, dfEPupil.getSENProvision(),
                    getDataDictionary());
            student.setGradeLevel(dfEPupil.getNCYearActual());
            // Translating DfE Enroll Status code.
            String enrollStatus =
                    lookupRefCodeByStateCode(Student.class, Student.COL_ENROLLMENT_STATUS, dfEPupil.getEnrollStatus());
            student.setEnrollmentStatus(enrollStatus);
            // Translating DfE Language code.
            String languageCode = lookupRefCodeByStateCode(Student.class, Student.COL_HOME_LANGUAGE_CODE,
                    dfEPupil.getFirstLanguageCode());
            student.setHomeLanguageCode(languageCode);
            student.setNameView(dfEPupil.getSurname() + ", " + dfEPupil.getForename());
            student.setAddressView(addressView);

            student.setFieldD001(dfEPupil.getSENHistoryXML());
            student.setFieldD002(dfEPupil.getAttendanceXML());
            student.setFieldD003(dfEPupil.getAssessmentsXML());
            student.setFieldD004(dfEPupil.getSchoolHistoryXML());
            m_broker.saveBeanForced(student);

            return student;
        }

        /**
         * Save a DfE Pupil SEN information to Aspen Student Table.
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @return Student
         */
        public void saveStudentProgramParticiation(DfEPupil dfEPupil, String studentOid) {
            // FMS
            if (dfEPupil.getFSMStartDate() != null) {
                StudentProgramParticipation studentProgramParticipation =
                        X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                studentProgramParticipation.setStudentOid(studentOid);
                String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_FRL);
                studentProgramParticipation.setProgramCode(programCode);
                studentProgramParticipation.setStartDate(dfEPupil.getFSMStartDate());
                studentProgramParticipation.setEndDate(dfEPupil.getFSMEndDate());
                m_broker.saveBeanForced(studentProgramParticipation);
            }

            // SEN
            ArrayList<DfESENNeed> sENNeeds = dfEPupil.getSENNeeds();
            for (int i = 0; i < sENNeeds.size(); i++) {
                DfESENNeed sENNeed = sENNeeds.get(i);

                StudentProgramParticipation studentProgramParticipation =
                        X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                studentProgramParticipation.setStudentOid(studentOid);
                // Translating DfE Program code.
                String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_SE);
                studentProgramParticipation.setProgramCode(programCode);
                studentProgramParticipation.setStartDate(dfEPupil.getSENStartDate());
                // Not translating DfE SEN Provision code, save as is.
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_PROVISION, dfEPupil.getSENProvision(),
                        getDataDictionary());
                // Not translating DfE SEN Type code, save as is.
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_TYPE, sENNeed.getType(),
                        getDataDictionary());
                studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_RANK, sENNeed.getTypeRank(),
                        getDataDictionary());
                m_broker.saveBeanForced(studentProgramParticipation);
            }
        }

        /**
         * Save a DfE Pupil Disability information to Aspen Student Table.
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @return Student
         */
        public void saveStudentDisability(DfEPupil dfEPupil, String studentOid) {
            ArrayList<String> disabilities = dfEPupil.getDisabilities();
            for (int i = 0; i < disabilities.size(); i++) {
                String dfFDisabilityCode = disabilities.get(i);

                IepDisability disability = X2BaseBean.newInstance(IepDisability.class, m_broker.getPersistenceKey());
                disability.setStudentOid(studentOid);
                // Translating DfE Disability code.
                String disabilityCode = lookupRefCodeByStateCode(IepDisability.class, IepDisability.COL_DISABILITY_CODE,
                        dfFDisabilityCode);
                disability.setDisabilityCode(disabilityCode);
                disability.setPrimaryIndicator(false);
                disability.setTransportationIndicator(false);
                m_broker.saveBeanForced(disability);
            }
        }

        /**
         * Save a DfE Contact to Aspen Person Table.
         *
         * @param dfEContact DfEContact
         * @param contactAddressOid String
         * @return Person
         */
        public Person saveContactPerson(DfEContact dfEContact, String contactAddressOid) {
            Person contactPerson = X2BaseBean.newInstance(Person.class, m_broker.getPersistenceKey());
            contactPerson.setOrganization1Oid(DEFAULT_ORGANIZATION);
            String personTitleCode = dfEContact.getTitle();
            contactPerson.setNameTitleCode(personTitleCode);
            contactPerson.setLastName(dfEContact.getSurname());
            contactPerson.setFirstName(dfEContact.getForename());
            // Translating DfE Gender code.
            String genderCode = lookupRefCodeByStateCode(Person.class, Person.COL_GENDER_CODE, dfEContact.getGender());
            contactPerson.setGenderCode(genderCode);
            contactPerson.setMiddleName(dfEContact.getMiddleNames());
            contactPerson.setPhysicalAddressOid(contactAddressOid);
            contactPerson.setMailingAddressOid(contactAddressOid);
            contactPerson.setStudentIndicator(false);
            contactPerson.setContactIndicator(true);
            contactPerson.setEmail01(dfEContact.getEmail());

            ArrayList<DfETelephone> telephones = dfEContact.getTelephones();
            for (int l = 0; l < 3; l++) {
                if (l >= telephones.size()) {
                    break;
                }

                DfETelephone telephone = telephones.get(l);
                if (l == 0) {
                    contactPerson.setPhone01(telephone.getTelephoneNumber());
                } else if (l == 1) {
                    contactPerson.setPhone02(telephone.getTelephoneNumber());
                } else if (l == 2) {
                    contactPerson.setPhone03(telephone.getTelephoneNumber());
                }
            }
            m_broker.saveBeanForced(contactPerson);

            return contactPerson;
        }

        /**
         * Save a DfE Contact to Aspen Contact Table.
         *
         * @param dfEContact DfEContact
         * @param contactPersonOid String
         * @param addressView String
         * @return Contact
         */
        public Contact saveContact(DfEContact dfEContact, String contactPersonOid, String addressView) {
            Contact contact = X2BaseBean.newInstance(Contact.class, m_broker.getPersistenceKey());
            contact.setOrganization1Oid(DEFAULT_ORGANIZATION);
            contact.setAddressView(addressView);
            contact.setNameView(dfEContact.getSurname() + ", " + dfEContact.getForename());
            contact.setPersonOid(contactPersonOid);
            contact.setContactTypeCode(DEFAULT_CONTACT_TYPE);
            m_broker.saveBeanForced(contact);

            return contact;
        }

        /**
         * Save a DfE Contact to Aspen StudentContact Table.
         *
         * @param dfEContact DfEContact
         * @param contactOid String
         * @param studentOid String
         * @param livesWithStudent boolean
         * @return StudentContact
         */
        public void saveStudentContact(DfEContact dfEContact,
                                       String contactOid,
                                       String studentOid,
                                       boolean livesWithStudent) {
            StudentContact studentContact = X2BaseBean.newInstance(StudentContact.class, m_broker.getPersistenceKey());
            studentContact.setContactOid(contactOid);
            studentContact.setStudentOid(studentOid);
            studentContact.setLivesWithIndicator(livesWithStudent);
            // Translating DfE Relationship code.
            String relationshipCode = lookupRefCodeByStateCode(StudentContact.class,
                    StudentContact.COL_RELATIONSHIP_CODE, dfEContact.getRelationshipCode());
            studentContact.setRelationshipCode(relationshipCode);

            m_broker.saveBeanForced(studentContact);
        }

        /**
         * Get the DfECTFHeader from Student.
         *
         * @param sourceLEA String
         * @param sourceEstab String
         * @param schoolName String
         * @param sourceSchoolAcademicYear String
         * @param destLEA String
         * @param destEstab String
         * @return DfECTFHeader
         */
        public DfECTFHeader getDfECTFHeaderByStudent(String sourceLEA,
                                                     String sourceEstab,
                                                     String schoolName,
                                                     String sourceSchoolAcademicYear,
                                                     String destLEA,
                                                     String destEstab) {
            // Create DfECTFHeader
            DfECTFHeader dfECTFHeader = new DfECTFHeader();
            dfECTFHeader.setDocumentName(DfECTFHeader.CTF_DOCUMENT_NAME);
            dfECTFHeader.setCTFVersion(DfECTFHeader.CTF_VERSION);
            Date now = new Date();
            String dateTime = m_dateFormat.format(now) + "T" + m_timeFormat.format(now);
            dfECTFHeader.setDateTime(dateTime);
            dfECTFHeader.setDocumentQualifier(DfECTFHeader.DEFAULT_DOCUMENT_QUALIFIER);
            dfECTFHeader.setSupplierID(DfECTFHeader.DEFAULT_SUPPLIER_ID);
            dfECTFHeader.setSourceSchoolLEA(sourceLEA);
            dfECTFHeader.setSourceSchoolEstab(sourceEstab);
            dfECTFHeader.setSourceSchoolName(schoolName);
            dfECTFHeader.setSourceSchoolAcademicYear(sourceSchoolAcademicYear);
            dfECTFHeader.setDestSchoolLEA(destLEA);
            dfECTFHeader.setDestSchoolEstab(destEstab);

            return dfECTFHeader;
        }

        /**
         * Get Header Element from DfECTFHeader
         *
         * Note: Not include SuppInfo.
         *
         * @param dfECTFHeader DfECTFHeader
         * @return Element
         */
        public Element getHeaderElement(DfECTFHeader dfECTFHeader) {
            Element headerElement = new Element(DfEManager.ELEMENT_HEADER);

            Element documentNameElement = new Element(DfECTFHeader.ELEMENT_DOCUMENT_NAME);
            documentNameElement.addContent(dfECTFHeader.getDocumentName());
            headerElement.addContent(documentNameElement);
            Element cTFVersionElement = new Element(DfECTFHeader.ELEMENT_CTF_VERSION);
            cTFVersionElement.addContent(dfECTFHeader.getCTFVersion());
            headerElement.addContent(cTFVersionElement);
            Element dateTimeElement = new Element(DfECTFHeader.ELEMENT_DATE_TIME);
            dateTimeElement.addContent(dfECTFHeader.getDateTime());
            headerElement.addContent(dateTimeElement);
            if (dfECTFHeader.getDocumentQualifier() != null) {
                Element documentQualifierElement = new Element(DfECTFHeader.ELEMENT_DOCUMENT_QUALIFIER);
                documentQualifierElement.addContent(dfECTFHeader.getDocumentQualifier());
                headerElement.addContent(documentQualifierElement);
            }
            if (dfECTFHeader.getDataQualifier() != null) {
                Element dateQualifierElement = new Element(DfECTFHeader.ELEMENT_DATA_QUALIFIER);
                dateQualifierElement.addContent(dfECTFHeader.getDataQualifier());
                headerElement.addContent(dateQualifierElement);
            }
            if (dfECTFHeader.getDataDescriptor() != null) {
                Element dateDescriptorElement = new Element(DfECTFHeader.ELEMENT_DATA_DESCRIPTOR);
                dateDescriptorElement.addContent(dfECTFHeader.getDataDescriptor());
                headerElement.addContent(dateDescriptorElement);
            }
            if (dfECTFHeader.getSupplierID() != null) {
                Element supplierIDElement = new Element(DfECTFHeader.ELEMENT_SUPPLIER_ID);
                supplierIDElement.addContent(dfECTFHeader.getSupplierID());
                headerElement.addContent(supplierIDElement);
            }

            // Element SourceSchool
            Element sourceSchoolElement = new Element(DfECTFHeader.ELEMENT_SOURCE_SCHOOL);
            Element sourceSchoolLEAElement = new Element(DfECTFHeader.ELEMENT_LEA);
            sourceSchoolLEAElement.addContent(dfECTFHeader.getSourceSchoolLEA());
            sourceSchoolElement.addContent(sourceSchoolLEAElement);
            Element sourceSchoolEstabElement = new Element(DfECTFHeader.ELEMENT_ESTABISHMENT);
            sourceSchoolEstabElement.addContent(dfECTFHeader.getSourceSchoolEstab());
            sourceSchoolElement.addContent(sourceSchoolEstabElement);
            Element sourceSchoolNameElement = new Element(DfECTFHeader.ELEMENT_SCHOOL_NAME);
            sourceSchoolNameElement.addContent(dfECTFHeader.getSourceSchoolName());
            sourceSchoolElement.addContent(sourceSchoolNameElement);
            Element sourceSchoolAcademicYearElement = new Element(DfECTFHeader.ELEMENT_ACADEMIC_YEAR);
            sourceSchoolAcademicYearElement.addContent(dfECTFHeader.getSourceSchoolAcademicYear());
            sourceSchoolElement.addContent(sourceSchoolAcademicYearElement);
            headerElement.addContent(sourceSchoolElement);

            // Element DestSchool
            Element destSchoolElement = new Element(DfECTFHeader.ELEMENT_DESTINATION_SCHOOL);
            Element destSchoolLEAElement = new Element(DfECTFHeader.ELEMENT_LEA);
            destSchoolLEAElement.addContent(dfECTFHeader.getDestSchoolLEA());
            destSchoolElement.addContent(destSchoolLEAElement);
            Element destSchoolEstabElement = new Element(DfECTFHeader.ELEMENT_ESTABISHMENT);
            destSchoolEstabElement.addContent(dfECTFHeader.getDestSchoolEstab());
            destSchoolElement.addContent(destSchoolEstabElement);
            headerElement.addContent(destSchoolElement);

            return headerElement;
        }

        /**
         * Get DfE Pupil from Student
         *
         * Note: Not include SuppInfo.
         *
         * @param student SisStudent
         * @return DfEPupil
         */
        public DfEPupil getDfEPupilByStudent(SisStudent student) {
            // Get Student
            Person studentPerson = student.getPerson();

            DfEPupil dfEPupil = new DfEPupil();
            String uniquePupilNumber = (String) student.getFieldValueByAlias(ALIAS_NAME_UPN, getDataDictionary());
            if (!StringUtils.isEmpty(uniquePupilNumber)) {
                dfEPupil.setUniquePupilNumber(uniquePupilNumber);
            }
            String uniqueLearningNumber = (String) student.getFieldValueByAlias(ALIAS_NAME_ULN, getDataDictionary());
            if (!StringUtils.isEmpty(uniqueLearningNumber)) {
                dfEPupil.setUniqueLearnerNumber(uniqueLearningNumber);
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
            String formerSurname =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_FORMER_SURNAME, getDataDictionary());
            if (!StringUtils.isEmpty(formerSurname)) {
                dfEPupil.setFormerSurname(formerSurname);
            }
            String preferredForename =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_PREFERRED_FORENAME, getDataDictionary());
            if (!StringUtils.isEmpty(preferredForename)) {
                dfEPupil.setPreferredForename(preferredForename);
            }
            if (!StringUtils.isEmpty(studentPerson.getMiddleName())) {
                dfEPupil.setMiddleNames(studentPerson.getMiddleName());
            }
            dfEPupil.setNCYearActual(student.getGradeLevel());

            // Not translating DfE Ethnicity Code
            String ethnicity = (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_ETHNICITY, getDataDictionary());
            if (!StringUtils.isEmpty(ethnicity)) {
                dfEPupil.setEthnicity(ethnicity);
                dfEPupil.setEthnicitySource(DfEPupil.DEFAULT_ETHNICITY_SOURCE);
            }

            // Set Language
            // Translate Aspen Language Code to DfE Language Code.
            String languageCode = lookupStateValueByRefCode(Student.class, Student.COL_HOME_LANGUAGE_CODE,
                    student.getHomeLanguageCode());
            DfELanguage dfELanguage = new DfELanguage(DfELanguage.DEFAULT_LANGUAGE_TYPE, languageCode);
            dfEPupil.addDfELanguage(dfELanguage);

            // Setting Defaulting Medical Flag to false
            dfEPupil.setMedicalFlag(Boolean.valueOf(false));

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

            // SEN
            Collection<StudentProgramParticipation> studentPrograms = student.getProgramParticipation(m_broker);
            if (studentPrograms.size() > 0) {
                PlainDate sENStartDate = null;
                String sENProvision = null;
                for (StudentProgramParticipation studentProgramParticipation : studentPrograms) {
                    String programCode = studentProgramParticipation.getProgramCode();
                    if (ASPEN_PROGRAM_CODE_SE.equals(programCode)) {
                        // Not translating DfE SEN Provision code, save as is.
                        String type = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_SEN_TYPE,
                                getDataDictionary());
                        String rank = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_SEN_RANK,
                                getDataDictionary());
                        // Not translating DfE SEN Type code, save as is.
                        sENProvision = (String) studentProgramParticipation
                                .getFieldValueByAlias(ALIAS_NAME_SEN_PROVISION, getDataDictionary());
                        DfESENNeed sENNeed = new DfESENNeed(type, rank);
                        dfEPupil.addSENNeed(sENNeed);

                        sENStartDate = studentProgramParticipation.getStartDate();
                    }

                    // Using CTF Ver 11.
                    if (ASPEN_PROGRAM_CODE_FRL.equals(programCode)) {
                        dfEPupil.setFSMReviewDate(studentProgramParticipation.getStartDate());
                        dfEPupil.setFSMStartDate(studentProgramParticipation.getStartDate());
                        dfEPupil.setFSMEndDate(studentProgramParticipation.getEndDate());
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
                    dfEAddress.setSAON((String) null);
                    dfEAddress.setStreet(studentAddress.getStreetName());
                    if (!StringUtils.isEmpty(studentAddress.getCity())) {
                        dfEAddress.setTown(studentAddress.getCity());
                    }
                    dfEAddress.setAdministrativeArea((String) null);
                    dfEAddress.setPostTown((String) null);
                    dfEAddress.setUniquePropertyReferenceNumber((String) null);
                    if (!StringUtils.isEmpty(studentAddress.getAddressLine02())) {
                        dfEAddress.setLocality(studentAddress.getAddressLine02());
                    }
                } else {
                    if (!StringUtils.isEmpty(studentAddress.getAddressLine01())) {
                        dfEAddress.setAddressLine1(studentAddress.getAddressLine01());
                    }
                    if (!StringUtils.isEmpty(studentAddress.getAddressLine02())) {
                        dfEAddress.setAddressLine1(studentAddress.getAddressLine02());
                    }
                    if (!StringUtils.isEmpty(studentAddress.getAddressLine03())) {
                        dfEAddress.setAddressLine1(studentAddress.getAddressLine03());
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

                dfEPupil.setDfEAddress(dfEAddress);
            }

            // Add Telephones and Email
            if (!StringUtils.isEmpty(studentPerson.getPhone01())) {
                DfETelephone dfETelephone =
                        new DfETelephone(DfETelephone.DEFAULT_TELEPHONE_TYPE, studentPerson.getPhone01());
                dfEPupil.addTelephone(dfETelephone);
            }
            if (!StringUtils.isEmpty(studentPerson.getPhone02())) {
                DfETelephone dfETelephone =
                        new DfETelephone(DfETelephone.DEFAULT_TELEPHONE_TYPE, studentPerson.getPhone02());
                dfEPupil.addTelephone(dfETelephone);
            }
            if (!StringUtils.isEmpty(studentPerson.getPhone03())) {
                DfETelephone dfETelephone =
                        new DfETelephone(DfETelephone.DEFAULT_TELEPHONE_TYPE, studentPerson.getPhone03());
                dfEPupil.addTelephone(dfETelephone);
            }
            if (!StringUtils.isEmpty(studentPerson.getEmail01())) {
                dfEPupil.setEmail(studentPerson.getEmail01());
            }

            String attendanceXML = null;
            String studentAttendance = student.getFieldD002();
            attendanceXML = studentAttendance;
            if (!StringUtils.isEmpty(attendanceXML)) {
                dfEPupil.setAttendanceXML(attendanceXML);
            }
            String assessmentsXML = null;
            String studentAssessments = student.getFieldD003();
            assessmentsXML = studentAssessments;
            if (!StringUtils.isEmpty(assessmentsXML)) {
                dfEPupil.setAssessmentsXML(assessmentsXML);
            }
            String schoolHistoryXML = null;
            String studentSchoolHistory = student.getFieldD004();
            schoolHistoryXML = studentSchoolHistory;
            if (!StringUtils.isEmpty(schoolHistoryXML)) {
                dfEPupil.setSchoolHistoryXML(schoolHistoryXML);
            }

            Collection<StudentContact> studentContacts = student.getContacts();
            int i = 0;
            for (StudentContact studentContact : studentContacts) {
                i++;
                Contact contact = studentContact.getContact();
                Person contactPerson = contact.getPerson();

                DfEContact dfEContact = new DfEContact();
                dfEContact.setOrder(i);
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
                if (i == 1) {
                    dfEContact.setResponsible(Boolean.valueOf(true));
                } else {
                    dfEContact.setResponsible(Boolean.valueOf(false));
                }
                if (!StringUtils.isEmpty(studentContact.getRelationshipCode())) {
                    // Translate Aspen Gender Code to DfE Gender Code.
                    String relationshipCode = lookupStateValueByRefCode(StudentContact.class,
                            StudentContact.COL_RELATIONSHIP_CODE, studentContact.getRelationshipCode());
                    dfEContact.setRelationshipCode(relationshipCode);
                }

                // Add Telephones and Email
                if (!StringUtils.isEmpty(contactPerson.getPhone01())) {
                    DfETelephone dfETelephone =
                            new DfETelephone(DfETelephone.DEFAULT_TELEPHONE_TYPE, contactPerson.getPhone01());
                    dfEContact.addTelephone(dfETelephone);
                }
                if (!StringUtils.isEmpty(contactPerson.getPhone02())) {
                    DfETelephone dfETelephone =
                            new DfETelephone(DfETelephone.DEFAULT_TELEPHONE_TYPE, contactPerson.getPhone02());
                    dfEContact.addTelephone(dfETelephone);
                }
                if (!StringUtils.isEmpty(contactPerson.getPhone03())) {
                    DfETelephone dfETelephone =
                            new DfETelephone(DfETelephone.DEFAULT_TELEPHONE_TYPE, contactPerson.getPhone03());
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
                        dfEContact.setAddressAsPupil(Boolean.valueOf(true));
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
                            dfEAddress.setSAON((String) null);
                            dfEAddress.setStreet(contactAddress.getStreetName());
                            if (!StringUtils.isEmpty(contactAddress.getCity())) {
                                dfEAddress.setTown(contactAddress.getCity());
                            }
                            dfEAddress.setAdministrativeArea((String) null);
                            dfEAddress.setPostTown((String) null);
                            dfEAddress.setUniquePropertyReferenceNumber((String) null);
                            if (!StringUtils.isEmpty(contactAddress.getAddressLine02())) {
                                dfEAddress.setLocality(contactAddress.getAddressLine02());
                            }
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
                        dfEAddress.setZip((String) null);
                        dfEAddress.setEasting((String) null);
                        dfEAddress.setNorthing((String) null);

                        dfEContact.setDfEAddress(dfEAddress);
                    }
                }

                dfEPupil.addContact(dfEContact);
            }

            return dfEPupil;
        }

        /**
         * Get a DfE Pupil Data from DfEPupil
         *
         * Note: Not including SuppInfo Elements.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getCTFPupilDataElement(DfEPupil dfEPupil) {
            Element pupilElement = new Element(DfEManager.ELEMENT_PUPIL);

            // Pupil Primary Info Start
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
            if (!StringUtils.isEmpty(dfEPupil.getUniqueCandidateIdentifier())) {
                Element uniqueCandidateIdentifierElement = new Element(DfEPupil.ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER);
                uniqueCandidateIdentifierElement.addContent(dfEPupil.getUniqueCandidateIdentifier());
                pupilElement.addContent(uniqueCandidateIdentifierElement);
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
            // Pupil Primary Info End

            // Pupil Basic Details
            Element basicDetailsElement = getBasicDetailsElement(dfEPupil);
            if (basicDetailsElement != null) {
                pupilElement.addContent(basicDetailsElement);

                // SEN History
                if (!StringUtils.isEmpty(dfEPupil.getSENProvision())) {
                    Element sENHistoryElement = getSENHistoryElement(dfEPupil);

                    pupilElement.addContent(sENHistoryElement);
                }

                // Pupil Address
                if (dfEPupil.getDfEAddress() != null) {
                    Element addressElement = getAddressElement(dfEPupil.getDfEAddress());

                    pupilElement.addContent(addressElement);
                }

                // Pupil Telephone
                if (dfEPupil.getTelephones() != null && dfEPupil.getTelephones().size() > 0) {
                    Element phonesElement = getPhonesElement(dfEPupil.getTelephones());

                    pupilElement.addContent(phonesElement);
                }

                if (!StringUtils.isEmpty(dfEPupil.getEmail())) {
                    Element emailElement = new Element(DfEPupil.ELEMENT_EMAIL);
                    emailElement.addContent(dfEPupil.getEmail());
                    pupilElement.addContent(emailElement);
                }

                // Contacts Start
                if (dfEPupil.getContacts() != null && dfEPupil.getContacts().size() > 0) {
                    Element contactsElement = getContacts(dfEPupil.getContacts());
                    pupilElement.addContent(contactsElement);
                }

                // Pupil Attendance
                if (!StringUtils.isEmpty(dfEPupil.getAttendanceXML())) {
                    Element attendanceElement = getElementFromXMLString(dfEPupil.getAttendanceXML());
                    if (attendanceElement != null) {
                        pupilElement.addContent(attendanceElement);
                    }
                }

                // Pupil Assessments
                if (!StringUtils.isEmpty(dfEPupil.getAssessmentsXML())) {
                    Element assessmentsElement = getElementFromXMLString(dfEPupil.getAssessmentsXML());
                    if (assessmentsElement != null) {
                        pupilElement.addContent(assessmentsElement);
                    }
                }

                // Pupil School History
                if (!StringUtils.isEmpty(dfEPupil.getSchoolHistoryXML())) {
                    Element schoolHistoryElement = getElementFromXMLString(dfEPupil.getSchoolHistoryXML());
                    if (schoolHistoryElement != null) {
                        pupilElement.addContent(schoolHistoryElement);
                    }
                }
            }

            return pupilElement;
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
            if (dfEPupil.getFormerSurname() != null) {
                Element formerSurnameElement = new Element(DfEPupil.ELEMENT_FORMER_SURNAME);
                formerSurnameElement.addContent(dfEPupil.getFormerSurname());
                basicDetailsElement.addContent(formerSurnameElement);
            }
            if (dfEPupil.getPreferredForename() != null) {
                Element preferredForenameElement = new Element(DfEPupil.ELEMENT_PREFERRED_FORENAME);
                preferredForenameElement.addContent(dfEPupil.getPreferredForename());
                basicDetailsElement.addContent(preferredForenameElement);
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
                    medicalFlagElement.addContent(TRUE);
                } else {
                    medicalFlagElement.addContent(FALSE);
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

            // Using CTF Ver 11.
            if (dfEPupil.getFSMReviewDate() != null) {
                Element fSMReviewDateElement = new Element(DfEPupil.ELEMENT_FSM_REVIEW_DATE);
                fSMReviewDateElement.addContent(m_dateFormat.format(dfEPupil.getFSMReviewDate()));
                basicDetailsElement.addContent(fSMReviewDateElement);
            }
            if (dfEPupil.getFSMStartDate() != null) {
                Element fSMStartDateElement = new Element(DfEPupil.ELEMENT_FSM_START_DATE);
                fSMStartDateElement.addContent(m_dateFormat.format(dfEPupil.getFSMStartDate()));
                basicDetailsElement.addContent(fSMStartDateElement);
            }
            if (dfEPupil.getFSMEndDate() != null) {
                Element fSMEndDateElement = new Element(DfEPupil.ELEMENT_FSM_END_DATE);
                fSMEndDateElement.addContent(m_dateFormat.format(dfEPupil.getFSMEndDate()));
                basicDetailsElement.addContent(fSMEndDateElement);
            }

            // Wales
            if (dfEPupil.getSpeakWelsh() != null) {
                Element nAWDetailsElement = getNAWDetailsElement(dfEPupil);
                basicDetailsElement.addContent(nAWDetailsElement);
            }

            // Looked After
            if (dfEPupil.getInCare() != null) {
                Element lookedAfterElement = getLookAfterElement(dfEPupil);
                basicDetailsElement.addContent(lookedAfterElement);
            }

            return basicDetailsElement;
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

                Element telephoneTypeElement = new Element(DfETelephone.ELEMENT_TELEPHONE_TYPE);
                telephoneTypeElement.addContent(dfEtelephone.getTelephoneType());
                phoneElement.addContent(telephoneTypeElement);

                Element telephoneNumberElement = new Element(DfETelephone.ELEMENT_PHONE_NUMBER);
                telephoneNumberElement.addContent(dfEtelephone.getTelephoneNumber());
                phoneElement.addContent(telephoneNumberElement);

                phonesElement.addContent(phoneElement);
            }

            return phonesElement;
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

                if (dfEAddress.getPAON() != null) {
                    Element pAONElement = new Element(DfEAddress.ELEMENT_PAON);
                    pAONElement.addContent(dfEAddress.getPAON());
                    bS7666AddressElement.addContent(pAONElement);
                }
                if (dfEAddress.getSAON() != null) {
                    Element sAONElement = new Element(DfEAddress.ELEMENT_SAON);
                    sAONElement.addContent(dfEAddress.getSAON());
                    bS7666AddressElement.addContent(sAONElement);
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
         * Get a Languages Element from Collection<DfELanguage>.
         *
         * @param languagesList Collection<DfELanguage>
         * @return Element
         */
        public Element getLanguagesElement(Collection<DfELanguage> languagesList) {
            Element languagesElement = new Element(DfELanguage.ELEMENT_LANGUAGES);

            for (DfELanguage dfELanguage : languagesList) {
                Element typeElement = new Element(DfELanguage.ELEMENT_TYPE);

                Element languageTypeElement = new Element(DfELanguage.ELEMENT_LANGUAGE_TYPE);
                languageTypeElement.addContent(dfELanguage.getLanguageType());
                typeElement.addContent(languageTypeElement);

                Element languageCodeElement = new Element(DfELanguage.ELEMENT_LANGUAGE_CODE);
                languageCodeElement.addContent(dfELanguage.getLanguageCode());
                typeElement.addContent(languageCodeElement);

                languagesElement.addContent(typeElement);
            }

            return languagesElement;
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
         * Get a NAWDetails Element from DfEPupil.
         *
         * @param dfEPupil DfEPupil
         * @return Element
         */
        public Element getNAWDetailsElement(DfEPupil dfEPupil) {
            Element nAWDetailsElement = new Element(DfEPupil.ELEMENT_NAW_DETAILS);
            if (dfEPupil.getSpeakWelsh() != null) {
                Element speakWelshElement = new Element(DfEPupil.ELEMENT_SPEAK_WELSH);
                speakWelshElement.addContent(dfEPupil.getSpeakWelsh());
                nAWDetailsElement.addContent(speakWelshElement);
            }
            if (dfEPupil.getHomeWelsh() != null) {
                Element homeWelshElement = new Element(DfEPupil.ELEMENT_HOME_WELSH);
                homeWelshElement.addContent(dfEPupil.getHomeWelsh());
                nAWDetailsElement.addContent(homeWelshElement);
            }
            if (dfEPupil.getNationalIdentity() != null) {
                Element nationalIdentityElement = new Element(DfEPupil.ELEMENT_NATIONAL_IDENTITY);
                nationalIdentityElement.addContent(dfEPupil.getNationalIdentity());
                nAWDetailsElement.addContent(nationalIdentityElement);
            }
            if (dfEPupil.getWelshSource() != null) {
                Element welshSourceElement = new Element(DfEPupil.ELEMENT_WELSH_SOURCE);
                welshSourceElement.addContent(dfEPupil.getWelshSource());
                nAWDetailsElement.addContent(welshSourceElement);
            }
            if (dfEPupil.getEALAcquisition() != null) {
                Element eALAcquisitionElement = new Element(DfEPupil.ELEMENT_EAL_ACQUISITION);
                eALAcquisitionElement.addContent(dfEPupil.getEALAcquisition());
                nAWDetailsElement.addContent(eALAcquisitionElement);
            }

            return nAWDetailsElement;
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
                inCareElement.addContent(dfEPupil.getInCare());
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
         * Get a Contacts Element from Collection<DfEContact> .
         *
         * @param contactList Collection<DfEContact>
         * @return Element
         */
        public Element getContacts(Collection<DfEContact> contactList) {
            Element contactsElement = new Element(DfEPupil.ELEMENT_CONTACTS);

            Collection<DfEContact> contacts = contactList;
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
                        responsibilityElement.addContent(TRUE);
                    } else {
                        responsibilityElement.addContent(FALSE);
                    }
                    contactElement.addContent(responsibilityElement);
                }

                // Contact Address
                if (dfEContact.getAddressAsPupil() != null && dfEContact.getAddressAsPupil().booleanValue() == true) {
                    Element addressElement = new Element(DfEContact.ELEMENT_ADDRESS);

                    Element addressAsPupilElement = new Element(DfEContact.ELEMENT_ADDRESS_AS_PUPIL);
                    addressAsPupilElement.addContent(TRUE);
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

                contactsElement.addContent(contactElement);
            }

            return contactsElement;
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
         * Returns the state lookup code for field value.
         * Look up based on bean path.
         *
         * @param beanClass the class of the bean to find the reference table.
         * @param beanPath String
         * @param value String
         * @param referenceMap int
         * @return String state code for input value.
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
         * @param referenceTableOid String
         * @param value String
         * @param referenceMap int
         * @return String reference code lookup value for input value.
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
         * @param beanClass the class of the bean to find the reference table.
         * @param beanPath String
         * @param lookupLocalCode String
         * @return String reference code lookup value for local code.
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
         * Returns the state lookup code for field value.
         * Look up based on bean path.
         *
         * @param beanClass the class of the bean to find the reference table.
         * @param beanPath String
         * @param refCode String
         * @return String state code for input refCode.
         */
        public String lookupStateValueByRefCode(Class beanClass, String beanPath, String refCode) {
            String stateValue = lookupReferenceCodeByBeanPath(beanClass, beanPath, refCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return stateValue;
        }

        /**
         * Returns the lookup code value for State Code.
         * Look up based on the reference table.
         *
         * @param beanClass the class of the bean to find the reference table.
         * @param beanPath String
         * @param lookupStateCode String
         * @return String reference code lookup value for state code.
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

    }


}
