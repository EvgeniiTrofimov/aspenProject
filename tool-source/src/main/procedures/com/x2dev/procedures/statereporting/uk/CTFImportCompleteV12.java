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
import com.follett.fsc.core.k12.business.IdManager;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentEnrollment;
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
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;

/**
 * This class imports exam results for the UK education system.
 *
 * @author Follett Software Company
 */

public class CTFImportCompleteV12 extends ToolJavaSource {
    private List<String> m_messages = new LinkedList();
    private DfEImportManager m_dfEImportManager = null;
    private Document XMLDocument = null;

    private String m_lEA = null;
    private String m_schoolOid = null;
    private Boolean m_updateExisting = null;
    private HashMap<String, Boolean> m_activeSections = new HashMap<String, Boolean>();

    public static final String QUERY_BY_PARAM = "queryBy";
    public static final String QUERY_STRING_PARAM = "queryString";

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
     * Gets the input stream of the import file and passes it to the method that imports the data.
     *
     * @param sourceFile File
     */
    protected void importData(File sourceFile) {
        m_dfEImportManager = new DfEImportManager(getBroker(), getLocale());

        logMessage("CTF Import Starting...");

        String filename = sourceFile.getName();
        if (!StringUtils.isEmpty(filename)) {
            if (!filename.toLowerCase().endsWith(".xml")) {
                logMessage("Error: The file selected is not an XML file: " + filename);
            } else {
                logMessage("The file: " + filename + " was selected.");

                try {
                    SAXBuilder builder = new SAXBuilder();
                    byte[] fileBytes = m_dfEImportManager.getBytesFromFile(sourceFile);
                    XMLDocument = builder.build(new ByteArrayInputStream(fileBytes));

                    if (XMLDocument == null) {
                        logMessage("JDOM Error: Unable to parse CTF XML file: " + filename);
                    } else {
                        loadXMLDocument();
                    }
                } catch (JDOMException e) {
                    logMessage("JDOMException: Unable to create Document from the selected file: " + filename);
                } catch (IOException e) {
                    logMessage("IOException: Error getting the input stream from the selected file: " + filename);
                }
            }
        } else {
            logMessage("Error: A File was not selected.");
        }
    }

    /**
     * Load parameters.
     */
    protected void loadParameters() {
        m_updateExisting = (Boolean) getParameter(DfEManager.PARAM_UPDATE_EXISTING);

        Boolean code = (Boolean) getParameter(DfEManager.PARAM_CORE);
        m_activeSections.put(DfEManager.PARAM_CORE, code);
        Boolean basicDetails = (Boolean) getParameter(DfEManager.PARAM_BASIC_DETAILS);
        m_activeSections.put(DfEManager.PARAM_BASIC_DETAILS, basicDetails);
        Boolean sENHistory = (Boolean) getParameter(DfEManager.PARAM_SEN_HISTORY);
        m_activeSections.put(DfEManager.PARAM_SEN_HISTORY, sENHistory);
        Boolean fSMHistory = (Boolean) getParameter(DfEManager.PARAM_FSM_HISTORY);
        m_activeSections.put(DfEManager.PARAM_FSM_HISTORY, fSMHistory);
        Boolean nAWDetails = (Boolean) getParameter(DfEManager.PARAM_NAW_DETAILS);
        m_activeSections.put(DfEManager.PARAM_NAW_DETAILS, nAWDetails);
        Boolean lookedAfter = (Boolean) getParameter(DfEManager.PARAM_LOOKED_AFTER);
        m_activeSections.put(DfEManager.PARAM_LOOKED_AFTER, lookedAfter);
        Boolean address = (Boolean) getParameter(DfEManager.PARAM_ADDRESS_PHONE_EMAIL);
        m_activeSections.put(DfEManager.PARAM_ADDRESS_PHONE_EMAIL, address);
        Boolean contacts = (Boolean) getParameter(DfEManager.PARAM_CONTACTS);
        m_activeSections.put(DfEManager.PARAM_CONTACTS, contacts);
        Boolean assessments = (Boolean) getParameter(DfEManager.PARAM_ASSESSMENTS);
        m_activeSections.put(DfEManager.PARAM_ASSESSMENTS, assessments);
        Boolean attendance = (Boolean) getParameter(DfEManager.PARAM_ATTENDANCE);
        m_activeSections.put(DfEManager.PARAM_ATTENDANCE, attendance);
        Boolean attendanceSessions = (Boolean) getParameter(DfEManager.PARAM_ATTENDANCE_SESSIONS);
        m_activeSections.put(DfEManager.PARAM_ATTENDANCE_SESSIONS, attendanceSessions);
        Boolean schoolHistory = (Boolean) getParameter(DfEManager.PARAM_SCHOOL_HISTORY);
        m_activeSections.put(DfEManager.PARAM_SCHOOL_HISTORY, schoolHistory);
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

        Organization organization = getOrganization();
        if (organization != null) {
            m_lEA = (String) organization.getFieldValueByAlias(DfEManager.ALIAS_NAME_LEA);
        }

        File importFile = (File) getParameter(FILE_KEY);

        importData(importFile);

        displayResults();
    }

    /**
     * Print out the logs to the user.
     *
     * @throws X2BaseException exception
     */
    private void displayResults() throws X2BaseException {
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
     * Process DfE CTF XML Document.
     */
    private void loadXMLDocument() {
        Element xmlDefinition = XMLDocument.getRootElement();

        if (xmlDefinition != null && DfEHeader.ELEMENT_CTFILE.equals(xmlDefinition.getName())) {
            Element headerElement = xmlDefinition.getChild(DfEHeader.ELEMENT_HEADER);

            // Create DfEHeader Object from headerElement
            DfEHeader dfEHeader = new DfEHeader(headerElement);

            logMessage("Pupils transferring from Source LEA: " + dfEHeader.getSourceSchoolLEA()
                    + " to Destination LEA: " + dfEHeader.getDestSchoolLEA() + ".");

            if (validateHeader(dfEHeader)) {
                Element cTFPupilDataElement = xmlDefinition.getChild(DfEPupil.ELEMENT_CTF_PUPIL_DATA);

                // Get Pupils
                List<Element> pupilsElementList = cTFPupilDataElement.getChildren(DfEPupil.ELEMENT_PUPIL);
                int pupilsSaved = 0;
                for (int i = 0; i < pupilsElementList.size(); i++) {
                    Element pupilElement = pupilsElementList.get(i);

                    // Create DfcPupil Object from pupilElement
                    DfEPupil dfEPupil = new DfEPupil(pupilElement);

                    if (m_updateExisting != null && m_updateExisting.booleanValue() == true) {
                        Student student = m_dfEImportManager.getStudentByUPN(dfEPupil);
                        if (student != null) {
                            logMessage("Updating the Pupil " + dfEPupil.getForename() + " " + dfEPupil.getSurname()
                                    + " with UPN: " + dfEPupil.getUniquePupilNumber() + ".");

                            // Update Pupil with DfcPupil Object
                            m_dfEImportManager.updateDfEPupil(dfEPupil, student, m_schoolOid, m_activeSections);

                            pupilsSaved++;
                        } else {
                            logMessage("Adding the Pupil " + dfEPupil.getForename() + " " + dfEPupil.getSurname()
                                    + " to the system with " + dfEPupil.getContacts().size() + " Contacts.");

                            // Save Pupil from DfcPupil Object
                            m_dfEImportManager.saveDfEPupil(dfEPupil, m_schoolOid, m_activeSections);

                            pupilsSaved++;
                        }
                    } else {
                        logMessage("Adding the Pupil " + dfEPupil.getForename() + " " + dfEPupil.getSurname()
                                + " to the system with " + dfEPupil.getContacts().size() + " Contacts.");

                        // Save Pupil from DfcPupil Object
                        m_dfEImportManager.saveDfEPupil(dfEPupil, m_schoolOid, m_activeSections);

                        pupilsSaved++;
                    }
                }

                logMessage("Imported a total of " + pupilsSaved + " pupil(s).");
            }

        } else {
            logMessage("The File is not properly formatted or is not a CTF file.");
        }

        logMessage("CTF Import Completed.");
    }

    /**
     * Validate Header from CTF XML Document.
     *
     * @param dfEHeader DfEHeader
     * @return true, if successful
     * @returns boolean
     */
    private boolean validateHeader(DfEHeader dfEHeader) {
        Organization organization = m_dfEImportManager.getOrganizationByLea(dfEHeader.getDestSchoolLEA());
        if (organization == null) {
            logMessage("Error: The Destination LEA code provided by the CTF file: " + dfEHeader.getDestSchoolLEA()
                    + " is different for this System's LEA: " + m_lEA + ". Aborting Import.");

            // exit procedure
            return false;
        }

        School school = m_dfEImportManager.getSchoolByEstab(dfEHeader.getDestSchoolEstab());
        if (school == null) {
            logMessage("Error: The Destination Estab code provided by the CTF file: " + dfEHeader.getDestSchoolEstab()
                    + " is not within this LEA. Aborting Import.");

            // exit procedure
            return false;
        }
        m_schoolOid = school.getOid();

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
                } catch (ParseException e) {
                    this.resultDate = null;
                }
                this.resultDate = new PlainDate(date);
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
         * Date and Time formats.
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
     * The Class DfEImportManager.
     */
    class DfEImportManager extends DfEManager {
        private static final String DEFAULT_CONTACT_TYPE = "Student";
        private static final String DFE_PROGRAM_CODE_FRL = "FRL";
        private static final String DFE_PROGRAM_CODE_SE = "SE";
        private static final String DFE_PROGRAM_CODE_NAW = "NAW";

        /**
         * Constructor for DfEManager.
         *
         * @param broker X2Broker
         * @param locale Locale
         */
        public DfEImportManager(X2Broker broker, Locale locale) {
            super(broker, locale);
        }

        /**
         * Remove all Contacts of a student and the Contact objects.
         *
         * @param studentOid String
         * @param studentAddressOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void deleteContacts(String studentOid,
                                   String studentAddressOid,
                                   HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_CONTACTS)) {
                // Get StudentContact
                X2Criteria studentContactCriteria = new X2Criteria();
                studentContactCriteria.addEqualTo(StudentContact.COL_STUDENT_OID, studentOid);
                QueryByCriteria studentContactQuery = new QueryByCriteria(StudentContact.class, studentContactCriteria);
                Collection<StudentContact> studentContacts = m_broker.getCollectionByQuery(studentContactQuery);

                // If the student doesn't have any contacts then there is no need to continue.
                if (studentContacts.size() > 0) {
                    X2Criteria contactCriteria = new X2Criteria();
                    contactCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_STUDENT_OID,
                            studentOid);
                    QueryByCriteria contactQuery = new QueryByCriteria(Contact.class, contactCriteria);
                    Collection<Contact> contacts = m_broker.getCollectionByQuery(contactQuery);

                    SubQuery contactPersonSubQuery =
                            new SubQuery(Contact.class, Contact.COL_PERSON_OID, contactCriteria);

                    X2Criteria personCriteria = new X2Criteria();
                    personCriteria.addEqualTo(Person.COL_CONTACT_INDICATOR, Boolean.valueOf(true));
                    personCriteria.addIn(X2BaseBean.COL_OID, contactPersonSubQuery);
                    QueryByCriteria personQuery = new QueryByCriteria(Person.class, personCriteria);
                    Collection<Person> people = m_broker.getCollectionByQuery(personQuery);

                    X2Criteria addressCriteria = new X2Criteria();
                    addressCriteria.addIn(Address.REL_PEOPLE_PHYSICAL + "." + X2BaseBean.COL_OID,
                            contactPersonSubQuery);
                    addressCriteria.addNotEqualTo(X2BaseBean.COL_OID, studentAddressOid);
                    QueryByCriteria addressQuery = new QueryByCriteria(Address.class, addressCriteria);
                    Collection<Address> addresses = m_broker.getCollectionByQuery(addressQuery);

                    // Create a list of selected Oids.
                    ArrayList studentContactOids = new ArrayList();
                    ArrayList contactOids = new ArrayList();
                    ArrayList personOids = new ArrayList();
                    ArrayList addressOids = new ArrayList();
                    for (StudentContact studentContact : studentContacts) {
                        studentContactOids.add(studentContact.getOid());
                    }
                    for (Contact contact : contacts) {
                        contactOids.add(contact.getOid());
                    }
                    for (Person person : people) {
                        personOids.add(person.getOid());
                    }
                    for (Address address : addresses) {
                        addressOids.add(address.getOid());
                    }

                    // Delete all student Contacts and Contact related objects
                    if (addressOids.size() > 0) {
                        X2Criteria addressCriteria2 = new X2Criteria();
                        addressCriteria2.addIn(X2BaseBean.COL_OID, addressOids);
                        QueryByCriteria addressQuery2 = new QueryByCriteria(Address.class, addressCriteria2);
                        m_broker.deleteByQuery(addressQuery2);
                    }
                    if (personOids.size() > 0) {
                        X2Criteria personCriteria2 = new X2Criteria();
                        personCriteria2.addIn(X2BaseBean.COL_OID, personOids);
                        QueryByCriteria personQuery2 = new QueryByCriteria(Person.class, personCriteria2);
                        m_broker.deleteByQuery(personQuery2);
                    }
                    if (contactOids.size() > 0) {
                        X2Criteria contactCriteria2 = new X2Criteria();
                        contactCriteria2.addIn(X2BaseBean.COL_OID, contactOids);
                        QueryByCriteria contactQuery2 = new QueryByCriteria(Contact.class, contactCriteria2);
                        m_broker.deleteByQuery(contactQuery2);
                    }
                    if (studentContactOids.size() > 0) {
                        X2Criteria studentContactCriteria2 = new X2Criteria();
                        studentContactCriteria2.addIn(X2BaseBean.COL_OID, studentContactOids);
                        QueryByCriteria studentContactQuery2 =
                                new QueryByCriteria(StudentContact.class, studentContactCriteria2);
                        m_broker.deleteByQuery(studentContactQuery2);
                    }

                }

            }
        }

        /**
         * Delete a DfE Pupil's Assessments (Student Assessments) records.
         *
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void deleteStudentAssessments(String studentOid, HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
                if (studentOid != null) {
                    X2Criteria studentAssessmentCriteria = new X2Criteria();
                    studentAssessmentCriteria.addEqualTo(StudentAssessment.COL_STUDENT_OID, studentOid);

                    QueryByCriteria studentAssessmentQuery =
                            new QueryByCriteria(StudentAssessment.class, studentAssessmentCriteria);

                    m_broker.deleteByQuery(studentAssessmentQuery);
                }
            }
        }

        /**
         * Delete a DfE Pupil's Attendance (Student School) records.
         *
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void deleteStudentAttendanceHistories(String studentOid, HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
                if (studentOid != null) {
                    X2Criteria studentSchoolCriteria = new X2Criteria();
                    studentSchoolCriteria.addEqualTo(StudentSchool.COL_STUDENT_OID, studentOid);
                    studentSchoolCriteria.addEqualTo(StudentSchool.COL_SCHOOL_OID, m_cTFSchool.getOid());

                    QueryByCriteria studentSchoolQuery =
                            new QueryByCriteria(StudentSchool.class, studentSchoolCriteria);

                    m_broker.deleteByQuery(studentSchoolQuery);
                }
            }
        }

        /**
         * Delete all existing Disability for a student.
         *
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void deleteStudentDisabilities(String studentOid, HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
                if (studentOid != null) {
                    X2Criteria disabilityCriteria = new X2Criteria();
                    disabilityCriteria.addEqualTo(IepDisability.COL_STUDENT_OID, studentOid);

                    QueryByCriteria disabilityQuery = new QueryByCriteria(IepDisability.class, disabilityCriteria);

                    m_broker.deleteByQuery(disabilityQuery);
                }
            }
        }

        /**
         * Delete a DfE Pupil's School History (Student Enrollment) records.
         *
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void deleteStudentSchoolHistories(String studentOid, HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
                if (studentOid != null) {
                    X2Criteria studentEnrollmentCriteria = new X2Criteria();
                    studentEnrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, studentOid);

                    QueryByCriteria studentEnrollmentQuery =
                            new QueryByCriteria(StudentEnrollment.class, studentEnrollmentCriteria);

                    m_broker.deleteByQuery(studentEnrollmentQuery);
                }
            }
        }

        /**
         * Delete all existing StudentProgramParticipation for a student.
         *
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void deleteStudentProgramParticipations(String studentOid, HashMap<String, Boolean> activeSections) {
            if (studentOid != null) {
                ArrayList programCodes = new ArrayList();

                if (isSectionActive(activeSections, PARAM_FSM_HISTORY)) {
                    String fRLProgramCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_FRL);
                    programCodes.add(fRLProgramCode);
                }
                if (isSectionActive(activeSections, PARAM_SEN_HISTORY)) {
                    String sEProgramCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_SE);
                    programCodes.add(sEProgramCode);
                }
                if (isSectionActive(activeSections, PARAM_NAW_DETAILS)) {
                    String nAWProgramCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_NAW);
                    programCodes.add(nAWProgramCode);
                }

                if (programCodes.size() > 0) {
                    X2Criteria studentProgramParticipationCriteria = new X2Criteria();
                    studentProgramParticipationCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID,
                            studentOid);
                    studentProgramParticipationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE,
                            programCodes);

                    QueryByCriteria studentProgramParticipationQuery =
                            new QueryByCriteria(StudentProgramParticipation.class, studentProgramParticipationCriteria);

                    m_broker.deleteByQuery(studentProgramParticipationQuery);
                }

            }
        }

        /**
         * Generate a new Student Id
         *
         * If the District level Preference, Student category is not selected then a null is
         * returned.
         *
         * @param schoolOid String
         * @return String
         */
        public String generateStudentId(String schoolOid) {
            School school = (School) m_broker.getBeanByOid(School.class, schoolOid);
            String localId = null;

            try {
                localId = IdManager.generateId(m_broker,
                        school.getOrganization1().getRootOrganization(),
                        SystemPreferenceDefinition.ID_AUTO_ASSIGN,
                        SystemPreferenceDefinition.ID_NEXT_NUMBER,
                        SystemPreferenceDefinition.ID_LENGTH,
                        SystemPreferenceDefinition.ID_PREFIX,
                        SystemPreferenceDefinition.ID_INCREMENT,
                        false);
            } catch (InvalidPreferenceException e) {
                e.printStackTrace();
            }

            return localId;
        }

        /**
         * Save a DfE Address to Aspen Address Table.
         *
         * @param dfEAddress DfEAddress
         * @param address Address
         * @param activeSections HashMap<String,Boolean>
         * @return Address
         */
        public Address saveAddress(DfEAddress dfEAddress, Address address, HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_ADDRESS_PHONE_EMAIL)) {
                if (address == null) {
                    address = X2BaseBean.newInstance(Address.class, m_broker.getPersistenceKey());
                }

                address.setOrganization1Oid(DEFAULT_ORGANIZATION);
                if (dfEAddress.hasBS7666Address()) {
                    String streetNumber = dfEAddress.getPAON();
                    if (streetNumber != null) {
                        streetNumber = streetNumber.trim();
                        if (StringUtils.isNumeric(streetNumber)) {
                            int streetNum = Integer.parseInt(streetNumber);
                            address.setStreetNumber(streetNum);
                        } else {
                            if (streetNumber.length() <= STRING_10_MAX_LENGTH) {
                                address.setStreetLetter(streetNumber);
                            } else {
                                address.setStreetLetter(streetNumber.substring(0, STRING_10_MAX_LENGTH));
                            }
                        }
                    }

                    String streetName = dfEAddress.getStreet();
                    if (streetName != null) {
                        streetName = streetName.trim();
                        if (streetName.length() <= STRING_50_MAX_LENGTH) {
                            address.setStreetName(streetName);
                        } else {
                            address.setStreetName(streetName.substring(0, STRING_50_MAX_LENGTH));
                        }
                    }

                    // Used in Student Address view field
                    String streetAddress = dfEAddress.getPAON() + " " + dfEAddress.getStreet();
                    if (streetAddress != null) {
                        streetAddress = streetAddress.trim();
                        if (streetAddress.length() <= STRING_50_MAX_LENGTH) {
                            address.setAddressLine01(streetAddress);
                        } else {
                            address.setAddressLine01(streetAddress.substring(0, STRING_50_MAX_LENGTH));
                        }
                    }

                    String sAON = dfEAddress.getSAON();
                    if (sAON != null) {
                        sAON = sAON.trim();
                        if (sAON.length() <= DB_FIELD_B_MAX_LENGTH) {
                            address.setFieldValueByAlias(ALIAS_NAME_SAON, sAON, getDataDictionary());
                        } else {
                            address.setFieldValueByAlias(ALIAS_NAME_SAON, sAON.substring(0, DB_FIELD_B_MAX_LENGTH),
                                    getDataDictionary());
                        }
                    }

                    String locality = dfEAddress.getLocality();
                    if (locality != null) {
                        locality = locality.trim();
                        if (locality.length() <= DB_FIELD_B_MAX_LENGTH) {
                            address.setFieldValueByAlias(ALIAS_NAME_LOCALITY, locality, getDataDictionary());
                        } else {
                            address.setFieldValueByAlias(ALIAS_NAME_LOCALITY,
                                    locality.substring(0, DB_FIELD_B_MAX_LENGTH), getDataDictionary());
                        }
                    }

                    String administrativeArea = dfEAddress.getAdministrativeArea();
                    if (administrativeArea != null) {
                        administrativeArea = administrativeArea.trim();
                        if (administrativeArea.length() <= DB_FIELD_C_MAX_LENGTH) {
                            address.setFieldValueByAlias(ALIAS_NAME_ADMIN_AREA, administrativeArea,
                                    getDataDictionary());
                        } else {
                            address.setFieldValueByAlias(ALIAS_NAME_ADMIN_AREA,
                                    administrativeArea.substring(0, DB_FIELD_C_MAX_LENGTH), getDataDictionary());
                        }
                    }

                    String postTown = dfEAddress.getPostTown();
                    if (postTown != null) {
                        postTown = postTown.trim();
                        if (postTown.length() <= DB_FIELD_C_MAX_LENGTH) {
                            address.setFieldValueByAlias(ALIAS_NAME_POST_TOWN, postTown, getDataDictionary());
                        } else {
                            address.setFieldValueByAlias(ALIAS_NAME_POST_TOWN,
                                    postTown.substring(0, DB_FIELD_C_MAX_LENGTH), getDataDictionary());
                        }
                    }

                    String uniquePropertyReferenceNumber = dfEAddress.getUniquePropertyReferenceNumber();
                    if (uniquePropertyReferenceNumber != null) {
                        if (uniquePropertyReferenceNumber.length() <= DB_FIELD_B_MAX_LENGTH) {
                            address.setFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM, uniquePropertyReferenceNumber,
                                    getDataDictionary());
                        } else {
                            address.setFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM,
                                    uniquePropertyReferenceNumber.substring(0, DB_FIELD_B_MAX_LENGTH),
                                    getDataDictionary());
                        }
                    }

                    String town = dfEAddress.getTown();
                    if (town != null) {
                        town = town.trim();
                        if (town.length() <= STRING_40_MAX_LENGTH) {
                            address.setCity(town);
                        } else {
                            address.setCity(town.substring(0, STRING_40_MAX_LENGTH));
                        }
                    }

                } else {
                    String addressLine01 = dfEAddress.getAddressLine1();
                    if (addressLine01 != null) {
                        addressLine01 = addressLine01.trim();
                        if (addressLine01.length() <= STRING_50_MAX_LENGTH) {
                            address.setAddressLine01(addressLine01);
                        } else {
                            address.setAddressLine01(addressLine01.substring(0, STRING_50_MAX_LENGTH));
                        }
                    }

                    String addressLine02 = dfEAddress.getAddressLine2();
                    if (addressLine02 != null) {
                        addressLine02 = addressLine02.trim();
                        if (addressLine02.length() <= STRING_50_MAX_LENGTH) {
                            address.setAddressLine02(addressLine02);
                        } else {
                            address.setAddressLine02(addressLine02.substring(0, STRING_50_MAX_LENGTH));
                        }
                    }

                    String addressLine03 = dfEAddress.getAddressLine3();
                    if (addressLine03 != null) {
                        addressLine03 = addressLine03.trim();
                        if (addressLine03.length() <= STRING_50_MAX_LENGTH) {
                            address.setAddressLine03(addressLine03);
                        } else {
                            address.setAddressLine03(addressLine03.substring(0, STRING_50_MAX_LENGTH));
                        }
                    }

                    String addressLine04 = dfEAddress.getAddressLine4();
                    if (addressLine04 != null) {
                        addressLine04 = addressLine04.trim();
                        if (addressLine04.length() <= STRING_40_MAX_LENGTH) {
                            address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4, addressLine04, getDataDictionary());
                        } else {
                            address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4,
                                    addressLine04.substring(0, STRING_40_MAX_LENGTH), getDataDictionary());
                        }
                    }

                    String addressLine05 = dfEAddress.getAddressLine5();
                    if (addressLine05 != null) {
                        addressLine05 = addressLine05.trim();
                        if (addressLine05.length() <= STRING_20_MAX_LENGTH) {
                            address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5, addressLine05, getDataDictionary());
                        } else {
                            address.setFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5,
                                    addressLine05.substring(0, STRING_20_MAX_LENGTH), getDataDictionary());
                        }
                    }
                }

                String postCode = dfEAddress.getPostCode();
                if (postCode != null) {
                    postCode = postCode.trim();
                    if (postCode.length() <= STRING_20_MAX_LENGTH) {
                        address.setPostalCode(postCode);
                    } else {
                        address.setPostalCode(postCode.substring(0, STRING_20_MAX_LENGTH));
                    }
                } else {
                    String zip = dfEAddress.getZip();
                    if (zip != null) {
                        zip = zip.trim();
                        if (zip.length() <= STRING_20_MAX_LENGTH) {
                            address.setPostalCode(zip);
                        } else {
                            address.setPostalCode(zip.substring(0, STRING_20_MAX_LENGTH));
                        }
                    }
                }

                String county = dfEAddress.getCounty();
                if (county != null) {
                    county = county.trim();
                    if (county.length() <= STRING_40_MAX_LENGTH) {
                        address.setCounty(county);
                    } else {
                        address.setCounty(county.substring(0, STRING_40_MAX_LENGTH));
                    }
                }

                String country = dfEAddress.getCountry();
                if (country != null) {
                    if (country.length() <= STRING_40_MAX_LENGTH) {
                        address.setCountry(country);
                    } else {
                        address.setCountry(country.substring(0, STRING_40_MAX_LENGTH));
                    }
                }

                String easting = dfEAddress.getEasting();
                if (easting != null) {
                    easting = easting.trim();
                    if (easting.length() <= DB_FIELD_A_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_EASTING, easting, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_EASTING, easting.substring(0, DB_FIELD_A_MAX_LENGTH),
                                getDataDictionary());
                    }
                }

                String northing = dfEAddress.getNorthing();
                if (northing != null) {
                    northing = northing.trim();
                    if (northing.length() <= DB_FIELD_A_MAX_LENGTH) {
                        address.setFieldValueByAlias(ALIAS_NAME_NORTHING, northing, getDataDictionary());
                    } else {
                        address.setFieldValueByAlias(ALIAS_NAME_NORTHING, northing.substring(0, DB_FIELD_A_MAX_LENGTH),
                                getDataDictionary());
                    }
                }

                m_broker.saveBeanForced(address);
            }

            return address;
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
         * Save a DfE Contact to Aspen Person Table.
         *
         * @param dfEContact DfEContact
         * @param contactAddressOid String
         * @return Person
         */
        public Person saveContactPerson(DfEContact dfEContact, String contactAddressOid) {
            Person contactPerson = X2BaseBean.newInstance(Person.class, m_broker.getPersistenceKey());
            contactPerson.setOrganization1Oid(DEFAULT_ORGANIZATION);
            contactPerson.setFieldValueByAlias(ALIAS_NAME_ORDER, Integer.toString(dfEContact.getOrder()),
                    getDataDictionary());
            String personTitleCode = dfEContact.getTitle();
            contactPerson.setNameTitleCode(personTitleCode);
            contactPerson.setLastName(dfEContact.getSurname());
            contactPerson.setFirstName(dfEContact.getForename());
            // Translate DfE Gender code.
            String genderCode = lookupRefCodeByStateCode(Person.class, Person.COL_GENDER_CODE, dfEContact.getGender());
            contactPerson.setGenderCode(genderCode);
            String responsibleStr = DB_FALSE;
            if (dfEContact.getResponsible() != null && dfEContact.getResponsible().booleanValue()) {
                responsibleStr = DB_TRUE;
            }
            contactPerson.setFieldValueByAlias(ALIAS_NAME_RESPONSIBLE, responsibleStr, getDataDictionary());
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
                    contactPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1, telephone.getTelephoneType(),
                            getDataDictionary());
                    contactPerson.setPhone01(telephone.getTelephoneNumber());
                } else if (l == 1) {
                    contactPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2, telephone.getTelephoneType(),
                            getDataDictionary());
                    contactPerson.setPhone02(telephone.getTelephoneNumber());
                } else if (l == 2) {
                    contactPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3, telephone.getTelephoneType(),
                            getDataDictionary());
                    contactPerson.setPhone03(telephone.getTelephoneNumber());
                }
            }
            m_broker.saveBeanForced(contactPerson);

            return contactPerson;
        }

        /**
         * Save DfE Contacts's to Aspen Person, Address, Contact, StudentContact Tables .
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @param studentAddressOid String
         * @param studentAddressLine01 String
         * @param activeSections HashMap<String,Boolean>
         */
        public void saveContacts(DfEPupil dfEPupil,
                                 String studentOid,
                                 String studentAddressOid,
                                 String studentAddressLine01,
                                 HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_CONTACTS)) {
                // Re-add All Contacts, ContactPerson's and ContactAddresses
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
                        addressView = studentAddressLine01;
                    } else {
                        DfEAddress contactDfEAddress = dfEContact.getDfEAddress();

                        Address contactAddress = null;
                        if (contactDfEAddress != null) {
                            contactAddress = saveAddress(contactDfEAddress, null, activeSections);
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
        }

        /**
         * Save a DfE Pupil to Aspen Person, Address, Student, StudentProgramParticipation,
         * Disability, Contact & StudentContact Tables .
         *
         * @param dfEPupil DfEPupil
         * @param schoolOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void saveDfEPupil(DfEPupil dfEPupil, String schoolOid, HashMap<String, Boolean> activeSections) {
            try {
                m_broker.beginTransaction();

                // Save StudentAddress
                DfEAddress dfEAddress = dfEPupil.getDfEAddress();
                Address studentAddress = null;
                String studentAddressOid = null;
                String studentAddressLine1 = null;
                if (dfEAddress != null) {
                    studentAddress = saveAddress(dfEAddress, null, activeSections);

                    if (studentAddress != null) {
                        studentAddressOid = studentAddress.getOid();
                        studentAddressLine1 = studentAddress.getAddressLine01();
                    }
                }

                // Save StudentPerson
                Person studentPerson = saveStudentPerson(dfEPupil, null, studentAddressOid, activeSections);
                String studentPeriodOid = studentPerson.getOid();

                // Save Student
                Student student =
                        saveStudent(dfEPupil, null, studentPeriodOid, studentAddressLine1, schoolOid, activeSections);
                String studentOid = student.getOid();

                // Save StudentProgramParticiation
                saveStudentProgramParticipation(dfEPupil, studentOid, activeSections);

                // Save StudentDisability
                saveStudentDisability(dfEPupil, studentOid, activeSections);

                // Save All Contacts, ContactPerson's and ContactAddress's
                saveContacts(dfEPupil, studentOid, studentAddressOid, studentAddressLine1, activeSections);

                // Add Student Enrollment records
                saveStudentSchoolHistories(dfEPupil, studentOid, activeSections);

                // Add Student Assessment records
                saveStudentAssessments(dfEPupil, studentOid, schoolOid, activeSections);

                // Add Student Attendance Histories records
                saveStudentAttendanceHistories(dfEPupil, studentOid, activeSections);

                m_broker.commitTransaction();
            } catch (RuntimeException re) {
                m_broker.rollbackTransaction();
                throw re;
            }
        }

        /**
         * Save a DfE Pupil to Aspen Student Table.
         *
         * @param dfEPupil DfEPupil
         * @param student Student
         * @param studentPersonOid String
         * @param addressView String
         * @param schoolOid String
         * @param activeSections HashMap<String,Boolean>
         * @return Student
         */
        public Student saveStudent(DfEPupil dfEPupil,
                                   Student student,
                                   String studentPersonOid,
                                   String addressView,
                                   String schoolOid,
                                   HashMap<String, Boolean> activeSections) {
            // If the student object is null, create new student else update the existing student
            if (student == null) {
                student = X2BaseBean.newInstance(Student.class, m_broker.getPersistenceKey());
            }

            if (student.getLocalId() == null) {
                String localId = generateStudentId(schoolOid);
                student.setLocalId(localId);
            }

            student.setPersonOid(studentPersonOid);
            student.setOrganization1Oid(DEFAULT_ORGANIZATION);
            student.setSchoolOid(schoolOid);
            student.setFieldValueByAlias(ALIAS_NAME_APPLICATION_REFERENCE, dfEPupil.getApplicationReference(),
                    getDataDictionary());
            student.setFieldValueByAlias(ALIAS_NAME_UPN, dfEPupil.getUniquePupilNumber(), getDataDictionary());
            student.setFieldValueByAlias(ALIAS_NAME_ULN, dfEPupil.getUniqueLearnerNumber(), getDataDictionary());
            student.setFieldValueByAlias(ALIAS_NAME_UCI, dfEPupil.getUniqueCandidateIdentifier(), getDataDictionary());

            if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
                student.setFieldValueByAlias(ALIAS_NAME_FORMER_UPN, dfEPupil.getFormerUniquePupilNumber(),
                        getDataDictionary());
                student.setFieldValueByAlias(ALIAS_NAME_SEN_PUPIL_PROVISION, dfEPupil.getSENProvision(),
                        getDataDictionary());
                if (dfEPupil.getFSMReviewDate() != null) {
                    String fSMReviewDateStr = m_dateFormat.format(dfEPupil.getFSMReviewDate());
                    student.setFieldValueByAlias(ALIAS_NAME_FSM_REVIEW_DATE, fSMReviewDateStr, getDataDictionary());
                }
                student.setGradeLevel(dfEPupil.getNCYearActual());
                // Translate DfE Enroll Status code.
                String enrollStatus = lookupRefCodeByStateCode(Student.class, Student.COL_ENROLLMENT_STATUS,
                        dfEPupil.getEnrollStatus());
                student.setEnrollmentStatus(enrollStatus);
                // Translate DfE Language code.
                String languageCode = lookupRefCodeByStateCode(Student.class, Student.COL_HOME_LANGUAGE_CODE,
                        dfEPupil.getFirstLanguageCode());
                student.setHomeLanguageCode(languageCode);
            }

            student.setNameView(dfEPupil.getSurname() + ", " + dfEPupil.getForename());
            student.setAddressView(addressView);

            if (isSectionActive(activeSections, PARAM_LOOKED_AFTER)) {
                String inCare = DB_FALSE;
                if (dfEPupil.getInCare() != null && dfEPupil.getInCare().booleanValue()) {
                    inCare = DB_TRUE;
                }
                student.setFieldValueByAlias(ALIAS_NAME_IN_CARE, inCare, getDataDictionary());
                // No translation of DfE Care Authority Code (LEA)
                student.setFieldValueByAlias(ALIAS_NAME_CARE_AUTHORITY, dfEPupil.getCareAuthority(),
                        getDataDictionary());
            }

            // TODO Remove later
            // Should reference by alias, clob in alias file not specific fields
            if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
                student.setFieldValueByAlias(ALIAS_NAME_ATTENDANCE_XML, dfEPupil.getAttendanceXML(),
                        getDataDictionary());
            }

            if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
                student.setFieldValueByAlias(ALIAS_NAME_ASSESSMENTS_XML, dfEPupil.getAssessmentsXML(),
                        getDataDictionary());
            }

            if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
                student.setFieldValueByAlias(ALIAS_NAME_SCHOOL_HISTORY_XML, dfEPupil.getSchoolHistoryXML(),
                        getDataDictionary());
            }

            m_broker.saveBeanForced(student);

            return student;
        }


        /**
         * Save a DfE Pupil Attendance data to Student School table.
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void saveStudentAttendanceHistories(DfEPupil dfEPupil,
                                                   String studentOid,
                                                   HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
                // Including all StageAssessments
                ArrayList<DfEAttendance> attendances = dfEPupil.getAttendances();

                String cTFSchoolOid = m_cTFSchool.getOid();

                // Save Attendance data to Student School table.
                for (int i = 0; i < attendances.size(); i++) {
                    DfEAttendance dfEAttendance = attendances.get(i);

                    // Create Student Enrollment Entry
                    StudentSchool studentSchool =
                            X2BaseBean.newInstance(StudentSchool.class, m_broker.getPersistenceKey());
                    studentSchool.setSchoolOid(cTFSchoolOid);
                    studentSchool.setStudentOid(studentOid);
                    studentSchool.setType(StudentSchool.FORMER);

                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_YEAR, dfEAttendance.getYear());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_LEA, dfEAttendance.getLEA());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_ESTAB, dfEAttendance.getEstab());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SCHOOL_NAME, dfEAttendance.getSchoolName());
                    String sessionsPossibleStr = String.valueOf(dfEAttendance.getSessionsPossible());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_POSSIBLE, sessionsPossibleStr);
                    String sessionsAuthorisedStr = String.valueOf(dfEAttendance.getSessionsAuthorised());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_AUTHORIZED, sessionsAuthorisedStr);
                    String sessionsAttendedStr = String.valueOf(dfEAttendance.getSessionsAttended());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_ATTENDED, sessionsAttendedStr);
                    String sessionsUnauthorisedStr = String.valueOf(dfEAttendance.getSessionsUnauthorised());
                    studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_UNAUTHORIZED, sessionsUnauthorisedStr);
                    if (dfEAttendance.getAttendanceStartDate() != null) {
                        String startDateStr = m_dateFormat.format(dfEAttendance.getAttendanceStartDate());
                        studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_START_DATE, startDateStr);
                    }
                    if (dfEAttendance.getAttendanceMarks() != null) {
                        studentSchool.setFieldValueByAlias(ALIAS_NAME_ATTEND_MARKS, dfEAttendance.getAttendanceMarks());
                    }

                    m_broker.saveBeanForced(studentSchool);
                }
            }
        }


        /**
         * save a DfE Pupil's Stage Assessment (Student Assessment) records.
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @param schoolOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void saveStudentAssessments(DfEPupil dfEPupil,
                                           String studentOid,
                                           String schoolOid,
                                           HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
                // Including all StageAssessments
                ArrayList<DfEStageAssessment> stageAssessments = dfEPupil.getStageAssessments();

                if (m_sATAssessmentDefinition != null) {
                    String assessmentDefinitionOid = m_sATAssessmentDefinition.getOid();

                    DataDictionary extendedDataDictionary = null;

                    // Save Stage Assessment data to Student Assessment table.
                    for (int i = 0; i < stageAssessments.size(); i++) {
                        DfEStageAssessment dfEStageAssessment = stageAssessments.get(i);

                        // Create Student Enrollment Entry
                        StudentAssessment studentAssessment =
                                X2BaseBean.newInstance(StudentAssessment.class, m_broker.getPersistenceKey());
                        studentAssessment.setSchoolOid(schoolOid);
                        studentAssessment.setStudentOid(studentOid);
                        studentAssessment.setAssessmentDefinitionOid(assessmentDefinitionOid);
                        studentAssessment.setDate(dfEStageAssessment.getResultDate());

                        if (extendedDataDictionary == null) {
                            extendedDataDictionary = DataDictionary.getDistrictDictionary(
                                    studentAssessment.getExtendedDataDictionary(), m_broker.getPersistenceKey());
                        }

                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_STAGE, dfEStageAssessment.getStage(),
                                extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_LOCALE, dfEStageAssessment.getLocale(),
                                extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_YEAR_TAKEN, dfEStageAssessment.getYear(),
                                extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_SUBJECT, dfEStageAssessment.getSubject(),
                                extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_METHOD, dfEStageAssessment.getMethod(),
                                extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_COMPONENT, dfEStageAssessment.getComponent(),
                                extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT_STATUS,
                                dfEStageAssessment.getResultStatus(), extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT_QUALIFIER,
                                dfEStageAssessment.getResultQualifier(), extendedDataDictionary);
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT, dfEStageAssessment.getResult(),
                                extendedDataDictionary);
                        String resultDateStr = m_dateFormat.format(dfEStageAssessment.getResultDate());
                        studentAssessment.setFieldValueByAlias(ALIAS_NAME_RESULT_DATE, resultDateStr,
                                extendedDataDictionary);

                        m_broker.saveBeanForced(studentAssessment);
                    }
                }
            }
        }

        /**
         * Save a DfE Contact to Aspen StudentContact Table.
         *
         * @param dfEContact DfEContact
         * @param contactOid String
         * @param studentOid String
         * @param livesWithStudent boolean
         */
        public void saveStudentContact(DfEContact dfEContact,
                                       String contactOid,
                                       String studentOid,
                                       boolean livesWithStudent) {
            StudentContact studentContact = X2BaseBean.newInstance(StudentContact.class, m_broker.getPersistenceKey());
            studentContact.setContactOid(contactOid);
            studentContact.setStudentOid(studentOid);
            studentContact.setLivesWithIndicator(livesWithStudent);
            // Translate DfE Relationship code.
            String relationshipCode = lookupRefCodeByStateCode(StudentContact.class,
                    StudentContact.COL_RELATIONSHIP_CODE, dfEContact.getRelationshipCode());
            studentContact.setRelationshipCode(relationshipCode);

            m_broker.saveBeanForced(studentContact);
        }

        /**
         * Save a DfE Pupil Disability information to Aspen Student Disability Table.
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void saveStudentDisability(DfEPupil dfEPupil,
                                          String studentOid,
                                          HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
                ArrayList<String> disabilities = dfEPupil.getDisabilities();
                for (int i = 0; i < disabilities.size(); i++) {
                    String dfFDisabilityCode = disabilities.get(i);

                    IepDisability disability =
                            X2BaseBean.newInstance(IepDisability.class, m_broker.getPersistenceKey());
                    disability.setStudentOid(studentOid);
                    // Translate DfE Disability code.
                    String disabilityCode = lookupRefCodeByStateCode(IepDisability.class,
                            IepDisability.COL_DISABILITY_CODE, dfFDisabilityCode);
                    disability.setDisabilityCode(disabilityCode);
                    disability.setPrimaryIndicator(false);
                    disability.setTransportationIndicator(false);

                    m_broker.saveBeanForced(disability);
                }
            }
        }

        /**
         * Save a DfE Pupil to Aspen Person Table.
         *
         * @param dfEPupil DfEPupil
         * @param studentPerson Person
         * @param studentAddressOid String
         * @param activeSections HashMap<String,Boolean>
         * @return Person
         */
        public Person saveStudentPerson(DfEPupil dfEPupil,
                                        Person studentPerson,
                                        String studentAddressOid,
                                        HashMap<String, Boolean> activeSections) {
            // Save StudentPerson
            boolean isNew = false;
            if (studentPerson == null) {
                isNew = true;
                studentPerson = X2BaseBean.newInstance(Person.class, m_broker.getPersistenceKey());
            }

            // Core Fields
            studentPerson.setOrganization1Oid(DEFAULT_ORGANIZATION);
            String lastName = dfEPupil.getSurname();
            if (lastName != null) {
                lastName = lastName.trim();
                if (lastName.length() <= STRING_50_MAX_LENGTH) {
                    studentPerson.setLastName(lastName);
                } else {
                    studentPerson.setLastName(lastName.substring(0, STRING_50_MAX_LENGTH));
                }
            }
            String firstName = dfEPupil.getForename();
            if (firstName != null) {
                firstName = firstName.trim();
                if (firstName.length() <= STRING_32_MAX_LENGTH) {
                    studentPerson.setFirstName(firstName);
                } else {
                    studentPerson.setFirstName(firstName.substring(0, STRING_32_MAX_LENGTH));
                }
            }
            String middleName = dfEPupil.getMiddleNames();
            if (middleName != null) {
                middleName = middleName.trim();
                if (middleName.length() <= STRING_32_MAX_LENGTH) {
                    studentPerson.setMiddleName(middleName);
                } else {
                    studentPerson.setMiddleName(middleName.substring(0, STRING_32_MAX_LENGTH));
                }
            }
            studentPerson.setDob(dfEPupil.getBirthDate());
            // Translate DfE Gender code.
            String genderCode = lookupRefCodeByStateCode(Person.class, Person.COL_GENDER_CODE, dfEPupil.getGender());
            studentPerson.setGenderCode(genderCode);
            studentPerson.setStudentIndicator(true);

            // Address
            if (isNew) {
                studentPerson.setPhysicalAddressOid(studentAddressOid);
                studentPerson.setMailingAddressOid(studentAddressOid);
            }

            // Basic Details Fields
            if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
                studentPerson.setFieldValueByAlias(ALIAS_NAME_PREFERRED_SURNAME, dfEPupil.getPreferredSurname(),
                        getDataDictionary());
                studentPerson.setFieldValueByAlias(ALIAS_NAME_PREFERRED_FORENAME, dfEPupil.getPreferredForename(),
                        getDataDictionary());
                studentPerson.setFieldValueByAlias(ALIAS_NAME_FORMER_SURNAME, dfEPupil.getFormerSurname(),
                        getDataDictionary());
                studentPerson.setFieldValueByAlias(ALIAS_NAME_FORMER_FORENAME, dfEPupil.getFormerForename(),
                        getDataDictionary());
                // No translation of DfE Ethnicity code, save as is.
                studentPerson.setFieldValueByAlias(ALIAS_NAME_ETHNICITY, dfEPupil.getEthnicity(), getDataDictionary());
                String ethnicitySourceFieldName = translateAliasToJavaName(ALIAS_NAME_ETHNICITY_SOURCE, true);
                if (ethnicitySourceFieldName != null) {
                    String ethnicitySourceCode = lookupRefCodeByStateCode(Person.class, ethnicitySourceFieldName,
                            dfEPupil.getEthnicitySource());
                    studentPerson.setFieldValueByAlias(ALIAS_NAME_ETHNICITY_SOURCE, ethnicitySourceCode,
                            getDataDictionary());
                }
                String medicalFlagStr = DB_FALSE;
                if (dfEPupil.getMedicalFlag() != null && dfEPupil.getMedicalFlag().booleanValue()) {
                    medicalFlagStr = DB_TRUE;
                }
                studentPerson.setFieldValueByAlias(ALIAS_NAME_MEDICAL_FLAG, medicalFlagStr, getDataDictionary());
            }

            // Phone and Email Fields
            if (isSectionActive(activeSections, PARAM_ADDRESS_PHONE_EMAIL)) {
                ArrayList<DfETelephone> telephones = dfEPupil.getTelephones();
                for (int l = 0; l < 3; l++) {
                    if (l >= telephones.size()) {
                        break;
                    }

                    DfETelephone telephone = telephones.get(l);
                    String telephoneType = telephone.getTelephoneType();
                    String telephoneNumber = telephone.getTelephoneNumber();

                    if (l == 0) {
                        if (telephoneType != null) {
                            studentPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1, telephoneType,
                                    getDataDictionary());
                        }

                        if (telephoneNumber != null) {
                            telephoneNumber = telephoneNumber.trim();
                            if (telephoneNumber.length() <= STRING_20_MAX_LENGTH) {
                                studentPerson.setPhone01(telephoneNumber);
                            } else {
                                studentPerson.setPhone01(telephoneNumber.substring(0, STRING_20_MAX_LENGTH));
                            }
                        }
                    } else if (l == 1) {
                        if (telephoneType != null) {
                            studentPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2, telephoneType,
                                    getDataDictionary());
                        }

                        if (telephoneNumber != null) {
                            telephoneNumber = telephoneNumber.trim();
                            if (telephoneNumber.length() <= STRING_20_MAX_LENGTH) {
                                studentPerson.setPhone02(telephoneNumber);
                            } else {
                                studentPerson.setPhone02(telephoneNumber.substring(0, STRING_20_MAX_LENGTH));
                            }
                        }
                    } else if (l == 2) {
                        if (telephoneType != null) {
                            studentPerson.setFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3, telephoneType,
                                    getDataDictionary());
                        }

                        if (telephoneNumber != null) {
                            telephoneNumber = telephoneNumber.trim();
                            if (telephoneNumber.length() <= STRING_20_MAX_LENGTH) {
                                studentPerson.setPhone03(telephoneNumber);
                            } else {
                                studentPerson.setPhone03(telephoneNumber.substring(0, STRING_20_MAX_LENGTH));
                            }
                        }
                    }
                }

                String email = dfEPupil.getEmail();
                if (email != null) {
                    email = email.trim();
                    if (email.length() <= STRING_100_MAX_LENGTH) {
                        studentPerson.setEmail01(email);
                    } else {
                        studentPerson.setEmail01(email.substring(0, STRING_100_MAX_LENGTH));
                    }
                }
            }

            m_broker.saveBeanForced(studentPerson);

            return studentPerson;
        }

        /**
         * Save a DfE Pupil SEN information to Aspen Student Table.
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void saveStudentProgramParticipation(DfEPupil dfEPupil,
                                                    String studentOid,
                                                    HashMap<String, Boolean> activeSections) {
            // FMS History fields
            if (isSectionActive(activeSections, PARAM_FSM_HISTORY)) {
                ArrayList<DfEFSMInstance> fSMInstances = dfEPupil.getFSMInstances();
                for (int i = 0; i < fSMInstances.size(); i++) {
                    DfEFSMInstance dfEFSMInstance = fSMInstances.get(i);

                    StudentProgramParticipation studentProgramParticipation =
                            X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                    studentProgramParticipation.setStudentOid(studentOid);
                    // Translate DfE Program code.
                    String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_FRL);
                    studentProgramParticipation.setProgramCode(programCode);
                    studentProgramParticipation.setStartDate(dfEFSMInstance.getFSMStartDate());
                    studentProgramParticipation.setEndDate(dfEFSMInstance.getFSMEndDate());
                    // Translate DfE UK Country Code.
                    String uKCountryFieldName = translateAliasToJavaName(ALIAS_NAME_FSM_UK_COUNTRY, true);
                    if (uKCountryFieldName != null) {
                        String uKCountryRefCode = lookupRefCodeByStateCode(StudentProgramParticipation.class,
                                uKCountryFieldName, dfEFSMInstance.getUKCountry());
                        studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_FSM_UK_COUNTRY, uKCountryRefCode,
                                getDataDictionary());
                    }

                    m_broker.saveBeanForced(studentProgramParticipation);
                }
            }

            // SEN History fields
            if (isSectionActive(activeSections, PARAM_SEN_HISTORY)) {
                ArrayList<DfESENNeed> sENNeeds = dfEPupil.getSENNeeds();
                for (int i = 0; i < sENNeeds.size(); i++) {
                    DfESENNeed sENNeed = sENNeeds.get(i);

                    StudentProgramParticipation studentProgramParticipation =
                            X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                    studentProgramParticipation.setStudentOid(studentOid);
                    // Translate DfE Program code.
                    String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_SE);
                    studentProgramParticipation.setProgramCode(programCode);
                    studentProgramParticipation.setStartDate(dfEPupil.getSENStartDate());
                    // No translation of DfE SEN Provision code, save as is.
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_PROVISION,
                            dfEPupil.getSENProvision(), getDataDictionary());
                    // No translation of DfE SEN Type code, save as is.
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_TYPE, sENNeed.getType(),
                            getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_RANK, sENNeed.getTypeRank(),
                            getDataDictionary());
                    m_broker.saveBeanForced(studentProgramParticipation);
                }
            }

            // NAW Details fields
            if (isSectionActive(activeSections, PARAM_NAW_DETAILS)) {
                ArrayList<DfENAWDetail> nAWDetails = dfEPupil.getNAWDetails();
                if (nAWDetails.size() > 0) {
                    // Only interested in the first one.
                    DfENAWDetail nAWDetail = nAWDetails.get(0);

                    StudentProgramParticipation studentProgramParticipation =
                            X2BaseBean.newInstance(StudentProgramParticipation.class, m_broker.getPersistenceKey());
                    studentProgramParticipation.setStudentOid(studentOid);
                    // Translate DfE Program code.
                    String programCode = lookupRefCodeByLocalCode(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, DFE_PROGRAM_CODE_NAW);
                    studentProgramParticipation.setProgramCode(programCode);
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SPEAK_WELSH, nAWDetail.getSpeakWelsh(),
                            getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_HOME_WELSH, nAWDetail.getHomeWelsh(),
                            getDataDictionary());
                    // Translate DfE National Identity Code.
                    String nationalIdentityFieldName = translateAliasToJavaName(ALIAS_NAME_NATIONAL_IDENTITY, true);
                    if (nationalIdentityFieldName != null) {
                        String nationalIdentityRefCode = lookupRefCodeByStateCode(StudentProgramParticipation.class,
                                nationalIdentityFieldName, nAWDetail.getNationalIdentity());
                        studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_NATIONAL_IDENTITY,
                                nationalIdentityRefCode, getDataDictionary());
                    }
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_WELSH_SOURCE,
                            nAWDetail.getWelshSource(), getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_EAL_ACQUISITION,
                            nAWDetail.getEALAcquisition(), getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_LANGUAGE_SOURCE,
                            nAWDetail.getLanguageSource(), getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_CURR_TEACH_METHOD,
                            nAWDetail.getSENCurrTeachingMethods(), getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_GROUPING_SUPPORT,
                            nAWDetail.getSENGroupingAndSupport(), getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_SPEC_RESOURCES,
                            nAWDetail.getSENSpecialisedResources(), getDataDictionary());
                    studentProgramParticipation.setFieldValueByAlias(ALIAS_NAME_SEN_ADVICE_ASSESSMENT,
                            nAWDetail.getSENAdviceAndAssessment(), getDataDictionary());

                    m_broker.saveBeanForced(studentProgramParticipation);
                }
            }
        }

        /**
         * save a DfE Pupil's School History (Student Enrollment) records.
         *
         * @param dfEPupil DfEPupil
         * @param studentOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void saveStudentSchoolHistories(DfEPupil dfEPupil,
                                               String studentOid,
                                               HashMap<String, Boolean> activeSections) {
            if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
                ArrayList<DfESchoolHistory> schoolHistories = dfEPupil.getSchoolHistories();

                for (int i = 0; i < schoolHistories.size(); i++) {
                    DfESchoolHistory dfESchoolHistory = schoolHistories.get(i);

                    String lEA = dfESchoolHistory.getLEA();
                    String estab = dfESchoolHistory.getEstab();
                    String schoolName = dfESchoolHistory.getSchoolName();
                    School school = getSchoolByEstab(estab);
                    boolean isCTFSchool = false;

                    // If the school is not in our LEA then get the dummy "CTF School".
                    if (school == null) {
                        school = m_cTFSchool;
                        isCTFSchool = true;
                    }
                    String schoolOid = school.getOid();

                    PlainDate entryDate = dfESchoolHistory.getEntryDate();
                    if (entryDate != null) {
                        // Create Student Enrollment Entry
                        StudentEnrollment studentEnrollment =
                                X2BaseBean.newInstance(StudentEnrollment.class, m_broker.getPersistenceKey());
                        studentEnrollment.setSchoolOid(schoolOid);
                        studentEnrollment.setStudentOid(studentOid);
                        studentEnrollment.setEnrollmentDate(entryDate);
                        studentEnrollment.setEnrollmentType(StudentEnrollment.ENTRY);

                        // If the School is out-of-LEA then save the LEA and School information on
                        // the Student Enrollment record.
                        if (isCTFSchool) {
                            if (lEA.length() <= STRING_10_MAX_LENGTH) {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID, lEA,
                                        getDataDictionary());
                            } else {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID,
                                        lEA.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                            }
                            if (estab.length() <= STRING_10_MAX_LENGTH) {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB, estab,
                                        getDataDictionary());
                            } else {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                        estab.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                            }
                            if (schoolName.length() <= STRING_50_MAX_LENGTH) {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME, schoolName,
                                        getDataDictionary());
                            } else {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME,
                                        schoolName.substring(0, STRING_50_MAX_LENGTH), getDataDictionary());
                            }
                        }

                        m_broker.saveBeanForced(studentEnrollment);
                    }

                    PlainDate leavingDate = dfESchoolHistory.getLeavingDate();
                    String leavingReason = dfESchoolHistory.getLeavingReason();
                    if (leavingDate != null) {
                        // Create Student Enrollment Withdrawal
                        StudentEnrollment studentEnrollment =
                                X2BaseBean.newInstance(StudentEnrollment.class, m_broker.getPersistenceKey());
                        studentEnrollment.setSchoolOid(schoolOid);
                        studentEnrollment.setStudentOid(studentOid);
                        studentEnrollment.setEnrollmentDate(leavingDate);
                        studentEnrollment.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
                        if (leavingReason.length() <= STRING_10_MAX_LENGTH) {
                            studentEnrollment.setEnrollmentCode(leavingReason);
                        } else {
                            studentEnrollment.setEnrollmentCode(leavingReason.substring(0, STRING_10_MAX_LENGTH));
                        }

                        // If the School is out-of-LEA then save the LEA and School information on
                        // the Student Enrollment record.
                        if (isCTFSchool) {
                            if (lEA.length() <= STRING_10_MAX_LENGTH) {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID, lEA,
                                        getDataDictionary());
                            } else {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID,
                                        lEA.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                            }
                            if (estab.length() <= STRING_10_MAX_LENGTH) {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB, estab,
                                        getDataDictionary());
                            } else {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                        estab.substring(0, STRING_10_MAX_LENGTH), getDataDictionary());
                            }
                            if (schoolName.length() <= STRING_50_MAX_LENGTH) {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME, schoolName,
                                        getDataDictionary());
                            } else {
                                studentEnrollment.setFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME,
                                        schoolName.substring(0, STRING_50_MAX_LENGTH), getDataDictionary());
                            }
                        }

                        m_broker.saveBeanForced(studentEnrollment);
                    }
                }
            }
        }

        /**
         * Update a DfE Pupil's Aspen Person, Address, Student, StudentProgramParticipation,
         * Disability, Contact & StudentContact Tables .
         *
         * @param dfEPupil DfEPupil
         * @param student Student
         * @param schoolOid String
         * @param activeSections HashMap<String,Boolean>
         */
        public void updateDfEPupil(DfEPupil dfEPupil,
                                   Student student,
                                   String schoolOid,
                                   HashMap<String, Boolean> activeSections) {
            try {
                m_broker.beginTransaction();

                String studentOid = student.getOid();
                String studentPersonOid = student.getPersonOid();
                Person studentPerson = student.getPerson();

                // Save StudentPerson
                studentPerson = saveStudentPerson(dfEPupil, studentPerson, null, activeSections);
                String studentAddressOid = studentPerson.getPhysicalAddressOid();

                // Save StudentAddress
                DfEAddress dfEAddress = dfEPupil.getDfEAddress();
                Address studentAddress = null;
                String studentAddressLine1 = null;
                if (dfEAddress != null) {
                    studentAddress = studentPerson.getPhysicalAddress();
                    studentAddress = saveAddress(dfEAddress, studentAddress, activeSections);

                    studentAddressOid = studentAddress.getOid();
                    studentAddressLine1 = studentAddress.getAddressLine01();

                    // This scenario can happen if they didn't have an address on the first import
                    // but later provided their address information on a second import.
                    if (studentPerson.getPhysicalAddressOid() == null) {
                        studentPerson.setPhysicalAddressOid(studentAddressOid);
                        studentPerson = saveStudentPerson(dfEPupil, studentPerson, null, activeSections);
                    }
                }

                saveStudent(dfEPupil, student, studentPersonOid, studentAddressLine1, schoolOid, activeSections);

                // Remove any existing StudentProgramParticipation records
                deleteStudentProgramParticipations(studentOid, activeSections);
                // Re-add StudentProgramParticiation
                saveStudentProgramParticipation(dfEPupil, studentOid, activeSections);

                // Remove any existing StudentProgramParticipation records
                deleteStudentDisabilities(studentOid, activeSections);
                // Re-add StudentDisability
                saveStudentDisability(dfEPupil, studentOid, activeSections);

                // Remove all Contacts, ContactPerson's and ContactAddresses
                deleteContacts(studentOid, studentAddressOid, activeSections);
                // Re-add All Contacts, ContactPerson's and ContactAddresses
                saveContacts(dfEPupil, studentOid, studentAddressOid, studentAddressLine1, activeSections);

                // Remove any existing Student Enrollment records
                deleteStudentSchoolHistories(studentOid, activeSections);
                // Re-add Student Enrollment records
                saveStudentSchoolHistories(dfEPupil, studentOid, activeSections);

                // Remove any existing Student Assessment records
                deleteStudentAssessments(studentOid, activeSections);
                // Re-add Student Assessment records
                saveStudentAssessments(dfEPupil, studentOid, schoolOid, activeSections);

                // Remove any existing Student Attendance Histories records
                deleteStudentAttendanceHistories(studentOid, activeSections);
                // Re-add Student Attendance Histories records
                saveStudentAttendanceHistories(dfEPupil, studentOid, activeSections);

                m_broker.commitTransaction();
            } catch (RuntimeException re) {
                m_broker.rollbackTransaction();
                throw re;
            }

        }


    }


}
