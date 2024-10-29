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

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.struts.util.MessageResources;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * This class imports exam results for the UK education system.
 *
 * @author Follett Software Company
 */

public class CTFImport extends ToolJavaSource {
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

}
