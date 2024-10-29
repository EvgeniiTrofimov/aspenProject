/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.net.ssl.HttpsURLConnection;
// A third party ZIP library is used to support extracting password-protected zip files.
import net.lingala.zip4j.core.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.FileHeader;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * Procedure that updates the IEP and 504 status fields to "Active" based on students with an IEP or
 * 504 document
 * in ClearTrack. This is done by connecting to the remote ClearTrack server, retrieving an XML file
 * containing
 * the active students, and updating the database accordingly.
 *
 * A field with the alias "Cleartrack District ID" must be defined on the Organization and contain a
 * district ID
 * as defined in the cleartrack system.
 *
 * @author mmastrangelo
 */
public class CleartrackStatusImport extends ProcedureJavaSource {
    private static final String DOC_INFO_BASE_URL = "https://www.cleartrackrti.com/scripts/test.wsc/neric_conn.r";
    private static final String SMS_ID = "x4m79dw2p";
    private static final String MODE = "1";

    private static final String CLEARTRACK_DISTRICT_ID_ALIAS = "Cleartrack District ID";

    private File m_tempFolder = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        File zip = downloadZipFile();
        byte[] xmlBytes = extractXmlBytes(zip);

        if (xmlBytes != null && xmlBytes.length > 0) {
            updateStudents(xmlBytes);
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_tempFolder = userData.getUserTempFolder();
    }

    /**
     * Extracts the Doc_info.xml file from the passed zip file and returns its bytes.
     *
     * @param zip File
     * @return byte[]
     * @throws ZipException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private byte[] extractXmlBytes(File zip) throws ZipException, IOException {
        byte[] xmlBytes = null;

        if (zip != null && zip.length() > 0) {
            ZipFile zipFile = new ZipFile(zip);
            if (zipFile.isEncrypted()) {
                zipFile.setPassword(SMS_ID);
            }

            FileHeader fileHeader = zipFile.getFileHeader("Doc_Info.xml");
            if (fileHeader != null) {
                ByteArrayOutputStream boStream = new ByteArrayOutputStream();
                StreamUtils.copyStream(zipFile.getInputStream(fileHeader), boStream);

                xmlBytes = boStream.toByteArray();

                AppGlobals.getLog().info("ClearTrack Status Import: extracted zip file");
            }
        }

        return xmlBytes;
    }

    /**
     * Clears the IEP and 504 status fields for active students in the current organization.
     */
    private void resetStatusFields() {
        Map<String, Object> attributesToUpdate = new HashMap<>();
        attributesToUpdate.put(Student.COL_SECTION504_STATUS_CODE, null);
        attributesToUpdate.put(Student.COL_SPED_STATUS_CODE, null);

        X2Criteria orgCriteria = OrganizationManager.getOrganizationCriteria(getOrganization().getOid());
        orgCriteria.setEmbraced(true);

        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        criteria.addAndCriteria(orgCriteria);

        UpdateQuery updateQuery = new UpdateQuery(SisStudent.class, criteria, attributesToUpdate);

        int affected = getBroker().executeUpdateQuery(updateQuery);

        logMessage("Reset " + affected + " students");
        AppGlobals.getLog().info("ClearTrack Status Import: Reset " + affected + " students");
    }

    /**
     * Updates students with data contained in the passed Doc_info.xml file, passed as a byte array.
     * Existing IEP and 504 status values are cleared first for active students.
     *
     * @param xmlBytes byte[]
     * @throws JDOMException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void updateStudents(byte[] xmlBytes) throws JDOMException,
            IOException {
        String districtId = (String) getOrganization().getFieldValueByAlias(CLEARTRACK_DISTRICT_ID_ALIAS);

        if (StringUtils.isEmpty(districtId)) {
            logMessage("An ID must be present on the organization in a field with the alias '"
                    + CLEARTRACK_DISTRICT_ID_ALIAS + "'");
        } else {
            SAXBuilder builder = new SAXBuilder();
            org.jdom.Document document = builder.build(new ByteArrayInputStream(xmlBytes));
            Element root = document.getRootElement();

            int children = root.getContentSize();
            AppGlobals.getLog()
                    .info("ClearTrack Status Import: successfully loaded XML document with " + children + " elements");
            logMessage("Successfully loaded XML document with " + children + " elements");

            resetStatusFields();

            int updateCount = 0;
            List<Element> docInfoRows = root.getChildren("tt-Doc_InfoRow");
            for (Element docInfoRow : docInfoRows) {
                String districtCode = docInfoRow.getChildText("District_Code");
                if (!StringUtils.isEmpty(districtCode) && districtId.equals(districtCode)) {
                    String studentId = docInfoRow.getChildText("Student_Id");
                    if (!StringUtils.isEmpty(studentId)) {
                        String docType = docInfoRow.getChildText("Document_Type");
                        if (StringUtils.isEmpty(docType)) {
                            X2Criteria criteria = new X2Criteria();
                            criteria.addEqualTo(Student.COL_LOCAL_ID, studentId);

                            BeanQuery beanQuery = new BeanQuery(Student.class, criteria);

                            Student student = (Student) getBroker().getBeanByQuery(beanQuery);
                            if (student != null) {
                                if ("504".equals(docType)) {
                                    student.setSection504StatusCode("Active");
                                } else {
                                    student.setSpedStatusCode("Active");
                                }
                            }

                            getBroker().saveBeanForced(student);

                            logMessage("Updated " + docType + " student " + student.getLocalId() + " "
                                    + student.getNameView());
                            updateCount++;
                        }
                    }
                }
            }

            logMessage("Updated " + updateCount + " students");
            AppGlobals.getLog().info("ClearTrack Status Import: Updated " + updateCount + " students");
        }
    }

    /**
     * Downloads the zip file from the Cleatrack server containing the Doc_Info.xml file.
     *
     * @return File
     * @throws MalformedURLException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private File downloadZipFile() throws MalformedURLException, IOException {
        File tempFile = null;

        long start = System.currentTimeMillis();

        HttpsURLConnection connection = null;
        try {
            String urlString = DOC_INFO_BASE_URL + "?SMSId=" + SMS_ID + "&mode=" + MODE;
            URL url = new URL(urlString);

            connection = (HttpsURLConnection) url.openConnection();
            AppGlobals.getLog().info("ClearTrack Status Import: connected to remote server");

            tempFile = File.createTempFile("cltrk", "zip", m_tempFolder);
            FileOutputStream outStream = new FileOutputStream(tempFile);
            StreamUtils.copyStream(connection.getInputStream(), outStream);

            long elapsed = System.currentTimeMillis() - start;

            AppGlobals.getLog().info("ClearTrack Status Import: downloaded " + tempFile.length() + " bytes to "
                    + tempFile.getName() + " in " + elapsed + " ms.");
            logMessage("Downloaded " + tempFile.length() + " bytes from ClearTrack server in " + elapsed + " ms.");
        } finally {
            if (connection != null) {
                connection.disconnect();
                AppGlobals.getLog().info("ClearTrack Status Import: disconnected from remote server");
            }
        }

        return tempFile;
    }
}
