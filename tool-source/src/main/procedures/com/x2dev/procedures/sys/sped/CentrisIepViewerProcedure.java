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
package com.x2dev.procedures.sys.sped;

import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebConstants;
import com.x2dev.utils.WebServiceUtils;
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
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import javax.xml.soap.SOAPException;
import org.apache.commons.lang3.StringUtils;

/**
 * PageOutputProcedure that displays an iframe containing a document from Centris IEP direct. A
 * SOAP call is first made to the remote IEP direct system to retreive a URL for viewing the
 * document
 * (either an IEP or 504). After retrieving the URL, it is used as the "src" of the rendered iframe.
 *
 * @author mmastrangelo
 */
public class CentrisIepViewerProcedure extends PageOutputProcedure {
    private static final String WEB_SERVICE_URL = "https://www.iepdirect.com/centriswebservices/iepviewer.asmx";
    private static final String WEB_SERVICE_ACTION =
            "http://www.iepdirect.com/CentrisWebServices/IEPViwer/GetIEPUrlWithAudit";
    private static final String WEB_SERVICE_URI = "http://www.iepdirect.com/CentrisWebServices/IEPViwer";
    private static final String WEB_SERVICE_NAMESPACE = "";
    private static final String WEB_SERVICE_METHODNAME = "GetIEPUrlWithAudit";
    // also see values in getStaticParameters

    /**
     * Gets the window title.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure#getWindowTitle()
     */
    @Override
    public String getWindowTitle() {
        return "Centris IEP/504";
    }

    /**
     * Write initial page.
     *
     * @param writer JspWriter
     * @param request HttpServletRequest
     * @param userData UserDataContainer
     * @throws JspException exception
     * @throws IOException Signals that an I/O exception has occurred.
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure#writeInitialPage(javax.servlet.
     *      jsp.JspWriter, javax.servlet.http.HttpServletRequest,
     *      com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void writeInitialPage(JspWriter writer,
                                 HttpServletRequest request,
                                 UserDataContainer userData)
            throws JspException, IOException {
        Student student = null;

        String studentOid = request.getParameter(WebConstants.OID_PARAMETER);
        if (!StringUtils.isEmpty(studentOid)) {
            ModelBroker broker = new ModelBroker(userData.getPrivilegeSet());
            student = (Student) broker.getBeanByOid(Student.class, studentOid);
        } else {
            student = userData.getCurrentRecord(Student.class);
        }

        String studentId = null;
        if (student != null) {
            studentId = student.getLocalId();
        }

        if (StringUtils.isEmpty(studentId)) {
            writer.print(
                    "<div style=\"text-align: left\"><h2>Unable to find a current student local ID, which is required to display an IEP.</h2></div>");
        } else {
            Map<String, String> parameters = getStaticParameters();
            parameters.put("genEdID", studentId);
            parameters.put("Type", "");

            try {
                String xmlResults = WebServiceUtils.getXMLResults(WEB_SERVICE_URL,
                        WEB_SERVICE_ACTION,
                        WEB_SERVICE_NAMESPACE,
                        WEB_SERVICE_METHODNAME,
                        WEB_SERVICE_URI,
                        parameters,
                        AppGlobals.getLog());

                writer.print("<iframe src=\"");
                writer.print(xmlResults);
                writer.print("\" width=\"98%\" height=\"98%\"></iframe>");
            } catch (SOAPException soape) {
                AppGlobals.getLog().log(Level.SEVERE, "Error connecting to Centris IEP Viewer web service", soape);

                writer.print("<div style=\"text-align: left\"><h2>Error connecting to Centris. Details:</h2>");

                PrintWriter printWriter = new PrintWriter(writer);
                soape.printStackTrace(printWriter);

                writer.print("</div>");
            }
        }
    }

    /**
     * Gets the required privilege ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure#getRequiredPrivilegeIds()
     */
    @Override
    protected String[] getRequiredPrivilegeIds() {
        return new String[] {"std.bean.read"};
    }

    /**
     * Gets the supported nav ids.
     *
     * @return String[]
     * @see com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure#getSupportedNavIds()
     */
    @Override
    protected String[] getSupportedNavIds() {
        return null;
    }

    /**
     * Gets the supported views.
     *
     * @return Application context[]
     * @see com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure#getSupportedViews()
     */
    @Override
    protected ApplicationContext[] getSupportedViews() {
        return new ApplicationContext[] {ApplicationContext.ORGANIZATION_1,
                ApplicationContext.ORGANIZATION_2,
                ApplicationContext.ORGANIZATION_3,
                ApplicationContext.ORGANIZATION_4,
                ApplicationContext.ORGANIZATION_5,
                ApplicationContext.SCHOOL,
                ApplicationContext.STAFF};
    }

    /**
     * Gets the static parameters.
     *
     * @return Map
     */
    private Map<String, String> getStaticParameters() {
        HashMap<String, String> parameters = new HashMap<String, String>(6);

        parameters.put("token", "cne19a6abf1c334170acf29adb2ed03b3e");
        parameters.put("AuditInfo", "TEST");
        parameters.put("smstoken", "vc6ebb7f6df1644f4fbf7c374d103c2fc6");
        parameters.put("IpAddress", "173.251.125.34");
        parameters.put("Role", "");

        return parameters;
    }
}
