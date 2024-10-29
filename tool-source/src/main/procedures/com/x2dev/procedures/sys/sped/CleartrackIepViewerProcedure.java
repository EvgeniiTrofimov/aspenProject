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

import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebConstants;
import java.io.IOException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import org.apache.commons.lang3.StringUtils;

/**
 * PageOutputProcedure that displays an iframe containing a document from Cleartrack IEP direct. The
 * URL
 * is in the format:
 *
 * [base URL]&User=[username]&District=[districtId]&StudentID=[studentId]
 *
 * The user must authenticate with Cleartrack when the window opens.
 *
 * A field with the alias "Cleartrack District ID" must be defined on the Organization and contain a
 * district ID
 * as defined in the cleartrack system.
 *
 * @author mmastrangelo
 */
public class CleartrackIepViewerProcedure extends PageOutputProcedure {
    private static final String VIEWER_BASE_URL = "https://www.cleartrackrti.com/scripts/test.wsc/smspdf.r";
    // private static final String VIEWER_URL =
    // "https://cleartrack1.esboces.org/Scripts/cltkweb1.wsc/smspdf.r";
    private static final String SMS_ID = "x4m79dw2p";

    private static final String CLEARTRACK_DISTRICT_ID_ALIAS = "Cleartrack District ID";

    /**
     * Gets the window title.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.procedures.PageOutputProcedure#getWindowTitle()
     */
    @Override
    public String getWindowTitle() {
        return "ClearTrack IEP/504";
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
        String districtId = (String) userData.getRootOrganization().getFieldValueByAlias(CLEARTRACK_DISTRICT_ID_ALIAS);

        if (StringUtils.isEmpty(districtId)) {
            writer.print("<div style=\"text-align: left\"><h2>Unable to locate the ClearTrack District ID</h2></div>");
        } else {
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
                String fullUrl = VIEWER_BASE_URL + "?SMSId=" + SMS_ID + "&User=" + userData.getUser().getLoginName()
                        + "&District=" + districtId + "&StudentID=" + studentId;

                writer.print("<iframe src=\"");
                writer.print(fullUrl);
                writer.print("\" width=\"98%\" height=\"98%\"></iframe>");
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
}
