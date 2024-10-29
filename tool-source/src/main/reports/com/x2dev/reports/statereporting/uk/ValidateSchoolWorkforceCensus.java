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
package com.x2dev.reports.statereporting.uk;

import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.utils.X2BaseException;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.jdom.transform.XSLTransformer;

/**
 * Validates the School Census report.
 *
 * @author Follett Software Company
 */
public class ValidateSchoolWorkforceCensus extends ReportJavaSourceNet {

    /**
     * Param for the intermediate file xsl location on the system
     */
    private static final String PARAM_INTERMEDIATE_FILE = "intermediateFile";

    /**
     * Param for the procedure id for the export to validate
     */
    private static final String PARAM_PROCEDURE_ID = "procedureId";

    private Collection<StateReportValidationError> m_initErrors;

    private XMLStateReportData m_reportData;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        m_initErrors = new ArrayList<StateReportValidationError>();
        String procedureId = (String) getParameter(PARAM_PROCEDURE_ID);

        m_reportData =
                (XMLStateReportData) StateReportData.getReportDataFromProcedure(procedureId, getBroker(), m_initErrors);
        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setProcedureId(procedureId);

                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }

        if (m_reportData != null) {
            Element root = m_reportData.getRootElement();

            if (root != null) {
                Document input = new Document(root);

                XMLOutputter outputter = new XMLOutputter();
                outputter.setFormat(Format.getPrettyFormat());

                String intermediateFileLocation = (String) getParameter(PARAM_INTERMEDIATE_FILE);

                File intermediateFile = new File(intermediateFileLocation);
                XSLTransformer transformer = new XSLTransformer(intermediateFile);
                Document validationDoc = transformer.transform(input);

                Element validationRoot = validationDoc.getRootElement();
                String xSLTVersion = validationRoot.getChild("XSLTVersion").getTextTrim();
                addParameter("xSLTVersion", xSLTVersion);

                Element validationResults = validationRoot.getChild("ValidationResults");
                Element header = validationResults.getChild("Header");

                Element collectionDetails = header.getChild("CollectionDetails");
                String censusDate = collectionDetails.getChild("ReferenceDate").getTextTrim();
                addParameter("censusDate", censusDate);

                Element source = header.getChild("Source");
                String lEA = source.getChild("LEA").getTextTrim();
                addParameter("schoolRef", lEA);

                Element summary = validationResults.getChild("Summary");
                String usualTeacher = summary.getChild("UsualTeacher").getTextTrim();
                String agencyTeacher = summary.getChild("AgencyTeacher").getTextTrim();
                String teachingAssistant = summary.getChild("TeachingAssistant").getTextTrim();
                String otherSupport = summary.getChild("OtherSupport").getTextTrim();
                String noContract = summary.getChild("NoContract").getTextTrim();

                addParameter("usualTeacher", usualTeacher);
                addParameter("agencyTeacher", agencyTeacher);
                addParameter("teachingAssistant", teachingAssistant);
                addParameter("otherSupport", otherSupport);
                addParameter("noContract", noContract);

                Element errorsElement = validationRoot.getChild("Errors");
                List<Element> errorElements = errorsElement.getChildren("Error");
                int numErrors = 0;
                int numQueries = 0;
                for (Element element : errorElements) {
                    String type = element.getChild("Type").getTextTrim();
                    String sequence = element.getChild("Sequence").getTextTrim();
                    String location = element.getChild("Location").getTextTrim();
                    String message = element.getChild("Message").getTextTrim();

                    grid.append();

                    if ("Error".equals(type)) {
                        numErrors++;
                    } else if ("Query".equals(type)) {
                        numQueries++;
                    }

                    grid.set("type", type);
                    grid.set("sequence", sequence);
                    grid.set("message", (location + " " + message).trim());
                }

                addParameter("numErrors", Integer.valueOf(numErrors));
                addParameter("numQueries", Integer.valueOf(numQueries));

                grid.sort("sequence", false);

                grid.beforeTop();
            }
        }

        return grid;
    }

}
