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
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.text.SimpleDateFormat;
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
public class ValidateChildInNeedCensus extends ReportJavaSourceNet {
    /**
     * Import Definition Parameters
     */
    private static final String PARAM_PROCEDURE_ID = "procedureId";
    private static final String PARAM_CENSUS_DATE = "censusDate";
    private static final String PARAM_INTERMEDIATE_FILE = "intermediateFile";

    /**
     * Report Parameters
     */
    private static final String RPT_PARAM_PROGRAM_XSLT_VERSION = "xSLTVersion";
    private static final String RPT_PARAM_CENSUS_DATE = "censusDate";
    private static final String RPT_PARAM_REFERENCE_DATE = "referenceDate";
    private static final String RPT_PARAM_LA_CODE = "lACode";
    private static final String RPT_PARAM_NO_OF_CHILDREN_IN_CENSUS = "noOfChildrenInCensus";
    private static final String RPT_PARAM_NUM_ERRORS = "numErrors";
    private static final String RPT_PARAM_NUM_QUERIES = "numQueries";

    /**
     * XML Export Elements
     */
    private static final String ELEMENT_PROGRAM_XSLT_VERSION = "XSLTVersion";
    private static final String ELEMENT_VALIDATION_RESULTS = "ValidationResults";
    private static final String ELEMENT_SUMMARY = "Summary";
    private static final String ELEMENT_REFERENCE_DATE = "ReferenceDate";
    private static final String ELEMENT_LA_CODE = "LAcode";
    private static final String ELEMENT_NO_OF_CHILDREN_IN_CENSUS = "NoOfChildrenInCensus";
    private static final String ELEMENT_ERRORS = "Errors";
    private static final String ELEMENT_ERROR = "Error";
    private static final String ELEMENT_TYPE = "Type";
    private static final String ELEMENT_SEQUENCE = "Sequence";
    private static final String ELEMENT_LOCATION = "Location";
    private static final String ELEMENT_MESSAGE = "Message";
    private static final String ELEMENT_ERROR_TYPE_ERROR = "Error";
    private static final String ELEMENT_ERROR_TYPE_QUERY = "Query";

    /**
     * Report Grid Fields
     */
    private static final String GRID_FIELD_TYPE = "type";
    private static final String GRID_FIELD_SEQUENCE = "sequence";
    private static final String GRID_FIELD_MESSAGE = "message";
    private static final String GRID_SORT_SEQUENCE = "sequence";

    private Collection<StateReportValidationError> m_initErrors;

    private XMLStateReportData m_reportData;

    private PlainDate m_censusDate;

    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");


    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        m_initErrors = new ArrayList<StateReportValidationError>();
        String procedureId = (String) getParameter(PARAM_PROCEDURE_ID);

        m_censusDate = (PlainDate) getParameter(PARAM_CENSUS_DATE);

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
                m_reportData.postProcess();
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
                Element xSLTVersionElement = validationRoot.getChild(ELEMENT_PROGRAM_XSLT_VERSION);
                String xSLTVersion = xSLTVersionElement.getTextTrim();
                addParameter(RPT_PARAM_PROGRAM_XSLT_VERSION, xSLTVersion);

                Element validationResultsElement = validationRoot.getChild(ELEMENT_VALIDATION_RESULTS);
                Element summaryElement = validationResultsElement.getChild(ELEMENT_SUMMARY);
                String referenceDate = summaryElement.getChild(ELEMENT_REFERENCE_DATE).getTextTrim();
                String lAcode = summaryElement.getChild(ELEMENT_LA_CODE).getTextTrim();
                String noOfChildrenInCensus = summaryElement.getChild(ELEMENT_NO_OF_CHILDREN_IN_CENSUS).getTextTrim();

                addParameter(RPT_PARAM_CENSUS_DATE, m_dateFormat.format(m_censusDate));
                addParameter(RPT_PARAM_REFERENCE_DATE, referenceDate);
                addParameter(RPT_PARAM_LA_CODE, lAcode);
                addParameter(RPT_PARAM_NO_OF_CHILDREN_IN_CENSUS, noOfChildrenInCensus);

                Element errorsElement = validationRoot.getChild(ELEMENT_ERRORS);
                if (errorsElement != null) {
                    List<Element> errorElements = errorsElement.getChildren(ELEMENT_ERROR);
                    int numErrors = 0;
                    int numQueries = 0;
                    for (Element element : errorElements) {
                        String type = element.getChild(ELEMENT_TYPE).getTextTrim();
                        String sequence = element.getChild(ELEMENT_SEQUENCE).getTextTrim();
                        String location = element.getChild(ELEMENT_LOCATION).getTextTrim();
                        String message = element.getChild(ELEMENT_MESSAGE).getTextTrim();

                        grid.append();

                        if (ELEMENT_ERROR_TYPE_ERROR.equals(type)) {
                            numErrors++;
                        } else if (ELEMENT_ERROR_TYPE_QUERY.equals(type)) {
                            numQueries++;
                        }

                        grid.set(GRID_FIELD_TYPE, type);
                        grid.set(GRID_FIELD_SEQUENCE, sequence);
                        grid.set(GRID_FIELD_MESSAGE, (location + " " + message).trim());
                    }

                    addParameter(RPT_PARAM_NUM_ERRORS, Integer.valueOf(numErrors));
                    addParameter(RPT_PARAM_NUM_QUERIES, Integer.valueOf(numQueries));

                    grid.sort(GRID_SORT_SEQUENCE, false);

                    grid.beforeTop();
                }
            }
        }

        return grid;
    }

}
