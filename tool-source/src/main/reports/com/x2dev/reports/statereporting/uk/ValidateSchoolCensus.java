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

package com.x2dev.reports.statereporting.uk;

import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
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
public class ValidateSchoolCensus extends ReportJavaSourceNet {
    /**
     * Param for the intermediate file xsl location on the system
     */
    private static final String PARAM_AGE_AT_DATE = "ageAtDate";

    private static final String PARAM_INTERMEDIATE_FILE = "intermediateFile";

    private static final String PARAM_INTERMEDIATE_AUT_FILE = "intermediateFileAUT";

    private static final String PARAM_INTERMEDIATE_PRU_FILE = "intermediateFilePRU";

    private static final String PARAM_TERM = "term";

    private static final String PARAM_PROCEDURE_ID = "procedureId";

    private static final String PARAM_CENSUS_DATE = "censusDate";

    private static final String PARAM_PREVIOUS_TERM_START_DATE = "previousTermStartDate";

    private static final String DEFAULT_AGE_AT_DATE = "2012-08-31";

    private static final String CENSUS_TERM_SPRING = "SPR";
    private static final String CENSUS_TERM_AUTUMN = "AUT";
    private static final String PUPIL_REFERRAL_UNIT = "PRU";

    /**
     * General Constants
     */
    private static final String STRING_ZERO = "0";
    private static final String STRING_FALSE = "false";
    private static final char CHAR_SPACE = ' ';
    /**
     * Parameter for term's start date
     */
    public static final String PARAM_TERM_START_DATE = "termStartDate";

    private Collection<StateReportValidationError> m_initErrors;

    private XMLStateReportData m_reportData;

    private PlainDate m_ageAtDate;

    /**
     * Census Date Parameter
     */
    private PlainDate m_censusDate;

    /**
     * Previous term start date
     */
    private PlainDate m_previousTermStartDate;

    /**
     * Selected term code
     */
    private String m_term;

    /**
     * Selected Intermediate File Location
     */
    private String m_intermediateFileLocation = null;

    /**
     * Selected Procedure Id
     */
    private String m_procedureId;

    /**
     * Term start date.
     *
     * @return Object
     * @throws Exception exception
     */
    // private PlainDate m_termStartDate;

    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        m_initErrors = new ArrayList<StateReportValidationError>();

        m_procedureId = (String) getParameter(PARAM_PROCEDURE_ID);
        m_term = (String) getParameter(PARAM_TERM);
        m_censusDate = (PlainDate) getParameter(PARAM_CENSUS_DATE);
        // m_termStartDate = (PlainDate) getParameter(PARAM_TERM_START_DATE);
        m_previousTermStartDate = (PlainDate) getParameter(PARAM_PREVIOUS_TERM_START_DATE);
        if (PUPIL_REFERRAL_UNIT.equals(m_term)) {
            m_intermediateFileLocation = (String) getParameter(PARAM_INTERMEDIATE_PRU_FILE);
        } else {
            if (CENSUS_TERM_AUTUMN.equals(m_term)) {
                m_intermediateFileLocation = (String) getParameter(PARAM_INTERMEDIATE_AUT_FILE);
            } else {
                m_intermediateFileLocation = (String) getParameter(PARAM_INTERMEDIATE_FILE);
            }
        }

        String ageAsOfDate = (String) getParameter(PARAM_AGE_AT_DATE);
        m_ageAtDate = DateUtils.getDate(ageAsOfDate);
        if (m_ageAtDate == null) {
            m_ageAtDate = DateUtils.getDate(DEFAULT_AGE_AT_DATE);
        }

        m_reportData = (XMLStateReportData) StateReportData.getReportDataFromProcedure(m_procedureId, getBroker(),
                m_initErrors);
        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setProcedureId(m_procedureId);

                m_reportData.initializeExport();
                m_reportData.postProcess();

                postExport();
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

                if (m_intermediateFileLocation != null) {
                    File intermediateFile = new File(m_intermediateFileLocation);
                    if (intermediateFile.exists()) {
                        XSLTransformer transformer = new XSLTransformer(intermediateFile);
                        Document validationDoc = transformer.transform(input);

                        Element validationRoot = validationDoc.getRootElement();

                        Element validationResults = validationRoot.getChild("ValidationResults");
                        Element summary = validationResults.getChild("Summary");
                        String schoolName = summary.getChild("SchoolName").getTextTrim();
                        String schoolPhase = summary.getChild("SchoolPhase").getTextTrim();
                        String pupilsOnRoll = summary.getChild("PupilsOnRoll").getTextTrim();
                        String pupilsNoLongerOnRoll = summary.getChild("PupilsNoLongerOnRoll").getTextTrim();
                        String classes = summary.getChild("Classes").getTextTrim();

                        Element headerElement = validationResults.getChild("Header");
                        if (headerElement != null) {
                            Element collectionDetailsElement = headerElement.getChild("CollectionDetails");
                            Element termElement = collectionDetailsElement.getChild("Term");
                            String reportTerm = termElement.getTextTrim();

                            addParameter("schoolName", schoolName);
                            addParameter("schoolPhase", schoolPhase);
                            addParameter("pupilsOnRoll", pupilsOnRoll);
                            addParameter("pupilsNoLongerOnRoll", pupilsNoLongerOnRoll);
                            addParameter("classes", classes);

                            Element errorsElement = validationRoot.getChild("Errors");
                            List<Element> errorElements = errorsElement.getChildren("Error");
                            int numErrors = 0;
                            int numQueries = 0;
                            for (Element element : errorElements) {
                                String termAsString = element.getChild("Term").getTextTrim();
                                ArrayList<String> termAsList =
                                        StringUtils.convertDelimitedStringToList(termAsString, CHAR_SPACE);
                                HashSet<String> termSet = new HashSet<String>(termAsList);
                                String location = element.getChild("Location").getTextTrim();
                                String message = element.getChild("Message").getTextTrim();
                                String type = element.getChild("Type").getTextTrim();
                                String phaseAsString = element.getChild("Phase").getTextTrim();
                                ArrayList<String> phaseAsList =
                                        StringUtils.convertDelimitedStringToList(phaseAsString, CHAR_SPACE);
                                HashSet<String> phaseSet = new HashSet<String>(phaseAsList);

                                // only report error if the report term and school phase match
                                if (phaseSet.contains(schoolPhase) && termSet.contains(reportTerm)) {
                                    grid.append();

                                    if ("Error".equals(type)) {
                                        numErrors++;
                                    } else if ("Query".equals(type)) {
                                        numQueries++;
                                    }

                                    grid.set("type", type);
                                    grid.set("sequence", element.getChild("Sequence").getTextTrim());
                                    grid.set("message", (location + " " + message).trim());
                                }
                            }

                            addParameter("numErrors", Integer.valueOf(numErrors));
                            addParameter("numQueries", Integer.valueOf(numQueries));

                            grid.sort("sequence", false);

                            grid.beforeTop();
                        }

                    } else {
                        grid.append();
                        grid.set("type", "");
                        grid.set("sequence", "");
                        grid.set("message", "ERROR: intermediateFileLocation: '" + m_intermediateFileLocation
                                + "' not found in the file system.");
                        grid.beforeTop();
                    }
                } else {
                    grid.append();
                    grid.set("type", "");
                    grid.set("sequence", "");
                    grid.set("message", "ERROR: 'intermediateFile' parameter not set.");
                    grid.beforeTop();
                }
            }
        }

        if (grid.getRows().size() == 0) {
            grid.append();
            grid.set("type", "");
            grid.set("sequence", "");
            grid.set("message", "No errors were found.");
            grid.beforeTop();
        }

        return grid;
    }

    /**
     * Performance some additional processing of the XML before export.
     *
     */
    private void postExport() {
        Element rootElement = m_reportData.getRootElement();

        Element pupilsElement = rootElement.getChild("Pupils");

        if (pupilsElement != null) {
            List<Element> listOfPupilOnRollElement = pupilsElement.getChild("PupilsOnRoll").getChildren("PupilOnRoll");

            for (Element pupilOnRollElement : listOfPupilOnRollElement) {
                filterPupil(pupilOnRollElement);
            }

            Element pupilsNoLongerOnRollElement = pupilsElement.getChild("PupilsNoLongerOnRoll");
            if (pupilsNoLongerOnRollElement != null) {
                List<Element> listOfPupilsNoLongerOnRollElement =
                        pupilsNoLongerOnRollElement.getChildren("PupilNoLongerOnRoll");
                List<Element> pupilsToRemove = new LinkedList<Element>();
                for (Element pupilNoLongerOnRollElement : listOfPupilsNoLongerOnRollElement) {
                    filterPupil(pupilNoLongerOnRollElement);
                    if (filter1925(pupilNoLongerOnRollElement)) {
                        pupilsToRemove.add(pupilNoLongerOnRollElement);
                    }
                }

                for (Element pupil : pupilsToRemove) {
                    pupilsNoLongerOnRollElement.removeContent(pupil);

                }
            }
        }
    }

    /**
     * ERROR 1925 filtering.
     *
     * @param pupilNoLongerOnRollElement Element
     * @return true, if successful
     */
    private boolean filter1925(Element pupilNoLongerOnRollElement) {
        // between 5 and 15
        Element dOBElement = pupilNoLongerOnRollElement.getChild("PupilIdentifiers").getChild("DOB");
        String dobAsString = null;
        if (dOBElement != null) {
            dobAsString = dOBElement.getTextTrim();
        }

        PlainDate dob = DateUtils.getDate(dobAsString);
        int ageAsOfDate = 0;
        if (dob != null) {
            ageAsOfDate = Person.getAgeAsOfDate(dob, m_ageAtDate);
        }

        boolean between4and15 = (4 <= ageAsOfDate && ageAsOfDate <= 15);

        String boarderText = pupilNoLongerOnRollElement.getChild("PupilStatus").getChild("Boarder").getTextTrim();
        boolean boarderN = boarderText.matches("N");

        // check for if "LeavingDate" is between census date and autumn start date
        Element pupilStatusElement = pupilNoLongerOnRollElement.getChild("PupilStatus");
        String leavingDateStr = null;
        String entryDateStr = null;
        PlainDate leavingDate = null;
        PlainDate entryDate = null;
        if (pupilStatusElement != null) {
            leavingDateStr = pupilStatusElement.getChild("LeavingDate").getTextTrim();
            leavingDate = DateUtils.getDate(leavingDateStr);

            entryDateStr = pupilStatusElement.getChild("EntryDate").getTextTrim();
            entryDate = DateUtils.getDate(entryDateStr);
        }
        boolean isEntryBeforeTermStart = false;
        if (m_censusDate != null && entryDate != null) {
            isEntryBeforeTermStart = entryDate.before(m_censusDate);
        }
        boolean isLeavingDateBetween = false;
        boolean leftPriorToCensusDate = false;
        if (m_censusDate != null && m_previousTermStartDate != null && leavingDate != null) {
            isLeavingDateBetween = m_censusDate.after(leavingDate) && leavingDate.after(m_previousTermStartDate);
            leftPriorToCensusDate = leavingDate.before(m_censusDate);
        }

        // check for the existence of the "LearnerSupport" element
        Element learnerSupportElement = pupilNoLongerOnRollElement.getChild("LearnerSupport");
        boolean hasLearnerSupport = learnerSupportElement != null;

        // exclusions are also removed from the template if there aren't any
        Element exclusionsElement = pupilNoLongerOnRollElement.getChild("Exclusions");
        boolean hasExclusions = exclusionsElement != null;
        // check to see if attendance records exist
        Element attendanceElement = pupilNoLongerOnRollElement.getChild("Attendance");
        boolean hasAttendance = true;
        if (attendanceElement != null) {
            Element termlyAttendance = attendanceElement.getChild("TermlyAttendance");
            if (termlyAttendance != null) {
                String sessionsPossible = termlyAttendance.getChild("SessionsPossible").getTextTrim();

                if (STRING_ZERO.equals(sessionsPossible)) {
                    hasAttendance = false;
                }
            }

            Element annualAttendance = attendanceElement.getChild("AnnualAttendance");
            if (annualAttendance != null) {
                String sessionsPossible = annualAttendance.getChild("SessionsPossible").getTextTrim();

                if (STRING_ZERO.equals(sessionsPossible)) {
                    hasAttendance = false;
                }
            }

            Element summerAttendance = attendanceElement.getChild("SummerHalfTerm2Attendance");
            if (summerAttendance != null) {
                String sessionsPossible = summerAttendance.getChild("SessionsPossible").getTextTrim();

                if (STRING_ZERO.equals(sessionsPossible)) {
                    hasAttendance = false;
                }
            }
        } else {
            hasAttendance = false;
        }

        return !(leftPriorToCensusDate && (hasExclusions || hasAttendance)) || !(isEntryBeforeTermStart
                && isLeavingDateBetween && between4and15 && (boarderN || hasLearnerSupport));
    }

    /**
     * Filter pupil.
     *
     * @param pupilElement Element
     */
    private void filterPupil(Element pupilElement) {
        Element dOBElement = pupilElement.getChild("PupilIdentifiers").getChild("DOB");
        String dobAsString = null;
        if (dOBElement != null) {
            dobAsString = dOBElement.getTextTrim();
        }

        PlainDate dob = DateUtils.getDate(dobAsString);

        String boarderText = pupilElement.getChild("PupilStatus").getChild("Boarder").getTextTrim();
        Element pupilExclusions = pupilElement.getChild("Exclusions");

        if (pupilExclusions != null) {
            List<Element> termlyExclusions = pupilExclusions.getChildren("TermlyExclusion");
            List<Element> termlyExclusions2Delete = new ArrayList<Element>();
            if (termlyExclusions != null) {
                for (Element termlyExclusion : termlyExclusions) {
                    String categoryAsString = termlyExclusion.getChild("Category").getTextTrim();

                    if (StringUtils.isEmpty(categoryAsString)) {
                        termlyExclusions2Delete.add(termlyExclusion);
                    }
                }

                if (termlyExclusions2Delete.size() > 0) {
                    for (Element termlyExclusion2Delete : termlyExclusions2Delete) {
                        pupilElement.getChild("Exclusions").removeContent(termlyExclusion2Delete);
                    }
                }
            }

            if (pupilExclusions.getChildren("TermlyExclusion") == null
                    || pupilExclusions.getChildren("TermlyExclusion").isEmpty()) {
                pupilElement.removeChild("Exclusions");
            }
        }

        int ageAsOfDate = 0;
        if (dob != null) {
            ageAsOfDate = Person.getAgeAsOfDate(dob, m_ageAtDate);

            if ((ageAsOfDate < 4 || ageAsOfDate > 15) || boarderText.matches("B")) {
                /*
                 * 2470
                 *
                 * Data items <SessionsPossible> (100228), <SessionsAuthorised> (100230),
                 * <SessionsUnauthorised> (100231), <AttendanceReason> (100481) or
                 * <AbsenceSessions> (100233) must not be provided if ages
                 * are <5 and >15 and where <Boarder> (100067) is B, 6 or 7
                 */
                pupilElement.removeChild("Attendance");
            }

            if (ageAsOfDate >= 5) {
                /*
                 * 1851, 1854
                 */
                pupilElement.getChild("PupilCharacteristics").removeChild("FundedHours");
                pupilElement.getChild("PupilCharacteristics").removeChild("HoursAtSetting");
            }

            Element specialNeedsElement = pupilElement.getChild("SpecialEducationalNeeds");
            if (specialNeedsElement != null && m_term.equals(CENSUS_TERM_SPRING)) {
                String senProvision = specialNeedsElement.getChild("SENprovision").getTextTrim();
                Element resourcedProvisionIndicatorElement;
                if (!StringUtils.isEmpty(senProvision) && senProvision.matches("A|N")) {
                    specialNeedsElement.removeChild("SENneeds");
                    specialNeedsElement.getChild("SENunitIndicator").setText(STRING_ZERO);
                    resourcedProvisionIndicatorElement = specialNeedsElement.getChild("ResourcedProvisionIndicator");
                    if (resourcedProvisionIndicatorElement != null) {
                        resourcedProvisionIndicatorElement.setText(STRING_FALSE);
                    }
                }

                resourcedProvisionIndicatorElement = specialNeedsElement.getChild("ResourcedProvisionIndicator");
                if (resourcedProvisionIndicatorElement != null) {
                    String resourcedProvisionIndicator = resourcedProvisionIndicatorElement.getTextTrim();
                    if (resourcedProvisionIndicator.equals("N")) {
                        resourcedProvisionIndicatorElement.setText(STRING_FALSE);
                    }
                }
            }

            /*
             * if (m_term.equals("AUT"))
             * {
             * //Remove any LearningAims that have a Start Date after the Census Date or null
             * Element learningAimsElement = pupilElement.getChild("LearningAims");
             * if (learningAimsElement != null)
             * {
             * List<Element> learningAimElements = learningAimsElement.getChildren("LearningAim");
             * List<Element> learningAimElementsNewList = new ArrayList<Element>();
             * 
             * for (Element learningAimElement : learningAimElements)
             * {
             * boolean keep = false;
             * 
             * Element learningStartDateElement = learningAimElement.getChild("LearningStartDate");
             * String learningStartDateStr = null;
             * if (learningStartDateElement != null)
             * {
             * learningStartDateStr = learningStartDateElement.getTextTrim();
             * if (!StringUtils.isEmpty(learningStartDateStr))
             * {
             * PlainDate startDate = null;
             * try
             * {
             * startDate = new PlainDate(formatter.parse(learningStartDateStr));
             * }
             * catch (ParseException e)
             * {
             * e.printStackTrace();
             * }
             * 
             * if (startDate.before(m_censusDate))
             * {
             * keep = true;
             * }
             * }
             * }
             * 
             * if (keep)
             * {
             * Element learningAimElementClone = (Element) learningAimElement.clone();
             * learningAimElementClone.detach();
             * learningAimElementsNewList.add(learningAimElementClone);
             * }
             * }
             * 
             * pupilElement.removeChild("LearningAims");
             * if (learningAimElementsNewList.size() > 0)
             * {
             * Element learningAimsElementNew = new Element("LearningAims");
             * for (Element learningAimElementNew : learningAimElementsNewList)
             * {
             * learningAimsElementNew.addContent((Element) learningAimElementNew.clone());
             * }
             * pupilElement.addContent(learningAimsElementNew);
             * }
             * }
             * }
             */
        }

        Element studentAttendance = pupilElement.getChild("Attendance");
        if (studentAttendance != null) {
            Element termlyAttendance = studentAttendance.getChild("TermlyAttendance");
            boolean termlyExist = true;
            if (termlyAttendance != null) {
                String sessionsPossible = termlyAttendance.getChildTextTrim("SessionsPossible");
                Element sessionDetails = termlyAttendance.getChild("SessionDetails");

                if (StringUtils.isEmpty(sessionsPossible) || STRING_ZERO.equals(sessionsPossible)) {
                    studentAttendance.removeChild("TermlyAttendance");
                    termlyExist = false;
                    if (!CENSUS_TERM_AUTUMN.equals(m_term)) {
                        pupilElement.removeChild("Attendance");
                    }
                } else if (sessionDetails != null) {
                    Collection<Element> sessionDetailList = sessionDetails.getChildren();

                    if (sessionDetailList != null) {
                        Collection<Element> sessionDetailsFiltered = new ArrayList<Element>();
                        for (Element sessionDetail : sessionDetailList) {
                            String absenceSessions = sessionDetail.getChildTextTrim("AbsenceSessions");

                            if (!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions)) {
                                sessionDetailsFiltered.add(sessionDetail);
                            }
                        }

                        sessionDetails.removeChildren("SessionDetail");
                        pupilElement.getChild("Attendance").getChild("TermlyAttendance").getChild("SessionDetails")
                                .setContent(sessionDetailsFiltered);
                        sessionDetails = termlyAttendance.getChild("SessionDetails");
                    }

                    if (!sessionDetails.getChildren().isEmpty()) {
                        pupilElement.getChild("Attendance").getChild("TermlyAttendance")
                                .removeChild("SessionsAuthorised");
                        pupilElement.getChild("Attendance").getChild("TermlyAttendance")
                                .removeChild("SessionsUnauthorised");
                    } else {
                        pupilElement.getChild("Attendance").getChild("TermlyAttendance").removeChild("SessionDetails");
                    }
                }
            }

            Element summerAttendance = studentAttendance.getChild("SummerHalfTerm2Attendance");
            boolean summerExist = true;
            if (summerAttendance != null) {
                if (!CENSUS_TERM_AUTUMN.equals(m_term)) {
                    studentAttendance.removeChild("SummerHalfTerm2Attendance");
                    summerExist = false;
                } else {
                    String sessionsPossible = summerAttendance.getChildTextTrim("SessionsPossible");
                    Element sessionDetails = summerAttendance.getChild("SessionDetails");

                    if (StringUtils.isEmpty(sessionsPossible) || STRING_ZERO.equals(sessionsPossible)) {
                        studentAttendance.removeChild("SummerHalfTerm2Attendance");
                        summerExist = false;
                    } else if (sessionDetails != null) {
                        Collection<Element> sessionDetailList = sessionDetails.getChildren();

                        if (sessionDetailList != null) {
                            Collection<Element> sessionDetailsFiltered = new ArrayList<Element>();
                            for (Element sessionDetail : sessionDetailList) {
                                String absenceSessions = sessionDetail.getChildTextTrim("AbsenceSessions");

                                if (!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions)) {
                                    sessionDetailsFiltered.add(sessionDetail);
                                }
                            }

                            sessionDetails.removeChildren("SessionDetail");
                            pupilElement.getChild("Attendance").getChild("SummerHalfTerm2Attendance")
                                    .getChild("SessionDetails").setContent(sessionDetailsFiltered);
                            sessionDetails = summerAttendance.getChild("SessionDetails");
                        }

                        if (!sessionDetails.getChildren().isEmpty()) {
                            pupilElement.getChild("Attendance").getChild("SummerHalfTerm2Attendance")
                                    .removeChild("SessionsAuthorised");
                            pupilElement.getChild("Attendance").getChild("SummerHalfTerm2Attendance")
                                    .removeChild("SessionsUnauthorised");
                        } else {
                            pupilElement.getChild("Attendance").getChild("SummerHalfTerm2Attendance")
                                    .removeChild("SessionDetails");
                        }
                    }
                }
            }

            Element annualAttendance = studentAttendance.getChild("AnnualAttendance");
            if (annualAttendance != null) {
                if (CENSUS_TERM_AUTUMN.equals(m_term) || PUPIL_REFERRAL_UNIT.equals(m_term)) {
                    String sessionsPossible = annualAttendance.getChildTextTrim("SessionsPossible");
                    Element sessionDetails = annualAttendance.getChild("SessionDetails");

                    if (StringUtils.isEmpty(sessionsPossible) || STRING_ZERO.equals(sessionsPossible)) {
                        studentAttendance.removeChild("AnnualAttendance");
                    } else if (sessionDetails != null) {
                        Collection<Element> sessionDetailList = sessionDetails.getChildren();

                        if (sessionDetailList != null) {
                            Collection<Element> sessionDetailsFiltered = new ArrayList<Element>();
                            for (Element sessionDetail : sessionDetailList) {
                                String absenceSessions = sessionDetail.getChildTextTrim("AbsenceSessions");

                                if (!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions)) {
                                    sessionDetailsFiltered.add(sessionDetail);
                                }
                            }

                            sessionDetails.removeChildren("SessionDetail");
                            pupilElement.getChild("Attendance").getChild("AnnualAttendance").getChild("SessionDetails")
                                    .setContent(sessionDetailsFiltered);
                            sessionDetails = annualAttendance.getChild("SessionDetails");
                        }

                        if (!sessionDetails.getChildren().isEmpty()) {
                            pupilElement.getChild("Attendance").getChild("AnnualAttendance")
                                    .removeChild("SessionsAuthorised");
                            pupilElement.getChild("Attendance").getChild("AnnualAttendance")
                                    .removeChild("SessionsUnauthorised");
                        } else {
                            pupilElement.getChild("Attendance").getChild("AnnualAttendance")
                                    .removeChild("SessionDetails");
                        }
                    }
                } else {
                    studentAttendance.removeChild("AnnualAttendance");
                }
            }

            if (!termlyExist && !summerExist && CENSUS_TERM_AUTUMN.equals(m_term)) {
                pupilElement.removeChild("Attendance");
            }
        }
    }
}
