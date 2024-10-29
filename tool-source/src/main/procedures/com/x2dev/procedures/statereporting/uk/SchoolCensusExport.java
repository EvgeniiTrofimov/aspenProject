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

import static com.follett.fsc.core.k12.tools.exports.ExportJavaSource.FORMAT_EOL_WINDOWS;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * This is a report class that performs standardized data export for
 * the state report infrastructure.
 * <p>
 * This class will identify a procedure that contains a state report definition.
 * It will use that definition to find all data definitions and produce
 * an export file.
 * <p>
 * This is enhanced to support XML exports.
 * <p>
 * <strong>LIMITATION:</strong> This format cannot currently save results to the export result row
 * table.
 *
 * @author Follett Software Company
 *
 */
public class SchoolCensusExport extends ToolJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String DEBUG_OPTION_OFF = "off";
    private static final String DEBUG_OPTION_SUMMARY = "summary";
    private static final String INITIALIZE_KEY = "label.state.report.initialize";

    /**
     * Parameters
     */
    private static final String PARAM_AGE_AT_DATE = "ageAtDate";
    private static final String PARAM_CENSUS_DATE = "censusDate";
    private static final String PARAM_PREVIOUS_TERM_START_DATE = "previousTermStartDate";
    private static final String PARAM_PROCEDURE_ID = "procedureId";
    private static final String PARAM_TERM = "term";
    private static final String PARAM_VALIDATION_MODE = "validationMode";

    private static final String CENSUS_TERM_SPRING = "SPR";
    private static final String CENSUS_TERM_AUTUMN = "AUT";
    private static final String PUPIL_REFERRAL_UNIT = "PRU";

    private static final String DEFAULT_AGE_AT_DATE = "2012-08-31";

    /**
     * General Constants
     */
    private static final String STRING_ZERO = "0";
    private static final String STRING_FALSE = "false";

    private PlainDate m_ageAtDate;
    private PlainDate m_censusDate;
    private PlainDate m_previousTermStartDate;
    private XMLStateReportData m_reportData = null;
    private String m_term;
    private String m_validationMode;

    private int m_totalSessionsPossible;
    private int m_totalSessionsAuthorised;
    private int m_totalSessionsUnauthorised;

    /**
     * List of errors the export may have
     */
    private Collection<StateReportValidationError> m_initErrors = null;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_totalSessionsPossible = 0;
        m_totalSessionsAuthorised = 0;
        m_totalSessionsUnauthorised = 0;

        String procedureId = (String) getParameter(PARAM_PROCEDURE_ID);

        String ageAsOfDate = (String) getParameter(PARAM_AGE_AT_DATE);
        m_ageAtDate = DateUtils.getDate(ageAsOfDate);
        if (m_ageAtDate == null) {
            m_ageAtDate = DateUtils.getDate(DEFAULT_AGE_AT_DATE);
        }

        m_censusDate = (PlainDate) getParameter(PARAM_CENSUS_DATE);
        m_initErrors = new ArrayList<StateReportValidationError>();
        m_previousTermStartDate = (PlainDate) getParameter(PARAM_PREVIOUS_TERM_START_DATE);
        m_term = (String) getParameter(PARAM_TERM);

        m_validationMode = (String) getParameter(PARAM_VALIDATION_MODE);
        if (StringUtils.isEmpty(m_validationMode)) {
            m_validationMode = DEBUG_OPTION_OFF;
        }

        // Lookup State report source data procedure
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
                m_reportData.setDebug(!DEBUG_OPTION_OFF.equals(m_validationMode));

                m_reportData.initializeExport();

                postExport();
            } catch (X2BaseException x2be) {
                String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                        .getMessage(INITIALIZE_KEY);
                m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, x2be.getMessage()));

                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
    }

    /**
     * Clean up the final XML output.
     */
    private void postExport() {
        Element rootElement = m_reportData.getRootElement();

        Element pupilsElement = rootElement.getChild("Pupils");
        if (pupilsElement != null) {
            // Filter Pupils On Roll
            Element pupilsOnRollElement = pupilsElement.getChild("PupilsOnRoll");
            List<Element> listOfPupilOnRollElement = pupilsOnRollElement.getChildren("PupilOnRoll");

            for (Element pupilOnRollElement : listOfPupilOnRollElement) {
                filterPupil(pupilOnRollElement);
            }

            Element pupilsNoLongerOnRollElement = pupilsElement.getChild("PupilsNoLongerOnRoll");
            if (pupilsNoLongerOnRollElement != null) {
                List<Element> listOfPupilsNoLongerOnRollElement =
                        pupilsNoLongerOnRollElement.getChildren("PupilNoLongerOnRoll");
                List<Element> pupilsToRemove = new LinkedList<Element>();

                // Filter Pupils No Longer On Roll
                // TODO Test this
                for (Element pupilNoLongerOnRollElement : listOfPupilsNoLongerOnRollElement) {
                    filterPupil(pupilNoLongerOnRollElement);
                    /*
                     * if (filter1925(pupilNoLongerOnRollElement))
                     * {
                     * pupilsToRemove.add(pupilNoLongerOnRollElement);
                     * }
                     */
                }

                for (Element pupil : pupilsToRemove) {
                    pupilsNoLongerOnRollElement.removeContent(pupil);
                }

                for (Element pupilOnRollElement : listOfPupilOnRollElement) {
                    runAttendanceCalculations(pupilOnRollElement);
                }

                for (Element pupilNoLongerOnRollElement : listOfPupilsNoLongerOnRollElement) {
                    runAttendanceCalculations(pupilNoLongerOnRollElement);
                }

                AppGlobals.getLog().severe("totalSessionsPossible : " + m_totalSessionsPossible);
                AppGlobals.getLog().severe("total authorised absences : " + m_totalSessionsAuthorised);
                AppGlobals.getLog().severe("total unauthorised absences : " + m_totalSessionsUnauthorised);
                // for (Element pupilNoLongerOnRollElement : listOfPupilsNoLongerOnRollElement)
                // {
                // String upn =
                // pupilNoLongerOnRollElement.getChild("PupilIdentifiers").getChild("UPN").getTextTrim();
                // String surname =
                // pupilNoLongerOnRollElement.getChild("PupilIdentifiers").getChild("Surname").getTextTrim();
                // String forename =
                // pupilNoLongerOnRollElement.getChild("PupilIdentifiers").getChild("Forename").getTextTrim();
                //
                // X2Criteria criteria = new X2Criteria();
                // if (!StringUtils.isEmpty(upn))
                // {
                // criteria.addEqualTo(SisStudent.COL_STATE_ID, upn);
                // }
                // else
                // {
                // criteria.addEqualTo(SisStudent.REL_PERSON+"."+Person.COL_FIRST_NAME, forename);
                // criteria.addEqualTo(SisStudent.REL_PERSON+"."+Person.COL_LAST_NAME, surname);
                // }
                //
                // QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
                // SisStudent student = (SisStudent) getBroker().getBeanByQuery(query);
                //
                // if (student != null)
                // {
                // student.setFieldB052("1");
                // getBroker().saveBean(student);
                // }
                // }
            }
        }
    }

    /**
     * ERROR 1925 filtering.
     *
     * @param pupilNoLongerOnRollElement Element
     * @return boolean
     */
    @SuppressWarnings("unused")
    private boolean filter1925(Element pupilNoLongerOnRollElement) {
        // entry

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

        boolean filter1925Flag = false;

        Element pupilStatusElement = pupilNoLongerOnRollElement.getChild("PupilStatus");
        if (pupilStatusElement != null) {
            Element boarderElement = pupilStatusElement.getChild("Boarder");
            String boarderText = "";
            if (boarderElement != null) {
                boarderText = boarderElement.getTextTrim();
            }
            boolean boarderN = boarderText.matches("N");

            // check for if "LeavingDate" is between census date and autumn start date
            // Element pupilStatusElement = pupilNoLongerOnRollElement.getChild("PupilStatus");
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

            filter1925Flag = !(leftPriorToCensusDate && (hasExclusions || hasAttendance)) || !(isEntryBeforeTermStart
                    && isLeavingDateBetween && between4and15 && (boarderN || hasLearnerSupport));
        }

        return filter1925Flag;
    }

    /**
     * Filter Pupil.
     *
     * @param pupilElement Element
     */
    private void filterPupil(Element pupilElement) {
        // String upn = pupilElement.getChild("PupilIdentifiers").getChild("UPN").getTextTrim();
        // String surname =
        // pupilElement.getChild("PupilIdentifiers").getChild("Surname").getTextTrim();
        // String forename =
        // pupilElement.getChild("PupilIdentifiers").getChild("Forename").getTextTrim();
        //
        // X2Criteria criteria = new X2Criteria();
        // if (!StringUtils.isEmpty(upn))
        // {
        // criteria.addEqualTo(SisStudent.COL_STATE_ID, upn);
        // }
        // else
        // {
        // criteria.addEqualTo(SisStudent.REL_PERSON+"."+Person.COL_FIRST_NAME, forename);
        // criteria.addEqualTo(SisStudent.REL_PERSON+"."+Person.COL_LAST_NAME, surname);
        // }
        //
        // QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        // SisStudent student = (SisStudent) getBroker().getBeanByQuery(query);
        //
        // if (student != null)
        // {
        // student.setFieldB052("1");
        // getBroker().saveBean(student);
        // }

        Element dOBElement = pupilElement.getChild("PupilIdentifiers").getChild("DOB");
        String dobAsString = null;
        if (dOBElement != null) {
            dobAsString = dOBElement.getTextTrim();
        }

        // int attendanceAgeYear = Integer.parseInt(dobAsString.substring(0, 4));
        // int attendanceAgeMonth = Integer.parseInt(dobAsString.substring(5, 7));
        // int beforeAugustBooleanInt = 0;
        // if (attendanceAgeMonth > 8)
        // {
        // beforeAugustBooleanInt = 0;
        // }
        //
        // int attendanceAge = 2013 - attendanceAgeYear - beforeAugustBooleanInt - 1;
        // if ((attendanceAge >= 5) && (attendanceAge <= 15))
        // {
        // String boarderText =
        // pupilElement.getChild("PupilStatus").getChild("Boarder").getTextTrim();
        // if (boarderText != null && "N".equals(boarderText))
        // {
        // Element studentAttendance = pupilElement.getChild("Attendance");
        // if (studentAttendance != null)
        // {
        // ArrayList<String> authorisedCodes = new ArrayList<String>();
        // authorisedCodes.add("I");
        // authorisedCodes.add("M");
        // authorisedCodes.add("R");
        // authorisedCodes.add("S");
        // authorisedCodes.add("T");
        // authorisedCodes.add("H");
        // authorisedCodes.add("F");
        // authorisedCodes.add("E");
        // authorisedCodes.add("C");
        //
        // ArrayList<String> unAuthorisedCodes = new ArrayList<String>();
        // unAuthorisedCodes.add("G");
        // unAuthorisedCodes.add("U");
        // unAuthorisedCodes.add("O");
        // unAuthorisedCodes.add("N");
        //
        // // termly attendance calculation
        // Element termlyAttendance = studentAttendance.getChild("TermlyAttendance");
        // if (termlyAttendance != null)
        // {
        // String sessionsPossibleAsString = termlyAttendance.getChildTextTrim("SessionsPossible");
        // int sessionsPossible = 0;
        // if (!StringUtils.isEmpty(sessionsPossibleAsString))
        // {
        // sessionsPossible = Integer.parseInt(sessionsPossibleAsString);
        //
        // if (sessionsPossible > 0)
        // {
        // m_totalSessionsPossible += sessionsPossible;
        //
        // Element sessionDetails = termlyAttendance.getChild("SessionDetails");
        // if (sessionDetails != null)
        // {
        // Collection<Element> sessionDetailList = sessionDetails.getChildren();
        //
        // if (sessionDetailList != null)
        // {
        // for (Element sessionDetail : sessionDetailList)
        // {
        // String absenceSessions = sessionDetail.getChildTextTrim("AbsenceSessions");
        // String absenceReason = sessionDetail.getChildTextTrim("AttendanceReason");
        //
        // if ((!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions)) &&
        // (!StringUtils.isEmpty(absenceReason) && authorisedCodes.contains(absenceReason)))
        // {
        // m_totalSessionsAuthorised += Integer.parseInt(absenceSessions);
        // }
        // else if ((!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions))
        // &&
        // (!StringUtils.isEmpty(absenceReason) && unAuthorisedCodes.contains(absenceReason)))
        // {
        // m_totalSessionsUnauthorised += Integer.parseInt(absenceSessions);
        // }
        // }
        // }
        // }
        // }
        // }
        // }
        //
        // // summer attendance calculation
        // Element summerAttendance = studentAttendance.getChild("SummerHalfTerm2Attendance");
        // if (summerAttendance != null)
        // {
        // String sessionsPossibleAsString = summerAttendance.getChildTextTrim("SessionsPossible");
        // int sessionsPossible = 0;
        // if (!StringUtils.isEmpty(sessionsPossibleAsString))
        // {
        // sessionsPossible = Integer.parseInt(sessionsPossibleAsString);
        //
        // if (sessionsPossible > 0)
        // {
        // m_totalSessionsPossible += sessionsPossible;
        //
        // Element sessionDetails = summerAttendance.getChild("SessionDetails");
        // if (sessionDetails != null)
        // {
        // Collection<Element> sessionDetailList = sessionDetails.getChildren();
        //
        // if (sessionDetailList != null)
        // {
        // for (Element sessionDetail : sessionDetailList)
        // {
        // String absenceSessions = sessionDetail.getChildTextTrim("AbsenceSessions");
        // String absenceReason = sessionDetail.getChildTextTrim("AttendanceReason");
        //
        // if ((!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions)) &&
        // (!StringUtils.isEmpty(absenceReason) && authorisedCodes.contains(absenceReason)))
        // {
        // m_totalSessionsAuthorised += Integer.parseInt(absenceSessions);
        // }
        // else if ((!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions))
        // &&
        // (!StringUtils.isEmpty(absenceReason) && unAuthorisedCodes.contains(absenceReason)))
        // {
        // m_totalSessionsUnauthorised += Integer.parseInt(absenceSessions);
        // }
        // }
        // }
        // }
        // }
        // }
        // }
        // }
        // }
        // }

        PlainDate dob = DateUtils.getDate(dobAsString);

        Element pupilStatusElement = pupilElement.getChild("PupilStatus");
        if (pupilStatusElement != null) {
            Element boarderElement = pupilStatusElement.getChild("Boarder");
            String boarderText = "";
            if (boarderElement != null) {
                boarderText = boarderElement.getTextTrim();
            }

            // TODO
            Element pupilExclusions = pupilElement.getChild("Exclusions");
            if (pupilExclusions != null) {
                List<Element> termlyExclusions = pupilExclusions.getChildren("TermlyExclusion");
                List<Element> termlyExclusions2Delete = new ArrayList<Element>();
                if (termlyExclusions != null) {
                    for (Element termlyExclusion : termlyExclusions) {
                        Element categoryElement = termlyExclusion.getChild("Category");
                        if (categoryElement != null) {
                            String categoryAsString = categoryElement.getTextTrim();

                            // Delete Exclusions that don't have the Category set in the Incident
                            // Action
                            if (StringUtils.isEmpty(categoryAsString)) {
                                termlyExclusions2Delete.add(termlyExclusion);
                            }
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
            }
        }


        /*
         * if (m_term.equals(CENSUS_TERM_AUTUMN))
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
         * startDate = new PlainDate(m_dateFormat.parse(learningStartDateStr));
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
                } else {
                    String sessionsPossible = summerAttendance.getChildTextTrim("SessionsPossible");
                    Element sessionDetails = summerAttendance.getChild("SessionDetails");

                    if (StringUtils.isEmpty(sessionsPossible) || STRING_ZERO.equals(sessionsPossible)) {
                        studentAttendance.removeChild("SummerHalfTerm2Attendance");
                        summerExist = false;
                    } else if (sessionDetails != null) {
                        // AppGlobals.getLog().severe("'"+upn+"',");
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

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        String name = getJob().getTool().getName();
        String jobId = String.valueOf(getJob().getJobId());

        /*
         * Step 1. Get the root element
         */
        Element root = m_reportData.getRootElement();

        String[] logParameters = new String[] {name, jobId};
        AppGlobals.getLog().log(Level.INFO, LOG_DATA_PREPARED, logParameters);

        ThreadUtils.checkInterrupt();

        /*
         * If there are errors, print them out
         */
        OutputStream outputStream = getResultHandler().getOutputStream();
        if (m_initErrors.size() > 0 || root == null) {
            outputStream.write(("There were some errors initializing" + FORMAT_EOL_WINDOWS).getBytes());
            for (StateReportValidationError error : m_initErrors) {
                String errorId = error.getErrorId();
                String fieldName = error.getFieldName();
                String errorMessage = error.getErrorMessage();
                String message = String.format("%s %s - %s", errorId, fieldName, errorMessage);
                outputStream.write((message + FORMAT_EOL_WINDOWS).getBytes());
            }
        }

        /*
         * If the job hasn't been aborted, then format the results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            if (DEBUG_OPTION_SUMMARY.equals(m_validationMode)) {
                ReportDataGrid errorGrid = m_reportData.getErrorGrid();
                outputStream.write(errorGrid.format(true, false, false).getBytes());
            } else if (root != null) {
                Document document = new Document();
                document.setRootElement(root);
                XMLOutputter outputter = new XMLOutputter();
                try {
                    Format format = Format.getPrettyFormat();
                    outputter.setFormat(format);
                    outputter.output(document, outputStream);
                } catch (IOException ioe) {
                    String init_msg = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale())
                            .getMessage(INITIALIZE_KEY);
                    m_initErrors.add(new StateReportValidationError(init_msg, init_msg, init_msg, ioe.getMessage()));
                    throw ioe;
                }
            }
        }
    }

    /**
     * Increments attendance counts for an individual student.
     *
     * @param pupilElement Element
     */
    private void runAttendanceCalculations(Element pupilElement) {
        Element pupilIdentifiersElement = pupilElement.getChild("PupilIdentifiers");
        Element dateOfBirthElement = pupilIdentifiersElement.getChild("DOB");

        if (dateOfBirthElement != null) {
            String dobAsString = dateOfBirthElement.getTextTrim();
            if (!StringUtils.isEmpty(dobAsString)) {
                int attendanceAgeYear = Integer.parseInt(dobAsString.substring(0, 4));
                int attendanceAgeMonth = Integer.parseInt(dobAsString.substring(5, 7));

                int beforeAugustBooleanInt = 0;
                if (attendanceAgeMonth > 8) {
                    beforeAugustBooleanInt = 0;
                }

                int attendanceAge = 2013 - attendanceAgeYear - beforeAugustBooleanInt - 1;
                if ((attendanceAge >= 5) && (attendanceAge <= 15)) {
                    Element pupilStatusElement = pupilElement.getChild("PupilStatus");
                    if (pupilStatusElement != null) {
                        Element boarderElement = pupilStatusElement.getChild("Boarder");
                        String boarderText = null;
                        if (boarderElement != null) {
                            boarderText = boarderElement.getTextTrim();
                        }
                        if (boarderText != null && "N".equals(boarderText)) {
                            Element studentAttendance = pupilElement.getChild("Attendance");
                            if (studentAttendance != null) {
                                ArrayList<String> authorisedCodes = new ArrayList<String>();
                                authorisedCodes.add("I");
                                authorisedCodes.add("M");
                                authorisedCodes.add("R");
                                authorisedCodes.add("S");
                                authorisedCodes.add("T");
                                authorisedCodes.add("H");
                                authorisedCodes.add("F");
                                authorisedCodes.add("E");
                                authorisedCodes.add("C");

                                ArrayList<String> unAuthorisedCodes = new ArrayList<String>();
                                unAuthorisedCodes.add("G");
                                unAuthorisedCodes.add("U");
                                unAuthorisedCodes.add("O");
                                unAuthorisedCodes.add("N");

                                // termly attendance calculation
                                Element termlyAttendance = studentAttendance.getChild("TermlyAttendance");
                                if (termlyAttendance != null) {
                                    String sessionsPossibleAsString =
                                            termlyAttendance.getChildTextTrim("SessionsPossible");
                                    int sessionsPossible = 0;
                                    if (!StringUtils.isEmpty(sessionsPossibleAsString)) {
                                        sessionsPossible = Integer.parseInt(sessionsPossibleAsString);

                                        if (sessionsPossible > 0) {
                                            m_totalSessionsPossible += sessionsPossible;

                                            addSessionCalculations(termlyAttendance, authorisedCodes,
                                                    unAuthorisedCodes);
                                        }
                                    }
                                }

                                // summer attendance calculation
                                Element summerAttendance = studentAttendance.getChild("SummerHalfTerm2Attendance");
                                if (summerAttendance != null) {
                                    String sessionsPossibleAsString =
                                            summerAttendance.getChildTextTrim("SessionsPossible");
                                    int sessionsPossible = 0;
                                    if (!StringUtils.isEmpty(sessionsPossibleAsString)) {
                                        sessionsPossible = Integer.parseInt(sessionsPossibleAsString);

                                        if (sessionsPossible > 0) {
                                            m_totalSessionsPossible += sessionsPossible;

                                            addSessionCalculations(summerAttendance, authorisedCodes,
                                                    unAuthorisedCodes);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Increments the m_totalSessionsAuthorised and m_totalSessionsUnauthorised for an
     * attendanceElement.
     *
     * @param attendanceElement Element
     * @param authorisedCodes ArrayList<String>
     * @param unAuthorisedCodes ArrayList<String>
     */
    private void addSessionCalculations(Element attendanceElement,
                                        ArrayList<String> authorisedCodes,
                                        ArrayList<String> unAuthorisedCodes) {
        Element sessionDetails = attendanceElement.getChild("SessionDetails");
        if (sessionDetails != null) {
            Collection<Element> sessionDetailList = sessionDetails.getChildren();

            if (sessionDetailList != null) {
                for (Element sessionDetail : sessionDetailList) {
                    String absenceSessions = sessionDetail.getChildTextTrim("AbsenceSessions");
                    String absenceReason = sessionDetail.getChildTextTrim("AttendanceReason");

                    if ((!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions)) &&
                            (!StringUtils.isEmpty(absenceReason) && authorisedCodes.contains(absenceReason))) {
                        m_totalSessionsAuthorised += Integer.parseInt(absenceSessions);
                    } else if ((!StringUtils.isEmpty(absenceSessions) && !STRING_ZERO.equals(absenceSessions)) &&
                            (!StringUtils.isEmpty(absenceReason) && unAuthorisedCodes.contains(absenceReason))) {
                        m_totalSessionsUnauthorised += Integer.parseInt(absenceSessions);
                    }
                }
            }
        }
    }

}
