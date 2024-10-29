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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class DfEExportManager.
 */
public class DfEExportManager extends DfEManager {
    private static final String ASPEN_PROGRAM_CODE_SE = "Special Education";
    private static final String ASPEN_PROGRAM_CODE_FRL = "Free/Reduced Lunch";
    private static final String ASPEN_PROGRAM_CODE_NAW = "NAW";

    public static final char LABEL_PREFIX_CHAR = '$';

    public static final String ATTENDANCE_MARK_NOT_IN_SESSION = "#";
    public static final String ATTENDANCE_MARK_PRESENT_AM_SESSION = "/";
    public static final String ATTENDANCE_MARK_PRESENT_PM_SESSION = "\\";

    /**
     * A local copy XML File Element builder.
     */
    private SAXBuilder m_builder = null;

    /**
     * List of the start and end dates of each school year
     */
    protected ArrayList<ArrayList<PlainDate>> m_schoolYearRanges;

    /**
     * Set of authorized attendance codes
     */
    public final Set<String> VAL_AUTHORISED_CODE_SET =
            new HashSet<String>(Arrays.asList(new String[] {
                    "I", // authorized illness
                    "M", // authorized medical/dental appoint
                    "R", // authorized religious observance
                    "S", // authorized study leave
                    "T", // authorized traveler absence
                    "H", // authorized agreed family holiday
                    "F", // authorized agreed extended family holiday
                    "E", // authorized excluded no alt. provision
                    "C" // authorized other circumstances
            }));

    /**
     * Set of unauthorized attendance codes
     */
    public final Set<String> VAL_UNAUTHORISED_CODE_SET =
            new HashSet<String>(Arrays.asList(new String[] {
                    "G", // unauthorized family holiday
                    "U", // unauthorized arrived after registers closed
                    "O", // unauthorized other
                    "N" // unauthorized reason not provided
            }));


    /**
     * Constructor for DfEManager.
     *
     * @param broker X2Broker
     * @param locale Locale
     */
    public DfEExportManager(X2Broker broker, Locale locale) {
        super(broker, locale);
        m_builder = new SAXBuilder();
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

            if (dfEAddress.getSAON() != null) {
                Element sAONElement = new Element(DfEAddress.ELEMENT_SAON);
                sAONElement.addContent(dfEAddress.getSAON());
                bS7666AddressElement.addContent(sAONElement);
            }
            if (dfEAddress.getPAON() != null) {
                Element pAONElement = new Element(DfEAddress.ELEMENT_PAON);
                pAONElement.addContent(dfEAddress.getPAON());
                bS7666AddressElement.addContent(pAONElement);
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
            if (dfEAddress.getZip() != null) {
                Element postTownElement = new Element(DfEAddress.ELEMENT_ZIP);
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
     * Get a Admissions Element from a DfEPupil.
     *
     * @return Element
     */
    public Element getAdmissionsElement() {
        // Setting default to "true" because in Aspen a student has already been admitted as a
        // result of being Enrolled.
        Element admissionsElement = new Element(DfEPupil.ELEMENT_ADMISSIONS);
        Element acceptElement = new Element(DfEPupil.ELEMENT_ACCEPT);
        acceptElement.addContent(STRING_TRUE);
        admissionsElement.addContent(acceptElement);

        return admissionsElement;
    }

    /**
     * Get all Attendances for a Pupil.
     *
     * @param student Student
     * @param attendanceHistories Collection<StudentSchool>
     * @param attendances Collection<StudentAttendance>
     * @param attendancesByDate Map<PlainDate,StudentAttendance>
     * @param attendanceXML String
     * @return ArrayList<DfEAttendance>
     */
    public ArrayList<DfEAttendance> getAttendance(Student student,
                                                  Collection<StudentSchool> attendanceHistories,
                                                  Collection<StudentAttendance> attendances,
                                                  Map<PlainDate, StudentAttendance> attendancesByDate,
                                                  String attendanceXML) {
        ArrayList<DfEAttendance> dfeAttendances = new ArrayList<DfEAttendance>();
        DfEAttendance dfeAttendance = null;

        // TODO Remove later
        // All previously imported CTF attendances and add to attendances list
        /*
         * if (!StringUtils.isEmpty(attendanceXML))
         * {
         * ArrayList<DfEAttendance> oldAttendances = parseAttendanceXML(attendanceXML);
         * 
         * if (oldAttendances != null && oldAttendances.size() > 0)
         * {
         * dfeAttendances.addAll(oldAttendances);
         * }
         * }
         */

        // Retrieve Student Attendance data that was imported from a student's older schools before
        // entry into the Aspen system
        for (StudentSchool studentSchool : attendanceHistories) {
            String schoolyear = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_YEAR);
            String lEA = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_LEA);
            String estab = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_ESTAB);
            String schoolName = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SCHOOL_NAME);
            String sessionsPossibleStr = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_POSSIBLE);
            int sessionsPossible = 0;
            if (!StringUtils.isEmpty(sessionsPossibleStr) && StringUtils.isNumeric(sessionsPossibleStr)) {
                sessionsPossible = Integer.parseInt(sessionsPossibleStr);
            }
            String sessionsAuthorisedStr =
                    (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_AUTHORIZED);
            int sessionsAuthorised = 0;
            if (!StringUtils.isEmpty(sessionsAuthorisedStr) && StringUtils.isNumeric(sessionsAuthorisedStr)) {
                sessionsAuthorised = Integer.parseInt(sessionsAuthorisedStr);
            }
            String sessionsAttendedStr = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_ATTENDED);
            int sessionsAttended = 0;
            if (!StringUtils.isEmpty(sessionsAttendedStr) && StringUtils.isNumeric(sessionsAttendedStr)) {
                sessionsAttended = Integer.parseInt(sessionsAttendedStr);
            }
            String sessionsUnauthorisedStr =
                    (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_SESS_UNAUTHORIZED);
            int sessionsUnauthorised = 0;
            if (!StringUtils.isEmpty(sessionsUnauthorisedStr) && StringUtils.isNumeric(sessionsUnauthorisedStr)) {
                sessionsUnauthorised = Integer.parseInt(sessionsUnauthorisedStr);
            }
            String attendanceStartDateStr = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_START_DATE);
            String attendanceMarks = (String) studentSchool.getFieldValueByAlias(ALIAS_NAME_ATTEND_MARKS);

            dfeAttendance = new DfEAttendance(schoolyear, lEA, estab, schoolName, sessionsPossible, sessionsAuthorised,
                    sessionsAttended, sessionsUnauthorised, attendanceStartDateStr, attendanceMarks);
            dfeAttendances.add(dfeAttendance);
        }


        // Retrieve Student Attendance data that has been added since the student has entered the
        // system
        Calendar calendar = Calendar.getInstance();
        AttendanceStatistics attendanceStatistics;

        char presentAMSessionChar = ATTENDANCE_MARK_PRESENT_AM_SESSION.charAt(0);
        char presentPMSessionChar = ATTENDANCE_MARK_PRESENT_PM_SESSION.charAt(0);

        Map<String, Object> parametersMap = new HashMap<String, Object>();
        parametersMap.put("queryBy1", "oid");
        parametersMap.put("queryString1", student.getOid());

        // Prepare data source.
        attendanceStatistics = new AttendanceStatistics();
        attendanceStatistics.setBroker(m_broker);
        attendanceStatistics.setOrganization(student.getSchool().getParentOrganization());
        attendanceStatistics.setSchoolContext(false);
        attendanceStatistics.setParameters(parametersMap);
        try {
            attendanceStatistics.initializeExport();
        } catch (X2BaseException e) {
            e.printStackTrace();
        }

        if (attendances != null) {
            for (ArrayList<PlainDate> dates : m_schoolYearRanges) {
                PlainDate startDate = dates.get(0);
                PlainDate endDate = dates.get(1);

                String attendanceMarks = STRING_EMPTY;
                PlainDate attendanceStartDate = null;
                boolean findAttendanceStartDate = true;
                int sessionsPossible = 0;
                int sessionsAttended = 0;
                int sessionsAuthorised = 0;
                int sessionsUnauthorised = 0;
                SisSchool school = null;

                if (startDate != null) {
                    calendar.setTime(startDate);

                    List<StudentEnrollmentSpan> spans =
                            attendanceStatistics.m_helper.getStudentEnrollmentSpans(student, false);

                    for (StudentEnrollmentSpan span : spans) {
                        PlainDate firstActiveDate = null;
                        if (span.getFirstActiveEnrollment() != null) {
                            firstActiveDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                        }

                        if (firstActiveDate == null) {
                            firstActiveDate = startDate;
                        } else if (firstActiveDate.before(startDate)) {
                            firstActiveDate = startDate;
                        }

                        PlainDate lastActiveDate = null;
                        if (span.getFirstInactiveEnrollment() != null) {
                            lastActiveDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
                        }

                        if (lastActiveDate == null) {
                            lastActiveDate = endDate;
                        } else if (lastActiveDate.after(endDate)) {
                            lastActiveDate = endDate;
                        }

                        if (findAttendanceStartDate) {
                            attendanceStartDate = firstActiveDate;

                            if (attendanceStartDate == null) {
                                attendanceStartDate = startDate;
                            }

                            findAttendanceStartDate = false;
                        }

                        if (firstActiveDate != null) {
                            attendanceMarks +=
                                    getAttendanceMarks(student, firstActiveDate, lastActiveDate, attendancesByDate);

                            Collection<PlainDate> inSessionDates = CalendarManager.getInSessionDates(firstActiveDate,
                                    lastActiveDate, (SisStudent) student, m_broker);

                            for (PlainDate date : inSessionDates) {
                                if (!date.before(firstActiveDate) && !date.after(lastActiveDate)) {
                                    sessionsPossible += 2;
                                }
                            }
                        } else {
                            attendanceMarks +=
                                    getAttendanceMarks(student, startDate, lastActiveDate, attendancesByDate);
                        }

                        for (StudentAttendance attendance : attendances) {
                            PlainDate attendanceDate = attendance.getDate();
                            if (school == null) {
                                school = attendance.getSchool();
                            }

                            if ((attendanceDate.after(startDate) || attendanceDate.equals(startDate)) &&
                                    (attendanceDate.before(endDate) || attendanceDate.equals(endDate))) {
                                /*
                                 * ------------------------------------------
                                 * AM
                                 */
                                String otherCodeAM = attendance.getOtherCode();
                                if (VAL_AUTHORISED_CODE_SET.contains(otherCodeAM)) {
                                    sessionsAuthorised++;
                                } else if (VAL_UNAUTHORISED_CODE_SET.contains(otherCodeAM)) {
                                    sessionsUnauthorised++;
                                }

                                /*
                                 * -------------------------------------------
                                 * PM
                                 */
                                String otherCodePM = attendance.getOtherCode02();
                                if (VAL_AUTHORISED_CODE_SET.contains(otherCodePM)) {
                                    sessionsAuthorised++;
                                } else if (VAL_UNAUTHORISED_CODE_SET.contains(otherCodePM)) {
                                    sessionsUnauthorised++;
                                }
                            }
                        }
                    }
                }

                if (!StringUtils.isEmpty(attendanceMarks)) {
                    for (int i = 0; i < attendanceMarks.length(); i++) {
                        if (attendanceMarks.charAt(i) == presentAMSessionChar
                                || attendanceMarks.charAt(i) == presentPMSessionChar) {
                            sessionsAttended++;
                        }
                    }
                }

                if (school == null) {
                    school = (SisSchool) student.getSchool();
                }
                Organization organizaion = school.getOrganization1();
                String schoolyear = Integer.toString(calendar.get(Calendar.YEAR));
                String lEA = (String) organizaion.getFieldValueByAlias(ALIAS_NAME_LEA, getDataDictionary());
                String estab = (String) school.getFieldValueByAlias(ALIAS_NAME_ESTAB, getDataDictionary());
                String schoolName = school.getName();
                String attendanceStartDateStr = null;
                if (attendanceStartDate != null) {
                    attendanceStartDateStr = m_dateFormat.format(attendanceStartDate);
                }

                dfeAttendance =
                        new DfEAttendance(schoolyear, lEA, estab, schoolName, sessionsPossible, sessionsAuthorised,
                                sessionsAttended, sessionsUnauthorised, attendanceStartDateStr, attendanceMarks);
                dfeAttendances.add(dfeAttendance);
            }
        }

        return dfeAttendances;
    }

    /**
     * Get a Attendance Element from DfEPupil.
     *
     * @param dfEPupil DfEPupil
     * @param includeSessions boolean
     * @return Element
     */
    public Element getAttendanceElement(DfEPupil dfEPupil, boolean includeSessions) {
        Element studentAttendanceElement = null;
        Element attendSessionsElement = null;

        if (dfEPupil.getAttendances() != null && dfEPupil.getAttendances().size() > 0) {
            studentAttendanceElement = new Element(DfEPupil.ELEMENT_ATTENDANCE);

            ArrayList<DfEAttendance> attendances = dfEPupil.getAttendances();

            // Sort by Year, LEA, Estab and School Name
            TreeMap<String, DfEAttendance> attendancesMap = new TreeMap<String, DfEAttendance>();
            for (DfEAttendance dfEAttendance : attendances) {
                String key = dfEAttendance.getYear() + ":" + dfEAttendance.getLEA() + ":" + dfEAttendance.getEstab()
                        + ":" + dfEAttendance.getSchoolName();
                attendancesMap.put(key, dfEAttendance);
            }

            for (DfEAttendance dfEAttendance : attendancesMap.values()) {
                Element yearDataElement = new Element(DfEAttendance.ELEMENT_YEAR_DATA);

                if (dfEAttendance.getSessionsPossible() > 0) {
                    if (dfEAttendance.getYear() != null) {
                        Element yearElement = new Element(DfEAttendance.ELEMENT_YEAR);
                        yearElement.addContent(dfEAttendance.getYear());
                        yearDataElement.addContent(yearElement);
                    }
                    if (dfEAttendance.getLEA() != null) {
                        Element leaElement = new Element(DfEAttendance.ELEMENT_LEA);
                        leaElement.addContent(dfEAttendance.getLEA());
                        yearDataElement.addContent(leaElement);
                    }
                    if (dfEAttendance.getEstab() != null) {
                        Element estabElement = new Element(DfEAttendance.ELEMENT_ESTAB);
                        estabElement.addContent(dfEAttendance.getEstab());
                        yearDataElement.addContent(estabElement);
                    }
                    if (dfEAttendance.getSchoolName() != null) {
                        Element schoolNameElement = new Element(DfEAttendance.ELEMENT_SCHOOL_NAME);
                        schoolNameElement.addContent(dfEAttendance.getSchoolName());
                        yearDataElement.addContent(schoolNameElement);
                    }
                    if (dfEAttendance.getSessionsPossible() >= 0) {
                        Element sessionsPossibleElement = new Element(DfEAttendance.ELEMENT_SESSIONS_POSSIBLE);
                        sessionsPossibleElement.addContent(String.valueOf(dfEAttendance.getSessionsPossible()));
                        yearDataElement.addContent(sessionsPossibleElement);
                    }
                    if (dfEAttendance.getSessionsAuthorised() >= 0) {
                        Element sessionsAuthorisedElement = new Element(DfEAttendance.ELEMENT_SESSIONS_AUTHORISED);
                        sessionsAuthorisedElement.addContent(String.valueOf(dfEAttendance.getSessionsAuthorised()));
                        yearDataElement.addContent(sessionsAuthorisedElement);
                    }
                    if (dfEAttendance.getSessionsAttended() >= 0) {
                        Element sessionsAttendedElement = new Element(DfEAttendance.ELEMENT_SESSIONS_ATTENDED);
                        sessionsAttendedElement.addContent(String.valueOf(dfEAttendance.getSessionsAttended()));
                        yearDataElement.addContent(sessionsAttendedElement);
                    }
                    if (dfEAttendance.getSessionsUnauthorised() >= 0) {
                        Element sessionsUnauthorisedElement = new Element(DfEAttendance.ELEMENT_SESSIONS_UNAUTHORISED);
                        sessionsUnauthorisedElement.addContent(String.valueOf(dfEAttendance.getSessionsUnauthorised()));
                        yearDataElement.addContent(sessionsUnauthorisedElement);
                    }

                    if (includeSessions) {
                        attendSessionsElement = new Element(DfEAttendance.ELEMENT_ATTEND_SESSIONS);

                        if (dfEAttendance.getAttendanceStartDate() != null) {
                            Element attendanceStartDateElement = new Element(DfEAttendance.ELEMENT_ATTEND_START_DATE);
                            attendanceStartDateElement
                                    .addContent(m_dateFormat.format(dfEAttendance.getAttendanceStartDate()));
                            attendSessionsElement.addContent(attendanceStartDateElement);
                        }
                        if (dfEAttendance.getAttendanceMarks() != null) {
                            Element attendanceMarksElement = new Element(DfEAttendance.ELEMENT_ATTEND_MARKS);
                            attendanceMarksElement.addContent(dfEAttendance.getAttendanceMarks());
                            attendSessionsElement.addContent(attendanceMarksElement);
                        }

                        if (dfEAttendance.getAttendanceStartDate() != null
                                || dfEAttendance.getAttendanceMarks() != null) {
                            yearDataElement.addContent(attendSessionsElement);
                        }
                    }

                    studentAttendanceElement.addContent(yearDataElement);
                }
            }
        }

        return studentAttendanceElement;
    }

    /**
     * Gets the attendance marks for a student between a two date parameters.
     *
     * @param student Student
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param attendancesByDate Map<PlainDate,StudentAttendance>
     * @return attendanceMarks
     */
    public String getAttendanceMarks(Student student,
                                     PlainDate startDate,
                                     PlainDate endDate,
                                     Map<PlainDate, StudentAttendance> attendancesByDate) {
        Criteria criteria = new Criteria();
        criteria.addGreaterOrEqualThan(SchoolCalendarDate.COL_DATE, startDate);
        criteria.addLessOrEqualThan(SchoolCalendarDate.COL_DATE, endDate);
        criteria.addEqualTo(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                student.getSchoolOid());

        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

        QueryIterator iterator = m_broker.getIteratorByQuery(query);

        StringBuffer attendanceMarks = new StringBuffer(STRING_EMPTY);
        while (iterator.hasNext()) {
            SchoolCalendarDate scd = (SchoolCalendarDate) iterator.next();
            PlainDate date = scd.getDate();
            boolean inSession = scd.getInSessionIndicator();

            if (!inSession) {
                attendanceMarks.append(ATTENDANCE_MARK_NOT_IN_SESSION + ATTENDANCE_MARK_NOT_IN_SESSION);
            } else if (attendancesByDate != null && attendancesByDate.containsKey(date) && inSession) {
                StudentAttendance attendance = attendancesByDate.get(date);

                if (!StringUtils.isEmpty(attendance.getOtherCode())) {
                    attendanceMarks.append(attendance.getOtherCode());
                } else {
                    attendanceMarks.append(ATTENDANCE_MARK_PRESENT_AM_SESSION);
                }

                if (!StringUtils.isEmpty(attendance.getOtherCode02())) {
                    attendanceMarks.append(attendance.getOtherCode02());
                } else {
                    attendanceMarks.append(ATTENDANCE_MARK_PRESENT_PM_SESSION);
                }
            } else {
                attendanceMarks.append(ATTENDANCE_MARK_PRESENT_AM_SESSION + ATTENDANCE_MARK_PRESENT_PM_SESSION);
            }
        }
        iterator.close();

        return attendanceMarks.toString();
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
        if (dfEPupil.getPreferredForename() != null) {
            Element preferredForenameElement = new Element(DfEPupil.ELEMENT_PREFERRED_FORENAME);
            preferredForenameElement.addContent(dfEPupil.getPreferredForename());
            basicDetailsElement.addContent(preferredForenameElement);
        }
        if (dfEPupil.getFormerSurname() != null) {
            Element formerSurnameElement = new Element(DfEPupil.ELEMENT_FORMER_SURNAME);
            formerSurnameElement.addContent(dfEPupil.getFormerSurname());
            basicDetailsElement.addContent(formerSurnameElement);
        }
        if (dfEPupil.getFormerForename() != null) {
            Element formerForenameElement = new Element(DfEPupil.ELEMENT_FORMER_FORENAME);
            formerForenameElement.addContent(dfEPupil.getFormerForename());
            basicDetailsElement.addContent(formerForenameElement);
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
                medicalFlagElement.addContent(STRING_TRUE);
            } else {
                medicalFlagElement.addContent(STRING_FALSE);
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

        return basicDetailsElement;
    }

    /**
     * Get a Contacts Element from Collection<DfEContact> .
     *
     * @param contactList Collection<DfEContact>
     * @return Element
     */
    public Element getContacts(Collection<DfEContact> contactList) {
        Element contactsElement = new Element(DfEPupil.ELEMENT_CONTACTS);

        // Make sure the contacts are in order
        TreeMap<String, DfEContact> contactsMap = new TreeMap<String, DfEContact>();
        for (DfEContact dfEContact : contactList) {
            Integer order = Integer.valueOf(dfEContact.getOrder());
            String key = String.format("%03d", order) + dfEContact.getSurname() + dfEContact.getForename()
                    + dfEContact.getRelationshipCode();
            contactsMap.put(key, dfEContact);
        }
        Collection<DfEContact> contacts = contactsMap.values();

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
                    responsibilityElement.addContent(STRING_TRUE);
                } else {
                    responsibilityElement.addContent(STRING_FALSE);
                }
                contactElement.addContent(responsibilityElement);
            }

            // Contact Address
            if (dfEContact.getAddressAsPupil() != null && dfEContact.getAddressAsPupil().booleanValue() == true) {
                Element addressElement = new Element(DfEContact.ELEMENT_ADDRESS);

                Element addressAsPupilElement = new Element(DfEContact.ELEMENT_ADDRESS_AS_PUPIL);
                addressAsPupilElement.addContent(STRING_TRUE);
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

            if (!StringUtils.isEmpty(dfEContact.getEmail())) {
                Element emailElement = new Element(DfEContact.ELEMENT_EMAIL);
                emailElement.addContent(dfEContact.getEmail());
                contactElement.addContent(emailElement);
            }


            contactsElement.addContent(contactElement);
        }

        return contactsElement;
    }

    /**
     * Get the DfEHeader.
     *
     * @param headerType String
     * @param sourceLEA String
     * @param sourceEstab String
     * @param schoolName String
     * @param sourceSchoolAcademicYear String
     * @param destLEA String
     * @param destEstab String
     * @return DfECTFHeader
     */
    public DfEHeader getDfEHeader(String headerType,
                                  String sourceLEA,
                                  String sourceEstab,
                                  String schoolName,
                                  String sourceSchoolAcademicYear,
                                  String destLEA,
                                  String destEstab) {
        DfEHeader dfEHeader = new DfEHeader();
        dfEHeader.setHeaderType(headerType);
        if (REPORT_TYPE_CTF.equals(headerType)) {
            dfEHeader.setDocumentName(DfEHeader.CTF_DOCUMENT_NAME);
            dfEHeader.setVersion(DfEHeader.CTF_VERSION);
        } else if (REPORT_TYPE_ATF.equals(headerType)) {
            dfEHeader.setDocumentName(DfEHeader.ATF_DOCUMENT_NAME);
            dfEHeader.setVersion(DfEHeader.ATF_VERSION);
        }
        Date now = new Date();
        String dateTime = m_dateFormat.format(now) + DFE_DATA_TIME_DELIMITER + m_timeFormat.format(now);
        dfEHeader.setDateTime(dateTime);
        dfEHeader.setDocumentQualifier(DfEHeader.DEFAULT_DOCUMENT_QUALIFIER);
        dfEHeader.setSupplierID(DfEHeader.DEFAULT_SUPPLIER_ID);
        dfEHeader.setSourceSchoolLEA(sourceLEA);
        dfEHeader.setSourceSchoolEstab(sourceEstab);
        dfEHeader.setSourceSchoolName(schoolName);
        dfEHeader.setSourceSchoolAcademicYear(sourceSchoolAcademicYear);
        dfEHeader.setDestSchoolLEA(destLEA);
        dfEHeader.setDestSchoolEstab(destEstab);

        return dfEHeader;
    }

    /**
     * Get DfEPupil object from the Student information from the DB
     *
     * Used in ATF Export.
     *
     * @param student SisStudent
     * @param enrollments Collection<StudentEnrollment>
     * @return DfEPupil
     */
    public DfEPupil getDfEPupilByStudent(SisStudent student, Collection<StudentEnrollment> enrollments) {
        // Provide empty collections
        Collection<StudentAssessment> studentAssessments = new ArrayList<StudentAssessment>();
        Collection<StudentSchool> studentAttendanceHistories = new ArrayList<StudentSchool>();
        Collection<StudentAttendance> studentAttendances = new ArrayList<StudentAttendance>();
        Map<PlainDate, StudentAttendance> attendancesByDate = new HashMap<PlainDate, StudentAttendance>();

        return getDfEPupilByStudent(student, enrollments, studentAssessments, studentAttendanceHistories,
                studentAttendances, attendancesByDate);
    }

    /**
     * Get DfEPupil object from the Student information from the DB
     *
     * Used in CTF Export.
     *
     * @param student SisStudent
     * @param enrollments Collection<StudentEnrollment>
     * @param assessments Collection<StudentAssessment>
     * @param attendanceHistories Collection<StudentSchool>
     * @param attendances Collection<StudentAttendance>
     * @param attendancesByDate Map<PlainDate,StudentAttendance>
     * @return DfEPupil
     */
    public DfEPupil getDfEPupilByStudent(SisStudent student,
                                         Collection<StudentEnrollment> enrollments,
                                         Collection<StudentAssessment> assessments,
                                         Collection<StudentSchool> attendanceHistories,
                                         Collection<StudentAttendance> attendances,
                                         Map<PlainDate, StudentAttendance> attendancesByDate) {
        // Get Student
        Person studentPerson = student.getPerson();

        DfEPupil dfEPupil = new DfEPupil();
        String applicationReference =
                (String) student.getFieldValueByAlias(ALIAS_NAME_APPLICATION_REFERENCE, getDataDictionary());
        if (!StringUtils.isEmpty(applicationReference)) {
            dfEPupil.setApplicationReference(applicationReference);
        }
        String uniquePupilNumber = (String) student.getFieldValueByAlias(ALIAS_NAME_UPN, getDataDictionary());
        if (!StringUtils.isEmpty(uniquePupilNumber)) {
            dfEPupil.setUniquePupilNumber(uniquePupilNumber);
        }
        String uniqueLearningNumber = (String) student.getFieldValueByAlias(ALIAS_NAME_ULN, getDataDictionary());
        if (!StringUtils.isEmpty(uniqueLearningNumber)) {
            dfEPupil.setUniqueLearnerNumber(uniqueLearningNumber);
        }
        String uniqueCandidateIdentifier = (String) student.getFieldValueByAlias(ALIAS_NAME_UCI, getDataDictionary());
        if (!StringUtils.isEmpty(uniqueCandidateIdentifier)) {
            dfEPupil.setUniqueCandidateIdentifier(uniqueCandidateIdentifier);
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
        String preferredForename =
                (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_PREFERRED_FORENAME, getDataDictionary());
        if (!StringUtils.isEmpty(preferredForename)) {
            dfEPupil.setPreferredForename(preferredForename);
        }
        String formerSurname =
                (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_FORMER_SURNAME, getDataDictionary());
        if (!StringUtils.isEmpty(formerSurname)) {
            dfEPupil.setFormerSurname(formerSurname);
        }
        String formerForename =
                (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_FORMER_FORENAME, getDataDictionary());
        if (!StringUtils.isEmpty(formerForename)) {
            dfEPupil.setFormerForename(formerForename);
        }
        if (!StringUtils.isEmpty(studentPerson.getMiddleName())) {
            dfEPupil.setMiddleNames(studentPerson.getMiddleName());
        }
        dfEPupil.setNCYearActual(student.getGradeLevel());

        // No translation of DfE Ethnicity Code
        String ethnicity = (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_ETHNICITY, getDataDictionary());
        if (!StringUtils.isEmpty(ethnicity)) {
            dfEPupil.setEthnicity(ethnicity);
        }
        String ethnicitySource =
                (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_ETHNICITY_SOURCE, getDataDictionary());
        if (!StringUtils.isEmpty(ethnicitySource)) {
            String ethnicitySourceFieldName = translateAliasToJavaName(ALIAS_NAME_ETHNICITY_SOURCE, true);
            if (ethnicitySourceFieldName != null) {
                String ethnicitySourceStateCode =
                        lookupStateValueByRefCode(Person.class, ethnicitySourceFieldName, ethnicitySource);
                dfEPupil.setEthnicitySource(ethnicitySourceStateCode);
            }
        }

        // Setting Medical Flag
        String medicalFlagStr =
                (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_MEDICAL_FLAG, getDataDictionary());
        if (DB_TRUE.equals(medicalFlagStr)) {
            dfEPupil.setMedicalFlag(BOOLEAN_TRUE);
        } else {
            dfEPupil.setMedicalFlag(BOOLEAN_FALSE);
        }

        // Set Language
        // Translate Aspen Language Code to DfE Language Code.
        String languageCode =
                lookupStateValueByRefCode(Student.class, Student.COL_HOME_LANGUAGE_CODE, student.getHomeLanguageCode());
        DfELanguage dfELanguage = new DfELanguage(DfELanguage.DEFAULT_LANGUAGE_TYPE, languageCode);
        dfEPupil.addDfELanguage(dfELanguage);

        // Set FSM Review Date
        String fSMReviewDateStr =
                (String) student.getFieldValueByAlias(ALIAS_NAME_FSM_REVIEW_DATE, getDataDictionary());
        if (!StringUtils.isEmpty(fSMReviewDateStr)) {
            dfEPupil.setFSMReviewDate(fSMReviewDateStr);
        }

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
        String enrollStatus =
                lookupStateValueByRefCode(Student.class, Student.COL_ENROLLMENT_STATUS, student.getEnrollmentStatus());
        dfEPupil.setEnrollStatus(enrollStatus);

        // Looked After
        String inCare = (String) student.getFieldValueByAlias(ALIAS_NAME_IN_CARE, getDataDictionary());
        if (!StringUtils.isEmpty(inCare)) {
            if (DB_TRUE.equalsIgnoreCase(inCare) || STRING_TRUE.equalsIgnoreCase(inCare)) {
                dfEPupil.setInCare(BOOLEAN_TRUE);
            } else {
                dfEPupil.setInCare(BOOLEAN_FALSE);
            }
            // No Translation of DfE Care Authority Code
            String careAuthority =
                    (String) student.getFieldValueByAlias(ALIAS_NAME_CARE_AUTHORITY, getDataDictionary());
            if (!StringUtils.isEmpty(careAuthority)) {
                dfEPupil.setCareAuthority(careAuthority);
            }
        }

        Collection<StudentProgramParticipation> studentPrograms = student.getProgramParticipation(m_broker);
        if (studentPrograms.size() > 0) {
            // FSM
            for (StudentProgramParticipation studentProgramParticipation : studentPrograms) {
                String programCode = studentProgramParticipation.getProgramCode();
                if (ASPEN_PROGRAM_CODE_FRL.equals(programCode)) {
                    PlainDate startDate = studentProgramParticipation.getStartDate();
                    PlainDate endDate = studentProgramParticipation.getEndDate();
                    // Translation DfE FSM UK Country Code
                    String fSMUKCountry = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_FSM_UK_COUNTRY, getDataDictionary());
                    String uKCountryFieldName = translateAliasToJavaName(ALIAS_NAME_FSM_UK_COUNTRY, true);
                    String fSMUKCountryStateCode = null;
                    if (uKCountryFieldName != null) {
                        fSMUKCountryStateCode = lookupStateValueByRefCode(StudentProgramParticipation.class,
                                uKCountryFieldName, fSMUKCountry);
                    }

                    DfEFSMInstance dfEFSMInstance = new DfEFSMInstance(startDate, endDate, fSMUKCountryStateCode);

                    dfEPupil.addFSMInstance(dfEFSMInstance);
                }
            }

            // NAW
            for (StudentProgramParticipation studentProgramParticipation : studentPrograms) {
                String programCode = studentProgramParticipation.getProgramCode();
                if (ASPEN_PROGRAM_CODE_NAW.equals(programCode)) {
                    String speakWelsh = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_SPEAK_WELSH, getDataDictionary());
                    String homeWelsh = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_HOME_WELSH,
                            getDataDictionary());
                    // Translation DfE National Identity Code
                    String nationalIdentity = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_NATIONAL_IDENTITY, getDataDictionary());
                    String nationalIdentityFieldName = translateAliasToJavaName(ALIAS_NAME_NATIONAL_IDENTITY, true);
                    String nationalIdentityStateCode = null;
                    if (nationalIdentityFieldName != null) {
                        nationalIdentityStateCode = lookupStateValueByRefCode(StudentProgramParticipation.class,
                                nationalIdentityFieldName, nationalIdentity);
                    }
                    String welshSource = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_WELSH_SOURCE, getDataDictionary());
                    String languageSource = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_EAL_ACQUISITION, getDataDictionary());
                    String eALAcquisition = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_LANGUAGE_SOURCE, getDataDictionary());
                    String sENCurrTeachingMethods = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_SEN_CURR_TEACH_METHOD, getDataDictionary());
                    String sENGroupingAndSupport = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_SEN_GROUPING_SUPPORT, getDataDictionary());
                    String sENSpecialisedResources = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_SEN_SPEC_RESOURCES, getDataDictionary());
                    String sENAdviceAndAssessment = (String) studentProgramParticipation
                            .getFieldValueByAlias(ALIAS_NAME_SEN_ADVICE_ASSESSMENT, getDataDictionary());

                    DfENAWDetail dfENAWDetail = new DfENAWDetail(speakWelsh, homeWelsh, nationalIdentityStateCode,
                            welshSource, languageSource, eALAcquisition, sENCurrTeachingMethods, sENGroupingAndSupport,
                            sENSpecialisedResources, sENAdviceAndAssessment);
                    dfEPupil.addNAWDetail(dfENAWDetail);
                }
            }

            // SEN
            PlainDate sENStartDate = null;
            String sENProvision = null;
            for (StudentProgramParticipation studentProgramParticipation : studentPrograms) {
                String programCode = studentProgramParticipation.getProgramCode();
                if (ASPEN_PROGRAM_CODE_SE.equals(programCode)) {
                    // No translation of DfE SEN Provision code, save as is.
                    String type = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_SEN_TYPE,
                            getDataDictionary());
                    String rank = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_SEN_RANK,
                            getDataDictionary());
                    // Not translation of DfE SEN Type code, save as is.
                    sENProvision = (String) studentProgramParticipation.getFieldValueByAlias(ALIAS_NAME_SEN_PROVISION,
                            getDataDictionary());
                    DfESENNeed sENNeed = new DfESENNeed(type, rank);
                    dfEPupil.addSENNeed(sENNeed);

                    sENStartDate = studentProgramParticipation.getStartDate();
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
                String sAON = (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_SAON, getDataDictionary());
                dfEAddress.setSAON(sAON);
                dfEAddress.setStreet(studentAddress.getStreetName());
                if (!StringUtils.isEmpty(studentAddress.getCity())) {
                    dfEAddress.setTown(studentAddress.getCity());
                }
                String locality =
                        (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_LOCALITY, getDataDictionary());
                dfEAddress.setLocality(locality);
                String adminArea =
                        (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_ADMIN_AREA, getDataDictionary());
                dfEAddress.setAdministrativeArea(adminArea);
                String postTown =
                        (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_POST_TOWN, getDataDictionary());
                dfEAddress.setPostTown(postTown);
                String uniquePropertyReferenceNumber = (String) studentAddress
                        .getFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM, getDataDictionary());
                dfEAddress.setUniquePropertyReferenceNumber(uniquePropertyReferenceNumber);
            } else {
                if (!StringUtils.isEmpty(studentAddress.getAddressLine01())) {
                    dfEAddress.setAddressLine1(studentAddress.getAddressLine01());
                }
                if (!StringUtils.isEmpty(studentAddress.getAddressLine02())) {
                    dfEAddress.setAddressLine2(studentAddress.getAddressLine02());
                }
                if (!StringUtils.isEmpty(studentAddress.getAddressLine03())) {
                    dfEAddress.setAddressLine3(studentAddress.getAddressLine03());
                }
                String addressLine4 =
                        (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4, getDataDictionary());
                if (!StringUtils.isEmpty(addressLine4)) {
                    dfEAddress.setAddressLine4(addressLine4);
                }
                String addressLine5 =
                        (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5, getDataDictionary());
                if (!StringUtils.isEmpty(addressLine5)) {
                    dfEAddress.setAddressLine5(addressLine5);
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
            String easting = (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_EASTING, getDataDictionary());
            if (!StringUtils.isEmpty(easting)) {
                dfEAddress.setEasting(easting);
            }
            String northing = (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_NORTHING, getDataDictionary());
            if (!StringUtils.isEmpty(northing)) {
                dfEAddress.setNorthing(northing);
            }

            dfEPupil.setDfEAddress(dfEAddress);
        }


        // Add Telephones and Email
        if (!StringUtils.isEmpty(studentPerson.getPhone01())) {
            String telephoneType1 =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1, getDataDictionary());
            DfETelephone dfETelephone = new DfETelephone(telephoneType1, studentPerson.getPhone01());
            dfEPupil.addTelephone(dfETelephone);
        }
        if (!StringUtils.isEmpty(studentPerson.getPhone02())) {
            String telephoneType2 =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2, getDataDictionary());
            DfETelephone dfETelephone = new DfETelephone(telephoneType2, studentPerson.getPhone02());
            dfEPupil.addTelephone(dfETelephone);
        }
        if (!StringUtils.isEmpty(studentPerson.getPhone03())) {
            String telephoneType3 =
                    (String) studentPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3, getDataDictionary());
            DfETelephone dfETelephone = new DfETelephone(telephoneType3, studentPerson.getPhone03());
            dfEPupil.addTelephone(dfETelephone);
        }

        if (!StringUtils.isEmpty(studentPerson.getEmail01())) {
            dfEPupil.setEmail(studentPerson.getEmail01());
        }

        Collection<StudentContact> studentContacts = student.getContacts();
        for (StudentContact studentContact : studentContacts) {
            Contact contact = studentContact.getContact();
            Person contactPerson = contact.getPerson();

            DfEContact dfEContact = new DfEContact();

            String order = (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_ORDER, getDataDictionary());
            if (StringUtils.isInteger(order)) {
                dfEContact.setOrder(Integer.parseInt(order));
            } else {
                dfEContact.setOrder(0);
            }
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
                String genderCode =
                        lookupStateValueByRefCode(Person.class, Person.COL_GENDER_CODE, contactPerson.getGenderCode());
                dfEContact.setGender(genderCode);
            }
            String responsibleStr =
                    (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_RESPONSIBLE, getDataDictionary());
            if (DB_TRUE.equals(responsibleStr)) {
                dfEContact.setResponsible(BOOLEAN_TRUE);
            } else {
                dfEContact.setResponsible(BOOLEAN_FALSE);
            }

            if (!StringUtils.isEmpty(studentContact.getRelationshipCode())) {
                // Translate Aspen Gender Code to DfE Gender Code.
                String relationshipCode = lookupStateValueByRefCode(StudentContact.class,
                        StudentContact.COL_RELATIONSHIP_CODE, studentContact.getRelationshipCode());
                dfEContact.setRelationshipCode(relationshipCode);
            }

            // Add Telephones and Email
            if (!StringUtils.isEmpty(contactPerson.getPhone01())) {
                String telephoneType1 =
                        (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_1, getDataDictionary());
                DfETelephone dfETelephone = new DfETelephone(telephoneType1, contactPerson.getPhone01());
                dfEContact.addTelephone(dfETelephone);
            }
            if (!StringUtils.isEmpty(contactPerson.getPhone02())) {
                String telephoneType2 =
                        (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_2, getDataDictionary());
                DfETelephone dfETelephone = new DfETelephone(telephoneType2, contactPerson.getPhone02());
                dfEContact.addTelephone(dfETelephone);
            }
            if (!StringUtils.isEmpty(contactPerson.getPhone03())) {
                String telephoneType3 =
                        (String) contactPerson.getFieldValueByAlias(ALIAS_NAME_TELEPHONE_TYPE_3, getDataDictionary());
                DfETelephone dfETelephone = new DfETelephone(telephoneType3, contactPerson.getPhone03());
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
                    dfEContact.setAddressAsPupil(BOOLEAN_TRUE);
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
                        String sAON =
                                (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_SAON, getDataDictionary());
                        dfEAddress.setSAON(sAON);
                        dfEAddress.setStreet(contactAddress.getStreetName());
                        if (!StringUtils.isEmpty(contactAddress.getCity())) {
                            dfEAddress.setTown(contactAddress.getCity());
                        }
                        String locality =
                                (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_LOCALITY, getDataDictionary());
                        dfEAddress.setLocality(locality);
                        String adminArea = (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_ADMIN_AREA,
                                getDataDictionary());
                        dfEAddress.setAdministrativeArea(adminArea);
                        String postTown =
                                (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_POST_TOWN, getDataDictionary());
                        dfEAddress.setPostTown(postTown);
                        String uniquePropertyReferenceNumber = (String) contactAddress
                                .getFieldValueByAlias(ALIAS_NAME_UNIQUE_PROP_REF_NUM, getDataDictionary());
                        dfEAddress.setUniquePropertyReferenceNumber(uniquePropertyReferenceNumber);
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
                        String addressLine4 = (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_4,
                                getDataDictionary());
                        if (!StringUtils.isEmpty(addressLine4)) {
                            dfEAddress.setAddressLine4(addressLine4);
                        }
                        String addressLine5 = (String) contactAddress.getFieldValueByAlias(ALIAS_NAME_ADDRESS_LINE_5,
                                getDataDictionary());
                        if (!StringUtils.isEmpty(addressLine5)) {
                            dfEAddress.setAddressLine5(addressLine5);
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
                    String easting =
                            (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_EASTING, getDataDictionary());
                    if (!StringUtils.isEmpty(easting)) {
                        dfEAddress.setEasting(easting);
                    }
                    String northing =
                            (String) studentAddress.getFieldValueByAlias(ALIAS_NAME_NORTHING, getDataDictionary());
                    if (!StringUtils.isEmpty(northing)) {
                        dfEAddress.setNorthing(northing);
                    }

                    dfEContact.setDfEAddress(dfEAddress);
                }
            }

            dfEPupil.addContact(dfEContact);

        }

        // Don't return for ATF
        // Get Student Attendance Summary
        // TODO Remove later
        String attendanceXML = (String) student.getFieldValueByAlias(ALIAS_NAME_ATTENDANCE_XML, getDataDictionary());
        if (!StringUtils.isEmpty(attendanceXML)) {
            dfEPupil.setAttendanceXML(attendanceXML);
        }

        // Add all old and recently created Student Attendance
        ArrayList<DfEAttendance> studentAttendances =
                getAttendance(student, attendanceHistories, attendances, attendancesByDate, attendanceXML);
        dfEPupil.addAllAttendances(studentAttendances);

        // Don't return for ATF
        // Get Student Assessment Summary
        // TODO Remove later
        String assessmentsXML = (String) student.getFieldValueByAlias(ALIAS_NAME_ASSESSMENTS_XML, getDataDictionary());
        if (!StringUtils.isEmpty(assessmentsXML)) {
            dfEPupil.setAssessmentsXML(assessmentsXML);
        }

        // Add all old and recently created Student Assessments
        ArrayList<DfEStageAssessment> studentAssessments = getStageAssessment(student, assessments, assessmentsXML);
        dfEPupil.addAllStageAssessment(studentAssessments);

        // Get School History Summary
        // Set Previously imported CTF Student School History
        // TODO Remove later
        String schoolHistoryXML =
                (String) student.getFieldValueByAlias(ALIAS_NAME_SCHOOL_HISTORY_XML, getDataDictionary());
        if (!StringUtils.isEmpty(schoolHistoryXML)) {
            dfEPupil.setSchoolHistoryXML(schoolHistoryXML);
        }

        // Add all old and recently created Student School histories
        ArrayList<DfESchoolHistory> studentSchoolHistories = getSchoolHistory(student, enrollments, schoolHistoryXML);
        dfEPupil.addAllSchoolHistory(studentSchoolHistories);

        return dfEPupil;
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
     * Get a FSMHistory Element from DfEPupil.
     *
     * @param dfEPupil DfEPupil
     * @return Element
     */
    public Element getFSMHistoryElement(DfEPupil dfEPupil) {
        Element fSMHistoryElement = null;

        if (dfEPupil.getFSMInstances() != null && dfEPupil.getFSMInstances().size() > 0) {
            fSMHistoryElement = new Element(DfEPupil.ELEMENT_FSM_HISTORY);

            if (dfEPupil.getFSMReviewDate() != null) {
                Element fSMReviewDateElement = new Element(DfEPupil.ELEMENT_FSM_REVIEW_DATE);
                fSMReviewDateElement.addContent(m_dateFormat.format(dfEPupil.getFSMReviewDate()));
                fSMHistoryElement.addContent(fSMReviewDateElement);
            }

            Collection<DfEFSMInstance> fSMInstances = dfEPupil.getFSMInstances();
            for (DfEFSMInstance dfEFSMInstance : fSMInstances) {
                Element fSMInstanceElement = new Element(DfEFSMInstance.ELEMENT_FSM_INSTANCE);

                if (dfEFSMInstance.getFSMStartDate() != null) {
                    Element fSMStartDateElement = new Element(DfEFSMInstance.ELEMENT_FSM_START_DATE);
                    fSMStartDateElement.addContent(m_dateFormat.format(dfEFSMInstance.getFSMStartDate()));
                    fSMInstanceElement.addContent(fSMStartDateElement);
                }
                if (dfEFSMInstance.getFSMEndDate() != null) {
                    Element fSMEndDateElement = new Element(DfEFSMInstance.ELEMENT_FSM_END_DATE);
                    fSMEndDateElement.addContent(m_dateFormat.format(dfEFSMInstance.getFSMEndDate()));
                    fSMInstanceElement.addContent(fSMEndDateElement);
                }
                if (dfEFSMInstance.getUKCountry() != null) {
                    Element fSMUKCountryElement = new Element(DfEFSMInstance.ELEMENT_FSM_UK_COUNTRY);
                    fSMUKCountryElement.addContent(dfEFSMInstance.getUKCountry());
                    fSMInstanceElement.addContent(fSMUKCountryElement);
                }

                fSMHistoryElement.addContent(fSMInstanceElement);
            }

        }

        return fSMHistoryElement;
    }

    /**
     * Get Header Element from DfEHeader
     *
     * Note: Not include SuppInfo.
     *
     * @param dfEHeader DfEHeader
     * @return Element
     */
    public Element getHeaderElement(DfEHeader dfEHeader) {
        Element headerElement = new Element(DfEHeader.ELEMENT_HEADER);

        Element documentNameElement = new Element(DfEHeader.ELEMENT_DOCUMENT_NAME);
        documentNameElement.addContent(dfEHeader.getDocumentName());
        headerElement.addContent(documentNameElement);
        Element versionElement = null;
        if (REPORT_TYPE_CTF.equals(dfEHeader.getHeaderType())) {
            versionElement = new Element(DfEHeader.ELEMENT_CTF_VERSION);
        } else if (REPORT_TYPE_ATF.equals(dfEHeader.getHeaderType())) {
            versionElement = new Element(DfEHeader.ELEMENT_ATF_VERSION);
        }

        if (versionElement != null) {
            versionElement.addContent(dfEHeader.getVersion());
            headerElement.addContent(versionElement);
        }

        Element dateTimeElement = new Element(DfEHeader.ELEMENT_DATE_TIME);
        dateTimeElement.addContent(dfEHeader.getDateTime());
        headerElement.addContent(dateTimeElement);
        if (dfEHeader.getDocumentQualifier() != null) {
            Element documentQualifierElement = new Element(DfEHeader.ELEMENT_DOCUMENT_QUALIFIER);
            documentQualifierElement.addContent(dfEHeader.getDocumentQualifier());
            headerElement.addContent(documentQualifierElement);
        }
        if (dfEHeader.getDataQualifier() != null) {
            Element dateQualifierElement = new Element(DfEHeader.ELEMENT_DATA_QUALIFIER);
            dateQualifierElement.addContent(dfEHeader.getDataQualifier());
            headerElement.addContent(dateQualifierElement);
        }
        if (dfEHeader.getDataDescriptor() != null) {
            Element dateDescriptorElement = new Element(DfEHeader.ELEMENT_DATA_DESCRIPTOR);
            dateDescriptorElement.addContent(dfEHeader.getDataDescriptor());
            headerElement.addContent(dateDescriptorElement);
        }
        if (dfEHeader.getSupplierID() != null) {
            Element supplierIDElement = new Element(DfEHeader.ELEMENT_SUPPLIER_ID);
            supplierIDElement.addContent(dfEHeader.getSupplierID());
            headerElement.addContent(supplierIDElement);
        }

        // Element SourceSchool
        Element sourceSchoolElement = new Element(DfEHeader.ELEMENT_SOURCE_SCHOOL);
        Element sourceSchoolLEAElement = new Element(DfEHeader.ELEMENT_LEA);
        sourceSchoolLEAElement.addContent(dfEHeader.getSourceSchoolLEA());
        sourceSchoolElement.addContent(sourceSchoolLEAElement);
        Element sourceSchoolEstabElement = new Element(DfEHeader.ELEMENT_ESTABISHMENT);
        sourceSchoolEstabElement.addContent(dfEHeader.getSourceSchoolEstab());
        sourceSchoolElement.addContent(sourceSchoolEstabElement);
        Element sourceSchoolNameElement = new Element(DfEHeader.ELEMENT_SCHOOL_NAME);
        sourceSchoolNameElement.addContent(dfEHeader.getSourceSchoolName());
        sourceSchoolElement.addContent(sourceSchoolNameElement);
        Element sourceSchoolAcademicYearElement = new Element(DfEHeader.ELEMENT_ACADEMIC_YEAR);
        sourceSchoolAcademicYearElement.addContent(dfEHeader.getSourceSchoolAcademicYear());
        sourceSchoolElement.addContent(sourceSchoolAcademicYearElement);
        headerElement.addContent(sourceSchoolElement);

        // Element DestSchool
        Element destSchoolElement = new Element(DfEHeader.ELEMENT_DESTINATION_SCHOOL);
        Element destSchoolLEAElement = new Element(DfEHeader.ELEMENT_LEA);
        destSchoolLEAElement.addContent(dfEHeader.getDestSchoolLEA());
        destSchoolElement.addContent(destSchoolLEAElement);
        Element destSchoolEstabElement = new Element(DfEHeader.ELEMENT_ESTABISHMENT);
        destSchoolEstabElement.addContent(dfEHeader.getDestSchoolEstab());
        destSchoolElement.addContent(destSchoolEstabElement);
        headerElement.addContent(destSchoolElement);

        return headerElement;
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

            if (dfELanguage.getLanguageType() != null) {
                Element languageTypeElement = new Element(DfELanguage.ELEMENT_LANGUAGE_TYPE);
                languageTypeElement.addContent(dfELanguage.getLanguageType());
                typeElement.addContent(languageTypeElement);
            }
            if (dfELanguage.getLanguageCode() != null) {
                Element languageCodeElement = new Element(DfELanguage.ELEMENT_LANGUAGE_CODE);
                languageCodeElement.addContent(dfELanguage.getLanguageCode());
                typeElement.addContent(languageCodeElement);
            }

            languagesElement.addContent(typeElement);
        }

        return languagesElement;
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
            Boolean inCare = dfEPupil.getInCare();
            if (inCare.booleanValue()) {
                inCareElement.addContent(STRING_TRUE);
            } else {
                inCareElement.addContent(STRING_FALSE);
            }

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
     * Get a NAWDetails Element from DfEPupil.
     *
     * @param dfEPupil DfEPupil
     * @return Element
     */
    public Element getNAWDetailsElement(DfEPupil dfEPupil) {
        Element nAWDetailsElement = null;

        if (dfEPupil.getNAWDetails() != null && dfEPupil.getNAWDetails().size() > 0) {
            nAWDetailsElement = new Element(DfEPupil.ELEMENT_NAW_DETAILS);

            ArrayList<DfENAWDetail> nAWDetails = dfEPupil.getNAWDetails();

            // Only interested in the first one.
            DfENAWDetail nAWDetail = nAWDetails.get(0);

            if (nAWDetail.getSpeakWelsh() != null) {
                Element speakWelshElement = new Element(DfENAWDetail.ELEMENT_SPEAK_WELSH);
                speakWelshElement.addContent(nAWDetail.getSpeakWelsh());
                nAWDetailsElement.addContent(speakWelshElement);
            }
            if (nAWDetail.getHomeWelsh() != null) {
                Element homeWelshElement = new Element(DfENAWDetail.ELEMENT_HOME_WELSH);
                homeWelshElement.addContent(nAWDetail.getHomeWelsh());
                nAWDetailsElement.addContent(homeWelshElement);
            }
            if (nAWDetail.getNationalIdentity() != null) {
                Element nationalIdentityElement = new Element(DfENAWDetail.ELEMENT_NATIONAL_IDENTITY);
                nationalIdentityElement.addContent(nAWDetail.getNationalIdentity());
                nAWDetailsElement.addContent(nationalIdentityElement);
            }
            if (nAWDetail.getWelshSource() != null) {
                Element welshSourceElement = new Element(DfENAWDetail.ELEMENT_WELSH_SOURCE);
                welshSourceElement.addContent(nAWDetail.getWelshSource());
                nAWDetailsElement.addContent(welshSourceElement);
            }
            if (nAWDetail.getEALAcquisition() != null) {
                Element eALAcquisitionElement = new Element(DfENAWDetail.ELEMENT_EAL_ACQUISITION);
                eALAcquisitionElement.addContent(nAWDetail.getEALAcquisition());
                nAWDetailsElement.addContent(eALAcquisitionElement);
            }
            if (nAWDetail.getLanguageSource() != null) {
                Element languageSourceElement = new Element(DfENAWDetail.ELEMENT_LANGAUGE_SOURCE);
                languageSourceElement.addContent(nAWDetail.getLanguageSource());
                nAWDetailsElement.addContent(languageSourceElement);
            }
            if (nAWDetail.getSENCurrTeachingMethods() != null) {
                Element sENCurrTeachingMethodsElement = new Element(DfENAWDetail.ELEMENT_SEN_CURR_TEACHING_METHODS);
                sENCurrTeachingMethodsElement.addContent(nAWDetail.getSENCurrTeachingMethods());
                nAWDetailsElement.addContent(sENCurrTeachingMethodsElement);
            }
            if (nAWDetail.getSENGroupingAndSupport() != null) {
                Element sENGroupingAndSupportElement = new Element(DfENAWDetail.ELEMENT_SEN_GROUPINGS_SUPPORT);
                sENGroupingAndSupportElement.addContent(nAWDetail.getSENGroupingAndSupport());
                nAWDetailsElement.addContent(sENGroupingAndSupportElement);
            }
            if (nAWDetail.getSENSpecialisedResources() != null) {
                Element sENSpecialisedResourcesElement = new Element(DfENAWDetail.ELEMENT_SEN_SPECIALISED_RESOURCES);
                sENSpecialisedResourcesElement.addContent(nAWDetail.getSENSpecialisedResources());
                nAWDetailsElement.addContent(sENSpecialisedResourcesElement);
            }
            if (nAWDetail.getSENAdviceAndAssessment() != null) {
                Element sENAdviceAndAssessmentElement = new Element(DfENAWDetail.ELEMENT_SEN_ADVICE_ASSESSMENT);
                sENAdviceAndAssessmentElement.addContent(nAWDetail.getSENAdviceAndAssessment());
                nAWDetailsElement.addContent(sENAdviceAndAssessmentElement);
            }
        }

        return nAWDetailsElement;
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

            // Optional, Hide default
            if (dfEtelephone.getTelephoneType() != null) {
                Element telephoneTypeElement = new Element(DfETelephone.ELEMENT_TELEPHONE_TYPE);
                telephoneTypeElement.addContent(dfEtelephone.getTelephoneType());
                phoneElement.addContent(telephoneTypeElement);
            }

            Element telephoneNumberElement = new Element(DfETelephone.ELEMENT_PHONE_NUMBER);
            telephoneNumberElement.addContent(dfEtelephone.getTelephoneNumber());
            phoneElement.addContent(telephoneNumberElement);

            phonesElement.addContent(phoneElement);
        }

        return phonesElement;
    }

    /**
     * Get a DfE Pupil Data from DfEPupil
     *
     * Note: Not including SuppInfo Elements.
     *
     * @param reportType String
     * @param dfEPupil DfEPupil
     * @param activeSections
     * @return Element
     */
    public Element getPupilDataElement(String reportType, DfEPupil dfEPupil, HashMap<String, Boolean> activeSections) {
        Element pupilElement = new Element(DfEPupil.ELEMENT_PUPIL);

        // Pupil Core Fields
        if (REPORT_TYPE_ATF.equals(reportType)) {
            if (!StringUtils.isEmpty(dfEPupil.getApplicationReference())) {
                Element applicationReferenceElement = new Element(DfEPupil.ELEMENT_APPLICATION_REFERENCE);
                applicationReferenceElement.addContent(dfEPupil.getApplicationReference());
                pupilElement.addContent(applicationReferenceElement);
            }
        }
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
        if (REPORT_TYPE_CTF.equals(reportType)) {
            if (!StringUtils.isEmpty(dfEPupil.getUniqueCandidateIdentifier())) {
                Element uniqueCandidateIdentifierElement = new Element(DfEPupil.ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER);
                uniqueCandidateIdentifierElement.addContent(dfEPupil.getUniqueCandidateIdentifier());
                pupilElement.addContent(uniqueCandidateIdentifierElement);
            }
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

        // Pupil Basic Details
        if (isSectionActive(activeSections, PARAM_BASIC_DETAILS)) {
            Element basicDetailsElement = getBasicDetailsElement(dfEPupil);
            pupilElement.addContent(basicDetailsElement);
        }

        // FSMHistory fields
        if (isSectionActive(activeSections, PARAM_FSM_HISTORY)) {
            if (dfEPupil.getFSMInstances() != null && dfEPupil.getFSMInstances().size() > 0) {
                Element fSMHistoryElement = getFSMHistoryElement(dfEPupil);

                pupilElement.addContent(fSMHistoryElement);
            }
        }

        // NAW Details Fields
        if (isSectionActive(activeSections, PARAM_NAW_DETAILS)) {
            if (REPORT_TYPE_CTF.equals(reportType)) {
                if (dfEPupil.getNAWDetails() != null && dfEPupil.getNAWDetails().size() > 0) {
                    Element nAWDetailsElement = getNAWDetailsElement(dfEPupil);

                    pupilElement.addContent(nAWDetailsElement);
                }
            }
        }

        // Looked After Fields
        if (isSectionActive(activeSections, PARAM_LOOKED_AFTER)) {
            if (dfEPupil.getInCare() != null) {
                Element lookedAfterElement = getLookAfterElement(dfEPupil);
                pupilElement.addContent(lookedAfterElement);
            }
        }

        // SEN History Fields
        if (isSectionActive(activeSections, PARAM_SEN_HISTORY)) {
            if (dfEPupil.getSENNeeds() != null && dfEPupil.getSENNeeds().size() > 0) {
                Element sENHistoryElement = getSENHistoryElement(dfEPupil);

                pupilElement.addContent(sENHistoryElement);
            }
        }

        // Admissions Fields
        if (isSectionActive(activeSections, PARAM_ADMISSIONS)) {
            if (REPORT_TYPE_ATF.equals(reportType)) {
                Element admissionsElement = getAdmissionsElement();

                pupilElement.addContent(admissionsElement);
            }
        }

        // Address Fields
        if (isSectionActive(activeSections, PARAM_ADDRESS_PHONE_EMAIL)) {
            if (dfEPupil.getDfEAddress() != null) {
                Element addressElement = getAddressElement(dfEPupil.getDfEAddress());

                pupilElement.addContent(addressElement);
            }

            // Telephone Fields
            if (dfEPupil.getTelephones() != null && dfEPupil.getTelephones().size() > 0) {
                Element phonesElement = getPhonesElement(dfEPupil.getTelephones());

                pupilElement.addContent(phonesElement);
            }

            if (!StringUtils.isEmpty(dfEPupil.getEmail())) {
                Element emailElement = new Element(DfEPupil.ELEMENT_EMAIL);
                emailElement.addContent(dfEPupil.getEmail());
                pupilElement.addContent(emailElement);
            }
        }

        // Contacts Fields
        if (isSectionActive(activeSections, PARAM_CONTACTS)) {
            if (dfEPupil.getContacts() != null && dfEPupil.getContacts().size() > 0) {
                Element contactsElement = getContacts(dfEPupil.getContacts());
                pupilElement.addContent(contactsElement);
            }
        }

        if (REPORT_TYPE_CTF.equals(reportType)) {
            // Pupil Attendance Fields
            if (isSectionActive(activeSections, PARAM_ATTENDANCE)) {
                if (dfEPupil.getAttendances() != null && dfEPupil.getAttendances().size() > 0) {
                    Element attendanceElement = getAttendanceElement(dfEPupil, true);
                    pupilElement.addContent(attendanceElement);
                }
            }

            // Pupil Assessments Fields
            if (isSectionActive(activeSections, PARAM_ASSESSMENTS)) {
                if (dfEPupil.getStageAssessments() != null && dfEPupil.getStageAssessments().size() > 0) {
                    Element stateAssessmentsElement = getStageAssessmentsElement(dfEPupil);
                    pupilElement.addContent(stateAssessmentsElement);
                }
            }
        }

        // Pupil School History Fields
        if (isSectionActive(activeSections, PARAM_SCHOOL_HISTORY)) {
            if (dfEPupil.getSchoolHistories() != null && dfEPupil.getSchoolHistories().size() > 0) {
                Element schoolHistoryElement = getSchoolHistoryElement(dfEPupil);
                pupilElement.addContent(schoolHistoryElement);
            }
        }

        return pupilElement;
    }

    /**
     * Get all Student School History
     * Use any Imported CTF/ATF SchoolHistories as well as any Aspen school's from StudentEnrollment
     * records.
     *
     * @param student Student
     * @param enrollments Collection<StudentEnrollment>
     * @param schoolHistoryXML String
     * @return ArrayList<DfESchoolHistory>
     */
    public ArrayList<DfESchoolHistory> getSchoolHistory(Student student,
                                                        Collection<StudentEnrollment> enrollments,
                                                        String schoolHistoryXML) {
        ArrayList<DfESchoolHistory> schoolHistories = new ArrayList();

        // TODO Remove later
        // All previously imported CTF school history and add to school history list
        /*
         * if (!StringUtils.isEmpty(schoolHistoryXML))
         * {
         * ArrayList<DfESchoolHistory> oldStudentHistories =
         * parseSchoolHistoryXML(schoolHistoryXML);
         * 
         * if (oldStudentHistories != null && oldStudentHistories.size() > 0)
         * {
         * schoolHistories.addAll(oldStudentHistories);
         * }
         * }
         */

        String cTFSchoolId = m_cTFSchool.getOid();

        // Create two maps containing enrollment and withdrawals from the student's
        // StudentEnrollments records
        HashMap<String, StudentEnrollment> schoolEnroll = new HashMap();
        HashMap<String, StudentEnrollment> schoolWithdrawl = new HashMap();
        StudentEnrollment prevStudentEnrollment = null;
        if (enrollments != null && enrollments.size() > 0) {
            // SchoolId is a unique identifier can be a Aspen schoolOid or a UK DfE Establishment
            // Number
            String schoolId = null;
            String prevSchoolId = null;
            String currEnrollmentType = null;
            String prevEnrollmentType = null;
            int k = 0; // over all
            int l = 0; // change of school
            for (StudentEnrollment studentEnrollment : enrollments) {
                k++;
                l++;

                // If the Enrollment record is referencing the CTF School, get the Estab Number from
                // the UDF
                schoolId = studentEnrollment.getSchoolOid();
                if (cTFSchoolId.equals(schoolId)) {
                    schoolId = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                            getDataDictionary());
                }

                currEnrollmentType = studentEnrollment.getEnrollmentType();
                if (!schoolId.equals(prevSchoolId)) {
                    l = 0;
                }
                if (l == 0
                        && StudentEnrollment.ENTRY.equalsIgnoreCase(currEnrollmentType)) {
                    schoolEnroll.put(schoolId, studentEnrollment);
                }
                if (k > 1
                        && !schoolId.equals(prevSchoolId)
                        && StudentEnrollment.WITHDRAWAL.equalsIgnoreCase(prevEnrollmentType)) {
                    schoolWithdrawl.put(prevSchoolId, prevStudentEnrollment);
                }

                prevSchoolId = schoolId;
                prevEnrollmentType = currEnrollmentType;
                prevStudentEnrollment = studentEnrollment;
            }
            if (l == 0
                    && StudentEnrollment.ENTRY.equalsIgnoreCase(prevEnrollmentType)) {
                schoolEnroll.put(prevSchoolId, prevStudentEnrollment);
            }
            if (!schoolId.equals(prevSchoolId)
                    && StudentEnrollment.WITHDRAWAL.equalsIgnoreCase(prevEnrollmentType)) {
                schoolWithdrawl.put(prevSchoolId, prevStudentEnrollment);
            }

            // Add to DfESchoolHistory objects from the enrollment maps.
            Collection<StudentEnrollment> studentEnrollments = schoolEnroll.values();
            for (StudentEnrollment studentEnrollment : studentEnrollments) {
                // If the Enrollment record is referencing the CTF School, get the Estab Number from
                // the Enrollment record UDF
                String currSchoolId = studentEnrollment.getSchoolOid();
                if (cTFSchoolId.equals(currSchoolId)) {
                    currSchoolId = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                            getDataDictionary());
                }

                School school = studentEnrollment.getSchool();
                if (school != null) {
                    String lEA = null;
                    String estab = null;
                    String schoolName = null;

                    // If the Enrollment record is referencing the CTF School, get the school
                    // information from the Enrollment record UDF's
                    if (cTFSchoolId.equals(studentEnrollment.getSchoolOid())) {
                        lEA = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ID,
                                getDataDictionary());
                        estab = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB,
                                getDataDictionary());
                        schoolName = (String) studentEnrollment.getFieldValueByAlias(ALIAS_NAME_OUT_OF_LEA_ESTAB_NAME,
                                getDataDictionary());
                    } else {
                        Organization organization = school.getOrganization1();
                        lEA = (String) organization.getFieldValueByAlias(ALIAS_NAME_LEA, getDataDictionary());
                        estab = (String) school.getFieldValueByAlias(ALIAS_NAME_ESTAB, getDataDictionary());
                        schoolName = school.getName();
                    }

                    Boolean lastSchool = BOOLEAN_FALSE;
                    PlainDate entryDate = studentEnrollment.getEnrollmentDate();
                    PlainDate leavingDate = null;
                    String leavingReason = null;

                    if (schoolWithdrawl.containsKey(currSchoolId)) {
                        StudentEnrollment studentWithdrawl = schoolWithdrawl.get(currSchoolId);
                        leavingDate = studentWithdrawl.getEnrollmentDate();
                        leavingReason = studentWithdrawl.getEnrollmentCode();
                    }
                    if (student.getSchoolOid().equals(currSchoolId)) {
                        lastSchool = BOOLEAN_TRUE;
                    }

                    DfESchoolHistory schoolHistory = new DfESchoolHistory(lEA, estab, schoolName, entryDate,
                            leavingDate, leavingReason, lastSchool);
                    schoolHistories.add(schoolHistory);
                }
            }

        }

        return schoolHistories;
    }

    /**
     * Get a SchoolHistory Element from DfEPupil.
     *
     * @param dfEPupil DfEPupil
     * @return Element
     */
    public Element getSchoolHistoryElement(DfEPupil dfEPupil) {
        Element schoolHistoryElement = null;

        if (dfEPupil.getSchoolHistories() != null && dfEPupil.getSchoolHistories().size() > 0) {
            schoolHistoryElement = new Element(DfEPupil.ELEMENT_SCHOOL_HISTORY);

            ArrayList<DfESchoolHistory> schoolHistories = dfEPupil.getSchoolHistories();

            // Sort by Entry Date
            TreeMap<String, DfESchoolHistory> schoolHistoriesMap = new TreeMap<String, DfESchoolHistory>();
            for (DfESchoolHistory dfESchoolHistory : schoolHistories) {
                String entryDateStr = STRING_EMPTY;
                if (dfESchoolHistory.getEntryDate() != null) {
                    entryDateStr = m_dateFormat.format(dfESchoolHistory.getEntryDate());
                }
                String key = entryDateStr + ":" + dfESchoolHistory.getSchoolName();
                schoolHistoriesMap.put(key, dfESchoolHistory);
            }

            for (DfESchoolHistory dfESchoolHistory : schoolHistoriesMap.values()) {
                Element schoolElement = new Element(DfESchoolHistory.ELEMENT_SCHOOL);

                if (dfESchoolHistory.getLEA() != null) {
                    Element lEAElement = new Element(DfESchoolHistory.ELEMENT_LEA);
                    lEAElement.addContent(dfESchoolHistory.getLEA());
                    schoolElement.addContent(lEAElement);
                }
                if (dfESchoolHistory.getEstab() != null) {
                    Element estabElement = new Element(DfESchoolHistory.ELEMENT_ESTAB);
                    estabElement.addContent(dfESchoolHistory.getEstab());
                    schoolElement.addContent(estabElement);
                }
                if (dfESchoolHistory.getSchoolName() != null) {
                    Element schoolNameElement = new Element(DfESchoolHistory.ELEMENT_SCHOOL_NAME);
                    schoolNameElement.addContent(dfESchoolHistory.getSchoolName());
                    schoolElement.addContent(schoolNameElement);
                }
                if (dfESchoolHistory.getEntryDate() != null) {
                    Element entryDateElement = new Element(DfESchoolHistory.ELEMENT_ENTRY_DATE);
                    entryDateElement.addContent(m_dateFormat.format(dfESchoolHistory.getEntryDate()));
                    schoolElement.addContent(entryDateElement);
                }
                if (dfESchoolHistory.getLeavingDate() != null) {
                    Element leavingDateElement = new Element(DfESchoolHistory.ELEMENT_LEAVING_DATE);
                    leavingDateElement.addContent(m_dateFormat.format(dfESchoolHistory.getLeavingDate()));
                    schoolElement.addContent(leavingDateElement);
                }
                if (dfESchoolHistory.getLeavingReason() != null) {
                    Element leavingReasonElement = new Element(DfESchoolHistory.ELEMENT_LEAVING_REASON);
                    leavingReasonElement.addContent(dfESchoolHistory.getLeavingReason());
                    schoolElement.addContent(leavingReasonElement);
                }
                if (dfESchoolHistory.getLastSchool() != null) {
                    Element lastSchoolElement = new Element(DfESchoolHistory.ELEMENT_LAST_SCHOOL);
                    Boolean lastSchool = dfESchoolHistory.getLastSchool();
                    if (lastSchool.booleanValue()) {
                        lastSchoolElement.addContent(STRING_TRUE);
                        schoolElement.addContent(lastSchoolElement);
                    }
                }

                schoolHistoryElement.addContent(schoolElement);
            }

        }

        return schoolHistoryElement;
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
     * Get all Student Stage Assessment
     * Use any Imported CTF/ATF SchoolHistories as well as any Aspen school's from StudentAssessment
     * records.
     *
     * @param student Student
     * @param assessments Collection<StudentAssessment>
     * @param stageAssessmentXML String
     * @return ArrayList<DfEStageAssessment>
     */
    public ArrayList<DfEStageAssessment> getStageAssessment(Student student,
                                                            Collection<StudentAssessment> assessments,
                                                            String stageAssessmentXML) {
        ArrayList<DfEStageAssessment> stageAssessments = new ArrayList();

        // TODO Remove later
        // All previously imported CTF school assessments and add to school history list
        /*
         * if (!StringUtils.isEmpty(stageAssessmentXML))
         * {
         * ArrayList<DfEStageAssessment> oldStudentHistories =
         * parseStageAssessmentXML(stageAssessmentXML);
         * 
         * if (oldStudentHistories != null && oldStudentHistories.size() > 0)
         * {
         * stageAssessments.addAll(oldStudentHistories);
         * }
         * }
         */

        // Add to DfEStageAssessment objects from StudentAssessment.
        DataDictionary extendedDataDictionary = null;
        if (assessments != null && assessments.size() > 0) {
            for (StudentAssessment studentAssessment : assessments) {
                if (extendedDataDictionary == null) {
                    extendedDataDictionary = DataDictionary.getDistrictDictionary(
                            studentAssessment.getExtendedDataDictionary(), m_broker.getPersistenceKey());
                }

                String stage =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_STAGE, extendedDataDictionary);
                String locale =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_LOCALE, extendedDataDictionary);
                String yearTakenStr =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_YEAR_TAKEN, extendedDataDictionary);
                Integer yearTaken = Integer.valueOf(0);
                if (StringUtils.isNumeric(yearTakenStr)) {
                    yearTaken = Integer.valueOf(yearTakenStr);
                }
                String subject =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_SUBJECT, extendedDataDictionary);
                String method =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_METHOD, extendedDataDictionary);
                String component =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_COMPONENT, extendedDataDictionary);
                String resultStatus = (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_RESULT_STATUS,
                        extendedDataDictionary);
                String resultQualifier = (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_RESULT_QUALIFIER,
                        extendedDataDictionary);
                String result =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_RESULT, extendedDataDictionary);
                String resultDateStr =
                        (String) studentAssessment.getFieldValueByAlias(ALIAS_NAME_RESULT_DATE, extendedDataDictionary);

                DfEStageAssessment stageAssessment = new DfEStageAssessment(stage, locale, yearTaken.toString(),
                        subject, method, component, resultStatus, resultQualifier, result, resultDateStr);
                stageAssessments.add(stageAssessment);
            }
        }

        return stageAssessments;
    }


    /**
     * Get a StageAssessment Element from DfEStageAssessment.
     *
     * @param stageAssessment DfEStageAssessment
     * @return Element
     */
    public Element getStageAssessmentElement(DfEStageAssessment stageAssessment) {
        Element stageAssessmentElement = new Element(DfEStageAssessment.ELEMENT_STAGE_ASSESSMENT);

        if (stageAssessment.getLocale() != null) {
            Element localeElement = new Element(DfEStageAssessment.ELEMENT_LOCALE);
            localeElement.addContent(stageAssessment.getLocale());
            stageAssessmentElement.addContent(localeElement);
        }
        if (stageAssessment.getYear() != null) {
            Element yearElement = new Element(DfEStageAssessment.ELEMENT_YEAR);
            yearElement.addContent(stageAssessment.getYear());
            stageAssessmentElement.addContent(yearElement);
        }
        if (stageAssessment.getSubject() != null) {
            Element subjectElement = new Element(DfEStageAssessment.ELEMENT_SUBJECT);
            subjectElement.addContent(stageAssessment.getSubject());
            stageAssessmentElement.addContent(subjectElement);
        }
        if (stageAssessment.getMethod() != null) {
            Element methodElement = new Element(DfEStageAssessment.ELEMENT_METHOD);
            methodElement.addContent(stageAssessment.getMethod());
            stageAssessmentElement.addContent(methodElement);
        }
        if (stageAssessment.getComponent() != null) {
            Element componentElement = new Element(DfEStageAssessment.ELEMENT_COMPONENT);
            componentElement.addContent(stageAssessment.getComponent());
            stageAssessmentElement.addContent(componentElement);
        }
        if (stageAssessment.getResultStatus() != null) {
            Element resultStatusElement = new Element(DfEStageAssessment.ELEMENT_RESULT_STATUS);
            resultStatusElement.addContent(stageAssessment.getResultStatus());
            stageAssessmentElement.addContent(resultStatusElement);
        }
        if (stageAssessment.getResultQualifier() != null) {
            Element resultQualifierElement = new Element(DfEStageAssessment.ELEMENT_RESULT_QUALIFIER);
            resultQualifierElement.addContent(stageAssessment.getResultQualifier());
            stageAssessmentElement.addContent(resultQualifierElement);
        }
        if (stageAssessment.getResult() != null) {
            Element resultElement = new Element(DfEStageAssessment.ELEMENT_RESULT);
            resultElement.addContent(stageAssessment.getResult());
            stageAssessmentElement.addContent(resultElement);
        }
        if (stageAssessment.getResultDate() != null) {
            Element resultDateElement = new Element(DfEStageAssessment.ELEMENT_RESULT_DATE);
            resultDateElement.addContent(m_dateFormat.format(stageAssessment.getResultDate()));
            stageAssessmentElement.addContent(resultDateElement);
        }

        return stageAssessmentElement;
    }

    /**
     * Get a StageAssessments Element from DfEPupil.
     *
     * @param dfEPupil DfEPupil
     * @return Element
     */
    public Element getStageAssessmentsElement(DfEPupil dfEPupil) {
        Element stageAssessmentsElement = null;

        if (dfEPupil.getStageAssessments() != null && dfEPupil.getStageAssessments().size() > 0) {
            stageAssessmentsElement = new Element(DfEPupil.ELEMENT_STAGE_ASSESSMENTS);

            ArrayList<DfEStageAssessment> stageAssessments = dfEPupil.getStageAssessments();

            String currentStage = null;
            String prevStage = null;
            DfEStageAssessment prevStageAssessment = null;

            Element keyStageElement = null;
            Element stageAssessmentElement = null;

            int i = 0;
            for (DfEStageAssessment stageAssessment : stageAssessments) {
                i++;

                currentStage = stageAssessment.getStage();

                if (i == 1) {
                    // Add first KeyStage
                    keyStageElement = new Element(DfEStageAssessment.ELEMENT_KEY_STAGE);

                    Element stageElement = new Element(DfEStageAssessment.ELEMENT_STAGE);
                    if (StringUtils.isEmpty(currentStage)) {
                        stageElement.addContent(STRING_EMPTY);
                    } else {
                        stageElement.addContent(currentStage);
                    }
                    keyStageElement.addContent(stageElement);
                } else {
                    // Add previous StageAssessment
                    stageAssessmentElement = getStageAssessmentElement(prevStageAssessment);

                    keyStageElement.addContent(stageAssessmentElement);

                    // If Stage has changes close the previous and start the next
                    if (!currentStage.equals(prevStage)) {
                        // Close out previous KeyStage
                        stageAssessmentsElement.addContent(keyStageElement);

                        // Start next KeyStage
                        keyStageElement = new Element(DfEStageAssessment.ELEMENT_KEY_STAGE);

                        Element stageElement = new Element(DfEStageAssessment.ELEMENT_STAGE);
                        // currentStage = stageAssessment.getStage();
                        if (StringUtils.isEmpty(currentStage)) {
                            stageElement.addContent(STRING_EMPTY);
                        } else {
                            stageElement.addContent(currentStage);
                        }
                        keyStageElement.addContent(stageElement);
                    }

                    // End if i > 0
                }

                prevStageAssessment = stageAssessment;
                prevStage = prevStageAssessment.getStage();

                // End DfEStageAssessments loop
            }

            // Add Last StageAssessment
            stageAssessmentElement = getStageAssessmentElement(prevStageAssessment);

            keyStageElement.addContent(stageAssessmentElement);

            // Close out last KeyStage
            stageAssessmentsElement.addContent(keyStageElement);
        }

        return stageAssessmentsElement;
    }

    /**
     * Loads a List of Lists of School Year date ranges.
     */
    public void loadSchoolYears() {
        BeanQuery query = new BeanQuery(DistrictSchoolYearContext.class);
        query.addOrderByDescending(DistrictSchoolYearContext.COL_START_DATE);

        Iterator iterator = m_broker.getIteratorByQuery(query);

        m_schoolYearRanges = new ArrayList<ArrayList<PlainDate>>();
        while (iterator.hasNext()) {
            DistrictSchoolYearContext dst = (DistrictSchoolYearContext) iterator.next();

            ArrayList<PlainDate> listOfDates = new ArrayList<PlainDate>();
            listOfDates.add(dst.getStartDate());
            listOfDates.add(dst.getEndDate());
            m_schoolYearRanges.add(listOfDates);
        }
    }

    /**
     * Parse the imported CTF Attendance in an ArrayList for DfEAttendance.
     *
     * @param AttendancesXML String
     * @return ArrayList<DfEAttendance>
     */
    public ArrayList<DfEAttendance> parseAttendanceXML(String AttendancesXML) {
        ArrayList<DfEAttendance> Attendances = new ArrayList();
        Element AttendancesElement = getElementFromXMLString(AttendancesXML);

        if (AttendancesElement != null) {
            List<Element> yearDataElementList = AttendancesElement.getChildren(DfEAttendance.ELEMENT_YEAR_DATA);

            for (int i = 0; i < yearDataElementList.size(); i++) {
                Element yearDataElement = yearDataElementList.get(i);

                DfEAttendance Attendance = new DfEAttendance(yearDataElement);

                Attendances.add(Attendance);
            }
        }

        return Attendances;
    }

    /**
     * Parse the imported CTF/ATF School History in an ArrayList for DfESchoolHistory.
     *
     * @param schoolHistoryXML String
     * @return ArrayList<DfESchoolHistory>
     */
    public ArrayList<DfESchoolHistory> parseSchoolHistoryXML(String schoolHistoryXML) {
        ArrayList<DfESchoolHistory> schoolHistories = new ArrayList();
        Element schoolHistoryElement = getElementFromXMLString(schoolHistoryXML);

        if (schoolHistoryElement != null) {
            List<Element> schoolHistoryElementList = schoolHistoryElement.getChildren(DfESchoolHistory.ELEMENT_SCHOOL);

            for (int i = 0; i < schoolHistoryElementList.size(); i++) {
                Element schoolElement = schoolHistoryElementList.get(i);
                DfESchoolHistory schoolHistory = new DfESchoolHistory(schoolElement);
                // Only the current school will be the last school
                schoolHistory.setLastSchool(BOOLEAN_FALSE);
                schoolHistories.add(schoolHistory);
            }
        }

        return schoolHistories;
    }

    /**
     * Parse the imported CTF Stage Assessment in an ArrayList for DfEStageAssessment.
     *
     * @param stageAssessmentsXML String
     * @return ArrayList<DfEStageAssessment>
     */
    public ArrayList<DfEStageAssessment> parseStageAssessmentXML(String stageAssessmentsXML) {
        ArrayList<DfEStageAssessment> stageAssessments = new ArrayList();
        Element stageAssessmentsElement = getElementFromXMLString(stageAssessmentsXML);

        if (stageAssessmentsElement != null) {
            List<Element> keyStageElementList =
                    stageAssessmentsElement.getChildren(DfEStageAssessment.ELEMENT_KEY_STAGE);

            for (int i = 0; i < keyStageElementList.size(); i++) {
                Element keyStageElement = keyStageElementList.get(i);
                Element stageElement = keyStageElement.getChild(DfEStageAssessment.ELEMENT_STAGE);
                String stage = stageElement.getText().trim();

                List<Element> stageAssessmentElementList =
                        keyStageElement.getChildren(DfEStageAssessment.ELEMENT_STAGE_ASSESSMENT);

                for (int j = 0; j < stageAssessmentElementList.size(); j++) {
                    Element stageAssessmentElement = stageAssessmentElementList.get(j);

                    DfEStageAssessment stageAssessment = new DfEStageAssessment(stage, stageAssessmentElement);
                    stageAssessment.setStage(stage);

                    stageAssessments.add(stageAssessment);
                }

            }
        }

        return stageAssessments;
    }

    /**
     * State Report Data class for gathering data, using StudentHistoryHelper, calculating
     * enrollment history.
     * This export should report a row for each student/school combination.
     * It should report positive attendance, and report positive codes for each day in membership.
     * Days not in membership should be reported as empty.
     *
     * @author X2 Development Corporation
     */
    class AttendanceStatistics extends StateReportData {
        /*
         * Constants: Parameters, Constants
         */
        protected static final String DEFAULT_CALENDAR_NAME = "Standard";
        protected static final String PARAM_END_DATE = "endDate";
        protected static final String PARAM_START_DATE = "startDate";

        /*
         * Instance variables.
         */
        protected PlainDate m_endDate;
        protected EnrollmentManager m_enrollmentManager;
        protected StudentHistoryHelper m_helper;
        protected Map<String, Map<String, Set<PlainDate>>> m_schoolsToCalendars =
                new HashMap<String, Map<String, Set<PlainDate>>>();
        protected PlainDate m_startDate;
        protected SisSchool m_schoolLocal;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            loadSchoolYears();

            int numOfSchoolYears = m_schoolYearRanges.size() - 1;

            PlainDate firstDateAllYears = m_schoolYearRanges.get(0).get(0);
            PlainDate lastDateAllYears = m_schoolYearRanges.get(numOfSchoolYears).get(1);

            m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, firstDateAllYears);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, lastDateAllYears);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.FALSE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_INACTIVE, Boolean.TRUE);
            m_helper.setSelectionProperty("PROPERTY_LOAD_ALL_ATTENDANCE", Boolean.TRUE);
            if (m_schoolLocal != null) {
                Criteria studentCriteria = m_helper.getStudentCriteria();
                studentCriteria.addEqualTo(Student.COL_SCHOOL_OID, m_schoolLocal.getOid());
            }
            setQuery(m_helper.getStudentQuery(false));
        }

        /**
         * Set the initial school from the report save state.
         *
         * @param school void
         */
        public void setSchool(SisSchool school) {
            m_schoolLocal = school;
        }

        /**
         * Returns a list of all in-session PlainDate for the specified school and calendarId.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Set<PlainDate>
         */
        protected Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
            if (!m_schoolsToCalendars.containsKey(school.getOid())) {
                Map<String, Set<PlainDate>> calendarData =
                        m_enrollmentManager.getCalendarLookup(school, m_startDate, m_endDate);
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            }

            Set<PlainDate> set = m_schoolsToCalendars.get(school.getOid()).get(calendar);
            if (set == null) {
                set = m_schoolsToCalendars.get(school.getOid()).get(DEFAULT_CALENDAR_NAME);
                if (set == null) {
                    Set<Entry<String, Set<PlainDate>>> entrySet = m_schoolsToCalendars.get(school.getOid()).entrySet();
                    if (entrySet.size() == 1) {
                        set = entrySet.iterator().next().getValue();
                    }
                }
            }
            return set;
        }
    }

}
