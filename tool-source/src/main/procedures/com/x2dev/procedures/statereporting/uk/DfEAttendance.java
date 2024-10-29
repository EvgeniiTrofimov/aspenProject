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

import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.jdom.Element;

/**
 * The Class DfEAttendance.
 */
public class DfEAttendance {
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
