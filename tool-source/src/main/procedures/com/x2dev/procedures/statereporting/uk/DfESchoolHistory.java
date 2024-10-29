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
 * The Class DfESchoolHistory.
 */
public class DfESchoolHistory {
    public static final String ELEMENT_SCHOOL_HISTORY = "SchoolHistory";
    public static final String ELEMENT_SCHOOL = "School";
    public static final String ELEMENT_LEA = "LEA";
    public static final String ELEMENT_ESTAB = "Estab";
    public static final String ELEMENT_SCHOOL_NAME = "SchoolName";
    public static final String ELEMENT_ENTRY_DATE = "EntryDate";
    public static final String ELEMENT_LEAVING_DATE = "LeavingDate";
    public static final String ELEMENT_LEAVING_REASON = "LeavingReason";
    public static final String ELEMENT_LAST_SCHOOL = "LastSchool";

    public static final String TRUE = "true";

    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    private String lEA = null;
    private String estab = null;
    private String schoolName = null;
    private PlainDate entryDate = null;
    private PlainDate leavingDate = null;
    private String leavingReason = null;
    private Boolean lastSchool = null;


    /**
     * Constructor for DfE School History Object.
     *
     * @param schoolHistoryElement Element
     */
    public DfESchoolHistory(Element schoolHistoryElement) {
        setLEA(schoolHistoryElement.getChild(ELEMENT_LEA));
        setEstab(schoolHistoryElement.getChild(ELEMENT_ESTAB));
        setSchoolName(schoolHistoryElement.getChild(ELEMENT_SCHOOL_NAME));
        setEntryDate(schoolHistoryElement.getChild(ELEMENT_ENTRY_DATE));
        setLeavingDate(schoolHistoryElement.getChild(ELEMENT_LEAVING_DATE));
        setLeavingReason(schoolHistoryElement.getChild(ELEMENT_LEAVING_REASON));
        setLastSchool(schoolHistoryElement.getChild(ELEMENT_LAST_SCHOOL));
    }

    /**
     * Constructor for DfE School History Object.
     *
     * @param lEA String
     * @param estab String
     * @param schoolName String
     * @param entryDate PlainDate
     * @param leavingDate PlainDate
     * @param leavingReason String
     * @param lastSchool Boolean
     */
    public DfESchoolHistory(String lEA, String estab, String schoolName, PlainDate entryDate, PlainDate leavingDate,
            String leavingReason, Boolean lastSchool) {
        setLEA(lEA);
        setEstab(estab);
        setSchoolName(schoolName);
        setEntryDate(entryDate);
        setLeavingDate(leavingDate);
        setLeavingReason(leavingReason);
        setLastSchool(lastSchool);
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
     * @param lEAElement void
     */
    public void setLEA(Element lEAElement) {
        if (lEAElement != null) {
            this.lEA = lEAElement.getTextTrim();
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
     * @return PlainDate
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
     * Gets the entryDate.
     *
     * @return PlainDate
     */
    public PlainDate getEntryDate() {
        return entryDate;
    }

    /**
     * Sets the entryDate.
     *
     * @param entryDate void
     */
    public void setEntryDate(PlainDate entryDate) {
        this.entryDate = entryDate;
    }

    /**
     * Sets the entryDate from a DfE XML Element.
     *
     * @param entryDateElement void
     */
    public void setEntryDate(Element entryDateElement) {
        if (entryDateElement != null) {
            String entryDateStr = entryDateElement.getTextTrim();
            if (!StringUtils.isEmpty(entryDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(entryDateStr);
                    this.entryDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.entryDate = null;
                }
            }
        }
    }

    /**
     * Gets the leavingDate.
     *
     * @return PlainDate
     */
    public PlainDate getLeavingDate() {
        return leavingDate;
    }

    /**
     * Sets the leavingDate.
     *
     * @param leavingDate void
     */
    public void setLeavingDate(PlainDate leavingDate) {
        this.leavingDate = leavingDate;
    }

    /**
     * Sets the entryDate from a DfE XML Element.
     *
     * @param leavingDateElement void
     */
    public void setLeavingDate(Element leavingDateElement) {
        if (leavingDateElement != null) {
            String leavingDateStr = leavingDateElement.getTextTrim();
            if (!StringUtils.isEmpty(leavingDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(leavingDateStr);
                    this.leavingDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.leavingDate = null;
                }
            }
        }
    }

    /**
     * Gets the leavingReason.
     *
     * @return PlainDate
     */
    public String getLeavingReason() {
        return leavingReason;
    }

    /**
     * Sets the leavingReason.
     *
     * @param leavingReason void
     */
    public void setLeavingReason(String leavingReason) {
        this.leavingReason = leavingReason;
    }

    /**
     * Sets the leavingReason from a DfE XML Element.
     *
     * @param leavingReasonElement void
     */
    public void setLeavingReason(Element leavingReasonElement) {
        if (leavingReasonElement != null) {
            this.leavingReason = leavingReasonElement.getTextTrim();
        }
    }

    /**
     * Gets the lastSchool.
     *
     * @return String
     */
    public Boolean getLastSchool() {
        return lastSchool;
    }

    /**
     * Sets the lastSchool.
     *
     * @param lastSchool void
     */
    public void setLastSchool(Boolean lastSchool) {
        this.lastSchool = lastSchool;
    }

    /**
     * Sets the lastSchool from a DfE XML Element.
     *
     * @param lastSchoolElement void
     */
    public void setLastSchool(Element lastSchoolElement) {
        if (lastSchoolElement != null) {
            String resp = lastSchoolElement.getTextTrim().toLowerCase();
            this.lastSchool = Boolean.valueOf(TRUE.equals(resp));
        }
    }

}
