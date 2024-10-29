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
 * The Class DfEFSMInstance.
 */
public class DfEFSMInstance {
    public static final String PROGRAM_CODE_FSM = "FSM";
    public static final String ELEMENT_FSM_HISTORY = "FSMhistory";
    public static final String ELEMENT_FSM_REVIEW_DATE = "FSMreviewDate";
    public static final String ELEMENT_FSM_INSTANCE = "FSMinstance";
    public static final String ELEMENT_FSM_START_DATE = "FSMstartDate";
    public static final String ELEMENT_FSM_END_DATE = "FSMendDate";
    public static final String ELEMENT_FSM_UK_COUNTRY = "UKcountry";

    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    private PlainDate fSMStartDate = null;
    private PlainDate fSMEndDate = null;
    private String uKCountry = null;

    /**
     * Constructor for DfE (UK Department for Education) FSM Instance Object.
     *
     * @param fSMInstanceElement Element
     */
    public DfEFSMInstance(Element fSMInstanceElement) {
        setFSMStartDate(fSMInstanceElement.getChild(ELEMENT_FSM_START_DATE));
        setFSMEndDate(fSMInstanceElement.getChild(ELEMENT_FSM_END_DATE));
        setUKCountry(fSMInstanceElement.getChild(ELEMENT_FSM_UK_COUNTRY));
    }

    /**
     * Constructor for DfE (UK Department for Education) SEN Need Object.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @param uKCountry String
     */
    public DfEFSMInstance(PlainDate startDate, PlainDate endDate, String uKCountry) {
        setFSMStartDate(startDate);
        setFSMEndDate(endDate);
        setUKCountry(uKCountry);
    }

    /**
     * Gets the fSMStartDate.
     *
     * @return PlainDate
     */
    public PlainDate getFSMStartDate() {
        return fSMStartDate;
    }

    /**
     * Sets the fSMStartDate.
     *
     * @param fSMStartDate void
     */
    public void setFSMStartDate(PlainDate fSMStartDate) {
        this.fSMStartDate = fSMStartDate;
    }

    /**
     * Sets the fSMStartDate from a DfE XML Element.
     *
     * @param fSMStartDateElement void
     */
    public void setFSMStartDate(Element fSMStartDateElement) {
        if (fSMStartDateElement != null) {
            String fSMStartDateStr = fSMStartDateElement.getTextTrim();
            if (!StringUtils.isEmpty(fSMStartDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(fSMStartDateStr);
                    this.fSMStartDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.fSMStartDate = null;
                }
            }
        }
    }

    /**
     * Gets the fSMEndDate.
     *
     * @return PlainDate
     */
    public PlainDate getFSMEndDate() {
        return fSMEndDate;
    }

    /**
     * Sets the fSMEndDate.
     *
     * @param fSMEndDate void
     */
    public void setFSMEndDate(PlainDate fSMEndDate) {
        this.fSMEndDate = fSMEndDate;
    }

    /**
     * Sets the fSMEndDate from a DfE XML Element.
     *
     * @param fSMEndDateElement void
     */
    public void setFSMEndDate(Element fSMEndDateElement) {

        if (fSMEndDateElement != null) {
            String fSMEndDateStr = fSMEndDateElement.getTextTrim();
            if (!StringUtils.isEmpty(fSMEndDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(fSMEndDateStr);
                    this.fSMEndDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.fSMEndDate = null;
                }
            }
        }
    }

    /**
     * Gets the uKCountry.
     *
     * @return String
     */
    public String getUKCountry() {
        return uKCountry;
    }

    /**
     * Sets the uKCountry.
     *
     * @param uKCountry void
     */
    public void setUKCountry(String uKCountry) {
        this.uKCountry = uKCountry;
    }

    /**
     * Sets the uKCountry from a DfE XML Element.
     *
     * @param uKCountryElement void
     */
    public void setUKCountry(Element uKCountryElement) {
        if (uKCountryElement != null) {
            this.uKCountry = uKCountryElement.getTextTrim();
        }
    }

}
