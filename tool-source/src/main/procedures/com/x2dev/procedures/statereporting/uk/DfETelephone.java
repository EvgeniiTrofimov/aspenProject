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

import org.jdom.Element;

/**
 * The Class DfETelephone.
 */
public class DfETelephone {
    public static final String ELEMENT_PHONES = "Phones";
    public static final String ELEMENT_PHONE = "Phone";
    public static final String ELEMENT_TELEPHONE_TYPE = "TelephoneType";
    public static final String ELEMENT_PHONE_NUMBER = "PhoneNo";

    public static final String DEFAULT_TELEPHONE_TYPE = "H";

    private String telephoneType = null;
    private String telephoneNumber = null;

    /**
     * Constructor for DfE (UK Department for Education) telephone Object.
     *
     * @param telephoneElement Element
     */
    public DfETelephone(Element telephoneElement) {
        setTelephoneType(telephoneElement.getChild(ELEMENT_TELEPHONE_TYPE));
        setTelephoneNumber(telephoneElement.getChild(ELEMENT_PHONE_NUMBER));
    }

    /**
     * Constructor for DfE (UK Department for Education) telephone Object.
     *
     * @param telephoneType String
     * @param telephoneNumber String
     */
    public DfETelephone(String telephoneType, String telephoneNumber) {
        setTelephoneType(telephoneType);
        setTelephoneNumber(telephoneNumber);
    }

    /**
     * Gets the telephoneType.
     *
     * @return String
     */
    public String getTelephoneType() {
        return telephoneType;
    }

    /**
     * Sets the telephoneType.
     *
     * @param telephoneType void
     */
    public void setTelephoneType(String telephoneType) {
        this.telephoneType = telephoneType;
    }

    /**
     * Sets the telephoneType from a DfE XML Element.
     *
     * @param telephoneTypeElement void
     */
    public void setTelephoneType(Element telephoneTypeElement) {
        if (telephoneTypeElement != null) {
            this.telephoneType = telephoneTypeElement.getTextTrim();
        }
    }

    /**
     * Gets the telephoneNumber.
     *
     * @return String
     */
    public String getTelephoneNumber() {
        return telephoneNumber;
    }

    /**
     * Sets the telephoneNumber.
     *
     * @param telephoneNumber void
     */
    public void setTelephoneNumber(String telephoneNumber) {
        this.telephoneNumber = telephoneNumber;
    }

    /**
     * Sets the telephoneNumber from a DfE XML Element.
     *
     * @param telephoneNumberElement void
     */
    public void setTelephoneNumber(Element telephoneNumberElement) {
        if (telephoneNumberElement != null) {
            this.telephoneNumber = telephoneNumberElement.getTextTrim();
        }
    }

}
