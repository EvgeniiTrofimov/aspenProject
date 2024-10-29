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
 * The Class DfELanguage.
 */
public class DfELanguage {
    public static final String ELEMENT_LANGUAGES = "Languages";
    public static final String ELEMENT_TYPE = "Type";
    public static final String ELEMENT_LANGUAGE_TYPE = "LanguageType";
    public static final String ELEMENT_LANGUAGE_CODE = "Language";

    public static final String DEFAULT_LANGUAGE_TYPE = "F"; // First

    private String languageType = null;
    private String languageCode = null;

    /**
     * Constructor for DfE (UK Department for Education) Language Object.
     *
     * @param languageElement Element
     */
    public DfELanguage(Element languageElement) {
        setLanguageType(languageElement.getChild(ELEMENT_LANGUAGE_TYPE));
        setLanguageCode(languageElement.getChild(ELEMENT_LANGUAGE_CODE));
    }

    /**
     * Constructor for DfE (UK Department for Education) Language Object.
     *
     * @param languageType String
     * @param languageCode String
     */
    public DfELanguage(String languageType, String languageCode) {
        setLanguageType(languageType);
        setLanguageCode(languageCode);
    }

    /**
     * Gets the languageType.
     *
     * @return String
     */
    public String getLanguageType() {
        return languageType;
    }

    /**
     * Sets the languageType.
     *
     * @param languageType void
     */
    public void setLanguageType(String languageType) {
        this.languageType = languageType;
    }

    /**
     * Sets the languageType from a DfE XML Element.
     *
     * @param languageTypeElement void
     */
    public void setLanguageType(Element languageTypeElement) {
        if (languageTypeElement != null) {
            this.languageType = languageTypeElement.getTextTrim();
        }
    }

    /**
     * Gets the languageCode.
     *
     * @return String
     */
    public String getLanguageCode() {
        return languageCode;
    }

    /**
     * Sets the languageCode.
     *
     * @param languageCode void
     */
    public void setLanguageCode(String languageCode) {
        this.languageCode = languageCode;
    }

    /**
     * Sets the languageCode from a DfE XML Element.
     *
     * @param languageCodeElement void
     */
    public void setLanguageCode(Element languageCodeElement) {
        if (languageCodeElement != null) {
            this.languageCode = languageCodeElement.getTextTrim();
        }
    }

}
