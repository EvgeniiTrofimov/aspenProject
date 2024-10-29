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
 * The Class DfESENNeed.
 */
public class DfESENNeed {
    public static final String PROGRAM_CODE_SEN = "SEN";
    public static final String ELEMENT_SEN = "SEN";
    public static final String ELEMENT_SEN_START_DATE = "StartDate";
    public static final String ELEMENT_SEN_PROVISION = "SENprovision";
    public static final String ELEMENT_SEN_NEEDS = "SENneeds";
    public static final String ELEMENT_SEN_NEED = "SENneed";
    public static final String ELEMENT_SEN_TYPE = "SENtype";
    public static final String ELEMENT_SEN_TYPE_RANK = "SENtypeRank";

    private String type = null;
    private String typeRank = null;

    /**
     * Constructor for DfE (UK Department for Education) SEN Need Object.
     *
     * @param sENNeedElement Element
     */
    public DfESENNeed(Element sENNeedElement) {
        setType(sENNeedElement.getChild(ELEMENT_SEN_TYPE));
        setTypeRank(sENNeedElement.getChild(ELEMENT_SEN_TYPE_RANK));
    }

    /**
     * Constructor for DfE (UK Department for Education) SEN Need Object.
     *
     * @param type String
     * @param typeRank String
     */
    public DfESENNeed(String type, String typeRank) {
        setType(type);
        setTypeRank(typeRank);
    }

    /**
     * Gets the type.
     *
     * @return String
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the type.
     *
     * @param type void
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Sets the type from a DfE XML Element.
     *
     * @param typeElement void
     */
    public void setType(Element typeElement) {
        if (typeElement != null) {
            this.type = typeElement.getTextTrim();
        }
    }

    /**
     * Gets the typeRank.
     *
     * @return String
     */
    public String getTypeRank() {
        return typeRank;
    }

    /**
     * Sets the typeRank.
     *
     * @param typeRank void
     */
    public void setTypeRank(String typeRank) {
        this.typeRank = typeRank;
    }

    /**
     * Sets the typeRank from a DfE XML Element.
     *
     * @param typeRankElement void
     */
    public void setTypeRank(Element typeRankElement) {
        if (typeRankElement != null) {
            this.typeRank = typeRankElement.getTextTrim();
        }
    }

}
