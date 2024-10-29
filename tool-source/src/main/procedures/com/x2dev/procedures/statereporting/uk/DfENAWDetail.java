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
 * The Class DfENAWDetail.
 */
public class DfENAWDetail {
    public static final String ELEMENT_NAW_DETAILS = "NAWdetails";
    public static final String ELEMENT_SPEAK_WELSH = "SpeakWelsh";
    public static final String ELEMENT_HOME_WELSH = "HomeWelsh";
    public static final String ELEMENT_NATIONAL_IDENTITY = "NationalIdentity";
    public static final String ELEMENT_WELSH_SOURCE = "WelshSource";
    public static final String ELEMENT_LANGAUGE_SOURCE = "LanguageSource";
    public static final String ELEMENT_EAL_ACQUISITION = "EALAcquisition";
    public static final String ELEMENT_SEN_CURR_TEACHING_METHODS = "SENCurriculumandTeachingMethods";
    public static final String ELEMENT_SEN_GROUPINGS_SUPPORT = "SENGroupingandSupport";
    public static final String ELEMENT_SEN_SPECIALISED_RESOURCES = "SENSpecialisedResources";
    public static final String ELEMENT_SEN_ADVICE_ASSESSMENT = "SENAdviceandAssessment";

    private String speakWelsh = null;
    private String homeWelsh = null;
    private String nationalIdentity = null;
    private String welshSource = null;
    private String languageSource = null;
    private String eALAcquisition = null;
    private String sENCurrTeachingMethods = null;
    private String sENGroupingAndSupport = null;
    private String sENSpecialisedResources = null;
    private String sENAdviceAndAssessment = null;

    /**
     * Constructor for DfE NAWDetail Object.
     *
     * @param nAWDetailElement Element
     */
    public DfENAWDetail(Element nAWDetailElement) {
        setSpeakWelsh(nAWDetailElement.getChild(ELEMENT_SPEAK_WELSH));
        setHomeWelsh(nAWDetailElement.getChild(ELEMENT_HOME_WELSH));
        setNationalIdentity(nAWDetailElement.getChild(ELEMENT_NATIONAL_IDENTITY));
        setWelshSource(nAWDetailElement.getChild(ELEMENT_WELSH_SOURCE));
        setLanguageSource(nAWDetailElement.getChild(ELEMENT_LANGAUGE_SOURCE));
        setEALAcquisition(nAWDetailElement.getChild(ELEMENT_EAL_ACQUISITION));
        setSENCurrTeachingMethods(nAWDetailElement.getChild(ELEMENT_SEN_CURR_TEACHING_METHODS));
        setSENGroupingAndSupport(nAWDetailElement.getChild(ELEMENT_SEN_GROUPINGS_SUPPORT));
        setSENSpecialisedResources(nAWDetailElement.getChild(ELEMENT_SEN_SPECIALISED_RESOURCES));
        setSENAdviceAndAssessment(nAWDetailElement.getChild(ELEMENT_SEN_ADVICE_ASSESSMENT));
    }

    /**
     * Constructor for DfE NAWDetail Object.
     *
     * @param speakWelsh String
     * @param homeWelsh String
     * @param nationalIdentity String
     * @param welshSource String
     * @param languageSource String
     * @param eALAcquisition String
     * @param sENCurrTeachingMethods String
     * @param sENGroupingandSupport String
     * @param sENSpecialisedResources String
     * @param sENAdviceandAssessment String
     */
    public DfENAWDetail(String speakWelsh, String homeWelsh, String nationalIdentity, String welshSource,
            String languageSource, String eALAcquisition, String sENCurrTeachingMethods, String sENGroupingandSupport,
            String sENSpecialisedResources, String sENAdviceandAssessment) {
        setSpeakWelsh(speakWelsh);
        setHomeWelsh(homeWelsh);
        setNationalIdentity(nationalIdentity);
        setWelshSource(welshSource);
        setLanguageSource(languageSource);
        setEALAcquisition(eALAcquisition);
        setSENCurrTeachingMethods(sENCurrTeachingMethods);
        setSENGroupingAndSupport(sENGroupingandSupport);
        setSENSpecialisedResources(sENSpecialisedResources);
        setSENAdviceAndAssessment(sENAdviceandAssessment);
    }


    /**
     * Gets the speakWelsh.
     *
     * @return String
     */
    public String getSpeakWelsh() {
        return speakWelsh;
    }

    /**
     * Sets the speakWelsh.
     *
     * @param speakWelsh void
     */
    public void setSpeakWelsh(String speakWelsh) {
        this.speakWelsh = speakWelsh;
    }

    /**
     * Sets the speakWelsh from a DfE XML Element.
     *
     * @param speakWelshElement void
     */
    public void setSpeakWelsh(Element speakWelshElement) {
        if (speakWelshElement != null) {
            this.speakWelsh = speakWelshElement.getTextTrim();
        }
    }

    /**
     * Gets the homeWelsh.
     *
     * @return String
     */
    public String getHomeWelsh() {
        return homeWelsh;
    }

    /**
     * Sets the homeWelsh.
     *
     * @param homeWelsh void
     */
    public void setHomeWelsh(String homeWelsh) {
        this.homeWelsh = homeWelsh;
    }

    /**
     * Sets the homeWelsh from a DfE XML Element.
     *
     * @param homeWelshElement void
     */
    public void setHomeWelsh(Element homeWelshElement) {
        if (homeWelshElement != null) {
            this.homeWelsh = homeWelshElement.getTextTrim();
        }
    }

    /**
     * Gets the nationalIdentity.
     *
     * @return String
     */
    public String getNationalIdentity() {
        return nationalIdentity;
    }

    /**
     * Sets the nationalIdentity.
     *
     * @param nationalIdentity void
     */
    public void setNationalIdentity(String nationalIdentity) {
        this.nationalIdentity = nationalIdentity;
    }

    /**
     * Sets the nationalIdentity from a DfE XML Element.
     *
     * @param nationalIdentityElement void
     */
    public void setNationalIdentity(Element nationalIdentityElement) {
        if (nationalIdentityElement != null) {
            this.nationalIdentity = nationalIdentityElement.getTextTrim();
        }
    }

    /**
     * Gets the welshSource.
     *
     * @return String
     */
    public String getWelshSource() {
        return welshSource;
    }

    /**
     * Sets the welshSource.
     *
     * @param welshSource void
     */
    public void setWelshSource(String welshSource) {
        this.welshSource = welshSource;
    }

    /**
     * Sets the welshSource from a DfE XML Element.
     *
     * @param welshSourceElement void
     */
    public void setWelshSource(Element welshSourceElement) {
        if (welshSourceElement != null) {
            this.welshSource = welshSourceElement.getTextTrim();
        }
    }

    /**
     * Gets the languageSource.
     *
     * @return String
     */
    public String getLanguageSource() {
        return languageSource;
    }

    /**
     * Sets the languageSource.
     *
     * @param languageSource void
     */
    public void setLanguageSource(String languageSource) {
        this.languageSource = languageSource;
    }

    /**
     * Sets the languageSource from a DfE XML Element.
     *
     * @param languageSourceElement void
     */
    public void setLanguageSource(Element languageSourceElement) {
        if (languageSourceElement != null) {
            this.languageSource = languageSourceElement.getTextTrim();
        }
    }

    /**
     * Gets the eALAcquisition.
     *
     * @return String
     */
    public String getEALAcquisition() {
        return eALAcquisition;
    }

    /**
     * Sets the eALAcquisition.
     *
     * @param eALAcquisition void
     */
    public void setEALAcquisition(String eALAcquisition) {
        this.eALAcquisition = eALAcquisition;
    }

    /**
     * Sets the eALAcquisition from a DfE XML Element.
     *
     * @param eALAcquisitionElement void
     */
    public void setEALAcquisition(Element eALAcquisitionElement) {
        if (eALAcquisitionElement != null) {
            this.eALAcquisition = eALAcquisitionElement.getTextTrim();
        }
    }

    /**
     * Gets the sENCurrTeachingMethods.
     *
     * @return String
     */
    public String getSENCurrTeachingMethods() {
        return sENCurrTeachingMethods;
    }

    /**
     * Sets the sENCurrTeachingMethods.
     *
     * @param sENCurrTeachingMethods void
     */
    public void setSENCurrTeachingMethods(String sENCurrTeachingMethods) {
        this.sENCurrTeachingMethods = sENCurrTeachingMethods;
    }

    /**
     * Sets the sENCurrTeachingMethods from a DfE XML Element.
     *
     * @param sENCurrTeachingMethodsElement void
     */
    public void setSENCurrTeachingMethods(Element sENCurrTeachingMethodsElement) {
        if (sENCurrTeachingMethodsElement != null) {
            this.sENCurrTeachingMethods = sENCurrTeachingMethodsElement.getTextTrim();
        }
    }

    /**
     * Gets the sENGroupingAndSupport.
     *
     * @return String
     */
    public String getSENGroupingAndSupport() {
        return sENGroupingAndSupport;
    }

    /**
     * Sets the sENGroupingAndSupport.
     *
     * @param sENGroupingAndSupport void
     */
    public void setSENGroupingAndSupport(String sENGroupingAndSupport) {
        this.sENGroupingAndSupport = sENGroupingAndSupport;
    }

    /**
     * Sets the sENGroupingAndSupport from a DfE XML Element.
     *
     * @param sENGroupingAndSupportElement void
     */
    public void setSENGroupingAndSupport(Element sENGroupingAndSupportElement) {
        if (sENGroupingAndSupportElement != null) {
            this.sENGroupingAndSupport = sENGroupingAndSupportElement.getTextTrim();
        }
    }

    /**
     * Gets the sENSpecialisedResources.
     *
     * @return String
     */
    public String getSENSpecialisedResources() {
        return sENSpecialisedResources;
    }

    /**
     * Sets the sENSpecialisedResources.
     *
     * @param sENSpecialisedResources void
     */
    public void setSENSpecialisedResources(String sENSpecialisedResources) {
        this.sENSpecialisedResources = sENSpecialisedResources;
    }

    /**
     * Sets the sENSpecialisedResources from a DfE XML Element.
     *
     * @param sENSpecialisedResourcesElement void
     */
    public void setSENSpecialisedResources(Element sENSpecialisedResourcesElement) {
        if (sENSpecialisedResourcesElement != null) {
            this.sENSpecialisedResources = sENSpecialisedResourcesElement.getTextTrim();
        }
    }

    /**
     * Gets the sENAdviceAndAssessment.
     *
     * @return String
     */
    public String getSENAdviceAndAssessment() {
        return sENAdviceAndAssessment;
    }

    /**
     * Sets the sENAdviceAndAssessment.
     *
     * @param sENAdviceAndAssessment void
     */
    public void setSENAdviceAndAssessment(String sENAdviceAndAssessment) {
        this.sENAdviceAndAssessment = sENAdviceAndAssessment;
    }

    /**
     * Sets the sENAdviceAndAssessment from a DfE XML Element.
     *
     * @param sENAdviceAndAssessmentElement void
     */
    public void setSENAdviceAndAssessment(Element sENAdviceAndAssessmentElement) {
        if (sENAdviceAndAssessmentElement != null) {
            this.sENAdviceAndAssessment = sENAdviceAndAssessmentElement.getTextTrim();
        }
    }

}
