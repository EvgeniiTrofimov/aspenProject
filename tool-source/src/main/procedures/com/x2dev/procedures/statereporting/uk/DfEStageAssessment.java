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
 * The Class DfEStageAssessment.
 */
public class DfEStageAssessment {
    public static final String ELEMENT_STAGE_ASSESSMENTS = "StageAssessments";
    public static final String ELEMENT_KEY_STAGE = "KeyStage";
    public static final String ELEMENT_STAGE = "Stage";
    public static final String ELEMENT_STAGE_ASSESSMENT = "StageAssessment";
    public static final String ELEMENT_LOCALE = "Locale";
    public static final String ELEMENT_YEAR = "Year";
    public static final String ELEMENT_SUBJECT = "Subject";
    public static final String ELEMENT_METHOD = "Method";
    public static final String ELEMENT_COMPONENT = "Component";
    public static final String ELEMENT_RESULT_STATUS = "ResultStatus";
    public static final String ELEMENT_RESULT_QUALIFIER = "ResultQualifier";
    public static final String ELEMENT_RESULT = "Result";
    public static final String ELEMENT_RESULT_DATE = "ResultDate";

    public static final String TRUE = "true";

    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    private String stage = null;
    private String locale = null;
    private String year = null;
    private String subject = null;
    private String method = null;
    private String component = null;
    private String resultStatus = null;
    private String resultQualifier = null;
    private String result = null;
    private PlainDate resultDate = null;

    /**
     * Constructor for DfE School History Object.
     *
     * @param stage String
     * @param stageAssessmentElement Element
     */
    public DfEStageAssessment(String stage, Element stageAssessmentElement) {
        setStage(stage);
        setLocale(stageAssessmentElement.getChild(ELEMENT_LOCALE));
        setYear(stageAssessmentElement.getChild(ELEMENT_YEAR));
        setResultDate(stageAssessmentElement.getChild(ELEMENT_RESULT_DATE));
        setSubject(stageAssessmentElement.getChild(ELEMENT_SUBJECT));
        setMethod(stageAssessmentElement.getChild(ELEMENT_METHOD));
        setComponent(stageAssessmentElement.getChild(ELEMENT_COMPONENT));
        setResultStatus(stageAssessmentElement.getChild(ELEMENT_RESULT_STATUS));
        setResultQualifier(stageAssessmentElement.getChild(ELEMENT_RESULT_QUALIFIER));
        setResult(stageAssessmentElement.getChild(ELEMENT_RESULT));
        setResultDate(stageAssessmentElement.getChild(ELEMENT_RESULT_DATE));
    }

    /**
     * Constructor for DfE School History Object.
     *
     * @param stage String
     * @param locale String
     * @param year String
     * @param subject String
     * @param method String
     * @param component String
     * @param resultStatus String
     * @param resultQualifier String
     * @param result String
     * @param resultDateStr String
     */
    public DfEStageAssessment(String stage, String locale, String year, String subject, String method, String component,
            String resultStatus, String resultQualifier, String result, String resultDateStr) {
        setStage(stage);
        setLocale(locale);
        setYear(year);
        setSubject(subject);
        setMethod(method);
        setComponent(component);
        setResultStatus(resultStatus);
        setResultQualifier(resultQualifier);
        setResult(result);
        setResultDate(resultDateStr);
    }


    /**
     * Gets the stage.
     *
     * @return PlainDate
     */
    public String getStage() {
        return stage;
    }

    /**
     * Sets the stage.
     *
     * @param stage void
     */
    public void setStage(String stage) {
        this.stage = stage;
    }

    /**
     * Sets the stage from a DfE XML Element.
     *
     * @param stageElement void
     */
    public void setStage(Element stageElement) {
        if (stageElement != null) {
            this.stage = stageElement.getTextTrim();
        }
    }

    /**
     * Gets the locale.
     *
     * @return PlainDate
     */
    public String getLocale() {
        return locale;
    }

    /**
     * Sets the locale.
     *
     * @param locale void
     */
    public void setLocale(String locale) {
        this.locale = locale;
    }

    /**
     * Sets the locale from a DfE XML Element.
     *
     * @param localeElement void
     */
    public void setLocale(Element localeElement) {
        if (localeElement != null) {
            this.locale = localeElement.getTextTrim();
        }
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
     * Gets the subject.
     *
     * @return PlainDate
     */
    public String getSubject() {
        return subject;
    }

    /**
     * Sets the subject.
     *
     * @param subject void
     */
    public void setSubject(String subject) {
        this.subject = subject;
    }

    /**
     * Sets the subject from a DfE XML Element.
     *
     * @param subjectElement void
     */
    public void setSubject(Element subjectElement) {
        if (subjectElement != null) {
            this.subject = subjectElement.getTextTrim();
        }
    }



    /**
     * Gets the method.
     *
     * @return String
     */
    public String getMethod() {
        return method;
    }

    /**
     * Sets the method.
     *
     * @param method void
     */
    public void setMethod(String method) {
        this.method = method;
    }

    /**
     * Sets the component from a DfE XML Element.
     *
     * @param methodElement void
     */
    public void setMethod(Element methodElement) {
        if (methodElement != null) {
            this.method = methodElement.getTextTrim();
        }
    }

    /**
     * Gets the component.
     *
     * @return String
     */
    public String getComponent() {
        return component;
    }

    /**
     * Sets the component.
     *
     * @param component void
     */
    public void setComponent(String component) {
        this.component = component;
    }

    /**
     * Sets the component from a DfE XML Element.
     *
     * @param componentElement void
     */
    public void setComponent(Element componentElement) {
        if (componentElement != null) {
            this.component = componentElement.getTextTrim();
        }
    }

    /**
     * Gets the resultStatus.
     *
     * @return String
     */
    public String getResultStatus() {
        return resultStatus;
    }

    /**
     * Sets the resultStatus.
     *
     * @param resultStatus void
     */
    public void setResultStatus(String resultStatus) {
        this.resultStatus = resultStatus;
    }

    /**
     * Sets the resultStatus from a DfE XML Element.
     *
     * @param resultStatusElement void
     */
    public void setResultStatus(Element resultStatusElement) {
        if (resultStatusElement != null) {
            this.resultStatus = resultStatusElement.getTextTrim();
        }
    }

    /**
     * Gets the uniqueCandidateNumber.
     *
     * @return String
     */
    public String getResultQualifier() {
        return resultQualifier;
    }

    /**
     * Sets the uniqueCandidateNumber.
     *
     * @param resultQualifier void
     */
    public void setResultQualifier(String resultQualifier) {
        this.resultQualifier = resultQualifier;
    }

    /**
     * Sets the resultQualifier from a DfE XML Element.
     *
     * @param resultQualifierElement void
     */
    public void setResultQualifier(Element resultQualifierElement) {
        if (resultQualifierElement != null) {
            this.resultQualifier = resultQualifierElement.getTextTrim();
        }
    }

    /**
     * Gets the result.
     *
     * @return String
     */
    public String getResult() {
        return result;
    }

    /**
     * Sets the result.
     *
     * @param result void
     */
    public void setResult(String result) {
        this.result = result;
    }

    /**
     * Sets the result from a DfE XML Element.
     *
     * @param resultElement void
     */
    public void setResult(Element resultElement) {
        if (resultElement != null) {
            this.result = resultElement.getTextTrim();
        }
    }

    /**
     * Gets the resultDate.
     *
     * @return PlainDate
     */
    public PlainDate getResultDate() {
        return resultDate;
    }

    /**
     * Sets the resultDate.
     *
     * @param resultDate void
     */
    public void setResultDate(PlainDate resultDate) {
        this.resultDate = resultDate;
    }

    /**
     * Sets the resultDate from a String Date.
     *
     * @param resultDate void
     */
    public void setResultDate(String resultDate) {
        if (resultDate != null) {
            String reviewDateStr = resultDate.trim();
            if (!StringUtils.isEmpty(reviewDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(reviewDateStr);
                    this.resultDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.resultDate = null;
                }
            }
        }
    }

    /**
     * Sets the resultDate from a DfE XML Element.
     *
     * @param resultDateElement void
     */
    public void setResultDate(Element resultDateElement) {
        if (resultDateElement != null) {
            String resultDateStr = resultDateElement.getTextTrim();
            Date date = null;
            try {
                date = m_dateFormat.parse(resultDateStr);
                this.resultDate = new PlainDate(date);
            } catch (ParseException e) {
                this.resultDate = null;
            }
        }
    }

}
