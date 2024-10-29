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
 * The Class DfEHeader.
 */
public class DfEHeader {
    public static final String ELEMENT_CTFILE = "CTfile";
    public static final String ELEMENT_ATFILE = "ATfile";
    public static final String ELEMENT_HEADER = "Header";
    public static final String ELEMENT_DOCUMENT_NAME = "DocumentName";
    public static final String ELEMENT_CTF_VERSION = "CTFversion";
    public static final String ELEMENT_ATF_VERSION = "ATFversion";
    public static final String ELEMENT_DATE_TIME = "DateTime";
    public static final String ELEMENT_DOCUMENT_QUALIFIER = "DocumentQualifier";
    public static final String ELEMENT_DATA_QUALIFIER = "DataQualifier";
    public static final String ELEMENT_DATA_DESCRIPTOR = "DataDescriptor";
    public static final String ELEMENT_SUPPLIER_ID = "SupplierID";
    public static final String ELEMENT_SOURCE_SCHOOL = "SourceSchool";
    public static final String ELEMENT_LEA = "LEA";
    public static final String ELEMENT_ESTABISHMENT = "Estab";
    public static final String ELEMENT_SCHOOL_NAME = "SchoolName";
    public static final String ELEMENT_ACADEMIC_YEAR = "AcademicYear";
    public static final String ELEMENT_DESTINATION_SCHOOL = "DestSchool";
    public static final String ELEMENT_SUPP_ID = "SuppID";

    public static final String HEADER_TYPE_CTF = "CTF";
    public static final String CTF_DOCUMENT_NAME = "Common Transfer File";
    public static final String CTF_VERSION = "12.0";
    public static final String HEADER_TYPE_ATF = "ATF";
    public static final String ATF_DOCUMENT_NAME = "Admissions Data Transfer File";
    public static final String ATF_VERSION = "9.1";
    public static final String DEFAULT_DOCUMENT_QUALIFIER = "full";
    public static final String DEFAULT_SUPPLIER_ID = "FSC";

    private String headerType = null;
    private String documentName = null;
    private String version = null;
    private String dateTime = null;
    private String documentQualifier = null;
    private String dataQualifier = null;
    private String dataDescriptor = null;
    private String supplierID = null;
    private String sourceSchoolLEA = null;
    private String sourceSchoolEstab = null;
    private String sourceSchoolName = null;
    private String sourceSchoolAcademicYear = null;
    private String destSchoolLEA = null;
    private String destSchoolEstab = null;
    private String suppID = null;

    /**
     * Constructor for DfE (UK Department for Education) Header Object.
     */
    public DfEHeader() {}

    /**
     * Constructor for DfE (UK Department for Education) Header Object.
     *
     * @param HeaderElement Element
     */
    public DfEHeader(Element HeaderElement) {
        setDocumentName(HeaderElement.getChild(ELEMENT_DOCUMENT_NAME));
        if (CTF_DOCUMENT_NAME.equals(documentName)) {
            setHeaderType(HEADER_TYPE_CTF);
            setVersion(HeaderElement.getChild(ELEMENT_CTF_VERSION));
        } else {
            setHeaderType(HEADER_TYPE_ATF);
            setVersion(HeaderElement.getChild(ELEMENT_ATF_VERSION));
        }
        setDateTime(HeaderElement.getChild(ELEMENT_DATE_TIME));
        setDocumentQualifier(HeaderElement.getChild(ELEMENT_DOCUMENT_QUALIFIER));
        setDataQualifier(HeaderElement.getChild(ELEMENT_DATA_QUALIFIER));
        setDataDescriptor(HeaderElement.getChild(ELEMENT_DATA_DESCRIPTOR));
        setSupplierID(HeaderElement.getChild(ELEMENT_SUPPLIER_ID));

        Element sourceSchoolElement = HeaderElement.getChild(ELEMENT_SOURCE_SCHOOL);
        if (sourceSchoolElement != null) {
            setSourceSchoolLEA(sourceSchoolElement.getChild(ELEMENT_LEA));
            setSourceSchoolEstab(sourceSchoolElement.getChild(ELEMENT_ESTABISHMENT));
            setSourceSchoolName(sourceSchoolElement.getChild(ELEMENT_SCHOOL_NAME));
            setSourceSchoolAcademicYear(sourceSchoolElement.getChild(ELEMENT_ACADEMIC_YEAR));
        }

        Element destSchoolElement = HeaderElement.getChild(ELEMENT_DESTINATION_SCHOOL);
        if (destSchoolElement != null) {
            setDestSchoolLEA(destSchoolElement.getChild(ELEMENT_LEA));
            setDestSchoolEstab(destSchoolElement.getChild(ELEMENT_ESTABISHMENT));
        }

        setSuppID(HeaderElement.getChild(ELEMENT_SUPP_ID));
    }

    /**
     * Gets the documentName.
     *
     * @return String
     */
    public String getDocumentName() {
        return documentName;
    }

    /**
     * Sets the documentName.
     *
     * @param documentName void
     */
    public void setDocumentName(String documentName) {
        this.documentName = documentName;
    }

    /**
     * Sets the documentName from a DfE XML Element.
     *
     * @param documentNameElement void
     */
    public void setDocumentName(Element documentNameElement) {
        if (documentNameElement != null) {
            this.documentName = documentNameElement.getTextTrim();
        }
    }

    /**
     * Gets the headerType.
     *
     * @return String
     */
    public String getHeaderType() {
        return headerType;
    }

    /**
     * Sets the headerType.
     *
     * @param headerType void
     */
    public void setHeaderType(String headerType) {
        this.headerType = headerType;
    }

    /**
     * Sets the headerType from a DfE XML Element.
     *
     * @param headerTypeElement void
     */
    public void setHeaderType(Element headerTypeElement) {
        if (headerTypeElement != null) {
            this.headerType = headerTypeElement.getTextTrim();
        }
    }

    /**
     * Gets the version.
     *
     * @return String
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the Version.
     *
     * @param version void
     */
    public void setVersion(String version) {
        this.version = version;
    }

    /**
     * Sets the Version from a DfE XML Element.
     *
     * @param versionElement void
     */
    public void setVersion(Element versionElement) {
        if (versionElement != null) {
            this.version = versionElement.getTextTrim();
        }
    }

    /**
     * Gets the dateTime.
     *
     * @return String
     */
    public String getDateTime() {
        return dateTime;
    }

    /**
     * Sets the dateTime.
     *
     * @param dateTime void
     */
    public void setDateTime(String dateTime) {
        this.dateTime = dateTime;
    }

    /**
     * Sets the dateTime from a DfE XML Element.
     *
     * @param dateTimeElement void
     */
    public void setDateTime(Element dateTimeElement) {
        if (dateTimeElement != null) {
            this.dateTime = dateTimeElement.getTextTrim();
        }
    }

    /**
     * Gets the documentQualifier.
     *
     * @return String
     */
    public String getDocumentQualifier() {
        return documentQualifier;
    }

    /**
     * Sets the documentQualifier.
     *
     * @param documentQualifier void
     */
    public void setDocumentQualifier(String documentQualifier) {
        this.documentQualifier = documentQualifier;
    }

    /**
     * Sets the documentQualifier from a DfE XML Element.
     *
     * @param documentQualifierElement void
     */
    public void setDocumentQualifier(Element documentQualifierElement) {
        if (documentQualifierElement != null) {
            this.documentQualifier = documentQualifierElement.getTextTrim();
        }
    }

    /**
     * Gets the dataQualifier.
     *
     * @return String
     */
    public String getDataQualifier() {
        return dataQualifier;
    }

    /**
     * Sets the dataQualifier.
     *
     * @param dataQualifier void
     */
    public void setDataQualifier(String dataQualifier) {
        this.dataQualifier = dataQualifier;
    }

    /**
     * Sets the dataQualifier from a DfE XML Element.
     *
     * @param dataQualifierElement void
     */
    public void setDataQualifier(Element dataQualifierElement) {
        if (dataQualifierElement != null) {
            this.dataQualifier = dataQualifierElement.getTextTrim();
        }
    }

    /**
     * Gets the dataDescriptor.
     *
     * @return String
     */
    public String getDataDescriptor() {
        return dataDescriptor;
    }

    /**
     * Sets the dataDescriptor.
     *
     * @param dataDescriptor void
     */
    public void setDataDescriptor(String dataDescriptor) {
        this.dataDescriptor = dataDescriptor;
    }

    /**
     * Sets the dataDescriptor from a DfE XML Element.
     *
     * @param dataDescriptorElement void
     */
    public void setDataDescriptor(Element dataDescriptorElement) {
        if (dataDescriptorElement != null) {
            this.dataDescriptor = dataDescriptorElement.getTextTrim();
        }
    }

    /**
     * Gets the supplierID.
     *
     * @return String
     */
    public String getSupplierID() {
        return supplierID;
    }

    /**
     * Sets the supplierID.
     *
     * @param supplierID void
     */
    public void setSupplierID(String supplierID) {
        this.supplierID = supplierID;
    }

    /**
     * Sets the supplierID from a DfE XML Element.
     *
     * @param supplierIDElement void
     */
    public void setSupplierID(Element supplierIDElement) {
        if (supplierIDElement != null) {
            this.supplierID = supplierIDElement.getTextTrim();
        }
    }

    /**
     * Gets the sourceSchoolLEA.
     *
     * @return String
     */
    public String getSourceSchoolLEA() {
        return sourceSchoolLEA;
    }

    /**
     * Sets the sourceSchoolLEA.
     *
     * @param sourceSchoolLEA void
     */
    public void setSourceSchoolLEA(String sourceSchoolLEA) {
        this.sourceSchoolLEA = sourceSchoolLEA;
    }

    /**
     * Sets the sourceSchoolLEA from a DfE XML Element.
     *
     * @param sourceSchoolLEAElement void
     */
    public void setSourceSchoolLEA(Element sourceSchoolLEAElement) {
        if (sourceSchoolLEAElement != null) {
            this.sourceSchoolLEA = sourceSchoolLEAElement.getTextTrim();
        }
    }

    /**
     * Gets the sourceSchoolEstab.
     *
     * @return String
     */
    public String getSourceSchoolEstab() {
        return sourceSchoolEstab;
    }

    /**
     * Sets the sourceSchoolEstab.
     *
     * @param sourceSchoolEstab void
     */
    public void setSourceSchoolEstab(String sourceSchoolEstab) {
        this.sourceSchoolEstab = sourceSchoolEstab;
    }

    /**
     * Sets the sourceSchoolEstab from a DfE XML Element.
     *
     * @param sourceSchoolEstabElement void
     */
    public void setSourceSchoolEstab(Element sourceSchoolEstabElement) {
        if (sourceSchoolEstabElement != null) {
            this.sourceSchoolEstab = sourceSchoolEstabElement.getTextTrim();
        }
    }

    /**
     * Gets the sourceSchoolName.
     *
     * @return String
     */
    public String getSourceSchoolName() {
        return sourceSchoolName;
    }

    /**
     * Sets the sourceSchoolName.
     *
     * @param sourceSchoolName void
     */
    public void setSourceSchoolName(String sourceSchoolName) {
        this.sourceSchoolName = sourceSchoolName;
    }

    /**
     * Sets the sourceSchoolName from a DfE XML Element.
     *
     * @param sourceSchoolNameElement void
     */
    public void setSourceSchoolName(Element sourceSchoolNameElement) {
        if (sourceSchoolNameElement != null) {
            this.sourceSchoolName = sourceSchoolNameElement.getTextTrim();
        }
    }

    /**
     * Gets the sourceSchoolAcademicYear.
     *
     * @return String
     */
    public String getSourceSchoolAcademicYear() {
        return sourceSchoolAcademicYear;
    }

    /**
     * Sets the sourceSchoolAcademicYear.
     *
     * @param sourceSchoolAcademicYear void
     */
    public void setSourceSchoolAcademicYear(String sourceSchoolAcademicYear) {
        this.sourceSchoolAcademicYear = sourceSchoolAcademicYear;
    }

    /**
     * Sets the sourceSchoolAcademicYear from a DfE XML Element.
     *
     * @param sourceSchoolAcademicYearElement void
     */
    public void setSourceSchoolAcademicYear(Element sourceSchoolAcademicYearElement) {
        if (sourceSchoolAcademicYearElement != null) {
            this.sourceSchoolAcademicYear = sourceSchoolAcademicYearElement.getTextTrim();
        }
    }

    /**
     * Gets the destSchoolLEA.
     *
     * @return String
     */
    public String getDestSchoolLEA() {
        return destSchoolLEA;
    }

    /**
     * Sets the destSchoolLEA.
     *
     * @param destSchoolLEA void
     */
    public void setDestSchoolLEA(String destSchoolLEA) {
        this.destSchoolLEA = destSchoolLEA;
    }

    /**
     * Sets the destSchoolLEA from a DfE XML Element.
     *
     * @param destSchoolLEAElement void
     */
    public void setDestSchoolLEA(Element destSchoolLEAElement) {
        if (destSchoolLEAElement != null) {
            this.destSchoolLEA = destSchoolLEAElement.getTextTrim();
        }
    }

    /**
     * Gets the destSchoolEstab.
     *
     * @return String
     */
    public String getDestSchoolEstab() {
        return destSchoolEstab;
    }

    /**
     * Sets the destSchoolEstab.
     *
     * @param destSchoolEstab void
     */
    public void setDestSchoolEstab(String destSchoolEstab) {
        this.destSchoolEstab = destSchoolEstab;
    }

    /**
     * Sets the destSchoolEstab from a DfE XML Element.
     *
     * @param destSchoolEstabElement void
     */
    public void setDestSchoolEstab(Element destSchoolEstabElement) {
        if (destSchoolEstabElement != null) {
            this.destSchoolEstab = destSchoolEstabElement.getTextTrim();
        }
    }

    /**
     * Gets the suppID.
     *
     * @return String
     */
    public String getSuppID() {
        return suppID;
    }

    /**
     * Sets the suppID.
     *
     * @param suppID void
     */
    public void setSuppID(String suppID) {
        this.suppID = suppID;
    }

    /**
     * Sets the suppID from a DfE XML Element.
     *
     * @param suppIDElement void
     */
    public void setSuppID(Element suppIDElement) {
        if (suppIDElement != null) {
            this.suppID = suppIDElement.getTextTrim();
        }
    }

}
