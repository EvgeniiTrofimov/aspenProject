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
 * The Class DfEAddress.
 */
public class DfEAddress {
    public static final String ELEMENT_ADDRESS = "Address";
    public static final String ELEMENT_BS7666_ADDRESS = "BS7666Address";
    public static final String ELEMENT_PAON = "PAON";
    public static final String ELEMENT_SAON = "SAON";
    public static final String ELEMENT_STREET = "Street";
    public static final String ELEMENT_LOCALITY = "Locality";
    public static final String ELEMENT_TOWN = "Town";
    public static final String ELEMENT_ADMINISTRATIVE_AREA = "AdministrativeArea";
    public static final String ELEMENT_POST_TOWN = "PostTown";
    public static final String ELEMENT_UNIQUE_PROPERTY_REFERENCE_NUMBER = "UniquePropertyReferenceNumber";
    public static final String ELEMENT_ADDRESS_LINES = "AddressLines";
    public static final String ELEMENT_ADDRESS_LINE_1 = "AddressLine1";
    public static final String ELEMENT_ADDRESS_LINE_2 = "AddressLine2";
    public static final String ELEMENT_ADDRESS_LINE_3 = "AddressLine3";
    public static final String ELEMENT_ADDRESS_LINE_4 = "AddressLine4";
    public static final String ELEMENT_ADDRESS_LINE_5 = "AddressLine5";
    public static final String ELEMENT_COUNTY = "County";
    public static final String ELEMENT_POST_CODE = "PostCode";
    public static final String ELEMENT_ZIP = "Zip";
    public static final String ELEMENT_COUNTRY = "Country";
    public static final String ELEMENT_EASTING = "Easting";
    public static final String ELEMENT_NORTHING = "Northing";

    private boolean bS7666Address = false;
    private String pAON = null;
    private String sAON = null;
    private String street = null;
    private String locality = null;
    private String town = null;
    private String administrativeArea = null;
    private String postTown = null;
    private String uniquePropertyReferenceNumber = null;
    private String addressLine1 = null;
    private String addressLine2 = null;
    private String addressLine3 = null;
    private String addressLine4 = null;
    private String addressLine5 = null;
    private String county = null;
    private String postCode = null;
    private String zip = null;
    private String country = null;
    private String easting = null;
    private String northing = null;

    /**
     * Constructor for DfE (UK Department for Education) Address Object
     *
     * Convert DfE Address XML Element to DfEAddress Object
     * Used in CTF/ATF Import.
     *
     * @param addressElement Element
     */
    public DfEAddress(Element addressElement) {
        Element bS7666AddressElement = addressElement.getChild(ELEMENT_BS7666_ADDRESS);
        if (bS7666AddressElement != null) {
            bS7666Address = true;
            setSAON(bS7666AddressElement.getChild(ELEMENT_SAON));
            setPAON(bS7666AddressElement.getChild(ELEMENT_PAON));
            setStreet(bS7666AddressElement.getChild(ELEMENT_STREET));
            setLocality(bS7666AddressElement.getChild(ELEMENT_LOCALITY));
            setTown(bS7666AddressElement.getChild(ELEMENT_TOWN));
            setAdministrativeArea(bS7666AddressElement.getChild(ELEMENT_ADMINISTRATIVE_AREA));
            setPostTown(bS7666AddressElement.getChild(ELEMENT_POST_TOWN));
            setUniquePropertyReferenceNumber(bS7666AddressElement.getChild(ELEMENT_UNIQUE_PROPERTY_REFERENCE_NUMBER));
        } else {
            bS7666Address = false;
            Element addressLinesElement = addressElement.getChild(ELEMENT_ADDRESS_LINES);
            if (addressLinesElement != null) {
                setAddressLine1(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_1));
                setAddressLine2(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_2));
                setAddressLine3(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_3));
                setAddressLine4(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_4));
                setAddressLine5(addressLinesElement.getChild(ELEMENT_ADDRESS_LINE_5));
            }
        }

        setCounty(addressElement.getChild(ELEMENT_COUNTY));
        setPostCode(addressElement.getChild(ELEMENT_POST_CODE));
        setCountry(addressElement.getChild(ELEMENT_COUNTRY));
        setZip(addressElement.getChild(ELEMENT_ZIP));
        setEasting(addressElement.getChild(ELEMENT_EASTING));
        setNorthing(addressElement.getChild(ELEMENT_NORTHING));
    }

    /**
     * Constructor for DfE (UK Department for Education) Address Object.
     */
    public DfEAddress() {}

    /**
     * Gets the hasBS7666Address flag.
     *
     * @return boolean
     */
    public boolean hasBS7666Address() {
        return bS7666Address;
    }

    /**
     * Sets the hasBS7666Address flag.
     *
     * @param hasBS7666Address void
     */
    public void setBS7666Address(boolean hasBS7666Address) {
        this.bS7666Address = hasBS7666Address;
    }

    /**
     * Gets the pAON.
     *
     * @return String
     */
    public String getPAON() {
        return pAON;
    }

    /**
     * Sets the pAON.
     *
     * @param pAON void
     */
    public void setPAON(String pAON) {
        this.pAON = pAON;
    }

    /**
     * Sets the pAON from a DfE XML Element.
     *
     * @param pAONElement void
     */
    public void setPAON(Element pAONElement) {
        if (pAONElement != null) {
            this.pAON = pAONElement.getTextTrim();
        }
    }

    /**
     * Gets the sAON.
     *
     * @return String
     */
    public String getSAON() {
        return sAON;
    }

    /**
     * Sets the sAON.
     *
     * @param sAON void
     */
    public void setSAON(String sAON) {
        this.sAON = sAON;
    }

    /**
     * Sets the sAON from a DfE XML Element.
     *
     * @param sAONElement void
     */
    public void setSAON(Element sAONElement) {
        if (sAONElement != null) {
            this.sAON = sAONElement.getTextTrim();
        }
    }

    /**
     * Gets the street.
     *
     * @return String
     */
    public String getStreet() {
        return street;
    }

    /**
     * Sets the street.
     *
     * @param street void
     */
    public void setStreet(String street) {
        this.street = street;
    }

    /**
     * Sets the street from a DfE XML Element.
     *
     * @param streetElement void
     */
    public void setStreet(Element streetElement) {
        if (streetElement != null) {
            this.street = streetElement.getTextTrim();
        }
    }

    /**
     * Gets the locality.
     *
     * @return String
     */
    public String getLocality() {
        return locality;
    }

    /**
     * Sets the locality.
     *
     * @param locality void
     */
    public void setLocality(String locality) {
        this.locality = locality;
    }

    /**
     * Sets the locality from a DfE XML Element.
     *
     * @param localityElement void
     */
    public void setLocality(Element localityElement) {
        if (localityElement != null) {
            this.locality = localityElement.getTextTrim();
        }
    }

    /**
     * Gets the town.
     *
     * @return String
     */
    public String getTown() {
        return town;
    }

    /**
     * Sets the town.
     *
     * @param town void
     */
    public void setTown(String town) {
        this.town = town;
    }

    /**
     * Sets the town from a DfE XML Element.
     *
     * @param townElement void
     */
    public void setTown(Element townElement) {
        if (townElement != null) {
            this.town = townElement.getTextTrim();
        }
    }

    /**
     * Gets the addressLine1.
     *
     * @return String
     */
    public String getAddressLine1() {
        return addressLine1;
    }

    /**
     * Sets the addressLine1.
     *
     * @param addressLine1 void
     */
    public void setAddressLine1(String addressLine1) {
        this.addressLine1 = addressLine1;
    }

    /**
     * Sets the addressLine1 from a DfE XML Element.
     *
     * @param addressLine1Element void
     */
    public void setAddressLine1(Element addressLine1Element) {
        if (addressLine1Element != null) {
            this.addressLine1 = addressLine1Element.getTextTrim();
        }
    }

    /**
     * Gets the addressLine2.
     *
     * @return String
     */
    public String getAddressLine2() {
        return addressLine2;
    }

    /**
     * Sets the addressLine2.
     *
     * @param addressLine2 void
     */
    public void setAddressLine2(String addressLine2) {
        this.addressLine2 = addressLine2;
    }

    /**
     * Sets the addressLine2 from a DfE XML Element.
     *
     * @param addressLine2Element void
     */
    public void setAddressLine2(Element addressLine2Element) {
        if (addressLine2Element != null) {
            this.addressLine2 = addressLine2Element.getTextTrim();
        }
    }

    /**
     * Gets the addressLine3.
     *
     * @return String
     */
    public String getAddressLine3() {
        return addressLine3;
    }

    /**
     * Sets the addressLine3.
     *
     * @param addressLine3 void
     */
    public void setAddressLine3(String addressLine3) {
        this.addressLine3 = addressLine3;
    }

    /**
     * Sets the addressLine3 from a DfE XML Element.
     *
     * @param addressLine3Element void
     */
    public void setAddressLine3(Element addressLine3Element) {
        if (addressLine3Element != null) {
            this.addressLine3 = addressLine3Element.getTextTrim();
        }
    }

    /**
     * Gets the addressLine4.
     *
     * @return String
     */
    public String getAddressLine4() {
        return addressLine4;
    }

    /**
     * Sets the addressLine4.
     *
     * @param addressLine4 void
     */
    public void setAddressLine4(String addressLine4) {
        this.addressLine4 = addressLine4;
    }

    /**
     * Sets the addressLine4 from a DfE XML Element.
     *
     * @param addressLine4Element void
     */
    public void setAddressLine4(Element addressLine4Element) {
        if (addressLine4Element != null) {
            this.addressLine4 = addressLine4Element.getTextTrim();
        }
    }

    /**
     * Gets the addressLine5.
     *
     * @return String
     */
    public String getAddressLine5() {
        return addressLine5;
    }

    /**
     * Sets the addressLine5.
     *
     * @param addressLine5 void
     */
    public void setAddressLine5(String addressLine5) {
        this.addressLine5 = addressLine5;
    }

    /**
     * Sets the addressLine5 from a DfE XML Element.
     *
     * @param addressLine5Element void
     */
    public void setAddressLine5(Element addressLine5Element) {
        if (addressLine5Element != null) {
            this.addressLine5 = addressLine5Element.getTextTrim();
        }
    }

    /**
     * Gets the county.
     *
     * @return String
     */
    public String getCounty() {
        return county;
    }

    /**
     * Sets the county.
     *
     * @param county void
     */
    public void setCounty(String county) {
        this.county = county;
    }

    /**
     * Sets the county from a DfE XML Element.
     *
     * @param countyElement void
     */
    public void setCounty(Element countyElement) {
        if (countyElement != null) {
            this.county = countyElement.getTextTrim();
        }
    }

    /**
     * Gets the postCode.
     *
     * @return String
     */
    public String getPostCode() {
        return postCode;
    }

    /**
     * Sets the postCode.
     *
     * @param postCode void
     */
    public void setPostCode(String postCode) {
        this.postCode = postCode;
    }

    /**
     * Sets the postCode from a DfE XML Element.
     *
     * @param postCodeElement void
     */
    public void setPostCode(Element postCodeElement) {
        if (postCodeElement != null) {
            this.postCode = postCodeElement.getTextTrim();
        }
    }

    /**
     * Gets the country.
     *
     * @return String
     */
    public String getCountry() {
        return country;
    }

    /**
     * Sets the country.
     *
     * @param country void
     */
    public void setCountry(String country) {
        this.country = country;
    }

    /**
     * Sets the country from a DfE XML Element.
     *
     * @param countryElement void
     */
    public void setCountry(Element countryElement) {
        if (countryElement != null) {
            this.country = countryElement.getTextTrim();
        }
    }

    /**
     * Gets the administrativeArea.
     *
     * @return String
     */
    public String getAdministrativeArea() {
        return administrativeArea;
    }

    /**
     * Sets the administrativeArea.
     *
     * @param administrativeArea void
     */
    public void setAdministrativeArea(String administrativeArea) {
        this.administrativeArea = administrativeArea;
    }

    /**
     * Sets the administrativeArea from a DfE XML Element.
     *
     * @param administrativeAreaElement void
     */
    public void setAdministrativeArea(Element administrativeAreaElement) {
        if (administrativeAreaElement != null) {
            this.administrativeArea = administrativeAreaElement.getTextTrim();
        }
    }

    /**
     * Gets the postTown.
     *
     * @return String
     */
    public String getPostTown() {
        return postTown;
    }

    /**
     * Sets the postTown.
     *
     * @param postTown void
     */
    public void setPostTown(String postTown) {
        this.postTown = postTown;
    }

    /**
     * Sets the postTown from a DfE XML Element.
     *
     * @param postTownElement void
     */
    public void setPostTown(Element postTownElement) {
        if (postTownElement != null) {
            this.postTown = postTownElement.getTextTrim();
        }
    }

    /**
     * Gets the uniquePropertyReferenceNumber.
     *
     * @return String
     */
    public String getUniquePropertyReferenceNumber() {
        return uniquePropertyReferenceNumber;
    }

    /**
     * Sets the uniquePropertyReferenceNumber.
     *
     * @param uniquePropertyReferenceNumber void
     */
    public void setUniquePropertyReferenceNumber(
                                                 String uniquePropertyReferenceNumber) {
        this.uniquePropertyReferenceNumber = uniquePropertyReferenceNumber;
    }

    /**
     * Sets the uniquePropertyReferenceNumber from a DfE XML Element.
     *
     * @param uniquePropertyReferenceNumberElement void
     */
    public void setUniquePropertyReferenceNumber(Element uniquePropertyReferenceNumberElement) {
        if (uniquePropertyReferenceNumberElement != null) {
            this.uniquePropertyReferenceNumber = uniquePropertyReferenceNumberElement.getTextTrim();
        }
    }

    /**
     * Gets the zip.
     *
     * @return String
     */
    public String getZip() {
        return zip;
    }

    /**
     * Sets the zip.
     *
     * @param zip void
     */
    public void setZip(String zip) {
        this.zip = zip;
    }

    /**
     * Sets the zip from a DfE XML Element.
     *
     * @param zipElement void
     */
    public void setZip(Element zipElement) {
        if (zipElement != null) {
            this.zip = zipElement.getTextTrim();
        }
    }

    /**
     * Gets the easting.
     *
     * @return String
     */
    public String getEasting() {
        return easting;
    }

    /**
     * Sets the easting.
     *
     * @param easting void
     */
    public void setEasting(String easting) {
        this.easting = easting;
    }

    /**
     * Sets the easting from a DfE XML Element.
     *
     * @param eastingElement void
     */
    public void setEasting(Element eastingElement) {
        if (eastingElement != null) {
            this.easting = eastingElement.getTextTrim();
        }
    }

    /**
     * Gets the northing.
     *
     * @return String
     */
    public String getNorthing() {
        return northing;
    }

    /**
     * Sets the northing.
     *
     * @param northing void
     */
    public void setNorthing(String northing) {
        this.northing = northing;
    }

    /**
     * Sets the northing from a DfE XML Element.
     *
     * @param northingElement void
     */
    public void setNorthing(Element northingElement) {
        if (northingElement != null) {
            this.northing = northingElement.getTextTrim();
        }
    }
}
