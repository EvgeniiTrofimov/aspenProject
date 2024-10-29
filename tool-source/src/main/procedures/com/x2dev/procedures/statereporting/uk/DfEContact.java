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
import java.util.ArrayList;
import java.util.List;
import org.jdom.Element;

/**
 * The Class DfEContact.
 */
public class DfEContact {
    public static final String ELEMENT_ORDER = "Order";
    public static final String ELEMENT_TITLE = "Title";
    public static final String ELEMENT_FORENAME = "Forename";
    public static final String ELEMENT_SURNAME = "Surname";
    public static final String ELEMENT_GENDER = "Gender";
    public static final String ELEMENT_MIDDLE_NAMES = "MiddleNames";
    public static final String ELEMENT_RELATIONSHIP = "Relationship";
    public static final String ELEMENT_RESPONSIBILITY = "Responsibility";
    public static final String ELEMENT_ADDRESS = "Address";
    public static final String ELEMENT_ADDRESS_AS_PUPIL = "AddressAsPupil";
    public static final String ELEMENT_PHONES = "Phones";
    public static final String ELEMENT_PHONE = "Phone";
    public static final String ELEMENT_TELEPHONE_TYPE = "TelephoneType";
    public static final String ELEMENT_PHONE_NUMBER = "PhoneNo";
    public static final String ELEMENT_EMAIL = "Email";

    private int order = 0;
    private String title = null;
    private String surname = null;
    private String forename = null;
    private String middleNames = null;
    private String gender = null;
    private String relationshipCode = null;
    private Boolean isResponsible = null;
    private Boolean hasAddressAsPupil = null;
    private DfEAddress dfEAddress = null;
    private ArrayList<DfETelephone> telephones = new ArrayList();
    private String email = null;

    /**
     * Constructor for DfE (UK Department for Education) Contact Object.
     *
     * @param contactElement Element
     */
    public DfEContact(Element contactElement) {
        setOrder(contactElement.getChild(ELEMENT_ORDER));
        setTitle(contactElement.getChild(ELEMENT_TITLE));
        setSurname(contactElement.getChild(ELEMENT_SURNAME));
        setForename(contactElement.getChild(ELEMENT_FORENAME));
        setGender(contactElement.getChild(ELEMENT_GENDER));
        setMiddleNames(contactElement.getChild(ELEMENT_MIDDLE_NAMES));
        setRelationship(contactElement.getChild(ELEMENT_RELATIONSHIP));
        setResponsible(contactElement.getChild(ELEMENT_RESPONSIBILITY));

        Element addressElement = contactElement.getChild(ELEMENT_ADDRESS);
        if (addressElement != null) {
            Element addressAsPupilElement = addressElement.getChild(ELEMENT_ADDRESS_AS_PUPIL);
            if (addressAsPupilElement != null) {
                setAddressAsPupil(addressElement.getChild(ELEMENT_ADDRESS_AS_PUPIL));
            } else {
                this.dfEAddress = new DfEAddress(addressElement);
            }
        }

        Element phonesElement = contactElement.getChild(ELEMENT_PHONES);
        if (phonesElement != null) {
            List<Element> phonesList = phonesElement.getChildren(ELEMENT_PHONE);
            for (int k = 0; k < phonesList.size(); k++) {
                Element telephoneElement = phonesList.get(k);
                DfETelephone dfETelephone = new DfETelephone(telephoneElement);
                telephones.add(dfETelephone);
            }
        }

        setEmail(contactElement.getChild(ELEMENT_EMAIL));
    }

    /**
     * Constructor for DfE (UK Department for Education) Contact Object.
     */
    public DfEContact() {}

    /**
     * Gets the order.
     *
     * @return int
     */
    public int getOrder() {
        return order;
    }

    /**
     * Sets the order.
     *
     * @param order void
     */
    public void setOrder(int order) {
        this.order = order;
    }

    /**
     * Sets the order from a DfE XML Element.
     *
     * @param orderElement void
     */
    public void setOrder(Element orderElement) {
        if (orderElement != null) {
            int orderInt = 0;
            String orderStr = orderElement.getTextTrim();
            if (StringUtils.isInteger(orderStr)) {
                orderInt = Integer.parseInt(orderStr);
                this.order = orderInt;
            }
        }
    }

    /**
     * Gets the title.
     *
     * @return String
     */
    public String getTitle() {
        return title;
    }

    /**
     * Sets the title.
     *
     * @param title void
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * Sets the title from a DfE XML Element.
     *
     * @param titleElement void
     */
    public void setTitle(Element titleElement) {
        if (titleElement != null) {
            this.title = titleElement.getTextTrim();
        }
    }

    /**
     * Gets the surname.
     *
     * @return String
     */
    public String getSurname() {
        return surname;
    }

    /**
     * Sets the surname.
     *
     * @param surname void
     */
    public void setSurname(String surname) {
        this.surname = surname;
    }

    /**
     * Sets the surname from a DfE XML Element.
     *
     * @param surnameElement void
     */
    public void setSurname(Element surnameElement) {
        if (surnameElement != null) {
            this.surname = surnameElement.getTextTrim();
        }
    }

    /**
     * Gets the forename.
     *
     * @return String
     */
    public String getForename() {
        return forename;
    }

    /**
     * Sets the forename.
     *
     * @param forename void
     */
    public void setForename(String forename) {
        this.forename = forename;
    }

    /**
     * Sets the forename from a DfE XML Element.
     *
     * @param forenameElement void
     */
    public void setForename(Element forenameElement) {
        if (forenameElement != null) {
            this.forename = forenameElement.getTextTrim();
        }
    }

    /**
     * Gets the gender.
     *
     * @return String
     */
    public String getGender() {
        return gender;
    }

    /**
     * Sets the gender.
     *
     * @param gender void
     */
    public void setGender(String gender) {
        this.gender = gender;
    }

    /**
     * Sets the gender from a DfE XML Element.
     *
     * @param genderElement void
     */
    public void setGender(Element genderElement) {
        if (genderElement != null) {
            this.gender = genderElement.getTextTrim();
        }
    }

    /**
     * Gets the middleNames.
     *
     * @return String
     */
    public String getMiddleNames() {
        return middleNames;
    }

    /**
     * Sets the middleNames.
     *
     * @param middleNames void
     */
    public void setMiddleNames(String middleNames) {
        this.middleNames = middleNames;
    }

    /**
     * Sets the middleNames from a DfE XML Element.
     *
     * @param middleNamesElement void
     */
    public void setMiddleNames(Element middleNamesElement) {
        if (middleNamesElement != null) {
            this.middleNames = middleNamesElement.getTextTrim();
        }
    }

    /**
     * Gets the relationshipCode.
     *
     * @return String
     */
    public String getRelationshipCode() {
        return relationshipCode;
    }

    /**
     * Sets the relationshipCode.
     *
     * @param relationshipCode void
     */
    public void setRelationshipCode(String relationshipCode) {
        this.relationshipCode = relationshipCode;
    }

    /**
     * Sets the relationshipCode from a DfE XML Element.
     *
     * @param relationshipCodeElement void
     */
    public void setRelationship(Element relationshipCodeElement) {
        if (relationshipCodeElement != null) {
            this.relationshipCode = relationshipCodeElement.getTextTrim();
        }
    }

    /**
     * Gets the Responsible flag.
     *
     * @return Boolean
     */
    public Boolean getResponsible() {
        return isResponsible;
    }

    /**
     * Sets the responsible flag.
     *
     * @param responsible void
     */
    public void setResponsible(Boolean responsible) {
        this.isResponsible = responsible;
    }

    /**
     * Sets the responsible from a DfE XML Element.
     *
     * @param responsibleElement void
     */
    public void setResponsible(Element responsibleElement) {
        if (responsibleElement != null) {
            String resp = responsibleElement.getTextTrim().toLowerCase();
            this.isResponsible = Boolean.valueOf(DfEManager.STRING_TRUE.equals(resp));
        }
    }

    /**
     * Gets the isAddressAsPupil flag.
     *
     * @return Boolean
     */
    public Boolean getAddressAsPupil() {
        return hasAddressAsPupil;
    }

    /**
     * Sets the addressAsPupil flag.
     *
     * @param addressAsPupil void
     */
    public void setAddressAsPupil(Boolean addressAsPupil) {
        this.hasAddressAsPupil = addressAsPupil;
    }

    /**
     * Sets the addressAsPupil from a DfE XML Element.
     *
     * @param addressAsPupilElement void
     */
    public void setAddressAsPupil(Element addressAsPupilElement) {
        if (addressAsPupilElement != null) {
            String sameAddress = addressAsPupilElement.getTextTrim().toLowerCase();
            this.hasAddressAsPupil = Boolean.valueOf(DfEManager.STRING_TRUE.equals(sameAddress));
        }
    }

    /**
     * Gets the DfEAddress for Contact.
     *
     * @return DfEAddress
     */
    public DfEAddress getDfEAddress() {
        return dfEAddress;
    }

    /**
     * Sets the dfEAddress for Contact.
     *
     * @param dfEAddress void
     */
    public void setDfEAddress(DfEAddress dfEAddress) {
        this.dfEAddress = dfEAddress;
    }

    /**
     * Gets the ArrayList of DfETelephone for Contact.
     *
     * @return ArrayList<DfETelephone>
     */
    public ArrayList<DfETelephone> getTelephones() {
        return telephones;
    }

    /**
     * Adds a DfETelephone to the Pupil's DfETelephone Collection.
     *
     * @param dfETelephone DfETelephone
     */
    public void addTelephone(DfETelephone dfETelephone) {
        telephones.add(dfETelephone);
    }

    /**
     * Gets the email.
     *
     * @return String
     */
    public String getEmail() {
        return email;
    }

    /**
     * Sets the email.
     *
     * @param email void
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Sets the email from a DfE XML Element.
     *
     * @param emailElement void
     */
    public void setEmail(Element emailElement) {
        if (emailElement != null) {
            this.email = emailElement.getTextTrim();
        }
    }

}
