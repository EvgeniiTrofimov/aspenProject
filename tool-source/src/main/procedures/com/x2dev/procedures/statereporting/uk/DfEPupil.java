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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.jdom.Element;
import org.jdom.output.XMLOutputter;

/**
 * The Class DfEPupil.
 */
public class DfEPupil {
    public static final String ELEMENT_CTF_PUPIL_DATA = "CTFpupilData";
    public static final String ELEMENT_ATF_PUPIL_DATA = "ATFpupilData";
    public static final String ELEMENT_PUPIL = "Pupil";
    public static final String ELEMENT_APPLICATION_REFERENCE = "ApplicationReference";
    public static final String ELEMENT_UNIQUE_PUPIL_NUMBER = "UPN";
    public static final String ELEMENT_UNIQUE_LEARNER_NUMBER = "UniqueLearnerNumber";
    public static final String ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER = "UCI";
    public static final String ELEMENT_SURNAME = "Surname";
    public static final String ELEMENT_FORENAME = "Forename";
    public static final String ELEMENT_DATE_OF_BIRTH = "DOB";
    public static final String ELEMENT_GENDER = "Gender";
    public static final String ELEMENT_BASIC_DETAILS = "BasicDetails";
    public static final String ELEMENT_FORMER_UNIQUE_PUPIL_NUMBER = "FormerUPN";
    public static final String ELEMENT_PREFERRED_SURNAME = "PreferredSurname";
    public static final String ELEMENT_PREFERRED_FORENAME = "PreferredForename";
    public static final String ELEMENT_FORMER_SURNAME = "FormerSurname";
    public static final String ELEMENT_FORMER_FORENAME = "FormerForename";
    public static final String ELEMENT_MIDDLE_NAMES = "MiddleNames";
    public static final String ELEMENT_NC_YEAR_ACTUAL = "NCyearActual";
    public static final String ELEMENT_ETHINICITY = "Ethnicity";
    public static final String ELEMENT_ETHINICITY_SOURCE = "EthnicitySource";
    public static final String ELEMENT_ENROLL_STATUS = "EnrolStatus";
    public static final String ELEMENT_NAW_DETAILS = "NAWdetails";
    public static final String ELEMENT_LOOK_AFTER = "LookedAfter";
    public static final String ELEMENT_IN_CARE = "InCare";
    public static final String ELEMENT_CARE_AUTHORITY = "CareAuthority";
    public static final String ELEMENT_LANGUAGES = "Languages";
    public static final String ELEMENT_TYPE = "Type";
    public static final String ELEMENT_MEDICAL_FLAG = "MedicalFlag";
    public static final String ELEMENT_DISABILITIES = "Disabilities";
    public static final String ELEMENT_DISABILITY = "Disability";
    public static final String ELEMENT_FSM_HISTORY = "FSMhistory";
    public static final String ELEMENT_FSM_REVIEW_DATE = "FSMreviewDate";
    public static final String ELEMENT_FSM_INSTANCE = "FSMinstance";
    public static final String ELEMENT_FSM_START_DATE = "FSMstartDate";
    public static final String ELEMENT_FSM_END_DATE = "FSMendDate";
    public static final String ELEMENT_FSM_UK_COUNTRY = "UKcountry";
    public static final String ELEMENT_SEN_HISTORY = "SENhistory";
    public static final String ELEMENT_SEN = "SEN";
    public static final String ELEMENT_SEN_START_DATE = "StartDate";
    public static final String ELEMENT_SEN_PROVISION = "SENprovision";
    public static final String ELEMENT_SEN_NEEDS = "SENneeds";
    public static final String ELEMENT_SEN_NEED = "SENneed";
    public static final String ELEMENT_ADMISSIONS = "Admissions";
    public static final String ELEMENT_ACCEPT = "Accept";
    public static final String ELEMENT_ADDRESS = "Address";
    public static final String ELEMENT_PHONES = "Phones";
    public static final String ELEMENT_PHONE = "Phone";
    public static final String ELEMENT_EMAIL = "Email";
    public static final String ELEMENT_CONTACTS = "Contacts";
    public static final String ELEMENT_CONTACT = "Contact";
    public static final String ELEMENT_ATTENDANCE = "Attendance";
    public static final String ELEMENT_YEAR_DATA = "YearData";
    public static final String ELEMENT_STAGE_ASSESSMENTS = "StageAssessments";
    public static final String ELEMENT_KEY_STAGE = "KeyStage";
    public static final String ELEMENT_STAGE = "Stage";
    public static final String ELEMENT_STAGE_ASSESSMENT = "StageAssessment";
    public static final String ELEMENT_SCHOOL_HISTORY = "SchoolHistory";
    public static final String ELEMENT_SCHOOL = "School";

    public static final String DEFAULT_ETHNICITY_SOURCE = "C";
    public static final String DEFAULT_ENROLL_STATUS = "C";
    public final Boolean DEFAULT_MEDICAL_FLAG = Boolean.valueOf(false);

    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    // Pupil variables
    private String applicationReference = null;
    private String uniquePupilNumber = null;
    private String uniqueLearnerNumber = null;
    private String uniqueCandidateIdentifier = null;
    private String surname = null;
    private String forename = null;
    private PlainDate birthDate = null;
    private String gender = null;
    private String formerUniquePupilNumber = null;
    private String preferredSurname = null;
    private String preferredForename = null;
    private String formerSurname = null;
    private String formerForename = null;
    private String middleNames = null;
    private String nCYearActual = null;
    private String ethnicity = null;
    private String ethnicitySource = null;
    private ArrayList<DfELanguage> languages = new ArrayList();
    private Boolean medicalFlag = null;
    private ArrayList<String> disabilities = new ArrayList();
    private String enrollStatus = null;
    private PlainDate fSMReviewDate = null;
    private Boolean inCare = null;
    private String careAuthority = null;
    private PlainDate sENStartDate = null;
    private String sENProvision = null;
    private ArrayList<DfEFSMInstance> fSMInstances = new ArrayList();
    private ArrayList<DfENAWDetail> nAWDetails = new ArrayList();
    private ArrayList<DfESENNeed> sENNeeds = new ArrayList();
    private ArrayList<DfEAttendance> studentAttendances = new ArrayList();
    private ArrayList<DfESchoolHistory> schoolHistories = new ArrayList();
    private ArrayList<DfEStageAssessment> stageAssessments = new ArrayList();
    private DfEAddress dfEAddress = null;
    private ArrayList<DfEContact> contacts = new ArrayList();
    private ArrayList<DfETelephone> telephones = new ArrayList();
    private String email = null;
    private String attendanceXML = null;
    private String assessmentsXML = null;
    private String schoolHistoryXML = null;

    /**
     * Constructor for DfE (UK Department for Education) Pupil Object
     *
     * Convert DfE Pupil XML Element to DfEPupil Object
     * Used in CTF/ATF Import.
     *
     * @param pupilElement Element
     */
    public DfEPupil(Element pupilElement) {
        setApplicationReference(pupilElement.getChild(ELEMENT_APPLICATION_REFERENCE));
        setUniquePupilNumber(pupilElement.getChild(ELEMENT_UNIQUE_PUPIL_NUMBER));
        setUniqueLearnerNumber(pupilElement.getChild(ELEMENT_UNIQUE_LEARNER_NUMBER));
        setUniqueCandidateIdentifier(pupilElement.getChild(ELEMENT_UNIQUE_CANDIDATE_IDENTIFIER));
        setForename(pupilElement.getChild(ELEMENT_FORENAME));
        setSurname(pupilElement.getChild(ELEMENT_SURNAME));
        setBirthDate(pupilElement.getChild(ELEMENT_DATE_OF_BIRTH));
        setGender(pupilElement.getChild(ELEMENT_GENDER));

        Element basicDetailsElement = pupilElement.getChild(ELEMENT_BASIC_DETAILS);
        if (basicDetailsElement != null) {
            setFormerUniquePupilNumber(basicDetailsElement.getChild(ELEMENT_FORMER_UNIQUE_PUPIL_NUMBER));
            setPreferredSurname(basicDetailsElement.getChild(ELEMENT_PREFERRED_SURNAME));
            setPreferredForename(basicDetailsElement.getChild(ELEMENT_PREFERRED_FORENAME));
            setFormerSurname(basicDetailsElement.getChild(ELEMENT_FORMER_SURNAME));
            setFormerForename(basicDetailsElement.getChild(ELEMENT_FORMER_FORENAME));
            setMiddleNames(basicDetailsElement.getChild(ELEMENT_MIDDLE_NAMES));
            setNCyearActual(basicDetailsElement.getChild(ELEMENT_NC_YEAR_ACTUAL));
            setEthnicity(basicDetailsElement.getChild(ELEMENT_ETHINICITY));
            setEthnicitySource(basicDetailsElement.getChild(ELEMENT_ETHINICITY_SOURCE));

            Element languagesElement = basicDetailsElement.getChild(ELEMENT_LANGUAGES);
            if (languagesElement != null) {
                List<Element> languageElementList = languagesElement.getChildren(ELEMENT_TYPE);
                for (int j = 0; j < languageElementList.size(); j++) {
                    Element languageElement = languageElementList.get(j);
                    DfELanguage dfELanguage = new DfELanguage(languageElement);
                    languages.add(dfELanguage);
                }
            }

            setEnrollStatus(basicDetailsElement.getChild(ELEMENT_ENROLL_STATUS));
            setMedicalFlag(basicDetailsElement.getChild(ELEMENT_MEDICAL_FLAG));

            Element disabiliesElement = basicDetailsElement.getChild(ELEMENT_DISABILITIES);
            if (disabiliesElement != null) {
                List<Element> disabilityElementList = disabiliesElement.getChildren(ELEMENT_DISABILITY);
                for (int l = 0; l < disabilityElementList.size(); l++) {
                    Element disabilityElement = disabilityElementList.get(l);
                    if (disabilityElement != null) {
                        String disability = disabilityElement.getTextTrim();
                        disabilities.add(disability);
                    }
                }
            }
        }

        // Look After
        Element lookAfterElement = pupilElement.getChild(ELEMENT_LOOK_AFTER);
        if (lookAfterElement != null) {
            setInCare(lookAfterElement.getChild(ELEMENT_IN_CARE));
            setCareAuthority(lookAfterElement.getChild(ELEMENT_CARE_AUTHORITY));
        }

        // Phone
        Element phonesElement = pupilElement.getChild(ELEMENT_PHONES);
        if (phonesElement != null) {
            List<Element> phonesList = phonesElement.getChildren(ELEMENT_PHONE);
            for (int k = 0; k < phonesList.size(); k++) {
                Element telephoneElement = phonesList.get(k);
                DfETelephone dfETelephone = new DfETelephone(telephoneElement);
                telephones.add(dfETelephone);
            }
        }
        setEmail(pupilElement.getChild(ELEMENT_EMAIL));

        // FSM
        Element fSMHistoryElement = pupilElement.getChild(ELEMENT_FSM_HISTORY);
        if (fSMHistoryElement != null) {
            Element fsmReviewDateElement = fSMHistoryElement.getChild(ELEMENT_FSM_REVIEW_DATE);
            setFSMReviewDate(fsmReviewDateElement);

            List<Element> fsmInstancesElementList = fSMHistoryElement.getChildren(ELEMENT_FSM_INSTANCE);
            if (fsmInstancesElementList.size() > 0) {
                for (int m = 0; m < fsmInstancesElementList.size(); m++) {
                    Element fsmInstanceElement = fsmInstancesElementList.get(m);
                    DfEFSMInstance dfEFSMInstance = new DfEFSMInstance(fsmInstanceElement);
                    fSMInstances.add(dfEFSMInstance);
                }
            }
        }

        // NAWDetail
        List<Element> nAWDetailsElementList = pupilElement.getChildren(ELEMENT_NAW_DETAILS);
        if (nAWDetailsElementList.size() > 0) {
            for (int n = 0; n < nAWDetailsElementList.size(); n++) {
                Element nAWDetailsElement = nAWDetailsElementList.get(n);
                DfENAWDetail dfENAWDetail = new DfENAWDetail(nAWDetailsElement);
                nAWDetails.add(dfENAWDetail);
            }
        }

        // SEN
        Element sENHistoryElement = pupilElement.getChild(ELEMENT_SEN_HISTORY);
        if (sENHistoryElement != null) {
            Element sENElement = sENHistoryElement.getChild(ELEMENT_SEN);
            Element sENStartDateElement = sENElement.getChild(ELEMENT_SEN_START_DATE);
            setSENStartDate(sENStartDateElement);
            Element sENProvisionElement = sENElement.getChild(ELEMENT_SEN_PROVISION);
            setSENProvision(sENProvisionElement);

            Element sENNeedsElement = sENHistoryElement.getChild(ELEMENT_SEN_NEEDS);
            if (sENNeedsElement != null) {
                List<Element> sENNeedsElementList = sENNeedsElement.getChildren(ELEMENT_SEN_NEED);
                for (int k = 0; k < sENNeedsElementList.size(); k++) {
                    Element sENNeedElement = sENNeedsElementList.get(k);
                    DfESENNeed dfESENNeed = new DfESENNeed(sENNeedElement);
                    sENNeeds.add(dfESENNeed);
                }
            }
        }

        // Address
        Element addressElement = pupilElement.getChild(ELEMENT_ADDRESS);
        if (addressElement != null) {
            setDfEAddress(new DfEAddress(addressElement));
        }

        // Contact
        Element contactsElement = pupilElement.getChild(ELEMENT_CONTACTS);
        if (contactsElement != null) {
            List<Element> contactsElementList = contactsElement.getChildren(ELEMENT_CONTACT);
            for (int l = 0; l < contactsElementList.size(); l++) {
                Element contactElement = contactsElementList.get(l);
                DfEContact dfEContact = new DfEContact(contactElement);
                contacts.add(dfEContact);
            }
        }

        // Save all XML as text in the database.
        XMLOutputter xMLOutputter = new XMLOutputter();

        Element attendanceElement = pupilElement.getChild(ELEMENT_ATTENDANCE);
        if (attendanceElement != null) {
            // TODO Remove later
            String attendanceStr = xMLOutputter.outputString(attendanceElement);
            setAttendanceXML(attendanceStr);

            List<Element> yearDataList = attendanceElement.getChildren(ELEMENT_YEAR_DATA);
            for (int r = 0; r < yearDataList.size(); r++) {
                Element yearDataElement = yearDataList.get(r);
                DfEAttendance dfEAttendance = new DfEAttendance(yearDataElement);
                studentAttendances.add(dfEAttendance);
            }
        }

        // Student Assessment
        Element assessmentsElement = pupilElement.getChild(ELEMENT_STAGE_ASSESSMENTS);
        if (assessmentsElement != null) {
            // TODO Remove later
            String assessmentsStr = xMLOutputter.outputString(assessmentsElement);
            setAssessmentsXML(assessmentsStr);

            List<Element> keyStageList = assessmentsElement.getChildren(ELEMENT_KEY_STAGE);
            for (int p = 0; p < keyStageList.size(); p++) {
                Element keyStageElement = keyStageList.get(p);

                Element stageElement = keyStageElement.getChild(ELEMENT_STAGE);
                String stage = stageElement.getTextTrim();

                List<Element> stageAssessmentList = keyStageElement.getChildren(ELEMENT_STAGE_ASSESSMENT);
                for (int q = 0; q < stageAssessmentList.size(); q++) {
                    Element stageAssessmentElement = stageAssessmentList.get(q);

                    DfEStageAssessment dfEStageAssessment = new DfEStageAssessment(stage, stageAssessmentElement);
                    stageAssessments.add(dfEStageAssessment);
                }
            }
        }

        // School History
        Element schoolHistoryElement = pupilElement.getChild(ELEMENT_SCHOOL_HISTORY);
        if (schoolHistoryElement != null) {
            // TODO Remove later
            String schoolHistoryStr = xMLOutputter.outputString(schoolHistoryElement);
            setSchoolHistoryXML(schoolHistoryStr);

            List<Element> schoolList = schoolHistoryElement.getChildren(ELEMENT_SCHOOL);
            for (int m = 0; m < schoolList.size(); m++) {
                Element schoolElement = schoolList.get(m);
                DfESchoolHistory dfESchoolHistory = new DfESchoolHistory(schoolElement);
                schoolHistories.add(dfESchoolHistory);
            }
        }

    }


    /**
     * Constructor for DfE (UK Department for Education) Pupil Object.
     */
    public DfEPupil() {}


    /**
     * Gets the uniquePupilNumber.
     *
     * @return String
     */
    public String getUniquePupilNumber() {
        return uniquePupilNumber;
    }

    /**
     * Sets the uniquePupilNumber.
     *
     * @param uniquePupilNumber void
     */
    public void setUniquePupilNumber(String uniquePupilNumber) {
        this.uniquePupilNumber = uniquePupilNumber;
    }

    /**
     * Sets the uniquePupilNumber from a DfE XML Element.
     *
     * @param uniquePupilNumberElement void
     */
    public void setUniquePupilNumber(Element uniquePupilNumberElement) {
        if (uniquePupilNumberElement != null) {
            this.uniquePupilNumber = uniquePupilNumberElement.getTextTrim();
        }
    }

    /**
     * Gets the applicationReference.
     *
     * @return String
     */
    public String getApplicationReference() {
        return applicationReference;
    }

    /**
     * Sets the applicationReference.
     *
     * @param applicationReference void
     */
    public void setApplicationReference(String applicationReference) {
        this.applicationReference = applicationReference;
    }

    /**
     * Sets the uniquePupilNumber from a DfE XML Element.
     *
     * @param applicationReferenceElement void
     */
    public void setApplicationReference(Element applicationReferenceElement) {
        if (applicationReferenceElement != null) {
            this.applicationReference = applicationReferenceElement.getTextTrim();
        }
    }

    /**
     * Gets the uniqueLearnerNumber.
     *
     * @return String
     */
    public String getUniqueLearnerNumber() {
        return uniqueLearnerNumber;
    }

    /**
     * Sets the uniqueLearnerNumber.
     *
     * @param uniqueLearnerNumber void
     */
    public void setUniqueLearnerNumber(String uniqueLearnerNumber) {
        this.uniqueLearnerNumber = uniqueLearnerNumber;
    }

    /**
     * Sets the uniqueLearnerNumber from a DfE XML Element.
     *
     * @param uniqueLearnerNumberElement void
     */
    public void setUniqueLearnerNumber(Element uniqueLearnerNumberElement) {
        if (uniqueLearnerNumberElement != null) {
            this.uniqueLearnerNumber = uniqueLearnerNumberElement.getTextTrim();
        }
    }

    /**
     * Gets the uniqueCandidateNumber.
     *
     * @return String
     */
    public String getUniqueCandidateIdentifier() {
        return uniqueCandidateIdentifier;
    }

    /**
     * Sets the uniqueCandidateNumber.
     *
     * @param uniqueCandidateIdentifier void
     */
    public void setUniqueCandidateIdentifier(String uniqueCandidateIdentifier) {
        this.uniqueCandidateIdentifier = uniqueCandidateIdentifier;
    }

    /**
     * Sets the uniqueCandidateIdentifier from a DfE XML Element.
     *
     * @param uniqueCandidateIdentifierElement void
     */
    public void setUniqueCandidateIdentifier(Element uniqueCandidateIdentifierElement) {
        if (uniqueCandidateIdentifierElement != null) {
            this.uniqueCandidateIdentifier = uniqueCandidateIdentifierElement.getTextTrim();
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
     * Gets the birthDate.
     *
     * @return PlainDate
     */
    public PlainDate getBirthDate() {
        return birthDate;
    }

    /**
     * Sets the birthDate.
     *
     * @param birthDate void
     */
    public void setBirthDate(PlainDate birthDate) {
        this.birthDate = birthDate;
    }

    /**
     * Sets the birthDate from a DfE XML Element.
     *
     * @param birthDateElement void
     */
    public void setBirthDate(Element birthDateElement) {
        if (birthDateElement != null) {
            String birthDateStr = birthDateElement.getTextTrim();
            if (!StringUtils.isEmpty(birthDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(birthDateStr);
                    this.birthDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.birthDate = null;
                }
            }
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
     * Gets the formerUniquePupilNumber.
     *
     * @return String
     */
    public String getFormerUniquePupilNumber() {
        return formerUniquePupilNumber;
    }

    /**
     * Sets the formerUniquePupilNumber.
     *
     * @param formerUniquePupilNumber void
     */
    public void setFormerUniquePupilNumber(String formerUniquePupilNumber) {
        this.formerUniquePupilNumber = formerUniquePupilNumber;
    }

    /**
     * Sets the formerUniquePupilNumber from a DfE XML Element.
     *
     * @param formerUniquePupilNumberElement void
     */
    public void setFormerUniquePupilNumber(Element formerUniquePupilNumberElement) {
        if (formerUniquePupilNumberElement != null) {
            this.formerUniquePupilNumber = formerUniquePupilNumberElement.getTextTrim();
        }
    }

    /**
     * Gets the preferredSurname.
     *
     * @return String
     */
    public String getPreferredSurname() {
        return preferredSurname;
    }

    /**
     * Sets the preferredSurname.
     *
     * @param preferredSurname void
     */
    public void setPreferredSurname(String preferredSurname) {
        this.preferredSurname = preferredSurname;
    }

    /**
     * Sets the preferredSurname from a DfE XML Element.
     *
     * @param preferredSurnameElement void
     */
    public void setPreferredSurname(Element preferredSurnameElement) {
        if (preferredSurnameElement != null) {
            this.preferredSurname = preferredSurnameElement.getTextTrim();
        }
    }

    /**
     * Gets the formerSurname.
     *
     * @return String
     */
    public String getFormerSurname() {
        return formerSurname;
    }

    /**
     * Sets the formerSurname.
     *
     * @param formerSurname void
     */
    public void setFormerSurname(String formerSurname) {
        this.formerSurname = formerSurname;
    }

    /**
     * Sets the formerSurname from a DfE XML Element.
     *
     * @param formerSurnameElement void
     */
    public void setFormerSurname(Element formerSurnameElement) {
        if (formerSurnameElement != null) {
            this.formerSurname = formerSurnameElement.getTextTrim();
        }
    }

    /**
     * Gets the formerForename.
     *
     * @return String
     */
    public String getFormerForename() {
        return formerForename;
    }

    /**
     * Sets the formerForename.
     *
     * @param formerForename void
     */
    public void setFormerForename(String formerForename) {
        this.formerForename = formerForename;
    }

    /**
     * Sets the formerForename from a DfE XML Element.
     *
     * @param formerForenameElement void
     */
    public void setFormerForename(Element formerForenameElement) {
        if (formerForenameElement != null) {
            this.formerForename = formerForenameElement.getTextTrim();
        }
    }

    /**
     * Gets the preferredForename.
     *
     * @return String
     */
    public String getPreferredForename() {
        return preferredForename;
    }

    /**
     * Sets the preferredForename.
     *
     * @param preferredForename void
     */
    public void setPreferredForename(String preferredForename) {
        this.preferredForename = preferredForename;
    }

    /**
     * Sets the preferredForename from a DfE XML Element.
     *
     * @param preferredForenameElement void
     */
    public void setPreferredForename(Element preferredForenameElement) {
        if (preferredForenameElement != null) {
            this.preferredForename = preferredForenameElement.getTextTrim();
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
     * Gets the NCYearActual.
     *
     * @return String
     */
    public String getNCYearActual() {
        return nCYearActual;
    }

    /**
     * Sets the NCYearActual.
     *
     * @param nCYearActual void
     */
    public void setNCYearActual(String nCYearActual) {
        this.nCYearActual = nCYearActual;
    }

    /**
     * Sets the NCYearActual from a DfE XML Element.
     *
     * @param nCYearActualElement void
     */
    public void setNCyearActual(Element nCYearActualElement) {
        if (nCYearActualElement != null) {
            this.nCYearActual = nCYearActualElement.getTextTrim();
        }
    }

    /**
     * Gets the ethnicity.
     *
     * @return String
     */
    public String getEthnicity() {
        return ethnicity;
    }

    /**
     * Sets the ethnicity.
     *
     * @param ethnicity void
     */
    public void setEthnicity(String ethnicity) {
        this.ethnicity = ethnicity;
    }

    /**
     * Sets the ethnicity from a DfE XML Element.
     *
     * @param ethnicityElement void
     */
    public void setEthnicity(Element ethnicityElement) {
        if (ethnicityElement != null) {
            this.ethnicity = ethnicityElement.getTextTrim();
        }
    }

    /**
     * Gets the ethnicitySource.
     *
     * @return String
     */
    public String getEthnicitySource() {
        return ethnicitySource;
    }

    /**
     * Sets the ethnicitySource.
     *
     * @param ethnicitySource void
     */
    public void setEthnicitySource(String ethnicitySource) {
        this.ethnicitySource = ethnicitySource;
    }

    /**
     * Sets the ethnicitySource from a DfE XML Element.
     *
     * @param ethnicitySourceElement void
     */
    public void setEthnicitySource(Element ethnicitySourceElement) {
        if (ethnicitySourceElement != null) {
            this.ethnicitySource = ethnicitySourceElement.getTextTrim();
        }
    }

    /**
     * Gets the first languageCode in the language List.
     *
     * @return String
     */
    public String getFirstLanguageCode() {
        String languageCode = null;
        if (languages != null && languages.size() > 0) {
            DfELanguage langauge = languages.get(0);
            languageCode = langauge.getLanguageCode();
        }
        return languageCode;
    }

    /**
     * Add to the languages collection.
     *
     * @param dfELangage DfELanguage
     */
    public void addDfELanguage(DfELanguage dfELangage) {
        languages.add(dfELangage);
    }

    /**
     * Gets the fSMReviewDate.
     *
     * @return PlainDate
     */
    public PlainDate getFSMReviewDate() {
        return fSMReviewDate;
    }

    /**
     * Sets the fSMReviewDate.
     *
     * @param fSMReviewDate void
     */
    public void setFSMReviewDate(PlainDate fSMReviewDate) {
        this.fSMReviewDate = fSMReviewDate;
    }

    /**
     * Sets the fSMReviewDate from a DfE XML Element.
     *
     * @param fSMReviewDateElement void
     */
    public void setFSMReviewDate(Element fSMReviewDateElement) {
        if (fSMReviewDateElement != null) {
            String reviewDateStr = fSMReviewDateElement.getTextTrim();
            if (!StringUtils.isEmpty(reviewDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(reviewDateStr);
                    this.fSMReviewDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.fSMReviewDate = null;
                }
            }
        }
    }

    /**
     * Sets the fSMReviewDate from a String Date.
     *
     * @param fSMReviewDateStr void
     */
    public void setFSMReviewDate(String fSMReviewDateStr) {
        if (fSMReviewDateStr != null) {
            fSMReviewDateStr = fSMReviewDateStr.trim();
            if (!StringUtils.isEmpty(fSMReviewDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(fSMReviewDateStr);
                    this.fSMReviewDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.fSMReviewDate = null;
                }
            }
        }
    }

    /**
     * Gets the enrollStatus.
     *
     * @return String
     */
    public String getEnrollStatus() {
        return enrollStatus;
    }

    /**
     * Sets the enrollStatus.
     *
     * @param enrollStatus void
     */
    public void setEnrollStatus(String enrollStatus) {
        this.enrollStatus = enrollStatus;
    }

    /**
     * Sets the enrollStatus from a DfE XML Element.
     *
     * @param enrollStatusElement void
     */
    public void setEnrollStatus(Element enrollStatusElement) {
        if (enrollStatusElement != null) {
            this.enrollStatus = enrollStatusElement.getTextTrim();
        }
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

    /**
     * Gets the languages list.
     *
     * @return ArrayList<DfELanguage>
     */
    public ArrayList<DfELanguage> getLanguages() {
        return languages;
    }

    /**
     * Gets the SENNeed list.
     *
     * @return ArrayList<SENNeed>
     */
    public ArrayList<DfESENNeed> getSENNeeds() {
        return sENNeeds;
    }

    /**
     * Adds a DfESENNeed to the Pupil's DfESENNeed Collection.
     *
     * @param dfESENNeed DfESENNeed
     */
    public void addSENNeed(DfESENNeed dfESENNeed) {
        sENNeeds.add(dfESENNeed);
    }

    /**
     * Gets the DfEFSMInstance list.
     *
     * @return ArrayList<DfEFSMInstance>
     */
    public ArrayList<DfEFSMInstance> getFSMInstances() {
        return fSMInstances;
    }

    /**
     * Adds a DfEFSMInstance to the Pupil's DfEFSMInstances Collection.
     *
     * @param dfEFSMInstance DfEFSMInstance
     */
    public void addFSMInstance(DfEFSMInstance dfEFSMInstance) {
        fSMInstances.add(dfEFSMInstance);
    }

    /**
     * Gets the nAWDetails list.
     *
     * @return ArrayList<DfENAWDetail>
     */
    public ArrayList<DfENAWDetail> getNAWDetails() {
        return nAWDetails;
    }

    /**
     * Adds a DfENAWDetail to the Pupil's nAWDetails Collection.
     *
     * @param dfENAWDetail DfENAWDetail
     */
    public void addNAWDetail(DfENAWDetail dfENAWDetail) {
        nAWDetails.add(dfENAWDetail);
    }

    /**
     * Gets the Pupil's DfEAddress.
     *
     * @return DfEAddress
     */
    public DfEAddress getDfEAddress() {
        return dfEAddress;
    }

    /**
     * Sets the dfEAddress.
     *
     * @param dfEAddress void
     */
    public void setDfEAddress(DfEAddress dfEAddress) {
        this.dfEAddress = dfEAddress;
    }

    /**
     * Gets the Pupil's Contacts list.
     *
     * @return ArrayList<DfEContact>
     */
    public ArrayList<DfEContact> getContacts() {
        return contacts;
    }

    /**
     * Adds a DfEContact to the Pupil's Contact Collection.
     *
     * @param dfEContact DfEContact
     */
    public void addContact(DfEContact dfEContact) {
        contacts.add(dfEContact);
    }

    /**
     * Gets the DfEAttendance list.
     *
     * @return ArrayList<DfEAttendance>
     */
    public ArrayList<DfEAttendance> getAttendances() {
        return studentAttendances;
    }

    /**
     * Adds all DfEAttendance's to the Pupil's studentAttendances Collection.
     *
     * @param attendance DfEAttendance
     */
    public void addAttendance(DfEAttendance attendance) {
        studentAttendances.add(attendance);
    }

    /**
     * Adds all DfEAttendance's to the Pupil's studentAttendances Collection.
     *
     * @param OldAttendances ArrayList<DfEAttendance>
     */
    public void addAllAttendances(ArrayList<DfEAttendance> OldAttendances) {
        studentAttendances.addAll(OldAttendances);
    }

    /**
     * Gets the DfESchoolHistory list.
     *
     * @return ArrayList<DfESchoolHistory>
     */
    public ArrayList<DfESchoolHistory> getSchoolHistories() {
        return schoolHistories;
    }

    /**
     * Adds a DfESchoolHistory to the Pupil's schoolHistories Collection.
     *
     * @param dfESchoolHistory DfESchoolHistory
     */
    public void addSchoolHistory(DfESchoolHistory dfESchoolHistory) {
        schoolHistories.add(dfESchoolHistory);
    }

    /**
     * Adds all DfESchoolHistory's to the Pupil's schoolHistories Collection.
     *
     * @param OldSchoolHistories ArrayList<DfESchoolHistory>
     */
    public void addAllSchoolHistory(ArrayList<DfESchoolHistory> OldSchoolHistories) {
        schoolHistories.addAll(OldSchoolHistories);
    }

    /**
     * Gets the DfEStageAssessment list.
     *
     * @return ArrayList<DfEStageAssessment>
     */
    public ArrayList<DfEStageAssessment> getStageAssessments() {
        return stageAssessments;
    }

    /**
     * Adds a DfEStageAssessment to the Pupil's stageAssessments Collection.
     *
     * @param dfEStageAssessment DfEStageAssessment
     */
    public void addStageAssessment(DfEStageAssessment dfEStageAssessment) {
        stageAssessments.add(dfEStageAssessment);
    }

    /**
     * Adds all DfEStageAssessment's to the Pupil's stageAssessments Collection.
     *
     * @param OldStageAssessments ArrayList<DfEStageAssessment>
     */
    public void addAllStageAssessment(ArrayList<DfEStageAssessment> OldStageAssessments) {
        stageAssessments.addAll(OldStageAssessments);
    }

    /**
     * Gets the sENStartDate.
     *
     * @return PlainDate
     */
    public PlainDate getSENStartDate() {
        return sENStartDate;
    }

    /**
     * Sets the sENStartDate.
     *
     * @param sENStartDate void
     */
    public void setSENStartDate(PlainDate sENStartDate) {
        this.sENStartDate = sENStartDate;
    }

    /**
     * Sets the SENStartDate from a DfE XML Element.
     *
     * @param sENStartDateElement void
     */
    public void setSENStartDate(Element sENStartDateElement) {
        if (sENStartDateElement != null) {
            String sENStartDateStr = sENStartDateElement.getTextTrim();
            if (!StringUtils.isEmpty(sENStartDateStr)) {
                Date date = null;
                try {
                    date = m_dateFormat.parse(sENStartDateStr);
                    this.sENStartDate = new PlainDate(date);
                } catch (ParseException e) {
                    this.sENStartDate = null;
                }
            }
        }
    }

    /**
     * Gets the ArrayList of DfETelephone for Pupil.
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
     * Gets the ArrayList of Disabilities for Pupil.
     *
     * @return ArrayList<String>
     */
    public ArrayList<String> getDisabilities() {
        return disabilities;
    }

    /**
     * Adds a disability to the Pupil's disability Collection.
     *
     * @param disability String
     */
    public void addDisability(String disability) {
        disabilities.add(disability);
    }

    /**
     * Gets the sENProvision.
     *
     * @return String
     */
    public String getSENProvision() {
        return sENProvision;
    }

    /**
     * Sets the sENProvision.
     *
     * @param sENProvision void
     */
    public void setSENProvision(String sENProvision) {
        this.sENProvision = sENProvision;
    }

    /**
     * Sets the sENProvision from a DfE XML Element.
     *
     * @param sENProvisionElement void
     */
    public void setSENProvision(Element sENProvisionElement) {
        if (sENProvisionElement != null) {
            this.sENProvision = sENProvisionElement.getTextTrim();
        }
    }

    /**
     * Gets the attendanceXML.
     *
     * @return String
     */
    public String getAttendanceXML() {
        return attendanceXML;
    }

    /**
     * Sets the attendanceXML.
     *
     * @param attendanceXML void
     */
    public void setAttendanceXML(String attendanceXML) {
        this.attendanceXML = attendanceXML;
    }

    /**
     * Gets the assessmentsXML.
     *
     * @return String
     */
    public String getAssessmentsXML() {
        return assessmentsXML;
    }

    /**
     * Sets the assessmentsXML.
     *
     * @param assessmentsXML void
     */
    public void setAssessmentsXML(String assessmentsXML) {
        this.assessmentsXML = assessmentsXML;
    }

    /**
     * Gets the schoolHistoryXML.
     *
     * @return String
     */
    public String getSchoolHistoryXML() {
        return schoolHistoryXML;
    }

    /**
     * Sets the schoolHistoryXML.
     *
     * @param schoolHistoryXML void
     */
    public void setSchoolHistoryXML(String schoolHistoryXML) {
        this.schoolHistoryXML = schoolHistoryXML;
    }

    /**
     * Gets the medicalFlag.
     *
     * @return String
     */
    public Boolean getMedicalFlag() {
        return medicalFlag;
    }

    /**
     * Sets the medicalFlag.
     *
     * @param medicalFlag void
     */
    public void setMedicalFlag(Boolean medicalFlag) {
        this.medicalFlag = medicalFlag;
    }

    /**
     * Sets the medicalFlag from a DfE XML Element.
     *
     * @param medicalFlagElement void
     */
    public void setMedicalFlag(Element medicalFlagElement) {
        if (medicalFlagElement != null) {
            String resp = medicalFlagElement.getTextTrim().toLowerCase();
            this.medicalFlag = Boolean.valueOf(DfEManager.STRING_TRUE.equals(resp));
        }
    }

    /**
     * Gets the inCare.
     *
     * @return String
     */
    public Boolean getInCare() {
        return inCare;
    }

    /**
     * Sets the inCare.
     *
     * @param inCare void
     */
    public void setInCare(Boolean inCare) {
        this.inCare = inCare;
    }

    /**
     * Sets the inCare from a DfE XML Element.
     *
     * @param inCareElement void
     */
    public void setInCare(Element inCareElement) {
        if (inCareElement != null) {
            String resp = inCareElement.getTextTrim().toLowerCase();
            this.inCare = Boolean.valueOf(DfEManager.STRING_TRUE.equals(resp));
        }
    }

    /**
     * Gets the careAuthority.
     *
     * @return String
     */
    public String getCareAuthority() {
        return careAuthority;
    }

    /**
     * Sets the careAuthority.
     *
     * @param careAuthority void
     */
    public void setCareAuthority(String careAuthority) {
        this.careAuthority = careAuthority;
    }

    /**
     * Sets the careAuthority from a DfE XML Element.
     *
     * @param careAuthorityElement void
     */
    public void setCareAuthority(Element careAuthorityElement) {
        if (careAuthorityElement != null) {
            this.careAuthority = careAuthorityElement.getTextTrim();
        }
    }

}
