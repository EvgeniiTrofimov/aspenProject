/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.health.ma;

import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.HealthCondition;
import com.x2dev.sis.model.beans.HealthScreening;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.GradeLevelHistory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the MA School Health Record. This report includes a list of health
 * screenings,
 * student information and contact information for a student.
 *
 * @author X2 Development Corporation
 *
 */
public class MASchoolHealthRecordData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final int YEARS_TO_CALCULATE = 15;

    private static final int DEFAULT_MAX_GRADE_LEVEL = 12;

    /**
     * Extended Dictionary names for General, Postural, Hearing, and Vision
     *
     */
    private static final String DDX_HSC_GENERAL = "ddxHscGeneral";
    private static final String DDX_HSC_POSTURAL = "ddxHscPostural";
    private static final String DDX_HSC_HEARING = "ddxHscHearing";
    private static final String DDX_HSC_VISION = "ddxHscVision";
    private static final String PARAM_PRIMARY_CARE = "primary_care";
    private static final String PARAM_DENTAL_CARE = "dental_care";
    private static final String PARAM_ALLERGIES = "allergies";

    /**
     * Report parameter names
     */
    private static final String PARAM_IS_FEMALE = "is_female";
    private static final String PARAM_IS_MALE = "is_male";
    private static final String PARAM_STUDENT = "student";
    private static final String PARAM_SCHOOL = "school";
    private static final String PARAM_PLACE_OF_BIRTH = "place_of_birth";
    private static final String PARAM_HEALTH_INSURANCE = "health_insurance";
    private static final String PARAM_CONTACT_PARENT = "parent_";
    private static final String PARAM_CONTACT_EMERGENCY = "contact_";
    /**
     * Grid fields
     */
    private static final String FIELD_GRADE_LEVEL = "grade_level";
    private static final String FIELD_STUDENT_AGE = "student_age";
    private static final String FIELD_SCHOOL_DISTRICT = "school_district";
    private static final String FIELD_SCHOOL_YEAR = "school_year";
    private static final String FIELD_BMI = "bmi";
    private static final String FIELD_WEIGHT = "weight";
    private static final String FIELD_HEIGHT = "height";
    private static final String FIELD_POSTURAL_REFER = "postural_refer";
    private static final String FIELD_POSTURAL_PASS = "postural_pass";
    private static final String FIELD_RIGHT_EAR_REFER = "right_ear_refer";
    private static final String FIELD_LEFT_EAR_REFER = "left_ear_refer";
    private static final String FIELD_RIGHT_EAR_PASS = "right_ear_pass";
    private static final String FIELD_LEFT_EAR_PASS = "left_ear_pass";
    private static final String STEREO_REFER = "stereo_refer";
    private static final String STEREO_PASS = "stereo_pass";
    private static final String RIGHT_EYE_REFER = "right_eye_refer";
    private static final String LEFT_EYE_REFER = "left_eye_refer";
    private static final String RIGHT_EYE_PASS = "right_eye_pass";
    private static final String LEFT_EYE_PASS = "left_eye_pass";

    /**
     * Aliases
     */
    private static final String ALIAS_HEARING_POSTURAL_REFER = "hsc-postural-refer";
    private static final String ALIAS_HEARING_POSTURAL_PASS = "hsc-postural-pass";
    private static final String ALIAS_HEARING_RIGHT_EAR_REFER = "hsc-hearing-rrefer";
    private static final String ALIAS_HEARING_LEFT_EAR_REFER = "hsc-hearing-lrefer";
    private static final String ALIAS_HEARING_RIGHT_EAR_PASS = "hsc-hearing-rpass";
    private static final String ALIAS_HEARING_LEFT_EAR_PASS = "hsc-hearing-lpass";
    private static final String ALIAS_VISION_BOTH_REFER = "hsc-vision-brefer";
    private static final String ALIAS_VISION_BOTH_PASS = "hsc-vision-bpass";
    private static final String ALIAS_VISION_RIGHT_EYE_REFER = "hsc-vision-rrefer";
    private static final String ALIAS_VISION_LEFT_EYE_REFER = "hsc-vision-lrefer";
    private static final String ALIAS_VISION_RIGHT_EYE_PASS = "hsc-vision-rpass";
    private static final String ALIAS_VISION_LEFT_EYE_PASS = "hsc-vision-lpass";
    private static final String ALIAS_GENERAL_BMI = "hsc-general-bmi";
    private static final String ALIAS_GENERAL_WEIGHT_LBS = "hsc-general-weight-lbs";
    private static final String ALIAS_GENERAL_HEIGHT_IN = "hsc-general-height-in";
    private static final String ALIAS_HEALTH_INSURANCE = "psn-health-insurance";
    private static final String ALIAS_STD_PLACE_OF_BIRTH = "DOE 08";

    /**
     * Gender Code
     */
    private static final String GENDER_CODE_MALE = "m";
    private static final String GENDER_CODE_FEMALE = "f";

    private static final String[] RESPONSIBLE_CODES =
            {"Parent", "Mother", "Father", "Guardian", "Guardian/Aunt", "Guardian/Uncle"};

    private static final String CONDITION_TYPE_ALLERGY = "Allergy";
    private static final String RELATIONSHIP_CODE_DOCTOR = "Doctor";
    private static final String RELATIONSHIP_CODE_DENTIST = "Dentist";

    private static final String STRING_EMPTY = "";
    private static final String STRING_X = "X";
    private static final String STRING_ONE = "1";

    private SisStudent m_currentStudent;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        addParameter(PARAM_SCHOOL, this.getSchool());
        addParameter(PARAM_STUDENT, m_currentStudent);

        addParameter(PARAM_IS_MALE,
                Boolean.valueOf(m_currentStudent.getPerson().getGenderCode().toLowerCase().equals(GENDER_CODE_MALE)));
        addParameter(PARAM_IS_FEMALE,
                Boolean.valueOf(m_currentStudent.getPerson().getGenderCode().toLowerCase().equals(GENDER_CODE_FEMALE)));

        // aliases for health insurance, and place of birth on the person table.
        DataDictionary aliasDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        String placeOfBirth =
                (String) m_currentStudent.getFieldValueByAlias(ALIAS_STD_PLACE_OF_BIRTH, aliasDictionary);
        String healthInsurance =
                (String) m_currentStudent.getPerson().getFieldValueByAlias(ALIAS_HEALTH_INSURANCE, aliasDictionary);
        addParameter(PARAM_PLACE_OF_BIRTH, placeOfBirth);
        addParameter(PARAM_HEALTH_INSURANCE, healthInsurance);

        addContactInformation(m_currentStudent.getOid());
        addAllergies(m_currentStudent.getOid());
        addPrimaryCareProvider(m_currentStudent.getOid());
        addDentalCareProvider(m_currentStudent.getOid());

        TreeMap<Integer, HashMap<String, HealthScreening>> map =
                new TreeMap<Integer, HashMap<String, HealthScreening>>();
        HashMap<Integer, DistrictSchoolYearContext> contextMap = new HashMap<Integer, DistrictSchoolYearContext>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(HealthScreening.COL_STUDENT_OID, m_currentStudent.getOid());
        QueryByCriteria query = new QueryByCriteria(HealthScreening.class, criteria);
        query.addOrderByDescending(HealthScreening.COL_DATE);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        // get a collection of school years for this student
        Criteria schoolYearCriteria = new Criteria();
        schoolYearCriteria.addEqualTo(DistrictSchoolYearContext.COL_ORGANIZATION1_OID,
                m_currentStudent.getOrganization1Oid());

        QueryByCriteria schoolYearQuery = new QueryByCriteria(DistrictSchoolYearContext.class, schoolYearCriteria);
        schoolYearQuery.addOrderByAscending(DistrictSchoolYearContext.COL_START_DATE);

        Collection<DistrictSchoolYearContext> schoolYears = getBroker().getCollectionByQuery(schoolYearQuery);

        try {
            while (iterator.hasNext()) {
                HealthScreening screening = (HealthScreening) iterator.next();

                int schoolYear = getSchoolContextForDate(schoolYears, screening.getDate()).getSchoolYear();
                contextMap.put(Integer.valueOf(schoolYear), getSchoolContextForDate(schoolYears, screening.getDate()));

                if (!map.containsKey(Integer.valueOf(schoolYear))) {
                    map.put(Integer.valueOf(schoolYear), new HashMap<String, HealthScreening>());
                }

                HashMap<String, HealthScreening> yearlyScreening = map.get(Integer.valueOf(schoolYear));

                if (!yearlyScreening.containsKey(screening.getExtendedDataDictionaryOid())) {
                    String type = screening.getExtendedDataDictionaryOid();
                    if (type != null) {
                        yearlyScreening.put(screening.getExtendedDataDictionaryOid(), screening);
                    }
                }
            }
        } finally {
            iterator.close();
        }

        ReportDataGrid grid = new ReportDataGrid();
        GradeLevelHistory gradeLevelHistory = new GradeLevelHistory(m_currentStudent.getOid(), YEARS_TO_CALCULATE,
                m_currentStudent.getOrganization1(), getBroker());
        HashMap<String, String> gradeSchoolHistory = getGradeLevelToSchoolHistoryMap(schoolYears);

        for (Integer year : map.keySet()) {
            HashMap<String, HealthScreening> content = map.get(year);
            grid.append();

            grid.set(FIELD_SCHOOL_YEAR, year.toString());
            grid.set(FIELD_SCHOOL_DISTRICT, gradeSchoolHistory
                    .get(gradeLevelHistory.getGradeLevel(m_currentStudent.getOid(), year.intValue())));
            grid.set(FIELD_STUDENT_AGE,
                    Person.getAgeAsOfDate(m_currentStudent.getPerson().getDob(), contextMap.get(year).getStartDate())
                            + STRING_EMPTY);
            grid.set(FIELD_GRADE_LEVEL, gradeLevelHistory.getGradeLevel(m_currentStudent.getOid(), year.intValue()));

            boolean hasRecords = false;

            for (String screeningType : content.keySet()) {
                HealthScreening screening = content.get(screeningType);

                if (screeningType == null) {
                    continue;

                }

                if (screeningType.startsWith(DDX_HSC_VISION)) {
                    ExtendedDataDictionary dictionary = (ExtendedDataDictionary) getBroker()
                            .getBeanByOid(ExtendedDataDictionary.class, DDX_HSC_VISION);
                    DataDictionary districtDictionary =
                            DataDictionary.getDistrictDictionary(dictionary, getBroker().getPersistenceKey());

                    String leftPass =
                            (String) screening.getFieldValueByAlias(ALIAS_VISION_LEFT_EYE_PASS, districtDictionary);
                    String rightPass =
                            (String) screening.getFieldValueByAlias(ALIAS_VISION_RIGHT_EYE_PASS, districtDictionary);
                    String leftRefer =
                            (String) screening.getFieldValueByAlias(ALIAS_VISION_LEFT_EYE_REFER, districtDictionary);
                    String rightRefer =
                            (String) screening.getFieldValueByAlias(ALIAS_VISION_RIGHT_EYE_REFER, districtDictionary);
                    String bothPass =
                            (String) screening.getFieldValueByAlias(ALIAS_VISION_BOTH_PASS, districtDictionary);
                    String bothRefer =
                            (String) screening.getFieldValueByAlias(ALIAS_VISION_BOTH_REFER, districtDictionary);

                    if (leftPass != null) {
                        grid.set(LEFT_EYE_PASS, leftPass.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (rightPass != null) {
                        grid.set(RIGHT_EYE_PASS, rightPass.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (leftRefer != null) {
                        grid.set(LEFT_EYE_REFER, leftRefer.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (rightRefer != null) {
                        grid.set(RIGHT_EYE_REFER, rightRefer.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (bothPass != null) {
                        grid.set(STEREO_PASS, bothPass.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (bothRefer != null) {
                        grid.set(STEREO_REFER, bothRefer.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }

                    hasRecords = true;
                } else if (screeningType.startsWith(DDX_HSC_HEARING)) {
                    ExtendedDataDictionary dictionary = (ExtendedDataDictionary) getBroker()
                            .getBeanByOid(ExtendedDataDictionary.class, DDX_HSC_HEARING);
                    DataDictionary districtDictionary =
                            DataDictionary.getDistrictDictionary(dictionary, getBroker().getPersistenceKey());

                    String leftPass =
                            (String) screening.getFieldValueByAlias(ALIAS_HEARING_LEFT_EAR_PASS, districtDictionary);
                    String rightPass =
                            (String) screening.getFieldValueByAlias(ALIAS_HEARING_RIGHT_EAR_PASS, districtDictionary);
                    String leftRefer =
                            (String) screening.getFieldValueByAlias(ALIAS_HEARING_LEFT_EAR_REFER, districtDictionary);
                    String rightRefer =
                            (String) screening.getFieldValueByAlias(ALIAS_HEARING_RIGHT_EAR_REFER, districtDictionary);

                    if (leftPass != null) {
                        grid.set(FIELD_LEFT_EAR_PASS, leftPass.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }

                    if (rightPass != null) {
                        grid.set(FIELD_RIGHT_EAR_PASS, rightPass.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (leftRefer != null) {
                        grid.set(FIELD_LEFT_EAR_REFER, leftRefer.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (rightRefer != null) {
                        grid.set(FIELD_RIGHT_EAR_REFER, rightRefer.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }

                    hasRecords = true;
                } else if (screeningType.startsWith(DDX_HSC_POSTURAL)) {
                    ExtendedDataDictionary dictionary = (ExtendedDataDictionary) getBroker()
                            .getBeanByOid(ExtendedDataDictionary.class, DDX_HSC_POSTURAL);
                    DataDictionary districtDictionary =
                            DataDictionary.getDistrictDictionary(dictionary, getBroker().getPersistenceKey());

                    String posturalPass =
                            (String) screening.getFieldValueByAlias(ALIAS_HEARING_POSTURAL_PASS, districtDictionary);
                    String posturalRefer =
                            (String) screening.getFieldValueByAlias(ALIAS_HEARING_POSTURAL_REFER, districtDictionary);

                    if (posturalPass != null) {
                        grid.set(FIELD_POSTURAL_PASS, posturalPass.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }
                    if (posturalRefer != null) {
                        grid.set(FIELD_POSTURAL_REFER, posturalRefer.equals(STRING_ONE) ? STRING_X : STRING_EMPTY);
                    }

                    hasRecords = true;
                } else if (screeningType.startsWith(DDX_HSC_GENERAL)) {
                    ExtendedDataDictionary dictionary = (ExtendedDataDictionary) getBroker()
                            .getBeanByOid(ExtendedDataDictionary.class, DDX_HSC_GENERAL);
                    DataDictionary districtDictionary =
                            DataDictionary.getDistrictDictionary(dictionary, getBroker().getPersistenceKey());

                    String height =
                            (String) screening.getFieldValueByAlias(ALIAS_GENERAL_HEIGHT_IN, districtDictionary);
                    String weight =
                            (String) screening.getFieldValueByAlias(ALIAS_GENERAL_WEIGHT_LBS, districtDictionary);
                    String bmi = (String) screening.getFieldValueByAlias(ALIAS_GENERAL_BMI, districtDictionary);

                    if (height != null) {
                        grid.set(FIELD_HEIGHT, height);
                    }
                    if (weight != null) {
                        grid.set(FIELD_WEIGHT, weight);
                    }
                    if (bmi != null) {
                        grid.set(FIELD_BMI, bmi);
                    }

                    hasRecords = true;
                }

            }
            if (!hasRecords) {
                grid.deleteRow();
            }
        }

        if (grid.isEmpty()) {
            grid.append();
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Returns the DistrictSchoolYearContext which occurs during the given date.
     *
     * @param schoolYears Collection<DistrictSchoolYearContext>
     * @param date PlainDate
     * @return DistrictSchoolYearContext
     */
    public DistrictSchoolYearContext getSchoolContextForDate(Collection<DistrictSchoolYearContext> schoolYears,
                                                             PlainDate date) {
        DistrictSchoolYearContext year = null;

        Iterator schoolYearIterator = schoolYears.iterator();
        while (schoolYearIterator.hasNext()) {
            year = (DistrictSchoolYearContext) schoolYearIterator.next();

            if (!date.before(year.getStartDate())) {
                if (!date.after(year.getEndDate())) {
                    break;
                }
            } else {
                break;
            }
        }

        return year;
    }

    /**
     * Queries for the first Dental Care provider for the student, and passing their
     * person relation to the report.
     *
     * @param studentOid String
     */
    public void addDentalCareProvider(String studentOid) {
        // Dental Care Provider
        Criteria dencCriteria = new Criteria();
        dencCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_STUDENT_OID, studentOid);
        dencCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_RELATIONSHIP_CODE,
                RELATIONSHIP_CODE_DENTIST);

        QueryByCriteria dencQuery = new QueryByCriteria(Contact.class, dencCriteria);
        QueryIterator dencIterator = getBroker().getIteratorByQuery(dencQuery);

        try {
            if (dencIterator.hasNext()) {
                Contact obj = (Contact) dencIterator.next();
                addParameter(PARAM_DENTAL_CARE, obj.getPerson());
            }
        } finally {
            dencIterator.close();
        }
    }

    /**
     * Sets the parameters for information regarding the primary care provider of the student.
     *
     * @param studentOid String
     */
    public void addPrimaryCareProvider(String studentOid) {
        // Primary Care Provider
        Criteria dcCriteria = new Criteria();
        dcCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_STUDENT_OID, studentOid);
        dcCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_RELATIONSHIP_CODE,
                RELATIONSHIP_CODE_DOCTOR);

        QueryByCriteria dcQuery = new QueryByCriteria(Contact.class, dcCriteria);
        QueryIterator dcIterator = getBroker().getIteratorByQuery(dcQuery);

        try {
            if (dcIterator.hasNext()) {
                Contact obj = (Contact) dcIterator.next();
                addParameter(PARAM_PRIMARY_CARE, obj.getPerson());
            }
        } finally {
            dcIterator.close();
        }
    }

    /**
     * Sets the parameters for information regarding the student's allergies.
     *
     * @param studentOid String
     */
    public void addAllergies(String studentOid) {
        // Student Allergies
        Criteria allergyCriteria = new Criteria();
        allergyCriteria.addEqualTo(HealthCondition.COL_STUDENT_OID, studentOid);
        allergyCriteria.addEqualTo(HealthCondition.COL_CONDITION_TYPE, CONDITION_TYPE_ALLERGY);

        QueryByCriteria allergyQuery = new QueryByCriteria(HealthCondition.class, allergyCriteria);
        QueryIterator allergyIterator = getBroker().getIteratorByQuery(allergyQuery);

        LinkedList<String> allergies = new LinkedList<String>();

        try {
            while (allergyIterator.hasNext()) {
                HealthCondition obj = (HealthCondition) allergyIterator.next();

                // Allergy code
                allergies.add(obj.getConditionCode());
            }
        } finally {
            allergyIterator.close();
        }

        addParameter(PARAM_ALLERGIES, StringUtils.convertCollectionToDelimitedString(allergies, ","));

    }

    /**
     * Sets the parameters for information regarding the student contact's information;
     * parents/guardians,
     * emergency contacts.
     *
     * @param studentOid String
     */
    public void addContactInformation(String studentOid) {
        // Parent Contacts
        Criteria pcCriteria = new Criteria();
        pcCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_STUDENT_OID, studentOid);
        pcCriteria.addIn(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_RELATIONSHIP_CODE,
                Arrays.asList(RESPONSIBLE_CODES));

        QueryByCriteria pcQuery = new QueryByCriteria(Contact.class, pcCriteria);
        pcQuery.addOrderByAscending(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_EMERGENCY_PRIORITY);
        Collection<Contact> studentContacts = getBroker().getCollectionByQuery(pcQuery);

        int parentCount = 1;
        for (Contact studentContact : studentContacts) {
            this.addParameter(PARAM_CONTACT_PARENT + parentCount, studentContact.getPerson());

            parentCount++;
            if (parentCount == 3) {
                break;
            }
        }

        // Emergency Contacts
        Criteria ecCriteria = new Criteria();
        ecCriteria.addEqualTo(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_STUDENT_OID, studentOid);
        QueryByCriteria ecQuery = new QueryByCriteria(Contact.class, ecCriteria);
        ecQuery.addOrderByAscending(Contact.REL_STUDENT_CONTACTS + "." + StudentContact.COL_EMERGENCY_PRIORITY);
        Collection<Contact> emergencyContacts = getBroker().getCollectionByQuery(ecQuery);

        // Emergency Contacts
        int emergencyCount = 1;
        for (Contact emergencyContact : emergencyContacts) {
            this.addParameter(PARAM_CONTACT_EMERGENCY + emergencyCount, emergencyContact.getPerson());

            emergencyCount++;
            if (emergencyCount == 3) {
                break;
            }
        }

    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
    }



    /**
     * Returns a map of grade levels to school names for the student's history.
     *
     * @param schoolYears Collection<DistrictSchoolYearContext>
     * @return Hash map
     */
    private HashMap<String, String> getGradeLevelToSchoolHistoryMap(Collection<DistrictSchoolYearContext> schoolYears) {

        StudentEnrollment previousRecord = null;
        int ageOutYog = Integer.parseInt(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.SYS_STD_AGEOUTYOG));
        String ageOutGrade = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.SYS_STD_AGEOUTGRADECODE);

        HashMap<String, String> gradeSchoolHistory = new HashMap<String, String>(YEARS_TO_CALCULATE);
        TreeMap<Integer, List<String>> gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());

        // Final year to calculate grade levels for
        int finalYear = m_currentStudent.getYog();
        if (finalYear == 0 || finalYear == ageOutYog) {
            TimeZone timeZone = OrganizationManager.getTimeZone(m_currentStudent);
            finalYear = getSchoolYearForDate(schoolYears, new PlainDate(timeZone));
        }

        /*
         * For each pair of successive enrollment records, calculate the grade level for
         * each school year that pair spans.
         */
        int startYear = finalYear - YEARS_TO_CALCULATE + 1; // +1 because final year counts
        int endYear = -1;
        int yog = -1;

        Calendar calendar = Calendar.getInstance();
        Collection<StudentEnrollment> yogChangeList = getBroker().getCollectionByQuery(getEnrollmentQuery());

        Iterator yogChangeIterator = yogChangeList.iterator();
        while (yogChangeIterator.hasNext()) {
            StudentEnrollment changeRecord = (StudentEnrollment) yogChangeIterator.next();

            // Don't consider records outside of the years we are calculating
            calendar.setTime(changeRecord.getEnrollmentDate());
            if (calendar.get(Calendar.YEAR) >= finalYear - YEARS_TO_CALCULATE + 1) {
                endYear = getSchoolYearForDate(schoolYears, changeRecord.getEnrollmentDate());

                if (previousRecord != null) {
                    startYear = getSchoolYearForDate(schoolYears, previousRecord.getEnrollmentDate());
                    yog = previousRecord.getYog();
                } else {
                    yog = changeRecord.getYog();
                }

                applyGradeLevels(startYear,
                        endYear,
                        yog,
                        ageOutYog,
                        ageOutGrade,
                        gradeLevelMap,
                        gradeSchoolHistory,
                        previousRecord);

                previousRecord = changeRecord;
            }
        }

        /*
         * The last pair of successive enrollment records is the most recent enrollment
         * record from the query up to the student's YOG. If the students YOG is the age out
         * YOG (they have no real YOG), calculate to the end of this year.
         */
            startYear = getSchoolYearForDate(schoolYears, previousRecord.getEnrollmentDate());

        applyGradeLevels(startYear,
                finalYear + 1, // last entry need to include the end year
                previousRecord.getYog(),
                ageOutYog,
                ageOutGrade,
                gradeLevelMap,
                gradeSchoolHistory,
                previousRecord);

        return gradeSchoolHistory;
    }

    /**
     * Adds entries into the grade history map from the start year to the end year (non-inclusive).
     * The passed YOG is the student's YOG for the passed year span.
     *
     * @param startYear int
     * @param endYear int
     * @param yog int
     * @param ageOutYog int
     * @param ageOutGrade String
     * @param gradeLevelMap TreeMap<Integer,List<String>>
     * @param gradeSchoolHistory HashMap<String,String>
     * @param enrollmentRecord StudentEnrollment
     */
    private void applyGradeLevels(int startYear,
                                  int endYear,
                                  int yog,
                                  int ageOutYog,
                                  String ageOutGrade,
                                  TreeMap<Integer, List<String>> gradeLevelMap,
                                  HashMap<String, String> gradeSchoolHistory,
                                  StudentEnrollment enrollmentRecord) {
        for (int i = startYear; i < endYear; i++) {
            /*
             * If the student's YOG isn't the age out YOG, then the grade level
             * equals the max grade level minus the difference between their YOG and
             * the currentYear.
             *
             * If the student's YOG is the age out YOG, then their grade level for
             * current year is the age out grade level.
             */
            if (yog != ageOutYog) {
                int grade = DEFAULT_MAX_GRADE_LEVEL - (yog - i);
                List<String> gradeCodes = gradeLevelMap.get(Integer.valueOf(grade));
                if (gradeCodes != null && !gradeCodes.isEmpty()) {
                    if (enrollmentRecord == null) {
                        gradeSchoolHistory.put(gradeCodes.get(0), STRING_EMPTY);
                    } else {
                        gradeSchoolHistory.put(gradeCodes.get(0), enrollmentRecord.getSchool().getName());
                    }
                }
            } else {
                if (enrollmentRecord == null) {
                    gradeSchoolHistory.put(ageOutGrade, STRING_EMPTY);
                } else {
                    gradeSchoolHistory.put(ageOutGrade, enrollmentRecord.getSchool().getName());
                }
            }
        }
    }

    /**
     * Returns the school year that includes the passed date.
     *
     * @param schoolYears Collection<DistrictSchoolYearContext>
     * @param date Date
     * @return int; -1 if a school year could not be determined
     */
    private int getSchoolYearForDate(Collection<DistrictSchoolYearContext> schoolYears, Date date) {
        DistrictSchoolYearContext year = null;

        Iterator schoolYearIterator = schoolYears.iterator();
        while (schoolYearIterator.hasNext()) {
            year = (DistrictSchoolYearContext) schoolYearIterator.next();

            if (!date.before(year.getStartDate())) {
                if (!date.after(year.getEndDate())) {
                    break;
                }
            } else {
                break;
            }
        }

        int yearValue = -1;

        if (year != null) {
            yearValue = year.getSchoolYear();
        }

        return yearValue;
    }

    /**
     * Returns the enrollment records to consider when creating the grade level history map.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria getEnrollmentQuery() {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        enrollmentCriteria.addNotEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.STATUS_CHANGE);
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, m_currentStudent.getOid());

        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);

        return enrollmentQuery;
    }
}
