/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.framework.persistence.adjusters.UnionAdjuster;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.admin.ReferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Exports student demographic information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class StudentDemographicExport extends GdeExportJavaSource {
    private static final String ALIAS_STD_PGM_PROGRAM_CODE_ALIAS = "std-pgm-designation-code";
    private static final String ALIAS_STUDENT_DESIGNATION = "std-sped-category";

    /**
     * Reflect field amount with field ID, field user friendly name and boolean parameter which
     * contains true if field is ID and false otherwise.
     * In constructor we will use Y/N chars to reflect if field ID or not
     */
    enum EXPORT_FIELDS {
        FIELD_SCHOOL_NUMBER("schoolNumber", "School number"), FIELD_STUDENT_NUMBER("studentNumber",
                "Student number"), FIELD_HOMEROOM("homeroom", "Homeroom"), FIELD_TEACHER_NAME("teacherName",
                        "Teacher name"), FIELD_PEN("pen", "PEN"), FIELD_USER_NAME("userName",
                                "User name"), FIELD_LEGAL_SURNAME("legalSurname",
                                        "Legal surname"), FIELD_LEGAL_FIRST_NAME("legalFirstName",
                                                "Legal first name"), FIELD_LEGAL_MIDDLE_NAME("legalMiddleName",
                                                        "Legal middle name"), FIELD_USUAL_SURNAME("usualSurname",
                                                                "Usual surname"), FIELD_USUAL_FIRST_NAME(
                                                                        "usualFirstName",
                                                                        "Usual first name"), FIELD_USUAL_MIDDLE_NAME(
                                                                                "usualMiddleName",
                                                                                "Usual middle name"), FIELD_STREET_NUMBER(
                                                                                        "streetNumber",
                                                                                        "Street number"), FIELD_STREET_NAME(
                                                                                                "streetName",
                                                                                                "Street name"), FIELD_APARTMENT(
                                                                                                        "apartment",
                                                                                                        "Apartment"), FIELD_LOT_NUMBER(
                                                                                                                "lotNumber",
                                                                                                                "Lot number"), FIELD_CONCESSION_NUMBER(
                                                                                                                        "concessionNumber",
                                                                                                                        "Concession number"), FIELD_MUNICIPALITY(
                                                                                                                                "municipality",
                                                                                                                                "Municipality"), FIELD_PROVINCE(
                                                                                                                                        "province",
                                                                                                                                        "Province"), FIELD_POSTAL_CODE(
                                                                                                                                                "postalCode",
                                                                                                                                                "Postal code"), FIELD_MAILING_ADDRESS(
                                                                                                                                                        "mailingAddress",
                                                                                                                                                        "Mailing address"), FIELD_DATE_OF_BIRTH(
                                                                                                                                                                "dateOfBirth",
                                                                                                                                                                "Date of birth"), FIELD_PROOF_OF_AGE(
                                                                                                                                                                        "proofOfAge",
                                                                                                                                                                        "Proof of age"), FIELD_GENDER(
                                                                                                                                                                                "gender",
                                                                                                                                                                                "Gender"), FIELD_GRADUATION_PROGRAM(
                                                                                                                                                                                        "graduationProgram",
                                                                                                                                                                                        "Graduation program"), FIELD_PHONE_NUMBER(
                                                                                                                                                                                                "phoneNumber",
                                                                                                                                                                                                "Phone number"), FIELD_GRADE(
                                                                                                                                                                                                        "grade",
                                                                                                                                                                                                        "Grade"), FIELD_SPECIAL_EDUCATION_CATEGORY(
                                                                                                                                                                                                                "specialEducationCategory",
                                                                                                                                                                                                                "Special education category"), FIELD_ABORIGINAL_ANCESTRY(
                                                                                                                                                                                                                        "aboriginalAncestry",
                                                                                                                                                                                                                        "Aboriginal ancestry"), FIELD_FIRST_NATIONS_BAND(
                                                                                                                                                                                                                                "firstNationsBand",
                                                                                                                                                                                                                                "First nations band"), FIELD_DIA(
                                                                                                                                                                                                                                        "dia",
                                                                                                                                                                                                                                        "DIA"), FIELD_HOME_LANGUAGE(
                                                                                                                                                                                                                                                "homeLanguage",
                                                                                                                                                                                                                                                "Home language"), FIELD_ESL(
                                                                                                                                                                                                                                                        "esl",
                                                                                                                                                                                                                                                        "ESL"), FIELD_FRENCH_IMMERSION(
                                                                                                                                                                                                                                                                "frenchImmersion",
                                                                                                                                                                                                                                                                "French immersion"), FIELD_IMMIGRATION_STATUS(
                                                                                                                                                                                                                                                                        "immigrationStatus",
                                                                                                                                                                                                                                                                        "Immigration status"), FIELD_CITIZENSHIP(
                                                                                                                                                                                                                                                                                "citizenship",
                                                                                                                                                                                                                                                                                "Citizenship"), FIELD_COUNTRY_OF_BIRTH(
                                                                                                                                                                                                                                                                                        "countryOfBirth",
                                                                                                                                                                                                                                                                                        "Country of birth"), FIELD_CARE_CARD_NUMBER(
                                                                                                                                                                                                                                                                                                "careCardNumber",
                                                                                                                                                                                                                                                                                                "Care card number"), FIELD_ADMISSION_REASON(
                                                                                                                                                                                                                                                                                                        "admissionReason",
                                                                                                                                                                                                                                                                                                        "Admission reason"), FIELD_ADMISSION_DATE(
                                                                                                                                                                                                                                                                                                                "admissionDate",
                                                                                                                                                                                                                                                                                                                "Admission date"), FIELD_WITHDRAW_REASON(
                                                                                                                                                                                                                                                                                                                        "WithdrawReason",
                                                                                                                                                                                                                                                                                                                        "Withdraw reason"), FIELD_WITHDRAW_DATE(
                                                                                                                                                                                                                                                                                                                                "withdrawDate",
                                                                                                                                                                                                                                                                                                                                "Withdraw date"), FIELD_PREVIOUS_SCHOOL_NUMBER(
                                                                                                                                                                                                                                                                                                                                        "previousSchoolNumber",
                                                                                                                                                                                                                                                                                                                                        "Previous school number"), FIELD_PREVIOUS_SCHOOL_NAME(
                                                                                                                                                                                                                                                                                                                                                "previousSchoolName",
                                                                                                                                                                                                                                                                                                                                                "Previous school name"), FIELD_HOME_SCHOOL_NUMBER(
                                                                                                                                                                                                                                                                                                                                                        "homeSchoolNumber",
                                                                                                                                                                                                                                                                                                                                                        "Home school number"), FIELD_HOME_SCHOOL_NAME(
                                                                                                                                                                                                                                                                                                                                                                "homeSchoolName",
                                                                                                                                                                                                                                                                                                                                                                "Home school name"), FIELD_COUNSELOR_ID(
                                                                                                                                                                                                                                                                                                                                                                        "counselorId",
                                                                                                                                                                                                                                                                                                                                                                        "Counselor id"), FIELD_COUNSELOR_NAME(
                                                                                                                                                                                                                                                                                                                                                                                "counselorName",
                                                                                                                                                                                                                                                                                                                                                                                "Counselor name"), FIELD_CASE_MANAGER(
                                                                                                                                                                                                                                                                                                                                                                                        "caseManager",
                                                                                                                                                                                                                                                                                                                                                                                        "Case manager"), FIELD_PROJECTED_GRADUATION_YEAR(
                                                                                                                                                                                                                                                                                                                                                                                                "projectedGraduationYear",
                                                                                                                                                                                                                                                                                                                                                                                                "Projected graduation year"), FIELD_DATE_GRADUATED(
                                                                                                                                                                                                                                                                                                                                                                                                        "dateGraduated",
                                                                                                                                                                                                                                                                                                                                                                                                        "Date graduated"), FIELD_FAMILY_COURIER_INDICATOR(
                                                                                                                                                                                                                                                                                                                                                                                                                "familyCourierIndicator",
                                                                                                                                                                                                                                                                                                                                                                                                                "Family courier indicator"), FIELD_AM_PM(
                                                                                                                                                                                                                                                                                                                                                                                                                        "ampm",
                                                                                                                                                                                                                                                                                                                                                                                                                        "AM / PM"), FIELD_MEMO_NOTE(
                                                                                                                                                                                                                                                                                                                                                                                                                                "memoNote",
                                                                                                                                                                                                                                                                                                                                                                                                                                "Memo note");

        private String m_fieldId;
        private String m_fieldName;

        /**
         * Gets the field id.
         *
         * @return String
         */
        public String getFieldId() {
            return m_fieldId;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Instantiates a new export fields.
         *
         * @param fieldId String
         * @param fieldName String
         */
        private EXPORT_FIELDS(String fieldId, String fieldName) {
            m_fieldId = fieldId;
            m_fieldName = fieldName;
        }
    }

    private final static String GIFTED_STUDENT_DISABILITY_CODE = "Gifted";

    // Field aliases
    private static final String FIELD_ALIAS_ADR_APARTMENT = "adr-apartment";
    private static final String FIELD_ALIAS_ADR_LOT_NUMBER = "adr-lot-number";
    private static final String FIELD_ALIAS_ADR_CONCESSION_NUMBER = "adr-concession-number";
    private static final String FIELD_ALIAS_PERSON_SURNAME = "psn-surname";
    private static final String FIELD_ALIAS_PERSON_PREF_FIRSTNAME = "psn-preferred-first-name";
    private static final String FIELD_ALIAS_PERSON_PREF_MIDDLENAME = "psn-preferred-middle-name";
    private static final String FIELD_ALIAS_PERSON_PROOF_OF_AGE = "psn-proof-of-age";
    private static final String FIELD_ALIAS_PERSON_INDIAN_ANCESTRY = "psn-indian-ancestry";
    private static final String FIELD_ALIAS_PERSON_TRIBAL_BAND = "psn-tribal-band";
    private static final String FIELD_ALIAS_STUDENT_CASE_MANAGER = "std-case-manager";
    private static final String FIELD_ALIAS_STUDENT_DIA_NUMBER = "std-status-card-num";
    private static final String FIELD_ALIAS_STUDENT_FAMILY_COURIER = "std-family-courier-ind";
    private static final String FIELD_ALIAS_STUDENT_AM_OR_PM = "std-am-or-pm";
    private static final String FIELD_ALIAS_STUDENT_FAMILY_ALERT = "std-family-alert-memo";
    private static final String FIELD_ALIAS_STUDENT_CITIZENSHIP_STATUS = "std-citizenship-status";
    private static final String FIELD_ALIAS_STUDENT_CITIZENSHIP_COUNTRY = "std-citizenship-country";
    private static final String FIELD_ALIAS_STUDENT_COUNTRY_OF_BIRTH = "std-country-of-birth";
    private static final String FIELD_ALIAS_STUDENT_COUNSELOR = "counselor";
    private static final String FIELD_ALIAS_STUDENT_CARE_CARD_NUMBER = "std-care-card";

    // Input parameters
    private static final String PARAM_GRADUATED_STATUS_LIST = "graduatedStatusList";
    private static final String PARAM_INCL_HEADINGS = "includeHeadings";
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";

    // Program constants
    private static final String PROGRAM_CODE_ESL = "1700";
    private static final String PROGRAM_CODE_EFI = "1100";
    private static final String PROGRAM_CODE_LFI = "1400";

    // Other constants
    private static final String ILLEGAL_PHONE_CHARACTERS = "[_\\s\\-\\(\\)x]";

    private DataDictionary m_dictionary;
    private SimpleDateFormat m_dateFormatter;
    private HashSet<String> m_graduatedStatusList;
    private StudentContextReportHelper m_helper;
    private Map<String, Map<String, Staff>> m_schoolHomeroomToStaffMap;
    private Pattern m_illegalPhoneCharacters;
    private boolean m_isIncludeHeadings = false;
    private X2Criteria m_schoolCriteria;

    /*
     * Reference lists
     */
    private Map<String, ReferenceCode> m_citizenshipStatusCodeMap;
    private Map<String, ReferenceCode> m_citizenshipCountryCodeMap;
    private Map<String, ReferenceCode> m_countryOfBirthCodeMap;
    private Map<String, ReferenceCode> m_counselorCodeMap;
    private Map<String, String> m_diplomaTypeMap;
    private Map<String, ReferenceCode> m_designationCodes;
    private Map<String, ReferenceCode> m_entryCodesMap;
    private Map<String, ReferenceCode> m_homeLangCodeMap;
    private Map<String, ReferenceCode> m_proofOfAgeCodeMap;
    private Map<String, ReferenceCode> m_referenceProgramParticipationCodeMap;
    private Map<String, ReferenceCode> m_tribalBandCodeMap;
    private Map<String, ReferenceCode> m_withdrawalCodesMap;

    /*
     * Data maps
     */
    private Map<String, SisStudent> m_studentDesignationsMap;
    private Map<String, StudentEnrollment> m_entryMap;
    private Map<String, SisSchool> m_previousSchoolMap;
    private Map<String, Collection<String>> m_programCodesMap;
    private Map<String, String> m_userLoginByPersonOid;
    private Map<String, StudentEnrollment> m_withdrawalMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(EXPORT_FIELDS.values().length) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        int progressCount = 0;

        /*
         * Load primary students
         */
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, buildPrimaryCriteria());
        query.addOrderByAscending(m_helper.getSchoolRelationship() + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(SisStudent.COL_LOCAL_ID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                SisSchool school = student.getSchool(getCurrentContext().getOid(), getBroker());

                /*
                 * Log progress
                 */
                if (++progressCount % 10000 == 0) {
                    AppGlobals.getLog().info("GDE StudentDemographic: processed " + progressCount + " students");
                }

                addStudentToGrid(student, school, grid);
            }
        } finally {
            iterator.close();
        }

        /*
         * Load secondary students
         */
        query = new QueryByCriteria(StudentSchool.class, buildSecondaryCriteria());
        query.addOrderByAscending(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_SCHOOL_ID);
        query.addOrderByAscending(StudentSchool.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);

        iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                StudentSchool secondary = (StudentSchool) iterator.next();

                /*
                 * Log progress
                 */
                if (++progressCount % 10000 == 0) {
                    AppGlobals.getLog().info("GDE StudentDemographic: processed " + progressCount + " students");
                }

                addStudentToGrid((SisStudent) secondary.getStudent(), (SisSchool) secondary.getSchool(), grid);
            }
        } finally {
            iterator.close();
        }

        /*
         * Cleanup temporary data
         */
        for (X2BaseBean bean : getTemporaryBeans()) {
            getBroker().deleteBean(bean);
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columnNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            columnNames.add(field.getFieldId());
        }

        return columnNames;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columnUserNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            columnUserNames.add(field.getFieldName());
        }

        return columnUserNames;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.reports.bc.GdeExportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        String pgmTblOid =
                BeanManager.getFullOid(StudentProgramParticipation.DICTIONARY_ID, getBroker().getPersistenceKey());
        m_designationCodes = loadRefCodes(getBroker(), pgmTblOid, StudentProgramParticipation.COL_PROGRAM_CODE);

        m_schoolHomeroomToStaffMap = new HashMap<String, Map<String, Staff>>();

        m_isIncludeHeadings = ((Boolean) getParameter(PARAM_INCL_HEADINGS)).booleanValue();
        setIncludeHeaderRow(m_isIncludeHeadings);
        setLineSeparator(FORMAT_EOL_WINDOWS);
        setUseValueWrappers(false);
        setUseEscapes(false);

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        String graduatedStatusParameter = (String) getParameter(PARAM_GRADUATED_STATUS_LIST);
        m_graduatedStatusList = new HashSet<String>(Arrays.asList(graduatedStatusParameter.split(",")));

        /*
         * Formatting options
         */
        m_dateFormatter = new SimpleDateFormat("dd-MMM-yyyy");
        m_illegalPhoneCharacters = Pattern.compile(ILLEGAL_PHONE_CHARACTERS);

        // load reference maps
        m_citizenshipStatusCodeMap = loadRefCodeMapByAlias(FIELD_ALIAS_STUDENT_CITIZENSHIP_STATUS);
        m_citizenshipCountryCodeMap = loadRefCodeMapByAlias(FIELD_ALIAS_STUDENT_CITIZENSHIP_COUNTRY);
        m_counselorCodeMap = loadRefCodeMapByAlias(FIELD_ALIAS_STUDENT_COUNSELOR);
        m_countryOfBirthCodeMap = loadRefCodeMapByAlias(FIELD_ALIAS_STUDENT_COUNTRY_OF_BIRTH);
        m_homeLangCodeMap = loadRefCodeMapByField(SisStudent.class, SisStudent.COL_HOME_LANGUAGE_CODE);
        m_proofOfAgeCodeMap = loadRefCodeMapByAlias(FIELD_ALIAS_PERSON_PROOF_OF_AGE);
        m_referenceProgramParticipationCodeMap =
                loadRefCodeMapByField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        m_tribalBandCodeMap = loadRefCodeMapByAlias(FIELD_ALIAS_PERSON_TRIBAL_BAND);

        // Load entry codes
        String tableOid =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        if (!StringUtils.isEmpty(tableOid)) {
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, tableOid);
            m_entryCodesMap = table.getCodeMap(getBroker());
        }

        // Load withdrawal codes
        tableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        if (!StringUtils.isEmpty(tableOid)) {
            ReferenceTable table = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class, tableOid);
            m_withdrawalCodesMap = table.getCodeMap(getBroker());
        }

        m_schoolCriteria = getSchoolCriteria();

        loadStudentProgramParticipationMap();
        loadEnrollmentMaps();
        loadPreviousSchools();
        loadUsers();
        loadDiplomaTypeMap();

        loadDesignations(true);
    }

    /**
     * Method encapsulate logic for filling in grid row with appropriate information.
     *
     * @param student SisStudent
     * @param reportingSchool SisSchool
     * @param grid DataGrid
     */
    private void addStudentToGrid(SisStudent student, SisSchool reportingSchool, DataGrid grid) {
        boolean deleteRow = false;
        try {
            SisPerson person = student.getPerson();

            if (person != null) {
                String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");
                String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());
                SisSchool school = student.getSchool(getCurrentContext().getOid(), getBroker());

                grid.append();
                deleteRow = true;
                grid.set(EXPORT_FIELDS.FIELD_SCHOOL_NUMBER.getFieldId(), reportingSchool.getSchoolId());
                grid.set(EXPORT_FIELDS.FIELD_STUDENT_NUMBER.getFieldId(), student.getLocalId());
                grid.set(EXPORT_FIELDS.FIELD_HOMEROOM.getFieldId(), homeroom);
                grid.set(EXPORT_FIELDS.FIELD_PEN.getFieldId(), student.getStateId());
                grid.set(EXPORT_FIELDS.FIELD_GENDER.getFieldId(), person.getGenderCode());
                grid.set(EXPORT_FIELDS.FIELD_MAILING_ADDRESS.getFieldId(),
                        getMailingAddress(person.getResolvedMailingAddress()));
                grid.set(EXPORT_FIELDS.FIELD_PHONE_NUMBER.getFieldId(),
                        stripNonNumericPhoneCharacters(person.getPhone01()));
                grid.set(EXPORT_FIELDS.FIELD_GRADE.getFieldId(),
                        StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                grid.set(EXPORT_FIELDS.FIELD_PROJECTED_GRADUATION_YEAR.getFieldId(), String.valueOf(student.getYog()));
                grid.set(EXPORT_FIELDS.FIELD_DATE_OF_BIRTH.getFieldId(), formatDate(person.getDob()));
                grid.set(EXPORT_FIELDS.FIELD_LEGAL_SURNAME.getFieldId(), person.getLastName());
                grid.set(EXPORT_FIELDS.FIELD_LEGAL_FIRST_NAME.getFieldId(), person.getFirstName());
                grid.set(EXPORT_FIELDS.FIELD_LEGAL_MIDDLE_NAME.getFieldId(), person.getMiddleName());
                grid.set(EXPORT_FIELDS.FIELD_MEMO_NOTE.getFieldId(),
                        student.getFieldValueByAlias(FIELD_ALIAS_STUDENT_FAMILY_ALERT));
                grid.set(EXPORT_FIELDS.FIELD_HOME_LANGUAGE.getFieldId(), getHomeLanguageDescription(student));
                grid.set(EXPORT_FIELDS.FIELD_GRADUATION_PROGRAM.getFieldId(), m_diplomaTypeMap.get(student.getOid()));
                grid.set(EXPORT_FIELDS.FIELD_AM_PM.getFieldId(),
                        student.getFieldValueByAlias(FIELD_ALIAS_STUDENT_AM_OR_PM));
                grid.set(EXPORT_FIELDS.FIELD_CARE_CARD_NUMBER.getFieldId(),
                        student.getFieldValueByAlias(FIELD_ALIAS_STUDENT_CARE_CARD_NUMBER));
                grid.set(EXPORT_FIELDS.FIELD_CASE_MANAGER.getFieldId(),
                        student.getFieldValueByAlias(FIELD_ALIAS_STUDENT_CASE_MANAGER));

                /*
                 * Get homeroom teacher display
                 */
                Map<String, Staff> homeroomToStaffMap = getStaffMap(school);
                if (homeroomToStaffMap != null) {
                    Staff staff = homeroomToStaffMap.get(homeroom);
                    if (staff != null) {
                        grid.set(EXPORT_FIELDS.FIELD_TEACHER_NAME.getFieldId(), staff.getNameView());
                    }
                }

                /*
                 * Resolve usual vs. legal name
                 */
                String usualLast = (String) person.getFieldValueByAlias(FIELD_ALIAS_PERSON_SURNAME);
                String usualFirst = (String) person.getFieldValueByAlias(FIELD_ALIAS_PERSON_PREF_FIRSTNAME);
                String usualMiddle = (String) person.getFieldValueByAlias(FIELD_ALIAS_PERSON_PREF_MIDDLENAME);

                grid.set(EXPORT_FIELDS.FIELD_USUAL_SURNAME.getFieldId(),
                        StringUtils.coalesce(usualLast, person.getLastName()));
                grid.set(EXPORT_FIELDS.FIELD_USUAL_FIRST_NAME.getFieldId(),
                        StringUtils.coalesce(usualFirst, person.getFirstName()));
                grid.set(EXPORT_FIELDS.FIELD_USUAL_MIDDLE_NAME.getFieldId(),
                        StringUtils.coalesce(usualMiddle, person.getMiddleName()));

                /*
                 * Set user elements
                 */
                String userLoginName = m_userLoginByPersonOid.get(person.getOid());
                if (userLoginName != null) {
                    grid.set(EXPORT_FIELDS.FIELD_USER_NAME.getFieldId(), userLoginName);
                }

                /*
                 * Set address elements
                 */
                SisAddress address = person.getPhysicalAddress();
                if (address != null) {
                    grid.set(EXPORT_FIELDS.FIELD_APARTMENT.getFieldId(),
                            getFieldValueByAlias(address, FIELD_ALIAS_ADR_APARTMENT));
                    grid.set(EXPORT_FIELDS.FIELD_LOT_NUMBER.getFieldId(),
                            getFieldValueByAlias(address, FIELD_ALIAS_ADR_LOT_NUMBER));
                    grid.set(EXPORT_FIELDS.FIELD_CONCESSION_NUMBER.getFieldId(),
                            getFieldValueByAlias(address, FIELD_ALIAS_ADR_CONCESSION_NUMBER));
                    grid.set(EXPORT_FIELDS.FIELD_MUNICIPALITY.getFieldId(), address.getCity());
                    grid.set(EXPORT_FIELDS.FIELD_PROVINCE.getFieldId(), address.getState());
                    grid.set(EXPORT_FIELDS.FIELD_POSTAL_CODE.getFieldId(), address.getPostalCode());

                    String aptNumber = String.valueOf(StringUtils.coalesce(address.getMiscellaneous(), ""));
                    String rawStreetNumber = String.valueOf(address.getStreetNumber());
                    String streetNumber = rawStreetNumber + StringUtils.coalesce(address.getStreetLetter(), "");
                    grid.set(EXPORT_FIELDS.FIELD_STREET_NUMBER.getFieldId(),
                            "0".equals(streetNumber) ? "" : streetNumber);

                    String streetName = StringUtils.coalesce(address.getAddressLine01(), "");
                    if (!StringUtils.isEmpty(aptNumber) && streetName.startsWith(aptNumber)) {
                        if ((aptNumber + "-" + streetNumber).length() < streetName.length()) {
                            streetName = streetName.substring((aptNumber + "-" + streetNumber).length());
                        } else {
                            int indexOfSeparator = streetName.indexOf("-");
                            streetName = streetName.substring(indexOfSeparator + streetNumber.length());
                        }
                    } else if (streetName.startsWith(rawStreetNumber)) {
                        streetName = streetName.substring(rawStreetNumber.length());
                    }
                    grid.set(EXPORT_FIELDS.FIELD_STREET_NAME.getFieldId(), streetName.trim());
                }

                /*
                 * Reference lookup elements
                 */
                grid.set(EXPORT_FIELDS.FIELD_ABORIGINAL_ANCESTRY.getFieldId(),
                        getFieldValueByAlias(student, FIELD_ALIAS_PERSON_INDIAN_ANCESTRY));
                grid.set(EXPORT_FIELDS.FIELD_DIA.getFieldId(),
                        getFieldValueByAlias(student, FIELD_ALIAS_STUDENT_DIA_NUMBER));
                grid.set(EXPORT_FIELDS.FIELD_IMMIGRATION_STATUS.getFieldId(), getRefDescriptionByAlias(person,
                        FIELD_ALIAS_STUDENT_CITIZENSHIP_STATUS, m_citizenshipStatusCodeMap, true));
                grid.set(EXPORT_FIELDS.FIELD_CITIZENSHIP.getFieldId(), getRefDescriptionByAlias(person,
                        FIELD_ALIAS_STUDENT_CITIZENSHIP_COUNTRY, m_citizenshipCountryCodeMap, true));
                grid.set(EXPORT_FIELDS.FIELD_COUNTRY_OF_BIRTH.getFieldId(), getRefDescriptionByAlias(person,
                        FIELD_ALIAS_STUDENT_COUNTRY_OF_BIRTH, m_countryOfBirthCodeMap, true));
                grid.set(EXPORT_FIELDS.FIELD_COUNSELOR_ID.getFieldId(),
                        getRefLocalCodeByAlias(student, FIELD_ALIAS_STUDENT_COUNSELOR, m_counselorCodeMap));
                grid.set(EXPORT_FIELDS.FIELD_COUNSELOR_NAME.getFieldId(),
                        getRefDescriptionByAlias(student, FIELD_ALIAS_STUDENT_COUNSELOR, m_counselorCodeMap, false));
                grid.set(EXPORT_FIELDS.FIELD_PROOF_OF_AGE.getFieldId(),
                        getRefDescriptionByAlias(student, FIELD_ALIAS_PERSON_PROOF_OF_AGE, m_proofOfAgeCodeMap, true));
                grid.set(EXPORT_FIELDS.FIELD_FIRST_NATIONS_BAND.getFieldId(),
                        getRefDescriptionByAlias(person, FIELD_ALIAS_PERSON_TRIBAL_BAND, m_tribalBandCodeMap, true));

                String disabilityCode = "";
                String designation = m_studentDesignationsMap.containsKey(student.getOid())
                        ? getFieldValueByAlias(student, ALIAS_STUDENT_DESIGNATION)
                        : null;
                if (!StringUtils.isEmpty(designation) && m_designationCodes.containsKey(designation)) {
                    ReferenceCode disabilityRef = m_designationCodes.get(designation);
                    disabilityCode = disabilityRef.getStateCode();
                }
                grid.set(EXPORT_FIELDS.FIELD_SPECIAL_EDUCATION_CATEGORY.getFieldId(), disabilityCode);

                /*
                 * Comparison elements
                 */
                String familyCourier = (String) student.getFieldValueByAlias(FIELD_ALIAS_STUDENT_FAMILY_COURIER);
                grid.set(EXPORT_FIELDS.FIELD_FAMILY_COURIER_INDICATOR.getFieldId(), getYesNoFlag(familyCourier));

                /*
                 * Enrollment-based elements
                 */
                StudentEnrollment entry = m_entryMap.get(student.getOid());
                if (entry != null) {
                    String admissionCode = entry.getEnrollmentCode();
                    grid.set(EXPORT_FIELDS.FIELD_ADMISSION_REASON.getFieldId(),
                            getRefDescriptionByCode(admissionCode, m_entryCodesMap, true));
                    grid.set(EXPORT_FIELDS.FIELD_ADMISSION_DATE.getFieldId(), formatDate(entry.getEnrollmentDate()));
                }

                StudentEnrollment withdrawal = m_withdrawalMap.get(student.getOid());
                if (withdrawal != null) {
                    String withdrawCode = withdrawal.getEnrollmentCode();
                    grid.set(EXPORT_FIELDS.FIELD_WITHDRAW_REASON.getFieldId(),
                            getRefDescriptionByCode(withdrawCode, m_withdrawalCodesMap, true));
                    grid.set(EXPORT_FIELDS.FIELD_WITHDRAW_DATE.getFieldId(),
                            formatDate(withdrawal.getEnrollmentDate()));

                    String enrollmentStatusCode = withdrawal.getStatusCode();
                    if (m_graduatedStatusList != null && m_graduatedStatusList.contains(enrollmentStatusCode)) {
                        grid.set(EXPORT_FIELDS.FIELD_DATE_GRADUATED.getFieldId(),
                                formatDate(withdrawal.getEnrollmentDate()));
                    }
                }

                /*
                 * Home school elements
                 */
                grid.set(EXPORT_FIELDS.FIELD_HOME_SCHOOL_NUMBER.getFieldId(), school.getSchoolId());
                grid.set(EXPORT_FIELDS.FIELD_HOME_SCHOOL_NAME.getFieldId(), school.getName());

                /*
                 * Previous school elements
                 */
                SisSchool previousSchool = m_previousSchoolMap.get(student.getOid());
                if (previousSchool != null) {
                    grid.set(EXPORT_FIELDS.FIELD_PREVIOUS_SCHOOL_NUMBER.getFieldId(), previousSchool.getSchoolId());
                    grid.set(EXPORT_FIELDS.FIELD_PREVIOUS_SCHOOL_NAME.getFieldId(), previousSchool.getName());
                }

                setProgramFields(grid, student);
            }
        } catch (NullPointerException npe) {
            StringBuilder strBldr = new StringBuilder();
            strBldr.append("Unable to export ");
            strBldr.append(SisStudent.class.getName());
            strBldr.append(" with OID: [");
            strBldr.append(student.getOid());
            strBldr.append("] for the Student with Local ID: [");
            strBldr.append(student.getLocalId());
            strBldr.append("].");

            // deleteRow is true if an incomplete row has been added to the grid from grid.append()
            if (!deleteRow) {
                strBldr.append("Null encountered before adding to export.");
            } else {
                strBldr.append("Null encountered when setting Columns.");
                grid.deleteRow(); // Delete the incomplete row that was appended to the grid.
            }

            strBldr.append("\n\n\nNullPointerException: \n");
            strBldr.append(ExceptionUtils.getStackTrace(npe));
            logToolMessage(Level.WARNING, strBldr.toString(), false);
        }
    }

    /**
     * Build the criteria for students included in the export.
     *
     * @return Criteria
     */
    private Criteria buildPrimaryCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                m_schoolCriteria.copyWithAdjustedPath(m_helper.getSchoolRelationship(), m_helper.getSchoolOidField()));

        return criteria;
    }

    /**
     * Build the criteria for secondary school students included in the export. This only looks at
     * secondary
     * associations in the current school year.
     *
     * @return Criteria
     */
    private Criteria buildSecondaryCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                m_schoolCriteria.copyWithAdjustedPath(StudentSchool.REL_SCHOOL, StudentSchool.COL_SCHOOL_OID));
        criteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

        return criteria;
    }

    /**
     * Formats the date as a String. If the date is null, empty string is returned
     *
     * @param date PlainDate
     * @return String
     */
    private String formatDate(PlainDate date) {
        String dateAsString = "";

        if (date != null) {
            dateAsString = m_dateFormatter.format(date);
        }

        return dateAsString.toUpperCase();
    }

    /**
     * Returns the data dcitionary instance.
     *
     * @return DataDictionary
     */
    private DataDictionary getDistrictDictionary() {
        return m_dictionary;
    }

    /**
     * Get field value by alias (with null pointer validation).
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return String
     */
    private String getFieldValueByAlias(X2BaseBean bean, String alias) {
        String value = null;

        if (bean != null) {
            value = (String) bean.getFieldValueByAlias(alias);
        }

        return value;
    }

    /**
     * Get home language code description. If no reference code exists, the language code is
     * returned.
     *
     * @param student SisStudent
     * @return String
     */
    private String getHomeLanguageDescription(SisStudent student) {
        String languageDescription = student.getHomeLanguageCode();

        ReferenceCode homeLangRefCode = m_homeLangCodeMap.get(languageDescription);
        if (homeLangRefCode != null) {
            languageDescription = homeLangRefCode.getDescription();
        }

        return languageDescription;
    }

    /**
     * Get concatenated mailing address.
     *
     * @param mailingAddress SisAddress
     * @return String
     */
    private String getMailingAddress(SisAddress mailingAddress) {
        String concatenatedMailingAddress = "";

        if (mailingAddress != null) {
            if (!StringUtils.isEmpty(mailingAddress.getAddressLine01())) {
                concatenatedMailingAddress += mailingAddress.getAddressLine01() + " ";
            }

            if (!StringUtils.isEmpty(mailingAddress.getAddressLine02())) {
                concatenatedMailingAddress += mailingAddress.getAddressLine02() + " ";
            }

            if (!StringUtils.isEmpty(mailingAddress.getAddressLine03())) {
                concatenatedMailingAddress += mailingAddress.getAddressLine03();
            }
        }

        return concatenatedMailingAddress;
    }

    /**
     * Return code description from reference map.
     *
     * @param code String
     * @param refCodeMap Map<String,ReferenceCode>
     * @param allowCode boolean
     * @return String
     */
    private String getRefDescriptionByCode(String code, Map<String, ReferenceCode> refCodeMap, boolean allowCode) {
        String refCodeDescription = allowCode ? code : null;

        if (refCodeMap != null && refCodeMap.containsKey(code)) {
            ReferenceCode refCode = refCodeMap.get(code);
            refCodeDescription = refCode.getDescription();
        }

        return refCodeDescription;
    }

    /**
     * Get ref. code description for fields retrieved by alias
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param refCodeMap Map<String,ReferenceCode>
     * @param allowCode boolean
     * @return String
     */
    private String getRefDescriptionByAlias(X2BaseBean bean,
                                            String alias,
                                            Map<String, ReferenceCode> refCodeMap,
                                            boolean allowCode) {
        String refCodeDescription = null;
        if (bean != null && refCodeMap != null) {
            String code = (String) bean.getFieldValueByAlias(alias);
            refCodeDescription = getRefDescriptionByCode(code, refCodeMap, allowCode);
        }

        return refCodeDescription;
    }

    /**
     * Get ref. local code for fields retrieved by alias
     *
     * @param bean X2BaseBean
     * @param alias String
     * @param refCodeMap Map<String,ReferenceCode>
     * @return String
     */
    private String getRefLocalCodeByAlias(X2BaseBean bean, String alias, Map<String, ReferenceCode> refCodeMap) {
        String refLocalCode = null;
        if (bean != null && refCodeMap != null) {
            String code = (String) bean.getFieldValueByAlias(alias);
            refLocalCode = getRefLocalCodeByCode(code, refCodeMap);
        }

        return refLocalCode;
    }

    /**
     * Get ref. local code for fields retrieved by alias
     *
     * @param code String
     * @param refCodeMap Map<String,ReferenceCode>
     * @return String
     */
    private String getRefLocalCodeByCode(String code, Map<String, ReferenceCode> refCodeMap) {
        String localCode = null;

        if (refCodeMap != null && refCodeMap.containsKey(code)) {
            ReferenceCode refCode = refCodeMap.get(code);
            localCode = refCode.getLocalCode();
        }

        return localCode;
    }

    /**
     * Returns a subquery to get student OIDs that are secondary students in the current year for
     * the reported schools.
     *
     * @return Sub query
     */
    private SubQuery getSecondaryStudentSubquery() {
        return new SubQuery(StudentSchool.class, StudentSchool.COL_STUDENT_OID, buildSecondaryCriteria());
    }

    /**
     * Returns the homeroom-to-staff map for the provided school. If the school hasn't been looked
     * up yet, build
     * the map and track for repeated lookups.
     *
     * @param school SisSchool
     * @return Map<String, Staff>
     */
    private Map<String, Staff> getStaffMap(SisSchool school) {
        Map<String, Staff> homeroomToStaffMap = m_schoolHomeroomToStaffMap.get(school.getOid());
        if (homeroomToStaffMap == null) {
            homeroomToStaffMap = ReportUtils.buildHomeroomToStaffMap(getBroker(), getOrganization(), school);
            m_schoolHomeroomToStaffMap.put(school.getOid(), homeroomToStaffMap);
        }

        return homeroomToStaffMap;
    }

    /**
     * Returns Y or N for the Yes/No field. This supports fields that are logical (0/1), Yes/No
     * (ingoring case), and
     * Y/N (ignoring case).
     *
     * @param value String
     * @return String
     */
    private String getYesNoFlag(String value) {
        boolean check = "1".equals(value) || "yes".equalsIgnoreCase(value) || "y".equalsIgnoreCase(value);

        return check ? "Y" : "N";
    }

    /**
     * Loads the disability code map.
     *
     * @param includeGifted boolean
     */
    public void loadDesignations(boolean includeGifted) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField studentDesigField = dictionary.findDataDictionaryFieldByAlias(ALIAS_STUDENT_DESIGNATION);
        if (studentDesigField != null) {
            X2Criteria studentCriteria = new X2Criteria();
            studentCriteria.addNotEmpty(studentDesigField.getJavaName(), getBroker().getPersistenceKey());

            if (!includeGifted) {
                studentCriteria.addNotEqualTo(studentDesigField.getJavaName(), GIFTED_STUDENT_DISABILITY_CODE);
            }

            /*
             * Load the designation for primary students.
             */
            X2Criteria primaryCriteria = new X2Criteria();
            primaryCriteria.addAndCriteria(
                    m_schoolCriteria.copyWithAdjustedPath(SisStudent.REL_SCHOOL, SisStudent.COL_SCHOOL_OID));
            primaryCriteria.addNotNull(SisStudent.COL_SCHOOL_OID);
            primaryCriteria.addAndCriteria(studentCriteria);

            QueryByCriteria primaryQuery = new QueryByCriteria(SisStudent.class, primaryCriteria);
            m_studentDesignationsMap = getBroker().getMapByQuery(primaryQuery, X2BaseBean.COL_OID, 1000);

            /*
             * Load the designation for secondary students.
             */
            X2Criteria secondaryCriteria = new X2Criteria();
            secondaryCriteria.addAndCriteria(
                    m_schoolCriteria.copyWithAdjustedPath(SisStudent.REL_SCHOOL, SisStudent.COL_SCHOOL_OID));
            secondaryCriteria.addNotNull(SisStudent.COL_SCHOOL_OID);

            secondaryCriteria.addEqualTo(
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            secondaryCriteria.addEqualTo(SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_TYPE,
                    Integer.valueOf(StudentSchool.SECONDARY));

            QueryByCriteria secondaryQuery = new QueryByCriteria(SisStudent.class, secondaryCriteria);
            m_studentDesignationsMap.putAll(getBroker().getMapByQuery(secondaryQuery, X2BaseBean.COL_OID, 1000));
        }
    }

    /**
     * Loads the primary diploma type for the students into a map keyed to the student OID.
     */
    private void loadDiplomaTypeMap() {
        m_diplomaTypeMap = new HashMap<String, String>(2048);

        String[] columns = new String[] {GraduationStudentProgram.COL_STUDENT_OID,
                GraduationStudentProgram.REL_PROGRAM_STUDIES + PATH_DELIMITER +
                        GraduationProgram.COL_DIPLOMA_TYPE};

        // Query for student programs for primary and secondary students
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(GraduationStudentProgram.COL_PRIMARY_INDICATOR, Boolean.TRUE);
        criteria.addNotNull(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);

        if (getSchool() != null) {
            X2Criteria schoolCriteria = new X2Criteria();

            X2Criteria primarySchoolCriteria = new X2Criteria();
            primarySchoolCriteria.addEqualTo(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            X2Criteria secondarySchoolCriteria = new X2Criteria();
            secondarySchoolCriteria.addEqualTo(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER +
                    StudentSchool.COL_SCHOOL_OID, getSchool().getOid());
            secondarySchoolCriteria.addEqualTo(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER +
                    StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            secondarySchoolCriteria.addEqualTo(GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER +
                    StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

            X2Criteria dummyUnionCriteria = new X2Criteria();
            dummyUnionCriteria.addEqualTo(UnionAdjuster.UNION_OPERATOR_ADJUSTER_KEYWORD,
                    UnionAdjuster.UNION_KEYWORD_VALUE);
            dummyUnionCriteria.addOrCriteria(secondarySchoolCriteria);

            primarySchoolCriteria.addOrCriteria(dummyUnionCriteria);
            schoolCriteria.addAndCriteria(primarySchoolCriteria);
        } else {
            Criteria secondaryStudentCriteria = new Criteria();
            secondaryStudentCriteria.addIn(GraduationStudentProgram.COL_STUDENT_OID, getSecondaryStudentSubquery());

            Criteria studentCriteria = new Criteria();
            studentCriteria.addAndCriteria(m_schoolCriteria.copyWithAdjustedPath(
                    GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL,
                    GraduationStudentProgram.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID));
            studentCriteria.addOrCriteria(secondaryStudentCriteria);
            criteria.addAndCriteria(studentCriteria);
        }

        ColumnQuery query = new ColumnQuery(GraduationStudentProgram.class, columns, criteria);
        if (UnionAdjuster.isContainsUnion(criteria)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryTable table =
                    dictionary.findDataDictionaryTableByClass(GraduationStudentProgram.class.getName());
            UnionAdjuster unionAdjuster = new UnionAdjuster(table.getPrimaryKeyColumn());
            query.setQueryAdjuster(unionAdjuster);
        }

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String stdOid = (String) row[0];
                String diplomaType = (String) row[1];

                m_diplomaTypeMap.put(stdOid, diplomaType);
            }
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }
    }

    /**
     * Loads enrollment information (entry and withdrawals).
     */
    private void loadEnrollmentMaps() {
        m_entryMap = new HashMap<String, StudentEnrollment>(2048);
        m_withdrawalMap = new HashMap<String, StudentEnrollment>(2048);

        X2Criteria criteria = new X2Criteria();
        if (getSchool() != null) {
            String currentSchoolOid = getSchool().getOid();
            X2Criteria primarySchoolEnrollments = new X2Criteria();
            primarySchoolEnrollments.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE,
                    Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));
            primarySchoolEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_SCHOOL_OID, currentSchoolOid);
            primarySchoolEnrollments.addEqualToField(StudentEnrollment.COL_SCHOOL_OID,
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);

            X2Criteria secondarySchoolEnrollments = new X2Criteria();
            secondarySchoolEnrollments.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE,
                    Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));
            secondarySchoolEnrollments.addEqualToField(StudentEnrollment.COL_SCHOOL_OID,
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);
            secondarySchoolEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_SCHOOL_OID,
                    currentSchoolOid);
            secondarySchoolEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            secondarySchoolEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_TYPE,
                    Integer.valueOf(StudentSchool.SECONDARY));

            X2Criteria dummyUnionCriteria = new X2Criteria();
            dummyUnionCriteria.addEqualTo(UnionAdjuster.UNION_OPERATOR_ADJUSTER_KEYWORD,
                    UnionAdjuster.UNION_KEYWORD_VALUE);
            dummyUnionCriteria.addOrCriteria(secondarySchoolEnrollments);

            primarySchoolEnrollments.addOrCriteria(dummyUnionCriteria);
            criteria.addAndCriteria(primarySchoolEnrollments);
        } else {
            criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE,
                    Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.WITHDRAWAL));
            criteria.addEqualToField(StudentEnrollment.COL_SCHOOL_OID,
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);

            Criteria secondaryStudentCriteria = new Criteria();
            secondaryStudentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, getSecondaryStudentSubquery());

            Criteria schoolCriteria = new Criteria();
            schoolCriteria.addAndCriteria(
                    m_schoolCriteria.copyWithAdjustedPath(StudentEnrollment.REL_SCHOOL,
                            StudentEnrollment.COL_SCHOOL_OID));
            schoolCriteria.addOrCriteria(secondaryStudentCriteria);
            criteria.addAndCriteria(schoolCriteria);
        }

        BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
        query.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);

        if (UnionAdjuster.isContainsUnion(criteria)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(StudentEnrollment.class.getName());
            UnionAdjuster unionAdjuster = new UnionAdjuster(table.getPrimaryKeyColumn());
            query.setQueryAdjuster(unionAdjuster);
        }

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            boolean hasEntry = false;

            while (iterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) iterator.next();
                SisStudent student = enrollment.getStudent();

                if (!ObjectUtils.match(student, lastStudent)) {
                    hasEntry = false;
                }

                if (!hasEntry) {
                    // Only want the withdrawal if there is no more recent entry
                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                        m_entryMap.put(student.getOid(), enrollment);
                        hasEntry = true;
                    } else {
                        m_withdrawalMap.put(student.getOid(), enrollment);
                    }
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Load the previous school for each student based on their most recent enrollment record that
     * is not their
     * current school.
     */
    private void loadPreviousSchools() {
        m_previousSchoolMap = new HashMap<String, SisSchool>(4096);

        X2Criteria criteria = new X2Criteria();
        if (getSchool() != null) {
            String currentSchoolOid = getSchool().getOid();
            X2Criteria primaryEnrollments = new X2Criteria();
            primaryEnrollments.addNotEqualToField(StudentEnrollment.COL_SCHOOL_OID,
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);
            primaryEnrollments.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
            primaryEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_SCHOOL_OID, currentSchoolOid);

            X2Criteria secondarySchoolEnrollments = new X2Criteria();
            secondarySchoolEnrollments.addNotEqualToField(StudentEnrollment.COL_SCHOOL_OID,
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);
            secondarySchoolEnrollments.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);

            secondarySchoolEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_SCHOOL_OID,
                    currentSchoolOid);
            secondarySchoolEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_DISTRICT_CONTEXT_OID,
                    getCurrentContext().getOid());
            secondarySchoolEnrollments.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER + StudentSchool.COL_TYPE,
                    Integer.valueOf(StudentSchool.SECONDARY));

            X2Criteria dummyUnionCriteria = new X2Criteria();
            dummyUnionCriteria.addEqualTo(UnionAdjuster.UNION_OPERATOR_ADJUSTER_KEYWORD,
                    UnionAdjuster.UNION_KEYWORD_VALUE);
            dummyUnionCriteria.addOrCriteria(secondarySchoolEnrollments);

            primaryEnrollments.addOrCriteria(dummyUnionCriteria);
            criteria.addAndCriteria(primaryEnrollments);
        } else {
            criteria.addNotEqualToField(StudentEnrollment.COL_SCHOOL_OID,
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID);
            criteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);

            Criteria secondaryStudentCriteria = new Criteria();
            secondaryStudentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, getSecondaryStudentSubquery());

            Criteria studentCriteria = new Criteria();
            studentCriteria.addAndCriteria(m_schoolCriteria.copyWithAdjustedPath(
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL,
                    StudentEnrollment.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID));
            studentCriteria.addOrCriteria(secondaryStudentCriteria);
            criteria.addAndCriteria(studentCriteria);
        }

        BeanQuery query = new BeanQuery(StudentEnrollment.class, criteria);
        query.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);

        if (UnionAdjuster.isContainsUnion(criteria)) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByClass(StudentEnrollment.class.getName());
            UnionAdjuster unionAdjuster = new UnionAdjuster(table.getPrimaryKeyColumn());
            query.setQueryAdjuster(unionAdjuster);
        }

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            SisStudent lastStudent = null;
            while (iterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) iterator.next();
                SisStudent student = enrollment.getStudent();
                SisSchool school = enrollment.getSchool();

                if (!ObjectUtils.match(student, lastStudent)) {
                    m_previousSchoolMap.put(student.getOid(), school);
                }

                lastStudent = student;
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Load reference code map by alias.
     *
     * @param alias String
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> loadRefCodeMapByAlias(String alias) {
        Map<String, ReferenceCode> refCodeMap = null;

        DataDictionaryField field = getDistrictDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                refCodeMap = referenceTable.getCodeMap(getBroker());
            }
        }
        return refCodeMap;
    }

    /**
     * Load reference code map by field name.
     *
     * @param beanClass Class
     * @param fieldName String
     * @return Map<String, ReferenceCode>
     */
    private Map<String, ReferenceCode> loadRefCodeMapByField(Class beanClass, String fieldName) {
        Map<String, ReferenceCode> refCodeMap = null;

        ModelProperty prop = new ModelProperty(beanClass, fieldName, getBroker().getPersistenceKey());
        DataDictionaryField field = getDistrictDictionary().findDataDictionaryField(prop.getFieldId());

        if (field != null) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                refCodeMap = referenceTable.getCodeMap(getBroker());
            }
        }

        return refCodeMap;
    }

    /**
     * Loads the reference code.
     *
     * @param broker X2Broker
     * @param tblOid String
     * @param columnJavaName String
     * @return Map
     */
    private Map loadRefCodes(X2Broker broker, String tblOid, String columnJavaName) {
        Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();

        X2Criteria extendedProgramFieldCriteria = new X2Criteria();
        extendedProgramFieldCriteria.addEqualTo(ExtendedDataField.COL_ALIAS, ALIAS_STD_PGM_PROGRAM_CODE_ALIAS);
        extendedProgramFieldCriteria.addEqualTo(ExtendedDataField.REL_DATA_FIELD_CONFIG + PATH_DELIMITER +
                DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_DATA_TABLE_OID, tblOid);
        extendedProgramFieldCriteria.addEqualTo(ExtendedDataField.REL_DATA_FIELD_CONFIG + PATH_DELIMITER +
                DataFieldConfig.REL_DATA_FIELD + PATH_DELIMITER + DataField.COL_JAVA_NAME, columnJavaName);

        QueryByCriteria extendedProgramFieldQuery =
                new QueryByCriteria(ExtendedDataField.class, extendedProgramFieldCriteria);

        ExtendedDataField pgmCodeExtended = (ExtendedDataField) broker.getBeanByQuery(extendedProgramFieldQuery);
        if (pgmCodeExtended != null && !StringUtils.isEmpty(pgmCodeExtended.getReferenceTableOid())) {
            X2Criteria refCodeCriteria = ReferenceManager.getCodesCriteria(pgmCodeExtended.getReferenceTableOid(),
                    getOrganization(),
                    getBroker().getPersistenceKey());

            QueryByCriteria refCodeQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
            refCodes = broker.getMapByQuery(refCodeQuery, ReferenceCode.COL_CODE, 100);
        } else {
            AppGlobals.getLog().log(Level.SEVERE, "Reference table not found for designation code");
        }

        return refCodes;
    }

    /**
     * Builds the query needed for the selected school(s).
     *
     * @return Collection<String>
     */
    @Override
    protected X2Criteria getSchoolCriteria() {
        X2Criteria criteria = new X2Criteria();
        if (getSchool() != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            String oids = (String) getParameter(PARAM_SCHOOL_OIDS);
            if (!StringUtils.isEmpty(oids)) {
                Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);

                Selection selection = X2BaseBean.newInstance(Selection.class, getBroker().getPersistenceKey());
                for (String oid : oidList) {
                    SelectionObject object =
                            X2BaseBean.newInstance(SelectionObject.class, getBroker().getPersistenceKey());
                    object.setObjectOid(oid);
                    selection.addToSelectionObjects(object);
                }
                getBroker().saveBean(selection);

                getTemporaryBeans().add(selection);
                getTemporaryBeans().addAll(selection.getSelectionObjects());

                Criteria subCriteria = new Criteria();
                subCriteria.addEqualTo(SelectionObject.COL_SELECTION_OID, selection.getOid());
                subCriteria.addEqualToField(SelectionObject.COL_OBJECT_OID, Criteria.PARENT_QUERY_PREFIX
                        + X2BaseBean.COL_OID);

                criteria.addExists(new SubQuery(SelectionObject.class, X2BaseBean.COL_OID, subCriteria));
            } else {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            }
        }

        return criteria;
    }

    /**
     * Load student program participation map.
     */
    private void loadStudentProgramParticipationMap() {
        m_programCodesMap = new HashMap<String, Collection<String>>();
        X2Criteria criteria = new X2Criteria();

        Criteria schoolCriteria = new Criteria();
        schoolCriteria.addAndCriteria(m_schoolCriteria.copyWithAdjustedPath(
                StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship(),
                StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolOidField()));

        if (getSchool() != null) {
            X2Criteria secondarySchoolCriteria = new X2Criteria();
            secondarySchoolCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT +
                    PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER +
                    StudentSchool.COL_SCHOOL_OID, getSchool().getOid());
            secondarySchoolCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT +
                    PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER +
                    StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
            secondarySchoolCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT +
                    PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS + PATH_DELIMITER +
                    StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

            X2Criteria dummyUnionCriteria = new X2Criteria();
            dummyUnionCriteria.addEqualTo(UnionAdjuster.UNION_OPERATOR_ADJUSTER_KEYWORD,
                    UnionAdjuster.UNION_KEYWORD_VALUE);
            dummyUnionCriteria.addOrCriteria(secondarySchoolCriteria);

            schoolCriteria.addOrCriteria(dummyUnionCriteria);
        }

        criteria.addAndCriteria(schoolCriteria);

        X2Criteria additionalCriteria = new X2Criteria();
        additionalCriteria.addNotNull(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER
                + SisStudent.COL_SCHOOL_OID);

        /*
         * Start date criteria
         */
        X2Criteria emptyStartDateCriteria = new X2Criteria();
        emptyStartDateCriteria.addIsNull(StudentProgramParticipation.COL_START_DATE);

        X2Criteria startDateCriteria = new X2Criteria();
        startDateCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getEndDate());
        startDateCriteria.addOrCriteria(emptyStartDateCriteria);

        /*
         * End date criteria
         */
        X2Criteria emptyEndDateCriteria = new X2Criteria();
        emptyEndDateCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, new PlainDate());
        endDateCriteria.addOrCriteria(emptyEndDateCriteria);

        additionalCriteria.addAndCriteria(startDateCriteria);
        additionalCriteria.addAndCriteria(endDateCriteria);

        boolean containsUnion = UnionAdjuster.isContainsUnion(criteria);
        if (containsUnion) {
            UnionAdjuster.addUnionWrapper(criteria, additionalCriteria);
        } else {
            criteria.addAndCriteria(additionalCriteria);
        }

        /*
         * Build and execute query
         */
        String[] columns = new String[] {StudentProgramParticipation.COL_STUDENT_OID,
                StudentProgramParticipation.COL_PROGRAM_CODE};
        ColumnQuery programQuery = new ColumnQuery(StudentProgramParticipation.class, columns, criteria);

        programQuery.addOrderByAscending(StudentProgramParticipation.COL_STUDENT_OID);
        programQuery.addOrderByAscending(StudentProgramParticipation.COL_PROGRAM_CODE);

        if (containsUnion) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryTable table =
                    dictionary.findDataDictionaryTableByClass(StudentProgramParticipation.class.getName());
            UnionAdjuster unionAdjuster = new UnionAdjuster(table.getPrimaryKeyColumn());
            programQuery.setQueryAdjuster(unionAdjuster);
        }

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(programQuery)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                String programCode = (String) row[1];

                Collection<String> codesForStudent = m_programCodesMap.get(studentOid);
                if (codesForStudent == null) {
                    codesForStudent = new ArrayList<String>();
                    m_programCodesMap.put(studentOid, codesForStudent);
                }
                codesForStudent.add(programCode);
            }
        }
    }

    /**
     * Loads a map of the user login names that are flagged as students into a map keyed to the
     * person OID.
     */
    private void loadUsers() {
        m_userLoginByPersonOid = new HashMap<String, String>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisUser.REL_PERSON + PATH_DELIMITER + SisPerson.COL_STUDENT_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(SisUser.COL_LOGIN_STATUS,
                Integer.valueOf(User.LoginStatus.DISABLED_AND_LOCKED.ordinal()));

        String[] columns = new String[] {SisUser.COL_PERSON_OID, SisUser.COL_LOGIN_NAME};
        ReportQueryByCriteria query = new ReportQueryByCriteria(SisUser.class, columns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                m_userLoginByPersonOid.put((String) row[0], (String) row[1]);
            }
        }
    }

    /**
     * Check the program records for the student and see if they are involved in an ESL or french
     * immerision program.
     *
     * @param grid DataGrid
     * @param student SisStudent
     */
    private void setProgramFields(DataGrid grid, SisStudent student) {
        boolean esl = false;
        boolean french = false;

        Collection<String> programsCodes = m_programCodesMap.get(student.getOid());
        if (!CollectionUtils.isEmpty(programsCodes)) {
            for (String programCode : programsCodes) {
                ReferenceCode referenceCode = m_referenceProgramParticipationCodeMap.get(programCode);

                if (referenceCode != null) {
                    if (PROGRAM_CODE_ESL.equalsIgnoreCase(referenceCode.getStateCode())) {
                        esl = true;
                    } else if (PROGRAM_CODE_EFI.equalsIgnoreCase(referenceCode.getStateCode())) {
                        french = true;
                    } else if (PROGRAM_CODE_LFI.equalsIgnoreCase(referenceCode.getStateCode())) {
                        french = true;
                    }
                }
            }
        }

        grid.set(EXPORT_FIELDS.FIELD_ESL.getFieldId(), esl ? "Y" : "N");
        grid.set(EXPORT_FIELDS.FIELD_FRENCH_IMMERSION.getFieldId(), french ? "Y" : "N");
    }

    /**
     * Returns the passed phone value stripped of any non-numeric characters.
     *
     * @param phoneValue String
     * @return String
     */
    private String stripNonNumericPhoneCharacters(String phoneValue) {
        String cleanValue = "";

        if (phoneValue != null) {
            Matcher matcher = m_illegalPhoneCharacters.matcher(phoneValue);
            cleanValue = matcher.replaceAll("");
        }

        return cleanValue;
    }
}
