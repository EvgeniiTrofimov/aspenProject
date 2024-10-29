/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for MEPID Staff request export.
 * This class implements the data export for Mass MEPID staff request export.
 *
 * @author X2 Development Corporation
 */
public class Mepid extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the MEPID Staff request export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class MEPIDEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public MEPIDEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStaff staff = (SisStaff) getBean();

            String name = staff.getNameView() +
                    " [ID: " + staff.getLocalId() +
                    (getData().isSchoolContext() ? ", SCHOOL: " + staff.getSchool().getName() : "") +
                    "]";
            return name;
        }
    }

    /*
     * MEPID field alias constants.
     */
    protected static final String ID_00_MEPID = "ID00";
    protected static final String ID_01_FIRST_NAME = "ID01";
    protected static final String ID_02_MIDDLE_NAME = "ID02";
    protected static final String ID_03_LAST_NAME = "ID03";
    protected static final String ID_04_BIRTH_DATE = "ID04";
    protected static final String ID_05_GENDER = "ID05";
    protected static final String ID_06_LICENSE = "ID06";
    protected static final String ID_07_LOCALID = "ID07";

    private static final String EOL = System.getProperty("line.separator");
    private static final String DOE_DISTRICT_ID = "DOE District ID";
    private static final String EPIMS_STATUS_FIELD = "SR09";
    private static final String EXPORT_TYPE_PARAM = "exportType";
    private static final int TYPE_ASSIGN = 0;
    private static final int TYPE_LOOKUP = 1;
    private static final int TYPE_VERIFICATION = 2;

    /*
     * Other internal constants
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    protected static final String DATE_FORMAT = "MM/dd/yyyy";
    protected static final String NO_MIDDLE_NAME = "NMN";
    protected static final String REGEX_ALPHANUMERIC = "[A-Za-z0123456789]*";
    protected static final String REGEX_ALPHANUMERIC_HYPHEN = "[-A-Za-z0-9]*";
    protected static final String REGEX_NAME = "[-'. A-Za-z0123456789]*";
    protected static final String REGEX_MNAME = "[-' A-Za-z0123456789]*";
    protected static final String REGEX_NUMERIC = "[0123456789]*";

    /*
     * Instance variables.
     */
    protected DateFormat m_dateFormat;
    protected int m_exportType = -1;
    protected Pattern m_illegalNameCharacters;
    protected Map<String, Collection<StaffCertification>> m_licensesMap;
    protected Date m_reportDateMax;

    /**
     * Returns the primary license number for the given staff member.
     *
     */
    protected class RetrieveLicense implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            SisStaff staff = (SisStaff) entity.getBean();
            String licenseNumber = null;

            Collection<StaffCertification> licenses = m_licensesMap.get(staff.getOid());
            if (!CollectionUtils.isEmpty(licenses)) {
                licenseNumber = licenses.iterator().next().getCertificationNumber();
                if (licenses.size() > 1) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "Multiple licenses ID06 marked as primary",
                            "Using: " + STYLE_BOLD + licenseNumber + STYLE_END);
                    entity.addRetrievalError(field.getFieldId(), error);
                }
            }
            return licenseNumber;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * Validates the birthdate format and age.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateBirthdate implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            // Check format.
            Date bday = null;
            if (!StringUtils.isEmpty(value)) {
                try {
                    bday = m_dateFormat.parse(value);
                } catch (ParseException e) {
                    // Fails to parse. Error.
                    errors.add(new StateReportValidationError(entity, field,
                            "Date of birth ID04 format error", "ID04=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // Check age
            if (bday != null && m_reportDateMax.before(bday)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Reporting age ID04 must be 16 years or older", "ID04=" + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Gets the bean class.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getBeanClass()
     */
    @Override
    public Class getBeanClass() {
        return SisStaff.class;
    }

    /**
     * Gets the export title.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "MEPID Export";
    }

    /**
     * Returns heading text to include at the top of the export file.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        StringBuilder header = new StringBuilder(64);

        header.append("EPIMS,MEPID_");

        switch (m_exportType) {
            case TYPE_ASSIGN:
                header.append("ASSIGN");
                break;

            case TYPE_LOOKUP:
                header.append("LOOKUP");
                break;

            case TYPE_VERIFICATION:
                header.append("VERIFICATION");
                break;
        }

        header.append(",");

        String code = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_ID);
        if (StringUtils.isEmpty(code)) {
            code = "[INSERT DISTRICT ID HERE]";
            addSetupError("Using a placeholder for the district ID.", "Set the '" + DOE_DISTRICT_ID
                    + "' alias in the Data Dictionary and update that field with the correct ID.");
        }

        header.append(code);
        header.append(EOL);

        return header.toString();
    }

    /**
     * Turn off header row in export file.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Turn off value wrappers in the export file.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getUseValueWrappers()
     */
    @Override
    public boolean getUseValueWrappers() {
        return false;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Job parameters.
         */
        m_exportType = ((Integer) getParameter(EXPORT_TYPE_PARAM)).intValue();

        /*
         * support variables.
         */
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        Calendar cal = Calendar.getInstance(); // current date.
        cal.add(Calendar.YEAR, -16);
        m_reportDateMax = cal.getTime(); // 16 years old (> 16), min for employment reporting.

        /*
         * Define all export fields.
         */
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(26);
        if (m_exportType == TYPE_VERIFICATION) {
            fieldDefinitions.add(getID00_mepid());
        }
        fieldDefinitions.add(getID01_firstName());
        fieldDefinitions.add(getID02_middleName());
        fieldDefinitions.add(getID03_lastName());
        fieldDefinitions.add(getID04_birthDate());
        fieldDefinitions.add(getID05_gender());
        fieldDefinitions.add(getID06_license());
        fieldDefinitions.add(getID07_localId());
        setFieldDefinitions(fieldDefinitions);

        /*
         * Build the criteia/query for the staff to include in this export based on user input.
         */
        X2Criteria criteria = new X2Criteria();

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addEqualTo(SisStaff.COL_ORGANIZATION1_OID, getOrganization().getOid());
        }

        String epimsStatusField = this.translateAliasToJavaName(EPIMS_STATUS_FIELD, true);
        criteria.addNotEmpty(epimsStatusField, getBroker().getPersistenceKey());

        if (m_exportType == TYPE_ASSIGN) {
            criteria.addEmpty(SisStaff.COL_STATE_ID, getBroker().getPersistenceKey());
        } else if (m_exportType == TYPE_VERIFICATION) {
            criteria.addNotEmpty(SisStaff.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, criteria);

        staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);

        // Set the query to be used for student selection.
        setQuery(staffQuery);
        setEntityClass(MEPIDEntity.class);

        // Load supporting information.
        loadLicenseNumbers(criteria);
    }

    /**
     * Build Field definition for ID 00, staff mepid.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID00_mepid() {
        FieldDefinition field = new FieldDefinition(ID_00_MEPID,
                SisStaff.COL_STATE_ID,
                null, false, 8, 8, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for ID 01, staff first name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID01_firstName() {
        FieldDefinition field = new FieldDefinition(ID_01_FIRST_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                null, false, 1, 30, REGEX_NAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for ID 02, staff middle name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID02_middleName() {
        FieldDefinition field = new FieldDefinition(ID_02_MIDDLE_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_MIDDLE_NAME,
                NO_MIDDLE_NAME, false, 1, 30, REGEX_MNAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for ID 03, staff last name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID03_lastName() {
        FieldDefinition field = new FieldDefinition(ID_03_LAST_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                null, false, 1, 30, REGEX_NAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for ID 04, staff date of birth.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID04_birthDate() {
        FieldDefinition field = new FieldDefinition(ID_04_BIRTH_DATE,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_DOB,
                null, false, 10, 10, null,
                m_dateFormat, null,
                new ValidateBirthdate(), null);
        return field;
    }

    /**
     * Build Field definition for ID 05, staff gender.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID05_gender() {
        FieldDefinition field = new FieldDefinition(ID_05_GENDER,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_GENDER_CODE,
                null, false, 1, 1, "M|F|N",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for ID 06, staff certification license.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID06_license() {
        FieldDefinition field = new FieldDefinition(ID_06_LICENSE,
                SisStaff.REL_CERTIFICATIONS + ModelProperty.PATH_DELIMITER
                        + StaffCertification.COL_CERTIFICATION_NUMBER,
                "00", false, 2, 20, REGEX_ALPHANUMERIC, null,
                new RetrieveLicense(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for ID 07, staff local identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getID07_localId() {
        FieldDefinition field = new FieldDefinition(ID_07_LOCALID,
                SisStaff.COL_LOCAL_ID,
                "0", false, 1, 20, REGEX_ALPHANUMERIC_HYPHEN,
                null, null, null, null);
        return field;
    }

    /**
     * Loads the primary licenses for the staff members included in the query.
     *
     * @param staffCriteria a Criteria object rooted at the Staff bean
     */
    private void loadLicenseNumbers(Criteria staffCriteria) {
        SubQuery staffQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);

        int count = getBroker().getCount(staffQuery);

        Criteria licenseCriteria = new Criteria();
        licenseCriteria.addIn(StaffCertification.COL_STAFF_OID, staffQuery);
        licenseCriteria.addEqualTo(StaffCertification.COL_PRIMARY_INDICATOR, Boolean.TRUE);

        QueryByCriteria licenseQuery = new QueryByCriteria(StaffCertification.class, licenseCriteria);
        licenseQuery.addOrderByDescending(StaffCertification.COL_ISSUE_DATE);
        m_licensesMap = getBroker().getGroupedCollectionByQuery(licenseQuery, StaffCertification.COL_STAFF_OID, count);
    }
}
