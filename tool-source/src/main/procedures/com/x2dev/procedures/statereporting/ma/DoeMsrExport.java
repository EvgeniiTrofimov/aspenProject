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
package com.x2dev.procedures.statereporting.ma;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for the Massachusetts Department of Education MSR data export.
 * <p>
 * This procedure is based on the MA DOE SIMS export.
 *
 * @author X2 Development Corporation
 */
public class DoeMsrExport extends ExportJavaSource {
    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- User-referencable code
    // ---------------------------------------------------------------------------------------------

    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /*
     * DOE field alias constants. These field aliases are all for the STUDENT table.
     */
    private static final String DOE_08_BIRTH_CITY = "DOE 08";
    private static final String DOE_15_SCHOOL = "DOE 15";
    private static final String DOE_34_SPED_PLACEMENT = "DOE 34";

    private static final String DOE_NO_MIDDLE_NAME = "DOE NMN";

    private static final String DOE_DISTRICT_ID = "DOE Organization ID";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_STATUS_FIELD = "DOE Status";
    private static final String DOE_STATUS_FIELD_REPORT_CODE = "Report";

    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- X2-specific code below
    // ---------------------------------------------------------------------------------------------

    /*
     * TODO: Calculation optimizations: perform one query for all students and store results in a
     * map for conduct, attendance, and membership lookups (possibly AP courses as well but that
     * might be more difficult to do in a single query). Also, put the term OID lookup in a map
     * keyed on schedule OID.
     */

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "SPED only" parameter. The value is a Boolean.
     */
    public static final String SPED_ONLY_PARAM = "spedOnly";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_BIRTH_CITY_CHARACTERS = "[_\\W&&[^\\s]]";
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    // private static final String DATE_FORMAT_CODE = "D10";
    private static final double HASHMAP_CAPACITY_MULTIPLIER = 1.5;
    private static final String VALIDATION_INVALID_VALUE = "Invalid value";
    private static final String VALIDATION_MISSING_VALUE = "Missing value";

    private List m_columnNames;
    private Map m_columnsToUserNames;
    private List m_columnUserNames;
    private DateFormat m_dateFormat;
    private String m_doe01Lasid;
    // private String m_doe02Sasid;
    private String m_doe03FirstName;
    private String m_doe04MiddleName;
    private String m_doe05LastName;
    private String m_doe06Dob;
    // private String m_doe07DateFormat;
    private String m_doe08BirthCity;
    private String m_doe09Gender;
    private String m_doe14ResidenceCity;
    private String m_doe15School;
    private String m_doe34SpedPlacement;
    private String m_doeNoMiddleName;
    private String m_doeStatusField;
    private HashMap m_discontinuedFields;
    private HashMap m_fieldDefaults;
    private HashMap m_fieldToRefTable;
    private Pattern m_illegalBirthCityCharacters;
    private Pattern m_illegalNameCharacters;
    private Collection m_suspensionInCodes;
    private Collection m_suspensionOutCodes;
    private boolean m_studentHasErrors;
    private String m_studentIdentifier;
    private Collection m_truancyCodes;
    private StringBuilder m_validationErrors;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() {
        DataGrid dataGrid = null;

        try {
            initializeFields();
            initializeColumns();

            /*
             * Set up converters, formatters, reference lookup tables, and other database-intense
             * operations. We do this once outside the student loop to improve performance.
             */
            m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
            m_illegalBirthCityCharacters = Pattern.compile(ILLEGAL_BIRTH_CITY_CHARACTERS);
            m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

            loadReferenceCodes();

            /*
             * Get the list of students to include and the sort order based on input parameters.
             * Always exclude archived students and students that belong to inactive schools.
             */
            Criteria criteria = new Criteria();
            criteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
            criteria.addEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            criteria.addEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);

            Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
            if (requireReportStatus.booleanValue()) {
                criteria.addEqualTo(m_doeStatusField, DOE_STATUS_FIELD_REPORT_CODE);
            }

            if (isSchoolContext()) {
                criteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            }

            Boolean spedOnly = (Boolean) getParameter(SPED_ONLY_PARAM);
            if (spedOnly.booleanValue()) {
                criteria.addNotNull(m_doe34SpedPlacement);
                criteria.addNotEqualTo(m_doe34SpedPlacement, "");
                criteria.addNotEqualTo("upper(" + m_doe34SpedPlacement + ")", "NONE");
            }

            String queryString = (String) getParameter(QUERY_STRING_PARAM);
            int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
            switch (queryBy) {
                case 1: // YOG
                    criteria.addEqualTo(SisStudent.COL_YOG, queryString);
                    break;

                case 2: // LASID
                    criteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                    break;

                case 3: // SASID
                    criteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                    break;

                case 4: // Snapshot
                    addRecordSetCriteria(criteria, queryString);
                    break;

                default:
                    // Take all students in the district
                    break;
            }

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    query.addOrderByAscending(SisStudent.COL_YOG);
                    query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    query.addOrderByAscending(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_doe15School);
                    query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    query.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    query.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            /*
             * Initialize the data grid and add students one at a time. Also initialize the size of
             * the
             * validation errors buffer - assume 1 error for every 5 students and each error takes
             * 128
             * characters to describe.
             */
            int count = getBroker().getCount(query);
            dataGrid = new DataGrid(count, 52);
            m_validationErrors = new StringBuilder(count / 5 * 128);

            QueryIterator students = getBroker().getIteratorByQuery(query);
            try {
                while (students.hasNext()) {
                    try {
                        SisStudent student = (SisStudent) students.next();

                        if (StringUtils.isEmpty(student.getStateId())) {
                            m_studentHasErrors = false;
                            m_studentIdentifier = student.getNameView() +
                                    " [LASID: " + student.getLocalId() + ", SASID: " + student.getStateId() + "]\n";

                            addStudentToGrid(dataGrid, student);
                        }
                    } catch (Exception e) {
                        /*
                         * Log the exception and continue on to the next student. The student will
                         * appear with null values in the export.
                         */
                        logValidationError("An exception occurred: " + e.getMessage());
                    }
                }
            } finally {
                students.close();
            }
        } catch (X2BaseException xbe) {
            dataGrid = new DataGrid(1, 1);
        }

        return dataGrid;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return m_validationErrors.toString();
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columnNames;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columnUserNames;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        StringBuilder header = new StringBuilder();

        String code = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_ID);
        if (StringUtils.isEmpty(code)) {
            code = "[INSERT DISTRICT ID HERE]";
            logValidationError("Using a placeholder for the district ID. Set the '" + DOE_DISTRICT_ID
                    + "' alias in the Data Dictionary and update that field with the correct ID.");
        }

        header.append("DOEHEADER,SCHOOL=");
        header.append(code);
        header.append(getLineSeparator());
        header.append("DOEHEADER,ELEMENTS=DOE001,DOE003,DOE004,DOE005,DOE006,DOE008,DOE009,DOE014");
        header.append(getLineSeparator());
        header.append("DOEHEADER,DATEFORMAT=D10");
        header.append(getLineSeparator());
        header.append("DOEHEADER,GENDERFORMAT,M=MALE,F=FEMALE,N=NONBINARY");
        header.append(getLineSeparator());

        return header.toString();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setIncludeHeaderRow(false);
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#releaseResources()
     */
    @Override
    protected void releaseResources() {
        if (m_discontinuedFields != null) {
            m_discontinuedFields.clear();
            m_discontinuedFields = null;
        }

        if (m_fieldDefaults != null) {
            m_fieldDefaults.clear();
            m_fieldDefaults = null;
        }

        if (m_fieldToRefTable != null) {
            Iterator tables = m_fieldToRefTable.values().iterator();
            while (tables.hasNext()) {
                Map table = (Map) tables.next();
                if (table != null) {
                    table.clear();
                    table = null;
                }

            }
            m_fieldToRefTable.clear();
            m_fieldToRefTable = null;
        }

        if (m_suspensionInCodes != null) {
            m_suspensionInCodes.clear();
            m_suspensionInCodes = null;
        }

        if (m_suspensionOutCodes != null) {
            m_suspensionOutCodes.clear();
            m_suspensionOutCodes = null;
        }

        if (m_truancyCodes != null) {
            m_truancyCodes.clear();
            m_truancyCodes = null;
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Adds the 52 elements for the given student to a new row in the data grid.
     *
     * @param dataGrid DataGrid
     * @param student SisStudent
     * @throws X2BaseException exception
     */
    private void addStudentToGrid(DataGrid dataGrid, SisStudent student) throws X2BaseException {
        dataGrid.append();

        dataGrid.set(m_doe01Lasid, getProperty(student, m_doe01Lasid));
        dataGrid.set(m_doe03FirstName, stripIllegalNameCharacters(getProperty(student, m_doe03FirstName)));
        dataGrid.set(m_doe04MiddleName, stripIllegalNameCharacters(getMiddleName(student)));
        dataGrid.set(m_doe05LastName, stripIllegalNameCharacters(getProperty(student, m_doe05LastName)));
        dataGrid.set(m_doe06Dob, m_dateFormat.format((PlainDate) getProperty(student, m_doe06Dob)));
        // dataGrid.set(m_doe07DateFormat, DATE_FORMAT_CODE);
        dataGrid.set(m_doe08BirthCity, stripIllegalBirthCityCharacters(getProperty(student, m_doe08BirthCity)));
        dataGrid.set(m_doe09Gender, getProperty(student, m_doe09Gender));
        dataGrid.set(m_doe14ResidenceCity, getReferenceCode(student, m_doe14ResidenceCity));
    }

    /**
     * Returns the middle name for the student. This method considers the "No Middle Name" flag.
     *
     * @param student SisStudent
     * @return String
     * @throws X2BaseException exception
     */
    private String getMiddleName(SisStudent student) throws X2BaseException {
        String middleName = null;

        String noMiddleName = (String) WebUtils.getProperty(student, m_doeNoMiddleName);
        if (BooleanAsStringConverter.TRUE.equals(noMiddleName)) {
            middleName = "NMN";
        } else {
            middleName = student.getPerson().getMiddleName();
            if (StringUtils.isEmpty(middleName)) {
                middleName = "NMN";
                logValidationError(m_doe04MiddleName, VALIDATION_MISSING_VALUE);
            }
        }

        return middleName;
    }

    /**
     * Returns the value for the specified property on the student bean. This method will log
     * missing values.
     *
     * @param student SisStudent
     * @param property String
     * @return Object
     * @throws X2BaseException exception
     */
    private Object getProperty(SisStudent student, String property) throws X2BaseException {
        Object value = WebUtils.getProperty(student, property);

        boolean missingValue = (value == null);
        if (!missingValue && value instanceof String) {
            missingValue = StringUtils.isEmpty((String) value);
        }

        if (missingValue) {
            logValidationError(property, VALIDATION_MISSING_VALUE);
        }

        return value;
    }

    /**
     * Returns the state equivalent for the given property on the student. Missing values will be
     * defaulted, if a default value exists. Discontinued fields will returned the required value.
     *
     * @param student SisStudent
     * @param propertyName String
     * @return String
     * @throws X2BaseException exception
     */
    private String getReferenceCode(SisStudent student, String propertyName) throws X2BaseException {
        String stateCode = null;

        if (m_discontinuedFields.containsKey(propertyName)) {
            stateCode = (String) m_discontinuedFields.get(propertyName);
        } else {
            String baseCode = (String) WebUtils.getProperty(student, propertyName);
            if (StringUtils.isEmpty(baseCode)) {
                stateCode = (String) m_fieldDefaults.get(propertyName);
                if (StringUtils.isEmpty(stateCode)) {
                    // There was no default for a missing value so this is a validation error.
                    logValidationError(propertyName, VALIDATION_MISSING_VALUE);
                }
            } else {
                Map baseToStateCodes = (Map) m_fieldToRefTable.get(propertyName);
                stateCode = (String) baseToStateCodes.get(baseCode);
                if (StringUtils.isEmpty(stateCode)) {
                    // There was no state code for the base code so this is a validation error.
                    logValidationError(propertyName, VALIDATION_INVALID_VALUE);
                }
            }
        }

        return stateCode;
    }

    /**
     * Returns a map of base reference codes to their state reference code equivalents for the
     * reference table used by the given student property. If the student property doesn't use a
     * reference table then an empty map is returned.
     *
     * @param propertyName String
     * @return A Map of String keys to String values
     */
    private Map getReferenceMap(String propertyName) {
        HashMap baseToStateCodes = null;

        ModelProperty property =
                new ModelProperty(SisStudent.class.getName(), propertyName, getBroker().getPersistenceKey());
        DataDictionaryField field = property.getField();
        if (field.hasReferenceTable()) {
            Collection codes = field.getReferenceTable().getReferenceCodes(getBroker());
            baseToStateCodes = new HashMap((int) (codes.size() * HASHMAP_CAPACITY_MULTIPLIER));
            Iterator codeIterator = codes.iterator();
            while (codeIterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) codeIterator.next();
                baseToStateCodes.put(code.getCode(), code.getStateCode());
            }
        } else {
            baseToStateCodes = new HashMap();
        }

        return baseToStateCodes;
    }

    /**
     * Adds the 8 DOE elements to the column name and column user name lists and populates the map
     * of the first to the latter.
     */
    private void initializeColumns() {
        m_columnNames = new ArrayList(8);
        m_columnUserNames = new ArrayList(8);
        m_columnsToUserNames = new HashMap(16);

        m_columnNames.add(m_doe01Lasid);
        String userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe01Lasid, getBroker().getPersistenceKey()), true,
                null, false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe01Lasid, userName);

        m_columnNames.add(m_doe03FirstName);
        userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe03FirstName, getBroker().getPersistenceKey()), true,
                null, false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe03FirstName, userName);

        m_columnNames.add(m_doe04MiddleName);
        userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe04MiddleName, getBroker().getPersistenceKey()), true,
                null, false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe04MiddleName, userName);

        m_columnNames.add(m_doe05LastName);
        userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe05LastName, getBroker().getPersistenceKey()), true,
                null, false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe05LastName, userName);

        m_columnNames.add(m_doe06Dob);
        userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe06Dob, getBroker().getPersistenceKey()), true, null,
                false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe06Dob, userName);

        // m_columnNames.add(m_doe07DateFormat);
        // m_columnUserNames.add(m_doe07DateFormat);
        // m_columnsToUserNames.put(m_doe07DateFormat, m_doe07DateFormat);

        m_columnNames.add(m_doe08BirthCity);
        userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe08BirthCity, getBroker().getPersistenceKey()), true,
                null, false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe08BirthCity, userName);

        m_columnNames.add(m_doe09Gender);
        userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe09Gender, getBroker().getPersistenceKey()), true,
                null, false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe09Gender, userName);

        m_columnNames.add(m_doe14ResidenceCity);
        userName = WebUtils.getLabel(
                new ModelProperty(SisStudent.class.getName(), m_doe14ResidenceCity, getBroker().getPersistenceKey()),
                true, null, false);
        m_columnUserNames.add(userName);
        m_columnsToUserNames.put(m_doe14ResidenceCity, userName);
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     *
     * @throws X2BaseException exception
     */
    private void initializeFields() throws X2BaseException {
        /*
         * Fields 1-7, 9, and 14 do not use aliases.
         */
        m_doe01Lasid = SisStudent.COL_LOCAL_ID;
        m_doe03FirstName = SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_FIRST_NAME;
        m_doe04MiddleName = SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_MIDDLE_NAME;
        m_doe05LastName = SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_LAST_NAME;
        m_doe06Dob = SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_DOB;
        // m_doe07DateFormat = "DATE_FORMAT";

        m_doe09Gender = SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_GENDER_CODE;

        m_doe14ResidenceCity = SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS
                + ModelProperty.PATH_DELIMITER + SisAddress.COL_CITY;

        m_doe08BirthCity = translateAliasToJavaName(DOE_08_BIRTH_CITY);

        m_doe15School = translateAliasToJavaName(DOE_15_SCHOOL);
        m_doe34SpedPlacement = translateAliasToJavaName(DOE_34_SPED_PLACEMENT);

        m_doeNoMiddleName = translateAliasToJavaName(DOE_NO_MIDDLE_NAME);
        m_doeStatusField = translateAliasToJavaName(DOE_STATUS_FIELD);
    }

    /**
     * Loads the all the reference codes for the export. This is done with three Maps - one that
     * maps discontinued fields to required values, another that maps fields to default values, and
     * a third that maps fields to maps of DOE equivalent values. For example:
     * <p>
     * 
     * <pre>
     * Map 1: Discontinued Fields to Required Values
     *
     *  - DOE Field 27 --> "500"
     *  - DOE Field 28 --> "500"
     *  - DOE Field 29 --> "500"
     *    ...
     *
     * Map 2: Fields to Default Values
     *
     *  - DOE Field 19 --> "00"
     *  - DOE Field 24 --> "267"
     *  - DOE Field 32 --> "500"
     *    ...
     *
     * Map 3: Fields to Maps of DOE Equivalent Values
     *
     *  - DOE Field 29 --> Map
     *                     "Asian"     --> "10"
     *                     "Hispanic"  --> "15"
     *                     "Caucasian" --> "20"
     *                     "African"   --> "25"
     *  - DOE Field 30 --> Map
     *                     "Free"      --> "05"
     *                     "Reduced"   --> "10"
     *    ...
     * </pre>
     */
    private void loadReferenceCodes() {
        /*
         * Discontinued fields: 27, 28, 29, 30, 39, 40, 41
         */
        m_discontinuedFields = new HashMap((int) (9 * HASHMAP_CAPACITY_MULTIPLIER));

        /*
         * Default values: fields 19-52 minus discontinued fields, AP course fields, and counts
         * (suspensions in, suspensions out, days truant).
         */
        m_fieldDefaults = new HashMap((int) (9 * HASHMAP_CAPACITY_MULTIPLIER));

        m_fieldDefaults.put(m_doe34SpedPlacement, "00");

        /*
         * State code maps: fields 10-14, 16, 19-52 minus discontinued fields, AP course fields,
         * and counts (suspensions in, suspensions out, days truant).
         */
        m_fieldToRefTable = new HashMap((int) (22 * HASHMAP_CAPACITY_MULTIPLIER));

        m_fieldToRefTable.put(m_doe14ResidenceCity, getReferenceMap(m_doe14ResidenceCity));
        m_fieldToRefTable.put(m_doe34SpedPlacement, getReferenceMap(m_doe34SpedPlacement));
    }

    /**
     * Logs a validation error with the message for the current student.
     *
     * @param message String
     */
    private void logValidationError(String message) {
        if (!m_studentHasErrors) {
            m_validationErrors.append(m_studentIdentifier);
            m_studentHasErrors = true;
        }

        m_validationErrors.append('\t');
        m_validationErrors.append(message);
        m_validationErrors.append('\n');
    }

    /**
     * Logs a validation error with the property and message for the current student.
     *
     * @param property String
     * @param message String
     */
    private void logValidationError(String property, String message) {
        logValidationError(((String) m_columnsToUserNames.get(property)) + " - " + message);
    }

    /**
     * Returns the passed city/town of birth value stripped of any illegal characters. No
     * punctuation is allowed in city/town of birth fields.
     *
     * @param cityTownOfBirth must be a String, although an Object is taken for convenience; a
     *        ClassCastException will occur if this argument is not a String
     *
     * @return String
     */
    private String stripIllegalBirthCityCharacters(Object cityTownOfBirth) {
        Matcher matcher = m_illegalBirthCityCharacters.matcher((String) cityTownOfBirth);
        return matcher.replaceAll("");
    }

    /**
     * Returns the passed name value stripped of any illegal characters. No punctuation is allowed
     * in name fields except for periods ("."), hyphens ("-"), and apostrophes ("'").
     *
     * @param nameValue must be a String, although an Object is taken for convenience; a
     *        ClassCastException will occur if this argument is not a String
     *
     * @return String
     */
    private String stripIllegalNameCharacters(Object nameValue) {
        Matcher matcher = m_illegalNameCharacters.matcher((String) nameValue);
        return matcher.replaceAll("");
    }

    /**
     * Translates an alias into a Java name. A FieldAliasException will be thrown if the alias does
     * not exist.
     *
     * @param alias String
     * @return String
     * @throws X2BaseException exception
     */
    private String translateAliasToJavaName(String alias) throws X2BaseException {
        String javaName = null;

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else {
            m_validationErrors = new StringBuilder(64);
            m_validationErrors.append("Unable to resolve dictionary alias \"");
            m_validationErrors.append(alias);
            m_validationErrors.append("\"");

            throw new X2BaseException();
        }

        return javaName;
    }
}
