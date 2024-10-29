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
package com.x2dev.reports.statereporting.ny;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DecimalAsStringConverter;
import com.x2dev.utils.converters.IntegerAsStringConverter;
import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

/**
 * Class for populating JasperReport design for "NY Individual Incident Report(IIR).
 */
public class NYIIRReportData extends ReportJavaSourceNet {

    /**
     * Sub-reports
     */
    public static final String IRR_OFFENDER_OTHER_SUBREPORT = "NY-IIR-OTHER-SUB";
    public static final String IRR_OFFENDER_SUBREPORT = "NY-IIR-OFF-SUB";
    public static final String IRR_STUDENT_VICTIMS_SUBREPORT = "NY-IIR-STD-VICT-SUB";

    public static final String PARAM_END_DATE = "endDate";
    public static final String PARAM_QUERY_BY = "queryBy";
    public static final String PARAM_QUERY_STRING = "queryString";
    public static final String PARAM_START_DATE = "startDate";

    private static final String ACTION_MAP_KEY_ALTERNATIVE_PLACEMENT = "alternativePlacement";
    private static final String ACTION_MAP_KEY_COMMUNITY_SERVICE = "communityService";
    private static final String ACTION_MAP_KEY_COUNSELING = "counseling";
    private static final String ACTION_MAP_KEY_ISS = "iss";
    private static final String ACTION_MAP_KEY_JUSTICE = "justice";
    private static final String ACTION_MAP_KEY_LAW_ENFORCEMENT = "lawEnforcement";
    private static final String ACTION_MAP_KEY_OSS = "oss";
    private static final String ACTION_MAP_KEY_TEACHER_REMOVAL = "teacherRemoval";

    private static final String ALIAS_IRR_ACCEPT_TRANSFER = "accept-school-transfer";
    private static final String ALIAS_IRR_ARREST = "arrest-related";
    private static final String ALIAS_IRR_BIAS = "bias";
    private static final String ALIAS_IRR_CATEGORY = "primary-category";
    private static final String ALIAS_IRR_DATE = "date";
    private static final String ALIAS_IRR_FIREARMS = "num-firearms";
    private static final String ALIAS_IRR_GANG = "gang-related";
    private static final String ALIAS_IRR_INCIDENT_ID = "incident-id";
    private static final String ALIAS_IRR_INVESTIGATED = "verified-by-investigation";
    private static final String ALIAS_IRR_INVESTIGATION = "investigation-description";
    private static final String ALIAS_IRR_INVOLVING_ALCOHOL = "involving-alcohol";
    private static final String ALIAS_IRR_INVOLVING_DRUGS = "involving-drugs";
    private static final String ALIAS_IRR_KNIVES = "num-knives";
    private static final String ALIAS_IRR_LOCATION_TIME = "location-time";
    private static final String ALIAS_IRR_OFFICER_PRESENT = "officer-present";
    private static final String ALIAS_IRR_OTHER_WEAPONS = "num-other-weapons";
    private static final String ALIAS_IRR_REQUEST_TRANSFER = "request-school-transfer";
    private static final String ALIAS_IRR_SECONDARY_CATEGORIES = "secondary-categories";
    private static final String ALIAS_IRR_VICTIM_CRIMINAL_OFFENSE = "victim-violent-offense";

    private static final String ALIAS_OFFENDER_AGE = "irr-offender-age";
    private static final String ALIAS_OFFENDER_ALTERNATIVE_PLACEMENT = "irr-offender-alt";
    private static final String ALIAS_OFFENDER_COMMUNITY_SERVICE = "irr-offender-service";
    private static final String ALIAS_OFFENDER_COMMUNITY_SERVICE_TIME = "irr-offender-service-time";
    private static final String ALIAS_OFFENDER_COUNSELING = "irr-offender-counseling";
    private static final String ALIAS_OFFENDER_COUNSELING_TIME = "irr-offender-counseling-time";
    private static final String ALIAS_OFFENDER_DISCIPLINARY_ACTION = "irr-offender-discipline-action";
    private static final String ALIAS_OFFENDER_GRADE = "irr-offender-grade";
    private static final String ALIAS_OFFENDER_ISS = "irr-offender-iss";
    private static final String ALIAS_OFFENDER_ISS_TIME = "irr-offender-iss-time";
    private static final String ALIAS_OFFENDER_JUSTICE_SYSTEM = "irr-offender-justice";
    private static final String ALIAS_OFFENDER_LAW_ENFORCEMENT = "irr-offender-law-enforcement";
    private static final String ALIAS_OFFENDER_NAME = "irr-offender-name";
    private static final String ALIAS_OFFENDER_OSS = "irr-offender-oss";
    private static final String ALIAS_OFFENDER_OSS_TIME = "irr-offender-oss-time";
    private static final String ALIAS_OFFENDER_OTHER_ACTION = "irr-offender-other-action";
    private static final String ALIAS_OFFENDER_TYPE = "irr-offender-type";
    private static final String ALIAS_OFFENDER_REFERRAL_ACTION = "irr-offender-referral-action";
    private static final String ALIAS_OFFENDER_TEACHER_REMOVAL = "irr-offender-teach-remove";
    private static final String ALIAS_OFFENDER_TEACHER_REMOVAL_TIME = "irr-offender-teach-remove-time";
    private static final String ALIAS_OFFENDER_DISABILITIES = "irr-offender-disabilities";
    private static final String ALIAS_OFFENDER_WEAPON = "irr-offender-weapon";

    private static final String ALIAS_VICTIM_AGE = "irr-victim-age";
    private static final String ALIAS_VICTIM_GRADE = "irr-victim-grade";
    private static final String ALIAS_VICTIM_NAME = "irr-victim-name";
    private static final String ALIAS_VICTIM_TYPE = "irr-victim-type";

    private static final String FIELD_IIR_ARREST = "schoolArrest";
    private static final String FIELD_IIR_CHECKBOXES = "checkboxes";
    private static final String FIELD_IIR_FIREARMS = "firearmsCount";
    private static final String FIELD_IIR_INCIDENT_CATEGORY = "incidentCategory";
    private static final String FIELD_IIR_INCIDENT_ID = "incidentId";
    private static final String FIELD_IIR_KNIVES = "knivesCount";
    private static final String FIELD_IIR_OTHER_WEAPONS = "otherWeaponsCount";
    private static final String FIELD_IIR_VICTIM_STUDENTS_COUNT = "victimStudentsCount";
    private static final String FIELD_IIR_VICTIM_STAFF_COUNT = "victimStaffCount";
    private static final String FIELD_IIR_OTHER_VICTIMS_COUNT = "otherVictimsCount";
    private static final String FIELD_IIR_OFFENDER_STUDENTS_COUNT = "offenderStudentsCount";
    private static final String FIELD_IIR_OFFENDER_STAFF_COUNT = "offenderStaffCount";
    private static final String FIELD_IIR_OTHER_OFFENDERS_COUNT = "otherOffendersCount";

    private static final String FIELD_IIR_VICTIM_CRIMINAL_OFFENCE = "victimCriminalOffence";
    private static final String FIELD_IIR_REQUEST_TRANSFER = "requestTransfer";
    private static final String FIELD_IIR_ACCEPT_TRANSFER = "acceptTransfer";
    private static final String FIELD_IIR_OFFICER_PRESENT = "officerPresent";
    private static final String FIELD_IIR_INVESTIGATION = "investigation";
    private static final String FIELD_IIR_INVESTIGATION_INDICATOR = "investigationIndicator";
    private static final String FIELD_IIR_WEAPONS_INVOLVED = "weaponsInvolved";
    private static final String FIELD_IIR_DISCIPLINARY_ACTION = "disciplinaryAction";
    private static final String FIELD_IIR_OFFENDER_ACTION_MAP = "actionMap";

    private static final String FIELD_OFFENDER_AGE = "offenderAge";
    private static final String FIELD_OFFENDER_DISCIPLINARY_ACTION = "disciplinaryAction";
    private static final String FIELD_OFFENDER_GRADE = "offenderGrade";
    private static final String FIELD_OFFENDER_NAME = "offenderName";
    private static final String FIELD_OFFENDER_REFERRAL_ACTION = "referralAction";
    private static final String FIELD_OFFENDER_OTHER_ACTION = "otherAction";

    private static final String FIELD_SUBREPORT_OFFENDERS_FORMAT = "offendersSectionFormat";
    private static final String FIELD_SUBREPORT_OFFENDERS_GRID = "offendersSectionGrid";
    private static final String FIELD_SUBREPORT_OFFENDERS_OTHER_FORMAT = "offendersOtherFormat";
    private static final String FIELD_SUBREPORT_OFFENDERS_OTHER_GRID = "offendersOtherGrid";
    private static final String FIELD_SUBREPORT_STUDENT_VICTIMS_FORMAT = "studentVictimsFormat";
    private static final String FIELD_SUBREPORT_STUDENT_VICTIMS_GRID = "studentVictimsGrid";

    private static final String FIELD_STUDENT_VICTIMS_AGE_GRADE = "ageGrade";
    private static final String FIELD_STUDENT_VICTIMS_CURRENT_NUM = "currentNum";

    private static final String PERSON_TYPE_OTHER = "Other";
    private static final String PERSON_TYPE_STAFF = "Staff";
    private static final String PERSON_TYPE_STUDENT = "Student";

    private static final String USER_DEFINED_TABLES_DDX_ID = "NY-IIR";

    /**
     * Aliases of 'IIR Victim' table
     */


    private BooleanAsStringConverter m_booleanConverter = (BooleanAsStringConverter) ConverterFactory
            .getConverterForClass(Converter.BOOLEAN_CONVERTER, Locale.getDefault(), true);
    private ExtendedDataDictionary m_ddxById = null;
    private DecimalAsStringConverter m_decimalConverter = (DecimalAsStringConverter) ConverterFactory
            .getConverterForClass(Converter.BIG_DECIMAL_CONVERTER, Locale.getDefault(), true);
    private DataDictionary m_dictionary = null;
    private IntegerAsStringConverter m_integerConverter = (IntegerAsStringConverter) ConverterFactory
            .getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);
    private List<UserDefinedTableA> m_IRRData;
    private Report m_subReportOffenders;
    private Report m_subReportOffendersOther;
    private Report m_subReportStudentVictims;
    private UserDefinedTableA m_currentIncident;

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    public void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_currentIncident = userData.getCurrentRecord(UserDefinedTableA.class);
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        m_IRRData = (List<UserDefinedTableA>) getBroker().getCollectionByQuery(buildIRRQuery());

        for (UserDefinedTableA irrData : m_IRRData) {
            Map<String, Collection<UserDefinedTableB>> victimMap = getVictimsByType(irrData);
            Map<String, Collection<UserDefinedTableC>> offenderMap = getOffendersByType(irrData);

            grid.append();
            // All incident categories
            grid.set(FIELD_IIR_INCIDENT_ID, irrData.getFieldValueByAlias(ALIAS_IRR_INCIDENT_ID, m_dictionary));
            grid.set(FIELD_IIR_INCIDENT_CATEGORY, getIncidentCategories(irrData));

            // Checkbox based values
            Set<String> checkboxes = getDFieldStateCodes(irrData, ALIAS_IRR_BIAS);
            checkboxes.addAll(getDFieldStateCodes(irrData, ALIAS_IRR_LOCATION_TIME));
            if (BooleanAsStringConverter.TRUE.equals(irrData.getFieldValueByAlias(ALIAS_IRR_GANG, m_dictionary))) {
                checkboxes.add("O");
            }
            if (BooleanAsStringConverter.TRUE
                    .equals(irrData.getFieldValueByAlias(ALIAS_IRR_INVOLVING_ALCOHOL, m_dictionary))) {
                checkboxes.add("R");
            }
            if (BooleanAsStringConverter.TRUE
                    .equals(irrData.getFieldValueByAlias(ALIAS_IRR_INVOLVING_DRUGS, m_dictionary))) {
                checkboxes.add("S");
            }
            grid.set(FIELD_IIR_CHECKBOXES, checkboxes);

            // Weapons counts
            grid.set(FIELD_IIR_KNIVES,
                    getIntegerString((String) irrData.getFieldValueByAlias(ALIAS_IRR_KNIVES, m_dictionary)));
            grid.set(FIELD_IIR_FIREARMS,
                    getIntegerString((String) irrData.getFieldValueByAlias(ALIAS_IRR_FIREARMS, m_dictionary)));
            grid.set(FIELD_IIR_OTHER_WEAPONS,
                    getIntegerString((String) irrData.getFieldValueByAlias(ALIAS_IRR_OTHER_WEAPONS, m_dictionary)));

            // Victim Counts
            grid.set(FIELD_IIR_VICTIM_STUDENTS_COUNT, getIntegerString(victimMap.get(PERSON_TYPE_STUDENT)));
            grid.set(FIELD_IIR_VICTIM_STAFF_COUNT, getIntegerString(victimMap.get(PERSON_TYPE_STAFF)));
            grid.set(FIELD_IIR_OTHER_VICTIMS_COUNT, getIntegerString(victimMap.get(PERSON_TYPE_OTHER)));

            // Offender Counts
            grid.set(FIELD_IIR_OFFENDER_STUDENTS_COUNT, getIntegerString(offenderMap.get(PERSON_TYPE_STUDENT)));
            grid.set(FIELD_IIR_OFFENDER_STAFF_COUNT, getIntegerString(offenderMap.get(PERSON_TYPE_STAFF)));
            grid.set(FIELD_IIR_OTHER_OFFENDERS_COUNT, getIntegerString(offenderMap.get(PERSON_TYPE_OTHER)));

            // Questions
            grid.set(FIELD_IIR_VICTIM_CRIMINAL_OFFENCE, getBoolean(irrData, ALIAS_IRR_VICTIM_CRIMINAL_OFFENSE));
            grid.set(FIELD_IIR_REQUEST_TRANSFER, getBoolean(irrData, ALIAS_IRR_REQUEST_TRANSFER));
            grid.set(FIELD_IIR_ACCEPT_TRANSFER, getBoolean(irrData, ALIAS_IRR_ACCEPT_TRANSFER));
            grid.set(FIELD_IIR_OFFICER_PRESENT, getBoolean(irrData, ALIAS_IRR_OFFICER_PRESENT));
            grid.set(FIELD_IIR_ARREST, getBoolean(irrData, ALIAS_IRR_ARREST));
            grid.set(FIELD_IIR_INVESTIGATION_INDICATOR, getBoolean(irrData, ALIAS_IRR_INVESTIGATED));
            grid.set(FIELD_IIR_INVESTIGATION, irrData.getFieldValueByAlias(ALIAS_IRR_INVESTIGATION, m_dictionary));

            // Weapons Involved
            grid.set(FIELD_IIR_WEAPONS_INVOLVED, getWeaponCounts(offenderMap.get(PERSON_TYPE_STUDENT)));

            // Disciplinary Actions
            grid.set(FIELD_IIR_DISCIPLINARY_ACTION, getDisciplinaryActionCount(offenderMap.get(PERSON_TYPE_STUDENT)));

            // Victims Grid
            grid.set(FIELD_SUBREPORT_STUDENT_VICTIMS_FORMAT,
                    new ByteArrayInputStream(m_subReportStudentVictims.getCompiledFormat()));
            grid.set(FIELD_SUBREPORT_STUDENT_VICTIMS_GRID, buildStudentVictimsGrid(victimMap.get(PERSON_TYPE_STUDENT)));

            // Student Offenders grid
            grid.set(FIELD_SUBREPORT_OFFENDERS_FORMAT,
                    new ByteArrayInputStream(m_subReportOffenders.getCompiledFormat()));
            grid.set(FIELD_SUBREPORT_OFFENDERS_GRID, buildOffendersGrid(offenderMap.get(PERSON_TYPE_STUDENT)));

            // Other Offenders grid
            grid.set(FIELD_SUBREPORT_OFFENDERS_OTHER_FORMAT,
                    new ByteArrayInputStream(m_subReportOffendersOther.getCompiledFormat()));
            Collection<UserDefinedTableC> others = new LinkedList();
            if (offenderMap.get(PERSON_TYPE_STAFF) != null) {
                others.addAll(offenderMap.get(PERSON_TYPE_STAFF));
            }
            if (offenderMap.get(PERSON_TYPE_OTHER) != null) {
                others.addAll(offenderMap.get(PERSON_TYPE_OTHER));
            }
            grid.set(FIELD_SUBREPORT_OFFENDERS_OTHER_GRID, buildOffendersOtherGrid(others));
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_subReportOffenders = ReportUtils.getReport(IRR_OFFENDER_SUBREPORT, getBroker());
        m_subReportOffendersOther = ReportUtils.getReport(IRR_OFFENDER_OTHER_SUBREPORT, getBroker());
        m_subReportStudentVictims = ReportUtils.getReport(IRR_STUDENT_VICTIMS_SUBREPORT, getBroker());

        m_ddxById = getExtendedDataDictionaryById(USER_DEFINED_TABLES_DDX_ID);
        m_dictionary = DataDictionary.getDistrictDictionary(m_ddxById, getBroker().getPersistenceKey());
    }

    /**
     * Adds the action.
     *
     * @param actionMap Map<String,BigDecimal>
     * @param offender UserDefinedTableC
     * @param booleanAlias String
     * @param numberAlias String
     * @param keyName String
     */
    private void addAction(Map<String, BigDecimal> actionMap,
                           UserDefinedTableC offender,
                           String booleanAlias,
                           String numberAlias,
                           String keyName) {
        if (BooleanAsStringConverter.TRUE.equals(offender.getFieldValueByAlias(booleanAlias, m_dictionary))) {
            BigDecimal value = null;
            if (!StringUtils.isEmpty(numberAlias)) {
                String systemString = (String) offender.getFieldValueByAlias(numberAlias, m_dictionary);
                value = (BigDecimal) m_decimalConverter.parseSystemString(systemString);
            }
            actionMap.put(keyName, value);
        }
    }

    /**
     * Builds the IRR query.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria buildIRRQuery() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(UserDefinedTableA.COL_EXTENDED_DATA_DICTIONARY_OID,
                m_dictionary.getExtendedDictionaryOid());
        if (m_currentIncident != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentIncident.getOid());
        } else {
            DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_IRR_DATE);
            if (field == null) {
                throw new IllegalStateException(
                        "This report requires a [date] alias on " + m_dictionary.getExtendedDictionary().getId());
            }
            criteria.addGreaterOrEqualThan(field.getJavaName(), getParameter(PARAM_START_DATE));
            criteria.addLessOrEqualThan(field.getJavaName(), getParameter(PARAM_END_DATE));
            if (isSchoolContext()) {
                criteria.addEqualTo(UserDefinedTableA.COL_SCHOOL_OID, getSchool().getOid());
            }
            String queryBy = (String) getParameter(PARAM_QUERY_BY);
            String queryString = (String) getParameter(PARAM_QUERY_STRING);
            addUserCriteria(criteria, queryBy, queryString, UserDefinedTableA.class, X2BaseBean.COL_OID,
                    X2BaseBean.COL_OID);
        }
        return new QueryByCriteria(UserDefinedTableA.class, criteria);
    }

    /**
     * Fill in offender data, describes the action.
     *
     * @param offenders Collection<UserDefinedTableC>
     * @return ReportDataGrid
     */
    private ReportDataGrid buildOffendersGrid(Collection<UserDefinedTableC> offenders) {
        ReportDataGrid offendersGrid = new ReportDataGrid();
        if (offenders != null && !offenders.isEmpty()) {
            for (UserDefinedTableC offenderRecord : offenders) {
                Map<String, BigDecimal> actionMap = getActionMap(offenderRecord);
                offendersGrid.append();
                offendersGrid.set(FIELD_IIR_OFFENDER_ACTION_MAP, actionMap);
                offendersGrid.set(FIELD_OFFENDER_AGE, getIntegerString(
                        (String) offenderRecord.getFieldValueByAlias(ALIAS_OFFENDER_AGE, m_dictionary)));
                offendersGrid.set(FIELD_OFFENDER_GRADE,
                        offenderRecord.getFieldValueByAlias(ALIAS_OFFENDER_GRADE, m_dictionary));
            }
        } else {
            offendersGrid.append();
        }

        offendersGrid.beforeTop();
        return offendersGrid;
    }

    /**
     * Fill in offender data and type of action.
     *
     * @param offenderRecords Collection<UserDefinedTableC>
     * @return ReportDataGrid
     */
    private ReportDataGrid buildOffendersOtherGrid(Collection<UserDefinedTableC> offenderRecords) {
        ReportDataGrid offendersOtherGrid = new ReportDataGrid();
        for (UserDefinedTableC offenderRecord : offenderRecords) {
            offendersOtherGrid.append();
            offendersOtherGrid.set(FIELD_OFFENDER_NAME,
                    offenderRecord.getFieldValueByAlias(ALIAS_OFFENDER_NAME, m_dictionary));
            offendersOtherGrid.set(FIELD_OFFENDER_DISCIPLINARY_ACTION,
                    offenderRecord.getFieldValueByAlias(ALIAS_OFFENDER_DISCIPLINARY_ACTION, m_dictionary));
            offendersOtherGrid.set(FIELD_OFFENDER_REFERRAL_ACTION,
                    offenderRecord.getFieldValueByAlias(ALIAS_OFFENDER_REFERRAL_ACTION, m_dictionary));
            offendersOtherGrid.set(FIELD_OFFENDER_OTHER_ACTION,
                    offenderRecord.getFieldValueByAlias(ALIAS_OFFENDER_OTHER_ACTION, m_dictionary));
        }

        offendersOtherGrid.append();
        offendersOtherGrid.beforeTop();
        return offendersOtherGrid;
    }



    /**
     * Fill in victim data, who have 'student' type.
     *
     * @param iirVictimData Collection<UserDefinedTableB>
     * @return ReportDataGrid
     */
    private ReportDataGrid buildStudentVictimsGrid(Collection<UserDefinedTableB> iirVictimData) {
        ReportDataGrid studentVictimsGrid = new ReportDataGrid();
        Integer currentStudentNumber = 0;
        if (iirVictimData != null && !iirVictimData.isEmpty()) {
            for (UserDefinedTableB victim : iirVictimData) {
                studentVictimsGrid.append();
                studentVictimsGrid.set(FIELD_STUDENT_VICTIMS_CURRENT_NUM,
                        String.valueOf("#" + (++currentStudentNumber)));
                String gradeAge = victim.getFieldValueByAlias(ALIAS_VICTIM_GRADE, m_dictionary) != null
                        ? (String) victim.getFieldValueByAlias(ALIAS_VICTIM_GRADE, m_dictionary)
                        : "";
                if (!StringUtils.isBlank(gradeAge)) {
                    String ageString = (String) victim.getFieldValueByAlias(ALIAS_VICTIM_AGE, m_dictionary);
                    if (!StringUtils.isEmpty(ageString)) {
                        Number age = (Number) m_integerConverter.parseSystemString(ageString);
                        if (age.intValue() > 0) {
                            gradeAge = gradeAge.concat("," + age.intValue());
                        }
                    }
                }
                studentVictimsGrid.set(FIELD_STUDENT_VICTIMS_AGE_GRADE, gradeAge);
            }
        }

        studentVictimsGrid.append();
        studentVictimsGrid.set(FIELD_STUDENT_VICTIMS_CURRENT_NUM,
                String.valueOf("#" + (++currentStudentNumber)));

        studentVictimsGrid.beforeTop();
        return studentVictimsGrid;
    }

    /**
     * Gets the action map.
     *
     * @param offender UserDefinedTableC
     * @return Map
     */
    private Map<String, BigDecimal> getActionMap(UserDefinedTableC offender) {
        Map<String, BigDecimal> actionMap = new HashMap();
        addAction(actionMap, offender, ALIAS_OFFENDER_COUNSELING, ALIAS_OFFENDER_COUNSELING_TIME,
                ACTION_MAP_KEY_COUNSELING);
        addAction(actionMap, offender, ALIAS_OFFENDER_TEACHER_REMOVAL, ALIAS_OFFENDER_TEACHER_REMOVAL_TIME,
                ACTION_MAP_KEY_TEACHER_REMOVAL);
        addAction(actionMap, offender, ALIAS_OFFENDER_ISS, ALIAS_OFFENDER_ISS_TIME, ACTION_MAP_KEY_ISS);
        addAction(actionMap, offender, ALIAS_OFFENDER_OSS, ALIAS_OFFENDER_OSS_TIME, ACTION_MAP_KEY_OSS);
        addAction(actionMap, offender, ALIAS_OFFENDER_ALTERNATIVE_PLACEMENT, null,
                ACTION_MAP_KEY_ALTERNATIVE_PLACEMENT);
        addAction(actionMap, offender, ALIAS_OFFENDER_COMMUNITY_SERVICE, ALIAS_OFFENDER_COMMUNITY_SERVICE_TIME,
                ACTION_MAP_KEY_COMMUNITY_SERVICE);
        addAction(actionMap, offender, ALIAS_OFFENDER_JUSTICE_SYSTEM, null, ACTION_MAP_KEY_JUSTICE);
        addAction(actionMap, offender, ALIAS_OFFENDER_LAW_ENFORCEMENT, null, ACTION_MAP_KEY_LAW_ENFORCEMENT);
        return actionMap;
    }

    /**
     * Gets the boolean.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return Boolean
     */
    private Boolean getBoolean(X2BaseBean bean, String alias) {
        String systemValue = (String) bean.getFieldValueByAlias(alias, m_dictionary);
        return (Boolean) m_booleanConverter.parseSystemString(systemValue);
    }

    /**
     * Gets the extended data dictionary by id.
     *
     * @param ddxId String
     * @return Extended data dictionary
     */
    private ExtendedDataDictionary getExtendedDataDictionaryById(String ddxId) {
        ExtendedDataDictionary extendedDataDictionary = null;
        X2Criteria ddxCriteria = new X2Criteria();

        ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);

        QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
        extendedDataDictionary =
                (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
        return extendedDataDictionary;
    }

    /**
     * Gets the d field state codes.
     *
     * @param irrData UserDefinedTableA
     * @param alias String
     * @return Sets the
     */
    private Set<String> getDFieldStateCodes(UserDefinedTableA irrData, String alias) {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null && field.hasReferenceTable()) {
            String biases = (String) irrData.getFieldValueByAlias(alias, m_dictionary);
            List<String> codes = null;
            if (!StringUtils.isEmpty(biases)) {
                codes = Arrays.asList(biases.split("\\s*,\\s*"));
            }
            return codes == null ? new HashSet() : getStateCodes(codes, field.getReferenceTable());
        }
        throw new IllegalStateException(
                "Alias " + alias + " must be defined and have a reference table");
    }

    /**
     * Gets the disciplinary action count.
     *
     * @param offenders Collection<UserDefinedTableC>
     * @return Map
     */
    private Map<String, Integer> getDisciplinaryActionCount(Collection<UserDefinedTableC> offenders) {
        Map<String, Integer> map = new HashMap();
        if (offenders != null && !offenders.isEmpty()) {
            for (UserDefinedTableC offender : offenders) {
                String disabilities =
                        (String) offender.getFieldValueByAlias(ALIAS_OFFENDER_DISABILITIES, m_dictionary);
                String prefix = BooleanAsStringConverter.TRUE.equals(disabilities) ? "B" : "A";
                String action = getStateCode(offender, ALIAS_OFFENDER_DISCIPLINARY_ACTION);
                if (!StringUtils.isEmpty(action)) {
                    String key = prefix + action;
                    Integer count = map.get(key);
                    if (count == null) {
                        map.put(key, Integer.valueOf(1));
                    } else {
                        map.put(key, Integer.valueOf(count.intValue() + 1));
                    }
                }
            }
        }
        return map;
    }

    /**
     * Gets the incident categories.
     *
     * @param irrData UserDefinedTableA
     * @return Sets the
     */
    private Set<String> getIncidentCategories(UserDefinedTableA irrData) {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_IRR_CATEGORY);
        if (field != null && field.hasReferenceTable()) {
            List<String> codes = new LinkedList();
            String primaryCategory = (String) irrData.getFieldValueByBeanPath(field.getJavaName());
            codes.add(primaryCategory);
            String secondaryCategories =
                    (String) irrData.getFieldValueByAlias(ALIAS_IRR_SECONDARY_CATEGORIES, m_dictionary);
            if (!StringUtils.isEmpty(secondaryCategories)) {
                codes.addAll(Arrays.asList(secondaryCategories.split("\\s*,\\s*")));
            }
            return getStateCodes(codes, field.getReferenceTable());
        }
        throw new IllegalStateException(
                "Alias " + ALIAS_IRR_CATEGORY + " must be defined and have a reference table");
    }

    /**
     * Gets the integer string.
     *
     * @param items Collection
     * @return String
     */
    private String getIntegerString(Collection items) {
        String result = "";
        if (items != null && !items.isEmpty()) {
            result = String.valueOf(items.size());
        }
        return result;
    }

    /**
     * Gets the integer string.
     *
     * @param systemValue String
     * @return String
     */
    private String getIntegerString(String systemValue) {
        String result = "";
        if (!StringUtils.isEmpty(systemValue)) {
            Number value = (Number) m_integerConverter.parseSystemString(systemValue);
            if (value != null && value.intValue() != 0) {
                result = String.valueOf(value.intValue());
            }
        }
        return result;
    }

    /**
     * Gets the offenders by type.
     *
     * @param irrData UserDefinedTableA
     * @return Map
     */
    private Map<String, Collection<UserDefinedTableC>> getOffendersByType(UserDefinedTableA irrData) {
        Map<String, Collection<UserDefinedTableC>> offenderMap = new HashMap();
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_OFFENDER_TYPE);
        if (field != null && field.hasReferenceTable()) {
            for (UserDefinedTableC offender : irrData.getUserDefinedRecordsC()) {
                String stateCode = getStateCode(offender, ALIAS_OFFENDER_TYPE);
                if (!StringUtils.isEmpty(stateCode)) {
                    Collection<UserDefinedTableC> offenders = offenderMap.get(stateCode);
                    if (offenders == null) {
                        offenders = new LinkedList();
                        offenderMap.put(stateCode, offenders);
                    }
                    updateOffenderAttributes(offender);
                    offenders.add(offender);
                }
            }
            return offenderMap;
        }
        throw new IllegalStateException("The alias " + ALIAS_OFFENDER_TYPE
                + " must have a reference table with state codes {Student, Staff, Other}");
    }

    /**
     * Gets the state code.
     *
     * @param bean X2BaseBean
     * @param alias String
     * @return String
     */
    private String getStateCode(X2BaseBean bean, String alias) {
        String result = null;
        String value = (String) bean.getFieldValueByAlias(alias, m_dictionary);
        if (!StringUtils.isEmpty(value)) {
            DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null && field.hasReferenceTable()) {
                Map<String, ReferenceCode> rcdMap = field.getReferenceTable().getCodeMap();
                ReferenceCode rcd = rcdMap.get(value);
                if (rcd != null) {
                    result = rcd.getStateCode();
                }
            }
        }
        return result;
    }

    /**
     * Gets the state codes.
     *
     * @param codes Collection<String>
     * @param refTable ReferenceTable
     * @return Sets the
     */
    private Set<String> getStateCodes(Collection<String> codes, ReferenceTable refTable) {
        Set<String> stateCodes = new HashSet();
        Map<String, ReferenceCode> codeMap = refTable.getCodeMap(getBroker());
        for (String code : codes) {
            ReferenceCode rcd = codeMap.get(code);
            if (rcd != null && !StringUtils.isEmpty(rcd.getStateCode())) {
                stateCodes.add(rcd.getStateCode());
            }
        }
        return stateCodes;
    }

    /**
     * Gets the victims by type.
     *
     * @param irrData UserDefinedTableA
     * @return Map
     */
    private Map<String, Collection<UserDefinedTableB>> getVictimsByType(UserDefinedTableA irrData) {
        Map<String, Collection<UserDefinedTableB>> victimMap = new HashMap();
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_VICTIM_TYPE);
        if (field != null && field.hasReferenceTable()) {
            for (UserDefinedTableB victim : irrData.getUserDefinedRecordsB()) {
                String stateCode = getStateCode(victim, ALIAS_VICTIM_TYPE);
                if (!StringUtils.isEmpty(stateCode)) {
                    Collection<UserDefinedTableB> victims = victimMap.get(stateCode);
                    if (victims == null) {
                        victims = new LinkedList();
                        victimMap.put(stateCode, victims);
                    }
                    updateVictimAttributes(victim);
                    victims.add(victim);
                }
            }
            return victimMap;
        }
        throw new IllegalStateException("The alias " + ALIAS_VICTIM_TYPE
                + " must have a reference table with state codes {Student, Staff, Other}");
    }

    /**
     * Gets the weapon counts.
     *
     * @param offenders Collection<UserDefinedTableC>
     * @return Map
     */
    private Map<String, Integer> getWeaponCounts(Collection<UserDefinedTableC> offenders) {
        Map<String, Integer> map = new HashMap();
        if (offenders != null && !offenders.isEmpty()) {
            for (UserDefinedTableC offender : offenders) {
                String disabilities =
                        (String) offender.getFieldValueByAlias(ALIAS_OFFENDER_DISABILITIES, m_dictionary);
                String prefix = BooleanAsStringConverter.TRUE.equals(disabilities) ? "B" : "A";
                String weapon = getStateCode(offender, ALIAS_OFFENDER_WEAPON);
                if (!StringUtils.isEmpty(weapon)) {
                    String key = prefix + weapon;
                    String totalKey = prefix + "E";
                    Integer count = map.get(key);
                    if (count == null) {
                        map.put(key, Integer.valueOf(1));
                    } else {
                        map.put(key, Integer.valueOf(count.intValue() + 1));
                    }
                    count = map.get(totalKey);
                    if (count == null) {
                        map.put(totalKey, Integer.valueOf(1));
                    } else {
                        map.put(totalKey, Integer.valueOf(count.intValue() + 1));
                    }
                }
            }
        }
        return map;
    }

    /**
     * Update offender attributes.
     *
     * @param offender UserDefinedTableC
     */
    private void updateOffenderAttributes(UserDefinedTableC offender) {
        String victimName = (String) offender.getFieldValueByAlias(ALIAS_OFFENDER_NAME, m_dictionary);
        if (StringUtils.isEmpty(victimName)) {
            if (offender.getStaff() != null) {
                offender.setFieldValueByAlias(ALIAS_OFFENDER_NAME, offender.getStaff().getNameView(), m_dictionary);
            } else if (offender.getStudent() != null) {
                offender.setFieldValueByAlias(ALIAS_OFFENDER_NAME, offender.getStudent().getNameView(),
                        m_dictionary);
            }
        }
        if (offender.getStudent() != null) {
            String grade = (String) offender.getFieldValueByAlias(ALIAS_OFFENDER_GRADE, m_dictionary);
            if (StringUtils.isEmpty(grade)) {
                offender.setFieldValueByAlias(ALIAS_OFFENDER_GRADE, offender.getStudent().getGradeLevel(),
                        m_dictionary);
            }
            String age = (String) offender.getFieldValueByAlias(ALIAS_OFFENDER_AGE, m_dictionary);
            if (StringUtils.isEmpty(age) || ((Number) m_integerConverter.parseSystemString(age)).intValue() == 0) {
                String studentAge =
                        m_integerConverter.getSystemString(Integer.valueOf(offender.getStudent().getPerson().getAge()));
                offender.setFieldValueByAlias(ALIAS_OFFENDER_AGE, studentAge, m_dictionary);
            }
        }
        if (offender.isDirty()) {
            getBroker().saveBeanForced(offender);
        }
    }

    /**
     * Update victim attributes.
     *
     * @param victim UserDefinedTableB
     */
    private void updateVictimAttributes(UserDefinedTableB victim) {
        String victimName = (String) victim.getFieldValueByAlias(ALIAS_VICTIM_NAME, m_dictionary);
        if (StringUtils.isEmpty(victimName)) {
            if (victim.getStaff() != null) {
                victim.setFieldValueByAlias(ALIAS_VICTIM_NAME, victim.getStaff().getNameView(), m_dictionary);
            } else if (victim.getStudent() != null) {
                victim.setFieldValueByAlias(ALIAS_VICTIM_NAME, victim.getStudent().getNameView(), m_dictionary);
            }
        }
        if (victim.getStudent() != null) {
            String grade = (String) victim.getFieldValueByAlias(ALIAS_VICTIM_GRADE, m_dictionary);
            if (StringUtils.isEmpty(grade)) {
                victim.setFieldValueByAlias(ALIAS_VICTIM_GRADE, victim.getStudent().getGradeLevel(), m_dictionary);
            }
            String age = (String) victim.getFieldValueByAlias(ALIAS_VICTIM_AGE, m_dictionary);
            if (StringUtils.isEmpty(age) || ((Number) m_integerConverter.parseSystemString(age)).intValue() == 0) {
                String studentAge =
                        m_integerConverter.getSystemString(Integer.valueOf(victim.getStudent().getPerson().getAge()));
                victim.setFieldValueByAlias(ALIAS_VICTIM_AGE, studentAge, m_dictionary);
            }
        }
        if (victim.isDirty()) {
            getBroker().saveBeanForced(victim);
        }
    }
}
