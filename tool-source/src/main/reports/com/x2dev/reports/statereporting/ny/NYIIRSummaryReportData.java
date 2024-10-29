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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisSchool;
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
public class NYIIRSummaryReportData extends ReportJavaSourceNet {

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

    private static final String FIELD_SKL_NAME = "schoolName";
    private static final String FIELD_SKL_ID = "sklId";
    private static final String FIELD_TOTAL_INC = "total";

    /**
     * Fields for BIAS area.
     */
    private static final String FIELD_BIAS_TOTAL = "biasTotal";
    private static final String FIELD_BIAS_RACE = "biasRace";
    private static final String FIELD_BIAS_ETHINIC_GROUP = "biasEthnic";
    private static final String FIELD_BIAS_NAT_ORIGIN = "biasNatOrigin";
    private static final String FIELD_BIAS_COLOR = "biasColor";
    private static final String FIELD_BIAS_RELIGION = "biasReligion";
    private static final String FIELD_BIAS_REL_PRACT = "biasRelPractices";
    private static final String FIELD_BIAS_DISABILITY = "biasDisability";
    private static final String FIELD_BIAS_GENDER = "biasGender";
    private static final String FIELD_BIAS_SEX_OREINT = "biasSexOrient";
    private static final String FIELD_BIAS_SEX = "biasSex";
    private static final String FIELD_BIAS_WEIGNT = "biasWeight";
    private static final String FIELD_BIAS_OTHER = "biasOther";

    /**
     * Fields for GANG area.
     */
    private static final String FIELD_GANG_TOTAL = "gangTotal";
    private static final String FIELD_GANG_TOTAL_NOT_WEAPON = "gangTotalNotWeapon";
    private static final String FIELD_GANG_TOTAL_WEAPON = "gangTotalWeapon";
    private static final String FIELD_GANG_FIREARMS = "gangFirearms";
    private static final String FIELD_GANG_KNIVES = "gangKnives";
    private static final String FIELD_GANG_OTHER = "gangOther";
    private static final String FIELD_GANG_ALCOHOL = "gangAlcohol";
    private static final String FIELD_GANG_DRUGS = "gangDrugs";

    /**
     * Fields for LOCATION area.
     */
    private static final String FIELD_LOC_ON_SKL_PROP = "locOnSchoolProp";
    private static final String FIELD_LOC_AT_SKL_GROUNDS = "locAtSchoolGrounds";
    private static final String FIELD_LOC_OFF_SKL_PROP = "locOffSchoolProp";
    private static final String FIELD_LOC_STR = "locTransportation";

    /**
     * Fields for TIME area.
     */
    private static final String FIELD_TIME_DURING = "timeDuring";
    private static final String FIELD_TIME_BEFORE_AFTER = "timeBeforeAfter";

    /**
     * Fields for TARGETS area.
     */
    private static final String FIELD_TARG_STD = "targetsStd";
    private static final String FIELD_TARG_STF = "targetsStf";
    private static final String FIELD_TARG_OTHER = "targetsOther";

    /**
     * Fields for OFFENDERS area.
     */
    private static final String FIELD_OFFENDERS_STD = "offendersStd";
    private static final String FIELD_OFFENDERS_STF = "offendersStf";
    private static final String FIELD_OFFENDERS_OTHER = "offendersOther";

    /**
     * Fields for DISCIPLINARY area.
     */
    private static final String FIELD_DISCIPL_CONSULT = "disciplCounseling";
    private static final String FIELD_DISCIPL_TEACHER_REMOVAL = "disciplTeacherRemove";
    private static final String FIELD_DISCIPL_IN_SKL_SUSP = "disciplInSklSuspension";
    private static final String FIELD_DISCIPL_OUT_SKL_SUSP = "disciplOutSklSuspension";
    private static final String FIELD_DISCIPL_INV_TRANSFER = "disciplInvTransfer";
    private static final String FIELD_DISCIPL_COMM_SERVICE = "disciplCommService";
    private static final String FIELD_DISCIPL_JUVENILE_JUSTICE = "disciplJuvJustice";
    private static final String FIELD_DISCIPL_LAW_ENFORCEMENT = "disciplLawEnforcement";

    private static final String REPORT_FIELD_DATA_MAP = "dataMap";

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
    private Map<String, Map<String, DataCounts>> m_ReportData = new HashMap<>();

    public class DataCounts {
        int m_total = 0;

        int m_biasTotal = 0;
        int m_biasRace = 0;
        int m_biasEthGroup = 0;
        int m_biasNationalOrigin = 0;
        int m_biasColor = 0;
        int m_biasReligion = 0;
        int m_biasRelPracticies = 0;
        int m_biasDisability = 0;
        int m_biasGender = 0;
        int m_biasSexOrient = 0;
        int m_biasSex = 0;
        int m_biasWeight = 0;
        int m_biasOther = 0;

        int m_totalGangOrGroupRelated = 0;
        int m_totalNotWeapon = 0;
        int m_totalWeapon = 0;
        int m_gangFirearms = 0;
        int m_gangKnives = 0;
        int m_gangOther = 0;
        int m_gangAlcohol = 0;
        int m_gangDrugs = 0;

        int m_offenderStudents = 0;
        int m_offenderStaffs = 0;
        int m_offenderOthers;

        int m_victimStudents = 0;
        int m_victimStaffs = 0;
        int m_victimOthers = 0;

        int m_locOnSklProperty = 0;
        int m_locAtSklFunctionGrounds = 0;
        int m_locOffSklProperty = 0;
        int m_locOnSklTransport = 0;
        int m_timeDuringSklHours = 0;
        int m_timeBeforeAfterHours = 0;

        int m_consOrTreatmentPgm = 0;
        int m_teacherRemoval = 0;
        int m_inSklSusp = 0;
        int m_outSklSusp = 0;
        int m_involuntary = 0;
        int m_communityService = 0;
        int m_juvenileJustice = 0;
        int m_lawEnforcement = 0;

        protected void incrementBias(String stateCodeBias) {
            m_total += 1;
            m_biasTotal += 1;
            switch (stateCodeBias) {
                case "C":
                    m_biasRace += 1;
                    break;
                case "D":
                    m_biasEthGroup += 1;
                    break;
                case "E":
                    m_biasNationalOrigin += 1;
                    break;
                case "F":
                    m_biasColor += 1;
                    break;
                case "G":
                    m_biasReligion += 1;
                    break;
                case "H":
                    m_biasRelPracticies += 1;
                    break;
                case "I":
                    m_biasDisability += 1;
                    break;
                case "J":
                    m_biasGender += 1;
                    break;
                case "K":
                    m_biasSexOrient += 1;
                    break;
                case "L":
                    m_biasSex += 1;
                    break;
                case "M":
                    m_biasWeight += 1;
                    break;
                case "N":
                    m_biasOther += 1;
                    break;
                default:
                    break;
            }
        }

        protected void incrementLocationTime(String stateCodeLocTime) {
            switch (stateCodeLocTime) {
                case "U":
                    m_locAtSklFunctionGrounds += 1;
                    break;
                case "Y":
                    m_timeBeforeAfterHours += 1;
                    break;
                case "X":
                    m_timeDuringSklHours += 1;
                    break;
                case "V":
                    m_locOffSklProperty += 1;
                    break;
                case "T":
                    m_locOnSklProperty += 1;
                    break;
                case "H":
                    m_locOnSklTransport += 1;
                    break;
                default:
                    break;
            }
        }

        protected void incrementGangOrGroupRelated(UserDefinedTableA irrData) {
            if (irrData.getFieldValueByAlias(ALIAS_IRR_KNIVES, m_dictionary) != null) {
                int knives = ((Integer) irrData.getFieldValueByAlias(ALIAS_IRR_KNIVES, m_dictionary)).intValue();
                m_total += knives;
                m_totalGangOrGroupRelated += knives;
                m_totalWeapon += knives;
                m_gangKnives += knives;
            }
            if (irrData.getFieldValueByAlias(ALIAS_IRR_FIREARMS, m_dictionary) != null) {
                int firearms = ((Integer) irrData.getFieldValueByAlias(ALIAS_IRR_FIREARMS, m_dictionary)).intValue();
                m_total += firearms;
                m_totalGangOrGroupRelated += firearms;
                m_totalWeapon += firearms;
                m_gangFirearms += firearms;
            }
            if (irrData.getFieldValueByAlias(ALIAS_IRR_OTHER_WEAPONS, m_dictionary) != null) {
                int otherWeapons =
                        ((Integer) irrData.getFieldValueByAlias(ALIAS_IRR_OTHER_WEAPONS, m_dictionary)).intValue();
                m_total += otherWeapons;
                m_totalGangOrGroupRelated += otherWeapons;
                m_totalWeapon += otherWeapons;
                m_gangOther += otherWeapons;
            }
            if (BooleanAsStringConverter.TRUE
                    .equals(irrData.getFieldValueByAlias(ALIAS_IRR_INVOLVING_ALCOHOL, m_dictionary))) {
                m_total += 1;
                m_totalGangOrGroupRelated += 1;
                m_totalNotWeapon += 1;
                m_gangAlcohol += 1;
            }
            if (BooleanAsStringConverter.TRUE
                    .equals(irrData.getFieldValueByAlias(ALIAS_IRR_INVOLVING_DRUGS, m_dictionary))) {
                m_total += 1;
                m_totalGangOrGroupRelated += 1;
                m_totalNotWeapon += 1;
                m_gangDrugs += 1;
            }
        }

        protected void incrementOffenders(UserDefinedTableA irrData) {
            Map<String, Collection<UserDefinedTableC>> offenderMap = getOffendersByType(irrData);
            if (offenderMap.get(PERSON_TYPE_STUDENT) != null) {
                m_offenderStudents += offenderMap.get(PERSON_TYPE_STUDENT).size();
            }
            if (offenderMap.get(PERSON_TYPE_STAFF) != null) {
                m_offenderStaffs += offenderMap.get(PERSON_TYPE_STAFF).size();
            }
            if (offenderMap.get(PERSON_TYPE_OTHER) != null) {
                m_offenderOthers += offenderMap.get(PERSON_TYPE_OTHER).size();
            }
        }

        protected void incrementVictims(UserDefinedTableA irrData) {
            Map<String, Collection<UserDefinedTableB>> victimMap = getVictimsByType(irrData);
            if (victimMap.get(PERSON_TYPE_STUDENT) != null) {
                m_victimStudents += victimMap.get(PERSON_TYPE_STUDENT).size();
            }
            if (victimMap.get(PERSON_TYPE_STAFF) != null) {
                m_victimStaffs += victimMap.get(PERSON_TYPE_STAFF).size();
            }
            if (victimMap.get(PERSON_TYPE_OTHER) != null) {
                m_victimOthers += victimMap.get(PERSON_TYPE_OTHER).size();
            }
        }

        protected void incrementDsciplinary(UserDefinedTableA irrData) {
            Collection<UserDefinedTableC> offenders = irrData.getUserDefinedRecordsC();
            for (UserDefinedTableC offender : offenders) {
                m_consOrTreatmentPgm += get(offender, ALIAS_OFFENDER_COUNSELING, ALIAS_OFFENDER_COUNSELING_TIME);
                m_teacherRemoval += get(offender, ALIAS_OFFENDER_TEACHER_REMOVAL, ALIAS_OFFENDER_TEACHER_REMOVAL_TIME);
                m_inSklSusp += get(offender, ALIAS_OFFENDER_ISS, ALIAS_OFFENDER_ISS_TIME);
                m_outSklSusp += get(offender, ALIAS_OFFENDER_OSS, ALIAS_OFFENDER_OSS_TIME);
                m_involuntary += get(offender, ALIAS_OFFENDER_ALTERNATIVE_PLACEMENT, null);
                m_communityService +=
                        get(offender, ALIAS_OFFENDER_COMMUNITY_SERVICE, ALIAS_OFFENDER_COMMUNITY_SERVICE_TIME);
                m_juvenileJustice += get(offender, ALIAS_OFFENDER_JUSTICE_SYSTEM, null);
                m_lawEnforcement += get(offender, ALIAS_OFFENDER_LAW_ENFORCEMENT, null);
            }
        }
    }

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
        countDataByCategory();
        for (String sklOid : m_ReportData.keySet()) {
            SisSchool school = getBroker().getBeanByOid(SisSchool.class, sklOid);
            Map<String, Integer> mapToPutToGrid = new HashMap<>();
            Map<String, DataCounts> counts = m_ReportData.get(sklOid);
            for (String category : counts.keySet()) {
                DataCounts data = counts.get(category);
                mapToPutToGrid.put(FIELD_TOTAL_INC + "-" + category, Integer.valueOf(data.m_total));
                mapToPutToGrid.put(FIELD_BIAS_TOTAL + "-" + category, Integer.valueOf(data.m_biasTotal));
                mapToPutToGrid.put(FIELD_BIAS_RACE + "-" + category, Integer.valueOf(data.m_biasRace));
                mapToPutToGrid.put(FIELD_BIAS_ETHINIC_GROUP + "-" + category, Integer.valueOf(data.m_biasEthGroup));
                mapToPutToGrid.put(FIELD_BIAS_NAT_ORIGIN + "-" + category, Integer.valueOf(data.m_biasNationalOrigin));
                mapToPutToGrid.put(FIELD_BIAS_COLOR + "-" + category, Integer.valueOf(data.m_biasColor));
                mapToPutToGrid.put(FIELD_BIAS_RELIGION + "-" + category, Integer.valueOf(data.m_biasReligion));
                mapToPutToGrid.put(FIELD_BIAS_REL_PRACT + "-" + category, Integer.valueOf(data.m_biasRelPracticies));
                mapToPutToGrid.put(FIELD_BIAS_DISABILITY + "-" + category, Integer.valueOf(data.m_biasDisability));
                mapToPutToGrid.put(FIELD_BIAS_GENDER + "-" + category, Integer.valueOf(data.m_biasGender));
                mapToPutToGrid.put(FIELD_BIAS_SEX_OREINT + "-" + category, Integer.valueOf(data.m_biasSexOrient));
                mapToPutToGrid.put(FIELD_BIAS_SEX + "-" + category, Integer.valueOf(data.m_biasSex));
                mapToPutToGrid.put(FIELD_BIAS_WEIGNT + "-" + category, Integer.valueOf(data.m_biasWeight));
                mapToPutToGrid.put(FIELD_BIAS_OTHER + "-" + category, Integer.valueOf(data.m_biasOther));

                mapToPutToGrid.put(FIELD_GANG_TOTAL + "-" + category, Integer.valueOf(data.m_totalGangOrGroupRelated));
                mapToPutToGrid.put(FIELD_GANG_TOTAL_NOT_WEAPON + "-" + category,
                        Integer.valueOf(data.m_totalNotWeapon));
                mapToPutToGrid.put(FIELD_GANG_TOTAL_WEAPON + "-" + category, Integer.valueOf(data.m_totalWeapon));
                mapToPutToGrid.put(FIELD_GANG_FIREARMS + "-" + category, Integer.valueOf(data.m_gangFirearms));
                mapToPutToGrid.put(FIELD_GANG_KNIVES + "-" + category, Integer.valueOf(data.m_gangKnives));
                mapToPutToGrid.put(FIELD_GANG_OTHER + "-" + category, Integer.valueOf(data.m_gangOther));
                mapToPutToGrid.put(FIELD_GANG_ALCOHOL + "-" + category, Integer.valueOf(data.m_gangAlcohol));
                mapToPutToGrid.put(FIELD_GANG_DRUGS + "-" + category, Integer.valueOf(data.m_gangDrugs));

                mapToPutToGrid.put(FIELD_LOC_ON_SKL_PROP + "-" + category, Integer.valueOf(data.m_locOnSklProperty));
                mapToPutToGrid.put(FIELD_LOC_AT_SKL_GROUNDS + "-" + category,
                        Integer.valueOf(data.m_locAtSklFunctionGrounds));
                mapToPutToGrid.put(FIELD_LOC_OFF_SKL_PROP + "-" + category, Integer.valueOf(data.m_locOffSklProperty));
                mapToPutToGrid.put(FIELD_LOC_STR + "-" + category, Integer.valueOf(data.m_locOnSklTransport));

                mapToPutToGrid.put(FIELD_TIME_DURING + "-" + category, Integer.valueOf(data.m_timeDuringSklHours));
                mapToPutToGrid.put(FIELD_TIME_BEFORE_AFTER + "-" + category,
                        Integer.valueOf(data.m_timeBeforeAfterHours));

                mapToPutToGrid.put(FIELD_TARG_STD + "-" + category, Integer.valueOf(data.m_victimStudents));
                mapToPutToGrid.put(FIELD_TARG_STF + "-" + category, Integer.valueOf(data.m_victimStaffs));
                mapToPutToGrid.put(FIELD_TARG_OTHER + "-" + category, Integer.valueOf(data.m_victimOthers));

                mapToPutToGrid.put(FIELD_OFFENDERS_STD + "-" + category, Integer.valueOf(data.m_offenderStudents));
                mapToPutToGrid.put(FIELD_OFFENDERS_STF + "-" + category, Integer.valueOf(data.m_offenderStaffs));
                mapToPutToGrid.put(FIELD_OFFENDERS_OTHER + "-" + category, Integer.valueOf(data.m_offenderOthers));

                mapToPutToGrid.put(FIELD_DISCIPL_CONSULT + "-" + category, Integer.valueOf(data.m_consOrTreatmentPgm));
                mapToPutToGrid.put(FIELD_DISCIPL_TEACHER_REMOVAL + "-" + category,
                        Integer.valueOf(data.m_teacherRemoval));
                mapToPutToGrid.put(FIELD_DISCIPL_IN_SKL_SUSP + "-" + category, Integer.valueOf(data.m_inSklSusp));
                mapToPutToGrid.put(FIELD_DISCIPL_OUT_SKL_SUSP + "-" + category, Integer.valueOf(data.m_outSklSusp));
                mapToPutToGrid.put(FIELD_DISCIPL_INV_TRANSFER + "-" + category, Integer.valueOf(data.m_involuntary));
                mapToPutToGrid.put(FIELD_DISCIPL_COMM_SERVICE + "-" + category,
                        Integer.valueOf(data.m_communityService));
                mapToPutToGrid.put(FIELD_DISCIPL_JUVENILE_JUSTICE + "-" + category,
                        Integer.valueOf(data.m_juvenileJustice));
                mapToPutToGrid.put(FIELD_DISCIPL_LAW_ENFORCEMENT + "-" + category,
                        Integer.valueOf(data.m_lawEnforcement));
            }
            grid.append();
            grid.set(FIELD_SKL_NAME, school.getName());
            grid.set(FIELD_SKL_ID, school.getSchoolId());
            grid.set(REPORT_FIELD_DATA_MAP, mapToPutToGrid);
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
        m_ddxById = getExtendedDataDictionaryById(USER_DEFINED_TABLES_DDX_ID);
        m_dictionary = DataDictionary.getDistrictDictionary(m_ddxById, getBroker().getPersistenceKey());
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
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_IRR_DATE);
        if (field == null) {
            throw new IllegalStateException(
                    "This report requires a [date] alias on " + m_dictionary.getExtendedDictionary().getId());
        }
        criteria.addGreaterOrEqualThan(field.getJavaName(), getCurrentContext().getStartDate());
        criteria.addLessOrEqualThan(field.getJavaName(), getCurrentContext().getEndDate());
        if (isSchoolContext()) {
            criteria.addEqualTo(UserDefinedTableA.COL_SCHOOL_OID, getSchool().getOid());
        }
        return new QueryByCriteria(UserDefinedTableA.class, criteria);
    }

    private void countDataByCategory() {
        Map<String, List<UserDefinedTableA>> irrData =
                getBroker().getGroupedCollectionByQuery(buildIRRQuery(), UserDefinedTableA.COL_SCHOOL_OID, 1024);
        if (irrData != null) {
            for (String sklOid : irrData.keySet()) {
                List<UserDefinedTableA> irrList = irrData.get(sklOid);
                Map<String, DataCounts> dataForSkl = new HashMap<>();
                m_ReportData.put(sklOid, dataForSkl);
                for (UserDefinedTableA irr : irrList) {
                    Set<String> categoriesStateCodes = getIncidentCategories(irr);
                    for (String category : categoriesStateCodes) {
                        DataCounts counts = dataForSkl.get(category);
                        if (counts == null) {
                            counts = new DataCounts();
                            dataForSkl.put(category, counts);
                        }
                        Set<String> biasCheckboxes = getDFieldStateCodes(irr, ALIAS_IRR_BIAS);
                        if (biasCheckboxes != null && !biasCheckboxes.isEmpty()) {
                            for (String biasCheckbox : biasCheckboxes) {
                                counts.incrementBias(biasCheckbox);
                            }
                        }
                        if (BooleanAsStringConverter.TRUE
                                .equals(irr.getFieldValueByAlias(ALIAS_IRR_GANG, m_dictionary))) {
                            counts.incrementGangOrGroupRelated(irr);
                        }
                        counts.incrementOffenders(irr);
                        counts.incrementVictims(irr);
                        counts.incrementDsciplinary(irr);
                        Set<String> locationTimeCheckboxes = getDFieldStateCodes(irr, ALIAS_IRR_LOCATION_TIME);
                        if (locationTimeCheckboxes != null && !locationTimeCheckboxes.isEmpty()) {
                            for (String locationTimeCheckbox : locationTimeCheckboxes) {
                                counts.incrementLocationTime(locationTimeCheckbox);
                            }
                        }
                    }
                }
            }
        }
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
     * Return number of disciplinary.
     *
     * @param offender UserDefinedTableC
     * @param booleanAlias String
     * @param numberAlias String
     */
    private int get(UserDefinedTableC offender,
                    String booleanAlias,
                    String numberAlias) {
        BigDecimal value = null;
        if (BooleanAsStringConverter.TRUE.equals(offender.getFieldValueByAlias(booleanAlias, m_dictionary))) {
            if (!StringUtils.isEmpty(numberAlias)) {
                String systemString = (String) offender.getFieldValueByAlias(numberAlias, m_dictionary);
                value = (BigDecimal) m_decimalConverter.parseSystemString(systemString);
            }
        }
        return value != null ? value.intValue() : 0;
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
