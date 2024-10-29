/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Georgia state report for GMAP Assessment export.
 * This class implements the data export for GMAP Assessment export.
 *
 * @author X2 Development Corporation
 */
public class GAGMAPAsmData extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the GA GMAP Asm Student export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class GAGMAPAsmEntity extends StateReportEntity {

        private List<StudentRow> m_rows = new ArrayList<>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public GAGMAPAsmEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent student = (SisStudent) bean;
            GAGMAPAsmData gmapData = (GAGMAPAsmData) data;
            StudentRow elaRow = gmapData.new StudentRow(student, "ELA");
            StudentRow mathRow = gmapData.new StudentRow(student, "MATH");
            m_rows.add(elaRow);
            m_rows.add(mathRow);
            setRowCount(m_rows.size());
        }

        /**
         * Gets the student data.
         *
         * @return Student row
         */
        public StudentRow getStudentData() {
            return m_rows.get(getCurrentRow());
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [ID: " + student.getLocalId() +
                    " [GTID: " + student.getStateId() +
                    "]";
            return name;
        }
    }


    /*
     * Report values and aliases.
     */
    private static final String ALIAS_SKL_DOE_CODE = "DOE School";
    private static final String ALIAS_STD_DOE_ELL = "DOE Meals";
    private static final String ALIAS_STD_DOE_MEALS = "DOE Meals";
    private static final String ALIAS_STD_DOE_MIGRANT = "DOE Migrant";
    private static final String ALIAS_STD_PRIM_EXP = "DOE Primary Exceptionality";
    private static final String ALIAS_STD_SKL_OVERRIDE = "DOE Override School Code";

    /*
     * Other internal constants
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    protected static final String STRING_NO = "N";
    protected static final String STRING_ONE = "1";
    protected static final String STRING_YES = "Y";
    protected static final String STRING_ZERO = "0";

    /*
     * Instance variables.
     */
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected String m_sklFieldCode;
    protected String m_stdActiveCode;
    protected String m_stdFieldDoeELL;
    protected String m_stdFieldDoeMeals;
    protected String m_stdFieldDoeMigrant;
    protected String m_stdFieldPrimExceptionality;
    protected String m_stdFieldSklOverride;
    protected Map<String, ReferenceCode> m_stdFieldSklOverrideCodes = new HashMap<>();
    protected StudentHistoryHelper m_stdHelper;

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In GA, this is:
     * "W" - White
     * "B" - Black
     * "S" - Asian
     * "I" - Indian/Native/Alaskan
     * "P" - Pacific
     *
     * Ex: "SNS" searches for the Asian code, returns "S" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        protected static final String CALC_ID = "GMAP-RACE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);

            String raceCode = falseChar;
            SisStudent student = ((GAGMAPAsmEntity) entity).getStudentData().getStudent();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());
            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(requestCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = trueChar;
                        break;
                    }
                }
            }

            return raceCode;
        }
    }

    /**
     * The Class RetrieveSRCFields.
     */
    protected class RetrieveSRCFields implements FieldRetriever {

        protected static final String CALC_ID = "GMAP-SRC";

        protected static final String CALC_PARAM_DISABILITIES = "DISABILITIES";
        protected static final String CALC_PARAM_ECON = "ECON_DISADVANTAGE";
        protected static final String CALC_PARAM_SRC_01 = "SRC_01";
        protected static final String CALC_PARAM_SRC_02 = "SRC_02";
        protected static final String CALC_PARAM_SRC_03 = "SRC_03";
        protected static final String CALC_PARAM_SRC_04 = "SRC_04";
        protected static final String CALC_PARAM_SRC_05 = "SRC_05";
        protected static final String CALC_PARAM_SRC_06 = "SRC_06";
        protected static final String CALC_PARAM_SRC_07 = "SRC_07";
        protected static final String CALC_PARAM_SRC_08 = "SRC_08";
        protected static final String CALC_PARAM_SRC_09 = "SRC_09";
        protected static final String CALC_PARAM_SRC_10 = "SRC_10";
        protected static final String CALC_PARAM_SRC_11 = "SRC_11";
        protected static final String CALC_PARAM_SRC_12 = "SRC_12";
        protected static final String CALC_PARAM_SRC_13 = "SRC_13";
        protected static final String CALC_PARAM_SRC_14 = "SRC_14";
        protected static final String CALC_PARAM_SRC_15 = "SRC_15";
        protected static final String CALC_PARAM_SRC_18 = "SRC_18";
        protected static final String CALC_PARAM_SRC_19 = "SRC_19";
        protected static final String CALC_PARAM_SUBJ_CODE = "SUBJ_CODE";


        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            GAGMAPAsmData gaData = (GAGMAPAsmData) data;
            SisStudent student = ((GAGMAPAsmEntity) entity).getStudentData().getStudent();
            String value = null;
            if (CALC_PARAM_ECON.equals(param)) {
                value = STRING_NO;
                String rowValue = (String) student.getFieldValueByBeanPath(gaData.m_stdFieldDoeMeals);
                if (!StringUtils.isEmpty(rowValue)) {
                    String rowValueState =
                            gaData.lookupStateValue(student.getClass(), gaData.m_stdFieldDoeMeals, rowValue);
                    if (!StringUtils.isEmpty(rowValueState)
                            && ("F".equalsIgnoreCase(rowValueState) || "R".equalsIgnoreCase(rowValueState))) {
                        value = STRING_YES;
                    }
                }
            } else if (CALC_PARAM_DISABILITIES.equals(param)) {
                value = gaData.m_stdActiveCode.equals(student.getSpedStatusCode()) ? STRING_ONE : STRING_ZERO;
            } else if (CALC_PARAM_SRC_13.equals(param)) {
                value = STRING_ZERO;
                String rowValue = (String) student.getFieldValueByBeanPath(gaData.m_stdFieldDoeELL);
                if (!StringUtils.isEmpty(rowValue)) {
                    String rowValueState =
                            gaData.lookupStateValue(student.getClass(), gaData.m_stdFieldDoeELL, rowValue);
                    if (!StringUtils.isEmpty(rowValueState)
                            && ("Y".equalsIgnoreCase(rowValueState))) {
                        value = STRING_ONE;
                    }
                }
            } else if (CALC_PARAM_SRC_14.equals(param)) {
                value = gaData.m_stdActiveCode.equals(student.getSection504StatusCode()) ? STRING_ONE : STRING_ZERO;
            } else if (CALC_PARAM_SRC_18.equals(param)) {
                value = STRING_ZERO;
                String rowValue = (String) student.getFieldValueByBeanPath(gaData.m_stdFieldDoeMigrant);
                if (!StringUtils.isEmpty(rowValue)) {
                    String rowValueState =
                            gaData.lookupStateValue(student.getClass(), gaData.m_stdFieldDoeMigrant, rowValue);
                    if (!StringUtils.isEmpty(rowValueState)
                            && Arrays.asList("Y", "C").contains(rowValueState)) {
                        value = STRING_ONE;
                    }
                }
            } else if (CALC_PARAM_SRC_19.equals(param)) {
                value = STRING_ZERO;
                String rowValue = (String) student.getFieldValueByBeanPath(gaData.m_stdFieldDoeELL);
                if (!StringUtils.isEmpty(rowValue)) {
                    String rowValueState =
                            gaData.lookupStateValue(student.getClass(), gaData.m_stdFieldDoeELL, rowValue);
                    if (!StringUtils.isEmpty(rowValueState)
                            && Arrays.asList("1", "2", "3", "4").contains(rowValueState)) {
                        value = STRING_ONE;
                    }
                }
            } else if (CALC_PARAM_SUBJ_CODE.equals(param)) {
                value = ((GAGMAPAsmEntity) entity).getStudentData().getSubjectCode();
            } else if (gaData.m_stdActiveCode.equals(student.getSpedStatusCode())) {
                String primExp = (String) student.getFieldValueByBeanPath(gaData.m_stdFieldPrimExceptionality);
                value = STRING_ZERO;
                if (!StringUtils.isEmpty(primExp)) {
                    String primExpState =
                            gaData.lookupStateValue(student.getClass(), gaData.m_stdFieldPrimExceptionality, primExp);
                    if (!StringUtils.isEmpty(primExpState)) {
                        if (CALC_PARAM_SRC_01.equals(param)) {
                            value = "Z".equalsIgnoreCase(primExpState) || "1".equalsIgnoreCase(primExpState)
                                    ? STRING_ONE
                                    : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_02.equals(param)) {
                            value = "W".equalsIgnoreCase(primExpState) || "X".equalsIgnoreCase(primExpState)
                                    ? STRING_ONE
                                    : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_03.equals(param)) {
                            value = "2".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_04.equals(param)) {
                            value = "U".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_05.equals(param)) {
                            value = "P".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_06.equals(param)) {
                            value = "7".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_07.equals(param)) {
                            value = "Q".equalsIgnoreCase(primExpState) || "R".equalsIgnoreCase(primExpState)
                                    || "S".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_08.equals(param)) {
                            value = "6".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_09.equals(param)) {
                            value = "V".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_10.equals(param)) {
                            value = "3".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_11.equals(param)) {
                            value = "T".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_12.equals(param)) {
                            value = "Y".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        } else if (CALC_PARAM_SRC_15.equals(param)) {
                            value = "8".equalsIgnoreCase(primExpState) ? STRING_ONE : STRING_ZERO;
                        }

                    }
                }
            } else {
                value = STRING_ZERO;
            }

            return value;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        protected static final String CALC_ID = "GMAP-CLEAN-NAME";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
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
            SisStudent student = ((GAGMAPAsmEntity) entity).getStudentData().getStudent();
            String nameValue = (String) getProperty(student, field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll(EMPTY_STRING);
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = EMPTY_STRING;
            }

            return cleanValue;
        }
    }

    /**
     * The Class RetrieveSchool.
     */
    protected class RetrieveSchool implements FieldRetriever {

        protected static final String CALC_ID = "GMAP-SCHOOL";

        protected static final String CALC_PARAM_CODE = "SKL_CODE";
        protected static final String CALC_PARAM_NAME = "SKL_NAME";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String param = (String) field.getParameter();
            GAGMAPAsmData gaData = (GAGMAPAsmData) data;
            SisStudent student = ((GAGMAPAsmEntity) entity).getStudentData().getStudent();
            if (CALC_PARAM_CODE.equals(param)) {
                String stdSklOverride = (String) student.getFieldValueByBeanPath(gaData.m_stdFieldSklOverride);
                if (!StringUtils.isEmpty(stdSklOverride)) {
                    value = gaData.lookupStateValue(student.getClass(), gaData.m_stdFieldSklOverride, stdSklOverride);
                }
                if (StringUtils.isEmpty(value)) {
                    value = (String) student.getSchool().getFieldValueByBeanPath(gaData.m_sklFieldCode);
                }
            } else if (CALC_PARAM_NAME.equals(param)) {
                String stdSklOverride = (String) student.getFieldValueByBeanPath(gaData.m_stdFieldSklOverride);
                if (!StringUtils.isEmpty(stdSklOverride) && m_stdFieldSklOverrideCodes.containsKey(stdSklOverride)) {
                    value = m_stdFieldSklOverrideCodes.get(stdSklOverride).getDescription();
                }
                if (StringUtils.isEmpty(value)) {
                    value = student.getSchool().getName();
                }
            }
            return value;
        }
    }

    /**
     * The Class StudentRow.
     */
    public class StudentRow {
        private SisStudent m_student;
        private String m_subjectCode;

        /**
         * Instantiates a new student row.
         *
         * @param std SisStudent
         * @param subjCode String
         */
        public StudentRow(SisStudent std, String subjCode) {
            m_student = std;
            m_subjectCode = subjCode;
        }

        /**
         * Gets the student.
         *
         * @return the m_student
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Gets the subject code.
         *
         * @return the m_subjectCode
         */
        public String getSubjectCode() {
            return m_subjectCode;
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();
        if (getSetupErrors().size() == 0) {
            m_stdHelper = new StudentHistoryHelper(this);
            m_stdHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
            m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
            m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
            m_stdHelper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL,
                    Arrays.asList("03", "04", "05", "06", "07", "08"));
            m_stdHelper.getStudentCriteria().addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));

            setQuery(m_stdHelper.getStudentQuery(true));
            setEntityClass(GAGMAPAsmEntity.class);

            loadMaps();

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            calcs.put(RetrieveRace.CALC_ID, new RetrieveRace());
            calcs.put(RetrieveSRCFields.CALC_ID, new RetrieveSRCFields());
            calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
            super.addCalcs(calcs);
        }
    }


    /**
     * Initialize instance variables, aliases, lookups.
     */
    private void initializeFields() {
        /*
         * Initialize local values
         */
        m_stdActiveCode =
                PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_stdFieldDoeMeals = translateAliasToJavaName(ALIAS_STD_DOE_MEALS, true);
        m_stdFieldDoeELL = translateAliasToJavaName(ALIAS_STD_DOE_ELL, true);
        m_stdFieldPrimExceptionality = translateAliasToJavaName(ALIAS_STD_PRIM_EXP, true);
        m_stdFieldDoeMigrant = translateAliasToJavaName(ALIAS_STD_DOE_MIGRANT, true);
        m_stdFieldSklOverride = translateAliasToJavaName(ALIAS_STD_SKL_OVERRIDE, true);
        m_sklFieldCode = translateAliasToJavaName(ALIAS_SKL_DOE_CODE, true);
    }

    /**
     * Load maps of supporting data.
     */
    private void loadMaps() {
        // Get race code reference codes for use in the race retriever.
        X2Criteria studentCriteria = m_stdHelper.getStudentCriteria();
        Criteria raceCodeCriteria = new Criteria();
        raceCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCodeCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

        // Load the race codes for all students included in the export.
        SubQuery studentQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, studentQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
        DataDictionaryField stdSklOverrideDD =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_STD_SKL_OVERRIDE);
        if (stdSklOverrideDD != null && stdSklOverrideDD.hasReferenceTable()) {
            m_stdFieldSklOverrideCodes = stdSklOverrideDD.getReferenceTable().getCodeMap();
        }
    }
}
