/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.on;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class OnSEAClaimData extends StateReportData {

    public static final String ALIAS_FUNDING_SOURCE = "udc-spedinv-funding-source";
    public static final String VALUE_FUNDING = "PPA";

    /**
     * Clean a comment string by removing new line characters and making it a single long string.
     *
     * @author Follett Software Company
     * @copyright 2020
     */
    public static class RetrieveCleanNL implements FieldRetriever {
        public static final String RETRIEVER_ID = "CLEANNL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String fieldValue = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            fieldValue = fieldValue.replace("\n", " "); // New line
            fieldValue = fieldValue.replace("\t", " "); // Tab
            fieldValue = fieldValue.replace("\r", ""); // Carriage return
            return fieldValue;
        }
    }

    /**
     * Return the Equipment Category if the equipment type matches the field parameter.
     *
     * @author Follett Software Company
     * @copyright 2020
     */
    public static class RetrieveEquipmentTypes implements FieldRetriever {
        public static final String RETRIEVER_ID = "TYPES";
        private static final String ALIAS_TYPE = "uda-spedinv-type";
        private static final String ALIAS_CATEGORY = "uda-spedinv-category";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            UserDefinedTableB recordB = (UserDefinedTableB) entity.getBean();
            UserDefinedTableA recordA = recordB.getUserDefinedTableC().getUserDefinedTableA();
            String category = (String) recordA.getFieldValueByAlias(ALIAS_CATEGORY, data.getDataDictionary());
            String type = (String) recordA.getFieldValueByAlias(ALIAS_TYPE, data.getDataDictionary());
            String checkType = (String) field.getParameter();
            if (checkType != null && checkType.equals(type)) {
                return category;
            }
            return null;
        }
    }

    /**
     * Retrieves the grade panel indicator.
     * For grades PK, K, 1-8 return 1.
     * For grades 9-12 return 2.
     *
     * @author Follett Software Company
     * @copyright 2020
     */
    public static class RetrieveGradePanel implements FieldRetriever {

        public static final String RETRIEVER_ID = "GRADE_PANEL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            UserDefinedTableB record = (UserDefinedTableB) entity.getBean();
            Student student = record.getStudent();
            String gradeLevel = student.getGradeLevel();
            if (StringUtils.isNumeric(gradeLevel)) {
                Integer grade = Integer.valueOf(gradeLevel);
                if (grade >= 9 && grade <= 12) {
                    return "2";
                }
            }
            return "1";
        }
    }

    /**
     * Retrieve a value from the current active IEP.
     * The basic value is based on a field alias in the calculation parameter field.
     * If the calc parameter is "disability", retrieve the primary disability value.
     *
     * @author Follett Software Company
     * @copyright 2020
     */
    public static class RetrieveIEP implements FieldRetriever {
        public static final String RETRIEVER_ID = "IEP";
        private static final String PARAM_DISABILITY = "disability";
        private static final String PARAM_DISABILITIES = "disabilities";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            UserDefinedTableB record = (UserDefinedTableB) entity.getBean();
            String param = (String) field.getParameter();
            SisStudent student = record.getStudent();
            IepData iep = student.getActiveIep(data.getBroker());
            String result = null;
            if (iep != null) {
                if (PARAM_DISABILITY.equals(param)) {
                    IepDisability disability = iep.getPrimaryDisability();
                    if (disability != null) {
                        result = disability.getDisabilityCode();
                    }
                    if (StringUtils.isEmpty(result)) {
                        result = "Not Identified";
                    }
                }
                if (PARAM_DISABILITIES.equals(param)) {
                    List<IepDisability> disabilities = (List) iep.getIepDisability();
                    StringBuilder builder = new StringBuilder();
                    if (disabilities != null) {
                        for (IepDisability disability : disabilities) {
                            if (!disability.getPrimaryIndicator()) {
                                if (builder.length() > 0) {
                                    builder.append(", ");
                                }
                                builder.append(disability.getDisabilityCode());
                            }
                        }
                        result = builder.toString();
                    }
                } else {
                    DataDictionary dictionary = DataDictionary.getDistrictDictionary(iep.getExtendedDataDictionary(),
                            data.getBroker().getPersistenceKey());
                    result = (String) iep.getFieldValueByAlias(param, dictionary);
                }
            }
            return result;
        }
    }

    public static class RetrieveSEAShared implements FieldRetriever {
        public static final String RETRIEVER_ID = "SEAShared";
        private static final String ALIAS_SEA = "uda-spedinv-sea";
        private static final String ALIAS_SHARED = "udc-spedinv-shared";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            UserDefinedTableB recordB = (UserDefinedTableB) entity.getBean();
            UserDefinedTableC recordC = recordB.getUserDefinedTableC();
            UserDefinedTableA recordA = recordC.getUserDefinedTableA();
            String shared = (String) recordC.getFieldValueByAlias(ALIAS_SHARED, data.getDataDictionary());
            String sea = (String) recordA.getFieldValueByAlias(ALIAS_SEA, data.getDataDictionary());
            if (BooleanAsStringConverter.TRUE.equals(shared) && BooleanAsStringConverter.TRUE.equals(sea)) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    /**
     * Create a heading string from the column headings of the fields.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder();
        List<FieldDefinition> fields = getFieldDefinitions();
        for (FieldDefinition field : fields) {
            if (heading.length() > 0 && getUseValueDelimiters()) {
                heading.append(getValueDelimiter());
            }
            heading.append(field.getFieldId());
        }
        if (heading.length() > 0) {
            heading.append(ExportJavaSource.FORMAT_EOL_WINDOWS);
        }
        return heading.toString();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        String dictionaryOid = getDataDictionary().getExtendedDictionaryOid();
        String fundingField = translateAliasToJavaName(ALIAS_FUNDING_SOURCE, true);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID, dictionaryOid);
        criteria.addEqualTo(UserDefinedTableB.REL_USER_DEFINED_TABLE_C + ModelProperty.PATH_DELIMITER +
                fundingField, VALUE_FUNDING);
        if (isSchoolContext()) {
            criteria.addEqualTo(UserDefinedTableB.COL_SCHOOL_OID, getSchool().getOid());
        }

        BeanQuery query = new BeanQuery(UserDefinedTableB.class, criteria);
        query.setFetchSize(1024);
        setQuery(query);

        Map<String, FieldRetriever> calcMap = new HashMap<String, FieldRetriever>(1);
        calcMap.put(RetrieveCleanNL.RETRIEVER_ID, new RetrieveCleanNL());
        calcMap.put(RetrieveEquipmentTypes.RETRIEVER_ID, new RetrieveEquipmentTypes());
        calcMap.put(RetrieveGradePanel.RETRIEVER_ID, new RetrieveGradePanel());
        calcMap.put(RetrieveIEP.RETRIEVER_ID, new RetrieveIEP());
        calcMap.put(RetrieveSEAShared.RETRIEVER_ID, new RetrieveSEAShared());
        this.addCalcs(calcMap);
    }
}
