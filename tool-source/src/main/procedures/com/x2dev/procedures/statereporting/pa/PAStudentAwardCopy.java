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

package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Copy procedure for student awards.
 *
 */
public class PAStudentAwardCopy extends ProcedureJavaSource {

    /**
     * Aliases
     */
    private static final String ALIAS_STD_AWARD_CODE = "all-std-AwardCode";
    private static final String ALIAS_STD_AWARD_DATE = "all-std-AwardDate";
    private static final String ALIAS_STD_AWARD_TYPE = "all-std-AwardType";

    /**
     * Messages
     */
    private static final String AWARD_COPY_ALREADY_EXISTS = "Award copy already exists";
    private static final String AWARD_FIELDS_COPIED_SUCCESSFULLY = "New award copy created successfully";
    private static final String AWARD_FIELDS_NOT_COMPLETED = "Award code, date, type should't be empty";

    /**
     * Input parameters
     */
    private static final String INPUT_PARAM_CURRENT_SELECTION = "##current";

    /**
     * IDs
     */
    private static final String USER_DEFINED_TABLEA_DDX_ID = "PA-AWARD-FIELDS";

    /**
     * Members
     */
    private List<String> m_awardCodePath = new ArrayList<String>();
    private List<String> m_awardDatePath = new ArrayList<String>();
    private List<String> m_awardTypePath = new ArrayList<String>();

    private ExtendedDataDictionary m_ddxById = null;
    private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();
    private DataDictionary m_dictionary = null;
    private Map<String, Collection<UserDefinedTableA>> m_existingAwards;
    private Map<String, DataDictionaryField> m_fieldsByAlias = new HashMap<>();
    private Map<String, SisStudent> m_students = new HashMap<>();

    /**
     * Execute.
     *
     */
    @Override
    protected void execute() {
        String awardDateValue;
        String awardCodeValue;
        String awardTypeValue;
        String awardNumber;

        for (SisStudent std : m_students.values()) {
            for (int i = 0; i < 6; i++) {
                if (i == 0) {
                    awardNumber = "";
                } else {
                    awardNumber = "" + i + 1;
                }

                awardDateValue = (String) std.getFieldValueByBeanPath(m_awardDatePath.get(i));
                awardCodeValue = (String) std.getFieldValueByBeanPath(m_awardCodePath.get(i));
                awardTypeValue = (String) std.getFieldValueByBeanPath(m_awardTypePath.get(i));

                if (StringUtils.isEmpty(awardDateValue) || StringUtils.isEmpty(awardCodeValue)
                        || StringUtils.isEmpty(awardTypeValue)) {
                    continue;
                }

                if (isStudentAwardExists(std, awardDateValue, awardCodeValue, awardTypeValue)) {
                    logMessage(std.getNameView() + "," + AWARD_COPY_ALREADY_EXISTS + ":"
                            + ALIAS_STD_AWARD_DATE + awardNumber + "=" + awardDateValue + ","
                            + ALIAS_STD_AWARD_CODE + awardNumber + "=" + awardCodeValue + ","
                            + ALIAS_STD_AWARD_TYPE + awardNumber + "=" + awardTypeValue);
                    continue;
                }

                studentAwardsCopy(awardNumber, awardCodeValue, awardDateValue, awardTypeValue, std);
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        getFieldsPathByAlias();

        m_ddxById = getExtendedDataDictionaryById(USER_DEFINED_TABLEA_DDX_ID);
        m_dictionary = DataDictionary.getDistrictDictionary(m_ddxById, getBroker().getPersistenceKey());

        X2Criteria stdCriteria = getStudentCriteria(0);

        for (int i = 1; i < 6; i++) {
            stdCriteria.addOrCriteria(getStudentCriteria(i));
        }

        QueryByCriteria stdQuery = new QueryByCriteria(SisStudent.class, stdCriteria);
        m_students = getBroker().getMapByQuery(stdQuery, SisStudent.COL_PERSON_OID, 100);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                + ExtendedDataDictionary.COL_ID, USER_DEFINED_TABLEA_DDX_ID);
        BeanQuery query = new BeanQuery(UserDefinedTableA.class, criteria);
        m_existingAwards = getBroker().getGroupedCollectionByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 1024);
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
     * Gets the field by alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return Data dictionary field
     */
    private DataDictionaryField getFieldByAlias(String alias, DataDictionary dataDictionary) {
        if (m_fieldsByAlias == null) {
            m_fieldsByAlias = new HashMap<>();
        }
        if (m_fieldsByAlias.get(alias) == null) {
            DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                m_fieldsByAlias.put(alias, field);
            }
        }
        return m_fieldsByAlias.get(alias);
    }

    /**
     * Gets the fields by paths.
     *
     * @return void
     */
    private void getFieldsPathByAlias() {
        for (int i = 1; i <= 6; i++) {
            if (i == 1) {
                m_awardDatePath.add(DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_STD_AWARD_DATE).getJavaName());
                m_awardCodePath.add(DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_STD_AWARD_CODE).getJavaName());
                m_awardTypePath.add(DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_STD_AWARD_TYPE).getJavaName());
            } else {
                m_awardDatePath.add(DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_STD_AWARD_DATE + i).getJavaName());
                m_awardCodePath.add(DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_STD_AWARD_CODE + i).getJavaName());
                m_awardTypePath.add(DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                        .findDataDictionaryFieldByAlias(ALIAS_STD_AWARD_TYPE + i).getJavaName());
            }
        }
    }

    /**
     * Gets the student criteria.
     *
     * @param i int
     * @return X 2 criteria
     */
    private X2Criteria getStudentCriteria(int i) {
        X2Criteria stdCriteria = new X2Criteria();

        stdCriteria.addNotEmpty(m_awardCodePath.get(i), getBroker().getPersistenceKey());
        stdCriteria.addNotEmpty(m_awardDatePath.get(i), getBroker().getPersistenceKey());
        stdCriteria.addNotEmpty(m_awardTypePath.get(i), getBroker().getPersistenceKey());

        return stdCriteria;
    }

    /**
     * Check if the student award already exist.
     *
     * @param std SisStudent
     * @param awardDateValue SisStudent
     * @param awardCodeValue SisStudent
     * @param awardTypeValue SisStudent
     * @return boolean
     */
    private boolean isStudentAwardExists(SisStudent std,
                                         String awardDateValue,
                                         String awardCodeValue,
                                         String awardTypeValue) {
        String stdAwardCodeValue = "";
        String stdAwardDateValue = "";
        String stdAwardTypeValue = "";
        boolean awardExists = false;

        Collection<UserDefinedTableA> filteredAwardRecords = m_existingAwards.get(std.getOid());

        if (filteredAwardRecords != null) {
            for (UserDefinedTableA awardRecord : filteredAwardRecords) {
                stdAwardCodeValue = (String) awardRecord.getFieldValueByAlias(ALIAS_STD_AWARD_CODE,
                        m_dictionary);

                stdAwardDateValue = (String) awardRecord.getFieldValueByAlias(ALIAS_STD_AWARD_DATE,
                        m_dictionary);

                stdAwardTypeValue = (String) awardRecord.getFieldValueByAlias(ALIAS_STD_AWARD_TYPE,
                        m_dictionary);

                if (!StringUtils.isEmpty(stdAwardDateValue) && !StringUtils.isEmpty(stdAwardTypeValue)
                        && !StringUtils.isEmpty(stdAwardCodeValue)) {
                    if (stdAwardDateValue.equals(awardDateValue) && (stdAwardTypeValue.equals(awardTypeValue))
                            && (stdAwardCodeValue.equals(awardCodeValue))) {
                        awardExists = true;
                        break;
                    }
                }
            }
        }
        return awardExists;
    }

    /**
     * Instantiates a new student award object.
     *
     * @param awardNumber String
     * @param awardCode String
     * @param awardDate String
     * @param awardType String
     * @param std SisStudent
     */
    private void studentAwardsCopy(String awardNumber,
                                   String awardCode,
                                   String awardDate,
                                   String awardType,
                                   SisStudent std) {
        UserDefinedTableA m_studentAward =
                X2BaseBean.newInstance(UserDefinedTableA.class, getBroker().getPersistenceKey());

        m_studentAward.setFieldValueByAlias(ALIAS_STD_AWARD_CODE, awardCode, m_dictionary);
        m_studentAward.setFieldValueByAlias(ALIAS_STD_AWARD_DATE, awardDate, m_dictionary);
        m_studentAward.setFieldValueByAlias(ALIAS_STD_AWARD_TYPE, awardType, m_dictionary);
        m_studentAward.setOrganization1Oid(std.getOrganization1Oid());

        m_studentAward.setStudentOid(std.getOid());
        m_studentAward.setSchoolOid(std.getSchoolOid());
        m_studentAward.setExtendedDataDictionaryOid(m_dictionary.getExtendedDictionaryOid());

        getBroker().saveBeanForced(m_studentAward, m_dictionary);

        logMessage(std.getNameView() + "," + AWARD_FIELDS_COPIED_SUCCESSFULLY + ":"
                + ALIAS_STD_AWARD_DATE + awardNumber + "=" + awardDate + ","
                + ALIAS_STD_AWARD_CODE + awardNumber + "=" + awardCode + ","
                + ALIAS_STD_AWARD_TYPE + awardNumber + "=" + awardType);
    }
}
