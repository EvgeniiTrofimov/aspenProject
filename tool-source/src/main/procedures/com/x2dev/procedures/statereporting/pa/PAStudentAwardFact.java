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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.pa.PAStudentAwardFact.PAStudentAwardFactEntity.AwardStatictics;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PAStudentAwardFact.
 */
public class PAStudentAwardFact extends StateReportData {

    /**
     * The Class PAStudentAwardFactEntity.
     */
    /* Entity class */
    public static class PAStudentAwardFactEntity extends StateReportEntity {

        PAStudentAwardFact m_exportData;
        List<AwardStatictics> m_awardStatistics;
        private StateReportData m_stateReportData;
        private UserDefinedTableA m_studentAwards = null;

        /**
         * Helper class to store award statistics.
         *
         * @author Follett Software Company
         * @copyright 2018
         */
        class AwardStatictics implements Comparable<AwardStatictics> {

            /**
             * Members.
             */
            private String m_awardCode;
            private Date m_awardDate;
            private String m_awardType;

            /**
             * Constructor.
             *
             * @param awardCode String
             * @param awardDate Date
             * @param awardType String
             */
            public AwardStatictics(String awardCode, Date awardDate, String awardType) {
                this.m_awardCode = awardCode;
                this.m_awardDate = awardDate;
                this.m_awardType = awardType;
            }

            /**
             * Compare to.
             *
             * @param o AwardStatictics
             * @return int
             * @see java.lang.Comparable#compareTo(java.lang.Object)
             */
            @Override
            public int compareTo(AwardStatictics o) {
                int result = m_awardDate.equals(o.getAwardDate()) ? 0 : 1;
                if (result == 0) {
                    result = m_awardCode.compareTo(o.getAwardCode());
                }
                if (result == 0) {
                    result = m_awardType.compareTo(o.getAwardType());
                }
                return result;
            }

            /**
             * Equals.
             *
             * @param obj Object
             * @return true, if successful
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                return compareTo((AwardStatictics) obj) == 0 ? true : false;
            }

            /**
             * Gets the award code.
             *
             * @return the m_awardCode
             */
            public String getAwardCode() {
                return m_awardCode;
            }

            /**
             * Gets the award date.
             *
             * @return the m_awardDate
             */
            public Date getAwardDate() {
                return m_awardDate;
            }

            /**
             * Gets the award type.
             *
             * @return the m_awardType
             */
            public String getAwardType() {
                return m_awardType;
            }
        }

        /**
         * Instantiates a new PA student award fact entity.
         */
        public PAStudentAwardFactEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the mtc.
         *
         * @return Teacher status
         */
        public AwardStatictics getCurrentAwardStatistics() {
            return m_awardStatistics.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    ", School ID: " + student.getSchool().getSchoolId() +
                    "] ";
            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent std = (SisStudent) bean;
            m_awardStatistics = new ArrayList<AwardStatictics>();
            m_exportData = (PAStudentAwardFact) data;
            m_stateReportData = data;
            addAwardStatistics(std);
            setRowCount(m_awardStatistics.size());
        }

        /**
         * Add award statistic to collection.
         *
         * @param std String
         */
        private void addAwardStatistics(SisStudent std) {
            DataDictionaryField stdAwardCode =
                    m_exportData.getFieldByAlias(ALIAS_STD_AWARD_CODE, m_exportData.getDictionaryAwards());
            DataDictionaryField stdAwardType =
                    m_exportData.getFieldByAlias(ALIAS_STD_AWARD_TYPE, m_exportData.getDictionaryAwards());

            String stdAwardCodeValue = "";
            String stdAwardStringValue = null;
            Date stdAwardDateValue = null;
            String stdAwardTypeValue = "";

            Collection<UserDefinedTableA> filteredAwardRecords = m_exportData.getStudentAwards(std.getOid());
            for (UserDefinedTableA awardRecord : filteredAwardRecords) {
                stdAwardCodeValue = (String) awardRecord.getFieldValueByBeanPath(stdAwardCode.getJavaName());
                stdAwardCodeValue = m_exportData.lookupReferenceCodeByRefTbl(stdAwardCode.getReferenceTableOid(),
                        stdAwardCodeValue, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                stdAwardTypeValue = (String) awardRecord.getFieldValueByBeanPath(stdAwardType.getJavaName());
                stdAwardTypeValue = m_exportData.lookupReferenceCodeByRefTbl(stdAwardType.getReferenceTableOid(),
                        stdAwardTypeValue, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                stdAwardStringValue = (String) awardRecord.getFieldValueByAlias(ALIAS_STD_AWARD_DATE,
                        m_exportData.m_dictionaryAwards);
                stdAwardDateValue = m_exportData.getDate(stdAwardStringValue);
                if (!m_exportData.getCurrentContext().getStartDate().after(stdAwardDateValue)) {
                    AwardStatictics awardStatictics =
                            new AwardStatictics(stdAwardCodeValue, stdAwardDateValue, stdAwardTypeValue);
                    if (!m_awardStatistics.contains(awardStatictics)) {
                        m_awardStatistics.add(awardStatictics);
                    }
                }
            }
        }
    }

    /**
     * Field retriever for school year date.
     */
    protected class RetrieveAwardData implements FieldRetriever {

        private static final String CALC_ID = "STD_AWARD_DATA";

        private static final String CALC_PARAM_AWARD_CODE = "AWARD-CODE";
        private static final String CALC_PARAM_AWARD_DATE = "AWARD-DATE";
        private static final String CALC_PARAM_AWARD_TYPE = "AWARD-TYPE";
        private static final String CALC_PARAM_SKL_YEAR = "SKL-YEAR";

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
            Object value = null;
            PAStudentAwardFactEntity safEntity = (PAStudentAwardFactEntity) entity;
            AwardStatictics currAwardStatistics = safEntity.getCurrentAwardStatistics();
            String param = (String) field.getParameter();
            if (CALC_PARAM_AWARD_CODE.equals(param)) {
                value = currAwardStatistics.getAwardCode();
            } else if (CALC_PARAM_AWARD_DATE.equals(param)) {
                value = currAwardStatistics.getAwardDate();
            } else if (CALC_PARAM_AWARD_TYPE.equals(param)) {
                value = currAwardStatistics.getAwardType();
            } else if (CALC_PARAM_SKL_YEAR.equals(param)) {
                Date awardDate = currAwardStatistics.getAwardDate();
                DistrictSchoolYearContext ctx = getDistrictContextByDate(awardDate);
                if (ctx != null) {
                    value = ctx.getEndDate();
                }
            }
            return value;
        }
    }

    /**
     * IDs
     */
    protected static final String USER_DEFINED_TABLEA_DDX_ID = "PA-AWARD-FIELDS";

    /**
     * Aliases
     */
    private static final String ALIAS_STD_AWARD_CODE = "all-std-AwardCode";
    private static final String ALIAS_STD_AWARD_DATE = "all-std-AwardDate";
    private static final String ALIAS_STD_AWARD_TYPE = "all-std-AwardType";

    /**
     * Members
     */
    protected StudentHistoryHelper m_helper;
    protected Collection<DistrictSchoolYearContext> m_schoolYears;

    private Map<String, DataDictionaryField> m_fieldsByAlias = new HashMap<>();
    private DateAsStringConverter m_dateConverter;
    private DataDictionary m_dictionaryAwards = null;
    private Map<String, Collection<UserDefinedTableA>> m_studentAwardsMap;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        loadSchoolYears();
        /*
         * Build helper object
         */
        if (getSetupErrors().size() == 0) {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_helper.getStudentCriteria()
                    .addEqualTo(SisStudent.REL_USER_DEFINED_RECORDS_A + ModelProperty.PATH_DELIMITER
                            + UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                            + ExtendedDataDictionary.COL_ID, USER_DEFINED_TABLEA_DDX_ID);

            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(PAStudentAwardFactEntity.class);

            // Build a map of calculations/retrievers.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveAwardData.CALC_ID, new RetrieveAwardData());
            super.addCalcs(calcs);
        }
    }

    /**
     * Gets the date.
     *
     * @param dateString String
     * @return Plain date
     */
    protected PlainDate getDate(String dateString) {
        if (m_dateConverter == null) {
            m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    Locale.getDefault(), true);
        }
        return (PlainDate) m_dateConverter.parseSystemString(dateString);
    }

    /**
     * Gets the field by alias.
     *
     * @param alias String
     * @param dataDictionary DataDictionary
     * @return Data dictionary field
     */
    protected DataDictionaryField getFieldByAlias(String alias, DataDictionary dataDictionary) {
        if (m_fieldsByAlias.get(alias) == null) {
            DataDictionaryField field = dataDictionary.findDataDictionaryFieldByAlias(alias);
            if (field != null) {
                m_fieldsByAlias.put(alias, field);
            }
        }
        return m_fieldsByAlias.get(alias);
    }

    /**
     * Gets the student awards.
     *
     * @param oid String
     * @return Collection
     */
    protected Collection<UserDefinedTableA> getStudentAwards(String oid) {
        if (m_studentAwardsMap == null) {
            BeanQuery query = m_helper.getStudentSelectionQuery(UserDefinedTableA.class, new X2Criteria(),
                    UserDefinedTableA.COL_STUDENT_OID);
            m_studentAwardsMap =
                    getBroker().getGroupedCollectionByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 1024);
        }
        return m_studentAwardsMap.get(oid);
    }

    /**
     * Gets the dictionary awards.
     *
     * @return the m_dictionaryAwards
     */
    private DataDictionary getDictionaryAwards() {
        if (m_dictionaryAwards == null) {
            X2Criteria ddxCriteria = new X2Criteria();
            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, USER_DEFINED_TABLEA_DDX_ID);
            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxQuery);
            m_dictionaryAwards = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
        }
        return m_dictionaryAwards;
    }

    /**
     * Calculate CTX by given date.
     *
     * @param date PlainDate
     * @return count differences
     */
    private DistrictSchoolYearContext getDistrictContextByDate(Date date) {
        DistrictSchoolYearContext ctxToReturn = null;
        for (DistrictSchoolYearContext ctx : m_schoolYears) {
            if (!ctx.getStartDate().after(date) && !ctx.getEndDate().before(date)) {
                ctxToReturn = ctx;
                break;
            }
        }
        return ctxToReturn;
    }

    /**
     * Load all school years.
     */
    private void loadSchoolYears() {
        QueryByCriteria distrctContextQuery = new QueryByCriteria(DistrictSchoolYearContext.class, new X2Criteria());
        distrctContextQuery.addOrderByDescending(DistrictSchoolYearContext.COL_CONTEXT_ID);
        m_schoolYears = getBroker().getCollectionByQuery(distrctContextQuery);
    }
}
