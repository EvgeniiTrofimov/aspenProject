/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Connecticut SPED state report for Early Childhood Outcome export.
 * This class implements the data export for Early Childhood Outcome export.
 *
 * @author X2 Development Corporation
 */
public class EarlyChildhoodOutcomes extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Early Childhood Outcome export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class EarlyChildhoodOutcomesEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public EarlyChildhoodOutcomesEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentAssessment studentAssessment = (StudentAssessment) getBean();
            SisStudent student = studentAssessment.getStudent();

            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] Assessment Date: " + studentAssessment.getDate().toString();
            return name;
        }

        /**
         * Initialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports#initialize()
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            EarlyChildhoodOutcomes ecoData = (EarlyChildhoodOutcomes) data;
            if (!ecoData.m_dictionarySet) {
                ExtendedDictionaryAttributes extendedDictionary =
                        ((StudentAssessment) bean).getExtendedDataDictionary();
                DataDictionary dictionary =
                        DataDictionary.getDistrictDictionary(extendedDictionary, data.getBroker().getPersistenceKey());
                data.setDataDictionary(dictionary);
                ecoData.m_dictionarySet = true;
            }
        }
    }

    /*
     * Instance variables.
     */
    protected boolean m_dictionarySet = false;

    /**
     * Class RetrieveAliasValue is the FieldRetriever used to retrieve alias values.
     * If the alias is prefixed with "t:", the reference table state code is retrieved.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveValueByAlias implements FieldRetriever {

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
            String alias = (String) field.getParameter();
            boolean prefix = alias != null && alias.startsWith("t:");
            if (prefix) {
                alias = alias.substring(2);
            }
            String aliasPath = data.translateAliasToJavaName(alias, true);
            String value = (String) data.getProperty(entity.getBean(), aliasPath);
            if (prefix) {
                value = lookupReferenceCodeByAlias(alias, value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
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
        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */

            X2Criteria assessmentCriteria = getAssessmentCriteria();
            QueryByCriteria assessmentQuery = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);
            applyInputSort(assessmentQuery, StudentAssessment.REL_STUDENT);

            /*
             * Set the query to be used for IepData selection.
             */
            setQuery(assessmentQuery);
            setEntityClass(EarlyChildhoodOutcomesEntity.class);

            /*
             * Build maps of retriever functions
             */
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ECO-VALUE-BY-ALIAS", new RetrieveValueByAlias());

            addCalcs(calcs);
        }
    }

    /**
     * Compose and return an iepCriteria .
     *
     * @return studentCriteria
     */
    private X2Criteria getAssessmentCriteria() {
        X2Criteria assessmentCriteria = new X2Criteria();

        /*
         * Active students with "ECO" assessments.
         */
        assessmentCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER +
                AssessmentDefinition.COL_ID, "ECO");

        /*
         * Where assessment date is greater than start of the school year
         */
        assessmentCriteria.addGreaterThan(StudentAssessment.COL_DATE, getCurrentContext().getStartDate());

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            assessmentCriteria.addEqualTo(StudentAssessment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            assessmentCriteria.addEqualTo(StudentAssessment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            assessmentCriteria.addEqualTo(StudentAssessment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        applyInputCriteria(assessmentCriteria, false, StudentAssessment.REL_STUDENT);

        return assessmentCriteria;
    }
}
