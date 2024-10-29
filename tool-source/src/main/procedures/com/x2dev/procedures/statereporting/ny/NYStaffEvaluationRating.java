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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.HashMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Staff Evaluation export.
 *
 * @author X2 Development Corporation
 */

public class NYStaffEvaluationRating extends StateReportData {
    /**
     * Entity class for Staff Evaluation export.
     *
     * @author X2 Development Corporation
     */

    public static class StaffEvaluationRatingEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */

        public StaffEvaluationRatingEntity() {
            // no argument constructor
        }

        ArrayList<String> evaluationCodes;
        ArrayList<String> evaluationRatings;

        /**
         * Local variables for reporting information.
         */
        protected String m_staffStatus;

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStaff staff = (SisStaff) bean;
            // //TODO Table replace
            NYStaffEvaluationRating serData = (NYStaffEvaluationRating) data;
            evaluationCodes = new ArrayList<String>();
            evaluationRatings = new ArrayList<String>();
            for (int a = 0; a < 4; a++) {
                String javaname = serData.m_aliasEvaluations.get(a);
                if (javaname != null) {
                    String element = (String) staff.getFieldValueByBeanPath(javaname);
                    if (element != null) {
                        evaluationCodes.add(element);
                    }
                }
                javaname = serData.m_aliasEvaluationRatings.get(a);
                if (javaname != null) {
                    String element = (String) staff.getFieldValueByBeanPath(javaname);
                    if (element != null) {
                        evaluationRatings.add(element);
                    }
                }

            }
            setRowCount(evaluationRatings.size());
        }

        /**
         * Gets the current evaluation code.
         *
         * @return String
         */
        // TODO Table replace
        public String getCurrentEvaluationCode() {
            return (evaluationCodes.size() > getCurrentRow()) ? evaluationCodes.get(getCurrentRow()) : null;
        }

        /**
         * Gets the current evaluation rating.
         *
         * @return String
         */
        public String getCurrentEvaluationRating() {
            return (evaluationRatings.size() > getCurrentRow()) ? evaluationRatings.get(getCurrentRow()) : null;
        }
    }

    /**
     * Retriever to get data off of the evaluation record.
     */
    protected class RetrieveEvaluation implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            StaffEvaluationRatingEntity serEntity = (StaffEvaluationRatingEntity) entity;
            if ("CODE".equals(param)) {
                value = serEntity.getCurrentEvaluationCode();
            } else if ("RATING".equals(param)) {
                value = serEntity.getCurrentEvaluationRating();
            }
            return value;
        }

    }

    /**
     * Aliases
     */
    protected static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";

    /**
     * Instance variables
     */
    protected String m_excludeStfField;
    // TODO Table Replace
    protected ArrayList<String> m_aliasEvaluations;
    protected ArrayList<String> m_aliasEvaluationCodes;
    protected ArrayList<String> m_aliasEvaluationRatings;

    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()) {
            return null;
        }
        return super.getHeading();
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        Criteria criteria = getStaffCriteria();

        // … create staff Query …
        QueryByCriteria query = new QueryByCriteria(Staff.class, criteria);
        applyInputSort(query, null);

        setQuery(query);

        setEntityClass(StaffEvaluationRatingEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();

        calcs.put("SER-EVAL", new RetrieveEvaluation());

        HashMap validators = new HashMap<String, FieldRetriever>();
        super.addCalcs(calcs);
        super.addValidators(validators);
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_excludeStfField = translateAliasToJavaName(ALIAS_EXCLUDE_STF, false);
        // TODO Table replace
        m_aliasEvaluations = new ArrayList<String>();
        m_aliasEvaluationRatings = new ArrayList<String>();
        for (int a = 1; a < 5; a++) {
            m_aliasEvaluations.add(translateAliasToJavaName("DOE EVAL CODE " + a, false));
            m_aliasEvaluationRatings.add(translateAliasToJavaName("DOE EVAL CRIT RATING POINTS" + a, false));
        }
    }

    /**
     * Returns the criteria that retrieves all staff that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStaffCriteria() {
        Criteria staffCriteria = new Criteria();
        staffCriteria.addNotNull(X2BaseBean.COL_OID);

        applyInputCriteria(staffCriteria, true, null);
        if (!isSchoolContext()) {
            staffCriteria.addNotEqualTo(
                    Staff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
            staffCriteria.addNotEqualTo(
                    Staff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }

        // Check exclude staff from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStfField)) {
            staffCriteria.addNotEqualTo(m_excludeStfField, BooleanAsStringConverter.TRUE);
        }

        return staffCriteria;
    }
}
