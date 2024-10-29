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

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

/**
 * New York state procedure for Marking Period export.
 *
 * @author X2 Development Corporation
 */

public class NYMarkingPeriod extends StateReportData {
    /**
     * Entity class for Marking Period export.
     *
     * @author X2 Development Corporation
     */

    public static class MarkingPeriodEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public MarkingPeriodEntity() {
            // no argument constructor
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            NYMarkingPeriod mpData = (NYMarkingPeriod) data;

            ArrayList<String> values = new ArrayList<String>(Arrays.asList(getFieldValues()));

            /*
             * Only unique values should be extracted.
             */
            if (mpData.m_uniqueValues.contains(values)) {
                setRowCount(0);
            } else {
                mpData.m_uniqueValues.add(values);
            }
        }
    }

    /**
     * Retrieves the Term Information that cannot be got from <code>GradeTerm</code> directly.
     */
    protected class RetrieveTermInfo implements FieldRetriever {
        public static final String CALC_ID_TERM_INFO = "TERM-INFO";

        public static final String PARAM_END_DATE = "END-DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();

            if (PARAM_END_DATE.equals(param)) {
                value = getOrganization().getCurrentContext().getEndDate();
            }

            return value;
        }
    }

    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if ((Boolean) getParameter(PARAM_REMOVE_HEADER) == Boolean.TRUE) {
            return null;
        }
        return super.getHeading();
    }

    /*
     * Only unique records should be exctracted, so use m_uniqueValues as store of unique values
     * lists.
     */
    protected HashSet<List> m_uniqueValues = new HashSet<List>();

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        setEntityClass(MarkingPeriodEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveTermInfo.CALC_ID_TERM_INFO, new RetrieveTermInfo());

        super.addCalcs(calcs);
    }
}
