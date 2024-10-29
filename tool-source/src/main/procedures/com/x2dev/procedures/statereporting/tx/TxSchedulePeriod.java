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
package com.x2dev.procedures.statereporting.tx;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This is a class for TXSchoolPeriod Complex Type export (Education Organization Interchange).
 *
 * @author X2 Development Corporation
 */
public class TxSchedulePeriod extends TxCoreReportData {
    /**
     * Retriever is used to get period number or id of ClassPeriodName.
     *
     * @author Follett Software Company
     */
    public class PeriodIdRetriever implements FieldRetriever {
        private static final String CALC_ID = "PER-CALC-ID";

        private static final String CALC_PARAM_PERIOD_ID = "PER-ID";
        private static final String CALC_PARAM_PERIOD_NUM = "PER-NUM";

        private static final String PATTERN_PERIOD = "^[\\d]{2}$";

        private static final String POSTFIX_ID = "-Traditional";
        private static final String PREFIX_ID = "CPER_";

        private static final String ZERO = "0";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public String getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            SchedulePeriod period = (SchedulePeriod) entity.getBean();

            Pattern pattern = Pattern.compile(PATTERN_PERIOD);
            Matcher matcher = pattern.matcher(String.valueOf(period.getNumber()));

            String periodNumber = null;

            if (matcher.find()) {
                periodNumber = matcher.group();
            } else {
                StringBuilder stringBuilder = new StringBuilder(2);
                stringBuilder.append(ZERO);
                stringBuilder.append(period.getNumber());

                periodNumber = stringBuilder.toString();
            }

            if (CALC_PARAM_PERIOD_NUM.equals(field.getParameter())) {
                value = periodNumber;
            } else if (CALC_PARAM_PERIOD_ID.equals(field.getParameter())) {
                value = new StringBuilder(PREFIX_ID).append(periodNumber).append(POSTFIX_ID).toString();
            }

            return value;
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            X2Criteria perCriteria = buildPerCriteria();
            QueryByCriteria perQuery = new QueryByCriteria(SchedulePeriod.class, perCriteria);

            perQuery.addGroupBy(SchedulePeriod.COL_NUMBER);
            perQuery.addOrderByAscending(SchedulePeriod.COL_NUMBER);

            setQuery(perQuery);

            Map<String, FieldRetriever> calc = new HashMap<String, FieldRetriever>();
            calc.put(PeriodIdRetriever.CALC_ID, new PeriodIdRetriever());
            addCalcs(calc);
        }
    }

    /**
     * Build period criteria for active schedule and current school year context.
     *
     * @return criteria
     */
    private X2Criteria buildPerCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualToField(SchedulePeriod.COL_SCHEDULE_OID,
                SchedulePeriod.REL_SCHEDULE + ModelProperty.PATH_DELIMITER
                        + Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + ModelProperty.PATH_DELIMITER
                        + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);
        criteria.addEqualTo(
                SchedulePeriod.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());
        criteria.addNotEqualTo(SchedulePeriod.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.REL_SCHOOL
                + ModelProperty.PATH_DELIMITER
                + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        return criteria;
    }
}
