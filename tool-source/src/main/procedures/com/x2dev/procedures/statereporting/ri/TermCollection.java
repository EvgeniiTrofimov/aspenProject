/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for Term Collection export. This class implements the data
 * export for the Term Collection export.
 *
 * @author X2 Development Corporation
 */
public class TermCollection extends RIStateReportData {

    /**
     * m_scheduleTermRefCodes map to populate the codes from Schedule Term Codes reference table
     *
     */
    protected Map<String, ReferenceCode> m_scheduleTermRefCodes = new HashMap<String, ReferenceCode>();

    /**
     * Implementation of StateReportEntity to be used by the Term Collection
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class TermCollectionEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TermCollectionEntity() {
            // Empty constructor.
        }

        /**
         * Generate a display name to print on the validation report for the
         * entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {

            ScheduleTerm term = (ScheduleTerm) getBean();

            String name = "Term: " + term.getOid() +
                    " [" + term.getSchedule().getSchool().getName() + "]";

            return name;
        }

    }


    /**
     * The Class RetrieveTermBeginDate.
     */
    protected class RetrieveTermBeginDate implements FieldRetriever {

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

            TermCollectionEntity termCollectionEntity = (TermCollectionEntity) entity;
            ScheduleTerm scheduleTerm = ((ScheduleTerm) termCollectionEntity.getBean());

            Collection<ScheduleTermDate> scheduleTermDates = scheduleTerm.getScheduleTermDates();
            PlainDate termBeginDate = null;

            for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                if (termBeginDate == null || scheduleTermDate.getStartDate().before(termBeginDate)) {
                    termBeginDate = scheduleTermDate.getStartDate();
                }
            }

            return termBeginDate;
        }

    }

    /**
     * The Class RetrieveTermEndDate.
     */
    protected class RetrieveTermEndDate implements FieldRetriever {

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

            TermCollectionEntity termCollectionEntity = (TermCollectionEntity) entity;
            ScheduleTerm scheduleTerm = ((ScheduleTerm) termCollectionEntity.getBean());

            Collection<ScheduleTermDate> scheduleTermDates = scheduleTerm.getScheduleTermDates();
            PlainDate termEndDate = null;

            for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                if (termEndDate == null || scheduleTermDate.getEndDate().after(termEndDate)) {
                    termEndDate = scheduleTermDate.getEndDate();
                }
            }

            return termEndDate;
        }

    }

    /**
     * To retrieve the description from the Reference Table Schedule Term Codes.
     */
    protected class RetrieveTermCodeDescription implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TermCollectionEntity termCollectionEntity = (TermCollectionEntity) entity;
            ScheduleTerm scheduleTermBean = (ScheduleTerm) termCollectionEntity.getBean();
            String scheduleTermCode = null;
            if (null != scheduleTermBean) {
                scheduleTermCode = scheduleTermBean.getCode();
            }
            String scheduleTermCodeDescription = null;
            if (scheduleTermCode != null) {
                ReferenceCode termReferenceCode = m_scheduleTermRefCodes.get(scheduleTermCode);
                if (termReferenceCode != null) {
                    scheduleTermCodeDescription = termReferenceCode.getDescription();
                }
            }

            return scheduleTermCodeDescription;
        }
    }

    /**
     * Initialize the data module.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() == 0) {
            X2Criteria termCollectionCriteria = getTermCollectionCriteria();
            QueryByCriteria termCollectionQuery = new QueryByCriteria(ScheduleTerm.class, termCollectionCriteria);

            applyInputSort(termCollectionQuery, ScheduleTerm.REL_SCHEDULE);

            // Set the query to be used for student selection.
            setQuery(termCollectionQuery);

            // Collection collectionByQuery = getBroker().getCollectionByQuery(termCollectionQuery);

            setEntityClass(TermCollectionEntity.class);

            // Get Schedule Term Code reference map.
            Criteria scheduleTermRefCriteria = new Criteria();
            scheduleTermRefCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbSchTermCode");
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, scheduleTermRefCriteria);
            m_scheduleTermRefCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 20);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TermBeginDate", new RetrieveTermBeginDate());
            calcs.put("TermEndDate", new RetrieveTermEndDate());
            calcs.put("TERM_CODE_DESC", new RetrieveTermCodeDescription());

            addCalcs(calcs);

        }
    }

    /**
     * Gets the term collection criteria.
     *
     * @return Schedule Term criteria
     */
    private X2Criteria getTermCollectionCriteria() {
        X2Criteria termCollectionCriteria = new X2Criteria();

        if (isSchoolContext()) {
            termCollectionCriteria.addEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_SCHOOL_ID, getSchool().getOid());
        }
        termCollectionCriteria.addEqualToField(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                MasterSchedule.COL_SCHEDULE_OID);

        termCollectionCriteria.addNotEqualTo(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);

        termCollectionCriteria.addNotEmpty(ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                m_sklIdField, getBroker().getPersistenceKey());

        return termCollectionCriteria;
    }


}
