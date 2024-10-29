/*
 * ====================================================================
 * X2 Development Corporation
 * Copyright (c) 2002-2010 X2 Development Corporation. All rights reserved. Redistribution and use
 * in source and binary forms, with or without modification, is not permitted without express
 * written agreement from X2 Development Corporation.
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.X2Broker;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolTranscript;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This class implements the data export for New Hampshire's<br>
 * <blockquote style="color:green; font-size:16pt">Extended Learning Opportunity Export
 * </blockquote>.
 *
 * @author X2 Development Corporation
 * @since v3.0
 */
public class NHStudentELOData extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the NHStudentELOData export. This must be a
     * public static inner class with a public no argument constructor so it can be instantiated
     * through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class NHStudentELOEntity extends ToolsSharedContainer.StateReportEntity {

        /** The m trn list. */
        List<ToolTranscript> m_trnList;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NHStudentELOEntity() {
            // public no argument constructor for dynamic instantiation.
        }


        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ToolStudent student = (ToolStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    (getData().isSchoolContext()
                            ? ", SCHOOL: " + student.getSchool(getData().getBroker()).getName()
                            : "")
                    +
                    "]";
            return name;
        }

        /**
         * Gets the NH transcipt.
         *
         * @return NH transcript
         */
        public NHTranscript getNHTranscipt() {
            return (NHTranscript) m_trnList.get(getCurrentRow());
        }

        /**
         * Set parent and bean values for the entity.
         * NOTE: This method name cannot be changed and a new identical method named initialize()
         * cannot be successfully introduced. The existing method with spelling error is called by
         * StateReportData and refactoring all existing code and customizations is impractical.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(ToolsSharedContainer.StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            ToolStudent std = (ToolStudent) bean;
            m_trnList = std.getStudentTranscripts(data.getBroker()).stream().filter(trn -> {
                NHTranscript trnToConsider = (NHTranscript) trn;
                return !StringUtils.isBlank(trnToConsider.getELOTypeState());
            }).collect(Collectors.toList());
            setRowCount(0);
            if (m_trnList != null && !m_trnList.isEmpty()) {
                setRowCount(m_trnList.size());
            }
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }

    /**
     * The Class Organization.
     */
    public static class NHOrganization extends ToolBean.ToolOrganization {
        private static final String ALIAS_ORG_DISTR_NUM = "i4see 030";
        private static final String ALIAS_ORG_SAU_NUM = "i4see 040";

        public static final ToolBeanColumn FIELD_ORG_DISTR_NUM =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION, ALIAS_ORG_DISTR_NUM);
        public static final ToolBeanColumn FIELD_ORG_SAU_NUM =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION, ALIAS_ORG_SAU_NUM);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolOrganization.FULL_DEFINITION.expand(
                        FIELD_ORG_DISTR_NUM,
                        FIELD_ORG_SAU_NUM);

        /**
         * Instantiates a new Organization.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public NHOrganization(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the district.
         *
         * @return String
         */
        public String getDistrictNum() {
            return getValueString(FIELD_ORG_DISTR_NUM);
        }

        /**
         * Gets the sau num.
         *
         * @return String
         */
        public String getSauNum() {
            return getValueString(FIELD_ORG_SAU_NUM);
        }
    }

    /**
     * The Class School.
     */
    public static class NHSchool extends ToolSchool {
        private static final String ALIAS_SKL_NUMBER = "i4see 050";

        public static final ToolBeanColumn FIELD_SKL_NUMBER =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_SKL_NUMBER);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchool.FULL_DEFINITION
                .expand(FIELD_SKL_NUMBER);

        /**
         * Instantiates a new School.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public NHSchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the organization 1.
         *
         * @param broker X2Broker
         * @return GA Organization Tool Bean
         */
        @Override
        public NHOrganization getOrganization1(X2Broker broker) {
            String orgOid = getValueString(FIELD_ORGANIZATION_1_OID);
            return getBeanByOid(broker, NHOrganization.class, orgOid, true);
        }

        /**
         * Gets the school code.
         *
         * @return String
         */
        public String getSchoolCode() {
            return getValueString(FIELD_SKL_NUMBER);
        }
    }

    /**
     * The Class NHTranscript.
     */
    public static class NHTranscript extends ToolTranscript {

        private static final String ALIAS_TRN_ELO_CARREER_CLUSTER = "all-trn-ELOCareerCluster";
        private static final String ALIAS_TRN_ELO_COMM_PARTNER = "all-trn-ELOCommunityPartner";
        private static final String ALIAS_TRN_ELO_OFF_CAMPUS = "all-trn-ELOOffCampus";
        private static final String ALIAS_TRN_ELO_SUBJ_AREA = "all-trn-ELOSubjectArea";
        private static final String ALIAS_TRN_ELO_TYPE = "all-trn-ELOType";

        public static final ToolBeanColumn FIELD_TRN_ELO_CARREER_CLUSTER =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_ELO_CARREER_CLUSTER);
        public static final ToolBeanColumn FIELD_TRN_ELO_COMM_PARTNER =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_ELO_COMM_PARTNER);
        public static final ToolBeanColumn FIELD_TRN_ELO_OFF_CAMPUS =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_ELO_OFF_CAMPUS);
        public static final ToolBeanColumn FIELD_TRN_ELO_SUBJ_AREA =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_ELO_SUBJ_AREA);
        public static final ToolBeanColumn FIELD_TRN_ELO_TYPE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_ELO_TYPE);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolTranscript.FULL_DEFINITION.expand(
                        FIELD_TRN_ELO_CARREER_CLUSTER,
                        FIELD_TRN_ELO_COMM_PARTNER,
                        FIELD_TRN_ELO_OFF_CAMPUS,
                        FIELD_TRN_ELO_SUBJ_AREA,
                        FIELD_TRN_ELO_TYPE);

        /**
         * Instantiates a new NH transcript.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public NHTranscript(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the ELO career cluster state.
         *
         * @return String
         */
        public String getELOCareerClusterState() {
            return getValueReferenceState(FIELD_TRN_ELO_CARREER_CLUSTER);
        }

        /**
         * Gets the ELO common partner.
         *
         * @return String
         */
        public String getELOCommPartner() {
            return getValueString(FIELD_TRN_ELO_COMM_PARTNER);
        }

        /**
         * Gets the ELO off campus indicator.
         *
         * @return boolean
         */
        public boolean getELOOffCampusIndicator() {
            return getValueLogical(FIELD_TRN_ELO_OFF_CAMPUS);
        }

        /**
         * Gets the ELO subject area state.
         *
         * @return String
         */
        public String getELOSubjectAreaState() {
            return getValueReferenceState(FIELD_TRN_ELO_SUBJ_AREA);
        }

        /**
         * Gets the ELO type state.
         *
         * @return String
         */
        public String getELOTypeState() {
            return getValueReferenceState(FIELD_TRN_ELO_TYPE);
        }
    }

    /**
     * The Class RetrieveELOData.
     */
    protected class RetrieveELOData implements FieldRetriever {
        private static final String CALC_ID = "NH-ELO";
        private static final String CALC_PARAM_CAREER_CLUSTER = "ELO_CAREER_CLUSTER";
        private static final String CALC_PARAM_COMM_PARTNER = "ELO_COMM_PARTNER";
        private static final String CALC_PARAM_CREDITS_ATTEMPTED = "CREDITS_ATTEMPTED";
        private static final String CALC_PARAM_CREDITS_EARNED = "CREDITS_EARNED";
        private static final String CALC_PARAM_IS_ELO_CREDITS_EARNED = "IS_CREDITS_EARNED";
        private static final String CALC_PARAM_NUM_SAU = "SAU_NUM";
        private static final String CALC_PARAM_NUM_DISTR = "DISTR_NUM";
        private static final String CALC_PARAM_NUM_SKL = "SKL_NUM";
        private static final String CALC_PARAM_OFF_CAMPUS = "ELO_OFF_CAMPUS";
        private static final String CALC_PARAM_SUBJ_AREA = "ELO_SUBJ_AREA";
        private static final String CALC_PARAM_TYPE = "ELO_TYPE";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            NHTranscript trnToConsider = ((NHStudentELOEntity) entity).getNHTranscipt();
            String param = (String) field.getParameter();
            String returnValue = null;
            if (trnToConsider != null) {
                if (CALC_PARAM_CAREER_CLUSTER.equalsIgnoreCase(param)) {
                    returnValue = trnToConsider.getELOCareerClusterState();
                } else if (CALC_PARAM_COMM_PARTNER.equalsIgnoreCase(param)) {
                    returnValue = trnToConsider.getELOCommPartner();
                } else if (CALC_PARAM_CREDITS_ATTEMPTED.equalsIgnoreCase(param)) {
                    BigDecimal creditAttempted = trnToConsider.getCourse(getBroker()) != null
                            ? trnToConsider.getCourse(getBroker()).getCourseCredit()
                            : null;
                    if (creditAttempted != null) {
                        creditAttempted = creditAttempted.setScale(2, RoundingMode.CEILING);
                    }
                    returnValue = creditAttempted == null ? "" : creditAttempted.toString();

                } else if (CALC_PARAM_CREDITS_EARNED.equalsIgnoreCase(param)) {
                    BigDecimal creditEarned = trnToConsider.getTotalCredit();
                    if (creditEarned != null) {
                        creditEarned = creditEarned.setScale(2, RoundingMode.CEILING);
                    }
                    returnValue = creditEarned == null ? "" : creditEarned.toString();
                } else if (CALC_PARAM_IS_ELO_CREDITS_EARNED.equalsIgnoreCase(param)) {
                    returnValue = trnToConsider.getTotalCredit() != null
                            && trnToConsider.getTotalCredit().compareTo(BigDecimal.ZERO) > 0 ? "Y" : "N";
                } else if (CALC_PARAM_OFF_CAMPUS.equalsIgnoreCase(param)) {
                    returnValue = trnToConsider.getELOOffCampusIndicator() ? "Y" : "N";
                } else if (CALC_PARAM_SUBJ_AREA.equalsIgnoreCase(param)) {
                    returnValue = trnToConsider.getELOSubjectAreaState();
                } else if (CALC_PARAM_TYPE.equalsIgnoreCase(param)) {
                    returnValue = trnToConsider.getELOTypeState();
                } else if (CALC_PARAM_NUM_SKL.equalsIgnoreCase(param)) {
                    returnValue = ((NHSchool) trnToConsider.getSchool(getBroker())).getSchoolCode();
                } else if (CALC_PARAM_NUM_SAU.equalsIgnoreCase(param)) {
                    NHSchool skl = (NHSchool) trnToConsider.getSchool(getBroker());
                    NHOrganization org1 = skl.getOrganization1(getBroker());
                    returnValue = org1.getSauNum();
                } else if (CALC_PARAM_NUM_DISTR.equalsIgnoreCase(param)) {
                    NHSchool skl = (NHSchool) trnToConsider.getSchool(getBroker());
                    NHOrganization org1 = skl.getOrganization1(getBroker());
                    returnValue = org1.getDistrictNum();
                }
            }
            return returnValue;
        }
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return getParameter(PARAM_REMOVE_HEADER) != null ? !((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()
                : false;
    }

    /**
     * Input Definition Parameters
     */
    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * Initialize the data module. Initialize necessary working resources. Define query for students
     * to load. Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {

        // Set the query to be used for student selection.
        if (getSetupErrors().size() == 0) {
            ToolBean.setDictionaryExtractor(getDictionaryExtractor());
            ToolBean.registerClass(NHSchool.class);
            ToolBean.registerClass(NHOrganization.class);
            ToolBean.registerClass(NHTranscript.class);
            ToolBean.registerClass(ToolStudent.class);

            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setCurrentContext(getCurrentContext())
                    .setEndDate(getCurrentContext().getEndDate())
                    .setIncludeSecondarySpans(false);
            if (isSchoolContext()) {
                spanCriteria.setSchoolOids(Arrays.asList(getSchool().getOid()));
            }
            X2Criteria inputCriteria = new X2Criteria();
            applyInputCriteria(inputCriteria, false, null);
            if (!inputCriteria.isEmpty()) {
                spanCriteria.setStudentLimitingCriteria(inputCriteria);
            }
            X2Criteria trnCriteria = new X2Criteria();
            trnCriteria.addEqualTo(ToolTranscript.FIELD_DISTRICT_CONTEXT_OID.resolve(getDictionaryExtractor()),
                    getCurrentContext().getOid());
            trnCriteria.addNotEmpty(NHTranscript.FIELD_TRN_ELO_TYPE.resolve(getDictionaryExtractor()),
                    getBroker().getPersistenceKey());
            ToolBean.addAndCriteria(getBroker(), NHTranscript.class, trnCriteria);
            ToolBean.load(getBroker(), getDictionaryExtractor(), NHTranscript.class);
            List<String> studentOids = ToolBean.getCachedToolBeans(NHTranscript.class).stream()
                    .map(trnELO -> trnELO.getStudentOid()).distinct().collect(Collectors.toList());

            X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria, getBroker());
            candidateCriteria.addIn(ToolBean.FIELD_OID.resolve(getDictionaryExtractor()),
                    studentOids);
            // Check user selection criteria.
            setEntityClass(NHStudentELOEntity.class);
            setFilterable(FilterableFactory.create(getBroker(), ToolStudent.class, candidateCriteria, null));

            // Add any retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveELOData.CALC_ID, new RetrieveELOData());
            super.addCalcs(calcs);
        }
    }
}
