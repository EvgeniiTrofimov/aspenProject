/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolBeanDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnGraduationRequirement;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnGraduationStudentProgram;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.ShsmAssessment;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisShsmProgram.OnsisShsmProgramEntity;
import com.x2dev.sis.model.beans.GraduationRequirement;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisShsmCertification extends OnsisStateReportData {
    /**
     * The Class OnsisShsmCertificationEntity.
     */
    public static class OnsisShsmCertificationEntity extends OnsisStateReportEntity {
        private List<ShsmAssessment> m_certifications;

        /**
         * Gets the assessment.
         *
         * @return Student assessment
         */
        public ShsmAssessment getAssessment() {
            return m_certifications.get(getCurrentRow());
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnsisStudent student = (OnsisStudent) getBean();
            String name = student.getNameView();

            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            final OnsisShsmCertification onsisData = (OnsisShsmCertification) data;
            OnsisShsmProgramEntity shsmProgramEntity = (OnsisShsmProgramEntity) onsisData.getParentEntity();
            OnGraduationStudentProgram studentProgram = shsmProgramEntity.getStudentProgram();

            Filterable<OnGraduationRequirement> graduationReqHelper =
                    getGlobalData().getFilterablesHelper().getFilterable(OnGraduationRequirement.class);
            ToolBeanDefinition graduationRequirementLookupColumns = new ToolBeanDefinition(
                    OnGraduationRequirement.FIELD_TYPE, OnGraduationRequirement.FIELD_PROGRAM_STUDIES_OID);

            /*
             * Find parent program requirements
             */
            List<OnGraduationRequirement> programGraduationRequirements =
                    graduationReqHelper.getGroup(graduationRequirementLookupColumns,
                            Arrays.asList(GraduationRequirement.TYPE_PROGRAM, studentProgram.getProgramStudiesOid()));

            /*
             * Find linked sub graduation requirements: Compulsory and Elective
             */
            Set<String> subProgramStudiesOids = programGraduationRequirements.stream()
                    .filter(grq -> OnGraduationRequirement.CERT_REQ_COMPULSORY.equals(grq.getCode())
                            || OnGraduationRequirement.CERT_REQ_ELECTIVE.equals(grq.getCode()))
                    .map(grq -> grq.getSubProgramStudiesOid()).collect(Collectors.toSet());

            /*
             * Gather certificate requirements
             */
            List<OnGraduationRequirement> graduationRequirements = subProgramStudiesOids.stream()
                    .map(programStudiesOid -> graduationReqHelper.getGroup(graduationRequirementLookupColumns,
                            Arrays.asList(GraduationRequirement.TYPE_OTHER, programStudiesOid)).stream())
                    .flatMap(graduationRequirementsStream -> graduationRequirementsStream
                            .filter(grq -> !StringUtils.isBlank(grq.getEvaluationDefinition())))
                    .collect(Collectors.toList());

            OnsisStudent student = (OnsisStudent) getBean();
            List<ShsmAssessment> candidates = student.getShsmAssessments(getBroker()).stream()
                    .filter(asm -> asm.getAttainedIndicator() && asm.getDate() != null
                            && !asm.getDate().after(getGlobalData().getEndDate()))
                    .collect(Collectors.toList());

            m_certifications = graduationRequirements.stream()
                    .flatMap(grq -> candidates.stream().filter(asm -> grq.matchesAssessment(getBroker(), asm)))
                    .distinct()
                    .sorted(new Comparator<ShsmAssessment>() {
                        @Override
                        public int compare(ShsmAssessment o1, ShsmAssessment o2) {
                            int compareTo = OnsisHelper.compareTo(o1.getCertificate(), o2.getCertificate());

                            if (compareTo == 0) {
                                PlainDate date1 = o1.getDate();
                                PlainDate date2 = o2.getDate();
                                if (date1 == null) {
                                    return -1;
                                } else if (date2 == null) {
                                    return 1;
                                }
                                compareTo = date1.compareTo(date2);
                            }
                            return compareTo;
                        }
                    })
                    .collect(Collectors.toList());

            setRowCount(m_certifications.size());
        }
    }

    private Map<String, Set<String>> m_asmByEvalDefinitions = new HashMap<>();

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(Arrays.asList(getParentEntity().getBean()));
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getMatchOnEmptyKeyValue(java.lang.String)
     */
    @Override
    public boolean getMatchOnEmptyKeyValue(String keyField) {
        return false;
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisShsmCertificationEntity.class);
    }

}
